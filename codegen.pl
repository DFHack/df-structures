#!/usr/bin/perl

use strict;
use warnings;
   
use XML::LibXML;

my $output_dir = 'codegen';
my $main_namespace = 'df';

my %types;
my %type_files;

# Misc XML analysis

our $typename;
our $filename;

sub parse_address($;$) {
    my ($str,$in_bits) = @_;
    return undef unless defined $str;
    
    # Parse the format used by offset attributes in xml
    $str =~ /^0x([0-9a-f]+)(?:\.([0-7]))?$/
        or die "Invalid address syntax: $str\n";
    my ($full, $bv) = ($1, $2);
    die "Bits not allowed: $str\n" unless $in_bits;
    return $in_bits ? (hex($full)*8 + ($bv||0)) : hex($full);
}

sub check_bad_attrs($;$$) {
    my ($tag, $allow_size, $allow_align) = @_;
    
    die "Cannot use size, alignment or offset for $typename in $filename\n"
        if ((!$allow_size && defined $tag->getAttribute('size')) ||
            defined $tag->getAttribute('offset') ||
            (!$allow_align && defined $tag->getAttribute('alignment')));
}

sub is_attr_true($$) {
    my ($tag, $name) = @_;
    return ($tag->getAttribute($name)||'') eq 'true';
}

# Text generation with indentation

our @lines;
our $prefix = '';

sub with_emit(&;$) { 
    # Executes the code block, and returns emitted lines
    my ($blk, $start_prefix) = @_;
    local @lines;
    local $prefix = ($start_prefix||'');
    $blk->();
    return @lines;
}

sub emit(@) {
    # Emit an indented line to be returned from with_emit
    my $line = join('',map { defined($_) ? $_ : '' } @_);
    $line = $prefix.$line unless length($line) == 0;
    push @lines, $line;
}

sub indent(&) {
    # Indent lines emitted from the block by one step
    my ($blk) = @_;
    local $prefix = $prefix.'  ';
    $blk->();
}

sub outdent(&) {
    # Unindent lines emitted from the block by one step
    my ($blk) = @_;
    local $prefix = substr($prefix,0,length($prefix)-2);
    $blk->();
}

sub emit_block(&;$$) {
    # Emit a full {...} block with indentation
    my ($blk, $prefix, $suffix) = @_;
    $prefix ||= '';
    $suffix ||= '';
    emit $prefix,'{';
    &indent($blk);
    emit '}',$suffix;
}

# Anonymous variable names

our $anon_id = 0;
our $anon_prefix;

sub ensure_name($) {
    # If the name is empty, assign an auto-generated one
    my ($name) = @_;
    unless ($name) {
        $name = $anon_prefix.(($anon_id == 0) ? '' : '_'.$anon_id);
        $anon_id++;
    }
    return $name;
}

sub with_anon(&;$) {
    # Establish a new anonymous namespace
    my ($blk,$stem) = @_;
    local $anon_id = $stem ? 0 : 1;
    local $anon_prefix = ($stem||'_anon');
    $blk->();
}

# Primitive types

my @primitive_type_list =
    qw(int8_t uint8_t int16_t uint16_t
       int32_t uint32_t int64_t uint64_t
       bool ptr-string stl-string flag-bit
       pointer);

my %primitive_aliases = (
    'stl-string' => 'std::string',
    'ptr-string' => 'char*',
    'flag-bit' => 'void',
    'pointer' => 'void*',
);

my %primitive_types;
$primitive_types{$_}++ for @primitive_type_list;

sub primitive_type_name($) {
    my ($tag_name) = @_;
    $primitive_types{$tag_name}
        or die "Not primitive: $tag_name in $typename in $filename\n";
    return $primitive_aliases{$tag_name} || $tag_name;
}

# ENUM

sub render_enum_core($$$) {
    my ($name,$tag,$emit_last) = @_;
    
    emit_block {
        my @items = $tag->findnodes('child::*');
        my $num = @items;
        $num++ if $emit_last;

        for my $item (@items) {
            ($item->nodeName eq 'enum-item')
                or die "Invalid enum member in $typename: $item\n";

            my $name = ensure_name $item->getAttribute('name');
            my $value = $item->getAttribute('value');

            emit $name, (defined($value) ? ' = '.$value : ''), (--$num > 0 ? ',' : '');
        }

        emit "_LAST_ENUM_ITEM" if $emit_last;
    } "enum $name ", ";";    
}

sub render_enum_type {
    my ($tag) = @_;

    emit_block {
        emit_block {
            render_enum_core($typename,$tag,1);
        } "namespace $typename ";
    } "namespace enums ";

    emit "using enums::",$typename,"::",$typename,";";
}

# BITFIELD

sub get_primitive_base($;$) {
    my ($tag, $default) = @_;

    my $base = $tag->getAttribute('base-type') || $default || 'uint32_t';
    $primitive_types{$base} or die "Must be primitive: $base in $typename in $filename\n";

    return $base;
}

sub render_bitfield_type {
    my ($tag) = @_;

    emit_block {
        emit get_primitive_base($tag), ' whole;';

        emit_block {
            for my $item ($tag->findnodes('child::*')) {
                ($item->nodeName eq 'flag-bit')
                    or die "Invalid bitfield member in $typename: $item\n";

                check_bad_attrs($item,1);
                my $name = ensure_name $item->getAttribute('name');
                my $size = parse_address($item->getAttribute('size'),1) || 1;
                emit "unsigned ", $name, " : ", $size, ";";
            }
        } "struct ", " bits;";
    } "union $typename ", ";";
}

# STRUCT

our %weak_refs;
our %strong_refs;

sub register_ref($;$) {
    # Register a reference to another type.
    # Strong ones require the type to be included.
    my ($ref, $is_strong) = @_;

    if ($ref) {
        my $type = $types{$ref}
            or die "Unknown type $ref referenced from $typename in $filename\n";

        if ($is_strong) {
            $strong_refs{$ref}++;
        } else {
            $weak_refs{$ref}++;
        }
    }
}

my %struct_field_handlers;

sub get_struct_fields($) {
    # Retrieve subtags that are actual struct fields
    my ($struct_tag) = @_;
    local $_;
    return grep {
        my $tag = $_->nodeName;
        die "Unknown tag: $tag in $typename in $filename\n"
            unless exists $struct_field_handlers{$tag};
        $struct_field_handlers{$tag};
    } $struct_tag->findnodes('child::*');
}

sub get_struct_field_type($) {
    # Dispatch on the tag name, and retrieve the type prefix & suffix
    my ($tag) = @_;
    my $handler = $struct_field_handlers{$tag->nodeName};
    return $handler->($tag);
}

our $is_strong_ref = 1;

sub render_struct_field($) {
    my ($tag) = @_;
    my $tag_name = $tag->nodeName;
    my $field_name = $tag->getAttribute('name');

    # Special case: anonymous compounds.
    if ($tag_name eq 'compound' && !defined $field_name &&
        !defined $tag->getAttribute('type-name')) 
    {
        check_bad_attrs($tag);
        with_anon {
            my $kwd = (is_attr_true($tag,'is-union') ? "union" : "struct");
            emit_block {
                local $_;
                local $is_strong_ref = 1;
                render_struct_field($_) for get_struct_fields($tag);
            } "$kwd ", ";";
        };
        return;
    }

    # Otherwise, create the name if necessary, and render
    my $name = ensure_name $field_name;
    with_anon {
        my ($prefix, $postfix) = get_struct_field_type($tag);
        emit $prefix, ' ', $name, $postfix, ';';
    } "T_$name";
}

sub emit_typedef($$) {
    # Convert a prefix/postfix pair into a single name
    my ($pre, $post) = @_;
    my $name = ensure_name undef;
    emit 'typedef ', $pre, ' ', $name, $post, ';';
    return $name;
}

sub get_container_item_type($$;$) {
    # Interpret the type-name and nested fields for a generic container type
    my ($tag,$strong_ref,$allow_void) = @_;
    
    local $is_strong_ref = $strong_ref;
    check_bad_attrs($tag);

    my $prefix = $tag->getAttribute('type-name'); 
    my $postfix = '';

    if ($prefix) {
        if ($primitive_types{$prefix}) {
            $prefix = primitive_type_name($prefix);
        } else {
            register_ref $prefix, $is_strong_ref;
        }
    } else {
        my @fields = get_struct_fields($tag);

        if (scalar(@fields) == 1) {
            ($prefix, $postfix) = get_struct_field_type($fields[0]);
        } elsif (scalar(@fields) == 0) {
            $allow_void or die "Empty container: ".$tag->nodeName." in $typename in $filename\n";
            $prefix = $allow_void;            
        } else {
            my $tname = ensure_name undef;
            with_anon {
                my $kwd = (is_attr_true($tag,'is-union') ? "union" : "struct");
                emit_block {
                    local $_;
                    local $is_strong_ref = 1;
                    render_struct_field($_) for @fields;
                } "$kwd $tname ", ";";
            };
            $prefix = $tname;
        }
    }
    
    return ($prefix,$postfix) if wantarray;
    return emit_typedef($prefix, $postfix) if $postfix;
    return $prefix;
}

sub get_primitive_field_type {
    # Primitive type handler
    my ($tag,$fname) = @_;
    check_bad_attrs($tag);
    my $name = $tag->nodeName;
    return (primitive_type_name($name), "");
}

sub get_static_string_type {
    # Static string handler
    my ($tag, $fname) = @_;
    check_bad_attrs($tag, 1);
    my $count = $tag->getAttribute('size') || 0;
    return ('char', "[$count]");
}

sub get_padding_type {
    # Padding handler. Supports limited alignment.
    my ($tag, $fname) = @_;

    check_bad_attrs($tag, 1, 1);
    my $count = $tag->getAttribute('size') || 0;
    my $align = $tag->getAttribute('alignment') || 1;

    if ($align == 1) {
        return ('char', "[$count]");
    } elsif ($align == 2) {
        ($count % 2 == 0) or die "Size not aligned in padding of $typename in $filename\n";
        return ('short', "[".($count/2)."]");
    } elsif ($align == 4) {
        ($count % 4 == 0) or die "Size not aligned in padding of $typename in $filename\n";
        return ('int', "[".($count/4)."]");
    } else {
        die "Bad padding alignment $align in $typename in $filename\n";
    }
}

sub get_static_array_type {
    # static-array handler
    my ($tag, $fname) = @_;
    my ($pre, $post) = get_container_item_type($tag, 1);
    my $count = $tag->getAttribute('count')
        or die "Count is mandatory for static-array in $typename in $filename\n";
    return ($pre, $post."[$count]");
}

sub get_pointer_type($) {
    # pointer handler
    my ($tag) = @_;
    my $item = get_container_item_type($tag, 0, 'void');
    return ($item.'*', '');
}

sub get_compound_type($) {
    # compound (nested struct) handler
    my ($tag) = @_;
    check_bad_attrs($tag);

    my $tname = $tag->getAttribute('type-name');
    if ($tname) {
        register_ref $tname, $is_strong_ref;
        return ($tname, '');
    } else {
        my $tname = ensure_name undef;
        with_anon {
            my $kwd = (is_attr_true($tag,'is-union') ? "union" : "struct");
            emit_block {
                local $_;
                local $is_strong_ref = 1;
                render_struct_field($_) for get_struct_fields($tag);
            } "$kwd $tname ", ";";
        };
        return ($tname,'');
    }
}

sub get_bitfield_type($) {
    # nested bitfield handler
    my ($tag) = @_;
    check_bad_attrs($tag);

    my $tname = $tag->getAttribute('type-name');
    if ($tname) {
        register_ref $tname, $is_strong_ref;
        return ($tname, '');
    } else {
        my $tname = ensure_name undef;
        with_anon {
            local $typename = $tname;
            render_bitfield_type($tag);
        };
        return ($tname,'');
    }
}

sub get_enum_type($) {
    # nested enum handler
    my ($tag) = @_;
    check_bad_attrs($tag);

    my $tname = $tag->getAttribute('type-name');
    my $base = get_primitive_base($tag, 'int32_t');

    if ($tname) {
        register_ref $tname, 1;
    } else {
        $tname = ensure_name undef;
        with_anon {
            render_enum_core($tname,$tag,0);
        }
    }

    return ("enum_field<$tname,$base>", '');
}

sub get_stl_vector_type($) {
    # STL vector
    my ($tag) = @_;
    my $item = get_container_item_type($tag,1,'void*');
    return ("std::vector<$item>", '');
}

sub get_df_flagarray_type($) {
    # DF flag array
    my ($tag) = @_;
    check_bad_attrs($tag);
    return ("flagarray", '');
}

# Struct dispatch table and core

%struct_field_handlers = (
    'comment' => undef, # skip
    'code-helper' => undef, # skip
    'static-string' => \&get_static_string_type,
    'padding' => \&get_padding_type,
    'static-array' => \&get_static_array_type,
    'pointer' => \&get_pointer_type,
    'compound' => \&get_compound_type,
    'bitfield' => \&get_bitfield_type,
    'enum' => \&get_enum_type,
    'stl-vector' => \&get_stl_vector_type,
    'df-flagarray' => \&get_df_flagarray_type,
);
$struct_field_handlers{$_} ||= \&get_primitive_field_type for @primitive_type_list;

sub render_struct_type {
    my ($tag) = @_;
    
    my $tag_name = $tag->nodeName;
    my $is_class = ($tag_name eq 'class-type');
    my $has_methods = $is_class || is_attr_true($tag, 'has-methods');
    my $inherits = $tag->getAttribute('inherits-from');
    my $ispec = '';

    if ($inherits) {
        register_ref $inherits, 1;
        $ispec = ': '.$inherits.' ';
    }

    emit_block {
        render_struct_field($_) for get_struct_fields($tag);

        if ($has_methods) {
            outdent {
                emit "protected:";
            };

            if ($is_class) {
                emit "virtual ~",$typename,"() {}";
            } elsif ($has_methods) {
                emit "~",$typename,"() {}";
            }
        }
    } "struct $typename $ispec", ";";
}

# MAIN BODY

# Collect all type definitions from XML files

my @xmls = @ARGV;

sub add_type_to_hash($) {
    my ($type) = @_;

    my $name = $type->getAttribute('type-name')
        or die "Type without a name in $filename\n";

    die "Duplicate definition of $name in $filename\n" if $types{$name};

    local $typename = $name;
    check_bad_attrs $type;
    $types{$name} = $type;
    $type_files{$name} = $filename;
}

for my $fn (@xmls) {
    local $filename = $fn;
    my $parser = XML::LibXML->new();
    my $doc    = $parser->parse_file($filename);

    add_type_to_hash $_ foreach $doc->findnodes('/data-definition/enum-type');
    add_type_to_hash $_ foreach $doc->findnodes('/data-definition/bitfield-type');
    add_type_to_hash $_ foreach $doc->findnodes('/data-definition/struct-type');
    add_type_to_hash $_ foreach $doc->findnodes('/data-definition/class-type');
}

# Generate text representations

my %type_handlers = (
    'enum-type' => \&render_enum_type,
    'bitfield-type' => \&render_bitfield_type,
    'class-type' => \&render_struct_type,
    'struct-type' => \&render_struct_type,
);

my %type_data;

sub type_header_def($) {
    my ($name) = @_;
    return uc($main_namespace).'_'.uc($name).'_H';
}

for my $name (keys %types) {
    local $typename = $name;
    local $filename = $type_files{$typename};
    local %weak_refs;
    local %strong_refs;

    eval {
        my $type = $types{$typename};

        # Emit the actual type definition
        my @code = with_emit {
            with_anon {
                $type_handlers{$type->nodeName}->($type);
            };
        };
        
        delete $weak_refs{$name};
        delete $strong_refs{$name};
        
        # Add wrapping
        my @all = with_emit {
            my $def = type_header_def($typename);
            emit "#ifndef $def";
            emit "#define $def";
        
            for my $strong (sort { $a cmp $b } keys %strong_refs) {
                my $sdef = type_header_def($strong);
                emit "#ifndef $sdef";
                emit "#include \"$strong.h\"";
                emit "#endif";            
            }

            emit_block {
                for my $weak (sort { $a cmp $b } keys %weak_refs) {
                    my $ttype = $types{$weak};
                    my $tstr = 'struct';
                    $tstr = 'enum' if $ttype->nodeName eq 'enum-type';
                    $tstr = 'union' if $ttype->nodeName eq 'bitfield-type';
                    $tstr = 'union' if ($ttype->nodeName eq 'struct-type' && is_attr_true($ttype,'is-union'));
                    emit $tstr, ' ', $weak, ';';
                }

                emit $_ for @code;
            } "namespace $main_namespace ";

            emit "#endif";
        };
        
        $type_data{$typename} = \@all;
    };
    if ($@) {
        print 'Error: '.$@."Type $typename in $filename ignored\n";
    }
}

# Write output files

mkdir $output_dir;

for my $name (keys %type_data) {
    open FH, ">$output_dir/$name.h";
    local $, = "\n";
    local $\ = "\n";
    print FH @{$type_data{$name}};
    close FH;
}
