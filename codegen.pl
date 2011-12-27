#!/usr/bin/perl

use strict;
use warnings;
   
use XML::LibXML;
use XML::LibXSLT;

my $input_dir = $ARGV[0] || '.';
my $output_dir = $ARGV[1] || 'codegen';
my $main_namespace = $ARGV[2] || 'df';
my $export_prefix = 'DFHACK_EXPORT ';

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
    
    die "Cannot use size, alignment or offset for ".$tag->nodeName."\n"
        if ((!$allow_size && defined $tag->getAttribute('size')) ||
            defined $tag->getAttribute('offset') ||
            (!$allow_align && defined $tag->getAttribute('alignment')));
}

sub check_name($) {
    my ($name) = @_;
    $name =~ /^[_a-zA-Z][_a-zA-Z0-9]*$/
        or die "Invalid identifier: $name\n";
    return $name;
}

sub is_attr_true($$) {
    my ($tag, $name) = @_;
    return ($tag->getAttribute($name)||'') eq 'true';
}

sub type_header_def($) {
    my ($name) = @_;
    return uc($main_namespace).'_'.uc($name).'_H';
}

sub translate_lookup($) {
    my ($str) = @_;
    return undef unless $str && $str =~ /^\$global((\.[_a-zA-Z0-9]+)+)$/;
    my @fields = split /\./, substr($1,1);
    my $expr = "df::global::".shift(@fields);
    for my $fn (@fields) {
        $expr = "_toref($expr).$fn";
    }
    return $expr;
}

# Text generation with indentation

our @lines;
our $indentation = 0;

sub with_emit(&;$) { 
    # Executes the code block, and returns emitted lines
    my ($blk, $start_indent) = @_;
    local @lines;
    local $indentation = ($start_indent||0);
    $blk->();
    return @lines;
}

sub emit(@) {
    # Emit an indented line to be returned from with_emit
    my $line = join('',map { defined($_) ? $_ : '' } @_);
    $line = (' 'x$indentation).$line unless length($line) == 0;
    push @lines, $line;
}

sub indent(&) {
    # Indent lines emitted from the block by one step
    my ($blk) = @_;
    local $indentation = $indentation+2;
    $blk->();
}

sub outdent(&) {
    # Unindent lines emitted from the block by one step
    my ($blk) = @_;
    local $indentation = ($indentation >= 2 ? $indentation-2 : 0);
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

# Static file output

my @static_lines;
my %static_includes;

sub with_emit_static(&) {
    my ($blk) = @_;
    $static_includes{$typename}++;
    push @static_lines, &with_emit($blk,2);
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
    return check_name($name);
}

sub with_anon(&;$) {
    # Establish a new anonymous namespace
    my ($blk,$stem) = @_;
    local $anon_id = $stem ? 0 : 1;
    local $anon_prefix = ($stem||'anon');
    $blk->();
}

# Primitive types

my @primitive_type_list =
    qw(int8_t uint8_t int16_t uint16_t
       int32_t uint32_t int64_t uint64_t
       s-float
       bool ptr-string stl-string flag-bit
       pointer padding
       static-string);

my %primitive_aliases = (
    'stl-string' => 'std::string',
    'ptr-string' => 'char*',
    'static-string' => 'char',
    'flag-bit' => 'void',
    'padding' => 'void',
    'pointer' => 'void*',
    's-float' => 'float',
);

my %primitive_types;
$primitive_types{$_}++ for @primitive_type_list;

sub primitive_type_name($) {
    my ($tag_name) = @_;
    $primitive_types{$tag_name}
        or die "Not primitive: $tag_name\n";
    return $primitive_aliases{$tag_name} || $tag_name;
}

# Type references

our %weak_refs;
our %strong_refs;

sub register_ref($;$) {
    # Register a reference to another type.
    # Strong ones require the type to be included.
    my ($ref, $is_strong) = @_;

    if ($ref) {
        my $type = $types{$ref}
            or die "Unknown type $ref referenced.\n";

        if ($is_strong) {
            $strong_refs{$ref}++;
        } else {
            $weak_refs{$ref}++;
        }
    }
}

sub with_struct_block(&$;$%) {
    my ($blk, $tag, $name, %flags) = @_;
    
    my $kwd = (is_attr_true($tag,'is-union') ? "union" : "struct");
    my $exp = $flags{-export} ? $export_prefix : '';
    my $prefix = $kwd.' '.$exp.($name ? $name.' ' : '');
    
    emit_block {
        local $_;
        if ($flags{-no_anon}) {
            $blk->();
        } else {
            &with_anon($blk);
        }
    } $prefix, ";";
}

sub decode_type_name_ref($;%) {
    # Interpret the type-name field of a tag
    my ($tag,%flags) = @_;
    my $force_type = $flags{-force_type};
    my $attr = $flags{-attr_name} || 'type-name';
    my $tname = $tag->getAttribute($attr) or return undef;

    if ($primitive_types{$tname}) {
        die "Cannot use type $tname as $attr here: $tag\n"
            if ($force_type && $force_type ne 'primitive');
        return primitive_type_name($tname);
    } else {
        register_ref $tname, !$flags{-weak};
        die "Cannot use type $tname as $attr here: $tag\n"
            if ($force_type && $force_type ne $types{$tname}->getAttribute('ld:meta'));
        return $main_namespace.'::'.$tname;
    }
}

# CONDITIONALS

sub is_conditional($) {
    my ($tag) = @_;
    return $tag->nodeName =~ /^(cond-if|cond-elseif)$/;    
}

sub translate_if_cond($) {
    my ($tag) = @_;

    my @rules;
    if (my $defvar = $tag->getAttribute('defined')) {
        push @rules, "defined($defvar)";
    }
    if (my $cmpvar = $tag->getAttribute('var')) {
        if (my $cmpval = $tag->getAttribute('lt')) {
            push @rules, "($cmpvar < $cmpval)";
        }
        if (my $cmpval = $tag->getAttribute('le')) {
            push @rules, "($cmpvar <= $cmpval)";
        }
        if (my $cmpval = $tag->getAttribute('eq')) {
            push @rules, "($cmpvar == $cmpval)";
        }
        if (my $cmpval = $tag->getAttribute('ge')) {
            push @rules, "($cmpvar >= $cmpval)";
        }
        if (my $cmpval = $tag->getAttribute('gt')) {
            push @rules, "($cmpvar > $cmpval)";
        }
        if (my $cmpval = $tag->getAttribute('ne')) {
            push @rules, "($cmpvar != $cmpval)";
        }
    }
    return '('.(join(' && ',@rules) || '1').')';
}

our $in_cond = 0;

sub render_cond_if($$$;@) {
    my ($tag, $in_elseif, $render_cb, @tail) = @_;

    local $in_cond = 1;

    {
        local $indentation = 0;
        my $op = ($in_elseif && $in_elseif >= 2) ? '#elif' : '#if';
        emit $op, ' ', translate_if_cond($tag);
    }

    for my $child ($tag->findnodes('child::*')) {
        &render_cond($child, $render_cb, @tail);
    }

    unless ($in_elseif) {
        local $indentation = 0;
        emit "#endif";
    }
}

sub render_cond($$;@) {
    my ($tag, $render_cb, @tail) = @_;
    
    my $tag_name = $tag->nodeName;
    if ($tag_name eq 'cond-if') {
        render_cond_if($tag, 0, $render_cb, @tail);
    } elsif ($tag_name eq 'cond-elseif') {
        my $idx = 1;
        for my $child ($tag->findnodes('child::*')) {
            ($child->nodeName eq 'cond-if')
                or die "Only cond-if tags may be inside a cond-switch: ".$child->nodeName."\n";
            render_cond_if($child, $idx++, $render_cb, @tail);
        }
        {
            local $indentation = 0;
            emit "#endif";
        }
    } else {
        local $_ = $tag;
        $render_cb->($tag, @tail);
    }
}

# ENUM

sub render_enum_core($$) {
    my ($name,$tag) = @_;

    my $base = 0;

    emit_block {
        my @items = $tag->findnodes('child::*');
        my $idx = 0;

        for my $item (@items) {
            render_cond $item, sub {
                my $tag = $_->nodeName;
                return if $tag eq 'enum-attr';
                ($tag eq 'enum-item')
                    or die "Invalid enum member: ".$item->nodeName."\n";

                my $name = ensure_name $_->getAttribute('name');
                my $value = $_->getAttribute('value');

                $base = ($idx == 0 && !$in_cond) ? $value : undef if defined $value;
                $idx++;

                emit $name, (defined($value) ? ' = '.$value : ''), ',';
            };
        }

        emit "_last_item_of_$name";
    } "enum $name ", ";";

    return $base;
}

sub render_enum_tables($$$) {
    my ($name,$tag,$base) = @_;

    # Enumerate enum attributes

    my %aidx = ('key' => 0);
    my @anames = ('key');
    my @avals = ('NULL');
    my @atypes = ('const char*');
    my @atnames = (undef);
    my @aprefix = ('');

    for my $attr ($tag->findnodes('child::enum-attr')) {
        my $name = $attr->getAttribute('name') or die "Unnamed enum-attr.\n";
        my $type = decode_type_name_ref $attr;
        my $def = $attr->getAttribute('default-value');

        my $base_tname = ($type && $type =~ /::(.*)$/ ? $1 : '');
        $type = $base_tname if $base_tname eq $typename;

        die "Duplicate attribute $name.\n" if exists $aidx{$name};

        check_name $name;
        $aidx{$name} = scalar @anames;
        push @anames, $name;
        push @atnames, $type;

        if ($type) {
            push @atypes, $type;
            push @aprefix, ($base_tname ? $base_tname."::" : '');
            push @avals, (defined $def ? $aprefix[-1].$def : "($type)0");
        } else {
            push @atypes, 'const char*';
            push @avals, (defined $def ? "\"$def\"" : 'NULL');
            push @aprefix, '';
        }
    }

    # Emit accessor function prototypes

    emit "const $name _first_item_of_$name = ($name)$base;";

    emit_block {
        emit "return (value >= _first_item_of_$name && value < _last_item_of_$name);";
    } "inline bool is_valid($name value) ";

    for (my $i = 0; $i < @anames; $i++) {
        emit "${export_prefix}$atypes[$i] get_$anames[$i]($name value);";
    }

    # Emit implementation

    with_emit_static {
        emit_block {
            emit_block {
                # Emit the entry type
                emit_block {
                    for (my $i = 0; $i < @anames; $i++) {
                        emit "$atypes[$i] $anames[$i];";
                    }
                } "struct _info_entry ", ";";
            
                # Emit the info table
                emit_block {
                    for my $item ($tag->findnodes('child::*')) {
                        render_cond $item, sub {
                            my $tag = $_->nodeName;
                            return if $tag eq 'enum-attr';
                            
                            # Assemble item-specific attr values
                            my @evals = @avals;
                            my $name = $_->getAttribute('name');
                            $evals[0] = "\"$name\"" if $name;

                            for my $attr ($_->findnodes('child::item-attr')) {
                                my $name = $attr->getAttribute('name') or die "Unnamed item-attr.\n";
                                my $value = $attr->getAttribute('value') or die "No-value item-attr.\n";
                                my $idx = $aidx{$name} or die "Unknown item-attr: $name\n";

                                if ($atnames[$idx]) {
                                    $evals[$idx] = $aprefix[$idx].$value;
                                } else {
                                    $evals[$idx] = "\"$value\"";
                                }
                            }

                            emit "{ ",join(', ',@evals)," },";
                        };
                    }

                    emit "{ ",join(', ',@avals)," }";
                } "static const _info_entry _info[] = ", ";";

                for (my $i = 0; $i < @anames; $i++) {
                    emit_block {
                        emit "return is_valid(value) ? _info[value - $base].$anames[$i] : $avals[$i];";
                    } "$atypes[$i] get_$anames[$i]($name value) ";
                }
            } "namespace $name ";
        } "namespace enums ";
    };
}

sub render_enum_type {
    my ($tag) = @_;

    emit_block {
        emit_block {
            my $base = render_enum_core($typename,$tag);
            
            if (defined $base) {
                render_enum_tables($typename,$tag,$base);
            } else {
                print STDERR "Warning: complex enum: $typename\n";
            }
        } "namespace $typename ";
    } "namespace enums ";

    emit "using enums::",$typename,"::",$typename,";";
}

# BITFIELD

sub get_primitive_base($;$) {
    my ($tag, $default) = @_;

    my $base = $tag->getAttribute('base-type') || $default || 'uint32_t';
    $primitive_types{$base} or die "Must be primitive: $base\n";

    return $base;
}

sub render_bitfield_core {
    my ($name, $tag) = @_;

    emit_block {
        emit get_primitive_base($tag), ' whole;';

        emit_block {
            for my $item ($tag->findnodes('child::*')) {
                render_cond $item, sub {
                    my ($item) = @_;
                    ($item->nodeName eq 'ld:field' &&
                     $item->getAttribute('ld:meta') eq 'primitive' &&
                     $item->getAttribute('type-name') eq 'flag-bit')
                        or die "Invalid bitfield member: ".$item->toString."\n";

                    check_bad_attrs($item);
                    my $name = ensure_name $item->getAttribute('name');
                    my $size = $item->getAttribute('count') || 1;
                    emit "unsigned ", $name, " : ", $size, ";";
                };
            }
        } "struct ", " bits;";
    } "union $name ", ";";
}

sub render_bitfield_type {
    my ($tag) = @_;
    render_bitfield_core($typename,$tag);
}

# STRUCT

my %struct_field_handlers;

sub get_struct_fields($) {
    return $_[0]->findnodes('ld:field');
}

sub get_container_item_type($;%) {
    my ($tag, %flags) = @_;
    my @items = $tag->findnodes('ld:item');
    if (@items) {
        return get_struct_field_type($items[0], %flags);
    } elsif ($flags{-void}) {
        return $flags{-void};
    } else {
        die "Container without element: $tag\n";
    }
}

my %atable = ( 1 => 'char', 2 => 'short', 4 => 'int' );

my %custom_container_handlers = (
    'stl-vector' => sub {
        my $item = get_container_item_type($_, -void => 'void*');
        $item = 'char' if $item eq 'bool';
        return "std::vector<$item>";
    },
    'stl-bit-vector' => sub {
        return "std::vector<bool>";
    },
    'df-flagarray' => sub {
        my $type = decode_type_name_ref($_, -attr_name => 'index-enum', -force_type => 'enum-type') || 'int';
        return "BitArray<$type>";
    },
);

sub emit_typedef($$) {
    # Convert a prefix/postfix pair into a single name
    my ($pre, $post) = @_;
    my $name = ensure_name undef;
    emit 'typedef ', $pre, ' ', $name, $post, ';';
    return $name;
}

sub get_struct_field_type($;%) {
    # Dispatch on the tag name, and retrieve the type prefix & suffix
    my ($tag, %flags) = @_;
    my $meta = $tag->getAttribute('ld:meta');
    my $tname = $tag->getAttribute('type-name');
    my $subtype = $tag->getAttribute('ld:subtype');
    my $prefix;
    my $suffix = '';

    if ($prefix = $tag->getAttribute('ld:typedef-name')) {
        # Nothing to do - already named
    } elsif ($meta eq 'primitive') {
        if ($tname eq 'static-string' && !$flags{-weak}) {
            my $count = $tag->getAttribute('size') || 0;
            $prefix = "char";
            $suffix = "[$count]";
        } elsif ($tname eq 'padding' && !$flags{-weak}) {
            my $count = $tag->getAttribute('size') || 0;
            my $alignment = $tag->getAttribute('alignment') || 1;
            $prefix = $atable{$alignment} or die "Invalid alignment: $alignment\n";
            ($count % $alignment) == 0 or die "Invalid size & alignment: $count $alignment\n";
            $suffix = "[".($count/$alignment)."]";
        } else {
            $prefix = primitive_type_name($tname);
        }
    } elsif ($meta eq 'global') {
        register_ref $tname, !$flags{-weak};
        $prefix = $main_namespace.'::'.$tname;
    } elsif ($meta eq 'compound') {
        $prefix = ensure_name undef;
        $tag->setAttribute('ld:typedef-name', $prefix);

        $subtype ||= 'compound';
        if ($subtype eq 'enum') {
            with_anon {
                render_enum_core($prefix,$tag);
            };
        } elsif ($subtype eq 'bitfield') {
            with_anon {
                render_bitfield_core($prefix,$tag);
            };
        } else {
            with_struct_block {
                &render_struct_field($_) for get_struct_fields($tag);
            } $tag, $prefix;
        }
    } elsif ($meta eq 'pointer') {
        $prefix = get_container_item_type($tag, -weak => 1, -void => 'void')."*";
    } elsif ($meta eq 'static-array') {
        ($prefix, $suffix) = get_container_item_type($tag);
        my $count = $tag->getAttribute('count') || 0;
        $suffix = "[$count]".$suffix;
    } elsif ($meta eq 'container') {
        local $_ = $tag;
        $prefix = $custom_container_handlers{$subtype}->($tag, %flags);
    } else {
        die "Invalid field meta type: $meta\n";
    }

    if ($subtype && $subtype eq 'enum') {
        my $base = get_primitive_base($tag, 'int32_t');
        $prefix = "enum_field<$prefix,$base>";
    }

    return ($prefix,$suffix) if wantarray;
    if ($suffix) {
        $prefix = emit_typedef($prefix, $suffix);
        $tag->setAttribute('ld:typedef-name', $prefix);
    }
    return $prefix;
}

sub do_render_struct_field($) {
    my ($tag) = @_;
    my $tag_name = $tag->getAttribute('ld:meta');
    my $field_name = $tag->getAttribute('name');

    # Special case: anonymous compounds.
    if ($tag_name eq 'compound' && !defined $field_name
        && ($tag->getAttribute('subtype')||'compound') eq 'compound')
    {
        check_bad_attrs($tag);
        with_struct_block {
            render_struct_field($_) for get_struct_fields($tag);
        } $tag, undef, -no_anon => 1;
        return;
    }

    # Otherwise, create the name if necessary, and render
    my $name = ensure_name $field_name;
    $tag->setAttribute('ld:anon-name', $name) unless $field_name;
    with_anon {
        my ($prefix, $postfix) = get_struct_field_type($tag);
        emit $prefix, ' ', $name, $postfix, ';';
    } "T_$name";
}

sub render_struct_field($) {
    my ($tag) = @_;
    render_cond $tag, \&do_render_struct_field;
}

sub emit_find_instance {
    my ($tag) = @_;

    my $instance_vector = translate_lookup $tag->getAttribute('instance-vector');
    if ($instance_vector) {
        emit "static std::vector<$typename*> &get_vector();";
        emit "static $typename *find(int id);";

        with_emit_static {
            emit_block {
                emit "return ", $instance_vector, ";";
            } "std::vector<$typename*>& ${typename}::get_vector() ";

            emit_block {
                emit "std::vector<$typename*> &vec_ = get_vector();";

                if (my $id = $tag->getAttribute('key-field')) {
                    emit "return binsearch_in_vector(vec_, &${typename}::$id, id_);";
                } else {
                    emit "return (id_ >= 0 && id_ < vec_.size()) ? vec_[id_] : NULL;";
                }
            } "$typename *${typename}::find(int id_) ";
        }
    }
}

sub render_virtual_methods {
    my ($tag) = @_;

    my @parents = ( $tag );
    for (;;) {
        my $inherits = $parents[0]->getAttribute('inherits-from') or last;
        my $parent = $types{$inherits} || die "Unknown parent: $inherits\n";
        unshift @parents, $parent;
    }

    my %name_index;
    my @vtable;
    my @starts;
    my $dtor_id = '~destructor';

    for my $type (@parents) {
        push @starts, scalar(@vtable);
        for my $method ($type->findnodes('virtual-methods/vmethod')) {
            my $is_destructor = is_attr_true($method, 'is-destructor');
            my $name = $is_destructor ? $dtor_id : $method->getAttribute('name');
            if ($name) {
                die "Duplicate method: $name in ".$type->getAttribute('type-name')."\n"
                    if exists $name_index{$name};
                $name_index{$name} = scalar(@vtable);
            }
            push @vtable, $method;
        }
    }

    my $dtor_idx = $name_index{$dtor_id};
    unless (defined $dtor_idx) {
        for (my $i = 0; $i <= $#vtable; $i++) {
            next if $vtable[$i]->getAttribute('name');
            $name_index{$dtor_id} = $dtor_idx = $i;
            last;
        }
    }
    unless (defined $dtor_idx) {
        push @vtable, undef;
        $dtor_idx = $#vtable;
    }

    my $min_id = $starts[-1];
    my $cur_mode = '';
    for (my $idx = $min_id; $idx <= $#vtable; $idx++) {
        my $method = $vtable[$idx];
        my $is_destructor = $method ? is_attr_true($method, 'is-destructor') : 1;
        my $name = $is_destructor ? $typename : $method->getAttribute('name');

        my $rq_mode = ($method && $name) ? 'public' : 'protected';
        unless ($rq_mode eq $cur_mode) {
            $cur_mode = $rq_mode;
            outdent { emit "$cur_mode:"; }
        }

        if ($idx == $dtor_idx) {
            $is_destructor = 1;
            $name = $typename;
        }

        with_anon {
            $name = ensure_name $name;
            my @ret_type = $is_destructor ? () : $method->findnodes('ret-type');
            my @arg_types = $is_destructor ? () : $method->findnodes('ld:field');
            my $ret_type = $ret_type[0] ? get_struct_field_type($ret_type[0]) : 'void';
            my $ret_stmt = '';
            unless ($ret_type eq 'void') {
                $ret_stmt = ' return '.($ret_type =~ /\*$/ ? '0' : "$ret_type()").'; ';
            }
            $ret_type = $is_destructor ? '~' : $ret_type.' ';
            my @arg_strs = map { scalar get_struct_field_type($_) } @arg_types;
            emit 'virtual ', $ret_type, $name, '(', join(', ', @arg_strs), ') {', $ret_stmt, '}; //', $idx;
        } "anon_vmethod_$idx";
    }
}

sub render_struct_type {
    my ($tag) = @_;

    my $tag_name = $tag->getAttribute('ld:meta');
    my $is_class = ($tag_name eq 'class-type');
    my $has_methods = $is_class || is_attr_true($tag, 'has-methods');
    my $inherits = $tag->getAttribute('inherits-from');
    my $original_name = $tag->getAttribute('original-name');
    my $ispec = '';

    if ($inherits) {
        register_ref $inherits, 1;
        $ispec = ' : '.$inherits;
    } elsif ($is_class) {
        $ispec = ' : virtual_class';
    }

    with_struct_block {
        render_struct_field($_) for get_struct_fields($tag);

        emit_find_instance($tag);
        
        if ($has_methods) {
            if ($is_class) {
                emit "static class_virtual_identity<$typename> _identity;";
                with_emit_static {
                    emit "class_virtual_identity<$typename> ${typename}::_identity(",
                         "\"$typename\",",
                         ($original_name ? "\"$original_name\"" : 'NULL'), ',',
                         ($inherits ? "&${inherits}::_identity" : 'NULL'),
                         ");";
                }
            }

            if ($is_class) {
                render_virtual_methods $tag;
            } else {
                emit "~",$typename,"() {}";
            }
        }
    } $tag, "$typename$ispec", -export => 1;
}

# MAIN BODY

# Collect all type definitions from XML files

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

$0 =~ /^(.*?)(?:[\/\\][^\/\\]*)?$/;
my $script_dir = $1;
my $parser = XML::LibXML->new();
my $xslt = XML::LibXSLT->new();
my @transforms =
    map { $xslt->parse_stylesheet_file("$script_dir/$_"); }
    ('lower-1.xslt', 'lower-2.xslt');
my @documents;

for my $fn (sort { $a cmp $b } glob "$input_dir/*.xml") {
    local $filename = $fn;
    my $doc = $parser->parse_file($filename);
    $doc = $_->transform($doc) for @transforms;
    
    push @documents, $doc;
    add_type_to_hash $_ foreach $doc->findnodes('/ld:data-definition/ld:global-type');
}

# Generate text representations

my %type_handlers = (
    'enum-type' => \&render_enum_type,
    'bitfield-type' => \&render_bitfield_type,
    'class-type' => \&render_struct_type,
    'struct-type' => \&render_struct_type,
);

my %type_data;

for my $name (sort { $a cmp $b } keys %types) {
    local $typename = $name;
    local $filename = $type_files{$typename};
    local %weak_refs;
    local %strong_refs;

    eval {
        my $type = $types{$typename};
        my $meta = $type->getAttribute('ld:meta') or die "Null meta";

        # Emit the actual type definition
        my @code = with_emit {
            with_anon {
                $type_handlers{$meta}->($type);
            };
        } 2;

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
                    next if $strong_refs{$weak};
                    my $ttype = $types{$weak};
                    my $tstr = 'struct';
                    $tstr = 'enum' if $ttype->nodeName eq 'enum-type';
                    $tstr = 'union' if $ttype->nodeName eq 'bitfield-type';
                    $tstr = 'union' if ($ttype->nodeName eq 'struct-type' && is_attr_true($ttype,'is-union'));
                    emit $tstr, ' ', $weak, ';';
                }

                push @lines, @code;
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

{
    # Delete the old files
    for my $name (glob "$output_dir/*.h") {
        unlink $name;
    }

    # Write out the headers
    local $, = "\n";
    local $\ = "\n";

    for my $name (keys %type_data) {
        open FH, ">$output_dir/$name.h";
        print FH "/* THIS FILE WAS GENERATED. DO NOT EDIT. */";
        print FH @{$type_data{$name}};
        close FH;
    }

    # Write out the static file
    open FH, ">$output_dir/static.inc";
    print FH "/* THIS FILE WAS GENERATED. DO NOT EDIT. */";
    for my $name (sort { $a cmp $b } keys %static_includes) {
        print FH "#include \"$name.h\"";
    }
    print FH "namespace $main_namespace {";
    print FH @static_lines;
    print FH '}';
    close FH;
    
    # Write an xml file with all types
    open FH, ">$output_dir/codegen.out.xml";
    print FH '<ld:data-definition xmlns:ld="http://github.com/peterix/dfhack/lowered-data-definition">';
    for my $doc (@documents) {
        for my $node ($doc->documentElement()->findnodes('*')) {
            print FH '    '.$node->toString();
        }
    }
    print FH '</ld:data-definition>';
    close FH;
}
