package StructFields;

use utf8;
use strict;
use warnings;

BEGIN {
    use Exporter  ();
    our $VERSION = 1.00;
    our @ISA     = qw(Exporter);
    our @EXPORT  = qw(
        *in_struct_body *in_union_body &with_struct_block
        &get_container_item_type
        &get_struct_fields &get_struct_field_type
        &render_field_metadata
        &emit_struct_fields
        &find_subfield
    );
    our %EXPORT_TAGS = ( ); # eg: TAG => [ qw!name1 name2! ],
    our @EXPORT_OK   = qw( );
}

END { }

use XML::LibXML;

use Common;
use Enum;
use Bitfield;

# MISC

our $in_struct_body = 0;
our $in_union_body = 0;

sub with_struct_block(&$;$%) {
    my ($blk, $tag, $name, %flags) = @_;

    my $is_union = is_attr_true($tag,'is-union');
    my $kwd = ($is_union ? "union" : "struct");
    my $exp = $export_prefix; #$flags{-export} ? $export_prefix : '';
    my $prefix = $kwd.' '.$exp.($name ? $name.' ' : '');

    emit_comment $tag, -attr => 1;

    emit_block {
        local $_;
        local $in_struct_body = 1;
        local $in_union_body = ($in_union_body || $is_union);
        if ($flags{-no_anon}) {
            $blk->();
        } else {
            &with_anon($blk);
        }
    } $prefix, ";";
}

# FIELD TYPE

our %container_flags;

sub get_container_item_type($;%) {
    my ($tag, %flags) = @_;
    my @items = $tag->findnodes('ld:item');
    if (@items) {
        return get_struct_field_type($items[0], -local => $in_struct_body, %container_flags, %flags);
    } elsif ($flags{-void}) {
        return $flags{-void};
    } else {
        die "Container without element: $tag\n";
    }
}

my %atable = ( 1 => 'char', 2 => 'short', 4 => 'int' );

our $cur_init_field = '';
our $cur_init_value = undef;
sub add_simple_init($);

my %custom_primitive_handlers = (
    'stl-string' => sub { return "std::string"; },
);

my %custom_primitive_inits = (
    'stl-string' => sub {
        if (defined $cur_init_value) {
            $cur_init_value =~ s/\\/\\\\/g;
            $cur_init_value =~ s/\"/\\\"/g;
            add_simple_init "\"$cur_init_value\"";
        }
    },
);

my %custom_container_handlers = (
    'stl-vector' => sub {
        my $item = get_container_item_type($_, -void => 'void*');
        $item = 'char' if $item eq 'bool';
        return "std::vector<$item >";
    },
    'stl-deque' => sub {
        my $item = get_container_item_type($_, -void => 'void*');
        return "std::deque<$item >";
    },
    'stl-bit-vector' => sub {
        return "std::vector<bool>";
    },
    'df-flagarray' => sub {
        my $type = decode_type_name_ref($_, -attr_name => 'index-enum', -force_type => 'enum-type') || 'int';
        return "BitArray<$type>";
    },
    'df-array' => sub {
        my $item = get_container_item_type($_, -void => 'void*');
        return "DfArray<$item >";
    },
    'df-linked-list' => sub {
        my $item = get_container_item_type($_);
        return $item; # Just use the link type
    },
);

my %custom_container_inits = (
    'df-flagarray' => sub {
        if (defined $cur_init_value) {
            add_simple_init $cur_init_value;
        } elsif (my $ienum = $_->getAttribute('index-enum')) {
            add_simple_init "ENUM_LAST_ITEM($ienum)";
        }
    },
);

sub emit_typedef($$) {
    # Convert a prefix/postfix pair into a single name
    my ($pre, $post) = @_;
    my $name = ensure_name undef;
    emit 'typedef ', $pre, ' ', $name, $post, ';';
    return $name;
}

sub get_struct_fields($) {
    return $_[0]->findnodes('ld:field');
}

sub find_subfield($$) {
    my ($tag, $name) = @_;
    return undef unless $name;
    my $level = $tag->getAttribute('ld:level');
    return undef unless defined $level;
    my $cond = 'descendant::ld:field[@ld:level='.($level+1).' and @name=\''.$name.'\']';
    my @items = $tag->findnodes($cond);
    return wantarray ? @items : $items[0];
}

sub get_struct_field_type($;%) {
    # Dispatch on the tag name, and retrieve the type prefix & suffix
    my ($tag, %flags) = @_;
    my $meta = $tag->getAttribute('ld:meta');
    my $subtype = $tag->getAttribute('ld:subtype');
    my $prefix;
    my $suffix = '';
    my $type_def = undef;

    local %container_flags = %flags;
    delete $container_flags{-weak};
    delete $container_flags{-void};

    if ($prefix = $tag->getAttribute('ld:typedef-name')) {
        $prefix = fully_qualified_name($tag,$prefix) unless $flags{-local};
        $type_def = $tag;
    } elsif ($meta eq 'number') {
        $prefix = primitive_type_name($subtype);
    } elsif ($meta eq 'bytes') {
        if ($flags{-local} && !$flags{-weak}) {
            if ($subtype eq 'static-string') {
                my $count = $tag->getAttribute('size') || 0;
                $prefix = "char";
                $suffix = "[$count]";
            } elsif ($subtype eq 'padding') {
                my $count = $tag->getAttribute('size') || 0;
                my $alignment = $tag->getAttribute('alignment') || 1;
                $prefix = $atable{$alignment} or die "Invalid alignment: $alignment\n";
                ($count % $alignment) == 0 or die "Invalid size & alignment: $count $alignment\n";
                $suffix = "[".($count/$alignment)."]";
            } else {
                die "Invalid bytes subtype: $subtype\n";
            }
        } else {
            $prefix = primitive_type_name($subtype);
        }
    } elsif ($meta eq 'global') {
        my $tname = $tag->getAttribute('type-name')
            or die "Global field without type-name";
        $type_def = register_ref($tname, !$flags{-weak});
        $prefix = $main_namespace.'::'.$tname;
    } elsif ($meta eq 'compound') {
        die "Unnamed compound in global mode: ".$tag->toString."\n" unless $flags{-local};

        $prefix = ensure_name undef;
        $tag->setAttribute('ld:typedef-name', $prefix) if $in_struct_body;
        $type_def = $tag;

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
                emit_struct_fields($tag, $prefix);
            } $tag, $prefix;
        }
    } elsif ($meta eq 'pointer') {
        $prefix = get_container_item_type($tag, -weak => 1, -void => 'void')."*";
    } elsif ($meta eq 'static-array') {
        ($prefix, $suffix) = get_container_item_type($tag);
        my $count = $tag->getAttribute('count') || 0;
        $suffix = "[$count]".$suffix;
    } elsif ($meta eq 'primitive') {
        local $_ = $tag;
        my $handler = $custom_primitive_handlers{$subtype} or die "Invalid primitive: $subtype\n";
        $prefix = $handler->($tag, %flags);
    } elsif ($meta eq 'container') {
        local $_ = $tag;
        my $handler = $custom_container_handlers{$subtype} or die "Invalid container: $subtype\n";
        $prefix = $handler->($tag, %flags);
    } elsif (!$flags{-local} && $tag->nodeName eq 'ld:global-type') {
        my $tname = $tag->getAttribute('type-name');
        $prefix = $main_namespace.'::'.$tname;
    } else {
        die "Invalid field meta type: $meta\n";
    }

    if ($subtype && ($flags{-local} || $flags{-fulltype}) && $subtype eq 'enum') {
        $type_def or die "Enum without a type definition";
        my $def_base = get_primitive_base($type_def,'int32_t');
        my $base = get_primitive_base($tag, $def_base);

        unless ($base eq $def_base) {
            $tag->setAttribute('ld:enum-size-forced', 'true') if $in_struct_body;

            if ($flags{-rettype} || $flags{-funcarg} || $in_union_body) {
                $prefix = $base;
            } else {
                $prefix = "enum_field<$prefix,$base>";
            }
        }
    }

    return ($prefix,$suffix) if wantarray;
    if ($suffix) {
        $prefix = emit_typedef($prefix, $suffix);
        $tag->setAttribute('ld:typedef-name', $prefix) if $flags{-local} && $in_struct_body;
    }
    return $prefix;
}

sub render_struct_field($) {
    my ($tag) = @_;

    # Special case: anonymous compounds.
    if (is_attr_true($tag, 'ld:anon-compound'))
    {
        check_bad_attrs($tag);
        with_struct_block {
            render_struct_field($_) for get_struct_fields($tag);
        } $tag, undef, -no_anon => 1;
        return;
    }

    # Otherwise, create the name if necessary, and render
    my $field_name = $tag->getAttribute('name');
    my $name = ensure_name $field_name;
    $tag->setAttribute('ld:anon-name', $name) unless $field_name;
    with_anon {
        my ($prefix, $postfix) = get_struct_field_type($tag, -local => 1);
        emit_comment $tag;
        emit $prefix, ' ', $name, $postfix, ';', get_comment($tag);
    } "T_$name";
}

our @simple_inits;
our $in_union = 0;
our $up_init_field = '';

sub add_simple_init($) {
    my ($value) = @_;

    if ($up_init_field || $in_union) {
        emit "$cur_init_field = $value;";
    } else {
        push @simple_inits, "$cur_init_field($value)";
    }
}

sub render_field_init($) {
    my ($field) = @_;
    local $_;

    my $meta = $field->getAttribute('ld:meta');
    my $subtype = $field->getAttribute('ld:subtype');
    my $name = $field->getAttribute('name') || $field->getAttribute('ld:anon-name');

    local $up_init_field = $cur_init_field;
    local $cur_init_field = ($up_init_field && $name ? $up_init_field.'.'.$name : ($name||$up_init_field));

    my $is_struct = $meta eq 'compound' && !$subtype;
    my $is_union = ($is_struct && is_attr_true($field, 'is-union'));

    local $in_union = $in_union || $is_union;

    if (is_attr_true($field, 'ld:anon-compound') || ($in_union && $is_struct))
    {
        my @fields = $is_union ? $field->findnodes('ld:field[1]') : get_struct_fields($field);
        &render_field_init($_) for @fields;
        return;
    }

    return unless ($name || $up_init_field =~ /\]$/);

    local $cur_init_value = $field->getAttribute('init-value');

    if ($meta eq 'number' || $meta eq 'pointer') {
        unless (defined $cur_init_value) {
            my $signed_ref =
                !is_attr_true($field,'ld:unsigned') &&
                ($field->getAttribute('ref-target') || $field->getAttribute('refers-to'));
            $cur_init_value = ($signed_ref ? '-1' : 0);
        }
        add_simple_init $cur_init_value;
    } elsif ($meta eq 'bytes') {
        emit "memset($cur_init_field, 0, sizeof($cur_init_field));";
    } elsif ($meta eq 'global' || $meta eq 'compound') {
        return unless $subtype;

        if ($subtype eq 'bitfield' && defined $cur_init_value) {
            emit $cur_init_field, '.whole = ', $cur_init_value;
        } elsif ($subtype eq 'enum') {
            if ($meta eq 'global') {
                my $tname = $field->getAttribute('type-name');
                if (defined $cur_init_value) {
                    $cur_init_value = $main_namespace.'::enums::'.$tname.'::'.$cur_init_value;
                } else {
                    $cur_init_value = "ENUM_FIRST_ITEM($tname)";
                }
            } else {
                $cur_init_value = $field->findvalue('enum-item[1]/@name')
                    unless defined $cur_init_value;
            }
            add_simple_init $cur_init_value;
        }
    } elsif ($meta eq 'static-array') {
        my $idx = ensure_name undef;
        my $count = $field->getAttribute('count')||0;
        emit_block {
            local $cur_init_field = $cur_init_field."[$idx]";
            render_field_init($_) for $field->findnodes('ld:item');
        } "for (int $idx = 0; $idx < $count; $idx++) ", "", -auto => 1;
    } elsif ($meta eq 'primitive') {
        local $_ = $field;
        my $handler = $custom_primitive_inits{$subtype};
        $handler->($field, $cur_init_value) if $handler;
    } elsif ($meta eq 'container') {
        local $_ = $field;
        my $handler = $custom_container_inits{$subtype};
        $handler->($field, $cur_init_value) if $handler;
    }
}

our $full_typename;
our @field_defs;

sub type_idfun_reference($) {
    my $type = get_struct_field_type($_[0], -fulltype => 1);
    return "identity_traits<$type >::get()";
}

sub auto_identity_reference($) {
    my $iref = type_identity_reference($_[0], -allow_complex => 1);
    return $iref || type_idfun_reference($_[0]);
}

sub render_field_metadata_rec($$) {
    my ($field, $FLD) = @_;

    local $_;
    local %weak_refs;
    local %strong_refs;

    my $meta = $field->getAttribute('ld:meta');
    my $subtype = $field->getAttribute('ld:subtype');
    my $name = $field->getAttribute('name') || $field->getAttribute('ld:anon-name');

    if ($FLD eq 'GFLD') {
        $name = $field->parentNode()->getAttribute('name');
    }

    if (is_attr_true($field, 'ld:anon-compound'))
    {
        my @fields = get_struct_fields($field);
        &render_field_metadata_rec($_, $FLD) for @fields;
        return;
    }

    my $enum = 'NULL';
    if (my $xe = $field->getAttribute('index-enum')) {
        static_include_type $xe;
        $enum = type_identity_reference($types{$xe});
    }

    if ($meta eq 'number') {
        my $tname = primitive_type_name($subtype);
        push @field_defs, [ "${FLD}(PRIMITIVE, $name)", "TID($tname)" ];
    } elsif ($meta eq 'bytes') {
        if ($subtype eq 'static-string') {
            my $count = $field->getAttribute('size') || 0;
            push @field_defs, [ "${FLD}(STATIC_STRING, $name)", 'NULL', $count ];
        }
    } elsif ($meta eq 'global' || $meta eq 'compound') {
        if (is_attr_true($field, 'ld:enum-size-forced')) {
            push @field_defs, [ "${FLD}(PRIMITIVE, $name)", type_idfun_reference($field) ];
        } else {
            if ($meta eq 'global') {
                my $tname = $field->getAttribute('type-name');
                $field = $types{$tname} or die "Invalid type: $tname";
            }

            if ($subtype && $subtype eq 'enum') {
                push @field_defs, [ "${FLD}(PRIMITIVE, $name)", type_identity_reference($field) ];
            } else {
                push @field_defs, [ "${FLD}(SUBSTRUCT, $name)", type_identity_reference($field) ];
            }
        }
    } elsif ($meta eq 'pointer') {
        my @items = $field->findnodes('ld:item');
        my $count = is_attr_true($field, 'is-array') ? 1 : 0;

        push @field_defs, [ "${FLD}(POINTER, $name)", auto_identity_reference($items[0]), $count, $enum ];
    } elsif ($meta eq 'static-array') {
        my @items = $field->findnodes('ld:item');
        my $count = $field->getAttribute('count')||0;

        push @field_defs, [ "${FLD}(STATIC_ARRAY, $name)", auto_identity_reference($items[0]), $count, $enum ];
    } elsif ($meta eq 'primitive') {
        push @field_defs, [ "${FLD}(PRIMITIVE, $name)", type_idfun_reference($field) ];
    } elsif ($meta eq 'container') {
        my @items = $field->findnodes('ld:item');

        if ($subtype eq 'stl-vector' && @items &&
            $items[0]->getAttribute('ld:meta') eq 'pointer')
        {
            my @items2 = $items[0]->findnodes('ld:item');

            push @field_defs, [ "${FLD}(STL_VECTOR_PTR, $name)",
                                auto_identity_reference($items2[0]), 0, $enum ];
        } else {
            push @field_defs, [ "${FLD}(CONTAINER, $name)", type_idfun_reference($field), 0, $enum ];
        }
    }
}

sub render_field_metadata($$\@\%) {
    my ($tag, $full_name, $fields, $info) = @_;

    local $in_struct_body = 0;
    local $in_union_body = 0;

    local $_;
    local $full_typename = $full_name;

    my $FLD = ($full_name eq 'global' ? 'GFLD' : 'FLD');

    static_include_type $_ for keys %weak_refs;

    return generate_field_table {
        render_field_metadata_rec($_, $FLD) for @$fields;

        for my $vmtag (@{$info->{vmethods}||[]}) {
            my $name = $vmtag->getAttribute('name');
            push @field_defs, [ "METHOD(OBJ_METHOD, $name)" ] if $name;
        }
        for my $name (@{$info->{statics}||[]}) {
            push @field_defs, [ "METHOD(CLASS_METHOD, $name)" ];
        }
    } $full_name;
}

sub emit_struct_fields($$;%) {
    my ($tag, $name, %flags) = @_;

    $tag->setAttribute('ld:in-union','true') if $in_union_body;

    local $_;
    my @fields = get_struct_fields($tag);
    &render_struct_field($_) for @fields;

    my $full_name = fully_qualified_name($tag, $name, 1);
    my %info;

    if ($in_union_body) {
        my $traits_name = 'identity_traits<'.$full_name.'>';

        with_emit_traits {
            emit_block {
                emit "static struct_identity identity;";
                emit "static struct_identity *get() { return &identity; }";
            } "template<> struct ${export_prefix}$traits_name ", ";";
        };

        with_emit_static {
            my $ftable = render_field_metadata $tag, $full_name, @fields, %info;
            emit "struct_identity ${traits_name}::identity(",
                    "sizeof($full_name), &allocator_fn<${full_name}>, ",
                    type_identity_reference($tag,-parent => 1), ', ',
                    "\"$name\", NULL, $ftable);";
        } 'fields';

        return;
    }

    local $in_struct_body = 0;
    local $in_union_body = 0;

    my $want_ctor = 0;
    my $ctor_args = '';
    my $ctor_arg_init = '';

    my $is_global = $tag->nodeName eq 'ld:global-type';
    my $inherits = $flags{-inherits};
    my $original_name = $tag->getAttribute('original-name');

    if ($flags{-class}) {
        emit "static virtual_identity _identity;";
    } else {
        emit "static struct_identity _identity;";
    }

    with_emit_static {
        local @simple_inits;
        my @ctor_lines = with_emit {
            if ($flags{-class}) {
                $ctor_args = "virtual_identity *_id";
                $ctor_arg_init = " = &".$name."::_identity";
                push @simple_inits, "$flags{-inherits}(_id)" if $flags{-inherits};
                emit "_identity.adjust_vtable(this, _id);";
            }
            render_field_init($_) for @fields;
        };
        if (1) { #@simple_inits || @ctor_lines) {
            $want_ctor = 1;
            my $full_name = get_struct_field_type($tag);
            emit $full_name,'::',$name,"($ctor_args)";
            emit "  :  ", join(', ', @simple_inits) if @simple_inits;
            emit_block {
                emit $_ for @ctor_lines;
            };
        }
    } 'ctors';

    if ($want_ctor) {
        emit "$name($ctor_args$ctor_arg_init);";
    }

    %info = $flags{-addmethods}->($tag) if $flags{-addmethods};

    with_emit_static {
        my $ftable = render_field_metadata $tag, $full_name, @fields, %info;

        if ($flags{-class}) {
            my $alloc_fn = $info{nodtor} ? 'allocator_nodel_fn' : 'allocator_fn';

            emit "virtual_identity ${full_name}::_identity(",
                    "sizeof($full_name), &${alloc_fn}<${full_name}>, ",
                    "\"$name\",",
                    ($original_name ? "\"$original_name\"" : 'NULL'), ',',
                    ($inherits ? "&${inherits}::_identity" : 'NULL'), ',',
                    "$ftable);";
        } else {
            emit "struct_identity ${full_name}::_identity(",
                    "sizeof($full_name), &allocator_fn<${full_name}>, ",
                    type_identity_reference($tag,-parent => 1), ', ',
                    "\"$name\",",
                    ($inherits ? "&${inherits}::_identity" : 'NULL'), ',',
                    "$ftable);";
        }
    } 'fields';
}

1;
