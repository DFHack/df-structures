package StructType;

use utf8;
use strict;
use warnings;

BEGIN {
    use Exporter  ();
    our $VERSION = 1.00;
    our @ISA     = qw(Exporter);
    our @EXPORT  = qw(
        &preprocess_struct_type
        &render_struct_type
    );
    our %EXPORT_TAGS = ( ); # eg: TAG => [ qw!name1 name2! ],
    our @EXPORT_OK   = qw( );
}

END { }

use XML::LibXML;

use Common;
use StructFields;

# track objects that contain members with vtables
# map of: type name -> @{names of classes containing type}
my %field_backrefs;

# MISC

sub translate_lookup($) {
    my ($str) = @_;
    return undef unless $str && $str =~ /^\$global((\.[_a-zA-Z0-9]+)+)$/;
    my @fields = split /\./, substr($1,1);
    my $expr = "df::global::".shift(@fields);
    for my $fn (@fields) {
        $expr = "_fieldptr($expr, $fn)";
    }
    return $expr;
}

sub emit_find_instance(\%$) {
    my ($rinfo, $tag) = @_;

    my $keyfield = $tag->getAttribute('key-field');
    my $keyfield_tag = find_subfield $tag, $keyfield;
    my $keytype = 'int';
    my $vectype = "std::vector<$typename*>";

    if ($keyfield) {
        die "Could not find field $keyfield in $typename\n" unless $keyfield_tag;
        my $type = get_struct_field_type($keyfield_tag);
        $keytype = 'key_field_type';

        emit "typedef $typename* key_pointer_type;";
        emit "typedef $type key_field_type;";

        emit "static int binsearch_index(const $vectype &vec, key_field_type key, bool exact = true);";
        emit "static int binsearch_index(const $vectype &vec, key_pointer_type key, bool exact = true);";

        with_emit_static {
            emit_block {
                emit "return ::binsearch_index(vec, &${typename}::$keyfield, key, exact);";
            } "int ${typename}::binsearch_index(const $vectype &vec, key_field_type key, bool exact) ";
            emit_block {
                emit "return binsearch_index(vec, key->$keyfield, exact);";
            } "int ${typename}::binsearch_index(const $vectype &vec, key_pointer_type key, bool exact) ";
        };
    }

    my $instance_vector = translate_lookup $tag->getAttribute('instance-vector');
    if ($instance_vector) {
        emit "static $vectype &get_vector();";
        emit "static $vectype *get_vector_ptr();";
        emit "static $typename *find($keytype id);";

        with_emit_static {
            emit_block {
                emit "return *get_vector_ptr();";
            } "$vectype& ${typename}::get_vector() ";

            emit_block {
                emit "return $instance_vector;";
            } "$vectype* ${typename}::get_vector_ptr() ";

            emit_block {
                emit "std::vector<$typename*> *vec_ = get_vector_ptr();";
                emit "if (!vec_) return NULL;";

                if ($keyfield) {
                    emit "return binsearch_in_vector(*vec_, id_);";
                } else {
                    emit "return (size_t(id_) < vec_->size()) ? (*vec_)[id_] : NULL;";
                }
            } "$typename *${typename}::find($keytype id_) ";
        };

        push @{$rinfo->{statics}}, {'get_vector' => 'get_vector_ptr'};
        push @{$rinfo->{statics}}, 'find';
    }
}

sub render_virtual_methods {
    my ($tag) = @_;

    # Collect all parent classes
    my @parents = ( $tag );
    for (;;) {
        my $inherits = $parents[0]->getAttribute('inherits-from') or last;
        my $parent = $types{$inherits} || die "Unknown parent: $inherits\n";
        unshift @parents, $parent;
    }

    # Build the vtable array
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
                die "Duplicate virtual method: $name in ".$type->getAttribute('type-name')."\n"
                    if exists $name_index{$name};
                $name_index{$name} = scalar(@vtable);
            }
            push @vtable, $method;
        }
    }

    my $dtor_idx = $name_index{$dtor_id};
    my $no_dtor = 0;

    # If the vtable is empty, conjure up a destructor
    unless (@vtable) {
        $no_dtor = 1;
        push @vtable, undef;
        $dtor_idx = $#vtable;
    }

    my @our_vmethods;

    # Generate the methods
    my $min_id = $starts[-1];
    my $cur_mode = '';
    for (my $idx = $min_id; $idx <= $#vtable; $idx++) {
        my $method = $vtable[$idx];
        my $is_destructor = 1;
        my $name = $typename;
        my $is_anon = 1;

        if ($method) {
            $is_destructor = is_attr_true($method, 'is-destructor');
            $name = $method->getAttribute('name') unless $is_destructor;
            $is_anon = 0 if $name;
        }

        push @our_vmethods, $method unless ($is_anon || $is_destructor);

        my $rq_mode = $is_anon ? 'protected' : 'public';
        unless ($rq_mode eq $cur_mode) {
            $cur_mode = $rq_mode;
            outdent { emit "$cur_mode:"; }
        }

        with_anon {
            $name = ensure_name $name;
            $method->setAttribute('ld:anon-name', $name) if $method && $is_anon;

            my @ret_type = $is_destructor ? () : $method->findnodes('ret-type');
            my @arg_types = $is_destructor ? () : $method->findnodes('ld:field');
            my $ret_type = $ret_type[0] ? get_struct_field_type($ret_type[0], -local => 1, -rettype => 1) : 'void';
            my @arg_strs = map { scalar get_struct_field_type($_, -local => 1, -funcarg => 1) } @arg_types;

            my $ret_stmt = '';
            unless ($ret_type eq 'void') {
                $ret_stmt = ' return '.($ret_type =~ /\*$/ ? '0' : "$ret_type()").';';
            }

            emit_comment $method;
            emit 'virtual ', ($is_destructor?'~':$ret_type.' '), $name,
                 '(', join(', ', @arg_strs), ') {', $ret_stmt, ' /*', $idx, '*/ };',
                 get_comment($method);
        } "anon_vmethod_$idx";
    }

    return ($no_dtor, \@our_vmethods);
}

sub preprocess_struct_type {
    my ($tag) = @_;

    for my $field (get_struct_fields($tag)) {
        if ($field->getAttribute('ld:meta') eq 'global') {
            my $field_type_name = $field->getAttribute('type-name');
            if (!$types{$field_type_name}) {
                die "unknown field type: ".$field->getAttribute('name').": $field_type_name\n";
            }
            my $field_type_meta = $types{$field_type_name}->getAttribute('ld:meta');
            if ($field_type_meta eq 'class-type') {
                push @{$field_backrefs{$field_type_name}}, $tag->getAttribute('type-name');
            }
        }
    }
}

sub render_struct_type {
    my ($tag) = @_;

    my $tag_name = $tag->getAttribute('ld:meta');
    my $is_class = ($tag_name eq 'class-type');
    my $is_linked_list = (($tag->getAttribute('ld:subtype') or '') eq 'df-linked-list-type');
    my $item_type = $tag->getAttribute('item-type');
    my $list_link_type = $tag->getAttribute('df-list-link-type');
    my $list_link_field = $tag->getAttribute('df-list-link-field');
    my $is_other_vectors = (($tag->getAttribute('ld:subtype') or '') eq 'df-other-vectors-type');
    my $index_enum = $tag->getAttribute('index-enum');
    my $custom_methods = is_attr_true($tag, 'custom-methods') || $tag->findnodes('custom-methods/cmethod');
    my $has_methods = $is_class || is_attr_true($tag, 'has-methods');
    my $inherits = $tag->getAttribute('inherits-from');
    my $original_name = $tag->getAttribute('original-name');
    my $ispec = '';

    for my $extra ($tag->findnodes('extra-include')) {
        my $tname = $extra->getAttribute('type-name');
        if ($tname) {
            register_ref $tname, 1;
        } else {
            header_ref $extra->getAttribute('filename');
        }
    }

    if ($inherits) {
        register_ref $inherits, 1;
        $ispec = ' : '.$inherits;
    } elsif ($is_class) {
        $ispec = ' : virtual_class';
    } elsif ($is_linked_list) {
        register_ref $item_type, 1;
        $ispec = ' : DfLinkedList<'.$typename.', '.$item_type.'>';
    } elsif ($is_other_vectors) {
        register_ref $item_type, 0;
        register_ref $index_enum, 1;
        $ispec = ' : DfOtherVectors<'.$typename.', '.$index_enum.', '.$item_type.'>';
    }

    if ($list_link_type) {
        register_ref $list_link_type, 0;
    }

    with_struct_block {
        my $vmethod_emit = sub {
            my %info;

            emit_find_instance(%info, $tag);

            if ($list_link_type) {
                emit $list_link_type," *dfhack_get_list_link();";
                emit "void dfhack_set_list_link(",$list_link_type," *);";
                with_emit_static {
                    emit_block {
                        if ($list_link_field) {
                            emit "return ",$list_link_field,";";
                        } else {
                            emit "return nullptr;";
                        }
                    } "$list_link_type *${typename}::dfhack_get_list_link()";
                    emit_block {
                        if ($list_link_field) {
                            emit "this->",$list_link_field," = l;";
                        }
                    } "void ${typename}::dfhack_set_list_link($list_link_type *l)";
                };
            }

            if ($has_methods || $custom_methods) {
                if ($custom_methods) {
                    local $indentation = 0;
                    emit '#include "custom/', $typename, '.methods.inc"';

                    my %name_index;
                    $info{cmethods} = [];
                    for my $method ($tag->findnodes('custom-methods/cmethod')) {
                        my $name = $method->getAttribute('name');
                        die "Custom method has no name in ".$typename."\n"
                            if not $name;
                        die "Duplicate custom method: $name in ".$typename."\n"
                            if exists $name_index{$name};
                        $name_index{$name} = 1;
                        push @{$info{cmethods}}, $method;
                    }
                }

                if ($is_class) {
                    my ($no_dtor, $vmethods) = render_virtual_methods $tag;
                    $info{nodtor} = $no_dtor;
                    $info{vmethods} = $vmethods;
                } elsif (!$custom_methods) {
                    emit "~",$typename,"() {}";
                }
            }

            return %info;
        };

        emit_struct_fields($tag, $typename, -class => $is_class, -inherits => $inherits,
                            -addmethods => $vmethod_emit);

        if ($field_backrefs{$typename}) {
            for my $backref (@{$field_backrefs{$typename}}) {
                register_ref $backref;
                emit 'friend struct ' . fully_qualified_name($types{$backref}, $backref) . ';';
            }
        }
    } $tag, "$typename$ispec", -export => 1;
}

1;
