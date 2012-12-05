package Enum;

use utf8;
use strict;
use warnings;

BEGIN {
    use Exporter  ();
    our $VERSION = 1.00;
    our @ISA     = qw(Exporter);
    our @EXPORT  = qw( &render_enum_core &render_enum_type );
    our %EXPORT_TAGS = ( ); # eg: TAG => [ qw!name1 name2! ],
    our @EXPORT_OK   = qw( );
}

END { }

use XML::LibXML;

use Common;

sub render_enum_tables($$$$);

sub render_enum_core($$) {
    my ($name,$tag) = @_;

    my $base = 0;
    my $count = 0;

    my $base_type = get_primitive_base($tag, 'int32_t');

    emit_comment $tag, -attr => 1;

    emit_block {
        my @items = $tag->findnodes('child::enum-item');

        for my $item (@items) {
            my $name = ensure_name $item->getAttribute('name');
            my $value = $item->getAttribute('value');

            $base = ($count == 0) ? $value : undef if defined $value;
            $count++;

            emit_comment $item, -attr => 1;
            emit $name, (defined($value) ? ' = '.$value : ''), ',';
        }

        $lines[-1] =~ s/,$//;
    } "enum $name : $base_type ", ";";

    render_enum_tables $name, $tag, $base, $count;

    return ($base, $count);
}

my $list_entry_id = 0;

sub render_enum_tables($$$$) {
    my ($name,$tag,$base,$count) = @_;

    my $is_global = $tag->nodeName eq 'ld:global-type';
    my $base_type = get_primitive_base($tag, 'int32_t');

    my $full_name = fully_qualified_name($tag, $name, 1);
    my $traits_name = 'traits<'.$full_name.'>';

    with_emit_traits {
        emit_block {
            emit "static enum_identity identity;";
            emit "static enum_identity *get() { return &identity; }";
        } "template<> struct ${export_prefix}identity_$traits_name ", ";";
    };

    unless (defined $base) {
        with_emit_static {
            emit "enum_identity identity_${traits_name}::identity(",
                    "sizeof($full_name), ",
                    type_identity_reference($tag,-parent => 1), ', ',
                    "\"$name\", TID($base_type), 0, -1, NULL, NULL, NULL);";
        } 'enums';
        return;
    }

    # Enumerate enum attributes

    my %aidx = ('key' => -1);
    my @anames = ();
    my @avals = ();
    my @atypes = ();
    my @atnames = ();
    my @aprefix = ();
    my @is_list = ();

    my @use_key = ();
    my @use_list = ();

    my @field_meta = ();

    for my $attr ($tag->findnodes('child::enum-attr')) {
        my $name = $attr->getAttribute('name') or die "Unnamed enum-attr.\n";
        my $type = decode_type_name_ref $attr;
        my $def = $attr->getAttribute('default-value');

        my $base_tname = ($type && $type =~ /::(.*)$/ ? $1 : '');

        die "Duplicate attribute $name.\n" if exists $aidx{$name};

        check_name $name;
        $aidx{$name} = scalar @anames;
        push @anames, $name;
        push @atnames, $type;
        push @is_list, undef;

        if ($type) {
            push @atypes, $type;
            push @aprefix, ($base_tname ? $base_tname."::" : '');
            push @avals, (defined $def ? $aprefix[-1].$def : "($type)0");
        } else {
            push @atypes, 'const char*';
            push @avals, (defined $def ? "\"$def\"" : 'NULL');
            push @aprefix, '';
        }

        push @field_meta, [ "FLD(PRIMITIVE, $name)", "identity_traits<$atypes[-1]>::get()" ];

        if (is_attr_true($attr, 'is-list')) {
            push @use_list, $#anames;
            $is_list[-1] = $atypes[-1];
            $atypes[-1] = "enum_list_attr<$atypes[-1]>";
            $avals[-1] = "{ 0, NULL }";
            $field_meta[-1] = [ "FLD(CONTAINER, $name)", "identity_traits<$atypes[-1]>::get()" ];
        } elsif (is_attr_true($attr, 'use-key-name')) {
            push @use_key, $#anames;
        }
    }

    # Emit traits

    with_emit_traits {
        emit_block {
            emit "typedef $base_type base_type;";
            emit "typedef $full_name enum_type;";
            emit "static const base_type first_item_value = $base;";
            emit "static const base_type last_item_value = ", ($base+$count-1), ";";
            emit_block {
                # Cast the enum to integer in order to avoid GCC assuming the value range is correct.
                emit "return (base_type(value) >= first_item_value && ",
                             "base_type(value) <= last_item_value);";
            } "static inline bool is_valid(enum_type value) ";
            emit "static const enum_type first_item = (enum_type)first_item_value;";
            emit "static const enum_type last_item = (enum_type)last_item_value;";
            emit "static const char *const key_table[", $count, "];";
            if (@anames) {
                emit_block {
                    for (my $i = 0; $i < @anames; $i++) {
                        emit "$atypes[$i] $anames[$i];";
                    }
                    emit "static struct_identity _identity;";
                } "struct attr_entry_type ", ";";
                emit "static const attr_entry_type attr_table[", $count, "+1];";
                emit "static const attr_entry_type &attrs(enum_type value);";
            }
        } "template<> struct ${export_prefix}enum_$traits_name ", ";";
    };

    # Emit implementation

    with_emit_static {
        # Emit keys

        emit_block {
            for my $item ($tag->findnodes('child::enum-item')) {
                if (my $name = $item->getAttribute('name')) {
                    emit '"'.$name.'",'
                } else {
                    emit 'NULL,';
                }
            }
            $lines[-1] =~ s/,$//;
        } "const char *const enum_${traits_name}::key_table[${count}] = ", ";";

        # Emit attrs

        my $atable_ptr = 'NULL';
        my $atable_meta = 'NULL';

        if (@anames) {
            my @table_entries;

            my $fmt_val = sub {
                my ($idx, $value) = @_;
                if ($atnames[$idx]) {
                    return $aprefix[$idx].$value;
                } else {
                    return "\"$value\"";
                }
            };

            for my $item ($tag->findnodes('child::enum-item')) {
                my $tag = $item->nodeName;

                # Assemble item-specific attr values
                my @evals = @avals;
                my $name = $item->getAttribute('name');
                if ($name) {
                    $evals[$_] = $fmt_val->($_, $name) for @use_key;
                }

                my @list;

                for my $attr ($item->findnodes('child::item-attr')) {
                    my $name = $attr->getAttribute('name') or die "Unnamed item-attr.\n";
                    my $value = $attr->getAttribute('value');
                    (defined $value) or die "No-value item-attr.\n";
                    my $idx = $aidx{$name};
                    (defined $idx && $idx >= 0) or die "Unknown item-attr: $name\n";

                    if ($is_list[$idx]) {
                        push @{$list[$idx]}, $fmt_val->($idx, $value);
                    } else {
                        $evals[$idx] = $fmt_val->($idx, $value);
                    }
                }

                for my $idx (@use_list) {
                    my @items = @{$list[$idx]||[]};
                    my $ptr = 'NULL';
                    if (@items) {
                        my $id = $list_entry_id++;
                        $ptr = "_list_items_${id}";
                        emit "static const $is_list[$idx] ${ptr}[] = { ", join(', ', @items), ' };';
                    }
                    $evals[$idx] = "{ ".scalar(@items).', '.$ptr.' }';
                }

                push @table_entries, "{ ".join(', ',@evals)." },";
            }

            my $entry_type = "enum_${traits_name}::attr_entry_type";

            # Emit the info table
            emit_block {
                emit $_ for @table_entries;
                emit "{ ", join(', ',@avals), " }";
            } "const $entry_type enum_${traits_name}::attr_table[${count}+1] = ", ";";

            emit_block {
                emit "return is_valid(value) ? attr_table[value - first_item_value] : attr_table[$count];";
            } "const $entry_type & enum_${traits_name}::attrs(enum_type value) ";

            # Emit the info table metadata
            with_emit_static {
                my $ftable = generate_field_table {
                    @field_defs = @field_meta;
                } $entry_type;

                emit "struct_identity ${entry_type}::_identity(",
                        "sizeof($entry_type), NULL, ",
                        type_identity_reference($tag), ', ',
                        "\"_attr_entry_type\", NULL, $ftable);";
            } 'fields';

            $atable_ptr = "enum_${traits_name}::attr_table";
            $atable_meta = "&${entry_type}::_identity";
        }

        emit "enum_identity identity_${traits_name}::identity(",
                "sizeof($full_name), ",
                type_identity_reference($tag,-parent => 1), ', ',
                "\"$name\", TID($base_type), $base, ",
                ($base+$count-1), ", enum_${traits_name}::key_table,
                $atable_ptr, $atable_meta);";
    } 'enums';
}

sub render_enum_type {
    my ($tag) = @_;

    emit_block {
        emit_block {
            my ($base,$count) = render_enum_core($typename,$tag);

            unless (defined $base) {
                print STDERR "Warning: complex enum: $typename\n";
            }
        } "namespace $typename ";
    } "namespace enums ";

    emit "using enums::",$typename,"::",$typename,";";
}

1;
