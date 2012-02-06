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

    return ($base, $count);
}

sub render_enum_tables($$$$) {
    my ($name,$tag,$base,$count) = @_;

    my $base_type = get_primitive_base($tag, 'int32_t');

    # Enumerate enum attributes

    my %aidx = ('key' => 0);
    my @anames = ('key');
    my @avals = ('NULL');
    my @atypes = ('const char*');
    my @atnames = (undef);
    my @aprefix = ('');
    my @is_list = (undef);

    my @use_key = (0);
    my @use_list = ();

    for my $attr ($tag->findnodes('child::enum-attr')) {
        my $name = $attr->getAttribute('name') or die "Unnamed enum-attr.\n";
        my $type = decode_type_name_ref $attr;
        my $def = $attr->getAttribute('default-value');

        my $base_tname = ($type && $type =~ /::(.*)$/ ? $1 : '');
        if ($base_tname eq $typename) {
            $type = $base_tname;
            $base_tname = '';
        }

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
        
        if (is_attr_true($attr, 'is-list')) {
            push @use_list, $#anames;
            $is_list[-1] = $atypes[-1];
            $atypes[-1] = "enum_list_attr<$atypes[-1]>";
            $avals[-1] = $atypes[-1].'()';
        } elsif (is_attr_true($attr, 'use-key-name')) {
            push @use_key, $#anames;
        }
    }

    # Emit accessor function prototypes

    emit "const $name _first_item_of_$name = ($name)$base;";
    emit "const $name _last_item_of_$name = ($name)", ($base+$count-1), ";";

    emit_block {
        # Cast the enum to integer in order to avoid GCC assuming the value range is correct.
        emit "$base_type ivalue = ($base_type)value;";
        emit "return (ivalue >= $base && ivalue <= ",($base+$count-1),");";
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

                my $list_entry_id;
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
                        my $value = $attr->getAttribute('value') or die "No-value item-attr.\n";
                        my $idx = $aidx{$name} or die "Unknown item-attr: $name\n";

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

                # Emit the info table
                emit_block {
                    emit $_ for @table_entries;
                    $lines[-1] =~ s/,$//;
                } "static const _info_entry _info[] = ", ";";

                for (my $i = 0; $i < @anames; $i++) {
                    emit_block {
                        emit "return is_valid(value) ? _info[value - $base].$anames[$i] : $avals[$i];";
                    } "$atypes[$i] get_$anames[$i]($name value) ";
                }
            } "namespace $name ";
        } "namespace enums ";
    } 'enums';
}

sub render_enum_type {
    my ($tag) = @_;

    emit_block {
        emit_block {
            my ($base,$count) = render_enum_core($typename,$tag);

            if (defined $base) {
                render_enum_tables($typename,$tag,$base,$count);
            } else {
                print STDERR "Warning: complex enum: $typename\n";
            }
        } "namespace $typename ";
    } "namespace enums ";

    emit "using enums::",$typename,"::",$typename,";";
}

1;
