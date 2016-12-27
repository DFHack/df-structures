package Bitfield;

use utf8;
use strict;
use warnings;

BEGIN {
    use Exporter  ();
    our $VERSION = 1.00;
    our @ISA     = qw(Exporter);
    our @EXPORT  = qw( &render_bitfield_core &render_bitfield_type );
    our %EXPORT_TAGS = ( ); # eg: TAG => [ qw!name1 name2! ],
    our @EXPORT_OK   = qw( );
}

END { }

use XML::LibXML;

use Common;

sub render_bitfield_core {
    my ($name, $tag) = @_;

    my $bitfield_name = $name;
    my $base = get_primitive_base($tag);
    my @fields = $tag->findnodes('child::ld:field');

    emit_comment $tag, -attr => 1;

    emit_block {
        emit $base, ' whole;';

        my $idx = 0;
        my @idlist;

        emit_block {
            for my $item (@fields) {
                ($item->getAttribute('ld:meta') eq 'number' &&
                    $item->getAttribute('ld:subtype') eq 'flag-bit')
                    or die "Invalid bitfield member: ".$item->toString."\n";

                check_bad_attrs($item);
                my $name = ensure_name $item->getAttribute('name');
                my $size = $item->getAttribute('count') || 1;

                my $fbase = $base;
                my ($etn, $ettag) = decode_type_name_ref($item, -force_type => 'enum-type');

                if ($etn) {
                    my $ebase = get_primitive_base($ettag, 'int32_t');
                    unless ($ebase eq $base) {
                        die "Bitfield item $name of $bitfield_name has a different base type: $ebase.\n";
                    }
                    $fbase = $etn;
                }

                push @idlist, [ $name, $idx, sprintf('0x%xU',((1<<$size)-1)<<$idx), ',' ];
                $idx += $size;

                emit_comment $item;
                emit $fbase, " ", $name, " : ", $size, ";", get_comment($item);
            }
        } "struct ", " bits;";

        $idlist[-1][3] = '';

        emit_block {
            for my $r (@idlist) {
                emit "shift_", $r->[0], " = ", $r->[1], $r->[3];
            }
        } "enum Shift ", ";";

        emit_block {
            for my $r (@idlist) {
                emit "mask_", $r->[0], " = ", $r->[2], $r->[3];
            }
        } "enum Mask : $base ", ";";

        emit $name, "($base whole_ = 0) : whole(whole_) {};";
    } "union $name ", ";";

    my $full_name = fully_qualified_name($tag, $name, 1);
    my $traits_name = 'traits<'.$full_name.'>';

    with_emit_traits {
        emit_block {
            emit "typedef $base base_type;";
            emit "typedef $full_name bitfield_type;";
            emit "static const int bit_count = sizeof(base_type)*8;";
            emit "static const bitfield_item_info bits[bit_count];";
        } "template<> struct ${export_prefix}bitfield_$traits_name ", ";";
        emit_block {
            emit "static bitfield_identity identity;";
            emit "static bitfield_identity *get() { return &identity; }";
        } "template<> struct ${export_prefix}identity_$traits_name ", ";";
    };

    with_emit_static {
        emit_block {
            for my $item (@fields) {
                my $name = $item->getAttribute('name');
                my $size = $item->getAttribute('count') || 1;
                emit "{ ", ($name?'"'.$name.'"':'NULL'), ", ", $size, " },";
                for (my $j = 1; $j < $size; $j++) {
                    emit "{ NULL, ", -$j, " },";
                }
            }

            $lines[-1] =~ s/,$//;
        } "const bitfield_item_info bitfield_${traits_name}::bits[bit_count] = ", ";";

        emit "bitfield_identity identity_${traits_name}::identity(",
             "sizeof($full_name), ",
             type_identity_reference($tag,-parent => 1), ', ',
             "\"$name\", bitfield_${traits_name}::bit_count, bitfield_${traits_name}::bits);";
    } 'enums';
}

sub render_bitfield_type {
    my ($tag) = @_;
    render_bitfield_core($typename,$tag);
}

1;
