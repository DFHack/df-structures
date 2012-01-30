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

    my $mod = ($tag->nodeName eq 'ld:global-type' ? "$export_prefix extern" : 'static');
    emit "$mod const bitfield_item_info ${name}_items_[sizeof($base)*8];";

    emit_comment $tag, -attr => 1;

    emit_block {
        emit $base, ' whole;';

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

                emit_comment $item;
                emit $fbase, " ", $name, " : ", $size, ";", get_comment($item);
            }
        } "struct ", " bits;";

        emit $name, "($base whole_ = 0) : whole(whole_) {};";

        emit "const bitfield_item_info *get_items() const { return ${name}_items_; }";
    } "union $name ", ";";

    with_emit_static {
        my $fname = fully_qualified_name($tag, $name.'_items_', 1);
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
        } "const bitfield_item_info ".$fname."[sizeof($base)*8] = ", ";";
    } 'enums';
}

sub render_bitfield_type {
    my ($tag) = @_;
    render_bitfield_core($typename,$tag);
}

1;
