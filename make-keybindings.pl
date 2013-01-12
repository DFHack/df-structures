#!/usr/bin/perl

use strict;
use warnings;

print <<'__END__';
<data-definition>
    WARNING: THIS FILE IS AUTO-GENERATED - DO NOT EDIT

    <enum-type type-name='interface_key' base-type='int32_t'>
__END__

my $startname;
my $after_start;

while(<>) {
    my ($name, $val);

    if ($after_start) {
        $after_start = 0;

        if (/^\s+([A-Z_0-9]+)=$startname,\s*$/) {
            $name = $1;
        } else {
            print "        <enum-item name='$startname'/>\n";
            redo;
        }
    } else {
        next unless /^\s+([A-Z_0-9]+)(?:=(\d+))?,\s*$/;
        ($name, $val) = ($1, $2);
    }

    if ($name =~ /_START$/) {
        $startname = $name;
        $after_start = 1;
        next;
    }

    last if $name eq 'INTERFACEKEYNUM';
    $name =~ s/^INTERFACEKEY_//;
    print "        <enum-item name='$name'";
    print " value='$val'" if defined $val;
    print "/>\n";
}

print <<'__END__';
    </enum-type>
</data-definition>

<!--
Local Variables:
indent-tabs-mode: nil
nxml-child-indent: 4
End:
-->
__END__
