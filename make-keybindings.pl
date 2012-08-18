#!/usr/bin/perl

use strict;
use warnings;

print <<'__END__';
<data-definition>
    <enum-type type-name='interface_key' base-type='int32_t'>
__END__

while(<>) {
    next unless /^\s+([A-Z_0-9]+)(?:=(\d+))?,\s*$/;
    my ($name, $val) = ($1, $2);
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
