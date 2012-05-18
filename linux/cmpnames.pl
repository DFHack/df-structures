#!/usr/bin/perl

use strict;
use warnings;

open FH, 'df.globals.xml' or die "Can't open globals\n";
my @globals = <FH>;
close FH;

open FH, 'df.globals.xml-old' or die "Can't open old globals\n";
my @old_globals = <FH>;
close FH;

my @names;
my @sizes;
for my $line (@old_globals) {
    next unless $line =~ /\<global-object name=\"([^\"]+)\" offset=\"(0x[0-9a-f]+)\" size=\"([0-9]+)\"/;
    my ($name, $size) = ($1, $3);
    $name = undef if $name =~ /^obj_/;
    push @names, $name;
    push @sizes, $size;
}

my $gidx = 0;
my $save = 0;
for (my $i = 0; $i <= $#globals; $i++) {
    $globals[$i] =~ /\<global-object name=\"([^\"]+)\" offset=\"(0x[0-9a-f]+)\" size=\"([0-9]+)\"/
        or next;
    my ($cname,$off,$csize) = ($1,$2,$3);
    die "Global count mismatch.\n" if $gidx > $#names;
    my $name = $names[$gidx];
    my $size = $sizes[$gidx];
    $gidx++;
    next unless $name;
    if ($size != $csize) {
        print "Size mismatch: $csize vs $size\n";
        die "Mismatch too big\n"
            unless abs(($size-$csize)/$size) < 0.1;
    }
    unless ($name eq $cname) {
        die "Name mismatch: $cname vs $name\n" unless $cname =~ /^obj_/;
        $globals[$i] =~ s/name=\"[^\"]+\"/name=\"$name\"/;
        $save++;
    }
    print "<global-address name='$name' value='$off'/>\n";
}
die "Global count mismatch.\n" if $gidx != @names;

if ($save) {
    open FH, '>df.globals.xml' or die "Can't open globals for write\n";
    print FH @globals;
    close FH;
}
