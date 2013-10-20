#!/usr/bin/perl

use strict;
use warnings;

my %vtable_size;
my %vtable_names;
my %is_class;
my %superclass;

open N, "all.csv" or die "Cannot open all.csv\n";
while (<N>) {
    next unless /^\"([^\"]*)\",\"(\d+)\",\"0x([0-9a-f]+)\",\"0x([0-9a-f]+)\",\"([^\"]*)\",\"([^\"]*)\",\"([^\"]*)\"/i;
    my ($top, $level, $addr, $size, $type, $name, $target) = ($1,$2,hex $3,hex $4,$5,$6,$7);
    if ($type eq 'class-type') {
        $is_class{$top} = $top;
    } elsif ($is_class{$top} && $level == 1 && $addr == 0 &&
                $name eq '' && $type ne 'pointer') {
        $superclass{$top} = $type;
    } elsif ($type eq 'vmethod' && $top =~ /^(.*)::vtable$/) {
        my $class = $1;
        $size /= 4;
        $addr /= 4;
        for (my $i = 0; $i < $size; $i++) {
            $vtable_names{$class}[$addr+$i] = $name . ($i > 0 ? "[$i]" : '');
        }
        $vtable_size{$class} = $addr+$size;
    }
}
close N;

my %vtable_addrs;
my %vmethod_addrs;
my %addr_names;

open N, "vtables-ext.txt" or die "Cannot open vtables-ext.txt\n";
my $cur_class;
while (<N>) {
    if (/<vtable-address name='(\S+)' value='0x([0-9a-f]+)'>/) {
        $cur_class = $1;
        $vtable_addrs{$1} = hex $2;
        $addr_names{hex $2} = $1.'::_vtable';
    } elsif (/<vtable-function index='\d+' addr='0x([0-9a-f]+)'/) {
        push @{$vmethod_addrs{$cur_class}}, hex $1;
    }
}
close N;

my %subst = (
    proj => 'projectile',
    task => 'adv_task',
);
my %rsubst = (
    projectile => 'projst',
    adv_task => 'taskst',
);

sub vtable_name($) {
    my ($class) = @_;
    return $class if $vtable_addrs{$class};
    return $rsubst{$class} if $vtable_addrs{$rsubst{$class}||''};
    return $class.'st' if $vtable_addrs{$class.'st'};
    return undef;
}

sub vtable_class($) {
    my ($class) = @_;
    while ($class && !$vtable_size{$class}) {
        $class = $superclass{$class};
    }
    return $class;
}

my %schecked;

my %processed;
for (;;) {
    my $found = 0;
    for my $class (sort keys %is_class) {
        next if $processed{$class};
        my $super = $superclass{$class};
        next if $super && !$processed{$super};
        $processed{$class} = 1;
        $found++;

        my $vtname = vtable_name $class or next;
        my $vmaddrs = $vmethod_addrs{$vtname} || [];
        my $num_vmaddrs = @$vmaddrs;

        my $vtclass = vtable_class $class or next;
        my $vmnames = $vtable_names{$vtclass} || [];
        my $num_vmnames = @$vmnames;

        $schecked{$vtclass}++ if $vtclass eq $class;

        if ($vtclass eq $class && $num_vmaddrs != $num_vmnames) {
            print STDERR "VTable size mismatch: $class ($vtname) - expected $num_vmnames, found $num_vmaddrs\n";
        }

        for (my $i = 0; $i < @$vmaddrs; $i++) {
            my $addr = $vmaddrs->[$i];
            next if $addr_names{$addr};
            my $name = $vmnames->[$i] || 'vmethod'.$i;
            $addr_names{$addr} = $class.'::'.$name;
        }
    }
    last unless $found;
}

for my $vt (keys %vtable_names) {
    print STDERR "VTable size unchecked: $vt\n" unless $schecked{$vt};
}

sub prefix_class($) {
    my ($name) = @_;
    for (;;) {
        return $name if $vtable_size{$subst{$name} || $name};
        return undef unless $name =~ s/_[^_]$//;
    }
}

for my $vtname (sort keys %vtable_addrs) {
    my $vmaddrs = $vmethod_addrs{$vtname};
    my $vtclass = prefix_class $vtname || '';
    my $vmnames = $vtable_names{$vtclass} || [];
    for (my $i = 0; $i < @$vmaddrs; $i++) {
        my $addr = $vmaddrs->[$i];
        next if $addr_names{$addr};
        my $name = $vmnames->[$i] || 'vmethod'.$i;
        $addr_names{$addr} = $vtname.'::'.$name;
    }
}

for my $addr (sort { $a <=> $b } keys %addr_names) {
    printf "%08x %s\n", $addr, $addr_names{$addr};
}
