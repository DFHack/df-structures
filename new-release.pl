#!/usr/bin/perl
#
# A script for starting a new release.
#
# Usage:
#
#   new-release.pl v0.??.?? [path]
#
# DF should be unpacked into df_linux and df_windows
# subdirs below the given path.
#

use strict;
use warnings;

my $version = $ARGV[0] or die "Usage: new-release.pl <version> [path]\n";
my $path = $ARGV[1] || glob("~/Games/DF/");

# Find the new md5 and timestamp

my $md5_hash = `md5sum "$path/df_linux/libs/Dwarf_Fortress"`;
$md5_hash =~ s/\s.*$//s;
$md5_hash =~ /^[0-9a-fA-F]+$/ or die "Could not determine md5 hash\n";

my $timestamp;
if (open FH, "winedump '$path/df_windows/Dwarf Fortress.exe'|") {
    while (<FH>) {
        next unless /TimeDateStamp:\s+([0-9A-F]+)\s/;
        $timestamp = $1;
        last;
    }
    close FH;
}
$timestamp or die "Could not find the timestamp.\n";

$md5_hash = lc $md5_hash;
$timestamp = lc $timestamp;

print "New version $version, timestamp $timestamp hash $md5_hash\n";

# Load symbols

open FH, 'symbols.xml' or die "Can't open symbols.\n";
my @symlines = <FH>;
close FH;

for $_ (@symlines) {
    if (/<md5-hash\s+value=[\'\"]([^\'\"]+)[\'\"]\s*\/>/) {
        my $v = lc $1;
        if ($v eq $md5_hash) {
            die "This md5 hash is already in symbols.xml\n";
        }
    } elsif (/<binary-timestamp\s+value=[\'\"]0x([^\'\"]+)[\'\"]\s*\/>/) {
        my $v = lc $1;
        if ($v eq $timestamp) {
            die "This timestamp is already in symbols.xml\n";
        }
    }
}

# Patch symbols

my @template;
my $in = 0;
for $_ (@symlines) {
    if (/^\s+<!--\s*$/) {
        $in = 1;
    } elsif (/^\s+-->\s*$/) {
        last;
    } elsif ($in) {
        push @template, $_;
    }
}
@template or die "Could not find the symtable template\n";

open FH, '>symbols.xml' or die "Can't write symbols.\n";
for $_ (@symlines) {
    if (/<!--\s*end windows\s*-->/) {
        for my $line (@template) {
            next if $line =~ /<md5-hash/;
            if ($line =~ /<symbol-table name/) {
                print FH "    <symbol-table name='$version SDL' os-type='windows'>\n";
            } elsif ($line =~ /<binary-timestamp/) {
                print FH "        <binary-timestamp value='0x$timestamp'/>\n";
            } else {
                print FH $line;
            }
        }
        print FH "\n";
    } elsif (/<!--\s*end linux\s*-->/) {
        for my $line (@template) {
            next if $line =~ /<binary-timestamp/;
            if ($line =~ /<symbol-table name/) {
                print FH "    <symbol-table name='$version linux' os-type='linux'>\n";
            } elsif ($line =~ /<md5-hash/) {
                print FH "        <md5-hash value='$md5_hash'/>\n";
            } else {
                print FH $line;
            }
        }
        print FH "\n";
    }

    print FH $_;
}
close FH;

# Globals

rename "linux/df.globals.xml", "linux/df.globals.xml-old";
rename "windows/df.globals.xml", "windows/df.globals.xml-old";
system "cp defs.xml-empty linux/df.globals.xml";
system "cp defs.xml-empty windows/df.globals.xml";
system "touch '$version.lst'";

# Lisp

open FH, 'start.lisp' or die "Can't open start.lisp.\n";
my @startlines = <FH>;
close FH;

open FH, '>start.lisp' or die "Can't write start.lisp.\n";
for $_ (@startlines) {
    if (/^(\s*\(open-annotations\s+\")[^\"]+(\"\s*\)\s*)$/) {
        $_ = $1."$version.lst".$2;
    } elsif (/^(\s*\(defparameter\s+\*windows-timestamp\*\s+\#x)[0-9a-fA-F]+(\s*\)\s*)$/) {
        $_ = $1.$timestamp.$2;
    } elsif (/^(\s*\(defparameter\s+\*linux-hash\*\s+\")[0-9a-fA-F]+(\"\s*\)\s*)$/) {
        $_ = $1.$md5_hash.$2;
    }
    print FH $_;
}
close FH;
