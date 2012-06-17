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

sub compute_md5($) {
    my ($fn) = @_;

    my $md5_hash = `md5sum "$fn"`;
    $md5_hash =~ s/\s.*$//s;
    $md5_hash =~ /^[0-9a-fA-F]+$/ or die "Could not determine md5 hash: $fn\n";
    return lc $md5_hash;
}

my $md5_hash = compute_md5 "$path/df_linux/libs/Dwarf_Fortress";
my $osx_md5 = compute_md5 "$path/df_osx/dwarfort.exe";

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

$timestamp = lc $timestamp;

print "New version $version, timestamp $timestamp, hash $md5_hash, osx hash $osx_md5\n";

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

# Generate next ids and vtables

system "./make-scans.sh '$path'";

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

sub import_genfile($$$;$) {
    my ($dir, $fn, $pfix, $substl) = @_;
    local *IFH;
    local $_;
    local @lines;
    if (open IFH, "${dir}/${fn}.txt") {
        @lines = <IFH>;
        close IFH;
    }
    if (@lines) {
        print FH "\n" unless defined $substl;
        print FH "$pfix$_" for @lines;
    } else {
        print FH $substl if defined $substl;
    }
}

open FH, '>symbols.xml' or die "Can't write symbols.\n";
for $_ (@symlines) {
    if (/<!--\s*end windows\s*-->/) {
        for my $line (@template) {
            next if $line =~ /<md5-hash/;
            if ($line =~ /<symbol-table name/) {
                print FH "    <symbol-table name='$version SDL' os-type='windows'>\n";
            } elsif ($line =~ /<binary-timestamp/) {
                print FH "        <binary-timestamp value='0x$timestamp'/>\n";
            } elsif ($line =~ /^(\s*)generated (\S+)\s*$/) {
                print FH $line;
                import_genfile 'windows', $2, $1;
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
            } elsif ($line =~ /^(\s*)generated (\S+)\s*$/) {
                print FH $line;
                import_genfile 'linux', $2, $1;
            } else {
                print FH $line;
            }
        }
        print FH "\n";
    } elsif (/<!--\s*end osx\s*-->/) {
        for my $line (@template) {
            next if $line =~ /<binary-timestamp/;
            if ($line =~ /<symbol-table name/) {
                print FH "    <symbol-table name='$version osx' os-type='darwin'>\n";
            } elsif ($line =~ /<md5-hash/) {
                print FH "        <md5-hash value='$osx_md5'/>\n";
            } elsif ($line =~ /^(\s*)generated (\S+)\s*$/) {
                print FH $line;
                import_genfile 'osx', $2, $1;
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

sub copy_globals($) {
    my ($dir) = @_;

    local $_;

    rename "$dir/df.globals.xml", "$dir/df.globals.xml-old";

    open IN, 'defs.xml-empty';
    open OUT, ">$dir/df.globals.xml";
    while (<IN>) {
        if (/(\s*)<!-- defs -->/) {
            import_genfile $dir, 'ctors', $1, $_;
        } else {
            print OUT $_;
        }
    }
    close IN;
    close OUT;
}

copy_globals 'linux';
copy_globals 'windows';
copy_globals 'osx';

system "touch '$version.lst'";

# Lisp

open FH, '>version.lisp' or die "Can't write version.lisp.\n";
print FH <<END;
(defparameter *df-version-str* "$version")
(defparameter *windows-timestamp* #x$timestamp)
(defparameter *linux-hash* "$md5_hash")
(defparameter *osx-hash* "$osx_md5")
END
close FH;
