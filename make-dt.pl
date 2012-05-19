#!/usr/bin/perl

use strict;
use warnings;

my ($version, $timestamp, $hash);

open FH, 'version.lisp' or die "Cannot open version";
while (<FH>) {
    if (/df-version-str.*\"(.*)\"/) {
        $version = $1;
    } elsif (/windows-timestamp.*#x([0-9a-f]+)/) {
        $timestamp = $1;
    } elsif (/linux-hash.*\"(.*)\"/) {
        $hash = $1;
    }
}
close FH;

sub load_csv(\%$) {
    my ($rhash, $fname) = @_;

    open FH, $fname or die "Cannot open $fname";
    while (<FH>) {
        next unless /^\"([^\"]*)\",\"(\d+)\",\"(?:0x([0-9a-fA-F]+))?\",\"[^\"]*\",\"([^\"]*)\",\"([^\"]*)\",\"([^\"]*)\"/;
        my ($top, $level, $addr, $type, $name, $target) = ($1,$2,$3,$4,$5,$6);
        next if defined $rhash->{$top}{$name};
        $rhash->{$top}{$name} = ($type eq 'enum-item' ? $target : hex $addr);
    }
    close FH;
}

our $complete;

sub lookup_addr(\%$$;$) {
    my ($rhash, $top, $name, $bias) = @_;

    my $val = $rhash->{$top}{$name};
    unless (defined $val) {
        $complete = 0;
        return 0;
    }
    return $val + ($bias||0);
}

our @lines;

sub emit_header($) {
    my ($name) = @_;
    push @lines, '' if @lines;
    push @lines, "[$name]";
}

sub emit_addr($\%$$;$) {
    my ($name, $rhash, $top, $var, $bias) = @_;

    my $val = $rhash->{$top}{$var};
    if (defined $val) {
        $val += ($bias||0);
        if ($val < 0x10000) {
            push @lines, sprintf('%s=0x%04x', $name, $val);
        } else {
            push @lines, sprintf('%s=0x%08x', $name, $val);
        }
    } else {
        $complete = 0;
        push @lines, "$name=0x0";
    }
}

sub generate_dt_ini($$$) {
    my ($subdir, $checksum, $vbias) = @_;

    my %globals;
    load_csv %globals, "$subdir/globals.csv";
    my %all;
    load_csv %all, "$subdir/all.csv";

    local $complete = 1;
    local @lines;

    emit_header 'addresses';
    emit_addr 'translation_vector',%globals,'world','world.raws.language.translations',$vbias;
    emit_addr 'language_vector',%globals,'world','world.raws.language.words',$vbias;
    emit_addr 'creature_vector',%globals,'world','world.units.all',$vbias;
    emit_addr 'dwarf_race_index',%globals,'ui','ui.race_id';
    emit_addr 'squad_vector',%globals,'world','world.squads.all',$vbias;
    emit_addr 'current_year',%globals,'cur_year','cur_year';

    emit_header 'offsets';
    emit_addr 'word_table',%all,'language_translation','words';
    push @lines, 'string_buffer_offset=0x0000';

    emit_header 'word_offsets';
    emit_addr 'base',%all,'language_word','word';
    emit_addr 'noun_singular',%all,'language_word','forms[Noun]';
    emit_addr 'noun_plural',%all,'language_word','forms[NounPlural]';
    emit_addr 'adjective',%all,'language_word','forms[Adjective]';
    emit_addr 'verb',%all,'language_word','forms[Verb]';
    emit_addr 'present_simple_verb',%all,'language_word','forms[Verb3rdPerson]';
    emit_addr 'past_simple_verb',%all,'language_word','forms[VerbPast]';
    emit_addr 'past_participle_verb',%all,'language_word','forms[VerbPassive]';
    emit_addr 'present_participle_verb',%all,'language_word','forms[VerbGerund]';

    emit_header 'dwarf_offsets';
    emit_addr 'first_name',%all,'unit','name',lookup_addr(%all,'language_name','first_name');
    emit_addr 'nick_name',%all,'unit','name',lookup_addr(%all,'language_name','nickname');
    emit_addr 'last_name',%all,'unit','name',lookup_addr(%all,'language_name','words');
    emit_addr 'custom_profession',%all,'unit','custom_profession';
    emit_addr 'profession',%all,'unit','profession';
    emit_addr 'race',%all,'unit','race';
    emit_addr 'flags1',%all,'unit','flags1';
    emit_addr 'flags2',%all,'unit','flags2';
    emit_addr 'sex',%all,'unit','sex';
    emit_addr 'id',%all,'unit','id';
    emit_addr 'recheck_equipment',%all,'unit','military.pickup_flags';
    emit_addr 'current_job',%all,'unit','job.current_job';
    emit_addr 'physical_attrs',%all,'unit','body.physical_attrs';
    emit_addr 'turn_count',%all,'unit','curse.time_on_site';
    emit_addr 'souls',%all,'unit','status.souls',$vbias;
    emit_addr 'states',%all,'unit','status.misc_traits',$vbias;
    emit_addr 'labors',%all,'unit','status.labors';
    emit_addr 'happiness',%all,'unit','status.happiness';
    emit_addr 'squad_ref_id',%all,'unit','hist_figure_id';

    emit_header 'soul_details';
    emit_addr 'skills',%all,'unit_soul','skills',$vbias;
    emit_addr 'traits',%all,'unit_soul','traits';

    emit_header 'job_details';
    emit_addr 'id',%all,'job','job_type';
    emit_addr 'on_break_flag',%all,'unit_misc_trait','OnBreak';
    emit_addr 'sub_job_id',%all,'job','reaction_name';

    emit_header 'squad_offsets';
    emit_addr 'id',%all,'squad','id';
    emit_addr 'name',%all,'squad','name',lookup_addr(%all,'language_name','words');
    emit_addr 'members',%all,'squad','positions',$vbias;

    my $body_str = join("\n",@lines);
    my $complete_str = ($complete ? 'true' : 'false');

    open OUT, ">$subdir/therapist.ini" or die "Cannot open output file";
    print OUT <<__END__;
[info]
checksum=0x$checksum
version_name=$version
complete=$complete_str

$body_str

[valid_flags_1]
size=1
1\\name=Not from around these parts
1\\value=0x80000000

[valid_flags_2]
size=0

[invalid_flags_1]
size=7
1\\name=a zombie
1\\value=0x00001000
2\\name=a skeleton
2\\value=0x00002000
3\\name=a merchant or diplomat
3\\value=0x00000040
4\\name=outpost liason
4\\value=0x00000800
5\\name=an invader or hostile
5\\value=0x00020000
6\\name=an invader or hostile
6\\value=0x00080000
7\\name=an invader or hostile
7\\value=0x000C0000

[invalid_flags_2]
size=2
1\\name="dead, Jim."
1\\value=0x00000080
2\\name=from the Underworld. SPOOKY!
2\\value=0x00040000
__END__
    close OUT;
}

generate_dt_ini 'linux', substr($hash,0,8), 0;
generate_dt_ini 'windows', $timestamp, -4;