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

sub generate_dt_ini($$$$$) {
    my ($subdir, $version, $checksum, $vbias, $ssize) = @_;

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
    emit_addr 'active_creature_vector',%globals,'world','world.units.active',$vbias;
    emit_addr 'dwarf_race_index',%globals,'ui','ui.race_id';
    emit_addr 'squad_vector',%globals,'world','world.squads.all',$vbias;
    emit_addr 'current_year',%globals,'cur_year','cur_year';
    emit_addr 'cur_year_tick',%globals,'cur_year_tick','cur_year_tick';
    emit_addr 'dwarf_civ_index',%globals,'ui','ui.civ_id';
    emit_addr 'races_vector',%globals,'world','world.raws.creatures.all',$vbias;
    emit_addr 'reactions_vector',%globals,'world','world.raws.reactions',$vbias;
    emit_addr 'historical_figures',%globals,'world','world.history.figures',$vbias;
    emit_addr 'fake_identities',%globals,'world','world.assumed_identities.all',$vbias;
    emit_addr 'historical_figures_vector',%globals,'world','world.history.figures',$vbias;
    emit_addr 'fake_identities_vector',%globals,'world','world.assumed_identities.all',$vbias;
    emit_addr 'fortress_entity',%globals,'ui','ui.main.fortress_entity';
    emit_addr 'historical_entities_vector',%globals,'world','world.entities.all',$vbias;
    emit_addr 'weapons_vector',%globals,'world','world.raws.itemdefs.weapons',$vbias;
    emit_addr 'trap_vector',%globals,'world','world.raws.itemdefs.trapcomps',$vbias;
    emit_addr 'toy_vector',%globals,'world','world.raws.itemdefs.toys',$vbias;
    emit_addr 'tool_vector',%globals,'world','world.raws.itemdefs.tools',$vbias;
    emit_addr 'instrument_vector',%globals,'world','world.raws.itemdefs.instruments',$vbias;
    emit_addr 'armor_vector',%globals,'world','world.raws.itemdefs.armor',$vbias;
    emit_addr 'ammo_vector',%globals,'world','world.raws.itemdefs.ammo',$vbias;
    emit_addr 'siegeammo_vector',%globals,'world','world.raws.itemdefs.siege_ammo',$vbias;
    emit_addr 'glove_vector',%globals,'world','world.raws.itemdefs.gloves',$vbias;
    emit_addr 'shoe_vector',%globals,'world','world.raws.itemdefs.shoes',$vbias;
    emit_addr 'shield_vector',%globals,'world','world.raws.itemdefs.shields',$vbias;
    emit_addr 'helm_vector',%globals,'world','world.raws.itemdefs.helms',$vbias;
    emit_addr 'pant_vector',%globals,'world','world.raws.itemdefs.pants',$vbias;
    emit_addr 'food_vector',%globals,'world','world.raws.itemdefs.food',$vbias;
    emit_addr 'colors_vector',%globals,'world','world.raws.language.colors',$vbias;
    emit_addr 'shapes_vector',%globals,'world','world.raws.language.shapes',$vbias;
    emit_addr 'base_materials',%globals,'world','world.raws.mat_table.builtin';
    emit_addr 'inorganics_vector',%globals,'world','world.raws.inorganics',$vbias;
    emit_addr 'plants_vector',%globals,'world','world.raws.plants.all',$vbias;

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
    emit_addr 'words',%all,'language_name','words';
    emit_addr 'word_type',%all,'language_name','parts_of_speech';
    emit_addr 'language_id',%all,'language_name','language';

    emit_header 'race_offsets';
    emit_addr 'name_singular',%all,'creature_raw','name';
    emit_addr 'name_plural',%all,'creature_raw','name',$ssize;
    emit_addr 'adjective',%all,'creature_raw','name',$ssize*2;
    emit_addr 'baby_name_singular',%all,'creature_raw','general_baby_name';
    emit_addr 'baby_name_plural',%all,'creature_raw','general_baby_name',$ssize;
    emit_addr 'child_name_singular',%all,'creature_raw','general_child_name';
    emit_addr 'child_name_plural',%all,'creature_raw','general_child_name',$ssize;
    emit_addr 'pref_string_vector',%all,'creature_raw','prefstring',$vbias;
    emit_addr 'castes_vector',%all,'creature_raw','caste',$vbias;
    emit_addr 'pop_ratio_vector',%all,'creature_raw','pop_ratio',$vbias;
    emit_addr 'materials_vector',%all,'creature_raw','material',$vbias;

    emit_header 'caste_offsets';
    emit_addr 'caste_name',%all,'caste_raw','caste_name';
    emit_addr 'caste_descr',%all,'caste_raw','description';
    emit_addr 'caste_phys_att_ranges',%all,'caste_raw','attributes.phys_att_range';
    emit_addr 'caste_ment_att_ranges',%all,'caste_raw','attributes.ment_att_range';
    emit_addr 'adult_size',%all,'caste_raw','misc.adult_size';

    emit_header 'hist_entity_offsets';
    emit_addr 'squads',%all,'historical_entity','squads',$vbias;
    emit_addr 'positions',%all,'historical_entity','positions.own',$vbias;
    emit_addr 'assignments',%all,'historical_entity','positions.assignments',$vbias;
    emit_addr 'assign_hist_id',%all,'entity_position_assignment','histfig';
    emit_addr 'assign_position_id',%all,'entity_position_assignment','position_id';
    emit_addr 'position_id',%all,'entity_position','id';
    emit_addr 'position_name',%all,'entity_position','name';
    emit_addr 'position_female_name',%all,'entity_position','name_female';
    emit_addr 'position_male_name',%all,'entity_position','name_male';

    emit_header 'hist_figure_offsets';
    emit_addr 'hist_race',%all,'historical_figure','race';
    emit_addr 'hist_name',%all,'historical_figure','name';
    emit_addr 'id',%all,'historical_figure','id';
    emit_addr 'hist_fig_info',%all,'historical_figure','info';
    emit_addr 'reputation',%all,'historical_figure_info','reputation';
    emit_addr 'current_ident',%all,'historical_figure_info::anon13','cur_identity';
    emit_addr 'fake_name',%all,'assumed_identity','name';
    emit_addr 'fake_birth_year',%all,'assumed_identity','birth_year';
    emit_addr 'fake_birth_time',%all,'assumed_identity','birth_second';

    emit_header 'weapon_offsets';
    emit_addr 'name_plural',%all,'itemdef_weaponst','name_plural';
    emit_addr 'single_size',%all,'itemdef_weaponst','two_handed';
    emit_addr 'multi_size',%all,'itemdef_weaponst','minimum_size';

    emit_header 'material_offsets';
    emit_addr 'solid_name',%all,'material_common','state_name[Solid]';
    emit_addr 'liquid_name',%all,'material_common','state_name[Liquid]';
    emit_addr 'gas_name',%all,'material_common','state_name[Gas]';
    emit_addr 'powder_name',%all,'material_common','state_name[Powder]';
    emit_addr 'paste_name',%all,'material_common','state_name[Paste]';
    emit_addr 'pressed_name',%all,'material_common','state_name[Pressed]';
    emit_addr 'inorganic_materials_vector',%all,'inorganic_raw','material';

    emit_header 'plant_offsets';
    emit_addr 'name',%all,'plant_raw','name';
    emit_addr 'name_plural',%all,'plant_raw','name_plural';
    emit_addr 'name_leaf_plural',%all,'plant_raw','leaves_plural';
    emit_addr 'name_seed_plural',%all,'plant_raw','seed_plural';
    emit_addr 'materials_vector',%all,'plant_raw','material',$vbias;

    emit_header 'item_offsets';
    emit_addr 'name_plural',%all,'itemdef_armorst','name_plural';
    emit_addr 'adjective',%all,'itemdef_armorst','name_preplural';
    emit_addr 'mat_name',%all,'itemdef_armorst','material_placeholder';

    emit_header 'descriptor_offsets';
    emit_addr 'color_name',%all,'descriptor_color','name';
    emit_addr 'shape_name_plural',%all,'descriptor_shape','name_plural';

    emit_header 'dwarf_offsets';
    emit_addr 'first_name',%all,'unit','name',lookup_addr(%all,'language_name','first_name');
    emit_addr 'nick_name',%all,'unit','name',lookup_addr(%all,'language_name','nickname');
    emit_addr 'last_name',%all,'unit','name',lookup_addr(%all,'language_name','words');
    emit_addr 'custom_profession',%all,'unit','custom_profession';
    emit_addr 'profession',%all,'unit','profession';
    emit_addr 'race',%all,'unit','race';
    emit_addr 'flags1',%all,'unit','flags1';
    emit_addr 'flags2',%all,'unit','flags2';
    emit_addr 'flags3',%all,'unit','flags3';
    emit_addr 'caste',%all,'unit','caste';
    emit_addr 'sex',%all,'unit','sex';
    emit_addr 'id',%all,'unit','id';
    emit_addr 'animal_type',%all,'unit','training_level';
    emit_addr 'civ',%all,'unit','civ_id';
    emit_addr 'specific_refs',%all,'unit','specific_refs',$vbias;
    emit_addr 'squad_id',%all,'unit','military.squad_id';
    emit_addr 'squad_position',%all,'unit','military.squad_position';
    emit_addr 'recheck_equipment',%all,'unit','military.pickup_flags';
    emit_addr 'mood',%all,'unit','mood';
    emit_addr 'birth_year',%all,'unit','relations.birth_year';
    emit_addr 'birth_time',%all,'unit','relations.birth_time';
    emit_addr 'current_job',%all,'unit','job.current_job';
    emit_addr 'physical_attrs',%all,'unit','body.physical_attrs';
    emit_addr 'body_size',%all,'unit','appearance.body_modifiers',$vbias;
    emit_addr 'curse',%all,'unit','curse.name';
    emit_addr 'turn_count',%all,'unit','curse.time_on_site';
    emit_addr 'souls',%all,'unit','status.souls',$vbias;
    emit_addr 'states',%all,'unit','status.misc_traits',$vbias;
    emit_addr 'labors',%all,'unit','status.labors';
    emit_addr 'happiness',%all,'unit','status.happiness';
    emit_addr 'squad_ref_id',%all,'unit','hist_figure_id';
    emit_addr 'hist_id',%all,'unit','hist_figure_id';

    emit_header 'soul_details';
    emit_addr 'name',%all,'unit_soul','name';
    emit_addr 'mental_attrs',%all,'unit_soul','mental_attrs';
    emit_addr 'skills',%all,'unit_soul','skills',$vbias;
    emit_addr 'traits',%all,'unit_soul','traits';

    emit_header 'job_details';
    emit_addr 'id',%all,'job','job_type';
    emit_addr 'on_break_flag',%all,'misc_trait_type','OnBreak';
    emit_addr 'sub_job_id',%all,'job','reaction_name';
    emit_addr 'reaction',%all,'reaction','name';
    emit_addr 'reaction_skill',%all,'reaction','skill';

    emit_header 'squad_offsets';
    emit_addr 'id',%all,'squad','id';
    emit_addr 'name',%all,'squad','name';
    emit_addr 'name_old',%all,'squad','name',lookup_addr(%all,'language_name','words');
    emit_addr 'alias',%all,'squad','alias';
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
size=9
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
8\\name=a merchant escort
8\\value=0x00000080
9\\name="Dead, Jim."
9\\value=0x00000002

[invalid_flags_2]
size=5
1\\name="killed, Jim."
1\\value=0x00000080
2\\name=from the Underworld. SPOOKY!
2\\value=0x00040000
3\\name=resident
3\\value=0x00080000
4\\name=visitor_uninvited
4\\value=0x00400000
5\\name=visitor
5\\value=0x00800000

[invalid_flags_3]
size=1
1\\name=a ghost
1\\value=0x00001000
__END__
    close OUT;
}

generate_dt_ini 'linux', $version, substr($hash,0,8), 0, 4;
generate_dt_ini 'windows', $version.' (graphics)', $timestamp, -4, 0x1C;
