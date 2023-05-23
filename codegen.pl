#!/usr/bin/perl

use strict;
use warnings;
use File::Glob 'bsd_glob';

BEGIN {
    our $script_root = '.';
    if ($0 =~ /^(.*)[\\\/][^\\\/]*$/) {
        $script_root = $1;
        unshift @INC, $1;
    }
};

use XML::LibXML;
use XML::LibXSLT;

use Common;

use Enum;
use Bitfield;
use StructFields;
use StructType;

my $input_dir = $ARGV[0] || '.';
my $output_dir = $ARGV[1] || 'codegen';

$main_namespace = $ARGV[2] || 'df';
$export_prefix = 'DFHACK_EXPORT ';

# Collect all type definitions from XML files

our $script_root;
my $parser = XML::LibXML->new();
my $xslt = XML::LibXSLT->new();
my @transforms =
    map { $xslt->parse_stylesheet_file("$script_root/$_"); }
    ('lower-1.xslt', 'lower-2.xslt');
my @documents;

for my $fn (sort { $a cmp $b } bsd_glob "$input_dir/df.*.xml") {
    local $filename = $fn;
    my $doc = $parser->parse_file($filename);
    $doc = $_->transform($doc) for @transforms;

    push @documents, $doc;
    add_type_to_hash $_ foreach $doc->findnodes('/ld:data-definition/ld:global-type');
    add_global_to_hash $_ foreach $doc->findnodes('/ld:data-definition/ld:global-object');
}

# Generate type text representations

my %type_preprocessors = (
    'class-type' => \&preprocess_struct_type,
    'struct-type' => \&preprocess_struct_type,
);

my %type_handlers = (
    'enum-type' => \&render_enum_type,
    'bitfield-type' => \&render_bitfield_type,
    'class-type' => \&render_struct_type,
    'struct-type' => \&render_struct_type,
);

for my $name (sort { $a cmp $b } keys %types) {
    local $typename = $name;

    eval {
        my $type = $types{$typename};
        my $meta = $type->getAttribute('ld:meta');
        my $handler = $type_preprocessors{$meta};
        if ($handler) {
            $handler->($type);
        }
    };
    if ($@) {
        print "Error preprocessing type $typename: ".$@;
    }
}

for my $name (sort { $a cmp $b } keys %types) {
    local $typename = $name;
    local $filename = $type_files{$typename};

    eval {
        my $type = $types{$typename};
        my $meta = $type->getAttribute('ld:meta') or die "Null meta";

        # Emit the actual type definition
        with_header_file {
            my $handler = $type_handlers{$meta} or die "Unknown type meta: $meta\n";
            $handler->($type);
        } $typename;
    };
    if ($@) {
        print 'Error: '.$@."Type $typename in $filename ignored\n";
    }
}

# Generate globals

with_header_file {
    my @items;
    my @fields;

    emit_block {
        emit "void InitGlobals();";
        emit "extern ", $export_prefix, "global_identity _identity;";

        for my $name (sort { $a cmp $b } keys %globals) {
            local $typename = $name;
            local $filename = $global_files{$typename};
            local $in_struct_body = 1;

            eval {
                my $tag = $globals{$typename};
                with_anon {
                    my $prefix = get_container_item_type($tag, -weak => 1, -void => 'void');
                    emit_comment $tag;
                    emit 'extern ', $export_prefix, $prefix, ' *', $name, ';', get_comment($tag);
                    header_ref("Export.h");

                    push @items, [ $prefix, $name ];
                    push @fields, $tag->findnodes('ld:item');
                } "T_$name";
            };
            if ($@) {
                print 'Error: '.$@."Global $typename in $filename failed\n";
            }
        }
    } "namespace global ";

    with_emit_static {
        my %info;
        my $ftable = render_field_metadata(undef, 'global', @fields, %info);
        emit "global_identity global::_identity($ftable);";
    } 'fields';

    with_emit_static {
        emit_block {
            for my $item (@items) {
                emit $item->[0], ' *', $item->[1], ' = NULL;';
            }

            emit_block {
                emit "INIT_GLOBAL_FUNCTION_PREFIX";

                for my $item (@items) {
                    emit "INIT_GLOBAL_FUNCTION_ITEM(", $item->[0], ', ', $item->[1], ");";
                }
            } "void InitGlobals() ";
        } "namespace global ";
    };
} 'global_objects';


sub replace_file {
    my ($filename, $new) = @_;
    if (-e $filename) {
        open(FH, '<', $filename);
        my $old = do { local $/; <FH> };
        close FH;
        if ($old eq $new) {
            return;
        }
    }
    open(FH, '>', $filename);
    do { local $\; print FH $new };
    close FH;
}

# Write output files

mkdir $output_dir;

{
    my %files;
    # Get a list of all the existing files
    for my $name (bsd_glob "$output_dir/*.h") {
        $files{$name} = 1;
    }
    for my $name (bsd_glob "$output_dir/static*.inc") {
        $files{$name} = 1;
    }
    $files{"$output_dir/codegen.out.xml"} = 1;

    # Write out the headers
    local $, = "\n";
    local $\ = "\n";

    my $data;

    for my $name (keys %header_data) {
        $data = "/* THIS FILE WAS GENERATED. DO NOT EDIT. */\n";
        $data .= join("\n", @{$header_data{$name}})."\n";
        replace_file("$output_dir/$name.h", $data);
        $files{"$output_dir/$name.h"} = 0;
    }

    # Write out the static file
    for my $tag (keys %static_lines) {
        $data = "/* THIS FILE WAS GENERATED. DO NOT EDIT. */\n";
        for my $name (sort { $a cmp $b } keys %{$static_includes{$tag}}) {
            $data .= "#include \"$name.h\"\n";
        }
        $data .= "namespace $main_namespace {\n";
        $data .= join("\n", @{$static_lines{$tag}})."\n";
        $data .= '}'."\n";

        my $name = $output_dir.'/static'.($tag?'.'.$tag:'').'.inc';
        replace_file($name, $data);
        $files{$name} = 0;
    }

    # Touch all static.fields-*.inc files
    for my $group ("a" .. "z") {
        my $name = $output_dir."/static.fields-$group.inc";
        unless (-f $name) {
            open my $fh, ">>", $name;
            close $fh;
        }
        $files{$name} = 0;
    }

    # Write an xml file with all types.
    # Always do it, so that its date could be used in make to
    # determine if the script had been run after inputs changed.
    open(FH, '>', "$output_dir/codegen.out.xml");
    print FH '<ld:data-definition xmlns:ld="http://github.com/peterix/dfhack/lowered-data-definition">';
    for my $doc (@documents) {
        for my $node ($doc->documentElement()->findnodes('*')) {
            print FH '    '.$node->toString();
        }
    }
    print FH '</ld:data-definition>';
    close FH;
    $files{"$output_dir/codegen.out.xml"} = 0;

    for my $name (keys %files) {
        if ($files{$name} == 1) {
            print("File $name was present but not generated - deleting\n");
            unlink($name);
        }
    }
}
