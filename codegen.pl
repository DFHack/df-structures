#!/usr/bin/perl

use strict;
use warnings;

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

for my $fn (sort { $a cmp $b } glob "$input_dir/df.*.xml") {
    local $filename = $fn;
    my $doc = $parser->parse_file($filename);
    $doc = $_->transform($doc) for @transforms;

    push @documents, $doc;
    add_type_to_hash $_ foreach $doc->findnodes('/ld:data-definition/ld:global-type');
    add_global_to_hash $_ foreach $doc->findnodes('/ld:data-definition/ld:global-object');
}

# Generate type text representations

my %type_handlers = (
    'enum-type' => \&render_enum_type,
    'bitfield-type' => \&render_bitfield_type,
    'class-type' => \&render_struct_type,
    'struct-type' => \&render_struct_type,
);

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

    emit_block {
        emit "void InitGlobals();";

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

                    push @items, [ $prefix, $name ];
                } "T_$name";
            };
            if ($@) {
                print 'Error: '.$@."Global $typename in $filename failed\n";
            }
        }
    } "namespace global ";

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

# Write output files

mkdir $output_dir;

{
    # Delete the old files
    for my $name (glob "$output_dir/*.h") {
        unlink $name;
    }
    for my $name (glob "$output_dir/static*.inc") {
        unlink $name;
    }
    unlink "$output_dir/codegen.out.xml";

    # Write out the headers
    local $, = "\n";
    local $\ = "\n";

    for my $name (keys %header_data) {
        open FH, ">$output_dir/$name.h";
        print FH "/* THIS FILE WAS GENERATED. DO NOT EDIT. */";
        print FH @{$header_data{$name}};
        close FH;
    }

    # Write out the static file
    for my $tag (keys %static_lines) {
        my $name = $output_dir.'/static'.($tag?'.'.$tag:'').'.inc';
        open FH, ">$name";
        print FH "/* THIS FILE WAS GENERATED. DO NOT EDIT. */";
        for my $name (sort { $a cmp $b } keys %{$static_includes{$tag}}) {
            print FH "#include \"$name.h\"";
        }
        print FH "namespace $main_namespace {";
        print FH @{$static_lines{$tag}};
        print FH '}';
        close FH;
    }

    # Write an xml file with all types
    open FH, ">$output_dir/codegen.out.xml";
    print FH '<ld:data-definition xmlns:ld="http://github.com/peterix/dfhack/lowered-data-definition">';
    for my $doc (@documents) {
        for my $node ($doc->documentElement()->findnodes('*')) {
            print FH '    '.$node->toString();
        }
    }
    print FH '</ld:data-definition>';
    close FH;
}
