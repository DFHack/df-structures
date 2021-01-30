import argparse
import json

import jinja2

parser = argparse.ArgumentParser()
parser.add_argument("--old", required=True)
parser.add_argument("--new", required=True)
parser.add_argument("--platform", required=True)
parser.add_argument("--output", required=True)
args = parser.parse_args()

def parse_sizes_file(filename):
    sizes = {}
    with open(filename) as f:
        for line_index, line in enumerate(f):
            type_name, size = line.split(" ")
            if type_name in sizes:
                raise ValueError("%s:%i: Duplicate type: %s" % (filename, line_index + 1, type_name))
            size = int(size)
            sizes[type_name] = size
    return sizes

old_sizes = parse_sizes_file(args.old)
new_sizes = parse_sizes_file(args.new)

rows = []
for type_name in set(old_sizes.keys()) | set(new_sizes.keys()):
    old_size = old_sizes.get(type_name, 0)
    new_size = new_sizes.get(type_name, 0)
    if old_size != new_size:
        rows.append({
            "type": type_name,
            "platform": args.platform,
            "old_size": old_size,
            "new_size": new_size,
        })

with open(args.output, "w") as f:
    json.dump(rows, f)
