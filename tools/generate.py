import argparse
import os
import subprocess

import jinja2

os.chdir(os.path.dirname(os.path.abspath(__file__)))

parser = argparse.ArgumentParser()
parser.add_argument("--template", required=True)
parser.add_argument("--output", required=True)
parser.add_argument("--perl", required=True)
args = parser.parse_args()

types = subprocess.check_output([args.perl, "../list.pl", "..", "df"]).decode().replace("\r", "").split("\n")
types = [t.split("/")[-1].split(".")[0] for t in types
    if t.endswith(".h") and "global_objects.h" not in t]
types.sort()

with open(args.template) as f:
    template = jinja2.Template(f.read())

with open(args.output, "w") as f:
    f.write(template.render(types=types))
