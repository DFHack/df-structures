import argparse
import json

import jinja2

parser = argparse.ArgumentParser()
parser.add_argument("--reports", required=True, metavar="REPORT", nargs="+")
parser.add_argument("--template", required=True)
parser.add_argument("--output", required=True)
parser.add_argument("--github-actions", action="store_true")
args = parser.parse_args()

rows = []
for report_path in args.reports:
    with open(report_path) as f:
        rows.extend(json.load(f))

# special case: ignore if all of the changes identified are new types
rows_with_old_size = [row for row in rows if row["old_size"] > 0]
if not rows_with_old_size:
    rows = []

rows.sort(key=lambda row: (row["type"], row["platform"]))

with open(args.template) as f:
    template = jinja2.Template(f.read())

with open(args.output, "w") as f:
    f.write(template.render(rows=rows))

if args.github_actions:
    print("::set-output name=has_rows::%s" % str(bool(rows)).lower())
