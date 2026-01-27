#!/usr/bin/env python3

"""

Simple script to aggregate changelog entries.
It does not support mixed types, but it takes care of markdown formatting
and checks that every entry has a link (to an issue/pr, but that is not verified).

It is by no means perfect, but it's better than nothing.


"""


import os, re, time
from collections import defaultdict as ddict

TYPES = ["ADDED","REMOVED","DEPRICATED","CHANGED","FIXED"]

print("Aggregate changelog entries.")
print("Be aware this code isn't perfect and you still need to make sure the end result is correct!")


# input version
version = input("Version: ")
match time.localtime().tm_mday:
    case 1 | 21 | 31:
        dd = "st"
    case 2 | 22:
        dd = "nd"
    case 3 | 23:
        dd = "rd"
    case _:
        dd = "th"
date = time.strftime(f"%b %d{dd}, %Y")

# get changelog files (all files without extension)
files = os.listdir()
files = [f for f in files if "." not in f]

files.sort()

# select those within a certain time frame
for i,f in enumerate(files):
    print(f"{i+1}\t{f}")

while True:
    indices = input("Enter which to include (a, a-b (inclusive), blank for all): ")
    ix = []
    for i in indices.split(","):
        if i.strip():
            try:
                ix.append(int(i))
            except:
                try:
                    a,b = i.split("-")
                    a,b=int(a),int(b)
                    assert a<=b
                    assert 1<=a
                    assert len(files) <= b
                    for j in range(a,b+1):
                        ix.append(j)
                except:
                    continue
    if not ix:
        ix = list(range(1,len(files)+1))
    break

files = [files[i-1] for i in ix]


# read all
entries = []
for f in files:
    with open(f,"r") as fp:
        entries.append((f,fp.read()))

# check that something is linked
link = re.compile("\[.*\]\(.*\)")
for (f,content) in entries:
    if not link.search(content):
        print(f"WARNING: {f} does not seem to have a link to a PR or issue")

# sort by type
per_type = ddict(list)
for (_,content) in entries:
    for ty in TYPES:
        if content.startswith(ty+":"):
            per_type[ty].append(content[len(ty)+1:].lstrip())
            break
    else:
        per_type["OTHER"].append(content)

if "OTHER" in per_type.keys():
    print("WARNING: Untyped entries found.")

# format as markdown
out = [f"## {version} *{date}*",""]
for ty in TYPES+["OTHER"]:
    if per_type[ty]:
        out.append(ty.capitalize()+":")
        for e in per_type[ty]:
            out.append("- "+e.replace("\n","\n  ").rstrip())
        out.append("")
print("\nOUTPUT:")
print("\n".join(out))
