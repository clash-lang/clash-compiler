#!/usr/bin/env python3

"""

Simple script to add changelog entries.

Formats the entries like:

<TYPE>: <the possibly
multiline description>

"""


import time

print("CREATE CHANGELOG ENTRY")

# time
t=time.strftime("%Y-%m-%dT%H_%M_%S%z")
print("Time:",t)

# name
while True:
    fname = input("Enter entry name: ")
    fname = fname.replace(" ","_")
    fname = "".join(c for c in fname if c.isalpha() or c.isnumeric() or c=="_")

    fname = f"{t[:-2]}_{t[-2:]}_{fname}"
    print("Filename:",fname)

    resp = input("Correct? [y/n]: ")
    if resp in ["n","N","no"]:
        continue
    break

# type
while True:
    ty = input("Enter entry type ([F]IXED/[A]DDED/[C]HANGED/[R]EMOVED/[D]EPRICATED): ").upper()
    match ty:
        case "F" | "FIX" | "FIXED":
            ty = "FIXED"
        case "A" | "ADD" | "ADDED":
            ty = "ADDED"
        case "C" | "CHANGE" | "CHANGED":
            ty = "CHANGED"
        case "R" | "REMOVE" | "REMOVED":
            ty = "REMOVED"
        case "D" | "DEPRICATE" | "DEPRICATED":
            ty = "DEPRICATED"
        case _:
            continue
    break

# content
N = 4
print(f"Enter contents - enter {N+1} consecutive newlines to complete")

content = []
while True:
    content.append(input())
    if content[-N:] == [""]*N:
        break
content = "\n".join(content[:-N])

# store:
with open(fname,"w") as fp:
    fp.write(f"{ty}: {content}")
