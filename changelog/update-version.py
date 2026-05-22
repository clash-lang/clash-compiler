#!/usr/bin/env python3

import re, os

os.chdir("..")

while True:
  version = input("New version: ")
  conf = input(f"Changing to version {version}. Confirm? [Y/n]: ")
  if conf in ["n","N","no"]:
    continue
  break

cabal = re.compile(r"(^[Vv]ersion:\s*)(.*)()",re.MULTILINE)
py = re.compile(r"(^version = \')(.*)(\')",re.MULTILINE)
dep = re.compile(r"(clash-prelude|clash-lib)(\s*== )([\d.]+)(,)")

for fname,pat in [
    ("clash-prelude/clash-prelude.cabal",cabal),
    ("clash-prelude-hedgehog/clash-prelude-hedgehog.cabal",cabal),
    ("clash-lib/clash-lib.cabal",cabal),
    ("clash-lib-hedgehog/clash-lib-hedgehog.cabal",cabal),
    ("clash-ghc/clash-ghc.cabal",cabal),
    ("docs/conf.py",py),
]:
    with open(fname,"r+") as fp:
        contents = fp.read()
        contents = dep.sub(r"\g<1>\g<2>"+version+r"\g<4>",contents)
        if pat.search(contents):
            contents = pat.sub(r"\g<1>"+version+r"\g<3>",contents,count=1)
            fp.seek(0)
            fp.write(contents)
            fp.truncate()
        else:
            print("WARNING: Could not find version in",fname)
