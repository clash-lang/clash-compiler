#!/usr/bin/env python3
"""Splice a freshly generated GhcPrimStepMap.hs into the working-tree
Primitive.hs, replacing everything from `ghcPrimStep :: PrimStep` up to and
including the closing `  ]` of ghcPrimStepImpls. Everything outside that
region (imports, literal helpers, ghcPrimUnwind, ...) is left untouched.
"""

import pathlib

from parse_arms import SOURCE

FRAGMENT = pathlib.Path(__file__).with_name("GhcPrimStepMap.hs")


def main() -> None:
    fragment = FRAGMENT.read_text().rstrip("\n").split("\n")
    fragment = fragment[fragment.index("ghcPrimStep :: PrimStep"):]
    assert fragment[-1] == "  ]", "fragment does not end with the map's `  ]`"

    lines = SOURCE.read_text().split("\n")
    start = lines.index("ghcPrimStep :: PrimStep")
    impls = lines.index("ghcPrimStepImpls = HashMap.fromList", start)
    end = lines.index("  ]", impls)

    print(f"replacing lines {start + 1}..{end + 1} of {SOURCE.name} "
          f"with {len(fragment)} fragment lines")
    lines[start:end + 1] = fragment
    SOURCE.write_text("\n".join(lines))


if __name__ == "__main__":
    main()
