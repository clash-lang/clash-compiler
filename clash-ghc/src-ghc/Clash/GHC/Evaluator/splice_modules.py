#!/usr/bin/env python3
"""Apply the module split rendered by ``render_modules.py`` to the source
tree: overwrite ``Primitive.hs``, write ``Primitive.hs-boot``,
``Primitive/Util.hs``, ``Primitives.hs`` and ``Primitives/*.hs``, and register
the new modules in ``clash-ghc.cabal``."""

from __future__ import annotations

import argparse

import parse_map
import render_modules

CABAL_FILE = parse_map.SOURCE.parents[4] / "clash-ghc.cabal"

# The new modules slot in alphabetically after this Other-Modules line.
CABAL_ANCHOR = "Clash.GHC.ClashFlags"


def update_cabal(modules: list[str]) -> int:
    lines = CABAL_FILE.read_text().split("\n")
    anchors = [i for i, line in enumerate(lines) if line.strip() == CABAL_ANCHOR]
    if len(anchors) != 1:
        raise SystemExit(
            f"expected exactly one {CABAL_ANCHOR!r} line in {CABAL_FILE}, "
            f"found {len(anchors)}"
        )
    anchor = anchors[0]
    indent = lines[anchor][: len(lines[anchor]) - len(lines[anchor].lstrip())]
    present = {line.strip() for line in lines}
    fresh = [indent + module for module in modules if module not in present]
    lines[anchor + 1 : anchor + 1] = fresh
    CABAL_FILE.write_text("\n".join(lines))
    return len(fresh)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--revision",
        help="split Primitive.hs as of this git revision instead of the "
        "working tree",
    )
    args = parser.parse_args()
    parsed = parse_map.parse_source(args.revision)
    files = render_modules.render_all(parsed)
    for relative_path, content in sorted(files.items()):
        destination = parse_map.SOURCE.parent / relative_path
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_text(content)
        print(f"wrote {destination}")
    groups = sorted(render_modules.groups_of(parsed.entries))
    modules = sorted(
        [render_modules.UTIL_MODULE, render_modules.COLLECTOR_MODULE]
        + [render_modules.module_name(group) for group in groups]
    )
    added = update_cabal(modules)
    print(f"registered {added} new modules in {CABAL_FILE}")


if __name__ == "__main__":
    main()
