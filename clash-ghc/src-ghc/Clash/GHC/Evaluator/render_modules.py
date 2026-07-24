#!/usr/bin/env python3
"""Render the pieces parsed by ``parse_map.py`` into the split module
hierarchy:

  Primitive.hs        -- public API: ghcPrimStep, ghcPrimUnwind, isUndefined*
  Primitive.hs-boot   -- breaks the Primitives.* -> Primitive import cycle
  Primitive/Util.hs   -- helpers used by the entries and by Primitive itself
  Primitives.hs       -- concatenates all per-module maps
  Primitives/*.hs     -- one module per primitive name prefix

The generated modules deliberately copy the full pragma and import block of
the original module; unused imports and pragmas are cleaned up in a separate
step, guided by ``-Werror``.

Run as a program it writes the generated files to ``--out`` (default:
``tmp/split-modules/out`` in the repository root).
"""

from __future__ import annotations

import argparse
import pathlib

import parse_map
from parse_map import Entry, ParsedSource

REPO_ROOT = parse_map.SOURCE.parents[5]

PRIMITIVE_MODULE = "Clash.GHC.Evaluator.Primitive"
UTIL_MODULE = "Clash.GHC.Evaluator.Primitive.Util"
COLLECTOR_MODULE = "Clash.GHC.Evaluator.Primitives"

SOURCE_IMPORT = "import {-# SOURCE #-} Clash.GHC.Evaluator.Primitive"
UTIL_IMPORT = "import Clash.GHC.Evaluator.Primitive.Util"
COLLECTOR_IMPORT = f"import {COLLECTOR_MODULE} (ghcPrimStepImpls)"

PRIMITIVES_SIGNATURE = "primitives :: [(Text, PrimStep)]"

# Declarations that stay in Primitive.hs; everything else outside the map
# moves to Primitive/Util.hs. These are exactly the exported names other than
# ghcPrimStep (whose definition is in ParsedSource.dispatcher).
STAY_BLOCKS = {"isUndefinedPrimVal", "isUndefinedXPrimVal", "ghcPrimUnwind"}

BOOT_FILE = """module Clash.GHC.Evaluator.Primitive where

import Clash.Core.Evaluator.Types (PrimStep, PrimUnwind)

ghcPrimStep :: PrimStep
ghcPrimUnwind :: PrimUnwind
"""


def module_name(group: str) -> str:
    return f"{COLLECTOR_MODULE}.{group}"


def module_path(group: str) -> str:
    return "Primitives/" + group.replace(".", "/") + ".hs"


def groups_of(entries: list[Entry]) -> dict[str, list[Entry]]:
    groups: dict[str, list[Entry]] = {}
    for entry in entries:
        groups.setdefault(entry.group, []).append(entry)
    return groups


def strip_blank_edges(lines: list[str]) -> list[str]:
    result = list(lines)
    while result and not result[0].strip():
        result.pop(0)
    while result and not result[-1].strip():
        result.pop()
    return result


def module_header(name: str, exports: list[str] | None) -> list[str]:
    if exports is None:
        return [f"module {name} where"]
    header = [f"module {name}"]
    for i, export in enumerate(exports):
        header.append(("  ( " if i == 0 else "  , ") + export)
    return header + ["  ) where"]


def assemble(
    parsed: ParsedSource,
    module_lines: list[str],
    extra_imports: list[str],
    sections: list[list[str]],
) -> str:
    out = strip_blank_edges(parsed.preamble)
    out += ["", *module_lines, ""]
    out += strip_blank_edges(parsed.imports)
    if extra_imports:
        out += ["", *extra_imports]
    for section in sections:
        out += ["", *strip_blank_edges(section)]
    return "\n".join(out) + "\n"


def emit_entry(out: list[str], entry: Entry, first: bool) -> None:
    prefix = strip_blank_edges(entry.prefix) if first else entry.prefix
    out.extend(prefix)
    out.append(("  [ " if first else "  , ") + entry.opener)
    out.extend(entry.lines)


def render_entry_list(entries: list[Entry]) -> list[str]:
    """Render entries as a list expression. Unconditional entries come first
    so that CPP directives never guard the opening bracket or a leading
    comma; complementary conditions merge into a single #if/#else/#endif."""
    unconditional = [entry for entry in entries if not entry.cpp]
    conditional = [entry for entry in entries if entry.cpp]
    if not unconditional:
        raise SystemExit("render_entry_list needs an unconditional entry")
    out: list[str] = []
    open_condition = None
    for i, entry in enumerate(unconditional + conditional):
        condition = entry.cpp[0] if entry.cpp else None
        if condition != open_condition:
            if open_condition is not None and condition == parse_map.negate(
                open_condition
            ):
                out.append("#else")
            else:
                if open_condition is not None:
                    out.append("#endif")
                if condition is not None:
                    out.append(f"#if {condition}")
            open_condition = condition
        emit_entry(out, entry, first=i == 0)
    if open_condition is not None:
        out.append("#endif")
    out.append(parse_map.MAP_CLOSER)
    return out


def render_primitives_binding(entries: list[Entry]) -> list[str]:
    if any(not entry.cpp for entry in entries):
        return [PRIMITIVES_SIGNATURE, "primitives ="] + render_entry_list(entries)
    # All entries share one CPP condition (e.g. GHC.PrimopWrappers): guard the
    # whole binding and fall back to an empty map.
    conditions = {entry.cpp for entry in entries}
    if len(conditions) != 1:
        raise SystemExit(
            f"mixed CPP conditions without unconditional entries: {conditions!r}"
        )
    out = [PRIMITIVES_SIGNATURE, f"#if {entries[0].cpp[0]}", "primitives ="]
    for i, entry in enumerate(entries):
        emit_entry(out, entry, first=i == 0)
    out += [parse_map.MAP_CLOSER, "#else", "primitives = []", "#endif"]
    return out


def render_group_module(
    parsed: ParsedSource, group: str, entries: list[Entry]
) -> str:
    return assemble(
        parsed,
        module_header(module_name(group), ["primitives"]),
        [SOURCE_IMPORT, UTIL_IMPORT],
        [render_primitives_binding(entries)],
    )


def render_collector(parsed: ParsedSource, groups: list[str]) -> str:
    imports = [f"import qualified {module_name(group)} as {group}" for group in groups]
    body = [
        "ghcPrimStepImpls :: HashMap.HashMap Text PrimStep",
        "ghcPrimStepImpls = HashMap.fromList $ concat",
    ]
    for i, group in enumerate(groups):
        body.append(("  [ " if i == 0 else "  , ") + f"{group}.primitives")
    body.append(parse_map.MAP_CLOSER)
    return assemble(
        parsed,
        module_header(COLLECTOR_MODULE, ["ghcPrimStepImpls"]),
        imports,
        [body],
    )


def render_util(parsed: ParsedSource) -> str:
    blocks = [
        block.lines
        for block in parsed.pre_blocks
        if block.name not in STAY_BLOCKS
    ]
    return assemble(
        parsed,
        module_header(UTIL_MODULE, None),
        [SOURCE_IMPORT],
        blocks + [parsed.context, parsed.post],
    )


def render_primitive(parsed: ParsedSource) -> str:
    blocks = [
        block.lines for block in parsed.pre_blocks if block.name in STAY_BLOCKS
    ]
    return assemble(
        parsed,
        parsed.module_decl,
        [UTIL_IMPORT, COLLECTOR_IMPORT],
        blocks + [parsed.dispatcher],
    )


def extract_entries(text: str) -> list[tuple[str, tuple[str, ...]]]:
    lines = text.split("\n")
    result = []
    i = 0
    while i < len(lines):
        opener_match = parse_map.OPENER_RE.match(lines[i])
        if opener_match:
            body = []
            i += 1
            while lines[i] != parse_map.ENTRY_CLOSER:
                body.append(lines[i])
                i += 1
            body.append(lines[i])
            result.append((opener_match.group(2), tuple(body)))
        i += 1
    return result


def verify_rendered(parsed: ParsedSource, files: dict[str, str]) -> None:
    """Every parsed entry must reappear verbatim, exactly once, in the module
    belonging to its group."""
    total = 0
    for group, entries in groups_of(parsed.entries).items():
        wanted = sorted((entry.opener, tuple(entry.lines)) for entry in entries)
        rendered = sorted(extract_entries(files[module_path(group)]))
        if wanted != rendered:
            raise SystemExit(f"entry mismatch in group {group}")
        total += len(rendered)
    if total != len(parsed.entries):
        raise SystemExit(f"entry count mismatch: {total} vs {len(parsed.entries)}")


def render_all(parsed: ParsedSource) -> dict[str, str]:
    groups = groups_of(parsed.entries)
    files = {
        "Primitive.hs": render_primitive(parsed),
        "Primitive.hs-boot": BOOT_FILE,
        "Primitive/Util.hs": render_util(parsed),
        "Primitives.hs": render_collector(parsed, sorted(groups)),
    }
    for group, entries in groups.items():
        files[module_path(group)] = render_group_module(parsed, group, entries)
    verify_rendered(parsed, files)
    return files


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--revision",
        help="render from Primitive.hs as of this git revision instead of "
        "the working tree",
    )
    parser.add_argument(
        "--out",
        type=pathlib.Path,
        default=REPO_ROOT / "tmp" / "split-modules" / "out",
    )
    args = parser.parse_args()
    parsed = parse_map.parse_source(args.revision)
    files = render_all(parsed)
    for relative_path, content in sorted(files.items()):
        destination = args.out / relative_path
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_text(content)
        print(f"{len(content.splitlines()):6d}  {destination}")


if __name__ == "__main__":
    main()
