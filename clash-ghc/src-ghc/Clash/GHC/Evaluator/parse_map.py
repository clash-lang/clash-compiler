#!/usr/bin/env python3
"""Parse the HashMap-based ``Clash.GHC.Evaluator.Primitive`` into structured
pieces, as a first step towards splitting it into:

  Clash.GHC.Evaluator.Primitive          -- public API (exports unchanged)
  Clash.GHC.Evaluator.Primitive.Util     -- helpers, used by Primitives.*
  Clash.GHC.Evaluator.Primitives         -- collects all per-module maps
  Clash.GHC.Evaluator.Primitives.*       -- one module per primitive name prefix

The parse is verified by reconstructing the source byte-for-byte. Run as a
program it parses the neighboring ``Primitive.hs`` (or a git revision of it,
with ``--revision``) and prints a summary of what it found.
"""

from __future__ import annotations

import argparse
import pathlib
import re
import subprocess
from collections import Counter
from dataclasses import dataclass

SOURCE = pathlib.Path(__file__).resolve().with_name("Primitive.hs")

# Module prefix of a fully qualified name, e.g. "GHC.Prim." in
# "GHC.Prim.gtChar#". Stops before the last component, also when that
# component is uppercase (e.g. "GHC.Types.C#") or symbolic ("GHC.Real.^_f").
MODULE_PREFIX_RE = re.compile(r"^(?:[A-Z][A-Za-z0-9_']*\.)*")

# First line of a map entry, e.g. "  [ ( $(textNameLit 'GHC.Prim.gtChar#)".
OPENER_RE = re.compile(r"^  ([,\[]) (\( .*)$")

# The key part of an opener: a TemplateHaskell name (plain or operator), or a
# verbatim string literal for primitives without a stable source name.
KEY_RE = re.compile(
    r"^\( (?:\$\(textNameLit '\((?P<operator>[^()]+)\)\)"
    r"|\$\(textNameLit '(?P<plain>[^()\s]+)\)"
    r"|\"(?P<literal>[^\"]*)\")"
)

CPP_RE = re.compile(r"^#(if|else|endif)\s*(.*)$")

ENTRY_CLOSER = "    )"
MAP_CLOSER = "  ]"


@dataclass
class Entry:
    """One key/implementation pair of ``ghcPrimStepImpls``."""

    name: str             # full primitive name, e.g. "GHC.Prim.gtChar#"
    group: str            # module part of the name, e.g. "GHC.Prim"
    kind: str             # "name" | "operator" | "literal"
    opener: str           # opener line minus its "  [ " / "  , " prefix
    lines: list[str]      # lines after the opener, up to and including "    )"
    prefix: list[str]     # comment/blank lines owned by this entry
    cpp: tuple[str, ...]  # CPP conditions guarding this entry ("!" = negated)
    raw: list[str]        # prefix + CPP directives + opener + lines, verbatim


@dataclass
class Block:
    """A top-level declaration (with owned comments) outside the map."""

    name: str
    lines: list[str]


@dataclass
class ParsedSource:
    preamble: list[str]      # copyright header, pragmas, #include
    module_decl: list[str]   # module declaration including export list
    imports: list[str]       # the import block, verbatim
    pre_blocks: list[Block]  # declarations between imports and ghcPrimStep
    dispatcher: list[str]    # ghcPrimStep signature and definition
    context: list[str]       # PrimStepContext + mkPrimStepContext, with haddock
    map_header: list[str]    # ghcPrimStepImpls signature + "= HashMap.fromList"
    entries: list[Entry]
    map_trailer: list[str]   # lines between the last entry and the closing "]"
    post: list[str]          # helper functions following the map
    lines: list[str]         # the original source, for verification


def read_source(revision: str | None = None) -> str:
    if revision is None:
        return SOURCE.read_text()
    result = subprocess.run(
        ["git", "-C", str(SOURCE.parent), "show", f"{revision}:./{SOURCE.name}"],
        check=True,
        capture_output=True,
        text=True,
    )
    return result.stdout


def find_line(lines: list[str], wanted: str, start: int = 0) -> int:
    for i in range(start, len(lines)):
        if lines[i] == wanted:
            return i
    raise SystemExit(f"could not find line: {wanted!r}")


def negate(condition: str) -> str:
    return condition[1:] if condition.startswith("!") else "!" + condition


def block_name(line: str) -> str:
    tokens = line.split()
    if tokens[0] in ("data", "newtype", "type", "class"):
        return tokens[1]
    if tokens[0] == "instance":
        return line.strip()
    return tokens[0]


def parse_blocks(lines: list[str]) -> list[Block]:
    """Split a region of top-level declarations into named blocks. Comments and
    blank lines directly preceding a declaration belong to that declaration;
    consecutive declarations with the same name (signature, multiple equations)
    form a single block."""
    blocks: list[Block] = []
    pending: list[str] = []
    for line in lines:
        if not line.strip() or line.startswith("--"):
            pending.append(line)
        elif line[0] not in " #":
            name = block_name(line)
            if not blocks or blocks[-1].name != name:
                blocks.append(Block(name, []))
            blocks[-1].lines.extend(pending + [line])
            pending = []
        else:
            if not blocks:
                raise SystemExit(f"continuation line before any block: {line!r}")
            blocks[-1].lines.extend(pending + [line])
            pending = []
    if pending:
        if not blocks:
            raise SystemExit("region contains no declarations")
        blocks[-1].lines.extend(pending)
    return blocks


def parse_entries(lines: list[str]) -> tuple[list[Entry], list[str]]:
    """Parse the inside of the ``HashMap.fromList`` list. Returns the entries
    and the trailing lines between the last entry and the closing bracket."""
    entries: list[Entry] = []
    stack: list[str] = []
    prefix: list[str] = []
    raw_pending: list[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        opener_match = OPENER_RE.match(line)
        if opener_match:
            raw = raw_pending + [line]
            body = []
            i += 1
            while lines[i] != ENTRY_CLOSER:
                body.append(lines[i])
                i += 1
            body.append(lines[i])
            i += 1
            opener = opener_match.group(2)
            key_match = KEY_RE.match(opener)
            if not key_match:
                raise SystemExit(f"cannot parse entry key: {opener!r}")
            if key_match.group("operator") is not None:
                kind, name = "operator", key_match.group("operator")
            elif key_match.group("plain") is not None:
                kind, name = "name", key_match.group("plain")
            else:
                kind, name = "literal", key_match.group("literal")
            group = MODULE_PREFIX_RE.match(name).group(0).rstrip(".")
            if not group or len(group) >= len(name):
                raise SystemExit(f"cannot split {name!r} into module and name")
            entries.append(
                Entry(
                    name=name,
                    group=group,
                    kind=kind,
                    opener=opener,
                    lines=body,
                    prefix=prefix,
                    cpp=tuple(stack),
                    raw=raw + body,
                )
            )
            prefix = []
            raw_pending = []
            continue
        cpp_match = CPP_RE.match(line)
        if cpp_match:
            directive, condition = cpp_match.group(1), cpp_match.group(2).strip()
            if directive == "if":
                stack.append(condition)
            elif directive == "else":
                stack[-1] = negate(stack[-1])
            else:
                stack.pop()
            raw_pending.append(line)
        elif not line.strip() or line.startswith("--") or line.startswith("  --"):
            prefix.append(line)
            raw_pending.append(line)
        else:
            raise SystemExit(f"unrecognized line between entries: {line!r}")
        i += 1
    if stack:
        raise SystemExit(f"unbalanced CPP at end of map: {stack!r}")
    if prefix != raw_pending or any(line.strip() for line in prefix):
        raise SystemExit(f"unexpected trailer after last entry: {raw_pending!r}")
    return entries, raw_pending


def verify(parsed: ParsedSource) -> None:
    rebuilt = (
        parsed.preamble
        + parsed.module_decl
        + parsed.imports
        + [line for block in parsed.pre_blocks for line in block.lines]
        + parsed.dispatcher
        + parsed.context
        + parsed.map_header
        + [line for entry in parsed.entries for line in entry.raw]
        + parsed.map_trailer
        + [MAP_CLOSER]
        + parsed.post
    )
    if rebuilt != parsed.lines:
        for i, (got, want) in enumerate(zip(rebuilt, parsed.lines)):
            if got != want:
                raise SystemExit(
                    f"reconstruction mismatch at line {i + 1}:\n"
                    f"  reconstructed: {got!r}\n"
                    f"  original:      {want!r}"
                )
        raise SystemExit(
            f"reconstruction length mismatch: {len(rebuilt)} vs {len(parsed.lines)}"
        )
    keys = Counter((entry.name, entry.cpp) for entry in parsed.entries)
    duplicates = [key for key, count in keys.items() if count > 1]
    if duplicates:
        raise SystemExit(f"duplicate entries: {duplicates!r}")
    for entry in parsed.entries:
        if len(entry.cpp) > 1:
            raise SystemExit(f"nested CPP around entry: {entry.name}")


def parse_source(revision: str | None = None) -> ParsedSource:
    lines = read_source(revision).split("\n")
    module_start = find_line(lines, "module Clash.GHC.Evaluator.Primitive")
    module_end = find_line(lines, "  ) where", module_start)
    first_decl = find_line(lines, "isUndefinedPrimVal :: Value -> Bool", module_end)
    dispatcher_start = find_line(lines, "ghcPrimStep :: PrimStep", first_decl)
    context_start = next(
        i
        for i in range(dispatcher_start, len(lines))
        if lines[i].startswith("data PrimStepContext")
    )
    while lines[context_start - 1].startswith("--"):
        context_start -= 1
    map_signature = find_line(
        lines, "ghcPrimStepImpls :: HashMap.HashMap Text PrimStep", context_start
    )
    if lines[map_signature + 1] != "ghcPrimStepImpls = HashMap.fromList":
        raise SystemExit("unexpected shape of the ghcPrimStepImpls definition")
    map_close = find_line(lines, MAP_CLOSER, map_signature)
    entries, map_trailer = parse_entries(lines[map_signature + 2 : map_close])
    parsed = ParsedSource(
        preamble=lines[:module_start],
        module_decl=lines[module_start : module_end + 1],
        imports=lines[module_end + 1 : first_decl],
        pre_blocks=parse_blocks(lines[first_decl:dispatcher_start]),
        dispatcher=lines[dispatcher_start:context_start],
        context=lines[context_start:map_signature],
        map_header=lines[map_signature : map_signature + 2],
        entries=entries,
        map_trailer=map_trailer,
        post=lines[map_close + 1 :],
        lines=lines,
    )
    verify(parsed)
    return parsed


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--revision",
        help="parse Primitive.hs as of this git revision instead of the "
        "working tree",
    )
    args = parser.parse_args()
    parsed = parse_source(args.revision)
    print(f"blocks: {', '.join(block.name for block in parsed.pre_blocks)}")
    print(f"entries: {len(parsed.entries)}")
    groups = Counter(entry.group for entry in parsed.entries)
    for group, count in sorted(groups.items()):
        print(f"{count:5d}  {group}")
    conditional = [entry for entry in parsed.entries if entry.cpp]
    print(f"CPP-conditional entries: {len(conditional)}")
    for entry in conditional:
        print(f"       {entry.name}  [{' && '.join(entry.cpp)}]")


if __name__ == "__main__":
    main()
