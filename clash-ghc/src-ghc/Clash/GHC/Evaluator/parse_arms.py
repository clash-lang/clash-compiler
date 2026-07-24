#!/usr/bin/env python3
"""Parse the arms of the big `case primName pInfo of` in the neighboring
Primitive.hs into structured data.

Every alternative ("arm") of the case expression starts at exactly 2-space
indentation and is either:

  * a Template Haskell name pattern:  $(namePat 'GHC.Prim.quotInt#)
    (operator form: $(namePat '(GHC.Prim.+#)) )                -> NamePatArm
  * a verbatim string literal:        "GHC.Real.$wf"           -> VerbatimArm

Everything else in the region is either a column-0 section header comment,
a 2-space-indented comment (owned by the arm that follows it, or interior to
the current arm's body), a CPP directive, a blank line, or the final
`_ -> Nothing` wildcard.

`body` does NOT contain the pattern itself: it is everything after the
matched pattern, starting with the remainder of the pattern line (trailing
guards or comments, possibly empty). The dumper/renderer re-inserts the
pattern from the arm's kind and name via `render_pattern`, which also
re-derives identifier vs operator quoting ('name vs '(name)).

CPP directives appear in two roles and are told apart by pairing #if/#endif
and checking whether any arm starts inside the group:

  * arm-level: the group conditionally includes whole arms. The directives
    become RawLines and every arm inside gets the active condition stack in
    its `cpp` field (an #else branch shows up as a `!`-negated condition).
  * interior: the group sits inside one arm's guards and stays in `body`.

The parse is lossless: re-concatenating all parsed items reproduces the
original region byte-for-byte, and the script asserts this.

Duplicate names are allowed and meaningful: when the guards of an earlier
arm fail, matching falls through to the next arm with the same name. The
resulting map is therefore name -> [Arm].
"""

from dataclasses import dataclass, field
from typing import Optional, Union
import collections
import json
import pathlib
import re
import subprocess
import sys

SOURCE = pathlib.Path(__file__).resolve().with_name("Primitive.hs")

CASE_HEADER_RE = re.compile(
    r"^ghcPrimStep .* = case primName pInfo of$")

# $(namePat 'GHC.Prim.quotInt#)   or   $(namePat '(GHC.Prim.+#))
NAMEPAT_RE = re.compile(
    r"^  \$\(namePat\s+'(?:\((?P<operator>[^()]+)\)|(?P<plain>[^()\s]+))\)")

# "GHC.Real.$wf"  (names never contain quotes or escapes)
STRING_RE = re.compile(r'^  "(?P<name>[^"]*)"')

WILDCARD_RE = re.compile(r"^  _\s*->")

ARM_COMMENT_RE = re.compile(r"^  --")          # comment at arm indentation
SECTION_RE = re.compile(r"^--")                # comment at column 0
CPP_RE = re.compile(r"^#\s*(if|else|endif)\b\s*(.*)$")


@dataclass
class Arm:
    name: str
    comment: Optional[str]   # 2-space comment block owned by this arm, verbatim
    body: str                # everything after the pattern: rest of the
                             # pattern line, guards, expression — verbatim
    cpp: Optional[list[str]] = None  # active arm-level CPP conditions, if any


@dataclass
class NamePatArm(Arm):
    pass


@dataclass
class VerbatimArm(Arm):
    pass


@dataclass
class RawLine:
    """A line not owned by any arm: section headers, arm-level CPP
    directives, blank separators, floating comments. Kept so the region can
    be reconstructed exactly."""
    text: str


Item = Union[Arm, RawLine]

# Maximal leading run of `Upper.`-style module segments in a qualified name.
MODULE_PREFIX_RE = re.compile(r"^(?:[A-Z][A-Za-z0-9_']*\.)*")


def is_operator_name(name: str) -> bool:
    """Whether the base name (module prefix stripped) is a Haskell operator,
    which needs parens after a TH quote: '(GHC.Prim.+#) vs 'GHC.Prim.ord#."""
    base = name[MODULE_PREFIX_RE.match(name).end():]
    return not (base and (base[0].isalpha() or base[0] == "_"))


def render_pattern(arm: Arm) -> str:
    """Re-render the case pattern that `body` was stripped of."""
    if isinstance(arm, VerbatimArm):
        return f'"{arm.name}"'
    if is_operator_name(arm.name):
        return f"$(namePat '({arm.name}))"
    return f"$(namePat '{arm.name})"


def is_blank(line: str) -> bool:
    return line.strip() == ""


def is_continuation(line: str) -> bool:
    """A line belonging to an arm body: indented deeper than the pattern."""
    return line.startswith("   ") and not is_blank(line)


def is_arm_start(line: str) -> bool:
    return bool(NAMEPAT_RE.match(line) or STRING_RE.match(line))


def find_region(lines: list[str]) -> tuple[int, int]:
    """Return (start, end): arm lines live in lines[start:end], where
    lines[end] is the top-level `_ -> ...` wildcard."""
    start = None
    for i, line in enumerate(lines):
        if CASE_HEADER_RE.match(line):
            start = i + 1
            break
    if start is None:
        sys.exit("could not find the ghcPrimStep case header "
                 "(working tree already converted? try a git revision, "
                 "e.g. --revision HEAD)")
    for i in range(start, len(lines)):
        if WILDCARD_RE.match(lines[i]):
            return start, i
    sys.exit("could not find the top-level `_ -> Nothing` wildcard")


def classify_cpp(lines: list[str], start: int, end: int) -> dict[int, str]:
    """Map line index of each CPP directive to 'arm-level' or 'interior'.

    A #if..#endif group (including its #else) is arm-level iff an arm start
    occurs between #if and #endif; then its directives separate arms.
    Otherwise the whole group sits inside a single arm's body.
    """
    roles: dict[int, str] = {}
    stack: list[tuple[int, list[int], bool]] = []  # (#if index, group indices, saw_arm)
    for i in range(start, end):
        m = CPP_RE.match(lines[i])
        if m:
            kind = m.group(1)
            if kind == "if":
                stack.append((i, [i], False))
            elif kind == "else":
                assert stack, f"line {i + 1}: #else without #if"
                stack[-1][1].append(i)
            else:  # endif
                assert stack, f"line {i + 1}: #endif without #if"
                _, group, saw_arm = stack.pop()
                group.append(i)
                role = "arm-level" if saw_arm else "interior"
                for j in group:
                    roles[j] = role
        elif is_arm_start(lines[i]) and stack:
            stack = [(a, g, True) for (a, g, _) in stack]
    assert not stack, "unterminated #if in region"
    return roles


def parse_arm(lines: list[str], i: int, end: int, cpp_roles: dict[int, str],
              comment_lines: list[str], cpp_stack: list[str],
              warnings: list[str]) -> tuple[Arm, int]:
    """Parse one arm starting at lines[i]. Returns (arm, next_index)."""
    pattern_line = lines[i]
    m = NAMEPAT_RE.match(pattern_line)
    if m:
        cls = NamePatArm
        name = m.group("operator") or m.group("plain")
    else:
        m = STRING_RE.match(pattern_line)
        assert m, f"line {i + 1}: not an arm start: {pattern_line!r}"
        cls = VerbatimArm
        name = m.group("name")

    # Body excludes the pattern; keep the rest of the pattern line (guards,
    # trailing comment, possibly empty) so the arm renders back verbatim.
    body_lines = [pattern_line[m.end():]]
    i += 1
    while i < end:
        line = lines[i]
        if CPP_RE.match(line):
            if cpp_roles[i] == "interior":
                body_lines.append(line)
                i += 1
                continue
            break  # arm-level directive: body is over
        if is_continuation(line):
            body_lines.append(line)
            i += 1
            continue
        if not (is_blank(line) or ARM_COMMENT_RE.match(line)):
            break  # arm start or section header: body is over

        # A run of blanks and/or 2-space comments. Look ahead to decide
        # ownership: if the run is followed by a continuation line (or
        # interior CPP) it is interior to this body; otherwise it belongs
        # between arms (and its trailing comment block, if any, to the
        # *next* arm).
        j = i
        while j < end and (is_blank(lines[j]) or ARM_COMMENT_RE.match(lines[j])):
            j += 1
        follows_body = j < end and (
            is_continuation(lines[j])
            or (CPP_RE.match(lines[j]) and cpp_roles[j] == "interior"))
        if follows_body:
            interior = lines[i:j + 1]
            if any(ARM_COMMENT_RE.match(l) for l in interior):
                warnings.append(
                    f"line {i + 1}: 2-space comment interior to body of "
                    f"{name!r}, kept inside body")
            body_lines.extend(interior)
            i = j + 1
            continue
        break  # run separates this arm from whatever follows

    comment = "\n".join(comment_lines) if comment_lines else None
    arm = cls(name=name, comment=comment, body="\n".join(body_lines),
              cpp=list(cpp_stack) if cpp_stack else None)
    assert "  " + render_pattern(arm) == pattern_line[:m.end()], \
        f"line {i + 1}: cannot re-render pattern of {name!r}"
    return arm, i


def parse(lines: list[str]) -> tuple[list[Item], list[str], int, int]:
    start, end = find_region(lines)
    cpp_roles = classify_cpp(lines, start, end)
    items: list[Item] = []
    warnings: list[str] = []
    pending: list[str] = []   # buffered blank/comment lines between arms
    cpp_stack: list[str] = []  # active arm-level CPP conditions

    def flush_pending_to(next_is_arm: bool) -> list[str]:
        """Emit buffered lines as RawLines, except the trailing contiguous
        comment block, which the upcoming arm owns (if one follows)."""
        owned: list[str] = []
        if next_is_arm:
            k = len(pending)
            while k > 0 and ARM_COMMENT_RE.match(pending[k - 1]):
                k -= 1
            owned = pending[k:]
            del pending[k:]
        for line in pending:
            if ARM_COMMENT_RE.match(line):
                warnings.append(f"floating 2-space comment left unowned: {line!r}")
            items.append(RawLine(line))
        pending.clear()
        return owned

    i = start
    while i < end:
        line = lines[i]
        cpp = CPP_RE.match(line)
        if cpp:
            assert cpp_roles[i] == "arm-level", \
                f"line {i + 1}: interior CPP directive at arm level"
            flush_pending_to(next_is_arm=False)
            kind, condition = cpp.group(1), cpp.group(2).strip()
            if kind == "if":
                cpp_stack.append(condition)
            elif kind == "else":
                cpp_stack.append("!" + cpp_stack.pop())
            else:  # endif
                cpp_stack.pop()
            items.append(RawLine(line))
            i += 1
        elif is_arm_start(line):
            owned = flush_pending_to(next_is_arm=True)
            arm, i = parse_arm(lines, i, end, cpp_roles, owned, cpp_stack,
                               warnings)
            items.append(arm)
        elif is_blank(line) or ARM_COMMENT_RE.match(line) or SECTION_RE.match(line):
            if SECTION_RE.match(line):
                flush_pending_to(next_is_arm=False)
                items.append(RawLine(line))
            else:
                pending.append(line)
            i += 1
        else:
            sys.exit(f"line {i + 1}: unrecognized line at arm level: {line!r}")
    flush_pending_to(next_is_arm=False)
    assert not cpp_stack, "unbalanced arm-level CPP at end of region"
    return items, warnings, start, end


def reconstruct(items: list[Item]) -> str:
    parts = []
    for item in items:
        if isinstance(item, RawLine):
            parts.append(item.text)
        else:
            if item.comment is not None:
                parts.append(item.comment)
            parts.append("  " + render_pattern(item) + item.body)
    return "\n".join(parts)


def parse_source(revision: Optional[str] = None,
                 ) -> tuple[list[str], list[Item], list[str], int, int]:
    """Read SOURCE (from the working tree, or from a git revision — needed
    once the working tree has been converted to the map representation) and
    parse it. Returns (lines, items, warnings, start, end)."""
    if revision is None:
        text = SOURCE.read_text()
    else:
        text = subprocess.run(
            ["git", "-C", str(SOURCE.parent), "show",
             f"{revision}:./{SOURCE.name}"],
            check=True, capture_output=True, text=True).stdout
    lines = text.split("\n")
    items, warnings, start, end = parse(lines)
    return lines, items, warnings, start, end


def main() -> None:
    revision = sys.argv[1] if len(sys.argv) > 1 else None
    lines, items, warnings, start, end = parse_source(revision)

    original_region = "\n".join(lines[start:end])
    assert reconstruct(items) == original_region, \
        "reconstruction does not match the original source region"

    arms = [item for item in items if isinstance(item, Arm)]
    arm_map: dict[str, list[Arm]] = collections.defaultdict(list)
    for arm in arms:
        arm_map[arm.name].append(arm)

    n_namepat = sum(isinstance(a, NamePatArm) for a in arms)
    n_verbatim = sum(isinstance(a, VerbatimArm) for a in arms)
    n_commented = sum(a.comment is not None for a in arms)
    n_cpp = sum(a.cpp is not None for a in arms)
    duplicates = {name: alts for name, alts in arm_map.items() if len(alts) > 1}

    print(f"region: lines {start + 1}..{end} of {SOURCE.name}")
    print(f"arms: {len(arms)} total = {n_namepat} NamePatArm + {n_verbatim} VerbatimArm")
    print(f"arms with an owned comment: {n_commented}")
    print(f"arms under arm-level CPP: {n_cpp}")
    print(f"distinct names: {len(arm_map)}")
    print("reconstruction check: OK (lossless)")

    if duplicates:
        print("\nduplicate names (guard fall-through, map must keep order):")
        for name, alts in duplicates.items():
            print(f"  {len(alts)}x {name}"
                  + "".join(f" [cpp: {a.cpp}]" if a.cpp else "" for a in alts))
    cpp_arms = [a for a in arms if a.cpp is not None]
    if cpp_arms:
        print("\narms under arm-level CPP:")
        for a in cpp_arms:
            print(f"  {a.name}  {a.cpp}")
    for w in warnings:
        print(f"warning: {w}")

    out = pathlib.Path(__file__).with_name("arms.json")
    out.write_text(json.dumps(
        [{"kind": type(a).__name__, "name": a.name, "comment": a.comment,
          "cpp": a.cpp, "body": a.body} for a in arms],
        indent=2))
    print(f"\nwrote {out}")


if __name__ == "__main__":
    main()
