#!/usr/bin/env python3
"""Render the parsed ghcPrimStep arms as a HashMap-based dispatch.

Imports parse_arms and emits a Haskell fragment where the giant
`case primName pInfo of` becomes:

  ghcPrimStep tcm isSubj pInfo tys args mach =
    case HashMap.lookup (primName pInfo) ghcPrimStepImpls of
      Just impl -> impl tcm isSubj pInfo tys args mach
      Nothing -> Nothing

  ghcPrimStepImpls :: HashMap FastString PrimStep
  ghcPrimStepImpls = HashMap.fromList [ (key, impl), ... ]

Scoping: the original arm bodies use helpers (`ty`, `reduce`,
`catchDivByZero`, ...) from ghcPrimStep's `where` clause, all of which
capture the six parameters. To keep every body verbatim, that `where`
clause is packaged once into a PrimStepContext record (built by
mkPrimStepContext, whose `where` clause is the original one, extracted
verbatim from the source), and every map entry opens with a
RecordWildCards match that brings the helpers back into scope:

  \\tcm isSubj pInfo tys args mach ->
    case mkPrimStepContext tcm isSubj pInfo tys args mach of
      PrimStepContext{..} <original body: guards and arrow, verbatim>
      _ -> Nothing

The body's guards and arrow attach directly to the PrimStepContext
alternative. The `_ -> Nothing` alternative preserves the original
fall-through semantics (names are unique across arms, so failing all
guards fell through to the case's `_ -> Nothing` in the original too).
It is omitted when the arm cannot fall through — no guards at all, or a
top-level `| otherwise` — since GHC would flag it as a redundant pattern.

Keys are Text by default ($(textNameLit '...) for NamePatArm so names keep
tracking GHC module moves, plain string literals for VerbatimArm via
OverloadedStrings), matching the current primName :: Text. Pass
--faststring-keys to emit FastString keys ($(fsNameLit '...) / fsLit "...")
for once primName has moved to FastString.

Body lines are shifted right by a constant so the layout stays valid at
the deeper nesting; CPP directives stay at column 0. Arm-level CPP,
section headers, owned comments, and blank separators are all re-emitted
in place.
"""

import argparse
import pathlib
import re

from parse_arms import (
    Arm, NamePatArm, RawLine, VerbatimArm, is_operator_name, parse_source,
)

ALT_COLUMN = 10            # column of the PrimStepContext{..} alternative
BODY_SHIFT = ALT_COLUMN - 2  # arms sat at column 2 in the original

USED_HELPERS = [
    ("ty", "Type"),
    ("checkNaturalRange1", "Type -> Integer -> (Natural -> Natural) -> Term"),
    ("checkNaturalRange2",
     "Type -> Integer -> Integer -> (Natural -> Natural -> Natural) -> Term"),
    ("checkNaturalRange", "Type -> [Integer] -> ([Natural] -> Term) -> Term"),
    ("reduce", "Term -> Maybe Machine"),
    ("reduceWith", "Machine -> Term -> Maybe Machine"),
    ("reduceWHNF", "Term -> Maybe Machine"),
    ("reduceWHNF'", "Machine -> Term -> Maybe Machine"),
    ("catchDivByZero", "Term -> Term"),
    ("catchErrorCall", "Term -> Term"),
]
# makeUndefinedIf is deliberately not a field: no body uses it directly
# (only via catchDivByZero/catchErrorCall) and its rank-1 type in the
# where clause would need a RankNTypes field here.

TOKEN_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_'#]*")


def uses_context(body: str) -> bool:
    """Whether the body references any PrimStepContext field. If not, the
    entry matches with PrimStepContext{} instead of {..}, which would draw
    a -Wunused-record-wildcards warning."""
    tokens = set(TOKEN_RE.findall(body))
    return any(name in tokens for name, _ in USED_HELPERS)


def render_key(arm: Arm, fast_strings: bool) -> str:
    if isinstance(arm, VerbatimArm):
        # Verbatim names deliberately do not track module moves.
        return f'fsLit "{arm.name}"' if fast_strings else f'"{arm.name}"'
    quoted = f"'({arm.name})" if is_operator_name(arm.name) else f"'{arm.name}"
    return f"$({'fsNameLit' if fast_strings else 'textNameLit'} {quoted})"


def shift_body(body: str) -> list[str]:
    """Re-indent body lines (all but the first-line fragment) for the deeper
    nesting. CPP directives stay at column 0; blank lines stay blank."""
    shifted = []
    for line in body.split("\n")[1:]:
        if line.strip() == "" or line.startswith("#"):
            shifted.append(line)
        else:
            shifted.append(" " * BODY_SHIFT + line)
    return shifted


def significant(line: str) -> bool:
    stripped = line.strip()
    return stripped != "" and not stripped.startswith("--")


def can_fall_through(body: str) -> bool:
    """Whether the arm's guards can all fail, so the entry needs a
    `_ -> Nothing` alternative. False for conditionless arms (no guards)
    and arms whose top-level guards end in `otherwise`."""
    fragment, *rest = body.split("\n")

    first = fragment.strip() if significant(fragment) else next(
        (line.strip() for line in rest if significant(line)), "")
    assert first.startswith("|") or first.startswith("->"), \
        f"unexpected arm body start: {first!r}"
    if first.startswith("->"):
        return False  # no guards: the alternative always succeeds

    # The arm's own guards sit at the minimum indentation of the body's
    # lines (the fragment shares the pattern line and does not count);
    # `| otherwise` there makes the guards total. Deeper ones are nested.
    def indent(line):
        return len(line) - len(line.lstrip())

    own = [line for line in rest if significant(line) and not line.startswith("#")]
    top = min(map(indent, own), default=None)
    return not any(indent(line) == top and line.strip().startswith("| otherwise")
                   for line in own)


def render_entry(arm: Arm, first: bool, fast_strings: bool) -> list[str]:
    lines = []
    if arm.comment is not None:
        lines.extend(arm.comment.split("\n"))
    fragment = arm.body.split("\n", 1)[0]
    wildcard = "{..}" if uses_context(arm.body) else "{}"
    lines.append(f"  {'[' if first else ','} ( {render_key(arm, fast_strings)}")
    lines.append("    , \\tcm isSubj pInfo tys args mach ->")
    lines.append("        case mkPrimStepContext tcm isSubj pInfo tys args mach of")
    lines.append(f"{' ' * ALT_COLUMN}PrimStepContext{wildcard}{fragment}")
    lines.extend(shift_body(arm.body))
    if can_fall_through(arm.body):
        lines.append(f"{' ' * ALT_COLUMN}_ -> Nothing")
    lines.append("    )")
    return lines


def extract_where_clause(lines: list[str], end: int) -> list[str]:
    """The original `where` clause of ghcPrimStep, verbatim: everything after
    the wildcard arm up to the first column-0 line (top-level code resumes)."""
    assert lines[end + 1].strip() == "where", \
        f"expected `where` after the wildcard arm, got: {lines[end + 1]!r}"
    clause = []
    for line in lines[end + 1:]:
        if line.strip() != "" and not line.startswith(" "):
            break
        clause.append(line)
    while clause and clause[-1].strip() == "":
        clause.pop()
    return clause


def render(fast_strings: bool, revision=None) -> str:
    lines, items, _warnings, _start, end = parse_source(revision)

    arms = [item for item in items if isinstance(item, Arm)]
    names = [arm.name for arm in arms]
    assert len(set(names)) == len(names), \
        "duplicate arm names: fromList keeps the last entry, but the case " \
        "gave the first arm priority — grouping needed"
    first_arm = next(item for item in items if isinstance(item, Arm))
    assert first_arm.cpp is None, \
        "first arm is CPP-conditional: the list opener `[` would land inside #if"

    key_ty = "FastString" if fast_strings else "Text"
    out = [
        "-- Generated by render_hashmap.py (see git history) from",
        "-- Clash/GHC/Evaluator/Primitive.hs — do not edit by hand.",
        "--",
        "-- Needs: LANGUAGE RecordWildCards",
        "--        import qualified Data.HashMap.Strict as HashMap",
    ]
    if fast_strings:
        out += [
            "--        import GHC.Data.FastString (FastString, fsLit)",
            "--        import GHC.Data.FastString.Extra ()  -- Hashable FastString",
            "--        import Clash.Util (fsNameLit)",
            "--        primName :: FastString (or convert at the lookup site)",
        ]
    else:
        out += [
            "--        import Data.Text (Text)",
            "--        import Clash.Util (textNameLit)",
        ]
    out += [
        "",
        "ghcPrimStep :: PrimStep",
        "ghcPrimStep tcm isSubj pInfo tys args mach =",
        "  case HashMap.lookup (primName pInfo) ghcPrimStepImpls of",
        "    Just impl -> impl tcm isSubj pInfo tys args mach",
        "    Nothing -> Nothing",
        "",
        "-- | Helpers from ghcPrimStep's pre-map implementation. This is mostly there to",
        "-- have a way to do a machine-based conversion of the old situation (one gigantic",
        "-- case expression) to the current one (HashMap based lookups).",
        "--",
        "-- TODO: Remove this in favor of a more Haskelly approach?",
        "data PrimStepContext = PrimStepContext",
    ]
    for i, (name, ty) in enumerate(USED_HELPERS):
        out.append(f"  {'{' if i == 0 else ','} {name} :: {ty}")
    out += [
        "  }",
        "",
        "mkPrimStepContext"
        " :: TyConMap -> Bool -> PrimInfo -> [Type] -> [Value] -> Machine",
        " -> PrimStepContext",
        "mkPrimStepContext tcm isSubj pInfo tys args mach = PrimStepContext{..}",
    ]
    out.extend(extract_where_clause(lines, end))
    out += [
        "",
        f"ghcPrimStepImpls :: HashMap.HashMap {key_ty} PrimStep",
        "ghcPrimStepImpls = HashMap.fromList",
    ]

    for item in items:
        if isinstance(item, RawLine):
            out.append(item.text)
        else:
            out.extend(render_entry(item, first=item is first_arm,
                                    fast_strings=fast_strings))
    out += ["  ]", ""]
    return "\n".join(out)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--faststring-keys", action="store_true",
                        help="key on FastString via fsNameLit/fsLit instead "
                             "of Text via textNameLit (for once primName is "
                             "a FastString)")
    parser.add_argument("--revision",
                        help="git revision to read the pre-conversion "
                             "Primitive.hs from (e.g. HEAD); default is the "
                             "working tree")
    parser.add_argument("--output", type=pathlib.Path,
                        default=pathlib.Path(__file__).with_name("GhcPrimStepMap.hs"))
    args = parser.parse_args()

    rendered = render(args.faststring_keys, args.revision)
    args.output.write_text(rendered)
    n_lines = rendered.count("\n")
    print(f"wrote {args.output} ({n_lines} lines, "
          f"{'FastString' if args.faststring_keys else 'Text'} keys)")


if __name__ == "__main__":
    main()
