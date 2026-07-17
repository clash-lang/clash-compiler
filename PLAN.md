# Shape-based transformations — session handoff

**Branch:** `martijn/shape-transformations` (rebased onto upstream master @
`62938cad4`, the merge of PR #3297; commit hashes below are post-rebase).
**Goal:** make shape-based transformations a first-class citizen of the
rewrite system, superseding the four hand-written dispatch commits of PR
#3323 (which will be dropped from that PR). Every transformation declares the
`Term` constructors it can fire on and receives the constructor's fields
directly; traversals match each node's constructor once and dispatch only the
registered transformations. Design decisions (made with Martijn): base on
master as an independent PR; fused traversals (one match drives both dispatch
and descent); replicate #3323's chain semantics exactly (the "guarded
suffix"). This file is a working note — drop it from the branch before
opening the PR.

## What is done (all committed, all building warning-free with `-w ghc-9.12.4`)

1. `7a8271753` — **Engine** (`clash-lib/src/Clash/Rewrite/Shape.hs`):
   `ShapeHandlers` (one `Maybe` handler per constructor + `handleAnyShape`
   escape hatch), `ShapedTransformation` (canonical name + handlers), builders
   (`applyLet`/`applyApp`/…, `onLet`/`onApp`/…, node-receiving `on*Node` for
   spine transformations), `compileBundle`(+`Quiet`) deriving per-constructor
   buckets from the flat member list, the guarded-suffix bucket runner, and
   the fused traversals `topdownBundle`, `bottomupBundle`, `topdownFixBundle`,
   `topdownSucBundle`, `innerMostBundle` (descent arms replicate `allR`
   verbatim, including NonRec→Rec — see Note [NonRec erasure during descent]).
   `apply` in `Clash/Rewrite/Util.hs` is split into the shared `applyWith`
   core. Unit tests: `clash-lib/tests/Clash/Tests/Rewrite/Shape.hs` (35 tests:
   singleton ≡ `apply`, bucket order, guarded suffix, fused ≡ unfused
   reference compositions, context-path probe, NonRec→Rec parity).
2. `67801858a` — **M2**: all normalization transformations converted to
   `NormShapedTransformation` (big single-equation bodies became top-level
   `*Worker` functions to keep diffs reviewable); `inlineBinders`/
   `inlineOrLiftBinders` retyped to Let handlers; strategies rewritten as
   singleton `runShapedTransformation` in the *unchanged* combinator
   structure (bit-for-bit vs master by construction). Escape hatches kept as
   plain rewrites: `makeANF`, `etaExpansionTL`, `recToLetRec`, `topLet`,
   `inlineNonRep`, `collectANF`. Site-specific debug aliases preserved via
   `withTransformationName` ("appProp", "appPropCS", "deadcode", "letFlat").
3. `10aa69bfe` — **M3**: strategies dispatch through compiled bundles + fused
   traversals; the 18-member propagate-and-inline list is one flat
   `compileBundle` list; flatten's cross-shape `reduceConst !-> deadCode`
   stays a combinator between two bundle segments; `applyMany` deleted;
   `topLet` runs via `applyAnyShape` under `topdownSucBundle`.
4. `ff755201e` — changelog entry + copyright years.

## Validation status (M4 — partially done, INTERRUPTED here)

Method: build a master baseline in a worktree, run both compilers over
`examples/*.hs`, compare (a) HDL byte-for-byte (plain flags, verilog+vhdl),
(b) per-name transformation counters (`-fclash-debug-count-transformations`),
(c) the binary rewrite history (`-fclash-debug-history=FILE` — records every
*applied* step: name, context, before/after term; NOT gated on `isDebugging`,
and verified deterministic run-to-run).

**Pitfall discovered:** running the `clash` binary directly (via
`cabal list-bin`) fails at startup with "No BlackBox definition for …"
because the data-file environment is missing — on master too. Always invoke
through `cabal run` from the respective checkout. An earlier validation pass
silently compared empty directories because of this; results below are from
the corrected runner.

Results so far (7 of 14 designs before interruption — ALU, Blinker,
BlockRamTest, Calculator, CHIP8, CochleaPlus, Fifo):

- HDL byte-identical: **7/7** (both backends; real files confirmed present).
- Per-name counters identical: **7/7** (after filtering wall-clock lines —
  the sed range in the runner sweeps in "took N.NNNs" lines; fix that).
- Rewrite history byte-identical: **5/7**; differs for **Calculator** and
  **Fifo**. Analysis: counts and HDL identical, so these are step-*order*
  permutations — the expected grouped-dispatch effect (after a mid-chain
  constructor change, members of *other* buckets defer to the next fixpoint
  pass instead of running later in the same flat chain). This is precisely
  the semantics PR #3323 validated as HDL-preserving.

Validation runner: `tmp/shape-validate.sh` (untracked; recreate from the
description above if absent — key points: run via `cabal run -v0 clash -w
ghc-9.12.4 --` from each checkout, absolute paths for `-i`/`-outputdir`/
`-fclash-hdldir`, baseline in a worktree). NOTE: the branch was rebased onto
`62938cad4` after the partial validation ran, so rebuild the baseline from
the branch's parent, not local `master`:
`git worktree add tmp/master-baseline 62938cad4 && cd tmp/master-baseline &&
cabal build clash-ghc -w ghc-9.12.4`. The earlier 7-design results compared
the pre-rebase branch against its then-base (`0d32dde3a`) and were valid for
that pairing; rerun the whole sweep against the new base.

## Remaining work, in order

1. **Confirm the history-divergence attribution**: build clash at the M2
   commit (`67801858a`, worktree) and check Fifo + Calculator history against
   the base (`62938cad4`). Expected: byte-identical at M2 (singleton dispatch
   is order preserving); the permutation appears only at M3 (grouping). If M2
   already differs, something is wrong — bisect before proceeding.
2. **Finish the examples sweep** (remaining designs: FIR, MAC, MatrixVect,
   Queens, Reducer, Sprockell, Windows) with the corrected runner; require
   HDL + counters identical everywhere, history identical modulo the analyzed
   grouping permutation.
3. **Full testsuite**: `cabal run clash-testsuite -w ghc-9.12.4 --
   --auto-detect-tools --hide-successes -j12` — `-p .Verilog` for triage
   first, then all backends for the final check. Run in the background with a
   generous (~1h) timeout; don't edit clash-lib/clash-ghc while it runs.
4. **Invariants spot check**: a couple of designs with
   `-fclash-debug-invariants` through `cabal run`, branch vs master (the
   `applyDebug` allowlist `["caseCon","reduceConst","constantSpec"]` keys on
   names that must still match).
5. `scripts/build-with-all-supported-ghcs` before opening the PR.
6. **PR presentation**: this refactor answers Martijn's review comment on
   PR #3323 (transformations must not match constructors themselves;
   `applyLet`/`applyApp`; combinators dispatch, matching once). Perf
   validation (benchmark-normalization / bittide wireDemoTest) is Martijn's
   own follow-up — do not include it. Accepted observable deviations to
   document in the PR text: debug-only ("Trying:" lines for structurally
   impossible attempts disappear; step order within a fixpoint pass can
   permute under grouped dispatch, counts and results unchanged).

## Explicit non-goals (rejected or deferred during planning)

- First-class spine buckets with a shared `collectArgsTicks` (defer; root-only
  dispatch would change `inlineWorkFree`/`inlineSmall` semantics).
- Preserving `NonRec` through descent (real behavior change; own PR; see
  Note [NonRec erasure during descent]).
- "Re-dispatch on change" simplification of the guarded suffix (not
  bit-for-bit).
- Predicate pre-checks like the rejected `reduceNonRepPrimCouldFire`.

## House rules for this repo (from earlier feedback)

Build with `-w ghc-9.12.4`. Ad-hoc artifacts under `tmp/` inside the repo,
never `/tmp`. Never pipe cabal through `head` (use `tee` + grep on the log).
Long testsuite runs in the background. American English; no abbreviations in
new identifiers.
