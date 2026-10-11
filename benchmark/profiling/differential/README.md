# clash-differential

Checks a port to scoped Clash Core (`Clash.Core.Scoped`) against the current
implementation: it runs both on every binding of serialized designs, or at the
sites at which the transformation fired while normalizing them (see
[Comparing at captured sites](#comparing-at-captured-sites)), and compares the
results up to alpha-equivalence.

The harness itself is `Test.Clash.Differential` and
`Test.Clash.Differential.Sites` in `clash-lib/tests`, so unit tests can use it
too. This directory has the executable, the lists of comparisons it can run
(`Comparisons.hs`) and of current transformations (`Registry.hs`), the
current implementations of ported transformations (`Current.hs`), and
demonstrations (`Demo.hs`).

## Running

```
cabal build --builddir=<yours> clash-differential
clash-differential --list
clash-differential [--show N] [--lines N] [--width N] [--binding NAME] COMPARISON FILE...
```

`cabal.project` forces `clash-lib +debug`, which is fine for checking
correctness. For speed, use a temporary project file with `import: cabal.project`
and `package clash-lib` / `flags: -debug`.

`FILE` is either:

* `*.bin`: the inputs of normalization, written by
  `clash-profile-normalization-prepare` (see `../README.profiling.md`);
* `*.net.bin`: normalized bindings, written by `clash-profile-netlist-prepare`.

It prints the first `--show` (default 3) differences of every design, then a
summary per design:

```
Reducer.bin: 464 bindings compared, 460 equal, 2 known-different (rec-to-nonrec: 2), 2 different (1 crashed)
```

A difference shows the binding, the path to the first differing subterm (e.g.
`Lam.body / Letrec.rhs[1] / App.arg`), both subterms, and both results,
truncated to `--lines` lines of `--width` characters. A crash shows where it
happened (either side, converting the current result to scoped Core, or
comparing) and the exception. It exits with 1 if any binding is different, so
it can be used in scripts.

Every binding is compared on its own, starting from the same initial state:
both sides get the same supply of uniques, so new global binders have the same
uniques if both sides draw them in the same order.

## Registering a comparison for a ported transformation

Add a `Comparison` to `comparisons` in `Comparisons.hs`:

```haskell
import Clash.Normalize.Transformations.Case (caseCase)
import qualified Clash.Normalize.Scoped.Transformations.Case as S

  , Comparison
      { cmpName = "caseCase"
      , cmpDescription = "caseCase, to a fixpoint"
      , cmpCurrent = currentRewrite "caseCase" $(compileStrategy (strategyFor Fixpoint caseCase))
      , cmpScoped = scopedRewrite Fixpoint $(asScopedRewriteQ S.caseCase)
      , cmpKnownDifferences = [recToNonRec]
      }
```

then run it on the corpus:

```
clash-differential caseCase inputs/*.bin inputs/*.net.bin
```

* `strategyFor Once t` runs a transformation bottom-up, once at every node;
  `strategyFor Fixpoint t` repeats that until it no longer fires. `t` can also
  be a `chain [t1, t2]`. Use both modes: `Once` finds differences in a single
  step, `Fixpoint` in how the steps interact.
* A transformation might not fire on the inputs of normalization at all, e.g.
  because it only applies to terms that other transformations create. Check
  that the comparison found differences with a deliberately broken port, or
  compare the current transformation against the identity
  (`cmpScoped = pureSide id`): the bindings it reports as different are the ones
  where it fires. If it doesn't fire, also run the comparison on the normalized
  inputs (`*.net.bin`), or compare a chain that includes the transformations
  that create its input.
* `scopedRewrite` runs a `ScopedRewrite` (`Clash.Rewrite.Scoped`): a scoped
  transformation in the normalizer's monad, which gets the context of a node
  and signals that it fired with `changed`. It runs it in the same modes as
  `strategyFor`, with the scoped `bottomupR` and `repeatR`.
  `asScopedRewriteQ` turns a ported `Transformation` into one. For other
  strategies, use `normalizeSide` with `runScopedTransform` of a strategy
  compiled with `compileScopedStrategy`: a side is just a function of a
  binding and its body, and how to run its monad.
* Both sides map the body of a binding to a term, in any monad: define a `Side`
  for another monad, e.g. the evaluator's. Ports whose results are not terms
  (Netlist) need the harness extended with another way to compare results.

### Known differences

A port can intentionally produce different output, e.g. a `NonRec` let where
the current code builds a `Rec` of a single binding. List such differences in
`cmpKnownDifferences`, so that they don't hide real ones. A `KnownDifference`
is a function applied to both results before comparing them again; results
that are only equal after that are counted as known-different, per known
difference. Every known difference needs a name and a description saying why
the difference is intended.

Available:

* `recToNonRec`: a `Rec` of one binding that doesn't refer to itself is a
  `NonRec`.
* `letrecOrder`: the bindings of a `Rec` are in the order in which the body,
  and then the bindings, refer to them. The current code orders bindings by
  unique in places (e.g. `removeUnusedBinders`), where ports keep their order.
* `coercionForm`: a coercion is the primitive `_CO_` without a type argument.
  The primitive reductions of the scoped evaluator apply `_CO_` to its type,
  as GHC2Core does.
* `primTypeForm`: `errorX` at an open type has that type, without a type
  argument. The type of a primitive is closed in scoped Core, so the port of
  `xOptimize` applies it to an open type.

Add new ones to `Test.Clash.Differential`, next to `recToNonRec`, and make them
as narrow as possible. A known difference gets the scope of the results.
`everywhereTm` applies a function at every node, and `everywhereScopedTm` also
gives it the scope of the node, e.g. to create fresh binders.

## Comparing at captured sites

Many transformations never fire on the inputs of normalization: they only
apply to terms that other transformations create, such as `caseCase`,
`caseLet`, `deadCode` and the cast transformations. A comparison on bindings
then proves nothing. So the normalizer can capture the sites at which every
transformation fires (`Clash.Rewrite.Capture`), and `clash-differential` can
run a transformation and its port at those sites.

### Capturing a corpus

Capturing is off unless `CLASH_CAPTURE_REWRITES` is set when the process
starts. `clash-profile-normalization-run` then writes the captures of every
design to the directory it names, as `<design>.corpus.zst`:

```
cabal build --builddir=<yours> clash-profile-normalization-run clash-profile-normalization-prepare
# Inputs of shouldwork designs: `clash_lib_datadir` lets it find the primitives
clash_lib_datadir=$PWD/clash-lib clash-profile-normalization-prepare -itests/shouldwork/GADTs tests/shouldwork/GADTs/Head.hs
# Writes corpus/Head.hs.corpus.zst; give the design without .bin
CLASH_CAPTURE_REWRITES=$PWD/corpus clash-profile-normalization-run tests/shouldwork/GADTs/Head.hs +RTS -A64m
```

Writing the captures keeps many stable names alive, which every garbage
collection traverses: give the runner a big allocation area (`+RTS -A64m`, or
`-A256m` for `wireDemoTest`), or writing takes several times longer.

Per transformation and design, it keeps a uniform sample of 2000 sites
(`CLASH_CAPTURE_LIMIT`), and of 50 sites at which the term or its result has
2000 nodes or more. A capture has the term the transformation was offered, its
result, the context (trimmed to the let-bindings the term can reach), and the
parts of the rewrite state that vary: the supply of uniques, `_curFun`,
`_nameCounter` and `_globalHeap`. The global bindings and the caches of the
normalizer are recorded once, at the end of normalization: they only grow, so
a transformation finds what it found during normalization, and a capture
records which global bindings existed. The caches can have entries that were
added after the site, and the counters of how often a function was inlined or
specialized are those at the end: see the module header of
`Clash.Rewrite.Capture`. That is why a few sites are not reproduced, see
below.

The corpus keeps the sharing of types and variables, and that of terms within
a capture (`Clash.Rewrite.Capture.Encoding`), and is compressed with `zstd`,
which must be installed.

```
clash-differential --corpus-stats corpus/*.corpus.zst
```

prints, per transformation, how often it fired, how many sites were captured
(and how many of those are large), and in how many designs. It lists the
transformations of `Registry.hs` that were never captured.

### Checking that the corpus is faithful

```
clash-differential --replay [--transformation NAME] corpus/*.corpus.zst
```

reruns every captured transformation at its sites, with its current
implementation from `Registry.hs`, and checks that it reproduces the captured
result up to alpha-equivalence. A site that it doesn't reproduce, or where it
doesn't fire, means that the transformation is not deterministic, or that it
depends on state that the capture lacks. On the corpus of the benchmark
designs and a selection of `shouldwork` tests, it reproduces all but 0.03% of
the 74,000 sites, all of `inlineNonRep`, `inlineWorkFree` and
`nonRepSpec`: they depend on the caches and counters that a capture doesn't
record per site. The normalizer runs the port of a ported transformation, so
its captured results are those of the port: its entry in `Registry.hs` lists
the known differences with the port.

### Comparing a port at its sites

Add a `SiteComparison` to `siteComparisons` in `Comparisons.hs`:

```haskell
  , SiteComparison
      { scName = "caseCase"
      , scDescription = "..."
      , scTransformations = ["caseCase"]  -- its names in captures, see --corpus-stats
      , scCurrent = $(asRewriteQ caseCase)
      , scScoped = Just $(asScopedRewriteQ S.caseCase)
      , scKnownDifferences = []
      }
```

Once the port replaced the transformation in the strategies, `caseCase` in
`Clash.Normalize.Transformations` is the port: refer to the current
implementation in `Current.hs`, as `deadCode` does, and move its entry in
`Registry.hs` to the ported ones, with its known differences. Then run it:

```
clash-differential --sites [--show N] [--binding NAME] [--timeout S] caseCase corpus/*.corpus.zst
```

At every site, it runs both sides from the state of the site, with the same
supply of uniques. The current side must reproduce the captured result (see
above); the scoped side runs through `viaFoil`, as the normalizer runs a ported
transformation, and its result is compared with that of the current side, up
to the known differences. A side that takes longer than `--timeout` seconds
(default 20) counts as crashed, unless it loops without allocating: then it
can't be interrupted. It prints the first differences, and a summary per
design:

```
corpus/Reducer.corpus.zst: 7 sites, current side: 1 reproduced, 6 reproduced up to known differences, 0 not reproduced, 0 not fired
  scoped side: 1 equal, 6 known-different (letrec-order: 6), 0 different
```

`--binding NAME` selects the sites in the bindings whose name contains `NAME`.
`Test.Clash.Differential.Sites` also runs comparisons at hand-written sites,
see `clash-lib/tests/Clash/Tests/Differential/Sites.hs`.

## Unit tests

`Test.Clash.Differential` works on small terms too:

```haskell
d <- mkDesign def (mkBindingMap [(f, tm)])
outcome <- compareBinding cmp d (mkBinding f tm)
```

See `clash-lib/tests/Clash/Tests/Differential.hs`.

## Replaying calls of the evaluator

`clash-differential whnf` compares the whnf evaluator of scoped Clash Core
(`Clash.Core.Scoped.Evaluator`) with that of Clash Core, on calls of
`Clash.Rewrite.Util.whnfRW` dumped during normalization:

```
clash-differential whnf [--reps N] [--show N] [--top N] [--call N] DESIGN.bin DUMP... +RTS -T
```

`DESIGN.bin` gives the global bindings and types: the normalization inputs of
the design the calls were dumped from. A dump holds the inputs of the calls:
whether the term is the subject of a case-expression, the let-bindings of its
context, and the term. The instrumented `whnfRW` of the evaluator spike
(branch `eval-spike`, commit `b8bd16587`) writes them with
`WHNF_DUMP=FILE:N`; see `WhnfReplay.hs` for the format.

For every call, both evaluators evaluate the term with the let-bindings of the
context on the heap, and their results are compared up to alpha-equivalence,
after unfolding the bindings the evaluators allocated a few times. It prints
how many agree, and the first `--show` (default 3) differences. With
`+RTS -T`, it also measures the allocation of both evaluators (`--reps` runs,
default 3), and with `--top N`, it lists the N calls on which the scoped
evaluator allocates the most extra. `--call N` replays only the call with
index N.

Measure allocation with `clash-lib -debug` (see above): the debug checks of
the two evaluators differ.
