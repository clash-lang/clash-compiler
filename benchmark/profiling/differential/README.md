# clash-differential

Checks a port to scoped Clash Core (`Clash.Core.Scoped`) against the current
implementation: it runs both on every binding of serialized designs, and
compares the results up to alpha-equivalence.

The harness itself is `Test.Clash.Differential` in `clash-lib/tests`, so unit
tests can use it too. This directory has the executable, the list of
comparisons it can run (`Comparisons.hs`), and demonstrations (`Demo.hs`).

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
      , cmpScoped = scopedRewrite Fixpoint S.caseCase
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
* `scopedRewrite` runs a `ScopedRewrite`: a function of the scope and a term in
  the normalizer's monad, which signals that it fired with `changed`. It is a
  stand-in until the scoped rewrite machinery and its strategies are there.
  Once they are, use `normalizeSide` with whatever runs a scoped strategy on
  the body of a binding: a side is just a function of a binding and its body,
  and how to run its monad.
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

Add new ones to `Test.Clash.Differential`, next to `recToNonRec`, and make them
as narrow as possible. `everywhereTm` applies a function at every node.

## Unit tests

`Test.Clash.Differential` works on small terms too:

```haskell
d <- mkDesign def (mkBindingMap [(f, tm)])
outcome <- compareBinding cmp d (mkBinding f tm)
```

See `clash-lib/tests/Clash/Tests/Differential.hs`.
