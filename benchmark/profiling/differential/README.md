# clash-differential

Compares serialized designs of Clash Core (`Clash.Core`):

* `normalized`: the normalized bindings of two runs of the normalizer, e.g.
  at two commits, up to alpha-equivalence;
* `binary`: the bindings of a design with the result of the round trip
  through the `Binary` instances of Clash Core
  (`Clash.Core.Binary`).

## Running

```
cabal build --builddir=<yours> clash-differential
clash-differential normalized [OPTIONS] DIR_A DIR_B DESIGN.bin...
clash-differential binary [OPTIONS] FILE...
```

`cabal.project` forces `clash-lib +debug`, which is fine for checking
correctness. For speed, use a temporary project file with `import: cabal.project`
and `package clash-lib` / `flags: -debug`.

Give it a hard heap limit, e.g. `+RTS -M16g -RTS`: some designs are large.

Options:

* `--show N`: print the first `N` differences per design (default 3);
* `--lines N`: print at most `N` lines of a term (default 30);
* `--width N`: print at most `N` characters of a line (default 200).

A difference shows the binding, the path to the first differing subterm (e.g.
`Lam.body / Letrec.rhs[1] / App.arg`), both subterms, and both terms,
truncated to `--lines` lines of `--width` characters. Both commands exit with
1 if any binding is different, so they can be used in scripts.

## Comparing whole normalizations

`clash-profile-normalization-run` writes the normalized bindings of every
design to `DIR/<design>.norm.bin` when `CLASH_NORMALIZED_OUT=DIR` is set.

```
clash-differential normalized [--show N] DIR_A DIR_B DESIGN.bin...
```

compares the normalized bindings of two runs binding by binding, up to
alpha-equivalence, and the types of their binders. `DESIGN.bin` are the
inputs of normalization, written by `clash-profile-normalization-prepare`
(see `../README.profiling.md`), for the top entity. The globals that
normalization creates get different uniques in both runs, so globals are
matched by where they are used: from the top entity on, the globals that two
matched bindings refer to are matched by the order in which they occur (see
`Normalized.hs`). It reports the bindings that differ, and the globals that
have a normalized binding in one run only:

```
Reducer.bin: 1 bindings compared, 1 equal, 0 different, 0 missing, 0 with binders of different types; 1 and 1 normalized bindings
```

## Checking the round trip of `Binary`

```
clash-differential binary [--show N] FILE...
```

(or `roundtrip`) encodes and decodes the term and the type of every binding of
the serialized designs, and compares the result with the original, up to
alpha-equivalence. `FILE` is either:

* `*.bin`: the inputs of normalization, written by
  `clash-profile-normalization-prepare`;
* `*.net.bin`: the inputs of netlist generation, written by
  `clash-profile-netlist-prepare`;
* `*.norm.bin`: normalized bindings, see above.
