# Inspection mechanism for optimizations in the CλaSH compiler

Terminal-based interactive UI, where you replay the optimization performed
by a previous run of the normalization phase.

The implementation is based on the `rewrite-inspector` library, available on [Hackage](http://hackage.haskell.org/package/rewrite-inspector).
For detailed documentation and example visualizations, see the following [README](https://github.com/omelkonian/rewrite-inspector/blob/master/README.md).

## Usage Instructions

0. Enable the `history` flag on the clash-lib package, by adding to your cabal.project.local:
```
package clash-lib
  flags: history
```

1. Run `clash`, which will write the rewrite history in `history.dat`:
```bash
> cabal v2-run -- clash --vhdl examples/ALU.hs
```

2. The rewrite history has now been dumped to the file system, in `history.dat`.

3. Run the terminal UI with the `clash-term` executable, giving the history file as input:
```bash
> cabal v2-run -- clash-term history.dat
```
