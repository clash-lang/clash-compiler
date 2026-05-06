# Hacking on Clash

This document covers tips for working on the Clash source tree itself. For
user-facing documentation, see [README.md](README.md). For an overview of the
codebase, see [ARCHITECTURE.md](ARCHITECTURE.md).

## General

### Test suites

The repository contains the following test suites:

| Package          | Suite                  | What it is                                            |
| ---------------- | ---------------------- | ----------------------------------------------------- |
| `clash-prelude`  | `unittests`            | Unit tests for `clash-prelude`                        |
| `clash-prelude`  | `doctests`             | Doctests for `clash-prelude`                          |
| `clash-lib`      | `unittests`            | Unit tests for `clash-lib`                            |
| `clash-lib`      | `doctests`             | Doctests for `clash-lib`                              |
| `clash-testsuite`| (executable)           | End-to-end compiler tests; see below                  |

Copy-pastable commands:

```bash
cabal run clash-prelude:unittests
cabal run clash-prelude:doctests
cabal run clash-lib:unittests
cabal run clash-lib:doctests
cabal run clash-testsuite -- --auto-detect-tools -j$(nproc)
```

### Passing arguments to test suites

`cabal run` treats anything after a bare `--` as arguments to the executable
itself, not to `cabal`. So to forward flags like `-j` (parallelism) or `-p`
(pattern filter) to a test suite, use the double-dash:

```bash
cabal run clash-prelude:doctests -- -j8
cabal run clash-lib:unittests -- -j4 -p Netlist
cabal run clash-testsuite -- --auto-detect-tools -j8 -p .Verilog
```

Without the `--`, flags are interpreted by `cabal` and you'll usually get a
confusing error.

### Switching between GHC versions

Pass `--with-compiler=ghc-A.B.C` to any `cabal` command to build/test with a
specific GHC. The compiler must be available on `$PATH` (e.g. via `ghcup`).

```bash
cabal build all --with-compiler=ghc-9.10.2
cabal run clash-lib:unittests --with-compiler=ghc-9.8.4 -- -j4
```

The script [scripts/build-with-all-supported-ghcs](scripts/build-with-all-supported-ghcs)
loops over all currently supported GHC versions and builds the world with
`-Werror`. Run it before submitting a PR if you've touched code that's
sensitive to GHC version differences.

## Hacking on `clash-prelude`

`clash-prelude` is a normal Haskell library, so most of the time you can just
use `cabal build clash-prelude` / `cabal repl clash-prelude` like any other
package.

HLS (haskell-language-server) does not work reliably on `clash-prelude`. As a
workaround, you can use the [scripts/repld](scripts/repld) helper, which wraps
`ghcid` and auto-restarts on changes to dependencies.

```bash
./scripts/repld clash-prelude        # or: ./scripts/repld p
./scripts/repld clash-prelude:tests  # or: ./scripts/repld p:tests
```

Any extra arguments are passed through to `ghcid`. For instance, to run a test
function `main` after every successful reload:

```bash
./scripts/repld p:tests -T main
```

## Hacking on `clash-lib` / `clash-ghc`

HLS doesn't work well here either. Use [scripts/repld](scripts/repld):

```bash
./scripts/repld clash-lib        # or: ./scripts/repld l
./scripts/repld clash-lib:tests  # or: ./scripts/repld l:tests
./scripts/repld clash-ghc        # or: ./scripts/repld g
```

When changes touch transitive dependencies (e.g. you're hacking `clash-lib`
but `clash-prelude` also changed), `repld` notices and restarts the session.

### `clash-dev`

For interactive Clash hacking, [./clash-dev](clash-dev) starts a `ghci`
session that loads `clash-lib` + `clash-ghc` directly from source (no install
step). It's the fastest way to poke at the compiler from a REPL.

```bash
./clash-dev
```

### `clash-testsuite`

`clash-testsuite` is the end-to-end test runner: it invokes the Clash
compiler on the test cases under [tests/shouldwork](tests/shouldwork) and
[tests/shouldfail](tests/shouldfail), then optionally runs the generated
HDL through external simulators/synthesizers.

```bash
cabal run clash-testsuite -- --hide-successes --auto-detect-tools -j$(nproc)
```

#### `--hide-successes`
The test suite is quite large, so you probably only want to be notified by
failing test, not every test's pass.

#### `--auto-detect-tools`

Many tests rely on external tools (`ghdl`, `iverilog`, `verilator`, `vivado`,
`vsim`/ModelSim, `sby`/SymbiYosys). Passing `--auto-detect-tools` tells the
testsuite to look on `$PATH` and silently skip tests for any tool that isn't
installed. Without it, you'd have to spell out `--no-ghdl --no-iverilog ...`
for everything you don't have.

#### `-j` (parallelism)

`-jN` runs up to `N` tests in parallel. Cranking this up to `$(nproc)` is usually
a big win. You need about 2GB of RAM for each thread.

```bash
cabal run clash-testsuite -- --auto-detect-tools -j$(nproc)
```

#### `-p` (pattern filter)

`-p PATTERN` is a [tasty pattern][tasty-patterns] that restricts which tests
run. A few useful examples:

```bash
# Only Verilog backend tests
cabal run clash-testsuite -- --auto-detect-tools -j8 -p .Verilog

# Anything with "clash" in the path
cabal run clash-testsuite -- --auto-detect-tools -j8 -p clash

# A specific test
cabal run clash-testsuite -- --auto-detect-tools -j8 -p BlockRamFile

# Combine: VHDL backend tests under shouldfail
cabal run clash-testsuite -- --auto-detect-tools -j8 -p .VHDL -p shouldfail
```

A leading `.` matches a path component exactly, which is handy for backend
filters (`.Verilog`, `.VHDL`, `.SystemVerilog`).

[tasty-patterns]: https://github.com/UnkindPartition/tasty#patterns
