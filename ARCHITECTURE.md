# Architecture

This document describes the high-level architecture of the Clash compiler. For convenience, this is laid out in an order matching the flow of data through the compiler from Clash source to HDL.

## An Overview of Clash

Clash leverages the front-end of the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) to parse, typecheck and simplify Haskell code to GHC's core representation. From there it is converted to Clash's own internal representation and normalized to produce an AST which can be readily expressed as a netlist. It is then converted to Clash's netlist type, and finally rendered as a hardware design in the specified (traditional) HDL.

```
                          +--------------------+
                          |   Haskell Source   |
                          +--------------------+
                                     |
                                     V
                          +--------------------+
                          |     Clash Core     |
                          +--------------------+
                                     |
                                     V
                          +--------------------+
                          |    Clash Netlist   |
                          +--------------------+
                                     |
          +--------------------------+-------------------------+
          |                          |                         |
          V                          V                         V
+--------------------+    +--------------------+    +--------------------+
|        VHDL        |    |      Verilog       |    |   SystemVerilog    |
+--------------------+    +--------------------+    +--------------------+
```

## Clash Executables

As Clash uses parts of GHC, the Clash executables are tied to the version of GHC that they were compiled with. As a consequence of this, `clash-ghc` is split into different source directories based on the version of GHC supported. The version of GHC used to compile Clash determines which source directory is used.  This is set in `clash-ghc.cabal`:

```
library
  hs-source-dirs: src-ghc, src-bin-common

  if impl(ghc >= 9.0.0)
    hs-source-dirs: src-bin-9.0
  elif impl(ghc >= 8.10.0)
    hs-source-dirs: src-bin-8.10
  ...
```

The directories `clash-ghc/src-ghc` and `clash-ghc/src-bin-common` are where code which is agnostic to GHC version lives. This makes up most of `clash-ghc`, with the version specific source providing the main driver for executables. The actual executables provided are

  * `clash`, provided by `clash-ghc/src-ghc/Batch.hs`
  * `clashi`, provided by `clash-ghc/src-ghc/Interactive.hs`

## Source Structure

  * `clash-lib`

    * `Clash.Core`

      These modules define Clash's core IR, and related functionality (such as substitution and pretty printing)

    * `Clash.Rewrite`

      To normalize programs, Clash uses a rewrite system where individual transformations can be specified and composed to form complex strategies. The fundamental types and combinators for this system are defined in these modules.

    * `Clash.Normalize`

      The specific transformations and the strategy used to normalize terms is defined in these modules. As of Clash 1.6, transformations are further split up into submodules in `Clash.Normalize.Transformations`. In addition, these modules also handle performing normalization on binders, and sanity checks for normalization (such as whether the result of normalization has a recursive call-graph).

    * `Clash.Netlist`

      These modules define Clash's [netlist](https://en.wikipedia.org/wiki/Netlist) representation, and handle the translation from core to netlist.

    * `Clash.Backend`

      These modules provide the translation from Clash's netlist format to code in the desired HDL.

    * `Clash.Driver`

      The driver is responsible for running the Clash core to HDL pipeline, as well as handling caching / manifest files etc.

  * `clash-ghc`

    * `Clash.GHC`

      These modules handle the loading of modules in GHC, and the conversion from GHC's core representation to Clash's core representation.

    * `Clash.GHCi`

      These modules provide the driver for the `clash` and `clashi` executables.

## Haskell Core to Clash Core

  * `Clash.Main.defaultMain`

    The main driver for executables. A lot of this code is similar to the equivalent code in GHC, performing common setup. Some clash-specific setup is also performed here, such as setting GHC's `DynFlags` to include desired language extensions and enabling the typechecker plugins to enhance inference of type-level natural numbers.

  * `Clash.GHC.ClashFlags.parseClashFlags`

    Parse the additional command line arguments that Clash provides, using the command line parsing API from GHC. The data type for the processed command line arguments (`ClashOpts`) is found in `clash-lib/src/Clash/Driver/Types.hs`.

  * `Clash.GHCi.UI.makeHDL`

    This module defines the commands for `clashi` which are available in interactive mode. Most importantly, this module also defines the function `makeHDL`, which starts the process of processing files and generating HDL.

  * `Clash.GHC.GenerateBindings.generateBindings`

    Load modules and primitives, converting GHC core to Clash core for consumption by `clash-lib`. This splits into subtasks, provided by other functions:

    * `Clash.GHC.LoadModules.loadModules`

      Load modules from either their interface file (`*.hi`) or their source (`*.hs`). Modules and their dependencies are loaded as GHC core, which is parsed, typechecked and simplified by GHC. In addition, loading modules also finds annotation on loaded definitions, such as `Synthesize` annotations on top entitiies and test benches.

    * `Clash.GHC.GHC2Core.coreToTerm`

      This translates between GHC core and Clash core. This translation discards information in the GHC core which is not useful to Clash, such as coercions and call stack types. Additionally, this conversion removes most functions on `Signal`, which is treated as transparent in Clash.

    * `Clash.Primitives.Util.generatePrimMap`

      Load primitive and black box definitions from

      * loaded modules and their dependencies (for primitives defined inline with `InlinePrimitive`)
      * locations specified by the HDL backend for code generation
      * locations specified on the command line with the `-iDIR` flag
      * the working directory of the invocation of Clash

## Clash Core to HDL

  * `Clash.Driver.generateHDL`

    This defines the main driver for `clash-lib`, which handles the normalization of top entities and conversion to netlist and finally HDL. Moreover, the driver in `clash-lib` produces a manifest for generated HDL, and uses this to implement features such as caching.

### Normalizing Clash Core

  * `Clash.Driver.normalizeEntity` / `Clash.Normalize.normalize`

    Transform a top entity and it's transitive dependencies to the desired normal form. This is currently a variant of ANF where the result is an identifer, and the main body of an expression is a `letrec` expression. This helps to ensure that everything to be included in the netlist has an identifier associated with it.

  * `Clash.Normalize.Strategy.normalization`

    Normalization uses a rewrite system which applies transformations in the order defined here. The individual transformations are defined in the submodules of `Clash.Normalize.Transformations`. When a binding is normalized, the free variables of the binding are then recursively normalized if they appear in the bindings map.

  * `Clash.Normalize.cleanupGraph`

    After normalization, check that the graph of bindings is not recursive. This is important, as recursive bindings mean Clash will generate an excessive number of HDL files. This pass also performs post-normalization inlining to achieve a more "desirable" netlist, e.g. having functions like `fst = \ds -> case ds of (x, wild) -> x` appear as individual nodes in the netlist is considered "undesirable".

### Clash Core to Netlist

  * `Clash.Netlist.genNetlist`

    Convert the normalized core to a netlist. This representation is closer to a hardware description, specifying components and their connections.

  * `Clash.Netlist.genComponentT`

    Convert a single component to a netlist. This corresponds to an individual entity in VHDL, or a module in Verilog/SystemVerilog. For each component the following is performed:

    * `Clash.Netlist.Util.mkUniqueNormalized`

      Create wrappers / unwrappers for the arguments and results of top entities. Additionally, this ensures that there are no naming conflicts between the identifiers in the generated HDL.

    * `Clash.Netlist.mkNetDecl`

      Create net declarations for all let bindings in the normalized core.

    * `Clash.Netlist.mkDeclarations`

      Create net assignements for all let bindings in the normalized core.

### Netlist to HDL

  * `Clash.Driver.createHDL`

    Convert the produced netlist to code in the specified HDL backend.

    * `Clash.Backend.genHDL`

      Pretty print a single netlist `Component` in HDL.

    * `Clash.Backend.mkTyPackage`

      A types package (VHDL) / module (Verilog and SystemVerilog) is created for the project which contains all type declarations used by the components in the design.

