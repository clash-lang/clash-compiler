# Changelog for the Clash project

## 1.2.5 *November 9th 2020*
Fixed:
  * The normalizeType function now fully normalizes types which require calls to
reduceTypeFamily [#1469](https://github.com/clash-lang/clash-compiler/issues/1469)
  * `flogBaseSNat`, `clogBaseSNat` and `logBaseSNat` primitives are now implemented correctly.Previously these primitives would be left unevaluated causing issues as demonstrated in [#1479](https://github.com/clash-lang/clash-compiler/issues/1469)
  * Specializing on functions with type family arguments no longer fails [#1477](https://github.com/clash-lang/clash-compiler/issues/1477)
  * `satSucc`, `satPred` correctly handle "small types" such as `Index 1`.
  * `msb` no longer fails on values larger than 64 bits
  * `undefined` can now be used as a reset value of `autoReg@Maybe` [#1507](https://github.com/clash-lang/clash-compiler/issues/1507)
  * Signal's `fmap` is now less strict, preventing infinite loops in very specific situations. See [#1521](https://github.com/clash-lang/clash-compiler/issues/1521)
  * Clash now uses correct function names in manifest and sdc files [#1533](https://github.com/clash-lang/clash-compiler/issues/1533)
  * Clash no longer produces erroneous HDL in very specific cases [#1536](https://github.com/clash-lang/clash-compiler/issues/1536)
  * Usage of `fold` inside other HO primitives (e.g., `map`) no longer fails [#1524](https://github.com/clash-lang/clash-compiler/issues/1524)

## 1.2.4 *July 28th 2020*
* Changed:
  * Relaxed upper bound versions of `aeson` and `dlist`, in preparation for the new Stack LTS.
  * Reverted changes to primitive definitions for 'zipWith', 'map', 'foldr', and 'init' introduced in 1.2.2. They have shown to cause problems in very specific circumstances.

## 1.2.3 *July 11th 2020*
* Changed:
  * Upgrade to nixos 20.03. Nix and snap users will now use packages present in 20.03.

* Added:
  * `instance Monoid a => Monoid (Vec n a)`
  * `instance Text.Printf(Index)`
  * `instance Text.Printf(Signed)`
  * `instance Text.Printf(Unsigned)`

* Fixed:
  * Clash renders incorrect VHDL when GHCs Worker/Wrapper transformation is enabled [#1402](https://github.com/clash-lang/clash-compiler/issues/1402)
  * Minor faults in generated HDL when using annotations from `Clash.Annotations.SynthesisAttributes`
  * Cabal installed through Snap (`clash.cabal`) can now access the internet to fetch pacakges. [#1411]https://github.com/clash-lang/clash-compiler/issues/1411
  * Generated QSys file for `altpll` incompatible with Quartus CLI (did work in Quartus GUI)
  * Clash no longer uses component names that clash with identifiers imported
    from:
    * IEEE.STD_LOGIC_1164.all
    * IEEE.NUMERIC_STD.all
    * IEEE.MATH_REAL.all
    * std.textio.all
    when generating VHDL.
    See https://github.com/clash-lang/clash-compiler/issues/1439.

## 1.2.2 *June 12th 2020*
* Changed:
  * The hardwired functions to unroll primitive definitions for 'zipWith', 'map', 'foldr', and 'init' have been changed to only unroll a single step, whereas they would previously unroll the whole definition in one step. This allows Clash to take advantage of the lazy nature of these functions, in turn speeding up compilation speeds significantly in some cases. Part of [PR 1354](https://github.com/clash-lang/clash-compiler/pull/1354).

* Added:
  * Support for GHC 8.10
  * Ability to load designs from precompiled modules (i.e., stored in a package database). See [#1172](https://github.com/clash-lang/clash-compiler/pull/1172)
  * Support for '-main-is' when used with `--vhdl`, `--verilog`, or `--systemverilog`
  * A partial instance for `NFDataX (Signal domain a)`

* Fixed:
  * Clash's evaluator now inlines work free definitions, preventing [situations where it would otherwise get stuck in an infinite loop](https://github.com/clash-lang/clash-compiler/pull/1354#issuecomment-635430374)
  * `caseCon` doesn't apply type-substitution correctly [#1340](https://github.com/clash-lang/clash-compiler/issues/1340)
  * Clash generates illegal SystemVerilog slice [#1313](https://github.com/clash-lang/clash-compiler/issues/1313)
  * Fix result type of head and tail Verilog blackboxes [#1351](https://github.com/clash-lang/clash-compiler/issues/1351)
  * Certain recursive let-expressions in side a alternatives of a case-expression throw the Clash compiler into an infinite loop [#1316](https://github.com/clash-lang/clash-compiler/issues/1316)
  * Fixes issue with one of Clash's transformations, `inlineCleanup`, introducing free variables [#1337](https://github.com/clash-lang/clash-compiler/issues/1337)
  * Fails to propagate type information of existential type [#1310](https://github.com/clash-lang/clash-compiler/issues/1310)
  * Certain case-expressions throw the Clash compiler into an infinite loop [#1320](https://github.com/clash-lang/clash-compiler/issues/1320)
  * Added blackbox implementation for 'Clash.Sized.Vector.iterateI', hence making it usable as a register reset value [#1240](https://github.com/clash-lang/clash-compiler/issues/1240)
  * `iterate` and `iterateI` can now be used in reset values [#1240](https://github.com/clash-lang/clash-compiler/issues/1240)
  * Prim evaluation fails on undefined arguments [#1297](https://github.com/clash-lang/clash-compiler/issues/1297)
  * Missing re-indexing in (Un)Signed fromSLV conversion [#1292](https://github.com/clash-lang/clash-compiler/issues/1292)
  * VHDL: generate a type qualification inside ~TOBV, fixes [#1360](https://github.com/clash-lang/clash-compiler/issues/1360)

## 1.2.1 *April 23rd 2020*
* Changed:
  * Treat `Signed 0`, `Unsigned 0`, `Index 1`, `BitVector 0` as unit. In effect this means that 'minBound' and 'maxBound' return 0, whereas previously they might crash [#1183](https://github.com/clash-lang/clash-compiler/issues/1183)
  * Infix use of `deepseqX` is now right-associative

* Added:
  * Add 'natToInteger', 'natToNatural', and 'natToNum'. Similar to 'snatTo*', but works solely on a type argument instead of an SNat.
  * `Clash.Sized.Vector.unfoldr` and `Clash.Sized.Vector.unfoldrI` to construct vectors from a seed value
  * Added NFDataX instances for `Data.Monoid.{First,Last}`

* Fixed:
  * The Verilog backend can now deal with non-contiguous ranges in custom bit-representations.
  * Synthesizing BitPack instances for type with phantom parameter fails [#1242](https://github.com/clash-lang/clash-compiler/issues/1242)
  * Synthesis of `fromBNat (toBNat d5)` failed due to `unsafeCoerce` coercing from `Any`
  * Memory leak in register primitives [#1256](https://github.com/clash-lang/clash-compiler/issues/1256)
  * Illegal VHDL slice when projecting nested SOP type [#1254](https://github.com/clash-lang/clash-compiler/issues/1254)
  * Vivado VHDL code path (`-fclash-hdlsyn Vivado`) generates illegal VHDL [#1264](https://github.com/clash-lang/clash-compiler/issues/1264)

## 1.2.0 *March 5th 2020*
As promised when releasing 1.0, we've tried our best to keep the API stable. We
think most designs will continue to compile with this new version, although special
care needs to be taken when using:

  * Use inline blackboxes. Instead of taking a single HDL, inline primitives now
    take multiple. For example, `InlinePrimitive VHDL ".."` must now be written
    as `InlinePrimitive [VHDL] ".."`.

  * Use the `Enum` instance for `BitVector`, `Index`, `Signed`, or `Unsigned`, as
    they now respect their `maxBound`. See [#1089](https://github.com/clash-lang/clash-compiler/issues/1089).

On top of that, we've added a number of new features:

  * `makeTopEntity`: Template Haskell function for generating TopEntity annotations. See [the documentation on Haddock](http://hackage.haskell.org/package/clash-prelude-1.2.0/docs/Clash-Annotations-TopEntity.html) for more information.

  * `Clash.Explicit.SimIO`: ((System)Verilog only) I/O actions that can be translated to HDL I/O. See [the documentation on Haddock](http://hackage.haskell.org/package/clash-prelude-1.2.0/docs/Clash-Explicit-SimIO.html) for more information.

  * `Clash.Class.AutoReg`: A smart register that improves the chances of synthesis tools inferring clock-gated registers, when used. See [the documentation on Haddock](http://hackage.haskell.org/package/clash-prelude-1.2.0/docs/Clash-Class-AutoReg.html) for more information.

The full list of changes follows. Happy hacking!

* New features (API):
  * `Clash.Class.Parity` type class replaces Prelude `odd` and `even` functions due to assumptions that don't hold for Clash specific numerical types, see [#970](https://github.com/clash-lang/clash-compiler/pull/970).
  * `NFDataX.ensureSpine`, see [#748](https://github.com/clash-lang/clash-compiler/pull/803)
  * `makeTopEntity` Template Haskell function for generating TopEntity annotations
    intended to cover the majority of use cases. Generation failures should either
    result in an explicit error, or a valid annotation of an empty `PortProduct`.
    Any discrepancy between the _shape_ of generated annotations and the _shape_
    of the Clash compiler is a bug. See [#795](https://github.com/clash-lang/clash-compiler/pull/795).
    Known limitations:
    * Type application (excluding `Signal`s and `:::`) is best effort:
    * Data types with type parameters will work if the generator can discover a single relevant constructor after attempting type application.
    * Arbitrary explicit clock/reset/enables are supported, but only a single `HiddenClockResetEnable` constraint is supported.
    * Data/type family support is best effort.
  * Added `Bundle ((f :*: g) a)` instance
  * Added `NFDataX CUShort` instance
  * Clash's internal type family solver now recognizes `AppendSymbol` and `CmpSymbol`
  * Added `Clash.Magic.suffixNameFromNat`: can be used in cases where `suffixName` is too slow
  * Added `Clash.Class.AutoReg`. Improves the chances of synthesis tools inferring clock-gated registers, when used. See [#873](https://github.com/clash-lang/clash-compiler/pull/873).
  * `Clash.Magic.suffixNameP`, `Clash.Magic.suffixNameFromNatP`: enable prefixing of name suffixes
  * Added `Clash.Magic.noDeDup`: can be used to instruct Clash to /not/ share a function between multiple branches
  * A `BitPack a` constraint now implies a `KnownNat (BitSize a)` constraint, so you won't have to add it manually anymore. See [#942](https://github.com/clash-lang/clash-compiler/pull/942).
  * `Clash.Explicit.SimIO`: ((System)Verilog only) I/O actions that can be translated to HDL I/O; useful for generated test benches.
  * Export `Clash.Explicit.Testbench.assertBitVector` [#888](https://github.com/clash-lang/clash-compiler/pull/888/files)
  * Add `Clash.Prelude.Testbench.assertBitVector` to achieve feature parity with `Clash.Explicit.Testbench`. [#891](https://github.com/clash-lang/clash-compiler/pull/891/files)
  * Add `Clash.XException.NFDataX.ensureSpine` [#803](https://github.com/clash-lang/clash-compiler/pull/803)
  * Add `Clash.Class.BitPack.bitCoerceMap` [#798](https://github.com/clash-lang/clash-compiler/pull/798)
  * Add `Clash.Magic.deDup`: instruct Clash to force sharing an operator between multiple branches of a case-expression
  * `InlinePrimitive` can now support multiple backends simultaneously [#425](https://github.com/clash-lang/clash-compiler/issues/425)
  * Add `Clash.XException.hwSeqX`: render declarations of an argument, but don't assign it to a result signal
  * Add `Clash.Signal.Bundle.TaggedEmptyTuple`: allows users to emulate the pre-1.0 behavior of "Bundle ()". See [#1100](https://github.com/clash-lang/clash-compiler/pull/1100)

* New features (Compiler):
  * [#961](https://github.com/clash-lang/clash-compiler/pull/961): Show `-fclash-*` Options in `clash --show-options`

* New internal features:
  * [#918](https://github.com/clash-lang/clash-compiler/pull/935): Add X-Optimization to normalization passes (-fclash-aggressive-x-optimization)
  * [#821](https://github.com/clash-lang/clash-compiler/pull/821): Add `DebugTry`: print name of all tried transformations, even if they didn't succeed
  * [#856](https://github.com/clash-lang/clash-compiler/pull/856): Add `-fclash-debug-transformations`: only print debug info for specific transformations
  * [#911](https://github.com/clash-lang/clash-compiler/pull/911): Add 'RenderVoid' option to blackboxes
  * [#958](https://github.com/clash-lang/clash-compiler/pull/958): Prefix names of inlined functions
  * [#947](https://github.com/clash-lang/clash-compiler/pull/947): Add "Clash.Core.TermLiteral"
  * [#887](https://github.com/clash-lang/clash-compiler/pull/887): Show nicer error messages when failing in TH code
  * [#884](https://github.com/clash-lang/clash-compiler/pull/884): Teach reduceTypeFamily about AppendSymbol and CmpSymbol
  * [#784](https://github.com/clash-lang/clash-compiler/pull/784): Print whether `Id` is global or local in ppr output
  * [#781](https://github.com/clash-lang/clash-compiler/pull/781): Use naming contexts in register names
  * [#1061](https://github.com/clash-lang/clash-compiler/pull/1061): Add 'usedArguments' to BlackBoxHaskell blackboxes

* Fixes issues:
  * [#974](https://github.com/clash-lang/clash-compiler/issues/974): Fix indirect shadowing in `reduceNonRepPrim`
  * [#964](https://github.com/clash-lang/clash-compiler/issues/964): SaturatingNum instance of `Index` now behaves correctly when the size of the index overflows
  an `Int`.
  * [#810](https://github.com/clash-lang/clash-compiler/issues/810): Verilog backend now correctly specifies type of `BitVector 1`
  * [#811](https://github.com/clash-lang/clash-compiler/issues/811): Improve module load behavior in clashi
  * [#439](https://github.com/clash-lang/clash-compiler/issues/439): Template Haskell splices and TopEntity annotations can now be used in clashi
  * [#662](https://github.com/clash-lang/clash-compiler/issues/662): Clash will now constant specialize partially constant constructs
  * [#700](https://github.com/clash-lang/clash-compiler/issues/700): Check work content of expression in cast before warning users. Should eliminate a lot of (superfluous) warnings about "specializing on non work-free cast"s.
  * [#837](https://github.com/clash-lang/clash-compiler/issues/837): Blackboxes will now report clearer error messages if they're given unexpected arguments.
  * [#869](https://github.com/clash-lang/clash-compiler/issues/869): PLL is no longer duplicated in Blinker.hs example
  * [#749](https://github.com/clash-lang/clash-compiler/issues/749): Clash's dependencies now all work with GHC 8.8, allowing `clash-{prelude,lib,ghc}` to be compiled from Hackage soon.
  * [#871](https://github.com/clash-lang/clash-compiler/issues/871): RTree Bundle instance is now properly lazy
  * [#895](https://github.com/clash-lang/clash-compiler/issues/895): VHDL type error when generating `Maybe (Vec 2 (Signed 8), Index 1)`
  * [#880](https://github.com/clash-lang/clash-compiler/issues/880): Custom bit representations can now be used on product types too
  * [#976](https://github.com/clash-lang/clash-compiler/issues/976): Prevent shadowing in Clash's core evaluator
  * [#1007](https://github.com/clash-lang/clash-compiler/issues/1007): Can't translate domain tagType.Errors.IfStuck...
  * [#967](https://github.com/clash-lang/clash-compiler/issues/967): Naming registers disconnects their output
  * [#990](https://github.com/clash-lang/clash-compiler/issues/990): Internal shadowing bug results in incorrect HDL
  * [#945](https://github.com/clash-lang/clash-compiler/issues/945): Rewrite rules for Vec Applicative Functor
  * [#919](https://github.com/clash-lang/clash-compiler/issues/919): Clash generating invalid Verilog after Vec operations #919
  * [#996](https://github.com/clash-lang/clash-compiler/issues/996): Ambiguous clock when using `ClearOnReset` and `resetGen` together
  * [#701](https://github.com/clash-lang/clash-compiler/issues/701): Unexpected behaviour with the `Synthesize` annotation
  * [#694](https://github.com/clash-lang/clash-compiler/issues/694): Custom bit representation error only with VHDL
  * [#347](https://github.com/clash-lang/clash-compiler/issues/347): topEntity synthesis fails due to insufficient type-level normalisation
  * [#626](https://github.com/clash-lang/clash-compiler/issues/626): Missing Clash.Explicit.Prelude definitions
  * [#960](https://github.com/clash-lang/clash-compiler/issues/626): Blackbox Error Caused by Simple map
  * [#1012](https://github.com/clash-lang/clash-compiler/issues/1012): Case-let doesn't look through ticks
  * [#430](https://github.com/clash-lang/clash-compiler/issues/430): Issue warning when not compiled with `executable-dynamic: True`
  * [#374](https://github.com/clash-lang/clash-compiler/issues/1012): Clash.Sized.Fixed: fromInteger and fromRational don't saturate correctly
  * [#836](https://github.com/clash-lang/clash-compiler/issues/836): Generate warning when `toInteger` blackbox drops MSBs
  * [#1019](https://github.com/clash-lang/clash-compiler/issues/1019): Clash breaks on constants defined in terms of `GHC.Natural.gcdNatural`
  * [#1025](https://github.com/clash-lang/clash-compiler/issues/1025): `inlineCleanup`will not produce empty letrecs anymore
  * [#1030](https://github.com/clash-lang/clash-compiler/issues/1030): `bindConstantVar` will bind (workfree) constructs
  * [#1034](https://github.com/clash-lang/clash-compiler/issues/1034): Error (10137): object "pllLock" on lhs must have a variable data type
  * [#1046](https://github.com/clash-lang/clash-compiler/issues/1046): Don't confuse term/type namespaces in 'lookupIdSubst'
  * [#1041](https://github.com/clash-lang/clash-compiler/issues/1041): Nested product types incorrectly decomposed into ports
  * [#1058](https://github.com/clash-lang/clash-compiler/issues/1058): Prevent substitution warning when using type equalities in top entities
  * [#1033](https://github.com/clash-lang/clash-compiler/issues/1033): Fix issue where Clash breaks when using Clock/Reset/Enable in product types in combination with Synthesize annotations
  * [#1075](https://github.com/clash-lang/clash-compiler/issues/1075): Removed superfluous constraints on 'maybeX' and 'maybeIsX'
  * [#1085](https://github.com/clash-lang/clash-compiler/issues/1085): Suggest exporting topentities if they can't be found in a module
  * [#1065](https://github.com/clash-lang/clash-compiler/pull/1065): Report polymorphic topEntities as errors
  * [#1089](https://github.com/clash-lang/clash-compiler/issues/1089): Respect maxBound in Enum instances for BitVector,Index,Signed,Unsigned

* Fixes without issue reports:
  * Fix bug in `rnfX` defined for `Down` ([baef30e](https://github.com/clash-lang/clash-compiler/commit/baef30eae03dc02ba847ffbb8fae7f365c5287c2))
  * Render numbers inside gensym ([bc76f0f](https://github.com/clash-lang/clash-compiler/commit/bc76f0f1934fd6e6ed9c33bcf950dae21e2f7903))
  * Report blackbox name when encountering an error in 'setSym' ([#858](https://github.com/clash-lang/clash-compiler/pull/858))
  * Fix blackbox issues causing Clash to generate invalid HDL ([#865](https://github.com/clash-lang/clash-compiler/pull/865))
  * Treat types with a zero-width custom bit representation like other zero-width constructs ([#874](https://github.com/clash-lang/clash-compiler/pull/874))
  * TH code for auto deriving bit representations now produces nicer error messages ([7190793](https://github.com/clash-lang/clash-compiler/commit/7190793928545f85157f9b8d4b8ec2edb2cd8a26))
  * Adds '--enable-shared-executables' for nix builds; this should make Clash run _much_ faster ([#894](https://github.com/clash-lang/clash-compiler/pull/894))
  * Custom bit representations can now mark fields as zero-width without crashing the compiler ([#898](https://github.com/clash-lang/clash-compiler/pull/898))
  * Throw an error if there's data left to parse after successfully parsing a valid JSON construct ([#904](https://github.com/clash-lang/clash-compiler/pull/904))
  * `Data.gfoldl` is now manually implemented, in turn fixing issues with `gshow` ([#933](https://github.com/clash-lang/clash-compiler/pull/933))
  * Fix a number of issues with blackbox implementations ([#934](https://github.com/clash-lang/clash-compiler/pull/934))
  * Don't inline registers with non-constant clock and reset ([#998](https://github.com/clash-lang/clash-compiler/pull/998))
  * Inline let-binders called [dsN | N <- [1..]] ([#992](https://github.com/clash-lang/clash-compiler/pull/992))
  * ClockGens use their name at the Haskell level [#827](https://github.com/clash-lang/clash-compiler/pull/827)
  * Render numbers inside gensym [#809](https://github.com/clash-lang/clash-compiler/pull/809)
  * Don't overwrite existing binders when specializing [#790](https://github.com/clash-lang/clash-compiler/pull/790)
  * Deshadow in 'caseCase' [#1067](https://github.com/clash-lang/clash-compiler/pull/1067)
  * Deshadow in 'caseLet' and 'nonRepANF' [#1071](https://github.com/clash-lang/clash-compiler/pull/1071)

* Deprecations & removals:
  * Removed support for GHC 8.2 ([#842](https://github.com/clash-lang/clash-compiler/pull/842))
  * Removed support for older cabal versions, only Cabal >=2.2 supported ([#851](https://github.com/clash-lang/clash-compiler/pull/851))
  * Reset and Enable constructors are now only exported from Clash.Signal.Internal
  * [#986](https://github.com/clash-lang/clash-compiler/issues/986) Remove -fclash-allow-zero-width flag

## 1.0.0 *September 3rd 2019*
* 10x - 50x faster compile times
* New features:
  * API changes: check the migration guide at the end of `Clash.Tutorial`
  * All memory elements now have an (implicit) enable line; "Gated" clocks have been removed as the clock wasn't actually gated, but implemented as an enable line.
  * Circuit domains are now configurable in:
    * (old) The clock period
    * (new) Clock edge on which memory elements latch their inputs (rising edge or falling edge)
    * (new) Whether the reset port of a memory element is level sensitive asynchronous reset) or edge sensitive (synchronous reset)
    * (new) Whether the reset port of a memory element is active-high or active-low (negated reset)
    * (new) Whether memory element power on in a configurable/defined state (common on FPGAs) or in an undefined state (ASICs)

    * See the [blog post](https://clash-lang.org/blog/0005-synthesis-domain/) on this new feature
  * Data types can now be given custom bit-representations: http://hackage.haskell.org/package/clash-prelude/docs/Clash-Annotations-BitRepresentation.html
  * Annotate expressions with attributes that persist in the generated HDL,
    e.g. synthesis directives: http://hackage.haskell.org/package/clash-prelude/docs/Clash-Annotations-SynthesisAttributes.html
  * Control (System)Verilog module instance, and VHDL entity instantiation names
    in generated code: http://hackage.haskell.org/package/clash-prelude/docs/Clash-Magic.html
  * Much improved infrastructure for handling of unknown values: defined spine,
    but unknown leafs: http://hackage.haskell.org/package/clash-prelude/docs/Clash-XException.html#t:NFDataX
  * Experimental: Multiple hidden clocks. Can be enabled by compiling
    `clash-prelude` with `-fmultiple-hidden`
  * Experimental: Limited GADT support (pattern matching on vectors, or custom
    GADTs as longs as their usage can be statically removed; no support of
    recursive GADTs)
  * Experimental: Use regular Haskell functions to generate HDL black boxes for
    primitives (in an addition to existing string templates for HDL black boxes)
    See for example: http://hackage.haskell.org/package/clash-lib/docs/Clash-Primitives-Intel-ClockGen.html

* Fixes issues:
  * [#316](https://github.com/clash-lang/clash-compiler/issues/316)
  * [#319](https://github.com/clash-lang/clash-compiler/issues/319)
  * [#323](https://github.com/clash-lang/clash-compiler/issues/323)
  * [#324](https://github.com/clash-lang/clash-compiler/issues/324)
  * [#329](https://github.com/clash-lang/clash-compiler/issues/329)
  * [#331](https://github.com/clash-lang/clash-compiler/issues/331)
  * [#332](https://github.com/clash-lang/clash-compiler/issues/332)
  * [#335](https://github.com/clash-lang/clash-compiler/issues/335)
  * [#348](https://github.com/clash-lang/clash-compiler/issues/348)
  * [#349](https://github.com/clash-lang/clash-compiler/issues/349)
  * [#350](https://github.com/clash-lang/clash-compiler/issues/350)
  * [#351](https://github.com/clash-lang/clash-compiler/issues/351)
  * [#352](https://github.com/clash-lang/clash-compiler/issues/352)
  * [#353](https://github.com/clash-lang/clash-compiler/issues/353)
  * [#358](https://github.com/clash-lang/clash-compiler/issues/358)
  * [#359](https://github.com/clash-lang/clash-compiler/issues/359)
  * [#363](https://github.com/clash-lang/clash-compiler/issues/363)
  * [#364](https://github.com/clash-lang/clash-compiler/issues/364)
  * [#365](https://github.com/clash-lang/clash-compiler/issues/365)
  * [#371](https://github.com/clash-lang/clash-compiler/issues/371)
  * [#372](https://github.com/clash-lang/clash-compiler/issues/372)
  * [#373](https://github.com/clash-lang/clash-compiler/issues/373)
  * [#378](https://github.com/clash-lang/clash-compiler/issues/378)
  * [#380](https://github.com/clash-lang/clash-compiler/issues/380)
  * [#381](https://github.com/clash-lang/clash-compiler/issues/381)
  * [#382](https://github.com/clash-lang/clash-compiler/issues/382)
  * [#383](https://github.com/clash-lang/clash-compiler/issues/383)
  * [#387](https://github.com/clash-lang/clash-compiler/issues/387)
  * [#393](https://github.com/clash-lang/clash-compiler/issues/393)
  * [#396](https://github.com/clash-lang/clash-compiler/issues/396)
  * [#398](https://github.com/clash-lang/clash-compiler/issues/398)
  * [#399](https://github.com/clash-lang/clash-compiler/issues/399)
  * [#401](https://github.com/clash-lang/clash-compiler/issues/401)
  * [#403](https://github.com/clash-lang/clash-compiler/issues/403)
  * [#407](https://github.com/clash-lang/clash-compiler/issues/407)
  * [#412](https://github.com/clash-lang/clash-compiler/issues/412)
  * [#413](https://github.com/clash-lang/clash-compiler/issues/413)
  * [#420](https://github.com/clash-lang/clash-compiler/issues/420)
  * [#422](https://github.com/clash-lang/clash-compiler/issues/422)
  * [#423](https://github.com/clash-lang/clash-compiler/issues/423)
  * [#424](https://github.com/clash-lang/clash-compiler/issues/424)
  * [#438](https://github.com/clash-lang/clash-compiler/issues/438)
  * [#450](https://github.com/clash-lang/clash-compiler/issues/450)
  * [#452](https://github.com/clash-lang/clash-compiler/issues/452)
  * [#455](https://github.com/clash-lang/clash-compiler/issues/455)
  * [#460](https://github.com/clash-lang/clash-compiler/issues/460)
  * [#461](https://github.com/clash-lang/clash-compiler/issues/461)
  * [#463](https://github.com/clash-lang/clash-compiler/issues/463)
  * [#468](https://github.com/clash-lang/clash-compiler/issues/468)
  * [#475](https://github.com/clash-lang/clash-compiler/issues/475)
  * [#476](https://github.com/clash-lang/clash-compiler/issues/476)
  * [#500](https://github.com/clash-lang/clash-compiler/issues/500)
  * [#507](https://github.com/clash-lang/clash-compiler/issues/507)
  * [#512](https://github.com/clash-lang/clash-compiler/issues/512)
  * [#516](https://github.com/clash-lang/clash-compiler/issues/516)
  * [#517](https://github.com/clash-lang/clash-compiler/issues/517)
  * [#526](https://github.com/clash-lang/clash-compiler/issues/526)
  * [#556](https://github.com/clash-lang/clash-compiler/issues/556)
  * [#560](https://github.com/clash-lang/clash-compiler/issues/560)
  * [#566](https://github.com/clash-lang/clash-compiler/issues/566)
  * [#567](https://github.com/clash-lang/clash-compiler/issues/567)
  * [#569](https://github.com/clash-lang/clash-compiler/issues/569)
  * [#573](https://github.com/clash-lang/clash-compiler/issues/573)
  * [#575](https://github.com/clash-lang/clash-compiler/issues/575)
  * [#581](https://github.com/clash-lang/clash-compiler/issues/581)
  * [#582](https://github.com/clash-lang/clash-compiler/issues/582)
  * [#586](https://github.com/clash-lang/clash-compiler/issues/586)
  * [#588](https://github.com/clash-lang/clash-compiler/issues/588)
  * [#591](https://github.com/clash-lang/clash-compiler/issues/591)
  * [#596](https://github.com/clash-lang/clash-compiler/issues/596)
  * [#601](https://github.com/clash-lang/clash-compiler/issues/601)
  * [#607](https://github.com/clash-lang/clash-compiler/issues/607)
  * [#629](https://github.com/clash-lang/clash-compiler/issues/629)
  * [#637](https://github.com/clash-lang/clash-compiler/issues/637)
  * [#644](https://github.com/clash-lang/clash-compiler/issues/644)
  * [#647](https://github.com/clash-lang/clash-compiler/issues/647)
  * [#661](https://github.com/clash-lang/clash-compiler/issues/661)
  * [#668](https://github.com/clash-lang/clash-compiler/issues/668)
  * [#677](https://github.com/clash-lang/clash-compiler/issues/677)
  * [#678](https://github.com/clash-lang/clash-compiler/issues/678)
  * [#682](https://github.com/clash-lang/clash-compiler/issues/682)
  * [#691](https://github.com/clash-lang/clash-compiler/issues/691)
  * [#703](https://github.com/clash-lang/clash-compiler/issues/703)
  * [#713](https://github.com/clash-lang/clash-compiler/issues/713)
  * [#715](https://github.com/clash-lang/clash-compiler/issues/715)
  * [#727](https://github.com/clash-lang/clash-compiler/issues/727)
  * [#730](https://github.com/clash-lang/clash-compiler/issues/730)
  * [#736](https://github.com/clash-lang/clash-compiler/issues/736)
  * [#738](https://github.com/clash-lang/clash-compiler/issues/738)

## 0.99.3 *July 28th 2018*
* Fixes bugs:
  * Evaluator recognizes `Bit` literals [#329](https://github.com/clash-lang/clash-compiler/issues/329)
  * Use existential type-variables in context of GADT pattern match
  * Do not create zero-bit temporary variables in generated HDL
  * Use correct arguments in nested primitives [#323](https://github.com/clash-lang/clash-compiler/issues/329)
  * Zero-constructor data type needs 0 bits [#238](https://github.com/clash-lang/clash-compiler/issues/238)
  * Create empty component when result needs 0 bits
  * Evaluator performs BigNat arithmetic

* Features:
  * Bundle and BitPack instances up to and including 62-tuples
  * Handle undefined writes to RAM properly
  * Handle undefined clock enables properly


## 0.99.1 *May 12th 2018*
* Allow `~NAME[N]` tag inside `~GENSYM[X]`
* Support HDL record selector generation [#313](https://github.com/clash-lang/clash-compiler/pull/313)
* `InlinePrimitive` support: specify HDL primitives inline with Haskell code
* Support for `ghc-typelits-natnormalise-0.6.1`
* `Lift` instances for `TopEntity` and `PortName`
* `InlinePrimitive` support: specify HDL primitives inline with Haskell code

## 0.99 *March 31st 2018*
* New features:
  * Major API overhaul: check the migration guide at the end of `Clash.Tutorial`
  * New features:
    * Explicit clock and reset arguments
    * Rename `CLaSH` to `Clash`
    * Implicit/`Hidden` clock and reset arguments using a combination of
    `reflection` and `ImplicitParams`.
    * Large overhaul of `TopEntity`  annotations
    * PLL and other clock sources can now be instantiated using regular functions:
    `Clash.Intel.ClockGen` and `Clash.Xilinx.ClockGen`.
    * DDR registers:
      * Generic/ASIC: `Clash.Explicit.DDR`
      * Intel: `Clash.Intel.DDR`
      * Xilinx: `Clash.Intel.Xilinx`
  * `Bit` is now a `newtype` instead of a `type` synonym and will be mapped to
    a HDL scalar instead of an array of one (e.g `std_logic` instead of
    `std_logic_vector(0 downto 0)`)
  * Hierarchies with multiple synthesisable boundaries by allowing more than one
    function in scope to have a `Synthesize` annotation.
    * Local caching of functions with a `Synthesize` annotation
  * `Bit` type is mapped to a HDL scalar type (e.g. `std_logic` in VHDL)
  * Improved name preservation
  * Zero-bit values are filtered out of the generated HDL
  * Improved compile-time computation
* Many bug fixes

## Older versions
Check out:
 * https://github.com/clash-lang/clash-compiler/blob/3649a2962415ea8ca2d6f7f5e673b4c14de26b4f/clash-prelude/CHANGELOG.md
 * https://github.com/clash-lang/clash-compiler/blob/3649a2962415ea8ca2d6f7f5e673b4c14de26b4f/clash-lib/CHANGELOG.md
 * https://github.com/clash-lang/clash-compiler/blob/3649a2962415ea8ca2d6f7f5e673b4c14de26b4f/clash-ghc/CHANGELOG.md
