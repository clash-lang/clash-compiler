# Changelog for the Clash project

## 1.8.3 *Oct 6th 2025*

Added:
* `Counter` instances for `Bool`, `Bit`, `Int`, `Int8`, `Int16`, `Int32`, `Int64`, `Word`, `Word8`, `Word16`, `Word32`, `Word64`, `Identity` and `Maybe`. [#2692](https://github.com/clash-lang/clash-compiler/pull/2692)
* The Vec type now has a [COMPLETE pragma](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pragmas.html#complete-pragma) to avoid incomplete pattern matches when using the `(:>)` pattern. [#3020](https://github.com/clash-lang/clash-compiler/pull/3020)
* RamOp now has an AutoReg instance. [#2792](https://github.com/clash-lang/clash-compiler/pull/2792)
* Added instance `NFDataX (SimOnly a)` [#2900](https://github.com/clash-lang/clash-compiler/pull/2900)
* Support for GHC 9.10 on Windows (macOS and Linux were already supported) [#2945](https://github.com/clash-lang/clash-compiler/pull/2945)
* Added a `BitPack` instance for `Char` [#2957](https://github.com/clash-lang/clash-compiler/pull/2957)
* Support for GHC 9.10.2 [#3003](https://github.com/clash-lang/clash-compiler/pull/3003)

Changed:
* Functions defined on `Clash.Class.Counter` are now public [#2692](https://github.com/clash-lang/clash-compiler/pull/2692)

Fixed:
* Clash hanging when rendering `Index n` literals, for large values of `n` [#2813](https://github.com/clash-lang/clash-compiler/issues/2813)
* Render overflowed Index literals as don't-cares in HDL [#2970](https://github.com/clash-lang/clash-compiler/pull/2970)
* Clash errors out when `Clash.Sized.Vector.splitAt` is compile-time evaluated in an illegal context [#2831]https://github.com/clash-lang/clash-compiler/issues/2831
* `Clash.Explicit.DDR`: [#2911](https://github.com/clash-lang/clash-compiler/pull/2911)
  - `ddrIn`: VHDL: Remove data input from sensitivity list of `ddrIn_neg_latch` register as it is superfluous. This should not affect functionality.
  - `ddrOut`: VHDL: Fix incorrect usage of `Enable` input when the domain is set to asynchronous resets. Deasserting the `Enable` exhibited wrong behavior before this fix.
* `Clash.Xilinx.DDR`: [#2911](https://github.com/clash-lang/clash-compiler/pull/2911)
  - These primitives only support clocks where the rising edge is the active edge. Using them in a domain with falling active edges now causes an error.
  - `oddr`: Fix VHDL and SystemVerilog erroring out during HDL generation
  - Symbols in HDL for both `iddr` and `oddr` were renamed to match their function.
* `Clash.Intel.DDR`: [#2911](https://github.com/clash-lang/clash-compiler/pull/2911)
  - These primitives only support clocks where the rising edge is the active edge. Using them in a domain with falling active edges now causes an error.
  - Fix rendering HDL. It variously errored out or generated non-working HDL.
  - Rendering HDL no longer causes Clash to issue a warning about an argument unused in Haskell but used in the primitive black box.
* `makeTopEntity` now accounts for `SimOnly` constructs. This can prevent warnings in situtations where the `SimOnly` type would contain types `makeTopEntity` cannot handle. [#2897](https://github.com/clash-lang/clash-compiler/pull/2897)
* Clash did not build on GHC 9.6.7 (but did on 9.6.6) [#2916](https://github.com/clash-lang/clash-compiler/issues/2916)
* Ignore `Tick`s in `TermLiteral Integer`, `TermLiteral Char`, `TermLiteral Natural`, and `TermLiteral (SNat n)` [#2925](https://github.com/clash-lang/clash-compiler/pull/2925)
* Fixed laziness issue in internal black box `imap_go` [#2542](https://github.com/clash-lang/clash-compiler/issues/2542)
* Clash's evaluator now uses `TemplateHaskell` names to detect renamed symbols in `GHC.*` and `Clash.*`. Fixes errors similar to `No blackbox found for: GHC.Internal.Base.eqString` [#2972](https://github.com/clash-lang/clash-compiler/issues/2972)
* No blackbox found for: `GHC.Internal.Control.Exception.Base.recSelError` on GHC 9.10 [#2966](https://github.com/clash-lang/clash-compiler/issues/2966)
* Verilog and System Verilog code gen bug for `map head` [#2809](https://github.com/clash-lang/clash-compiler/issues/2809)
* Error parsing blackbox: `Clash.Sized.Vector.head` [#2988](https://github.com/clash-lang/clash-compiler/issues/2988)
* Clash no longer duplicates included datafiles when component is instantiated multiple times [#3008](https://github.com/clash-lang/clash-compiler/issues/3008)
* Clash will no longer emit "no blackbox found for" `GHC.Real`'s exponentiation function if it is applied to constants [#3010](https://github.com/clash-lang/clash-compiler/pull/3010)
* Clash will no longer error out when converting `ensureSpine` on Clash number types to HDL [#3021](https://github.com/clash-lang/clash-compiler/issues/3021)
* Clash will no longer ignore Synthesize annotations when the function is used in an argument position [#3024](https://github.com/clash-lang/clash-compiler/issues/3024)

## 1.8.2 *Jan 3rd 2025*

Added:
* Support for GHC 9.10 [#2758](https://github.com/clash-lang/clash-compiler/pull/2758)
* Support for GHC 9.8.4 [#2852](https://github.com/clash-lang/clash-compiler/issues/2852)
* Add `ShowX`, `NFDataX` instances for `Proxy` [#2637](https://github.com/clash-lang/clash-compiler/pull/2637)
* Added `Clash.Sized.Vector.ToTuple.vecToTuple`: a way to safely work around incomplete patterns warnings on patterns involving `Vec`tors. [#2862](https://github.com/clash-lang/clash-compiler/pull/2682)
* Added operator precedences for infix usage of functions exported from `Clash.Class.Num`: `mul`, `add`, `sub`, `satMul`, `satAdd`, `satSub`, `boundedMul`, `boundedAdd`, and `boundedSub`. This means that expressions such as `` a `add` b `mul` c `` now get parsed as `` a `add` (b `mul` c) `` instead of `` (a `add` b) `mul` c ``. [#2719](https://github.com/clash-lang/clash-compiler/pull/2719)

Changed:
* `BitVector n` now has an implementation for `ensureSpine` which ensures the constructor is present. [#2702](https://github.com/clash-lang/clash-compiler/pull/2702)
* `xToBV` is now located in `Clash.Sized.Internal.BitVector` to avoid circular dependencies. [#2702](https://github.com/clash-lang/clash-compiler/pull/2702)
* The error messages that mention the valid ranges for out-of-range inputs have been improved to be more intuitive: one of `<empty range>`, `[n]` or `[n..m]`. All _n..m_ ranges are now ordered with the lower bound on the left. [#2733](https://github.com/clash-lang/clash-compiler/pull/2733)

Fixed:
* cabal: Make `workaround-ghc-mmap-crash` a noop on non-x86_64. Fixes [#2656](https://github.com/clash-lang/clash-compiler/issues/2656)
* Clash no longer hides error messages if it fails to load external (precompiled) modules. Note: this fix only works from GHC 9.0 on. See [#2365](https://github.com/clash-lang/clash-compiler/issues/2365)
* HDL generation fails when using multiple-hidden feature in combination with synthesis attributes [#2593](https://github.com/clash-lang/clash-compiler/issues/2593)
* Clash no longer errors out in the netlist generation stage when a polymorphic function is applied to type X in one alternative of a case-statement and applied to a newtype wrapper of type X in a different alternative. See [#2828](https://github.com/clash-lang/clash-compiler/issues/2628)
* various issues with black boxes and evaluator rules for number-related primitives [#2689](https://github.com/clash-lang/clash-compiler/pull/2689)
* `genBitVector` no longer contains off-by-one error on for generated Naturals [#2704](https://github.com/clash-lang/clash-compiler/pull/2704)
* (+>>.) and (.<<+) such that they are compliant with (+>>) and (<<+) for vectors of zero length in the sense that the input vector is kept unchanged. [#2730](https://github.com/clash-lang/clash-compiler/issues/2730)
* Removed `stringsearch` dependency from `v16-upgrade-primitives`. See [#2726](https://github.com/clash-lang/clash-compiler/issues/2726)
* Bug in the compile-time evaluator [#2781](https://github.com/clash-lang/clash-compiler/issues/2781)
* Exponentiation (`Clash.Class.Exp`) is now right-associative with a precedence level of 8 (`infixr 8`). By accident, it used to lack a fixity declaration, meaning it was implicitly left-associative at level 9. [#2818](https://github.com/clash-lang/clash-compiler/pull/2818)
* Unused argument warnings on writeToBiSignal# [#2822](https://github.com/clash-lang/clash-compiler/pull/2822)
* Clash errored saying it cannot translate a globally recursive function in code that originally only contains let-bound (local) recursion [#2839](https://github.com/clash-lang/clash-compiler/issues/2839)
* Clash generates illegal Verilog names [#2845](https://github.com/clash-lang/clash-compiler/issues/2845)

## 1.8.1 *Nov 10th 2023*

* Bump package dependencies to allow inclusion in stackage-nightly
* Bump package dependencies to allow building on GHC 9.8.1

## 1.8.0 *Nov 10th 2023*

Release highlights:

* Support for GHC 9.2, 9.4, 9.6 and 9.8. While GHC 9.2 is supported, we recommend users to skip this version as there is a severe degradation of error message quality. With this change, Clash now supports GHC versions 8.6 through 9.8.
* Major overhaul of the clocking functionality in `Clash.Xilinx.ClockGen` and `Clash.Intel.ClockGen`, see their respective entries below
* `mealyS` function (and several variations) to make writing state machines using the strict `State` monad easier
* Overhaul of `resetGlitchFilter`, see its respective entries below.

Added:

* `altpllSync` and `alteraPllSync` in `Clash.Intel.ClockGen`. These replace the deprecated functions without the `Sync` suffix. Unlike the old functions, these functions are safe to use and have a reset signal for each output domain that can be used to keep the domain in reset while the clock output stabilizes. All PLL functions now also support multiple clock outputs like the old `alteraPll` did. [#2592](https://github.com/clash-lang/clash-compiler/pull/2592)
*  A new clock type `DiffClock` is introduced to signify a differential clock signal that is passed to the design on two ports in antiphase. This is used by the differential Xilinx clock wizards in `Clash.Xilinx.ClockGen`. [#2592](https://github.com/clash-lang/clash-compiler/pull/2592)
* `Clash.Explicit.Testbench.clockToDiffClock`, to create a differential clock signal in a test bench. It is not suitable for synthesizing a differential output in hardware. [#2592](https://github.com/clash-lang/clash-compiler/pull/2592)
* `resetGlitchFilterWithReset`, which accomplishes the same task as `resetGlitchFilter` in domains with unknown initial values by adding a power-on reset input to reset the glitch filter itself. [#2544](https://github.com/clash-lang/clash-compiler/pull/2544)
* Convenience functions: `noReset`, `andReset`, `orReset` plus their unsafe counterparts [#2539](https://github.com/clash-lang/clash-compiler/pull/2539)
* Convenience constraint aliases: `HasSynchronousReset`, `HasAsynchronousReset`, and `HasDefinedInitialValues` [#2539](https://github.com/clash-lang/clash-compiler/pull/2539)
* `Clash.Prelude.Mealy.mealyS` and `Clash.Explicit.Mealy.mealyS` and their bundled equivalents `mealySB` which make writing state machines using the strict `State` monad easier. The tutorial has also been simplified by using this change. [#2484](https://github.com/clash-lang/clash-compiler/pull/2484)
* An experimental feature allowing clocks to vary their periods over time, called "dynamic clocks". Given that this is an experimental feature, it is not part of the public API. [#2295](https://github.com/clash-lang/clash-compiler/pull/2295)
* The prelude now exports `+>>.` and `.<<+`, which can be used to shift in a bit into a `BitVector` from the left or right respectively - similar to `+>>` and `<<+` for `Vec`s. [#2307](https://github.com/clash-lang/clash-compiler/pull/2307)
* `Clash.DataFiles.tclConnector` and the executable `static-files` in `clash-lib`. They provide the Tcl Connector, a Tcl script that allows Vivado to interact with the metadata generated by Clash (Quartus support will be added later). See `Clash.DataFiles.tclConnector` for further information. More documentation about the Tcl Connector and the Clash<->Tcl API will be made available later. [#2335](https://github.com/clash-lang/clash-compiler/pull/2335)
* Add `BitPack`, `NFDataX` and `ShowX` instances for `Ordering` [#2366](https://github.com/clash-lang/clash-compiler/pull/2366)
* Verilog users can now influence the "precision" part of the generated `timescale` pragma using `-fclash-timescale-precision`. [#2353](https://github.com/clash-lang/clash-compiler/pull/2353)
* Clash now includes blackboxes for `integerToFloat#`, `integerToDouble#` [#2342](https://github.com/clash-lang/clash-compiler/issues/2342)
* Instances `Arbitrary (Erroring a)`, `Arbitrary (Saturating a)`, `Arbitrary (Saturating a)`, and `Arbitrary (Zeroing a)` [#2356](https://github.com/clash-lang/clash-compiler/pull/2356)
* `Clash.Magic.clashSimulation`, a way to differentiate between Clash simulation and generating HDL. [#2473](https://github.com/clash-lang/clash-compiler/pull/2473)
* `Clash.Magic.clashCompileError`: make HDL generation error out with a custom error message. Simulation in Clash will also error when the function is evaluated, including a call stack. HDL generation unfortunately does not include a call stack. [#2399](https://github.com/clash-lang/clash-compiler/pull/2399)
* Added `Clash.XException.MaybeX`, a data structure with smart constructors that can help programmers deal with `XException` values in their blackbox model implementations [#2442](https://github.com/clash-lang/clash-compiler/pull/2442)
*  `Clash.Magic.SimOnly`, A container for data you only want to have around during simulation and is ignored during synthesis. Useful for carrying around things such as: a map of simulation/vcd traces, co-simulation state or meta-data, etc. [#2464](https://github.com/clash-lang/clash-compiler/pull/2464)
* `KnownNat (DomainPeriod dom)` as an implied constraint to `KnownDomain dom`. This reduces the amount of code needed to write - for example - clock speed dependent code. [#2541](https://github.com/clash-lang/clash-compiler/pull/2541)
* `Clash.Annotations.SynthesisAttributes.annotate`: a term level way of annotating signals with synthesis attributes [#2547](https://github.com/clash-lang/clash-compiler/pull/2547)
* `Clash.Annotations.SynthesisAttributes.markDebug`: a way of marking a signals "debug", instructing synthesizers to leave the signal alone and offer debug features [#2547](https://github.com/clash-lang/clash-compiler/pull/2547)
* Add hex and octal BitVector parsing. [#1772](https://github.com/clash-lang/clash-compiler/pull/2505)
* `1 <= n => Foldable1 (Vec n)` instance (`base-4.18+` only) [#2563](https://github.com/clash-lang/clash-compiler/pull/2563)
* You can now use `~PERIOD`, `~ISSYNC`, `~ISINITDEFINED` and `~ACTIVEEDGE` on arguments of type `Clock`, `Reset`, `Enable`,`ClockN` and `DiffClock`. [#2590](https://github.com/clash-lang/clash-compiler/pull/2590)

Removed:

* Deprecated module `Clash.Prelude.BitIndex`: functions have been moved to `Clash.Class.BitPack` [#2555](https://github.com/clash-lang/clash-compiler/pull/2555)
* Deprecated module `Clash.Prelude.BitReduction`: functions have been moved to `Clash.Class.BitPack` [#2555](https://github.com/clash-lang/clash-compiler/pull/2555)
* Deprecated function `Clash.Explicit.Signal.enable`: function has been renamed to `andEnable` [#2555](https://github.com/clash-lang/clash-compiler/pull/2555)
* The module `Clash.Clocks.Deriving` has been removed. [#2592](https://github.com/clash-lang/clash-compiler/pull/2592)

Deprecated:

* `unsafeFromLowPolarity`, `unsafeFromHighPolarity`, `unsafeToLowPolarity`, `unsafeToHighPolarity` have been replaced by `unsafeFromActiveLow`, `unsafeFromActiveHigh`, `unsafeToActiveLow`, `unsafeToActiveHigh`. While former ones will continue to exist, a deprecation warning has been added pointing to the latter ones. [#2540](https://github.com/clash-lang/clash-compiler/pull/2540)
* The functions `altpll` and `alteraPll` in `Clash.Intel.ClockGen` have been deprecated because they are unsafe to use while this is not apparent from the name. The `locked` output signal of these functions is an asynchronous signal which needs to be synchronized before it can be used (something the examples did in fact demonstrate). For the common use case, new functions are available, named `altpllSync` and `alteraPllSync`. These functions are safe. For advanced use cases, the old functionality can be obtained through `unsafeAltpll` and `unsafeAlteraPll`. [#2592](https://github.com/clash-lang/clash-compiler/pull/2592)

Changed:

* The wizards in `Clash.Xilinx.ClockGen` have been completely overhauled. The original functions were unsafe and broken in several ways. See the documentation in `Clash.Xilinx.ClockGen` for how to use the new functions. Significant changes are:
  * `clockWizard` and `clockWizardDifferential` now output a `Clock` and a `Reset` which can be directly used by logic. Previously, it outputted a clock and an asynchronous `locked` signal which first needed to be synchronized before it could be used (hence the old function being unsafe). Additionally, the original `locked` signal was strange: it mistakenly was an `Enable` instead of a `Signal dom Bool` and there was a polarity mismatch between Clash simulation and HDL. The `locked` signal was also not resampled to the output domain in Clash simulation.
  * There are new functions `unsafeClockWizard` and `unsafeClockWizardDifferential` for advanced use cases which directly expose the `locked` output of the wizard.
  * All clock generators now have the option to output multiple clocks from a single instance.
  * `clockWizardDifferential` now gets its input clock as a `DiffClock` type; use `clockToDiffClock` to generate this in your test bench if needed. Previously, the function received two clock inputs, but this generated `create_clock` statements in the top-level SDC file for both phases which is incorrect.
  * A constraint was removed: The _output_ clock domain no longer requires asynchronous resets. This was originally intended to signal that the outgoing lock signal is an asynchronous signal. The constraint does not convey this  information at all and is wrong; it also prevents using synchronous resets in the circuit as recommended by Xilinx. Note that if you use the `unsafe` functions, it is still necessary to synchronize the `locked` output in your design.
  * The port names of the primitives in HDL are now correctly lower case.
  * Add Tcl generation. This moves the responsibility of MMCM component generation from the user to `clashConnector.tcl`, which can be found in [`clash-lib:Clash.DataFiles`](https://hackage.haskell.org/package/clash-lib-1.8.0/docs/Clash-DataFiles.html).
  * The wizards now use the user-provided name as the name of the _instance_ rather than the name of the _IP core_. This change was also done for `Clash.Intel.ClockGen` in Clash v1.2.0 in March 2020, when Clash started generating Intel Qsys files. Before that, the user needed to generate a Qsys component manually. Now, in Clash v1.8.0, we also generate the Tcl for Xilinx wizards. When the user is responsible for creating the IP core, it makes sense to always set the component name to the user-provided value. But when that is also generated by Clash, that is no longer needed. Allowing users to set the instance name instead makes it possible to match on the instance in SDC files and such.
  [#2592](https://github.com/clash-lang/clash-compiler/pull/2592)
* The IP core generators in `Clash.Intel.ClockGen` now declare that their input domain needs to have asynchronous resets (`HasAsynchronousReset`), as the functions react asynchronously to their reset input and thus need to be glitch-free. The functions marked `unsafe` do not have this constraint; instead, the function documentation calls attention to the requirement. [#2592](https://github.com/clash-lang/clash-compiler/pull/2592)
* `resetGlitchFilter` now uses a counter instead of shift register, allowing glitch filtering over much larger periods. [#2374](https://github.com/clash-lang/clash-compiler/pull/2374)
* `resetGlitchFilter` now filters glitches symmetrically, only deasserting the reset after the incoming reset has stabilized. For more information, read [#2374](https://github.com/clash-lang/clash-compiler/pull/2374).
* `resetGlitchFilter` does not support domains with unknown initial values anymore. Its previous behavior could lead to unstable circuits. Domains not supporting initial values should consider using `resetGlitchFilterWithReset` or `holdReset`. The previous behavior can still be attained through the new `unsafeResetGlitchFilter`. [#2544](https://github.com/clash-lang/clash-compiler/pull/2544)
* `fromJustX` now uses `deepErrorX` instead of `errorX`. This adds `NFDataX` constraints to `blockRam` like functions, `asyncRam` and `writeToBiSignal`. [#2113](https://github.com/clash-lang/clash-compiler/pull/2113)
*  All memory functions now use `deepErrorX` for `XException`s. This adds `NFDataX` constraints to `asyncRom`, `asyncRomPow2` and `asyncRom#`. [#2113](https://github.com/clash-lang/clash-compiler/pull/2113)
* Before this release, `scanl1` was re-exported from the Haskell Prelude. Clash's Prelude now exports a `Vec` specialized version. [#2172](https://github.com/clash-lang/clash-compiler/pull/2172)
* When generating (System)Verilog, Clash now sets the default net type to `none`. This means any implicitly declared signal in the design will trigger an error when elaborating the design. [#2174](https://github.com/clash-lang/clash-compiler/pull/2174)
* Blackbox templates no longer have the `outputReg` key, it has been replaced with the more general `outputUsage` which specifies how signals are used in terms of whether writes are

    * continuous (i.e. a concurrent context)
    * procedural non-blocking (i.e. `signal` in a VHDL process)
    * procedural blocking (i.e. `variable` in a VHDL process)

  The `~OUTPUTWIREREG` tag continues to work for backwards compatibility, but there is also a new `~OUTPUTUSAGE` tag which is recommended. In the future, the `~OUTPUTWIREREG` tag may be removed. [#2230](https://github.com/clash-lang/clash-compiler/pull/2230)
* `Clash.Explicit.Testbench.outputVerifier` now takes an additional clock as an argument: the clock used by the circuit under test. If your tests use the same domain for the test circuit and design under test, consider using `Clash.Explicit.Testbench.outputVerifier'`. [#2295](https://github.com/clash-lang/clash-compiler/pull/2295)
* `Clash.Explicit.Signal.veryUnsafeSynchronizer` now accepts either a static clock period or a dynamic one. If you don't use dynamic clocks, convert your calls to use `Left`. [#2295](https://github.com/clash-lang/clash-compiler/pull/2295)
* `SDomainConfiguration` is now a record, easing field access. [#2349](https://github.com/clash-lang/clash-compiler/pull/2349)
*  Generalized the return types of `periodToHz` and `hzToPeriod`. Use a type application (`periodToHz @(Ratio Natural)`, `hzToPeriod @Natural`) to get the old behavior back, in case type errors arise. [#2436](https://github.com/clash-lang/clash-compiler/pull/2436)
* `periodToHz` and `hzToPeriod` now throw an `ErrorCall` with call stack when called with the argument 0 (zero), instead of a `RatioZeroDenominator :: ArithException`. [#2436](https://github.com/clash-lang/clash-compiler/pull/2436)
* `hasX` now needs an `NFDataX` constraint, in addition to an `NFData` one. This API change was made to fix an issue where `hasX` would hide error calls in certain situations, see [#2450](https://github.com/clash-lang/clash-compiler/issues/2450).
* Clock generators now wait at least 100 ns before producing their first tick. This change has been implemented to account for Xilinx's GSR in clock synchronization primitives. This change does not affect Clash simulation. See [#2455](https://github.com/clash-lang/clash-compiler/issues/2455).
* From GHC 9.4.1 onwards the following types: `BiSignalOut`, `Index`, `Signed`, `Unsigned`, `File`, `Ref`, and `SimIO` are all encoded as `newtype` instead of `data` now that [#2511](https://github.com/clash-lang/clash-compiler/pull/2511) is merged. This means you can once again use `Data.Coerce.coerce` to coerce between these types and their underlying representation. [#2535](https://github.com/clash-lang/clash-compiler/pull/2535)
* The `Foldable (Vec n)` instance and `Traversable (Vec n)` instance no longer have the `1 <= n` constraint. `Foldable.{foldr1,foldl1,maximum,minimum}` functions now throw an error at run-/simulation-time, and also at HDL-generation time, for vectors of length zero. [#2563](https://github.com/clash-lang/clash-compiler/pull/2563)
* The `maximum` and `minimum` functions exported by `Clash.Prelude` work on non-empty vectors, instead of the more generic version from `Data.Foldable`. [#2563](https://github.com/clash-lang/clash-compiler/pull/2563)
* `unsafeToReset` and `invertReset` now have a KnownDomain constraint This was done in preparation for [Remove KnownDomain #2589](https://github.com/clash-lang/clash-compiler/pull/2589)

Fixed:

* `altpll` and `alteraPll` in `Clash.Intel.ClockGen` now account for the input domain's `ResetPolarity`. Before this fix, the reset was always interpreted as an active-high signal. [#2592](https://github.com/clash-lang/clash-compiler/pull/2592)
* Fix `alteraPll` `qsys` generation. PR [#2417](https://github.com/clash-lang/clash-compiler/pull/2417) (included in Clash v1.6.5) caused a bug in the generation of the `qsys` file: it generated a spurious extra output clock which was completely unused otherwise. [#2587](https://github.com/clash-lang/clash-compiler/pull/2587)
* Files in `clash-manifest.json` are now (correctly) listed in reverse topological order [#2334](https://github.com/clash-lang/clash-compiler/issues/2334)
* Dependencies in `clash-manifest.json` are now listed in reverse topological ordering [#2325](https://github.com/clash-lang/clash-compiler/issues/2325)
* Clash now renders undefined bits set via `-fclash-force-undefined` correctly [#2360](https://github.com/clash-lang/clash-compiler/issues/2360)
* `resetGen`'s documentation now mentions it is non-synthesizable ([#2375](https://github.com/clash-lang/clash-compiler/issues/2375))
* `trueDualPortBlockRam` now handles undefined values in its input correctly  [#2350](https://github.com/clash-lang/clash-compiler/issues/2350)
* `trueDualPortBlockRam` now correctly handles port enables when clock edges coincide [#2351](https://github.com/clash-lang/clash-compiler/issues/2351)
* `Clash.Primitives.DSL.deconstructProduct` now projects fields out of a product [#2469](https://github.com/clash-lang/clash-compiler/issues/2469)
* BiSignal test does not look through `Annotate` [#2472](https://github.com/clash-lang/clash-compiler/issues/2472)
* Port size not rendered when type has more than one `Annotate` [#2475](https://github.com/clash-lang/clash-compiler/pull/2475)
* Clash now preserves `NOINLINE` of functions being specialized [#2502](https://github.com/clash-lang/clash-compiler/issues/2502)
* When `convertReset` was used with two domains that had a different reset polarity, the polarity of the signal was not changed.
* Functional arguments of primitives cannot have 0-bit results [#2549](https://github.com/clash-lang/clash-compiler/issues/2549)
* If the source reset of `convertReset` is synchronous, a flip-flop in the source domain is inserted to filter glitches from the source reset. [#2573](https://github.com/clash-lang/clash-compiler/pull/2573)
* SystemVerilog backend: Assignment patterns for unpacked arrays now have an index for every element; improves QuestaSim compatibility. [#2595](https://github.com/clash-lang/clash-compiler/pull/2595)
* Name duplication in generated Verilog involving reset synchronizer [#2598](https://github.com/clash-lang/clash-compiler/issues/2598)

Internal added:

* `Clash.Primitives.DSL.instDecl` now accepts `TExpr`s instead of `LitHDL`s as generics/parameters. This allows for VHDL black boxes to use all possible generic types. To ease transition, `litTExpr` has been added to `Clash.Primitives.DSL`. [#2471](https://github.com/clash-lang/clash-compiler/issues/2471)
* `Clash.Core.TermLiteral.deriveTermToData` now works on records [#2270](https://github.com/clash-lang/clash-compiler/pull/2270)
* `Clash.Primitives.getVec` tries to get all elements in a Vector from an expression [#2483](https://github.com/clash-lang/clash-compiler/pull/2483)
* Added `Clash.Primitives.DSL.deconstructMaybe`. This DSL function makes it easy to deconstruct a `Maybe` into its constructor bit and data. This is often useful for primitives taking 'enable' and 'data' signals. [#2202](https://github.com/clash-lang/clash-compiler/pull/2202)
* Added `unsafeToActiveHigh` and `unsafeToActiveLow` to `Clash.Primitives.DSL`. [#2270](https://github.com/clash-lang/clash-compiler/pull/2270)
* Added `TermLiteral` instance for `Either` [#2329](https://github.com/clash-lang/clash-compiler/pull/2329)
* `Clash.Primitives.DSL.declareN`, a companion to `declare` which declares multiple signals in one go. [#2592](https://github.com/clash-lang/clash-compiler/pull/2592)

Internal changes:

* `Clash.Primitives.DSL.boolFromBit` is now polymorphic in its HDL backend. [#2202](https://github.com/clash-lang/clash-compiler/pull/2202)
* `Clash.Primitives.DSL.unsignedFromBitVector` is now polymorphic in its HDL backend. [#2202](https://github.com/clash-lang/clash-compiler/pull/2202)
* `Clash.Primitives.DSL.fromBV` now converts some `BitVector` expression into some type. [#2202](https://github.com/clash-lang/clash-compiler/pull/2202)
* Add `CompDecl` to `Clash.Netlist.Types.Declaration` to accomodate VHDL's `component` declarations.
* Black box functions declare their usage, necessary for implicit netlist usage analysis implemented in [#2230](https://github.com/clash-lang/clash-compiler/pull/2230)
* Added `showsTypePrec` to `TermLiteral` to make `TermLiteral SNat` work as expected. Deriving an instance is now a bit simpler. Instances which previously had to be defined as:

  ```haskell
  instance TermLiteral Bool where
    termToData = $(deriveTermToData ''Bool)
  ```

  can now be defined using:

  ```haskell
  deriveTermLiteral ''Bool
  ```
  [#2329](https://github.com/clash-lang/clash-compiler/pull/2329)

## 1.6.6 *Oct 2nd 2023*

* Support Aeson 2.2
* Dropped the snap package

    The Clash snap package has not been a recommended way to use Clash for quite some time, and it is a hassle to support.

    In order to build a snap package, we build .deb packages for Clash with Ubuntu 20.04 LTS. But the interaction between the Debian build system and GHC is problematic, requiring significant effort to support and to upgrade to a more recent Ubuntu release.

    Additionally, snap packages have their own issues on distributions other than Ubuntu. Given that we no longer recommend people use our snap package and given the effort required to keep supporting them, we have decided to drop the snap package.

## 1.6.5 *Jun 27th 2023*

Fixed:

 * Support building with all combinations of specific versions of our dependencies `hashable` and `primitive`. [#2485](https://github.com/clash-lang/clash-compiler/pull/2485)
 * The Haskell simulation of the PLL lock signal in `Clash.Clocks` (used by `Clash.Intel.ClockGen`) is fixed: the signal is now unasserted for the time the reset input is asserted and vice versa, and no longer crashes the simulation. HDL generation is unchanged. The PLL functions now have an additional constraint: `KnownDomain pllLock`. [#2420](https://github.com/clash-lang/clash-compiler/pull/2420)

Changed:

 * Export the constructor for the `Wrapping` type in the `Clash.Num.Wrapping` module. See [#2292](https://github.com/clash-lang/clash-compiler/issues/2292)

## 1.6.4 *Aug 30th 2022*
Fixed:

 * Input validation of the used arguments in blackboxes is now complete. [#2184](https://github.com/clash-lang/clash-compiler/pull/2184)
 * `Clash.Annotations.BitRepresentation.Deriving.deriveAnnotation` no longer has quadratic complexity in the size of the constructors and fields. [#2209](https://github.com/clash-lang/clash-compiler/pull/2209)
 * Fully resolve type synonyms when deriving bit representations. [#2209](https://github.com/clash-lang/clash-compiler/pull/2209)
 * Disregard ticks when determining whether terms are shared. Fixes [#2233](https://github.com/clash-lang/clash-compiler/issues/2233).
 * The blackbox parser will make sure it fully parses its input, and report an error when it can't. [#2237](https://github.com/clash-lang/clash-compiler/issues/2237)
 * Wrap ~ARG[n] in parentheses. Fixes [#2213](https://github.com/clash-lang/clash-compiler/issues/2213)
 * The VHDL shift primitives no longer generate bound check failures. Fixes [#2215](https://github.com/clash-lang/clash-compiler/issues/2215)
 * Evaluator fails impredicative type instantiation of error values [#2272](https://github.com/clash-lang/clash-compiler/issues/2272)
 * Fix out of bound errors in toEnum/fromSLV for sum types [#2220](https://github.com/clash-lang/clash-compiler/issues/2220)
 * Netlist generation fails for certain uses of GADTs [#2289](https://github.com/clash-lang/clash-compiler/issues/2289)
 * The documentation for `ANN TestBench` had it backwards; it now correctly indicates the annotation is on the test bench, not the device under test. [#1750](https://github.com/clash-lang/clash-compiler/issues/1750)

Fixes with minor changes:

 * `reduceXor` now produces a result if the argument has undefined bits instead of throwing an `XException` (the result is an undefined bit). `reduceAnd` and `reduceOr` already always produced a result. [#2244](https://github.com/clash-lang/clash-compiler/pull/2244)

Added:

 * Support for symbols in types while deriving bit representations. [#2209](https://github.com/clash-lang/clash-compiler/pull/2209)
 * Support for promoted data types while deriving bit representations. [#2209](https://github.com/clash-lang/clash-compiler/pull/2209)
 * `scanlPar` and `scanrPar` in Clash's Prelude, as well as the `RTree` versions `tscanl` and `tscanr`. These variants of `scanl1` and `scanr1` compile to a binary tree of operations, with a depth of `O(log(n))` (`n` being the length of the vector) rather than a depth of `n` for `scanl1` and `scanr1`. [#2177](https://github.com/clash-lang/clash-compiler/pull/2177)
 * The GADT constructors for `RTree` (`RLeaf` and `RBranch`) are now exported directly in addition to the patterns `LR` and `BR`. [#2177](https://github.com/clash-lang/clash-compiler/pull/2177)
 * Added the `~ISSCALAR` template which can be used to check if an argument is rendered to a scalar in HDL. [#2184](https://github.com/clash-lang/clash-compiler/pull/2184)
 * Added support for records and infix constructors when using `Clash.Annotations.BitRepresentation.Deriving.deriveAnnotation`. [#2191](https://github.com/clash-lang/clash-compiler/pull/2191)
 * Clash now contains instances for `ShowX`, `NFDataX` and `BitPack` on the newtypes from the Data.Functor modules (`Identity`, `Const`, `Compose`, `Product` and `Sum`). [#2218](https://github.com/clash-lang/clash-compiler/issues/2218)

## 1.6.3 *Apr 7th 2022*
Fixed:
  * Handle `~ISUNDEFINED` hole in black boxes for `BitVector` and for product types. This means that with `-fclash-aggressive-x-optimization-blackboxes`, resets are now omitted for _undefined_ reset values of such types as well. [#2117](https://github.com/clash-lang/clash-compiler/issues/2117)
  * The `alteraPll` primitive was unusable since commit `d325557750` (release v1.4.0), it now works again. [#2136](https://github.com/clash-lang/clash-compiler/pull/2136)
  * Simulation/Synthesis mismatch for X-exception to undefined bitvector conversion [#2154](https://github.com/clash-lang/clash-compiler/issues/2154)
  * The VHDL blackbox for `Signed.fromInteger` can now handle any `Netlist Expr` as input [#2149](https://github.com/clash-lang/clash-compiler/issues/2149)
  * Clash no longer escapes extended identifiers when rendering SDC files. [#2142](https://github.com/clash-lang/clash-compiler/pull/2142)
  * The types defined in `clash-prelude-hedgehog` now come with `Show` instances [#2133](https://github.com/clash-lang/clash-compiler/issues/2133)
  * Extreme values are now generated from the input range instead of the type's bounds [#2138](https://github.com/clash-lang/clash-compiler/issues/2138)

Internal change:
  * Clash now always generates non-extended identifiers for port names, so that generated names play nicer with different vendor tools. [#2142](https://github.com/clash-lang/clash-compiler/pull/2142)
  * Top entity name available in netlist context. Top entity name used in generated name for include files. [#2146](https://github.com/clash-lang/clash-compiler/pull/2146)

## 1.6.2 *Feb 25th 2022*
Fixed:
  * Clash now compiles for users of Clang - i.e., all macOS users.
  * The `trueDualPortBlockRam` model did not accurately simulate concurrent active ports, thus causing a Haskell/HDL simulation mismatch for `asyncFIFOSynchronizer`.
  * `trueDualPortBlockRam` Haskell/HDL simulation mismatch for port enable.
  * Sometimes `trueDualPortBlockRam` swapped the names of the ports in exception messages. [#2102](https://github.com/clash-lang/clash-compiler/pull/2102)
  * The evaluator rule for unpack{Float,Double}# are now corrected to return boxed float and double instead of unboxed literals. [#2097](https://github.com/clash-lang/clash-compiler/issues/2097)

Changed:
  * The `trueDualPortBlockRam` model now only models read/write conflicts for concurrent active ports
  * The `trueDualPortBlockRam` model now models write/write conflicts for concurrent active ports

## 1.6.1 *Feb 11th 2022*
Changed:
  * We accidentally released `v1.6.0` with the Cabal flag `multiple-hidden` enabled. This is an experimental feature, supposed to be disabled by default for releases. `v1.6.1` disables it again.

Added:
  * `Clash.Class.HasDomain.TryDomain` instances for Clash sized types

## 1.6.0 *Feb 10th 2022*
Added:
  * `Clash.Class.Counter`: a class that defines a odometer-style supercounter. [#1763](https://github.com/clash-lang/clash-compiler/pull/1763)
  * `isLike` function for BitPack types. [#1774](https://github.com/clash-lang/clash-compiler/pull/1774)
  * 'seqErrorX' for catching both `XException` and `ErrorCall`. [#1774](https://github.com/clash-lang/clash-compiler/pull/1774)
  * `Clash.Explicit.BlockRam.File.memFile`, a function for creating the contents of the data files this blockRAM uses. Can also be imported from `Clash.Prelude.BlockRam.File`, `Clash.Prelude.ROM.File` and `Clash.Explicit.ROM.File`. [#1840](https://github.com/clash-lang/clash-compiler/pull/1840)
  * Support for Yosys compatible SVA to `Clash.Verification`. This enables formal verification using SymbiYosis for Verilog and SystemVerilog. [#1798](https://github.com/clash-lang/clash-compiler/pull/1798)
  * `Clash.Explicit.Signal.Delayed.forward`, a function that can be used to retime a `DSignal` into the future without applying any logic. [#1882](https://github.com/clash-lang/clash-compiler/pull/1882)
  * `Clash.Signal.andEnable` is the `HiddenEnable` version of `Clash.Explicit.Signal.andEnable` (formerly known as `enable`) [#1849](https://github.com/clash-lang/clash-compiler/pull/1849)
  * `runUntil`, a function to sample a signal until it returns a value that satisfies the user-given test. It is a convenience function that, among others, allow easy running of a `testBench` style function in Haskell simulation, logging assertion failures to stderr. [#1940](https://github.com/clash-lang/clash-compiler/pull/1940)
  * Support for true dual ported block ram through `Clash.Prelude.BlockRam.trueDualPortBlockRam` and `Clash.Explicit.BlockRam.trueDualPortBlockRam`. [#1726](https://github.com/clash-lang/clash-compiler/pull/1726) [#1975](https://github.com/clash-lang/clash-compiler/pull/1975)
  * `clash-{prelude,lib}-hedgehog` packages which provide generators for types in `clash-prelude` and `clash-lib`. The former is published on Hackage. [#1976](https://github.com/clash-lang/clash-compiler/pull/1976)
  * Clash now contains black boxes which are verilator compatible. When running with `--verilog` or `--systemverilog` a C++ shim is automatically produced which can be used to quickly generate a verilated executable. Users who wish to interact with verilator simulations are recommended to use [clashilator](https://github.com/gergoerdi/clashilator). [#2019](https://github.com/clash-lang/clash-compiler/pull/2019)
  * Support for YAML blackboxes. Clash will now pickup on files with a `.primitives.yaml` extension. While we recommend upgrading your primitive files to the new format, old style primitives are still supported. We've included a tool to automatically upgrade your JSON files, see [#2037](https://github.com/clash-lang/clash-compiler/pull/2037)
  * `MemBlob`: a datastructure for efficient constants, typically used for initializing memories. [#2041](https://github.com/clash-lang/clash-compiler/pull/2041)

Fixed:

  * BlockRam simulation is now less strict. [#1458](https://github.com/clash-lang/clash-compiler/issues/1458)
  * Don't overflow VHDL's integer type when addressing RAM/ROM in simulation.Addresses are masked to 32 bits to be sure to keep it within the simulator's range. [#1875](https://github.com/clash-lang/clash-compiler/pull/1875)
  * `show` on `BitVector 0` no longer results in an empty string. [#1785](https://github.com/clash-lang/clash-compiler/pull/1785)
  * Clash now preserves transfinite floating numbers (NaN, Infinity) when packing/unpacking [#1803](https://github.com/clash-lang/clash-compiler/issues/1803)
  * `SynthesisAnnotation`s can now be defined in type synoynms without being excluded from the generated HDL [#1771](https://github.com/clash-lang/clash-compiler/issues/1771)
  * Manifest files now correctly list bidirectional ports as "inout" rather than "in" [#1843](https://github.com/clash-lang/clash-compiler/issues/1843)
  * `div`/`rem`/`mod` now avoid division by zero during VHDL simulation. Due to the use of concurrent statements, even unreachable code would previously result in simulation error [#1873](https://github.com/clash-lang/clash-compiler/pull/1873)
  * Don't overflow the range of VHDL's natural type in shift/rotate, leading to simulation issues. Shift now saturates to a 31-bit shift amount. For rotate, in simulation only, the rotate amount is modulo the word width of the rotated value [#1874](https://github.com/clash-lang/clash-compiler/pull/1874)
  * `shiftL` for Clash datatypes does not cause a crash anymore when running Clash code with a really large shift amount [#1874](https://github.com/clash-lang/clash-compiler/pull/1874)
  * VHDL generated for `Signed.fromInteger` now truncates, like the Clash simulation, when the result is smaller than the argument [#1874](https://github.com/clash-lang/clash-compiler/pull/1874)
  * Clash now preserves boolean combinatorial logic better when generating HDL [#1881](https://github.com/clash-lang/clash-compiler/issues/1881)
  * `valid` field of `TemplateFunction` is now checked for includes [#1945](https://github.com/clash-lang/clash-compiler/issues/1945)
  * Clash now generates clock generators that ensure that the amount of time between simulation start and the first active edge of the clock is equal to (/or longer than/) the period of the clock. The first active edges of the clocks do still occur simultaneously. [#2001](https://github.com/clash-lang/clash-compiler/issues/2001)
  * Expected values in assert become undefined when using `-fclash-compile-ultra` [#2040](https://github.com/clash-lang/clash-compiler/issues/2040)
  * `toEnum`/`fromEnum` on sized types is now less eager to report warnings about integer functions being used [#2046](https://github.com/clash-lang/clash-compiler/issues/2046)

Changed:

  * `Clash.Verification.PrettyPrinters` has been moved from clash-prelude to to `Clash.Verification.Pretty` in `clash-lib`. [#1798](https://github.com/clash-lang/clash-compiler/pull/1798)
  * RAM/ROM functions: They now throw `XExeception` for out-of-bounds address inputs, so this condition no longer aborts simulation. [#1875](https://github.com/clash-lang/clash-compiler/pull/1875)
  * `Vec`'s show instance now generates valid Haskell. [#1776](https://github.com/clash-lang/clash-compiler/issues/1776)
  * `ShowX` and its functions now produce valid Haskell [#1782](https://github.com/clash-lang/clash-compiler/issues/1782)
  * `bLit` now infers the size of the generated BitVector from the string given to it. This means you don't have to give it an explicit type signature anymore. This does slightly modify the syntax needed to invoke `bLit`. E.g., `$$(bLit "00..1") :: BitVector 5` should be rewritten as `$(bLit "00..1")`. If you relied on the size inference, wrap the new invocation in `resize`. For example: `resize $(bLit "00..1")`. [#1784](https://github.com/clash-lang/clash-compiler/pull/1784)
  * `NumericUnderscores` is now enabled by default in `clash`, `clashi`, and starter projects using Clash >=1.6. [#1785](https://github.com/clash-lang/clash-compiler/pull/1785)
  * `Show` instance of `BitVector` now includes a `0b` prefix, making it a copyable expression for fully defined vectors. [#1785](https://github.com/clash-lang/clash-compiler/pull/1785)
  * `blockRam` uses `STArray` as the underlying representation to improve simulation performance [#1878](https://github.com/clash-lang/clash-compiler/pull/1878)
  * `asyncRom` now throws `XException` for out-of-bounds addressing, no longer aborting simulation [#1878](https://github.com/clash-lang/clash-compiler/pull/1878)
  * Clash now renders ADTs with all zero-width fields as enumerations in VHDL [#1879](https://github.com/clash-lang/clash-compiler/pull/1879)
  * A warning about possible hard-to-debug issues has been added to the `Clash.Signal` documentation on hidden clocks, resets, and enables, in the form of the section named "Monomorphism restriction leads to surprising behavior" [#1960](https://github.com/clash-lang/clash-compiler/pull/1960)
  * `Clash.Explicit.Testbench.outputVerifier` and `outputVerifierBitVector` now emit a warning if they are used improperly. This situation only arises when they are used in synthesized code rather than a test bench context. When the clock domains `circuitDom` and `testDom` are two different domains, the clock crossing inside `outputVerifier` is only suitable inside a test bench, not inside a synthesized circuit. Clash now emits a warning for this case. [#1931](https://github.com/clash-lang/clash-compiler/pull/1931)
  * `resetSynchronizer` now no longer takes an `Enable` argument. The argument was already marked for removal and was ignored. [#1964](https://github.com/clash-lang/clash-compiler/pull/1964)
  * Clash can now compile multiple entities concurrently, providing speedups to designs with multiple entities to build [#2034](https://github.com/clash-lang/clash-compiler/pull/2034)
  * All `asyncRam` variants and `asyncFIFOSynchronizer` now require that the data has an `NFDataX` instance. [#2055](https://github.com/clash-lang/clash-compiler/pull/2055)
  * Clash now respects the `-Werror` option from GHC [#2066](https://github.com/clash-lang/clash-compiler/pull/2066)
  * `asyncFIFOSynchronizer` now uses the synchronous dual-ported RAM `trueDualPortBlockRam`, where it previously used a dual-ported RAM with an asynchronous read port `asyncRam`. With this change it's nearly guaranteed that `asyncFIFOSynchronizer` actually synthesizes to a circuit that uses the dual-ported RAMs found on most FPGAs. [#2083](https://github.com/clash-lang/clash-compiler/pull/2083)

Deprecated:
  * The function `Clash.Explicit.Signal.enable` is renamed to `andEnable` and the existing name deprecated [#1849](https://github.com/clash-lang/clash-compiler/pull/1849)
  * '-fclash-float-support': it is now on by default and can't be turned off. [#2048](https://github.com/clash-lang/clash-compiler/pull/2048)

Removed:

  * GHC 8.4 is no longer supported. Users should upgrade to at least GHC 8.6. [#1762](https://github.com/clash-lang/clash-compiler/pull/1762)

Internal changes:
  * `clash-lib` now uses `Data.Monoid.Ap` instead of `Data.Semigroup.Monad.Mon`. This means users defining primitives with `TemplateFunction` will need to replace `Mon`/`getMon` with `Ap`/`getAp`. [#1835](https://github.com/clash-lang/clash-compiler/pull/1835)
  * Clash now supports more expressive debug options at the command line [#1800](https://github.com/clash-lang/clash-compiler/issues/1800).
  * Added `zeroWidthSpec` transformation [#1891](https://github.com/clash-lang/clash-compiler/pull/1891)
  * Added `collapseRHSNoops` inlining stage and `WorkIdentity` constructor [#1896](https://github.com/clash-lang/clash-compiler/pull/1896)
  * Added `HasType` and `InferType` classes for getting / inferring core types from data representing some typed "thing" [#1915](https://github.com/clash-lang/clash-compiler/pull/1915)
  * Added `HasFreeVars` class for getting free variables from data "containing" variables [#1917](https://github.com/clash-lang/clash-compiler/pull/1917)
  * Added the primitive equality type (`~#`) to `Clash.Core.TysPrim`. In order to make this change, `undefinedTy` and `unsafeCoerceTy` were moved from `Clash.Core.Type` to `Clash.Core.Util`. [#1955](https://github.com/clash-lang/clash-compiler/pull/1955)
  * Clash now keeps information about which let bindings are recursive from GHC core. This can be used to avoid performing free variable calculations, or sorting bindings in normalization. [#1980](https://github.com/clash-lang/clash-compiler/pull/1980) [#2000](https://github.com/clash-lang/clash-compiler/pull/2000)
  *  Manifest files now use SHA256 for a cache invalidation digest [#1985](https://github.com/clash-lang/clash-compiler/pull/1985)

## 1.4.7 *Jan 30th 2022*
Fixed:
  * Clash now shows days in time strings for compile runs which take longer than a day [#1989](https://github.com/clash-lang/clash-compiler/compare/issue-1989).
  * Types defined in the package head are no longer qualified in the package body when rendering VHDL [#1996](https://github.com/clash-lang/clash-compiler/issues/1996).
  * `asyncRam` with different read and write clocks no longer produce the wrong results in Haskell simulation. [#2031](https://github.com/clash-lang/clash-compiler/pull/2031)
  * `Clash.Explicit.RAM.asyncRam#` Haskell simulation incorrectly treated an _undefined_ write enable as asserted. It now causes an _undefined_ value to be written instead. This problem did not propagate to the other `asyncRam` functions, where the same condition would simultaneously lead to an undefined write address, which would be handled correctly. This problem also only affects Haskell simulation, not the generated HDL. [#2031](https://github.com/clash-lang/clash-compiler/pull/2031)
  * `Clash.Explicit.BlockRam.blockRam#` and `Clash.Explicit.BlockRam.File.blockRamFile#` Haskell simulation incorrectly treated an _undefined_ write enable as asserted. It now causes an _undefined_ value to be written instead. This problem did not propagate to the other `blockRam` functions, where the same condition would simultaneously lead to an undefined write address, which would be handled correctly. This problem also only affects Haskell simulation, not the generated HDL.([#2054](https://github.com/clash-lang/clash-compiler/pull/2054))

Internal changes:
  * Removed instances of `Hashable Term` and `Hashable Type` [#1986](https://github.com/clash-lang/clash-compiler/pull/1986)
  * Added structural equality on `Term` (`Clash.Core.Subst.eqTerm`) and `Type` (`Clash.Core.Subst.eqType`)

Internal fixes:
  * Enable used to be a `Bool` in the Blackbox DSL, so we could use `boolToBit`. However it now has its own type in the DSL (`Enable domainName`), so we've added a new conversion function in order to convert it to a Bool.


## 1.4.6 *Oct 26th 2021*

Fixed:

  * Clash tries to cast-specialize non-"global binders" resulting in "specialisation of non-work-free cast" warning [#1933](https://github.com/clash-lang/clash-compiler/issues/1945)
  * More consistently render bare untyped and unsized literals for `~LIT` tags. This fixes [#1934](https://github.com/clash-lang/clash-compiler/issues/1934)

## 1.4.5 *Oct 13th 2021*

Changed:

 * `clash-lib` now supports prettyprinter 1.7

Documentation:

 * The documentation on hidden clocks, resets, and enables has been corrected and extended in `Clash.Signal`.

## 1.4.4 *Oct 11th 2021*
Changed:

 * `clash-lib` now supports aeson >= 2.0

Fixed:

 * Dont' loop on recursive data types hiding behind type families [#1921](https://github.com/clash-lang/clash-compiler/issues/1921)
 * Recognize `enableGen` as workfree and don't duplicate registers [#1935](https://github.com/clash-lang/clash-compiler/issues/1935)


## 1.4.3 *Aug 8th 2021*
Fixed:

 * Clash no longer generates calls to `{shift,rotate}_{left,right}` in VHDL where the count is a negative number [#1810](https://github.com/clash-lang/clash-compiler/issues/1810).
 * Clash no longer incurs unnecessary compile-time complexity while compiling Vector indexing operator [#1557](https://github.com/clash-lang/clash-compiler/issues/1557)

## 1.4.2 *May 18th 2021*
Fixed:

 * Erroneous examples in `Clash.Annotation.TopEntity` documentation [#646](https://github.com/clash-lang/clash-compiler/issues/646) and [#654](https://github.com/clash-lang/clash-compiler/issues/654)
 * `unconcat` cannot be used as initial/reset value for a `register` [#1756](https://github.com/clash-lang/clash-compiler/issues/1756)
 * `showX` now doesn't crash if a spine of a `Vec` is undefined
 * `~ISACTIVEENABLE` in blackboxes works again, and now acts on `Signal dom Bool` in addition to `Enable dom`. Since [#1368](https://github.com/clash-lang/clash-compiler/pull/1368), enable lines were always generated even if they were known to be always enabled. Fixes [#1786](https://github.com/clash-lang/clash-compiler/issues/1786).
 * clash --show-options now shows -fclash-* options in GHC 9.0 [#1787](https://github.com/clash-lang/clash-compiler/issues/1787)
 * `makeRecursiveGroups` now correctly identifies mutual recursion between global binders ([#1796](https://github.com/clash-lang/clash-compiler/issues/1796)).

## 1.4.1 *April 6th 2021*
Fixed:

 * Broken VHDL primitive template for setSlice# [#1715](https://github.com/clash-lang/clash-compiler/issues/1715)
 * Unable to reduce nested type families [#1721](https://github.com/clash-lang/clash-compiler/issues/1721)
 * DEC transformation fails for functions applied to more than 62 arguments [#1669](https://github.com/clash-lang/clash-compiler/issues/1669)
 * Erroneous examples in BlockRam.File and ROM.File documentation [#1608](https://github.com/clash-lang/clash-compiler/issues/1608)
 * Blackboxes of `Clash.Sized.Vector` functions error on vectors containing `Clocks`, `Reset`, or `Enable` [#1606](https://github.com/clash-lang/clash-compiler/issues/1606)
 * `Clash.Signal.Delayed.delayI` cannot be reset, the `HiddenReset` constraint was unintentional. Asserting its reset has never worked. Removed the constraint [#1739](https://github.com/clash-lang/clash-compiler/pull/1739).
 * Annotate attributes cannot use type families [#1742](https://github.com/clash-lang/clash-compiler/issues/1742)

Changed:

 * `Clash.Prelude.ROM.File.romFile` now takes an `Enum addr => addr` as address argument, making it actually useful. [#407](https://github.com/clash-lang/clash-compiler/issues/407)

## 1.4.0 *March 12th 2021*
Highlighted changes (repeated in other categories):

  * Clash no longer disables the monomorphism restriction. See [#1270](https://github.com/clash-lang/clash-compiler/issues/1270), and mentioned issues, as to why. This can cause, among other things, certain eta-reduced descriptions of sequential circuits to no longer type-check. See [#1349](https://github.com/clash-lang/clash-compiler/pull/1349) for code hints on what kind of changes to make to your own code in case it no longer type-checks due to this change.
  * Type arguments of `Clash.Sized.Vector.fold` swapped: before `forall a n . (a -> a -> a) -> Vec (n+1) a -> a`, after `forall n a . (a -> a -> a) -> Vec (n+1) a`. This makes it easier to use `fold` in a `1 <= n` context so you can "simply" do `fold @(n-1)`
  * `Fixed` now obeys the laws for `Enum` as set out in the Haskell Report, and it is now consistent with the documentation for the `Enum` class on Hackage. As `Fixed` is also `Bounded`, the rule in the Report that `succ maxBound` and `pred minBound` should result in a runtime error is interpreted as meaning that `succ` and `pred` result in a runtime error whenever the result cannot be represented, not merely for `minBound` and `maxBound` alone.
  * Primitives should now be stored in `*.primitives` files instead of `*.json`. While primitive files very much look like JSON files, they're not actually spec complaint as they use newlines in strings. This has recently been brought to our attention by Aeson fixing an oversight in their parser implementation. We've therefore decided to rename the extension to prevent confusion.

Fixed:
  * Result of `Clash.Class.Exp.(^)` has enough bits in order to deal with `x^0`.
  * Resizes to `Signed 0` (e.g., `resize @(Signed n) @(Signed 0)`) don't throw an error anymore
  * `satMul` now correctly handles arguments of type `Index 2`
  * `Clash.Explicit.Reset.resetSynchronizer` now synchronizes on synchronous domains too [#1567](https://github.com/clash-lang/clash-compiler/pull/1567).
  * `Clash.Explicit.Reset.convertReset`: now converts synchronous domains too, if necessary [#1567](https://github.com/clash-lang/clash-compiler/pull/1567).
  * `inlineWorkFree` now never inlines a topentity. It previously only respected this invariant in one of the two cases [#1587](https://github.com/clash-lang/clash-compiler/pull/1587).
  * Clash now reduces recursive type families [#1591](https://github.com/clash-lang/clash-compiler/issues/1591)
  * Primitive template warning is now retained when a `PrimitiveGuard` annotation is present [#1625](https://github.com/clash-lang/clash-compiler/issues/1625)
  * `signum` and `RealFrac` for `Fixed` now give the correct results.
  * Fixed a memory leak in register when used on asynchronous domains. Although the memory leak has always been there, it was only triggered on asserted resets. These periods are typically short, hence typically unnoticable.
  * `createDomain` will not override user definitions of types, helping users who strive for complete documentation coverage [#1674] https://github.com/clash-lang/clash-compiler/issues/1674
  * `fromSNat` is now properly constrained [#1692](https://github.com/clash-lang/clash-compiler/issues/1692)
  * As part of an internal overhaul on netlist identifier generation [#1265](https://github.com/clash-lang/clash-compiler/pull/1265):
    * Clash no longer produces "name conflicts" between basic and extended identifiers. I.e., `\x\` and `x` are now considered the same variable in VHDL (likewise for other HDLs). Although the VHDL spec considers them distinct variables, some HDL tools - like Quartus - don't.
    * Capitalization of Haskell names are now preserved in VHDL. Note that VHDL is a case insensitive languages, so there are measures in place to prevent Clash from generating both `Foo` and `fOO`. This used to be handled by promoting every capitalized identifier to an extended one and wasn't handled for basic ones.
    * Names generated for testbenches can no longer cause collisions with previously generated entities.
    * Names generated for components can no longer cause collisions with user specified top entity names.
    * For (System)Verilog, variables can no longer cause collisions with (to be) generated entity names.
    * HO blackboxes can no longer cause collisions with identifiers declared in their surrounding architecture block.


Changed:
  * Treat enable lines specially in generated HDL [#1171](https://github.com/clash-lang/clash-compiler/issues/1171)
  * `Signed`, `Unsigned`, `SFixed`, and `UFixed` now correctly implement the `Enum` law specifying that the predecessor of `minBound` and the successor of `maxBound` should result in an error [#1495](https://github.com/clash-lang/clash-compiler/pull/1495).
  * `Fixed` now obeys the laws for `Enum` as set out in the Haskell Report, and it is now consistent with the documentation for the `Enum` class on Hackage. As `Fixed` is also `Bounded`, the rule in the Report that `succ maxBound` and `pred minBound` should result in a runtime error is interpreted as meaning that `succ` and `pred` result in a runtime error whenever the result cannot be represented, not merely for `minBound` and `maxBound` alone.
  * Type arguments of `Clash.Sized.Vector.fold` swapped: before `forall a n . (a -> a -> a) -> Vec (n+1) a -> a`, after `forall n a . (a -> a -> a) -> Vec (n+1) a`. This makes it easier to use `fold` in a `1 <= n` context so you can "simply" do `fold @(n-1)`
  * Moved `Clash.Core.Evaluator` into `Clash.GHC` and provided generic interface in `Clash.Core.Evalautor.Types`. This removes all GHC specific code from the evaluator in clash-lib.
  * Clash no longer disables the monomorphism restriction. See [#1270](https://github.com/clash-lang/clash-compiler/issues/1270), and mentioned issues, as to why. This can cause, among other things, certain eta-reduced descriptions of sequential circuits to no longer type-check. See [#1349](https://github.com/clash-lang/clash-compiler/pull/1349) for code hints on what kind of changes to make to your own code in case it no longer type-checks due to this change.
  * Clash now generates SDC files for each topentity with clock inputs
  * `deepErrorX` is now equal to `undefined#`, which means that instead of the whole BitVector being undefined, its individual bits are. This makes sure bit operations are possible on it. [#1532](https://github.com/clash-lang/clash-compiler/pull/1532)
  * From GHC 9.0.1 onwards the following types: `BiSignalOut`, `Index`, `Signed`, `Unsigned`, `File`, `Ref`, and `SimIO` are all encoded as `data` instead of `newtype` to work around an [issue](https://github.com/clash-lang/clash-compiler/pull/1624#discussion_r558333461) where the Clash compiler can no longer recognize primitives over these types. This means you can no longer use `Data.Coerce.coerce` to coerce between these types and their underlying representation.
  * Signals on different domains used to be coercable because the domain had a type role "phantom". This has been changed to "nominal" to prevent accidental, unsafe coercions. [#1640](https://github.com/clash-lang/clash-compiler/pull/1640)
  * Size parameters on types in Clash.Sized.Internal.* are now nominal to prevent unsafe coercions. [#1640](https://github.com/clash-lang/clash-compiler/pull/1640)
  * `hzToPeriod` now takes a `Ratio Natural` rather than a `Double`. It rounds slightly differently, leading to more intuitive results and satisfying the requested change in [#1253](https://github.com/clash-lang/clash-compiler/issues/1253). Clash expresses clock rate as the clock period in picoseconds. If picosecond precision is required for your design, please use the exact method of specifying a clock period rather than a clock frequency.
  * `periodToHz` now results in a `Ratio Natural`
  * `createDomain` doesn't override existing definitions anymore, fixing [#1674](https://github.com/clash-lang/clash-compiler/issues/1674)
  * Manifest files are now stored as `clash-manifest.json`
  * Manifest files now store hashes of the files Clash generated. This allows Clash to detect user changes on a next run, preventing accidental data loss.
  * Primitives should now be stored in `*.primitives` files. While primitive files very much look like JSON files, they're not actually spec complaint as they use newlines in strings. This has recently been brought to our attention by Aeson fixing an oversight in their parser implementation. We've therefore decided to rename the extension to prevent confusion.
  * Each binder marked with a `Synthesize` or `TestBench` pragma will be put in its own directory under their fully qualified Haskell name. For example, two binders `foo` and `bar` in module `A` will be synthesized in `A.foo` and `A.bar`.
  * Clash will no longer generate vhdl, verilog, or systemverilog subdirectories when using `-fclash-hdldir`.
  * `Data.Kind.Type` is now exported from `Clash.Prelude` [#1700](https://github.com/clash-lang/clash-compiler/issues/1700)


Added:
  * Support for GHC 9.0.1
  * `Clash.Signal.sameDomain`: Allows user obtain evidence whether two domains are equal.
  * `xToErrorCtx`: makes it easier to track the origin of `XException` where `pack` would hide them [#1461](https://github.com/clash-lang/clash-compiler/pull/1461)
  * Additional field with synthesis attributes added to `InstDecl` in `Clash.Netlist.Types` [#1482](https://github.com/clash-lang/clash-compiler/pull/1482)
  * `Data.Ix.Ix` instances for `Signed`, `Unsigned`, and `Index` [#1481](https://github.com/clash-lang/clash-compiler/pull/1481) [#1631](https://github.com/clash-lang/clash-compiler/pull/1631)
  * Added `nameHint` to allow explicitly naming terms, e.g. `Signal`s.
  * Checked versions of `resize`, `truncateB`, and `fromIntegral`. Depending on the type `resize`, `truncateB`, and `fromIntegral` either yield an `XException` or silently perform wrap-around if its argument does not fit in the resulting type's bounds. The added functions check the bound condition and fail with an error call if the condition is violated. They do not affect HDL generation. [#1491](https://github.com/clash-lang/clash-compiler/pull/1491)
  * `HasBiSignalDefault`: constraint to Clash.Signal.BiSignal, `pullUpMode` gives access to the pull-up mode. [#1498](https://github.com/clash-lang/clash-compiler/pull/1498)
  * Match patterns to bitPattern [#1545](https://github.com/clash-lang/clash-compiler/pull/1545)
  * Non TH `fromList` and `unsafeFromList` for Vec. These functions allow Vectors to be created from a list without needing to use template haskell, which is not always desirable. The unsafe version of the function does not compare the length of the list to the desired length of the vector, either truncating or padding with undefined if the lengths differ.
  * `Clash.Explicit.Reset.resetGlitchFilter`: filters glitchy reset signals. Useful when your reset signal is connected to sensitive actuators.
  * Clash can now generate EDAM for using Edalize. This generates edam.py files in all top entities with the configuration for building that entity. Users still need to edit this file to specify the EDA tool to use, and if necessary the device to target (for Quartus, Vivado etc.). [#1386](https://github.com/clash-lang/clash-compiler/issues/1386)
  * `-fclash-aggressive-x-optimization-blackboxes`: when enabled primitives can detect undefined values and change their behavior accordingly. For example, if `register` is used in combination with an undefined reset value, it will leave out the reset logic entirely. Related issue: [#1506](https://github.com/clash-lang/clash-compiler/issues/1506).
  * Automaton-based interface to simulation, to allow interleaving of cyle-by-cycle simulation and external effects [#1261](https://github.com/clash-lang/clash-compiler/pull/1261)


New internal features:
  * `constructProduct` and `deconstructProduct` in `Clash.Primitives.DSL`. Like `tuple` and `untuple`, but on arbitrary product types.
  * Support for multi result primitives. Primitives can now assign their results to multiple variables. This can help to work around synthesis tools limits in some cases. See [#1560](https://github.com/clash-lang/clash-compiler/pull/1560).
  * Added a rule for missing `Int` comparisons in `GHC.Classes` in the compile time evaluator. [#1648](https://github.com/clash-lang/clash-compiler/issues/1648)
  * Clash now creates a mapping from domain names to configurations in `LoadModules`. [#1405](https://github.com/clash-lang/clash-compiler/pull/1405)
  * The convenience functions in `Clash.Primitives.DSL` now take a list of HDLs, instead of just one.
  * `Clash.Netlist.Id` overhauls the way identifiers are generated in the Netlist part of Clash.
  * Added `defaultWithAction` to Clash-as-a-library API to work around/fix issues such as [#1686](https://github.com/clash-lang/clash-compiler/issues/1686)
  * Manifest files now list files and components in an reverse topological order. This means it can be used when calling EDA tooling without causing compilation issues.

Deprecated:
  * `Clash.Prelude.DataFlow`: see [#1490](https://github.com/clash-lang/clash-compiler/pull/1490). In time, its functionality will be replaced by [clash-protocols](https://github.com/clash-lang/clash-protocols).

Removed:
  * The deprecated function `freqCalc` has been removed.

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

Changed:
  * Due to difficulties using `resetSynchronizer` we've decided to make this function always insert a synchronizer. See: [#1528](https://github.com/clash-lang/clash-compiler/issues/1528).

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
