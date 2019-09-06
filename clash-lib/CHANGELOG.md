# Changelog for the [`clash-lib`](http://hackage.haskell.org/package/clash-lib) package

## 1.0.0 *September 3rd 2019*
* 10x - 50x faster compile times
* New features:
  * API changes: check the migration guide at the end of `Clash.Tutorial`
  * All memory elements now have an (implicit) enable line; "Gated" clocks have
    been removed as the clock wasn't actually gated, but implemented as an
    enable line.
  * Circuit domains are now configurable in:
    * (old) The clock period
    * (new) Clock edge on which memory elements latch their inputs
      (rising edge or falling edge)
    * (new) Whether the reset port of a memory element is level sensitive
      (asynchronous reset) or edge sensitive (synchronous reset)
    * (new) Whether the reset port of a memory element is active-high or
      active-low (negated reset)
    * (new) Whether memory element power on in a configurable/defined state
      (common on FPGAs) or in an undefined state (ASICs)

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

## 0.99.1 *May 12th 2018*
* Allow `~NAME[N]` tag inside `~GENSYM[X]`
* Support HDL record selector generation [#313](https://github.com/clash-lang/clash-compiler/pull/313)

## 0.99 *March 31st 2018*
* New features:
  * Support for `clash-prelude-0.99`:
      * Explicit clock and reset arguments
      * Overhaul of `TopEntity` annotations
  * Hierarchies with multiple synthesisable boundaries by allowing more than one
    function in scope to have a `Synthesize` annotation.
    * Local caching of functions with a `Synthesize` annotation
  * `Bit` type is mapped to a HDL scalar type (e.g. `std_logic` in VHDL)
  * Improved name preservation
  * Zero-bit values are filtered out of the generated HDL
  * Improved compile-time computation
* Many bug fixes

## 0.7.1 *April 11th 2017*
* New features:
  * Support distribution of primitive templates with Cabal/Hackage packages [commit](https://github.com/clash-lang/clash-compiler/commit/82cd31863aafcbaf3bdbf7746d89d13859af5aaf)
  * Find memory data files and primitive files relative to import dirs (`-i<DIR>`)
* Fixes bugs:
  * `case (EmptyCase ty) of ty' { ... }` -> `EmptyCase ty'` [#198](https://github.com/clash-lang/clash-compiler/issues/198)
  * `BitVector.split#` apply the correct type arguments

## 0.7 *January 16th 2017*
* New features:
  * Support for `clash-prelude` 0.11
  * Primitive templates can include QSys files
  * VHDL blackboxes: support additional libraries and uses keywords in generated VHDL
  * Highly limited Float/Double support (literals and `Rational` conversion), hidden behind the `-clash-float-support` flag.
* Fixes bugs:
  * Reduce type families inside clock period calculation [#180](https://github.com/clash-lang/clash-compiler/issues/180)
  * Only output signed literals as hex when they're multiple of 4 bits [#187](https://github.com/clash-lang/clash-compiler/issues/187)
  * Correctly print negative hex literals

## 0.6.21 *August 18th 2016*
* Fixes bugs:
  * Type families are not currently being reduced correctly [#167](https://github.com/clash-lang/clash-compiler/issues/167)

## 0.6.20 *August 3rd 2016*
* Fixes bugs:
  * Bug in DEC transformation overwrites case-alternatives
  * Bug in DEC transformation creates non-representable let-binders

## 0.6.19 *July 19th 2016*
* Fixes bugs:
  * Rounding error in `logBase` calculation

## 0.6.18 *July 15th 2016*
* New features:
  * Better error location reporting
* Fixes bugs:
  * Values of type Index 'n', where 'n' > 2^MACHINE_WIDTH, incorrectly considered non-synthesisable due to overflow

## 0.6.17 *June 9th 2016*
* Fixes bugs:
  * `Eq` instance of `Vec` sometimes not synthesisable

## 0.6.16 *June 7th 2016*
* New features:
  * DEC transformation also lifts HO-primitives applied to "interesting" primitives (i.e. `zipWith (*)`)
* Fixes bugs:
  * replicate unfolded incorrectly [#150](https://github.com/clash-lang/clash-compiler/issues/150)
  * `imap` is not unrolled [#151](https://github.com/clash-lang/clash-compiler/issues/151)

## 0.6.15 *April 7th 2016*
* New features:
  * Up to 2x reduced compilation times when working with large `Vec` literals
* Fixes bugs:
  * Bug in DEC transformation throws CLaSH into an endless loop [#140](https://github.com/clash-lang/clash-compiler/issues/140)

## 0.6.14 *March 21st 2016*
* New features:
  * Also generate testbench for circuits without input ports [#135](https://github.com/clash-lang/clash-compiler/issues/135)
* Fixes bugs:
  * `clockWizard` broken [#49](https://github.com/clash-lang/clash-prelude/issues/49)

## 0.6.13 *March 14th 2016*
* Fixes bugs:
  * Not all lambda's in a function position removed

## 0.6.12 *March 14th 2016*
* Fixes bugs:
  * Not all lambda's in a function position removed due to bad eta-expansion [#132](https://github.com/clash-lang/clash-compiler/issues/132)

## 0.6.11 *March 11th 2016*
* New features:
  * Add support for HDL synthesis tool specific HDL generation
  * Preserve more Haskell names in generated HDL [#128](https://github.com/clash-lang/clash-compiler/issues/128)

## 0.6.10 *February 10th 2016*
* New features:
  * hdl files can be written to a directory other than the current working directory [#125](https://github.com/clash-lang/clash-compiler/issues/125)
* Fixes bugs:
  * `caseCon` transformation does not work on non-exhaustive case-expressions [#123](https://github.com/clash-lang/clash-compiler/issues/123)
  * Primitive reductions don't look through `Signal` [#126](https://github.com/clash-lang/clash-compiler/issues/126)

## 0.6.9
* Fixes bugs:
  * `case undefined of ...` should reduce to `undefined` [#116](https://github.com/clash-lang/clash-compiler/issues/109)
  * Type families obscure eligibility for synthesis [#114](https://github.com/clash-lang/clash-compiler/issues/114)

## 0.6.8 *January 13th 2015*
* New features:
  * Support for Haskell's: `Char`, `Int8`, `Int16`, `Int32`, `Int64`, `Word`, `Word8`, `Word16`, `Word32`, `Word64`.
  * Int/Word/Integer bitwidth for generated HDL is configurable using the `-clash-intwidth=N` flag, where `N` can be either 32 or 64.
* Fixes bugs:
  * Cannot reduce `case error ... of ...` to `error ...` [#109](https://github.com/clash-lang/clash-compiler/issues/109)

## 0.6.7 *December 21st 2015*
* Support for `unbound-generics-0.3`
* New features:
  * Only look for 'topEntity' in the root module. [#22](https://github.com/clash-lang/clash-compiler/issues/22)

## 0.6.6 *December 11th 2015*
* New features:
  * Remove all existing HDL files before generating new ones. This can be disabled by the `-clash-noclean` flag. [#96](https://github.com/clash-lang/clash-compiler/issues/96)
  * Support for `clash-prelude` 0.10.4

## 0.6.5 *November 17th 2015*
* Fixes bugs:
  * Integer literals used as arguments not always properly annotated with their type.

## 0.6.4 *November 12th 2015*
* Fixes bugs:
  * Reversing alternatives is not meaning preserving for literal patterns [#91](https://github.com/clash-lang/clash-compiler/issues/91)
  * DEC: root of the case-tree must contain at least 2 alternatives [#92](https://github.com/clash-lang/clash-compiler/issues/92)

## 0.6.3 *October 24th 2015*
* New features:
  * Improve DEC transformation: consider alternatives before the subject when checking for disjoint expressions.
* Fixes bugs:
  * DEC: don't generate single-branch case-expressions [#90](https://github.com/clash-lang/clash-compiler/issues/90)

## 0.6.2 *October 21st 2015*
* Fixes bugs:
  * DEC: Subject and alternatives are not disjoint [#88](https://github.com/clash-lang/clash-compiler/issues/88)

## 0.6.1 *October 16th 2015*
* New features:
  * Support for `clash-prelude` 0.10.1
  * Transformation that lifts applications of the same function out of alternatives of case-statements. e.g.

    ```haskell
    case x of
      A -> f 3 y
      B -> f x x
      C -> h x
    ```

    is transformed into:

    ```haskell
    let f_arg0 = case x of {A -> 3; B -> x}
        f_arg1 = case x of {A -> y; B -> x}
        f_out  = f f_arg0 f_arg1
    in  case x of
          A -> f_out
          B -> f_out
          C -> h x
    ```

* Fixes bugs:
  * Case-statements acting like normal decoder circuits are erroneously synthesised to priority decoder circuits.

## 0.6 *October 3rd 2015*
* New features:
  * Support for `clash-prelude` 0.10
  * Add `~INDEXTYPE` tag: primitives get access to the `Index` clash-prelude type
  * Add `~IF` construct: primitives can do conditional templating
  * Unroll "definitions" of the following primitives: `fold`, `dfold`, `foldr`

## 0.5.13 *September 21st 2015*
* Fixes bugs:
  * Performance bug: top-level definitions of type "Signal" erroneously inlined.

## 0.5.12 *September 14th 2015*
* New features:
  * Completely unroll "definitions" of some higher-order primitives with non-representable argument or result vectors:
    It is now possible to translate e.g. `f xs ys = zipWith ($) (map (+) xs) ys :: Vec 4 Int -> Vec 4 Int -> Vec 4 Int`

* Fixes bugs:
  * `topLet` transformation erroneously not performed in a top-down traversal
  * Specialisation limit unchecked on types and constants
  * Vector of functions cannot be translated [#25](https://github.com/clash-lang/clash-compiler/issues/25 )
  * CLaSH fails to generate VHDL when map is applied [#78](https://github.com/clash-lang/clash-compiler/issues/78)

## 0.5.11 *September 7th 2015*
* Fixes bugs:
  * Clash running out of memory on Simple-ish project [#70](https://github.com/clash-lang/clash-compiler/issues/70)
  * `CLaSH.Sized.Vector.:>` was not allowed as a function argument to HO-primitives

## 0.5.10 *August 2nd 2015*
* Fixes bugs:
  * Make testbench generation deterministic
  * Compile against unbound-generics-0.2

## 0.5.9 *July 9th 2015*
* Fixes bugs:
  * `coreView` didn't look through newtypes of the form: `newtype Foo a = MkFoo (Maybe a)`

## 0.5.8 *June 26th 2015*
* Fixes bugs:
  * Allow text and tags in ~SIGD black box construct

## 0.5.7 *June 25th 2015*
* New features:
  * Support for copying string literals from Haskell to generated code
  * Collect and copy data-files

* Fixes bugs:
  * Signals declared twice when not using a clock-generating component [#60](https://github.com/clash-lang/clash-compiler/issues/60)
  * This piece of code eat up all CPU when generating verilog [#62](https://github.com/clash-lang/clash-compiler/issues/60)

## 0.5.6 *June 3rd 2015*
* New features:
  * Support Verilog backend
  * Generated component names are prefixed by the name of the module containing the `topEntity`

## 0.5.5 *May 18th 2015*
* New features:
  * Make inlining and specialisation limit configurable
  * Make debug message level configurable

* Fixes bugs:
  * Netlist: ensure that the arguments of a component instantiation are always simple variables
  * CaseCon transformation: ensure that we run the compile-time evaluator on the subject before handling the one-alternative case
  * Emit a warning if a function remains recursive, instead of producing an error: compilation can still be successful if the function is an argument to a higher-order blackbox that doesn't use the function.
  * Emit a warning if inlining limit is reached, instead of producing an error: compilation can still be successful if the function is an argument to a higher-order blackbox that doesn't use the function.
  * Always inline terms that have a type of kind `Constraint`

## 0.5.4 *May 10th 2015*
* New features:
  * Add `~COMPNAME` tag: primitives get access to the component name in which they are instantiated

## 0.5.3 *May 5th 2015*
* New features:
  * `TopEntity` wrappers are now specified as `ANN` annotation pragmas
* Fixes bugs:
  * Lost system1000 clock in VHDL generation... [#53](https://github.com/clash-lang/clash-compiler/issues/53)
  * `flattenCallTree` sometimes introduces free variables

## 0.5.2 *May 1st 2015*
* New features:
  * Generate wrappers around `topEntity` that have constant names and types

## 0.5.1 *April 20th 2015*
* GHC 7.10 support

## 0.5 *March 11th 2015*
* New features:
  * Simplify BlackBox handling, and improve VHDL generation. [#47](https://github.com/clash-lang/clash-compiler/issues/47)
  * Use unbound-generics. [#48](https://github.com/clash-lang/clash-compiler/issues/48)

* Fixes bugs:
  * VHDL generation error: wrapper for sum-of-products type. [#44](https://github.com/clash-lang/clash-compiler/issues/44)

## 0.4.1 *February 4th 2015*
* Fixes bugs:
  * Treat BlackBox expressions as declarations when DC args. [#37](https://github.com/christiaanb/clash2/issues/33)
  * Don't inline recursive closed bindings

## 0.4 *November 17th 2014*
* New features:
  * Support for clash-prelude 0.6

* Fixes bugs:
  * Ambiguous type: 'std_logic_vector' or 'std_ulogic_vector' [#33](https://github.com/christiaanb/clash2/issues/33)

## 0.3.2 *June 5th 2014*

* Fixes bugs:
  * VHDL array constant ambiguous [#18](https://github.com/christiaanb/clash2/issues/18)
  * Exception: can't create selector [#24](https://github.com/christiaanb/clash2/issues/24)
  * Calls to `vhdlTypeMark` don't result to inclusion of VHDL type in types.vhdl [#28](https://github.com/christiaanb/clash2/issues/28)

## 0.3.1 *May 15th 2014*

* New features:
  * Make ANF lift non-representable values [#7](https://github.com/christiaanb/clash2/issues/7)
  * Hardcode `fromInteger` for `Signed` and `Unsigned` [#9](https://github.com/christiaanb/clash2/issues/9)
  * Replace VHDL default hole by error hole [#13](https://github.com/christiaanb/clash2/issues/13)

* Fixes bugs:
  * Type families are not expanded [#3](https://github.com/christiaanb/clash2/issues/3)
  * Exception: CLaSH.Netlist.VHDL(512): fromSLV: Vector 13 Bool [#5](https://github.com/christiaanb/clash2/issues/5)
  * Incorrect vhdl generation for default value in blackbox [#6](https://github.com/christiaanb/clash2/issues/6)
  * Duplicate type names when multiple ADTs need the same amount of bits [#8](https://github.com/christiaanb/clash2/issues/8)
  * Circuit testbench generation with MAC example fails[#15](https://github.com/christiaanb/clash2/issues/15)

* Code improvements:
  * Refactor Netlist/BlackBox [#10](https://github.com/christiaanb/clash2/issues/10)
  * CPP special-case conversion of `Control.Exception.Base.irrefutPatError` [#11](https://github.com/christiaanb/clash2/issues/11)
