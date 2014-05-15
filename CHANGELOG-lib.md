# Changelog for the [`clash-lib`](http://hackage.haskell.org/package/clash-prelude) package

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
  * Circuit testbench generation with MAC example fails[#15](https://github.com/christiaanb/clash2/issues/8)
  * Missing begin keyword in Signed/Unsigned JSON files[#16](https://github.com/christiaanb/clash2/issues/16)

* Code improvements:
  * Refactor Netlist/BlackBox [#10](https://github.com/christiaanb/clash2/issues/10)
  * CPP special-case conversion of `Control.Exception.Base.irrefutPatError` [#11](https://github.com/christiaanb/clash2/issues/11)

