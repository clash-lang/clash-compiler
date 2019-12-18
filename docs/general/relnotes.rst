.. _relnotes:

Release Notes for Clash 1.0
===========================

The significant changes to core parts of the compiler and libraries are listed
in the following sections. Small bug fixes and performance improvements are not
listed here, for more information see the changelog_.

.. _changelog: https://github.com/clash-lang/clash-compiler/blob/1.0/CHANGELOG.md

.. note::
  Clash 1.0.0 is currently tested on GHC versions 8.4.4, 8.6.5 and 8.8.1. Using
  any GHC version outside of these may work, but is not officially supported.

Highlights
----------

Clash has seen several major updates for the 1.0 release, the most significant
improvements being:

- New, explicit clock lining and reset control, along with a simpler interface
  based on implicit parameters. As a result of this change, features like DDR
  and PLL primitives can now be defined almost entirely using Clash and some
  basic primitives.

  .. warning::
    This effectively breaks all known Clash programs, due to the ``Signal``
    type changing.

- A new namespace for ``clash-prelude``. Previously, it was under the name
  ``CLaSH``, but is now under the name ``Clash``.

  .. warning::
    This breaks all known Clash programs.

- Support for multiple ``TopEntity`` annotations in a single project.
  Previously, the Clash compiler only allowed you to have a single
  ``topEntity`` function within your set of modules, and would only generate a
  single HDL module per invocation. Clash now handles multiple ``TopEntity``
  functions and generates corresponding HDL modules.

- An improved optimizer has landed, that does better constant folding, is more
  aggressive in eliminating indirection, and generates more readable HDL output.

- The compile-time performance of the RTL synthesis has improved, in some cases
  exponentially (with respect to the input program). Additionally, the compiler
  does not generate large programs from small ones as much.

Full Details
------------

.. todo:: Lorem ipsum...

- The ``clash-{vhdl,verilog,systemverilog}`` packages no longer exist, and have
  been folded into ``clash-lib`` directly under the same module names. They've
  proven stable enough and do not require separate external packages.

- Several fixes to the Verilog and SystemVerilog backends concerning ``Integer``
  truncation and out-right compilation errors have been fixed. Please see Clash
  issues `#219 <https://github.com/clash-lang/clash-compiler/issues/219>`_,
  `#220 <https://github.com/clash-lang/clash-compiler/issues/220>`_, and `#222
  <https://github.com/clash-lang/clash-compiler/issues/222>`_.

Libraries
---------

Changes to core libraries maintained by the Clash developers are listed below.

clash-prelude
~~~~~~~~~~~~~

.. todo:: Lorem ipsum...

- The ``clash-prelude`` namespace now starts with ``Clash``, not ``CLaSH``!
  (`issue #125 <https://github.com/clash-lang/clash-prelude/issues/222>`_)

  .. warning:: This effectively breaks all known Clash programs! But you can fix
               it with ``sed``.

- Added the ``plusToLe`` and ``plusToLeKN`` functions to ``Clash.Promoted.Nat``.

- The superclass constraint for the ``Foldable`` instance of
  ``Clash.Sized.Vector.Vec`` has been improved: previously it required an
  awkward type equality constraint (``m ~ (n+1)``), whereas now it requires a
  simple inequality constraint (``1 <= n``).

- Added fixities for all infix-versions of ``SNat`` related operators, under
  ``Clash.Promoted.Nat`` (`issue #113
  <https://github.com/clash-lang/clash-prelude/issues/222>`_).

- Added ``BitPack`` instances for ``GHC.Generics`` product types (`issue #112
  <https://github.com/clash-lang/clash-prelude/issues/112>`_).

- Added an ``leToPlus`` function to ``Clash.Promoted.Nat`` (`issue #111
  <https://github.com/clash-lang/clash-prelude/issues/111>`_).

- Added a bounds check to ``Clash.Sized.Index.maxBound`` internally. (`issue #89
  <https://github.com/clash-lang/clash-prelude/issues/89>`_).

clash-intel
~~~~~~~~~~~

This is a brand new library, providing PLL and DDR primitives for Intel
(formerly *Altera*) devices. These primitves are available under the
``Clash.Intel`` namespace.

clash-lattice
~~~~~~~~~~~~~

This is a brand new library, providing PLL primitives for Lattice Semiconductor
iCE40 devices. These primitives are available under the ``Clash.Lattice``
namespace.

clash-xilinx
~~~~~~~~~~~~

This is a brand new library, providing PLL and DDR primitives for Xilinx
Series-7 (and later) devices. These primitives are available under the
``Clash.Xilinx`` namespace.


Known bugs and limitations
--------------------------

- Clash occasionally has bad compilation and synthesis complexity (in both time
  and space) on certain inputs. See `clash-compiler issue #240
  <https://github.com/clash-lang/clash-compiler/issues/240>`_ and
  `clash-compiler issue #251
  <https://github.com/clash-lang/clash-compiler/issues/251>`_ for both big and
  small examples. This is sometimes possible to work around (via rewriting or
  compiler option magic), and other times is not. If you suspect your circuit
  has unreasonably high memory usage or synthesis time, please file a bug with
  reproducible instructions and a "minimum viable sample" so we can help.

  .. note:: This only affects RTL synthesis; compilation of simulations to
            native executable code is still quite fast and efficient.

- Asynchronous and synchronous resets are globally positive in the current
  design of ``clash-prelude``. While "reset polarity" polymorphism for the
  ``Signal`` type (allowing async/sync positive *and* negative resets, and
  mixing them) is possible, it currently makes the API more complex.

  As a workaround, users can redefine primitive ``.json`` mappings for their
  designs, and remap ``clash-prelude`` functions (e.g. write a new mapping for
  ``Clash.Signal.register`` that uses negative resets and use this as
  necessary). (TODO FIXME: ref link) Currently ``clash-prelude`` offers no
  alternative primitive mappings for negative resets.

  It is unclear in the future whether or not, and how, this restriction may be
  lifted. (Features like Backpack may, in future GHC/Clash releases, make this
  possible.)

- Clash currently does not support `inout` parameters for compiled RTL code in
  any way, for any of its backends. In the future, this limitation may be lifted
  to some degree. See `clash-compiler issue #239
  <https://github.com/clash-lang/clash-compiler/issues/239>`_ for more.

- Clash does not allow pattern matching on "structurally recursive" GADT types,
  that would otherwise provide a type-driven proof of terminating recursion. As
  a result, you cannot pattern match on any GADTs.

  In the near future, we plan on lifting this restriction for the built-in
  ``Cons`` data type. In a farther future, we plan on lifting this restriction
  for *all* appropriately defined GADTs. This requires a new core language and
  synthesis analysis pass.

  See `clash-compiler issue #170
  <https://github.com/clash-lang/clash-compiler/issues/170>`_ for more.

- Clash needlessly recompiles any module that uses a compiler plugin, regardless
  of if it needs to be recompiled or not. This affects all compiler plugins and
  their users, but especially Clash users, as Clash comes equipped with several
  compiler plugins for type checking. For large Clash codebases, this often
  slows recompilation of large builds, as a build that would otherwise be a "no
  op" demands many needless recompilations.

  This bug is due to an upstream GHC limitation -- see `GHC issue #7414
  <https://ghc.haskell.org/trac/ghc/ticket/7414>`_ for more information. We hope
  to fix this in a future GHC release.

  There is currently no workaround for this bug.

- Clash occasionally has unnecessary overhead in the resulting circuits it
  generates. While Clash is *normally* quite low-level and "space efficient",
  has an aggressive "whole program" synthesizer, and generated circuits are
  often small -- the compiler currently *does not* remove all forms of
  compile-time overhead, as of right now.

  Working around this often requires deep knowledge of the Haskell toolchain and
  the input program. If you suspect the Clash compiler is generating
  *needlessly* inefficient circuits, please file a bug so we can reproduce it
  and help.

  In the future, we plan to tackle this with more aggressive optimizations (e.g.
  better constant propagation) and, inevitably, techniques like `Partial
  Evaluation <https://en.wikipedia.org/wiki/Partial_evaluation>`_.

- TODO FIXME: mention lack of register retiming passes, and FAQ entry, issue
  #165.

- TODO FIXME: mention that compiled clash code (e.g. a library from hackage)
  cannot use bang patterns
