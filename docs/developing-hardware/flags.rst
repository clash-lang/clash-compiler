.. _flags:

Clash Compiler Flags
====================

--vhdl
  Use the VHDL backend for code generation. This currently emits VHDL 1993
  source which can be consumed by other tools.

--verilog
  Use the Verilog backend for code generation. This currently emits
  Verilog 2001 source which can be consumed by other tools.

--systemverilog
  Use the SystemVerilog backend for code generation. This currently emits
  SystemVerilog 2012 source which can be consumed by other tools.

-fclash-debug
  Set the debugging mode for the compiler, exposing additional output. The
  available options are

  - ``DebugNone`` to show no debug messages
  - ``DebugSilent`` to test invariants and error if any are violated.
    This is implicitly enabled by any debug flag

  - ``DebugFinal`` to show expressions after they have been completely
    normalized

  - ``DebugName`` to show the names of transformations as they are applied
  - ``DebugTry`` to show names of tried and applied transformations
  - ``DebugApplied`` to show sub-expressions after they are rewritten
  - ``DebugAll`` to show all sub-expressions when a rewrite is attempted

  **Default:** ``DebugNone``

-fclash-debug-transformations
  List the transformations that are to be debugged. This is given as a
  comma-separated list of transformations, e.g.

  .. code-block:: bash

    clash -fclash-debug-transformations inlineNonRep,topLet,appProp

  **Default:** []

-fclash-debug-transformations-from
  Only print debug output from applied transformation ``n`` and onwards.

  .. code-block:: bash

    clash -fclash-debug-transformations-from=21570

  **Default:** 0

-fclash-debug-transformations-limit
  Only print debug output for ``n`` applied transformations.

  .. code-block:: bash

    clash -fclash-debug-transformations-limit=12

  **Default:** MAX_INT

-fclash-hdldir
  Specify the directory that generated HDL is written into. Generated code
  is still put into a directory named according to the output language within
  this directory. For example

  .. code-block:: bash

    clash -fclash-hdldir build/hdl

  will create a directory ``build/hdl/verilog`` if verilog is generated.

  **Default:** "."

-fclash-hdlsyn
  Specify the HDL synthesis tool which will be used. Available options are
  ``Vivado``, ``Quartus`` and ``Other``, but some synonyms for these exist
  (``Xilinx`` and ``ISE`` are synonyms for ``Vivado``, ``Altera`` and
  ``Intel`` are synyonyms for ``Quartus``).

  **Default:** ``Other``

-fclash-no-cache
  Don't reuse previously generated output from Clash, instead generating HDL
  from a clean state. While this leads to longer builds, it can be useful in
  development.

  .. warning:: Previously this flag was called ``-fclash-nocache``, however
    this is now deprecated.

  **Default:** Cache generated HDL

-fclash-no-check-inaccessible-idirs
  Check that all include directories (containing primitives) exist when running
  Clash. If any directory does not exist, an error is thrown.

  **Default:** Check directories

-fclash-no-clean
  Don't remove all previously generated HDL files before generating the new
  file. If Clash cannot generate the new file for whatever reason, the
  previously generated file will still be avilable.

  .. warning:: Previously this flag was called ``-fclash-noclean``, however
    this is now deprecated.

 **Default:** Clean before build

-fclash-no-prim-warn
  Disable warnings for primitives that are annotated with ``warnAlways``. This
  means warnings from annotations like

  .. code-block:: haskell

    {-# ANN f (warnAlways "This primitive is dangerous") #-}

  will not be shown when compiling.

  **Default:** Show warnings

-fclash-spec-limit
  Change the number of times a function can undergo specialization.

  **Default:** 20

-fclash-inline-limit
  Change the number of times a function ``f`` can undergo inlining inside some
  other function ``g``. This prevents the size of ``g`` growing dramatically.

  **Default:** 20

-fclash-inline-function-limit
  Set the threshold for function size. Below this threshold functions are
  always inlined (if it is not recursive).

  **Default:** 15

-fclash-inline-constant-limit
  Set the threshold for constant size. Below this threshold constants are
  always inlined. A value of 0 inlines all constants.

  **Default:** 0

-fclash-intwidth
  Set the bit width for the ``Int/Word/Integer`` types. The only allowed values
  are 32 or 64.

  **Default:** Machine word size (``WORD_SIZE_IN_BITS``)

-fclash-error-extra
  Print additional information with compiler errors if it as available. If
  there is extra information and this flag is not enabled, a message will be
  printed suggesting this flag.

  **Default:** False

-fclash-float-support
  Enable support for floating point numbers. If this is disabled, Clash will
  not attempt to convert Float and Double values for hardware.

  **Default:** False

-fclash-component-prefix
  Prefix the names of generated HDl components with a string. For example a
  component ``foo`` would be called ``xcorp_foo`` if run with

  .. code-block:: bash

    clash -fclash-component-prefix "xcorp"

  **Default:** ""

-fclash-old-inline-strategy
  The new inlining strategy for Clash inlines all functions which are not
  marked with ``NOINLINE`` or a synthesize attribute. The old inlining strategy
  differed, attempting only to inline functions which were deemed "cheap".
  The old inlining strategy may be quicker in practice for some circuits.

  **Default:** False

-fclash-no-escaped-identifiers
  Disable extended identifiers, as used in some HDLs like VHDL to allow more
  flexibility with names. Clash will only generate normal idenfiers if this
  is used.

  **Default:** Escaped identifiers are allowed

-fclash-compile-ultra
  Aggressively run the normalizer, potentially gaining much better runtime
  performance at the expense of compile time.

  **Default:** False

-fclash-force-undefined{,0,1}
  Set the value to use when an undefined value is inserted into generated HDL.
  This flag can be suffixed with either 0 or 1 to force use of that bit, or
  left without a suffix to use a HDL-specific default (e.g. ``x`` in Verilog).

  **Default:** Disabled

-fclash-aggressive-x-optimization
  Remove all undefined branches from case expressions, replacing them with
  another defined value in the expression. If only one branch is defined, the
  case expression is elided completely. If no branches are defined the entire
  expression is replaced with a call to ``errorX``.

  **Default:** False

-main-is
  When using one of ``--vhdl``, ``--verilog``, or ``--systemverilog``, this
  flag refers to synthesis target. For example, running Clash with
  ``clash My.Module -main-is top --vhdl`` would synthesize ``My.Module.top``.
