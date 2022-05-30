The Clash Compiler
==================

Prerequisites
-------------

Hacking on Clash requires more dependencies than simply running Clash. The
test suite requires having a tool available to synthesize any backend being
tested. This means you need

- ghdl_ installed to test *VHDL*
- iverilog_ installed to test *Verilog*
- ModelSim_ installed to test *System Verilog*
- SymbiYosys_ and Z3_ installed to test *Verilog* and *System Verilog*

.. _ghdl: https://github.com/ghdl/ghdl
.. _iverilog: https://github.com/steveicarus/iverilog
.. _ModelSim: https://fpgasoftware.intel.com/?product=modelsim_ae#tabs-2
.. _SymbiYosys: https://github.com/YosysHQ/SymbiYosys
.. _Z3: https://github.com/Z3Prover/z3

Subprojects
-----------

The Clash compiler consists of different cabal libraries, which together
provide a complete compiler. Primarily, this consists of

``clash-ghc``

  The front-end of the compiler, using parts of the GHC front-end. This
  provides the ability to load modules, translate GHC Core to Clash Core, and
  implements the ``clash`` and ``clashi`` executables.

  A lot of the code in this library is separated by the version of GHC it works
  with. For example, ``src-bin-9.0`` is specific to GHC 9.0.x.

``clash-lib``

  The back-end of the compiler, exposed as a library. This is the largest
  library in the project, and includes the various ASTs (e.g. Core, Netlist),
  normalization, code generation, and primitives / black boxes.

``clash-prelude``

  The standard library for Clash as a language. This includes anything that
  is used to develop hardware in Clash, such as Signals, Clocks and combinators
  for common forms of state machine.

  The ``clash-prelude`` library also re-exports parts of the Haskell ``base``
  library, allowing circuit designs to re-use common functions and definitions.

The repository also contains other libraries. These either provide additional
functionality which is not required, or are not yet production-ready. These are

``clash-cores``

  A collection of IP cores for use in Clash designs. Currently, this includes
  Lattice Ice IO cores, SPI (with slaves implemented with the Lattice
  SBIO found on Lattice FPGAs), a UART and support for Xilinx floating point IP.

  .. note:: This library is optional, and is not required to use Clash. In
    the future it may be extended with additional IP cores.

``clash-cosim``

  Co-simulation for Clash, allowing Verilog to be run inline as though it were
  a normal Haskell function. This provides a QuasiQuoter for use in Haskell.

  .. warning:: This library is very experimental, and is not guaranteed to work
    with the most recent development version of Clash.

``clash-term``

  A development tool for analysing how the normalizer in ``clash-lib`` affects
  the core of a particular design. It allows the result of each different
  optimizer pass to be seen for debugging purposes.

``clash-lib-hedgehog``

  Hedgehog Generators for ``clash-lib``.

``clash-prelude-hedgehog``

  Hedgehog Generators for ``clash-prelude``.
