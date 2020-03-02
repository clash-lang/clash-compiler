.. _intro:

Introduction to Clash
=====================

Functional Hardware
-------------------

Clash_ is an open-source functional hardware description
language (HDL) that closely mirrors the syntax and semantics of the Haskell_
programming language. It is used for creating hardware designs, typically for
running on *field programmable gate arrays* (FPGAs) or *application-specific
integrated circuits* (ASICs).

.. _`Clash`: https://clash-lang.org
.. _`Haskell`: https://www.haskell.org

Clash is both a compiler, and a set of libraries for circuit design, that
transform high level Haskell descriptions of synchronous, sequential logic into
low-level VHDL_, Verilog_, or SystemVerilog_. It provides a unique approach to
design of sequential circuits, but with a high amount of abstraction power that
blurs the line between strictly behavioral or structural synthesis approaches.

.. _VHDL: https://en.wikipedia.org/wiki/VHDL
.. _Verilog: https://en.wikipedia.org/wiki/Verilog
.. _SystemVerilog: https://en.wikipedia.org/wiki/SystemVerilog

Clash aims to modernize the hardware development experience, making it easier
to quickly and correctly develop complex circuit designs. This is achieved
by making Clash:

Expressive
  Clash uses the Haskell type system to its full potential -- including modern
  extensions and techniques -- to being a high level of type safety and
  expressiveness to hardware design.

  This expressive typing makes it easier to develop safe, maintainable
  hardware. Combinatorial and sequential logic is separated by type, and global
  safety invariants such as separating incompatible clock domains are enforced
  in the type system.

Intuitive
  Clash makes it easy to express circuit designs in an intuitive manner,
  allowing high level structural components to be easily connected in designs.
  Moreover, unlike most "high level synthesis" tools, this extends to precise
  control over register placement and pipelining.

Interactive
  Unlike traditional HDL tools, Clash has a fully interactive read-eval-print
  loop (REPL), allowing circuits to be interactively designed and tested.

Performant
  Clash reuses parts of the `Glasgow Haskell Compiler`_ to provide fast
  simulation of circuits for development and testing.

Efficient
  Clash uses a "whole program synthesis" approach in order to view the entire
  circuit at once, and optimizes this design before translating to a specific
  target. This allows meaningful optimizations to be performed on the entire
  design.

Extensible
  Additional primitives and black boxes can be added to Clash in the language
  of your choice, allowing you to use your own vendor or IP library within
  projects.

  Clash allows seamless interoperability with libraries written in Haskell,
  including ``mtl``, ``lens`` and ``QuickCheck``. This makes it even easier to
  quickly prototype complex designs.

.. _`Glasgow Haskell Compiler`: https://ghc.haskell.org

Intended Audience
-----------------

Clash is ideal for developers from different backgrounds, although the main
intended audiences are

Hardware Engineers
  You are a hardware engineer, used to using tools like VHDL_ and Verilog_ to
  implement circuit designs. Clash offers the familar mixed simulation /
  synthesis capabilities of these tools, while providing a langauge with
  powerful abstractions.

Haskell Programmers
   You are a Haskell_ programmer, looking to start developing hardware. Clash
   offers the ability to start prototyping and simulating designs in a familiar
   environment -- lowering the learning curve significantly.

Maturity and Support
--------------------

Clash is a continually evolving tool, having been actively developed since
2009. With the release of Clash 1.0 there has been an increased focus on
maintaining API stability between releases, meaning circuit designs written
in Clash should continue to work between minor releases. Today, the Clash
Compiler is actively developed by QBayLogic B.V. and volunteers.

Several companies and enthusiasts are already using Clash to develop circuit
designs, ranging from small designs on hobbyist boards to larger designs on
modern FPGA and ASIC architectures.

While care is taken to thoroughly test the Clash compiler, some bugs may exist.
We encourage users to file issues, or contribute pull requests on our
`GitHub repository`_.

.. _`GitHub repository`: https://github.com/clash-lang/clash-compiler

Meta-information: Web Sites, Mailing Lists, etc.
------------------------------------------------

.. include:: ../../README.rst
  :start-after: community_start
  :end-before: community_end

Clash Version Numbering Policy
------------------------------

Clash follows the `Haskell PVP Specification <https://pvp.haskell.org>`_ for
its version numbers, for all packages. The main libraries that make up the
Clash compiler maintain the same version numbers, making it easy to identify
which versions are compatible.

.. note::
  Due to the Clash's tight integration with GHC, updates to the GHC version
  that Clash uses result in changes to the Clash version. As GHC's internals
  change frequently, even for minor bumps, it cannot be guaranteed that these
  changes will not result in Clash changes.

It is recommended (but not required) that downstream Clash packages and
published Clash code also follow the PVP specification.

