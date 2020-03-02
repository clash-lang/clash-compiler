.. _faqs:

Frequently Asked Questions
==========================

Basic Questions
---------------

- **Q**: How do I install Clash?

  **A**: Check out the :ref:`installing` page in the *Getting Started* section
  of the manual.

----

- **Q**: Is the name "Clash", "CLaSH", or "CλaSH"?

  **A**: It's **Clash**.

  In its research stages Clash was called "CλaSH", an acronym for the **C**\ AES
  **La**\ nguage for **S**\ ynchronous **H**\ ardware. CAES is a group of the
  Faculty of Electrical Engineering, Mathematics and Computer Science at the
  University of Twente. Clash was originally developed by Christaan Baaij and
  supervisor Jan Kuper. The stylization "CλaSH" is an homage to Haskell_,
  whose official logo has long been the venerable Greek *lambda* character.

.. _Haskell: https://haskell.org

----

- **Q**: Is Clash a "high level synthesis" tool?

  **A**: While clash provides a high level language features, hardware
  descriptions written in Clash are not decoupled from clock-level
  timing. Clash does therefore not offer what is generally understood as
  "high level synthesis". Compared to the big three hardware description
  languages, *VHDL*, *Verilog*, and *SystemVerilog*, Clash arguably *is*
  high-level. It offers many of the powerful abstractions that modern
  software programming languages offer. In fact, it inherits many of
  the software's industry bleeding-edge features by virtue of basing its
  implemenation on Haskell.

Clash Support
-------------

- **Q**: Is Clash production ready?

  **A**: Clash is constantly evolving, and since the 1.0 release there is
  a focus on maintaining API backwards compatibility. Clash is used
  successfully in real-world scenarios, and `QBayLogic Clash support`_ can help
  with education and implementation of Clash projects.

.. _`QBayLogic Clash support`: https://qbaylogic.com/clash-support.html

----

- **Q**: Will Clash work with my EDA tools?

  **A**: In general, Clash should work well with Xilinx and Intel FPAGs and
  their EDA tools -- as development typically focuses on these vendors. Clash
  has also been successfully used on Microsemi (formerly Actel) SmartFusion 2
  and Lattice Semiconductor iCE40 FPGAs, and some basic IP for these exist.

  For most toolchains, the default primitives supplied by Clash should work
  with minimal effort. If not, it is possible to call your vendor's library
  manually, or use a tool like Yosys_ to do mapping. It is also possible to
  consult `QBayLogic Clash support`_ for more assistance.

.. _Yosys: http://clifford.at/yosys

----

- **Q**: Does Clash support `Project IceStorm <http://clifford.at/icestorm>`_?

  **A**: The Verilog backend for Clash emits Verilog 2001, which is supported
  by Yosys_. This means it can be placed and packed with *arachne-pnr* and
  *icestorm*. Additionally, Clash has some support for the Lattice
  Semiconductor iCE40 FPGA.

----

- **Q**: Can Clash be used for ASIC designs, as well as FPGA designs?

  Clash can be used for ASIC designs, however the RTL produced by Clash may not
  be immediately suitable as it is largely platform agnostic. While this is
  not a problem for FPGAs, it can make developing ASICs more complicated as
  many ASIC vendors have different proprietary tool flows, with limited
  information available about their workings.

  If you are using Clash to develop for ASIC, and need assitance with getting
  your toolchain to work, you can contact `QBayLogic Clash support`_ for
  assistance.

Clash and Haskell
-----------------

- **Q**: Is Clash its own programming language, or is it "Haskell"?

  **A**: Clash is a programming language in its own right, complete with its
  own executable and standard library. Clash is also related to the Haskell
  programming language, and may be thought of as a dialect of Haskell for
  developing hardware. While the surface syntax and typing rules are the same,
  the semantics change as code progresses through the compilation pipeline.

  Do to the shared behavior in the early stages of the compiler, components
  from GHC (the most common Haskell compiler) are reused in the Clash compiler.
  This is how Clash achieves such high interoperability with existing Haskell
  projects.

----

- **Q**: Clash has better inference for type level natural numbers than GHC.
  How is this possible?

  **A**: Clash's enhanced type checking functionality is due to the use of GHC
  compiler plugins, which can be used in any Haskell project. To enable these
  plugins, pass the following compiler flags to GHC:

  .. code-block:: haskell

      {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
      {-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver    #-}
      {-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

  These plugins come from the ``ghc-typelits-natnormalise``,
  ``ghc-typelits-extra``, and ``ghc-typelits-knownnat`` packages respectively,
  which are all available from Hackage and Stackage.

----

- **Q**: Do I need to know Haskell in order to use Clash?

  **A**: As Clash is deeply integrated with Haskell, it is recommended that
  users have some familiarity with Haskell, or functional programming in
  general. Clash uses some advanced features of Haskell, and real-world designs
  will often want to leverage the existing Haskell ecosystem.

  For developers who are particularly familiar with either Haskell or hardware
  design, Clash should be relatively intuitive to use. Additionally, obvious
  mistakes with designs will be identified and reported due to the strong type
  system identifying mistakes at compile-time.

Clash and other HDL Tools
-------------------------

- **Q**: Do I need to know existing RTL/HDL languages in order to use Clash?

  **A**: Clash currently outputs VHDL, Verilog, and SystemVerilog. While it's
  not necessary to understand these descriptions, you will need to some
  understanding of vendor tools to actually deploy it.

----

- **Q**: What's the difference between Clash and "Lava"?

  **A**: Lava dialects (including the modern variant Blarney) use deep-embedding
  (in other words, are EDSLs) instead of Clash's static analysis approach. The
  latter has a number of advantages:

    1. Clash can reliably detect sharing, while observable sharing within EDSLs
       is of a stochastic nature.
    2. Clash allows the use normal haskell operations such as (==) on both the
       meta-level (how the program is structured/generated), and the object-level
       (the functionality of the program).
    3. Clash allows the use of regular haskell syntax to model the concept of
       'choice' at the object-level (the functionality of the program):
       if-expressions, guards, case, etc.
    4. Clash allows programmers to use native Haskell pattern matching.

----

- **Q**: What's the difference between Clash and Chisel/Spinal, or Hardcaml?

  **A**: The biggest difference between these two toolchains is that Clash
  exists as a Haskell derivative, with a full synthesizing compiler to RTL --
  while Chisel exists as an embedding of hardware semantics inside Scala. Aside
  from the "host language" differences, this means that Chisel is conceptually
  closer to something like *Lava* than Clash. Hardware descriptions in both
  Chisel and *Lava* describe how to generate a hardware description instead of
  describing hardware directly. As a consequence these languages do not offer
  simulation in their host languages.

  Another fair point worth mentioning is that while Clash and Chisel have both
  been around for numerous years, Chisel has quite a lot more infrastructure and
  has public, taped out production cores (in the form of e.g. `Rocket
  <https://github.com/freechipsproject/rocket-chip>`_ and `BOOM
  <https://github.com/ucb-bar/riscv-boom>`_). Chisel also has accompanying add-on
  tools, such as `Spatial <https://github.com/stanford-ppl/spatial-lang>`_,
  which allow the clean co-development of hardware and software, all within
  Scala.

  Similarly, Hardcaml is an embedded DSL for RTL semantics, using `OCaml
  <https://ocaml.org>`_ as the host language.

