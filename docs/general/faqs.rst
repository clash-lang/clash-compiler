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

Clash and other HDLs
--------------------

- **Q**: Do I need to know existing RTL/HDL languages in order to use Clash?

  **A**: Clash currently outputs VHDL, Verilog, and SystemVerilog. While it's
  not necessary to understand these descriptions, you will need to some
  understanding of vendor tools to actually deploy it.

----

- **Q**: What's the difference between Clash and "Lava"?

  **A**: Lava dialects (including the modern variant
  `Blarney <https://github.com/mn416/blarney>`_) are all embedded domain specific
  languages (EDSLs) inside Haskell. On top of that they use a so-called
  *deep* embedding to be able to transform a circuit description into a netlist
  (to subsequently output that as a VHDL/Verilog file). Clash on the other hand
  uses "standard" compiler techniques to create a netlist from the Haskell
  abstract syntax tree (AST). This "standard" compiler technique enables the
  following features not available in (Haskell-based) EDSLs:

    1. Clash allows the use of normal Haskell operations such as (==) on both the
       meta-level (how the program is structured/generated), and the object-level
       (the functionality of the program).
    2. Clash allows the use of regular Haskell syntax to model the concept of
       'choice' at the object-level (the functionality of the program):
       if-expressions, guards, case, etc.
    3. Clash allows programmers to use native Haskell pattern matching.

Basically, with Clash you can use regular Haskell to describe the behavior of
the circuit, most importantly all of it's choice-constructs (case-expressions,
guards, etc.). With an EDSL you are "limited" by the constructs of the DSL,
making your circuit descriptions look less like regular Haskell functions.

----

- **Q**: What's the difference between Clash and Chisel/Spinal/Migen/Hardcaml?

  **A**: The biggest difference between these toolchains and Clash is that Clash
  exists as a Haskell derivative, with a full synthesizing compiler to RTL --
  while Chisel/Spinal/Migen/Hardcaml exists as an embedding of hardware semantics
  inside Scala/Scala/Python/OCaml. Aside from the "host language" differences,
  this means that Chisel/Spinal/Migen/Hardcaml are conceptually closer to
  something like *Lava/Blarney* than Clash. So within these languages you can
  only use the host language constructs to structure and compose the constructs
  of the EDSL, and you can't use host language constructs to describe the
  behavior of the circuit; i.e. you cannot use the host language's regular
  if-expression to model the concept of choice, but you have to use e.g. Chisel's
  *when*-function.

  Aside from the above, there is also a varying degree of *native* simulation
  and interactivity. In Clash you can evaluate/simulate any (sub-)component in
  the interactive interpreter for an immediate and localized design feedback loop.
  The only EDSLs that have a similar interactive interpreter for fast design
  feedback are the older variants of Lava. They used a so-called dual-embedding,
  where the EDSL primitives also contained a normal Haskell function which
  described their behavior, and so the composition of these primitives could be
  evaluated as a regular Haskell function.

  The other EDSLs all offer simulation, but there is a higher latency to get
  from a design to a simulation of a design, and they are not as interactive.
  Blarney emits Verilog, and you can then use a Verilog simulator to simulate
  the Blarney design. Spinal also emits Verilog, but it then uses Verilator to
  compile it to an object-file which is loaded back into Scala, allowing you
  to interact with your Spinal design from within Scala. Chisel is also not
  interpreted directly, instead, a Chisel description is "lowered" to FIRRTL
  where that FIRRTL description is then executed inside Scala by the FIRRTL
  interpreter. Migen works similarly to Chisel as far as the approach to
  simulation goes, although perhaps more direct: it directly interprets its own
  deep embedding data structure (its *IR*) to enable native simulation.

  All of this influences the style in which you write circuits and the creative
  process by which you come to a solution; the effects of this on the
  quality of results (QoR) and development time are, however, both hard to
  qualify and hard to quantify. That is, although all of these languages, both
  the EDSLs and Clash, enable full control over the QoR (i.e. you can get as
  many registers and as much logic as you intended), the way in which you get
  there can vary from problem domain to problem domain and person to person.
  If you have enough time, we encourage to try several of them and see which
  style is the most natural fit for you; if you're limited on time, we of course
  recommend that you just go with Clash ;-)
