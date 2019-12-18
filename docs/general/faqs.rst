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

  **A**: The original name "Clash" comes from the acronym "CLaSH" (prounounced:
  "clash"), the **C**\ AES **La**\ nguage for **S**\ ynchronous **H**\ ardware
  -- developed and maintained by the lead developer, Christiaan Baaij, at the
  University of Twente.

  The acronym "CLaSH" was originally used in the libraries and source code, but
  since the 1.0 release has moved more towards the simple name "Clash" for
  user-facing code and documentation. See the :ref:`relnotes` for more
  information.

  The stylization "CλaSH" is an homage to Haskell_, whose official logo has
  long been the venerable Greek *lambda* character, and graphically the name
  "CLaSH" has often been stylized this way.

.. _Haskell: https://haskell.org

----

- **Q**: Is Clash a "high level synthesis" tool?

  **A**: While clash provides a high level language, it is arguably quite low
  level in comparison with other HDL tools. Programs written in Clash
  correspond closely to a structural view of circuits (as opposed to
  behavioural or higher level views). Consequently, elements of circuits like
  clocks, routing and register usage are expressed fairly explicitly.

  In contrast, many alternative HDL tools offer a behavioural view of hardware,
  which is then used to infer a structural design of a circuit. This layer of
  indirection allows these tools to have a greater deal of abstraction --
  something that Clash can provide by virtue of being a functional language.
  This arguably makes Clash a lower level HDL with a higher level of
  abstraction.

Clash Support
-------------

- **Q**: Is Clash production ready?

  **A**: Clash is constantly evolving, although since the 1.0 release there is
  a focus on maintaining API backwards compatibility. Clash is already used
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

  Do to the shared behaviour in the early stages of the compiler, components
  from GHC (the most common Haskell compiler) are reused in the Clash compiler.
  This is how Clash achieves such high interoperability with existing Haskell.

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

  .. todo:: Answer

----

- **Q**: What's the difference between Clash and "Lava"?

  .. todo:: Answer

----

- **Q**: What's the difference between Clash and `Bluespec Verilog
  <http://bluespec.com>`_?

  .. todo:: Answer

----

- **Q**: What's the difference between Clash and Chisel/Spinal, or Hardcaml?

  .. todo:: Update

  **A**: The most obvious difference between these two toolchains is that Clash
  exists as a Haskell derivative, with a full synthesizing compiler to RTL --
  while Chisel exists as an embedding of hardware semantics inside Scala. The
  Chisel compiler does not synthesize RTL from Scala -- it synthesizes RTL from
  an embedded DSL, constructed by a Scala program at runtime.

  Aside from the "host language" differences, this means that Chisel is
  conceptually closer to something like *Kansas Lava* than Clash -- and this
  difference manifests in most of the same ways (other design points, aside).

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

