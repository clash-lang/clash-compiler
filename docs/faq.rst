.. _faq:

====================================
Frequently Asked Questions & Answers
====================================

- **Q**: How do I install Clash?

  **A**: Check out the :ref:`Obtaining Clash <intro>` section of the manual, in
  the introduction, for information on how to reliably install the latest
  version of the Clash compiler.

----

- **Q**: Is the name "Clash", "CLaSH", or "CλaSH"?

  **A**: The original name "Clash" comes from the acronym "CLaSH" (prounounced:
  "clash"), the **C**\ AES **La**\ nguage for **S**\ ynchronous **H**\ ardware
  -- developed and maintained by the lead developer, Christiaan Baaij, at the
  University of Twente.

  The acronym "CLaSH" was originally used pervasively in the libraries and
  source code originally, but since the 1.0 release has moved more towards the
  simple name "Clash" for user-facing code and documentation. (It's much easier
  to type, at least!)

  The stylization "CλaSH" is an homage to the `Haskell programming language
  <https://haskell.org>`_, whose official logo has long been the venerable Greek
  *lambda* character, and graphically the name "CLaSH" has often been stylized
  this way.

----

- **Q**: Is Clash its own programming language? Or is it "Haskell"? It uses
  GHC?

  **A**: The answers to these questions are, in order:

    1) **Yes**: Clash is a *hardware design language* (HDL) for describing
       synchronous hardware designs. The Clash programming language is composed
       of two componets: **compiler** and a **standard library** (the "prelude")
       for circuit design.

    2) **Yes**: Clash is *also* Haskell. It would be more accurate to call it
       "*a* Haskell", in a sense, as a member of the "Haskell family of
       programming languages".

       Broadly speaking, Clash views its "source language", its high level
       input, in terms of Haskell: the Clash compiler takes Haskell input and
       converts it into a circuit. This uses the same syntax and type checking
       rules as any ordinary Haskell implementation.

       However, Clash programs have different *semantics*, ones that match the
       domain of the hardware world. The key, binding insight is that *HDL
       semantics, Haskell semantics, and the semantics of the functional
       language called Clash* are approximately the same thing. (A result that
       has been known in its broad forms since the 1970s.)

       So it makes sense to think of Clash as "a Haskell", although it has
       different semantics and a different compilation pipeline. But the end
       result is a language that *looks like Haskell, talks like Haskell, and
       quacks like Haskell* -- but is also suitable for large-scale hardware
       design.

    3) **Yes**: Clash uses the Glasgow Haskell Compiler, which offers an API, in
       order to transform the internal GHC view of Haskell programs, called
       **GHC Core**, into Clash's internal representation, **Clash Core**. By
       doing this, Clash uses GHC in order to do all of the hard work of
       compiling Haskell: analysis, type checking, feature detection, parsing,
       and more. Clash instead only deals with a simpler internal representation
       and compiles that, making it easier and more robust to develop.

       Utilizing the Glasgow Haskell Compiler also comes with other notable
       advantages: it allows the Clash frontend to transparently interoperate
       with GHC; nearly 100% of "modern" Haskell syntax and
       type-system features are available; a large amount of existing Haskell
       code can be shared and leveraged; ordinary tools and packaging systems
       are already available. GHC is also the secret behind *executable
       simulation* of Clash programs, by using GHC's powerful native-code
       compiler to emit executables to simulate a circuit.

----

- **Q**: Clash has more powerful type level arithmetic than GHC. Can I get GHC
  and ``ghci`` to have the same functionality?

  **A**: **Yes**! Clash's enhanced type checking functionality is due to
  extensible compiler plugins that can be used in *any* Haskell codebase, not
  just Clash code.

  To enable these plugins in your code, just put the following pragmas in your
  modules at the very top. Clash and GHCi should then type check your code in
  the same way (modulo any language extensions that may need to be enabled):

  .. code-block:: haskell

      {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
      {-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver    #-}
      {-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

  These plugins come from the ``ghc-typelits-natnormalise``,
  ``ghc-typelits-extra``, and ``ghc-typelits-knownnat`` plugins respectively,
  which are all available from Hackage and Stackage.

  .. todo:: Move this to :ref:`troubleshooting <troubleshooting>`?

----

- **Q**: Is Clash a "high level synthesis" tool?

  **A**: This is a bit complicated to answer depending on your view of things,
  but for the purposes of this question, **the answer is no**. In fact, Clash is
  actually fairly *low level* in comparison to most HDLs, in some respects.

  For the purposes of the argument, "high level synthesis" tools sit above what
  we would call *behavioral synthesis*, which, in turn, sits on top of
  *structural synthesis* (or *RTL synthesis*). "Behavioral synthesis" is a
  synthesis model in which EDA tools interpret *behavioral* specifications of a
  circuit, such as "Increment this number on every rising clock" and create
  digital hardware designs -- *structural* designs -- that implement that
  behavior exactly, using hardware resources available on a device. For example,
  "Use a D Flip-Flop with a clock enable line to increment this counter, with a
  feedback loop directing the DFF output back to its input".

  These days, most hardware designers operate at the behavioral level of design
  where possible, and let tools infer exact resource usage. This is easier to do
  for larger and more complex designs, and truly helps keep the designs
  understandable.

  Behavioral synthesis, then, takes a high level description, where resource
  usage is not explicit, and turns it into a structural one: where physical
  device resources are explicit. High level synthesis is then "one step above"
  this model.

  The traditional flow of a high level synthesis tool is to normally take a
  feature-reduced, cut-down variant of a language like C, and infer the
  behavioral model of the input C code. This behavioral model is then turned
  into Verilog or VHDL code, and passed onto the ordinary synthesis tools.

  .. note:: The wording is a bit confusing since "high level synthesis" is also
            *sometimes* referred to as "behavioral synthesis", but in
            traditional parlance, when people say "high level synthesis", they
            *almost always* mean things like C/C++ based HDL compilers. Some of
            this could have been avoided if we just called it "Behavioral
            synthesis" and "C-based synthesis" instead.


  Clash is not high level, in this sense: it does not take a "reduced feature
  set" language where the semantics do not match the domain. It supports nearly
  all of the ordinary Haskell programming language, and retains its semantics,
  yet at the same time these semantics also closely match those of structural
  hardware languages.

  Clash, then, in a sense is a *structural HDL*, not a *behavioral* one, and by
  this definition is *lower level than most alternative HDLs*. Clash is a
  language where clocking, routing, register usage, pipelining and any kind of
  IP interfacing is fairly explicit. (In fact, by abandoning all pretense of
  convenient abstraction -- you can write nearly direct, structural HDL with
  manual clock routing between components, with little overhead!)

  But this also doesn't mean Clash is *bad at abstraction*: indeed, thanks to
  its Haskell heritage, and powerful abstraction capabilities, the structural
  approach to circuit design in Clash requires *rethinking* how you approach
  "behavioral" design in the first place. Using powerful tools like mealy
  machines, combined with regular Haskell abstractions (such as the ``State``
  Monad), can approximate extremely high level behavioral descriptions, while
  retaining structural levels of control. Simple techniques -- like using higher
  order functions -- can abstract stateful and sequential circuit components
  from being tied to underlying structural representations, like BlockRAM vs
  distributed RAM, or particular vendor IPs.

  As a result, Clash is both simultaneously *lower level*, while offering a
  *higher abstraction ceiling*, than most competing HDLs -- that are either
  traditional *or* modern.

  .. note:: This is made more interesting by the fact that while Clash *itself*
            is structural rather than behavioral in its semantics, it emits HDL
            that, in many ways, is *behavioral!* For example, while you
            instantiate a ``register`` or ``blockRam`` manually in Clash, which
            feels structural -- the underlying HDL often relies on the
            synthesizer's behavioral inference to e.g. infer usage of a BlockRAM
            or D-Flip-Flop.

            This gives a nice spectrum of trade offs, where designs feel
            structural, with powerful levels of control and good abstractions --
            but actual RTL results are *behavioral*, and carefully generated to
            allow the synthesizer to correctly and efficiently utilize device
            resources, as needed. This is important e.g. for writing external IP
            in Clash, which will fit into some unknown design, and where the
            synthesizer can likely choose structural device logic better than
            you can. Clash users can even overload the generation of RTL
            primitives if they want to *really* emit structural code, if they're
            daring.

----

- **Q**: Does Clash work with my EDA tools?

  **A**: We hope so! In general, Clash should work very well for the "big two"
  FPGA vendors and all their EDA tools (Xilinx and Intel), -- this is what most
  of the testing and "real world" deployments use. But Clash has also been
  successfully used on Microsemi (formerly *Actel*) SmartFusion 2 FPGAs, as well
  as Lattice Semiconductor iCE40 FPGAs, and the developers maintain basic IP
  libraries for most of these toolchains. (These 4 vendors make up approximately
  95% of the market in its entirety.)

  In general, Clash should support your toolchain just fine, and the default
  ``clash-prelude`` primitives should work effortlessly, provided it supports
  certain aspects of behavioral inference (e.g. inferring BlockRAMs). If it
  doesn't, you can often call out manually to your vendor's technology library,
  or use a tool like `Yosys <http://clifford.at/yosys>`_ to do technology
  mapping for you.

----

- **Q**: Does Clash support `Project IceStorm <http://clifford.at/icestorm>`_?

  **A**: Yes! Clash's Verilog backend emits clean Verilog 2001, which is
  supported by Yosys -- and can thus be placed and packed with *arachne-pnr* and
  *icestorm*. So you can immediately start using it with one of the only truly
  open source FPGA Flows.

  Due to the low cost of iCE40 FPGAs and the freely available toolchain, we
  expect Clash to support Project IceStorm (via Verilog) for the foreseeable
  future.

----

- **Q**: What's the difference between Clash and "Lava"?

  **A**: TODO FIXME: history chalmers->york->xilinx->kansas, DSL vs synthesis
  differences

----

- **Q**: What's the difference between Clash and `Bluespec Verilog
  <http://bluespec.com>`_?

  **A**: TODO FIXME: guarded atomic actions, history, etc

----

- **Q**: What's the difference between Clash and Chisel/Spinal, or Hardcaml?

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

----

- **Q**: Can Clash be used for ASIC designs, as well as FPGA designs?

  **A**: Yes, *but maybe not out of the box*! The RTL produced by the Clash
  compiler is largely vendor agnostic. However, ASIC tool flows will vary from
  foundry to foundry and different fabrication processes -- certain tools may
  not support e.g. behavioral inference in the same way FPGA tools do.
  Furthermore, all of the flows for these processes are very proprietary -- and
  they require you pass through a dangerous gauntlet, complete with a dramatic
  ritual, in order to obtain them. The nature and location of these gauntlets
  and rituals are a closely guarded secret. This makes testing particular ASIC
  flows relatively difficult for the developers.

  As a concrete example of this, Clash supports BlockRAMs in FPGA designs -- and
  while most modern FPGA EDA tools will infer BlockRAMs from e.g. ``initial``
  blocks in Verilog, ASIC tools may not -- this means that certain modules like
  ``Clash.Prelude.BlockRam`` *may not* work out of the box with your ASIC flow,
  as they expect behavioral inference to work. Or it might work -- we can't
  easily know!

  For this particular case, either using an external RTL primitive, to call out
  to a technology library for your ASIC toolchain (TODO FIXME: link), or
  remapping the ``Clash.Prelude.BlockRam`` module to a new set of primitives,
  would fix this.

  If you're attempting to do an ASIC design with Clash and need help supporting
  your particular toolchain, or find bugs, please contact the Clash Developers
  (TODO FIXME: link).

----

- **Q**: Do I need to know Haskell in order to use Clash?

  **A**: This is a complicated subject, but it is **strongly advised** that you
  know a bit of Haskell before approaching Clash. This isn't *necessarily*
  mandatory. However, in practice, Clash is a complex tool that is deeply
  integrated into the Haskell ecosystem and toolchain, and uses advanced Haskell
  language features to perform some of its more unique tricks. Advanced designs
  tend to blend *both* hardware RTL and Haskell software libraries for powerful
  code sharing, build systems, etc.

  If you're scared, don't be! You can always get help -- but you should temper
  your expectations if you started writing Haskell yesterday. In general, if
  you're reasonably comfortable with Haskell build tools, and can work around
  and work with the type checker, you should be good to go, if you're
  persistent. While advanced designs and the Prelude tend to use some highly
  powerful features, users can often get away without worrying about them.

  However, on the flip side of this, Clash's unique semantics and design (such
  as lazily-modeled feedback loops, ``undefined`` values, etc) makes the
  semantic framework behind RTL and hardware design *rather approachable* to a
  seasoned Haskell programmer. Sequential vs combinational logic is easily
  identified by the type (stateful vs pure), behavioral logic is often easily
  obtainable with tools like ``State`` monads, and applicative ``Signal`` types
  closely resemble structural, FRP-inspired programming models. The semantics of
  lambda calculus is, in many ways, relatively close to RTL semantics.

  But while Clash and Haskell give a good *conceptual* model for hardware
  design, they cannot (alone) teach you about critical, *physical* techniques:
  such as timing constraints, power analysis, critical paths, verification,
  floorplanning, understanding physical flip-flop/LUT design, and more. These
  are just as important as the semantic RTL model itself, and will be the
  difference between your design working -- or not at all.

  The most difficult part won't be knowing Haskell -- it will be learning about
  Hardware!

----

- **Q**: Do I need to know existing RTL/HDL languages in order to use Clash?

  **A**: Lorem ipsum...

----

- **Q**: Is Clash production ready? Can I use it today?

  **A**: Yes! Several companies and individuals have been using Clash
  successfully on real world designs, ranging from 100s of LUTs to large 200k+
  LUT designs, on modern FPGA fabric (UltraScale and Arria 10).

  However, **Clash is constantly evolving, so be prepared for some bumps**! It's
  possible you'll run into compiler or language deficiencies, or you'll need
  help for your toolchain, just outright bugs, or a number of things. (While we
  think Clash is great, we also want to be honest!)

  If you're prepared to make the jump, just be sure to join the Clash community
  in case you need to ask for help -- or help someone out! (TODO FIXME: ref
  link)
