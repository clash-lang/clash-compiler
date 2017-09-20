.. _intro:

=====================
Introduction to Clash
=====================

Welcome to the manual for the Clash programming language! The following few
sections of this guide will catch you up on what you're reading and quickly
explain the Clash programming language. If you landed here by choice, you might
have an idea about some of this, but read on and find out!

What is Clash?
--------------

Clash is a functional *hardware description language* (or *HDL*) that closely
mirrors the syntax and semantics of the `Haskell programming language
<https://haskell.org>`_, and is used for creating hardware designs -- most
commonly, on modern *Field-Programmable Gate Arrays* (FPGAs), or
*Application-Specific Integrated Circuits* (ASICs).

Clash is both a compiler, and a set of libraries for circuit design, that
transform high level Haskell descriptions of synchronous, sequential logic into
low-level VHDL_, Verilog_, or SystemVerilog_. It provides a unique approach to
design of sequential circuits, but with a high amount of abstraction power that
blurs the line between strict *behavioral* and *structural* synthesis
approaches.

.. _VHDL: https://en.wikipedia.org/wiki/VHDL
.. _Verilog: https://en.wikipedia.org/wiki/Verilog
.. _SystemVerilog: https://en.wikipedia.org/wiki/SystemVerilog

Clash is:

- **Statically typed**: Clash uses Haskell type system in all its glory --
  including all its modern extensions and techniques -- to bring a high level of
  type safety and polymorphic programming to hardware design. But it still
  retains a high degree of type inference: so users can quickly and easily
  prototype their designs with little effort.

- **Interactive**: unlike traditional older HDLs, Clash comes with a fully
  interactive Read-Eval-Print-Loop (REPL) that you can use to interactively
  design, write, and test your circuits.

- **Fast**: Clash uses the `Glasgow Haskell Compiler <https://ghc.haskell.org>`_
  to provide fast executable simulations of hardware circuits.

- **Safe**: Clash uses the type system to enforce global safety invariants in
  your design. For example, the Clash standard library **separates each clock
  domain by type**, and requires type safe clock domain crossing via
  synchronizers.

- **Efficient**: The compiler uses a "whole program synthesis" approach in order
  to view the entire circuit description at once, and optimizes this design
  aggressively before translating to the chosen HDL. While the overhead isn't
  always zero, the compiler is being improved all the time, and careful
  design can often be used to minimize any overhead.

- **Maintainable**: Clash uses the type system to separate sequential from
  combinational logic. Values that are of type ``Signal`` exist in the world of
  sequential circuits, while pure functions are naturally combinational. The
  type system helps maintain invariants and keep code clean.

- **Natural**: Clash uses ``Signal`` values to represent sequential streams of
  values over time. Thanks to the lazy semantics that Clash shares with Haskell,
  lazy ``Signal`` values give rise to natural, concise feedback loops that you'd
  see in traditional HDLs between different components. This allows high-level
  structural design components to be connected quickly and easily. (The usage of
  ``Signal`` values will be familiar to those who have experimented with
  *Functional Reactive Programming*.)

- **Extensible**: If Clash doesn't offer what you need, there's an extensible
  template language for defining your own HDL primitives in your language of
  choice. Remap how Clash translates your code into low-level RTL, or add new
  primitives for your vendor or IP library.

- **Low level, but with powerful abstraction tools**: unlike most "high level
  synthesis" tools, Clash gives you precise control over register placement and
  pipelining in your design. There's no "inference" of resource usage: it's
  roughly what you'd get in (behavioral) Verilog. But it offers nearly all of
  the modularity features of modern Haskell: namespaced modules, pattern
  matching with sum types, records, real tuples, type-level arithmetic, advanced
  type system features like ``DataKinds`` and ``TypeApplications``, a package
  manager and set of community-driven tools, and more. This leads to a powerful
  spectrum of design approaches.

- **Modern Haskell**: "Hasochism in anger". Dependent type features, type level
  arithmetic and constraint solving, full pattern matching, ``ghc-mod`` and
  ``hindent``, Spacemacs or Neovim, ``cabal`` or ``stack`` -- the type classes
  and libraries you're familiar with. Nearly all modern GHC Haskell features and
  tools "Just Work". (Including some surprising ones, like: the ``lens``
  library, test frameworks like *HSpec*, *QuickCheck* or *Hedgehog* -- or
  old-school abstractions like the ordinary ``State`` monad!)

Can you just *show* me?
~~~~~~~~~~~~~~~~~~~~~~~

Sure! While we won't take the time here to explain all these in detail, people
always want to know -- and this should give you a feeling for what you're
getting into if you have HDL experience already.

We'll write 5 circuits: we'll show the definition up-front, play with it in
various ways, and finally give some finer points about the implementation --
highlighting Clash's features.

.. warning:: These examples assume *some* Haskell and FPGA/DSP experience that
             will not be elaborated upon. Check out the :ref:`Tutorial` for more!

Toggled Counter
^^^^^^^^^^^^^^^

.. todo:: FIXME

Multiply-And-Accumulate
^^^^^^^^^^^^^^^^^^^^^^^

Our next example is a basic **multiply-and-accumulate (MAC)** circuit, that
multiplies and adds its inputs over a series of clock cycles. We separate the
MAC design into its *combinational* (pure) and *sequential* (stateful)
components.

.. literalinclude:: ./examples/MAC.hs
   :language: haskell

There are some important things to note about this snippet:

- **Polymorphic definition, monomorphic circuit**: The type of the underlying
  transfer and logic functions are polymorphic. We could instantiate the
  resulting circuit to any synthesizable numeric type we desired -- in this
  case, a 9-bit signed integer.

  The overall circuit we intend to emit *must*, however, be monmorphic, with all
  of its type variables and higher order parameters made concrete.

- **Separation of concerns**: Management of state is separated from circuit
  logic, in the ``transfer`` and ``circuit`` functions, respectively. The
  ``mealy`` function ties a *state transformation* and a *pure function of that
  state* into a sequential, synchronous circuit that is synchronized to some
  clock.

- **Higher-order, stateful circuits**: The ``mealy`` function is a completely
  parametric, higher order *sequential circuit transformer* -- a classic "Mealy
  Machine" FSM -- that creates stateful logic out of pure, functional parts.
  There are also ``moore`` and ``medvedev`` machines for various other circuit
  patterns, and all of these are fully reusable.

- **Type safe clocking**: Clash requires that ``Signal`` types be annotated with
  the appropriate clock domains they correspond to. Two signals can't cross
  without an explicit synchronizer to ensure type safety. Circuits can also be
  fully polymorphic over the clock domain, as well.

- **Clocking and reset control**: The MAC example shows how synthesizable
  hardware must tie the circuit *clock line* and *reset line* -- effectively,
  the *clock domain* of an incoming signal -- into the resulting HDL. These
  lines are controlled by the ``Clock`` and ``Reset`` types, parameterized over
  the clock domain. In this case, ``withClockReset`` establishes that the
  incoming ``Clock`` and ``Reset`` lines are used for the mealy machine. (You
  could *alternatively* tie the clock lines onto an onboard PLL, or establish a
  mix of multiple clocks from oscillators and PLLs.

- **Top entities and code generation**: Clash allows you to control the input
  and output ports of circuits relatively faithfully, making integration
  simpler. This is controlled by using a *TopEntity annotation* on the desired
  top-level function, specified with the ``ANN`` pragma.

The above code can also be synthesized immediately: to compile to Verilog, just
run ``clash`` with ``--verilog``:

.. code-block:: none

    $ clash --verilog MAC.hs

The resulting circuit is located in the ``./verilog/MAC/mac/mac.v`` file,
relative to where you ran the compiler. The ``mac.v`` file contains a ``mac``
Verilog module, which can be instantiated with a clock signal, a reset signal,
two 9-bit signed input ports named ``input1`` and ``input2``, and a resulting
9-bit signed output port named ``output``. The clock and reset aren't tied to
anything, so they can come from anywhere else in your design (like a button pin,
or a PLL). Here's what the entity looks like:

.. code-block:: verilog

    /* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
    ** GENERATED BY CLASH 1.0.0. DO NOT MODIFY.
    */
    module mac
        ( // Inputs
          input clk // clock
        , input rst // asynchronous reset: active high
        , input signed [8:0] in1
        , input signed [8:0] in2

          // Outputs
        , output wire signed [8:0] out
        );

Feel free to hook this result up to any synthesis tool you desire. This is your
first Clash circuit!

Finite Impulse Response
^^^^^^^^^^^^^^^^^^^^^^^

Next, we have a **Finite Impulse Response (FIR) filter**, a basic building block
of signal processing. It computes the dot product of a vector of coefficient
signals, against a *sliding window* over the input signal:

.. literalinclude:: ./examples/FIR.hs
   :language: haskell

This code is not an error: load it into ``clashi`` and ask for the type of
``fir``!

.. code-block:: none

    $ clashi FIR.hs
    *Main> :t fir

For simplicity, the cleaned up type is as follows:

.. code-block:: haskell

      fir :: ( KnownNat n                    -- number of coefficients minus 1
             , Num a, Default a              -- numeric type with default value
             , ?rst :: Reset dom sync        -- implicit: reset line
             , ?clk :: Clock dom gated       -- implicit: clock line
             ) => Vec (n + 1) (Signal dom a) -- a vector of coefficient signals
               -> Signal dom a               -- input signal
               -> Signal dom a               -- output signal

There are also some important notes to take away from this example:

- **Parametric in the number of taps**: the FIR definition is invariant in the
  underlying number of coefficients or "tap count" chosen by the user -- *you*
  chose the number of the taps, at the call site.

- **Parametric and extensible in the underlying data type**: The FIR can be
  defined over any data type that satisfies the ``Num`` constraint and
  ``Default`` constraint -- including any synthesizable numeric type *you*
  wrote.

- **No type annotations, yet completely type safe**: Despite all of the above,
  the Clash compiler can infer the type of the ``fir`` function with no help at
  all! The size of the input coefficient vector determines the size of the
  sliding ``window`` automatically.

And remember (from the MAC example): you *can't* immediately synthesize the
above example, because it contains polymorphic type variables!

Bitonic Sorting
^^^^^^^^^^^^^^^

Another classic FPGA example is a **Bitonic sorter** -- a parallel `sorting
network <https://en.wikipedia.org/wiki/Sorting_network>`_ that uses
:math:`O(\log^2(n))` comparators, with a latency of :math:`O(\log^2(n))` as
well:

.. code-block:: haskell

    module Sort
      ( bmerge
      , bsort
      , bsorter
      ) where

    import Clash.Prelude

    cas :: Ord a => a -> a -> (a, a)
    cas x y | x > y     = (x, y)
            | otherwise = (y, x)

.. todo:: INCOMPLETE

This design is purely combinational, not sequential -- but it still shows off
some distinctive, advanced features:

- **Advanced type level arithmetic**: The Clash compiler augments the normal
  type checking capabiltiies of GHC with a more advanced solver for *type level
  integers*, including new operations, as well as *limited* automatic discharge
  of equality proofs and constraints.

  For example, in the above, Clash is able to see that the type of ``bmerge`` is
  a function that takes a HOF, from vectors of length ``n -> n``, as well as an
  input vector of **twice that size**. Types like ``2 * n`` naturally constrain
  the given type to double the size. In the event you passed a ``Vec`` with a
  type like ``Vec (n * n) a``, Clash would be able to prove they sizes are
  equivalent automatically. Similarly, a type like ``Vec (2 ^ n) a`` constrains
  a vector to a size which is a power of two.

- **Dependent, type-level generic programming**: The ``bsorter`` code above
  shows off the power of Clash and Haskell's type system to *automatically*
  derive type-safe code that is generic in the "width" of the sorting network,
  using a limited form of dependent types.

  The ``dfold`` function performs a **dependent vector fold** using a given base
  case, induction step, and input vector. The Clash compiler type checks this
  code, and the type of ``dfold`` ensures that folding always terminates over
  the input, and is "structurally inductive" over the input list. With this
  knowledge/proof of termination, the Clash compiler is free to "unroll" this
  definition for every given concrete set of type variables.

  All of this however is wrapped up in a completely generic, reusable type that
  is easy to understand:

  .. code-block:: haskell

      -- | Size-generic Bitonic Sorter. Input must be a power-of-two size.
      bsorter :: (Ord a, KnownNat k) => Vec (2^k) a -> Vec (2^k) a

  For example, if we chose the type ``k=4``, this would give a bitonic sorter
  for arbitrary 16-entry vectors. The use of ``bsorter`` in this case would be
  equivalent to writing out the following definition of ``bsort16``, manually:

  .. code-block:: haskell

     bsort16 :: Ord a => Vec 16 a -> Vec 16
     bsort16 = sort16
       where
         sort16  = bsort sort8 merge16
         merge16 = bmerge merge8

         sort8  = bsort sort4 merge8
         merge8 = bmerge merge4

         sort4  = bsort sort2 merge4
         merge4 = bmerge merge2

         sort2  = bsort id merge2
         merge2 = bmerge id

Extreme Overengineering
^^^^^^^^^^^^^^^^^^^^^^^

Finally, to show you the range of design approaches in Clash, let's revisit the
second example -- a simple MAC circuit -- and make it an extremely
over-engineered, unreadable mess by using a lot of fancy features!

.. warning:: This is a **dark, spooky piece of overdone code** and if you stare
             at it for too long, your family line will be cursed!

.. literalinclude:: ./examples/Overdrive.hs
   :language: haskell

Aside from turning 3 lines of easy-to-read-code into 200 lines of obscure
nightmare code (improving our job security) -- this example *also* shows off
some important features, most of them unique to Clash:

- **(Almost) No feature loss**: Unlike DSLs or complex staging approaches, Clash
  *is* Haskell, and is a pure compiler from (GHC) Haskell to HDL. It supports
  almost every Haskell feature and tool -- like ``GeneralizedNewtypeDeriving``
  and ``TemplateHaskell``.

- **Clash and Haskell interoperate**: the above example cleanly uses ``lens``
  and ``mtl`` from the installed package set to define the circuit. You can just
  use Haskell packages!

- **Not a DSL**: Clash isn't a DSL -- it really takes Haskell input, and
  compiles it to HDL, using a static analysis approach to compilation. There is
  no "object" and "meta" language distinction -- there is only *one* language,
  with the same set of abstractions you're used to. In the above example,
  packages like ``lens`` or ``mtl`` aren't just managed with the same tools --
  *they are analyzed and optimized away during the compile stage, just like
  always!*

- **Existing code works**: much existing Haskell code -- like the core
  foundations of ``lens`` and ``mtl`` -- transparently work with Clash with
  little effort. While the programming model is different, the high level of
  abstraction in most Haskell code means that *most transformations are
  independent of the underlying semantic data representation or 'shape' of
  code*. So most existing, foundational abstractions -- ``Monoid``,
  ``Applicative``, ``Bounded`` types and ``Enum`` types, or classic tools like
  ``Maybe`` and ``Either`` -- work effortlessly in a completely new domain.

- **Synthesis and simulation, in one language**: Much like Verilog or VHDL,
  Clash can be simulated in software, *and* synthesized to hardware -- this
  means you can write test benches and "surrounding tools" for your circuits
  trivially in Haskell. The Clash compiler won't attempt to synthesize parts of
  the code your circuits don't use -- so a single file can contain both hardware
  *and* software.

Why should I use it? Who should use it?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Clash seems neat, and more importantly, *it obviously looks really cool*. It
even *sounds super cool* when you tell people you designed a usable RISC-V
processor *in Haskell*. There are already 20 of those, but in Haskell? But still
-- is it really for *you*? It might be, if:

- **You're a hardware engineer looking for something new and exciting**:

  If this is you, you're an hardware engineer looking for something new and
  exciting, and you hopefully aren't freightened by very *different* ways of
  thinking about the world. For example, maybe you want to try a tool that
  *only* takes about 2GB of space to install as opposed to 40GB, or you would
  like a language that has things like "data types", but everyone you've ever
  worked with still hated it anyway.

  As far as traditional HDLs and HDL alternatives go, Clash is unique -- it
  retains the mixed simulation/synthesis approach of Verilog/VHDL, and has a
  real compiler, with a powerful language -- allowing powerful abstractions. In
  fact, you can nearly transliterate Verilog into Clash, with a little work. But
  it primarily revolves around the concepts of structural design with a
  functional flavor.

  For experienced hardware engineers, knowledge of "design physicality", e.g.
  the physical consequences of FPGA and hardware design (timing, metastability,
  clocking, paths, etc) will already be at hand. As Clash is fairly low level
  and structural, most of the effort will go to learning effective Haskell
  programming.

  The most important takeaway is that, for an experienced hardware engineer,
  **learning to think like a Haskell programmer will be difficult**. But by
  using Clash, you should hopefully end up writing **circuits that are safer,
  efficient, more abstract, and more declarative** than you otherwise could.

- **You're a Haskell programmer, looking to get into hardware design**:

  If this is you, you woke up this morning and thought "I'd love to learn about
  hardware design". If you did, that's fantastic! You can learn *a lot* about
  software programming and software design when you learn about hardware. Or
  maybe you want to learn about the dark, scary world of extremely expensive,
  proprietary EDA software -- it all makes you cringe as a software programmer.
  Fantastic!

  You're also in luck: Clash is effectively "real Haskell" with all the bells
  and whistles you're used to. It acts like an ordinary Haskell compiler and
  Haskell toolchain for 99% of all features, in fact. It also works just fine
  with scary EDA tools. But more importantly, **the semantics of Haskell/Clash
  are close to the semantics of hardware**!

  If you've got some experience thinking about lazy functional programming --
  the idea of sequential circuit design should come relatively naturally. For
  familiar Haskell programmers, the ``Signal`` type in Clash feels reminiscent
  of a *Functional Reactive Programming* API. This lets you focus on the most
  important ideas you must master: physicality of the design, i.e. the physical
  characteristics and consequences of FPGA and hardware design.

  Using Clash will help free you from some of the irrelevant details like
  traditional, annoying HDL semantics (which only occasionally coincide with
  hardware), and unfamiliar toolchains **so you can focus on the truly important
  lessons of hardware design and implementation, not the annoying details**.

- **You're new to Haskell, new to hardware design, and looking to learn**:

  This might be tough, but we believe in you! If you join the community we'll
  help you out. And you can help us: suggest stuff that we can improve or
  editorialize - here, elsewhere, everywhere.

- **You're a Haskell programmer and a hardware engineer looking for a new tool**:

  Hello? Who are you? Can you help us work on Clash?

How mature is it?
~~~~~~~~~~~~~~~~~

Clash has had raving reviews from its users, including *"works"* and *"better
than Verilog"* -- but it's still evolving relatively frequently, despite being
several years old. We should say it now: circuits you write now will likely be
broken by future versions of the Clash compiler in big ways! You can always
stick to older compilers, but the language and toolchain is still evolving (TODO
FIXME: pvp ref?)

Furthermore, Clash still undeniably has rough edges (see :ref:`the
troubleshooting page <troubleshooting>`) and some bugs (see :ref:`the known bugs
and limitations <relnotes>`). We hope to polish these off in the future as time
allows -- we understand "prickly little issues" are big annoyances for people.

Hopefully that doesn't dissuade you: several companies and individuals have been
using Clash successfully for "real world" work, ranging from tiny 1k-LUT designs
on $20 FPGAs, to extremely modern FPGA fabrics and large-scale designs, to
ASICs, and more. (And some of these projects wouldn't have been nearly and easy
as they were, without Clash!)

.. note:: Some companies that **use Clash** include the following:

          - `QBayLogic <https://qbaylogic.com>`_
          - `Myrtle Software <https://myrtlesoftware.com>`_

          Some companies that **do not use Clash** include the following:

          - Enron Corporation
          - Red Bull GmbH

If you're already a functional programmer who's looking to learn hardware, we
think Clash fits in a unique space among the other functional HDLs available
(low level, synthesis-driven, with unique abstractions), and should be
considered stable for most use cases. If you're willing to give it a shot, you
might like it!

(And for a *Haskell programmer* who wants to develop hardware, *we* think Clash
is the most effective HDL choice there is, at least!)

What is Clash not good for?
~~~~~~~~~~~~~~~~~~~~~~~~~~~

TODO FIXME: mention clockless logic (e.g. greenarrays chips)? HLS advantages?

Obtaining Clash
---------------

.. note:: You **must** use ``cabal-install`` to install stable versions of Clash
          at this time. Stack LTS/nightly snapshots are not yet available.

The recommended way for users to currently install Clash is using
``cabal-install`` **2.0 or later with GHC 8.2.1**

The basic installation only requires ``cabal-install`` and GHC, and installation
of Clash from Hackage. After you're finished, you'll have ``clash`` and
``clashi`` executables available in your cabal binary directory.

Ubuntu
~~~~~~

Ubuntu is one of the easiest platforms to get started with. We'll grab GHC 8.2.1
and Cabal from the popular `hvr ppa
<https://launchpad.net/~hvr/+archive/ubuntu/ghc>`_ archives.

.. code-block:: none

    $ sudo apt-add-repository -y ppa:hvr/ghc
    $ sudo apt update
    $ sudo apt install -y ghc-8.2.1 cabal-install-2.0

GHC and Cabal have been installed under ``/opt/ghc`` and ``/opt/cabal``,
respectively. Next, add these to your ``$PATH`` -- here it will be temporary,
but you could also add this to your ``.profile`` or ``.bashrc`` to make the
change permanent:

.. code-block:: none

    $ export PATH=/opt/ghc/8.2.1/bin:/opt/cabal/2.0/bin:$PATH

Finally, you're ready to install Clash. This step might take a while, so be
ready to grab some coffee or tea:

.. code-block:: none

    $ cabal update
    $ cabal install clash-ghc

The resulting ``clash`` and ``clashi`` binaries are now installed (by default)
under ``$HOME/.cabal/bin``. Add these to your ``$PATH`` just like before, and
you're ready to go:

.. code-block:: none

    $ export PATH=$HOME/.cabal/bin:$PATH
    $ clashi
    CLaSHi, version 1.0.0 (using clash-lib, version 1.0.0):
    http://www.clash-lang.org/   :? for help
    Clash.Prelude>

macOS
~~~~~

.. todo:: FIXME

Windows
~~~~~~~

.. todo:: FIXME

NixOS and Nix
~~~~~~~~~~~~~

NixOS/Nix users can obtain Clash from the latest version of the *nixpkgs*
package set. Clash expressions for Hackage packages should already be available
upstream. The three packages you need are:

- ``haskellPackages.clash-prelude``
- ``haskellPackages.clash-lib``
- ``haskellPackages.clash-ghc``

Note that because Clash only works with GHC version 8.2.1, you'll probably need
to use the ``ghcWithPackages`` attribute of the ``ghc821`` compiler set, in
order to get the right packages. Otherwise, you may install older versions of
Clash!

Docker
~~~~~~

.. todo:: FIXME, the docker builds were wiped. We might be able to use Nix to
          publish images continuously...

Other
~~~~~

In general, the instructions for Ubuntu or any of the above platforms apply: the
most important thing is **you must have GHC 8.2.1 exactly, and cabal-install 2.0
at minimum**!

Once you have ``cabal`` and ``ghc`` ready, just install ``clash-ghc`` from Hackage, and the binaries will be inside ``$HOME/.cabal/bin``:

.. code-block:: none

    $ cabal update
    $ cabal install clash-ghc
    $ export PATH=$HOME/.cabal/bin:$PATH
    $ clashi
    CLaSHi, version 1.0.0 (using clash-lib, version 1.0.0):
    http://www.clash-lang.org/   :? for help
    Clash.Prelude>

Meta-information: web sites, mailing lists, etc.
------------------------------------------------

On the World-Wide Web, there are several places of interest for Clash hackers:

- `Clash home page <http://clash-lang.org>`_
- `Clash GitHub organization <https://github.com/clash-lang>`_

There is also a mailing list for Clash:

``clash-language``
  This is a list for Clash users to chat among themselves, and for development
  announcements. If you have a specific question about Clash, please see
  :ref:`the FAQ <faq>`, :ref:`the Troubleshooting guide <troubleshooting>`, and
  :ref:`the known bugs and limitations <relnotes>` first.

  The list is hosted by Google Groups at the following location:
  http://groups.google.com/group/clash-language

There are also several IRC channels that may be of interest to you:

``#clash-lang`` on Freenode
  This channel is for Clash users, and (mostly) for Clash development
  discussion.

``#haskell-embedded`` on Freenode
  This is a general channel for using Haskell/GHC on embedded devices. While not
  Clash-specific, many adjacent discussions and similar developers may be here.

Reporting bugs in Clash
-----------------------

Clash is constantly evolving (often in big ways, still) so reporting bugs is
vital, and we really appreciate you doing it! Clash uses `GitHub
<https://github.com>`_ to host its code and issue tracking, so if you find a
bug, please report it to the `clash-lang organization
<https://github.com/clash-lang>`_.

- If the bug/feature request affects the Prelude, please file the bug in the
  ``clash-prelude`` repository.

- If the bug is otherwise a deficiency in the compiler, please file it in the
  ``clash-compiler`` repository.

Clash version numbering policy
------------------------------

Clash follows the `Haskell PVP Specification <https://pvp.haskell.org>`_ for its
version numbers, for all packages. (The Haskell PVP is something of an
equivalent to *Semver* in other communities.)

Compiler and prelude libraries maintain the same major and supermajor number,
although they may have disjoint bugfix releases (with appropriate constraints
applied, depending on the nature of the fix).

 .. note:: As a slight exception to this rule, due to the nature of Clash's
           tight integration with GHC, **updates to the GHC version that Clash
           uses, even minor ones, often result in major upgrades to the Clash
           version**. As GHC's internals change frequently, even for minor
           bumps, it cannot be guaranteed that these changes will not result in
           Clash changes (for example, in ``clash-ghc``.)

           For example, Clash 1.0.0 might be compatible with GHC 8.2.1. But if
           Clash was then updated to use GHC 8.2.2, the next release would
           likely be Clash 1.1.0 -- even if there were no intervening changes
           otherwise.

IP/Vendor package, such as *clash-xilinx*, also follow the PVP specification,
but do not use the same supermajor/major number as Clash itself: they are
otherwise external packages, simply maintained by the Clash Developers.

It's recommended (but not required) that downstream Clash packages, and
published Clash code, follow the PVP specification.
