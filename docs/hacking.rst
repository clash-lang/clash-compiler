.. _hacking:

================
Hacking on Clash
================

If you're reading this, you're a lucky hacker who's looking to work on Clash.
Thanks for your effort!

Clash is a large Haskell project and may not always be the easiest to grapple
with -- hopefully this guide will get you up to speed quickly. If it doesn't, be
sure to file a bug or ask, so we can help!

Requirements
------------

By itself, Clash has fairly modest requirements, but for hacking on Clash,
you'll need a few things:

- You need **GHC 8.2.1** no matter what, as of this writing.
- You'll need a build tool: either

  - ``cabal-install`` (version 2.0, or preferably a recent ``git`` copy)
  - ``stack`` (any modern version)

- You'll need some simulation tools too, in order to run the tests and ensure
  you haven't broken code generation:

  - Icarus Verilog 10.x, for Verilog Simulation.
  - GHDL, for VHDL simulation.

- **Optionally**, if you have ModelSim available, you can also use it for
  SystemVerilog simulation.

Cloning the code
----------------

Clash is primarily structured in two main repositories:

1. ``clash-compiler.git``, containing the compiler code. This repository
   contains multiple packages, but the most important ones are:

   - ``clash-lib``, which exposes Clash Core and the compiler backends as a
     library.

   - ``clash-ghc``, which uses ``clash-lib`` to provide a GHC-compatible
     ``clash`` driver. This package provides the ``clash`` and ``clashi``
     executables.

   - ``clash-testsuite``, which contains the clash testsuite.

2. ``clash-prelude.git``, containing the ``Clash.Prelude`` code that
   user-defined circuits use. ``clash-prelude`` is a submodule of
   ``clash-compiler``.

Grab the code using ``git``. **Make sure you pass** ``--recursive`` **so you
grab the** ``clash-prelude`` **submodule**!

.. code-block:: sh

    $ git clone --recursive https://github.com/clash-lang/clash-compiler.git
    $ cd clash-compiler

You can use either ``cabal`` or ``stack`` in order to build clash. Either should
work fine, but **you must use GHC 8.2.1 for the build to work**!

Furthermore, if you choose to use ``cabal new-build``, a feature deficiency
currently requires another step as a workaround. In order to install proper,
local versions of the up to date ``ghc-plugins-*`` packages used by Clash,
you'll need to clone them **as a subdirectory of the** ``clash-compiler``
**repository**! Stack will automatically check out proper revisions, and in the
future, ``cabal new-build`` will be able to do so as well.

.. code-block:: sh

    $ git clone https://github.com/clash-lang/ghc-typelits-extra
    $ git clone https://github.com/clash-lang/ghc-typelits-knownnat
    $ git clone https://github.com/clash-lang/ghc-typelits-natnormalise
    $ git clone https://github.com/clash-lang/ghc-tcplugins-extra

Building the code
-----------------

Once you've cloned the repository, and got a proper GHC installation (or an
automated one, if you're using ``stack``), building the ``clash`` executables is
easy using either ``stack`` or ``cabal``.

.. warning:: While the official "user recommended" installation procedure uses
             the legacy ``cabal install`` scheme, if you are going to hack on
             Clash *itself*, we **strongly** recommend using ``stack`` or
             ``cabal new-build`` instead of the legacy ``cabal install``
             commands. Currently, there is no ``cabal new-install``, and there
             is also currently no Clash package available in any Stack snapshots
             or LTS releases, either. In the future, we'll recommend ``cabal
             new-install`` for users to get bleeding-edge releases, as well as
             up-to-date Stackage LTS specs, so developers and users are always
             using the same build tools.

.. warning:: ``stack`` version 1.5.x is rather unstable when building Clash 1.x
             with GHC 8.2.1. If you see intermittent and non-deterministic
             "package registration" related failures while attempting to build,
             simply re-running the commands will (normally) result in a fix.
             Alternatively, you may use ``cabal new-build``. **TODO FIXME**:
             report this bug upstream after diagnosing further.

.. note:: Cabal 2.0 is required for GHC 8.2.1, but it is **strongly** advised to
          build a recent copy of ``cabal-install`` from the `git repository
          <https://github.com/haskell/cabal>`_ in order to get all the latest
          bugfixes and features, for now.

.. note:: ``cabal new-build`` uses a locked set of Haskell dependencies from
          Hackage inside ``cabal.project.freeze``, as well as a locked index
          file version inside ``cabal.project`` (which control ``.cabal``
          "revisions" on Hackage). This ensures that every developer has a
          reproducible experience, and not even dependency revisions can break
          this, similar to Stack snapshots.

- Using ``stack`` to launch a copy of ``clashi``:

  .. code-block:: sh

      $ stack build clash-ghc
      $ stack exec clash-ghc -- clashi

- Using ``cabal new-build`` to launch a copy of ``clashi``:

  .. code-block:: sh

      $ cabal new-build clash-ghc
      $ cabal new-run -- clashi --help

  OR, in a single command:

  .. code-block:: sh

      $ cabal new-run clash-ghc:clashi -- --help

Running the test suite
----------------------

.. todo:: Lorem ipsum...

Convenient hacking tips
-----------------------

The following are some convenient tips for when you're hacking on the compiler
and library source code for Clash.

Debugging the compiler
~~~~~~~~~~~~~~~~~~~~~~

The `clash` and `clashi` executables provide a `-fclash-debug=<level>` flag which allows dumping of the various intermediate representations used by the compiler. This can be used in conjunction with GHC's usual `-ddump-simpl` flag to see how the compiler is transforming your program. 

`-fclash-debug` accepts several debug levels:

 * `DebugNone` disables debug output
 * `DebugFinal` shows the final, completely normalized expression
 * `DebugName` shows the names of transformations as they are performed
 * `DebugApplied` shows the result of each sub-expression rewrite
 * `DebugAll` enables all of the above.

Quick one-shot iteration
~~~~~~~~~~~~~~~~~~~~~~~~

.. todo:: Lorem ipsum...

GHC 8.2 environment files
~~~~~~~~~~~~~~~~~~~~~~~~~

.. note:: This **ONLY** works if you are developing with ``cabal new-build`` as
          of right now.

.. warning:: You **MUST** also have a version of Cabal 2.1, or later, from the
             ``git`` repository.

When using ``cabal new-build`` 2.1 or later, ``cabal`` writes out *package
environment files* into the root directory of the ``clash-compiler`` repository.

After you have successfully run ``new-build`` once, it will write out a file
into the root directory named ``.ghc.environment-<platform>``. This file is
automatically read by the ``ghci`` command (or ``clashi`` command!) in order to
load dependent packages of a project into scope.

At this point, simply executing ``ghci`` anywhere puts all dependent packages,
*and local packages*, into scope at the REPL. This makes it easy and convenient
to do things like run Clash directly from GHCi itself, using the ``clash-ghc``
library API:

.. code-block:: none

    $ cabal new-build clash-ghc
    ...
    $ ghci
    GHCi, version 8.2.1: http://www.haskell.org/ghc/  :? for help
    Prelude> import CLaSH.Main
    Prelude CLaSH.Main> defaultMain []
    <interactive>: no input files
    Usage: For basic information, try the `--help' option.
    *** Exception: ExitFailure 1

This REPL works just like any ordinary ``ghci`` repl, or just like ``cabal
new-repl``, so you're free to develop incrementally at this point.

Updating Cabal freeze files
~~~~~~~~~~~~~~~~~~~~~~~~~~~

``cabal new-build`` features a much power powerful version of the "freeze"
functionality available from the previous "Cabal Sandbox" features. This feature
allows the Clash developers to exactly control the given build dependencies
during development, when using ``cabal``.

Freezing is controled by two components:

- The **frozen dependency specification**, located in the
  ``cabal.project.freeze`` file at the root of the project. This exactly
  specifies which packages, their versions, and the build flags are needed for
  successful compilation.

- The **Hackage index state**, specified in the ``cabal.project`` file at the
  root of the directory. This specifies the exact "index state" of the Hackage
  package index at a given point in time. While a varying index state does not
  change the *versions* of needed, dependent packages, it may change their
  *constraints and options*, due to "metadata revisions" of the upstream
  ``.cabal`` files. (Revisions may occur in a package to try and help constrain
  invalid solver plans from being formed by ``cabal install``, and also keep
  constraint bounds between packages consistent.)

A lock-step upgrade and pinning, of both the index state, and dependency
specification, yield deterministic builds and upgrades.

Updating the index state
^^^^^^^^^^^^^^^^^^^^^^^^

.. warning:: Updating the index state is currently a **destructive operation**
             that will update the package index in your ``$HOME`` directory! In
             the future, ``cabal`` will provide a way to query the current local
             and upstream index states.

The easiest way to update the index state is simply to run ``cabal update``
*twice*. The first time will bring you up to date, if necessary. The second one
will be a no-op. When ``cabal update`` is run, it tells you what the *prior*
index state was. Therefore, running it twice tells you the "current state"
assuming the second operation was a no-op, like follows:

.. code-block:: none

    $ cabal update && echo && cabal update
    Downloading the latest package list from hackage.haskell.org
    To revert to previous state run:
        cabal update --index-state='2017-09-20T19:05:42Z'

    Downloading the latest package list from hackage.haskell.org
    To revert to previous state run:
        cabal update --index-state='2017-09-21T14:38:54Z'

In the above example, the new index state is **2017-09-21T14:38:54Z**. You can
write this value into the ``index-state`` field of the ``cabal.project`` file,
in the ``clash-compiler.git`` repository.

Updating the freeze file
^^^^^^^^^^^^^^^^^^^^^^^^

.. todo:: Lorem ipsum...

=======================================
Addendum: Hacking on this documentation
=======================================

Check out `the clash-docs README`_ for information on hacking on the documentation
that's in front of your eyes, right now!

.. _the clash-docs README: https://github.com/clash-lang/clash-docs
