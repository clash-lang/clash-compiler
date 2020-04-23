.. _installing:

Installing Clash
================


Installing via Snap
-------------------

Clash is released as a binary package on snapcraft. Snap is supported on
all major Linux distributions. Visit `Clash’s snapcraft
page <https://snapcraft.io/clash>`__, scroll down, and choose your
distribution for installation instructions. To install the latest stable
version, use:

.. code:: bash

   snap install clash

To install the latest development version of Clash, run:

.. code:: bash

   snap install clash --edge

This version is updated every 24 hours.

Installing via Source (Linux / MacOS)
-------------------------------------

Install the `latest nix <https://nixos.org/nix/download.html>`__ and
run:

.. code:: bash

   curl -s -L https://github.com/clash-lang/clash-compiler/archive/1.2.tar.gz | tar xz
   nix-shell clash-compiler-1.2/shell.nix

See the `releases
page <https://github.com/clash-lang/clash-compiler/releases>`__ for all
available versions of the compiler.

Installing via Source (Windows)
-------------------------------

1. Install `Stack <https://get.haskellstack.org/stable/windows-x86_64-installer.exe>`__
2. Download the source code of `Clash
   1.2 <https://github.com/clash-lang/clash-compiler/archive/1.2.tar.gz>`__
3. Unpack the archive
4. Use cd to navigate to the unpacked directory
5. Run ``stack build clash-ghc``. **This will take a while.**

See the `releases
page <https://github.com/clash-lang/clash-compiler/releases>`__ for all
available versions of the compiler. To run clashi, execute:

.. code:: bash

   stack run clashi
   
To compile a file (to VHDL) with Clash, run:


.. code:: bash

   stack run clash -- path/to/your/file.hs --vhdl

Installing via Source (HEAD)
----------------------------

Clone Clash from github using ``git`` and enter the cloned directory:

.. code:: bash

   git clone https://github.com/clash-lang/clash-compiler.git
   cd clash-compiler

Use one of the build tools below to get Clash up and running.

Cabal
~~~~~

Install Cabal >= 2.4 and GHC >= 8.4. Even though GHC 8.6 is supported,
we currently recommend running 8.4 as the former contains some known
bugs concerning documentation generation. If you’re using Ubuntu, add
`HVR’s PPA <https://launchpad.net/~hvr/+archive/ubuntu/ghc>`__ and
install them using APT:

.. code:: bash

   sudo add-apt-repository -u ppa:hvr/ghc
   sudo apt install ghc-8.4.4 cabal-install-2.4

Add ``/opt/ghc/bin`` `to your
PATH <https://askubuntu.com/questions/60218/how-to-add-a-directory-to-the-path>`__.
Finally, run Clash using ``cabal``:

.. code:: bash

   cabal new-run --write-ghc-environment-files=always -- clash

Stack
~~~~~

You can use
`Stack <https://docs.haskellstack.org/en/stable/install_and_upgrade/>`__
to build and run Clash too:

.. code:: bash

   stack run -- clash

Nix
~~~

Or `use Nix <https://nixos.org/nix/download.html>`__ to get a shell with
the ``clash`` and ``clashi`` binaries on your PATH:

.. code:: bash

   nix-shell

