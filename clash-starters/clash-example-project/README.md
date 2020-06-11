# Table of Contents
- [Table of Contents](#table-of-contents)
- [Downloading this example project](#downloading-this-example-project)
- [Using this project](#using-this-project)
  - [Stack (Windows, Linux, MacOS) [recommended]](#stack-windows-linux-macos-recommended)
  - [Snap (Linux)](#snap-linux)
  - [Cabal (Linux, MacOS)](#cabal-linux-macos)
  - [Nix (Linux, MacOS)](#nix-linux-macos)
  - [There's so many! Which should I pick?](#theres-so-many-which-should-i-pick)
- [IDE support](#ide-support)

# Downloading this example project
We publish an example project with every Clash release. You can find a list of releases at [github.com/clash-lang/clash-compiler/releases](https://github.com/clash-lang/clash-compiler/releases). Look for "starter project" under "Assets".

# Using this project
There's a number of ways to build this project on your machine. The recommended way of doing so is using _Stack_, whose instructions will follow directly after this section. Depending on your or your organization's needs, you might want to select another tool though. If you need help deciding, scroll down to [There's so many! Which should I pick?](#theres-so-many-which-should-i-pick).


## Stack (Windows, Linux, MacOS) [recommended]
Install Stack using your package manager or refer to the [How to install](https://docs.haskellstack.org/en/stable/README/#how-to-install) section of the [Stack manual](https://docs.haskellstack.org/en/stable/README/).

Build the project with:

```bash
stack build
```

To run the tests defined in `tests/`, use:

```bash
stack test
```

To compile the project to VHDL, run:

```bash
stack run clash -- Example.Project --vhdl
```


## Snap (Linux)
Linux users can use _snap_ to install Clash on their machines. See [snapcraft.io/clash](https://snapcraft.io/clash) how to do so. After installing Clash through snap, you can compile this project using:

```bash
clash.cabal update
clash.cabal build
```

You only have to run the update command once. After that, you can keep rebuilding your project by running the build command.

To run the tests defined in `tests/`, use:

```bash
clash.cabal run test-library
```

To compile the project to VHDL, run:

```bash
clash.cabal build --write-ghc-environment-files=always
clash Example.Project --vhdl
```

Clash will look for a function called `topEntity` in the module you specify and compile that to HDL.

<!-- omit in toc -->
### Notes on using snap for your project

1. By default, snaps will update to the latest stable release of Clash. This is a problem, because new (major) releases might break your design. To mitigate this, the next major Clash release (1.4) will introduce a [Snap channel](https://snapcraft.io/docs/channels) that will allow you to keep using 1.2.
2. The version of `clash-{prelude,lib,ghc}` your project depends on must exactly match the one supplied in the snap. For this reason, we recommend specifying a range of Clash versions your project can work with. This way, cabal will automatically prefer the version installed int he snap.

## Cabal (Linux, MacOS)
**The following instructions only work for Cabal >=3.0, GHC >=8.4, and Clash >=1.2.2.**

First, update your cabal package database:

```bash
cabal update
```

You only have to run the update command once. After that, you can keep rebuilding your project by running the build command:

```bash
cabal build
```

To run the tests defined in `tests/`, use:

```bash
cabal run test-library
```

To compile the project to VHDL, run:

```bash
cabal run clash -- Example.Project --vhdl
```


## Nix (Linux, MacOS)
To be done.

## There's so many! Which should I pick?
In general we recommend **Stack**. It offers a great balance between ease of use, flexibility, and reliability. On top of that, it's easy to install and use on Windows. Of course, it is not going to suit everyone. What follows is a comparison table between the different toolchains.

|                     | Snap | Cabal     | Stack    | Nix      |
|---------------------|------|-----------|----------|----------|
| Windows             |      | ¹         | ✓        |          |
| Linux               | ✓    | ✓         | ✓        | ✓        |
| macOS               |      | ✓         | ✓        | ✓        |
| Binary cache²       | ✓    |           |          | ✓        |
| Ease of use         | Easy | Moderate³ | Easy     | Hard⁴    |
| Flexibility⁵        | Low  | Moderate  | Moderate | High     |
| Snapshots⁵          |      | ⁶         | ✓        | ✓        |
| `ghcide` compatible |      | ✓         | ✓        | Probably |

Notes in table:

1. Although Cabal does run on Windows, as of the time of writing -May 2020- it doesn't offer an easy way to install itself.
2. Binary caches store project dependencies in their binary form on some centralized system. This helps to greatly reduce build times. See [the NixOS page on binary caches](https://nixos.wiki/wiki/Binary_Cache) for more information.
3. Ease of use is set to _moderate_ for now as it:
   * ..does not manage GHC installations. Users are responsible for installing the right `ghc` and passing it to cabal.
   * ..offers multiple ways of compiling a project ("old style" and "new style") which is reflected in the, scattered, cabal user documentation.
   * ..is hard to install for Windows users
4. Nix is notoriously hard to setup. However, many users claim that once it's setup it's a breeze to use. YMMV.
5. TODO
6. Cabal offers snapshots through "freeze files". Freeze files pin all (transitive) dependencies of a project to a specific version. In contrast, Stack and Nix offer snapshots of versions of Haskell packages known to work together. In other words, when using Cabal the burden of figuring out which dependency works with which is on the user.

# IDE support
We currently recommend Visual Studio Code in combination with `ghcide`. To use it, execute the following steps:

1. Install Visual Studio Code. If your Linux distribution supports `snap`, it's as simple as executing `sudo snap install code`. If you're running another platform, head over to [code.visualstudio.com](https://code.visualstudio.com/) for more information.
2. Launch Visual Studio Code, click on "Extensions" on the left, search for and install the extension `ghcide`.
3. Install `ghcide` using the instructions [over here](https://github.com/digital-asset/ghcide#using-it).
4. Open this folder in Visual Studio Code.
5. [Wait a few minutes](https://imgs.xkcd.com/comics/compiling.png)
6. Enjoy Clash!