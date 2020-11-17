# Clash Example Project
[![CC0](https://licensebuttons.net/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)
To the extent possible under law, QBayLogic B.V. has waived all copyright and related or neighboring rights to this "Clash Example Project".

# Table of Contents
- [Clash Example Project](#clash-example-project)
- [Table of Contents](#table-of-contents)
- [Downloading this example project](#downloading-this-example-project)
- [Using this project](#using-this-project)
  - [Stack (Windows, Linux, MacOS) [recommended]](#stack-windows-linux-macos-recommended)
  - [Cabal (Linux, MacOS)](#cabal-linux-macos)
  - [Nix (Linux, MacOS)](#nix-linux-macos)
  - [There's so many! Which should I pick?](#theres-so-many-which-should-i-pick)
- [Change the LICENSE](#change-the-license)
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
cabal run clash --write-ghc-environment-files=always -- Example.Project --vhdl
```

## Nix (Linux, MacOS)
To be done.

## There's so many! Which should I pick?
In general we recommend **Stack**. It offers a great balance between ease of use, flexibility, and reliability. On top of that, it's easy to install and use on Windows. Of course, it is not going to suit everyone. What follows is a comparison table between the different toolchains.

|                     | Cabal     | Stack    | Nix      |
|---------------------|-----------|----------|----------|
| Windows             | ¹         | ✓        |          |
| Linux               | ✓         | ✓        | ✓        |
| macOS               | ✓         | ✓        | ✓        |
| Binary cache²       |           |          | ✓        |
| Ease of use         | Moderate³ | Easy     | Hard⁴    |
| Flexibility⁵        | Moderate  | Moderate | High     |
| Snapshots⁵          | ⁶         | ✓        | ✓        |

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

# Change the LICENSE
This Clash Example Project is dedicated to the public domain by its authors.
It is likely that you might _not_ want to do the same when using this Clash Example Project as a template for your projects.
So before publishing your work, make sure to change the copyright and license statement in the LICENSE file.

# IDE support
We currently recommend Visual Studio Code in combination with the _Haskell_ plugin. All you need to do is open this folder in VSCode; it will prompt you to install the plugin.
