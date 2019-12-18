.. _language:

Clash as a Language
===================

As Clash reuses parts of the GHC compiler for its front-end, the syntax and
semantics should be familiar to Haskell programmers. For people unfamiliar
with Haskell, there are many resources to learn the language, such as

- `Learn You a Haskell <http://learnyouahaskell.com/chapters>`_
- `Real World Haskell <http://book.realworldhaskell.org/read/>`_
- `The Haskell Wikibook <https://en.wikibooks.org/wiki/Haskell>`_

Clash does make some use of more advances features of GHC Haskell, which are
exposed by GHC as language extensions. The extensions used by Clash are

- ``BinaryLiterals``
- ``ConstraintKinds``
- ``DataKinds``
- ``DeriveAnyClass``
- ``DeriveGeneric``
- ``DeriveLift``
- ``DerivingStrategies``
- ``ExplicitForAll``
- ``ExplicitNamespaces``
- ``FlexibleContexts``
- ``FlexibleInstances``
- ``KindSignatures``
- ``MagicHash``
- ``MonoLocalBinds``
- ``NoImplicitPrelude``
- ``NoMonomorphismRestriction``
- ``NoStarIsType``
- ``NoStrictData``
- ``NoStrict``
- ``QuasiQuotes``
- ``ScopedTypeVariables``
- ``TemplateHaskellQuotes``
- ``TemplateHaskell``
- ``TypeApplications``
- ``TypeFamilies``
- ``TypeInType``
- ``TypeOperators``

Clash also enables some GHC plugins by default which improve the type inference
for type level numbers. The plugins enabled by default are

- ``ghc-typelits-extra``
- ``ghc-typelits-knownnat``
- ``ghc-typelits-natnormalise``

Users are free to control the language extensions and GHC options with the
normal ``OPTIONS_GHC`` and ``LANGUAGE`` pragmas in source files. For more
information, see the GHC User's Guide.

