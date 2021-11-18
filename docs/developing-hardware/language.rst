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

- `BinaryLiterals <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/binary_literals.html>`_
- `ConstraintKinds <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/constraint_kind.html>`_
- `DataKinds <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/data_kinds.html>`_
- `DeriveAnyClass <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/derive_any_class.html>`_
- `DeriveGeneric <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/generics.html#extension-DeriveGeneric>`_
- `DeriveLift <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/deriving_extra.html#extension-DeriveLift>`_
- `DerivingStrategies <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/deriving_strategies.html>`_
- `ExplicitForAll <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/explicit_forall.html>`_
- `ExplicitNamespaces <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/explicit_namespaces.html>`_
- `FlexibleContexts <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/flexible_contexts.html>`_
- `FlexibleInstances <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/instances.html#extension-FlexibleInstances>`_
- `KindSignatures <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/kind_signatures.html>`_
- `MagicHash <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/magic_hash.html>`_
- `MonoLocalBinds <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/let_generalisation.html?highlight=monolocalbinds#extension-MonoLocalBinds>`_
- `NoImplicitPrelude <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/rebindable_syntax.html>`_
- `NoStarIsType <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/poly_kinds.html?#the-kind-type>`_
- `NoStrictData <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/strict.html#strict-by-default-data-types>`_
- `NoStrict <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/strict.html#strict-by-default-pattern-bindings>`_
- `QuasiQuotes <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/template_haskell.html#template-haskell-quasi-quotation>`_
- `ScopedTypeVariables <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/scoped_type_variables.html>`_
- `TemplateHaskellQuotes <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/template_haskell.html#extension-TemplateHaskellQuotes>`_
- `TemplateHaskell <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/template_haskell.html>`_
- `TypeApplications <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/type_applications.html>`_
- `TypeFamilies <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/type_families.html>`_
- `TypeOperators <https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/type_operators.html>`_

.. warning::
  Since GHC 8.6, the ``StarIsType`` extension is defined. This extension is
  explicitly turned off by Clash, meaning ``Data.Kind.Type`` must be used to
  refer to Haskell types.

Clash also enables some GHC plugins by default which improve the type inference
for type level numbers. The plugins enabled by default are

- `ghc-typelits-extra <https://hackage.haskell.org/package/ghc-typelits-extra>`_
- `ghc-typelits-knownnat <https://hackage.haskell.org/package/ghc-typelits-knownnat>`_
- `ghc-typelits-natnormalise <https://hackage.haskell.org/package/ghc-typelits-natnormalise>`_

Users are free to control the language extensions and GHC options with the
normal ``OPTIONS_GHC`` and ``LANGUAGE`` pragmas in source files. For more
information, see the `GHC User's Guide <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/>`_.
