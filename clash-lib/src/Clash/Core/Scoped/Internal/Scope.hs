{-|
  Copyright   :  (C) 2023, Nikolai Kudasov
                 (C) 2026, QBayLogic B.V.
  License     :  BSD3 (see below)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Scope-indexed names, binders and scopes: the machinery of the foil
  (Maclaurin, Radul, Paszke, "The Foil: Capture-Avoiding Substitution With No
  Sharp Edges", IFL 2022, <https://doi.org/10.1145/3587216.3587224>).

  The definitions are vendored from @Control.Monad.Foil.Internal@ of the
  @free-foil@ package, version 0.4.0, which is licensed as follows:

  > BSD 3-Clause License
  >
  > Copyright (c) 2023, Nikolai Kudasov
  >
  > Redistribution and use in source and binary forms, with or without
  > modification, are permitted provided that the following conditions are met:
  >
  > 1. Redistributions of source code must retain the above copyright notice, this
  >    list of conditions and the following disclaimer.
  >
  > 2. Redistributions in binary form must reproduce the above copyright notice,
  >    this list of conditions and the following disclaimer in the documentation
  >    and/or other materials provided with the distribution.
  >
  > 3. Neither the name of the copyright holder nor the names of its
  >    contributors may be used to endorse or promote products derived from
  >    this software without specific prior written permission.
  >
  > THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  > AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  > IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  > DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
  > FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  > DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  > SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  > CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  > OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  > OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  = Changes with respect to @free-foil@

  * A 'Scope' is a /watermark/, an upper bound of the names in scope, instead
    of the set of names in scope. See 'Scope'. 'withFresh' and 'withRefreshed'
    are adapted accordingly, and 'withFreshRaw' and 'withFreshRaws' are new.
    @member@ is left out: a watermark can only tell whether a name may be in
    scope.
  * 'sinkClosed' and 'withExtTrans' are new.
  * Every scope-indexed type has a @nominal@ role annotation, so that
    'Data.Coerce.coerce' can't change the scope of a value.
  * Only the parts used by "Clash.Core.Scoped" are included: there are no
    patterns (@CoSinkable@), substitutions, name maps or generic
    sinkability proofs.

  = Trusted code

  This module is part of the trusted core of "Clash.Core.Scoped": the scope
  safety of everything else relies on it. Users must not import it, nor define
  instances of t'Distinct', 'ExtEndo' or t'Ext'. Its unsafe operations, the
  invariants they rely on, and why these hold:

  * __The 'Scope' invariant__: every name in scope @n@ is at most the
    watermark of a @'Scope' n@. 'emptyScope' has no names. 'extendScope' takes
    the maximum of the watermark and the new name. 'withFresh',
    'withRefreshed', 'withFreshRaw' and 'withFreshRaws' only extend a scope
    with names above its watermark, and raise it to them. 'raiseWatermark'
    only raises the watermark of a scope, without changing its names. A
    'Scope' can't be sunk ('sink' needs a 'Sinkable' instance, and there is
    none for 'Scope'), so no other values of type @'Scope' n@ exist.

  * __'withFresh', 'withRefreshed'__ ('unsafeAssertFresh'): a name above the
    watermark of the scope is not in the scope, by the 'Scope' invariant. So
    extending a scope with distinct names by it yields a scope with distinct
    names.

  * __'withFreshRaw', 'withFreshRaws'__ ('unsafeDistinct', 'unsafeExt'): the
    same argument, for a sequence of names each of which is above the
    watermark of the scope extended by the names before it.

  * __'unsafeAssertFresh', 'unsafeDistinct', 'unsafeExt'__: these produce the
    evidence of the constraints out of thin air, and must only be used when
    the argument above applies. They are exported for the other trusted
    modules.

  * __'sink'__ ('unsafeCoerce'): a value in scope @n@ only mentions names of
    @n@, all of which are names of an extension @l@ of @n@. The 'Sinkable'
    instance of the type of the value proves that it only mentions names
    through the 'Name's in it: 'sinkabilityProof' maps them with the given
    renaming and typechecks. See section 3.5 of the paper.

  * __'sink1'__: 'sink' through a 'Functor', justified by the 'Sinkable'
    instance of 'Compose'.

  * __'sinkClosed'__ ('unsafeExt'): every scope extends the empty scope
    'VoidS', as it has no names.

  * __'withExtTrans'__ ('unsafeExt'): t'Ext' is transitive: if every name of
    @n@ is a name of @i@, and every name of @i@ is a name of @l@, then every
    name of @n@ is a name of @l@. GHC can derive this from the superclass of
    t'Ext' too; asserting it only avoids building that evidence out of the
    evidence of the two steps.

  * __'extendRenamingNameBinder'__ ('unsafeCoerce'): going under a binder in a
    sinkability proof keeps the binder as it is. This is justified in appendix
    A of the paper: a binder may shadow a name of the extended scope, but that
    name can't occur under the binder, as no value under the binder can be
    sunk into the scope it extends without a t'Distinct' scope.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}

module Clash.Core.Scoped.Internal.Scope
  ( -- * Scope indices
    S (..)
    -- * Names and binders
  , Name (UnsafeName)
  , nameId
  , NameBinder (UnsafeNameBinder)
  , nameOf
    -- * Scopes
  , Scope (UnsafeScope)
  , scopeWatermark
  , emptyScope
  , extendScope
  , raiseWatermark
  , withFresh
  , withRefreshed
  , withFreshRaw
  , withFreshRaws
    -- * Constraints
  , Distinct
  , ExtEndo
  , Ext
  , DExt
  , DistinctEvidence (..)
  , ExtEvidence (..)
  , unsafeDistinct
  , unsafeExt
  , unsafeAssertFresh
  , withExtTrans
    -- * Sinking
  , Sinkable (..)
  , sink
  , sink1
  , sinkClosed
  , extendRenamingNameBinder
  ) where

import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)

-- * Scope indices

-- | The kind of scope indices.
data S
  = VoidS
  -- ^ The empty scope. It is the only scope with a name: all other scopes are
  -- type variables, bound by rank-2 polymorphic functions such as 'withFresh'.

-- * Names and binders

-- | A name in scope @n@.
--
-- The raw name is an 'Int'; 'nameId' gives it, for printing, for maps, and
-- for conversion to Clash Core.
newtype Name (n :: S) = UnsafeName Int
  deriving (Eq, Ord, Show)
type role Name nominal

-- | The raw name of a name
nameId :: Name n -> Int
nameId (UnsafeName i) = i
{-# INLINE nameId #-}

-- | A binder of a name, that extends scope @n@ to scope @l@.
newtype NameBinder (n :: S) (l :: S) = UnsafeNameBinder (Name l)
  deriving (Eq, Ord, Show)
type role NameBinder nominal nominal

-- | The name a binder binds
nameOf :: NameBinder n l -> Name l
nameOf (UnsafeNameBinder name) = name
{-# INLINE nameOf #-}

-- * Scopes

-- | A scope, represented by its /watermark/: an upper bound of its names,
-- rather than by the set of its names.
--
-- __Invariant__: every name in scope @n@ is at most the watermark of the
-- @'Scope' n@.
--
-- So a name above the watermark is surely /not/ in the scope. That is all
-- t'Distinct' needs: such a binder is fresh, and extends the scope by raising
-- the watermark to it ('withRefreshed'). A binder at or below the watermark
-- /may/ clash, and is renamed to the name just above the watermark
-- ('withFresh').
--
-- Conversion from Clash Core ("Clash.Core.Scoped.Convert") numbers binders so
-- that names increase along every path from the root, so opening them only
-- raises the watermark: there is no set to insert into, nor a membership test.
-- Only binders of terms moved into a scope that already has larger names are
-- renamed: e.g. those of a closed term inlined deep inside another term.
--
-- See the module header for why the invariant holds.
newtype Scope (n :: S) = UnsafeScope Int
type role Scope nominal

-- | The watermark of a scope: no name in the scope is larger
scopeWatermark :: Scope n -> Int
scopeWatermark (UnsafeScope w) = w
{-# INLINE scopeWatermark #-}

-- | The empty scope. Names are positive, but anything works.
emptyScope :: Scope 'VoidS
emptyScope = UnsafeScope 0
{-# INLINE emptyScope #-}

-- | Extend a scope with a binder. Takes the maximum, so that the 'Scope'
-- invariant holds for any binder.
extendScope :: NameBinder n l -> Scope n -> Scope l
extendScope b (UnsafeScope w) = UnsafeScope (max w (nameId (nameOf b)))
{-# INLINE extendScope #-}

-- | Raise the watermark of a scope to at least the given name, keeping its
-- names. A binder with a fresh name ('withFresh') is then above that name too.
raiseWatermark :: Int -> Scope n -> Scope n
raiseWatermark k (UnsafeScope w) = UnsafeScope (max w k)
{-# INLINE raiseWatermark #-}

-- | A binder with a fresh name: the name just above the watermark
withFresh
  :: Distinct n
  => Scope n
  -> (forall l. DExt n l => NameBinder n l -> r)
  -> r
withFresh (UnsafeScope w) cont
  | w == maxBound = error "withFresh: name space exhausted"
  -- Trusted: w + 1 is above the watermark, see the module header
  | otherwise = unsafeAssertFresh (UnsafeNameBinder (UnsafeName (w + 1))) cont
{-# INLINE withFresh #-}

-- | A binder with the given name if it is surely fresh, i.e. above the
-- watermark, and otherwise with a fresh name (see 'withFresh').
withRefreshed
  :: Distinct n
  => Scope n
  -> Name i
  -> (forall l. DExt n l => NameBinder n l -> r)
  -> r
withRefreshed sc@(UnsafeScope w) n cont
  -- Trusted: k is above the watermark, see the module header
  | k > w = unsafeAssertFresh (UnsafeNameBinder (UnsafeName k)) cont
  | otherwise = withFresh sc cont
 where
  k = nameId n
{-# INLINE withRefreshed #-}

-- | Whether a binder with the given raw name would be fresh for the scope,
-- i.e. whether the name is above the watermark. If it is, the first
-- continuation gets the scope extended with it.
--
-- This lets a traversal that goes under an existing binder reuse it (and the
-- value in its scope) as it is, when it is fresh.
withFreshRaw
  :: forall n r
   . Distinct n
  => Scope n
  -> Int
  -- ^ Raw name of the binder
  -> (forall l. DExt n l => Scope l -> r)
  -- ^ When the name is fresh
  -> r
  -- ^ Otherwise
  -> r
withFreshRaw (UnsafeScope w) k onFresh onClash
  -- Trusted: k is above the watermark, see the module header
  | k > w = case (unsafeDistinct @'VoidS, unsafeExt @n @'VoidS) of
      (Distinct, Ext) -> onFresh (UnsafeScope k :: Scope 'VoidS)
  | otherwise = onClash
{-# INLINE withFreshRaw #-}

-- | Like 'withFreshRaw', for a sequence of binders, each of which extends the
-- scope extended by the ones before it.
--
-- The raw names are given as a fold, rather than a list, so that opening a
-- pattern doesn't allocate a list of its names: a case alternative binds every
-- field of its constructor, and normalized code has many of those.
withFreshRaws
  :: forall n r
   . Distinct n
  => Scope n
  -> (forall a. (Int -> (Int -> a) -> Int -> a) -> (Int -> a) -> Int -> a)
  -- ^ The raw names, in order: @\\step done -> step r1 (step r2 ... done)@
  -> (forall l. DExt n l => Scope l -> r)
  -- ^ When all names are fresh
  -> r
  -- ^ Otherwise
  -> r
withFreshRaws (UnsafeScope w0) raws onFresh onClash = raws step done w0
 where
  -- The accumulator is the watermark of the scope extended by the names so far
  step :: Int -> (Int -> r) -> Int -> r
  step k cont !w
    | k > w = cont k
    | otherwise = onClash

  done :: Int -> r
  done !w =
    -- Trusted: every name is above the watermark of the scope extended by the
    -- names before it, see the module header
    case (unsafeDistinct @'VoidS, unsafeExt @n @'VoidS) of
      (Distinct, Ext) -> onFresh (UnsafeScope w :: Scope 'VoidS)
{-# INLINE withFreshRaws #-}

-- * Constraints

-- | Every scope is a (trivial) extension of itself.
--
-- __Important__: this class exists to track scope extensions for type
-- variables of kind 'S'. It must not have instances.
class ExtEndo (n :: S)

-- | Scope @l@ extends scope @n@: every name of @n@ is a name of @l@.
--
-- __Important__: this class exists to track scope extensions for type
-- variables of kind 'S'. It must not have other instances.
class (ExtEndo n => ExtEndo l) => Ext (n :: S) (l :: S)
instance (ExtEndo n => ExtEndo l) => Ext n l

-- | The names of scope @n@ are distinct: no binder in it shadows another.
--
-- __Important__: this class exists to mark scopes with distinct names. It
-- must not have other instances.
class Distinct (n :: S)
instance Distinct 'VoidS

-- | Scope @l@ extends scope @n@, and has distinct names
type DExt n l = (Distinct l, Ext n l)

-- | Evidence that scope @n@ has distinct names
data DistinctEvidence (n :: S) where
  Distinct :: Distinct n => DistinctEvidence n

-- | Evidence that scope @l@ extends scope @n@
data ExtEvidence (n :: S) (l :: S) where
  Ext :: Ext n l => ExtEvidence n l

-- | Declare that scope @n@ has distinct names. Trusted, see the module header.
unsafeDistinct :: forall n. DistinctEvidence n
unsafeDistinct = unsafeCoerce (Distinct :: DistinctEvidence 'VoidS)

-- | Declare that scope @l@ extends scope @n@. Trusted, see the module header.
unsafeExt :: forall n l. ExtEvidence n l
unsafeExt = unsafeCoerce (Ext :: ExtEvidence 'VoidS 'VoidS)

-- | Declare that a binder is fresh for any scope @n'@. Trusted, see the module
-- header.
unsafeAssertFresh
  :: forall n l n' l' r
   . NameBinder n l
  -> (DExt n' l' => NameBinder n' l' -> r)
  -> r
unsafeAssertFresh binder cont =
  case unsafeDistinct @l' of
    Distinct -> case unsafeExt @n' @l' of
      Ext -> cont (unsafeCoerce binder)
{-# INLINE unsafeAssertFresh #-}

-- | Transitivity of t'Ext'. Trusted, see the module header.
--
-- GHC derives @'Ext' n l@ from @'Ext' n i@ and @'Ext' i l@ by itself, but the
-- evidence it builds refers to the evidence of both steps. A loop that moves
-- into a new extension at every iteration, such as the evaluator's, would
-- then build a chain of evidence as long as the number of iterations. The
-- evidence carries no information, so this asserts it instead.
withExtTrans :: forall n i l r. (Ext n i, Ext i l) => (Ext n l => r) -> r
withExtTrans r = case unsafeExt @n @l of Ext -> r
{-# INLINE withExtTrans #-}

-- * Sinking

-- | Values of type @e n@ can be moved into an extension of scope @n@, by
-- renaming the names in them through an injection.
--
-- Implement 'sinkabilityProof' by applying the renaming to every 'Name', and
-- going under binders with 'extendRenamingNameBinder' (or its sorted versions
-- in "Clash.Core.Scoped.Syntax"). Never call it: once it typechecks, 'sink' is
-- a coercion. So the proof must be total, and must not use 'undefined', 'error'
-- or unsafe functions: those would make any value sinkable.
class Sinkable (e :: S -> Type) where
  sinkabilityProof
    :: (Name n -> Name l)
    -- ^ Map names from scope @n@ to a (possibly larger) scope @l@
    -> e n
    -> e l

instance Sinkable Name where
  sinkabilityProof rename = rename

-- | A container of sinkable values is sinkable, elementwise. See 'sink1'.
instance (Functor f, Sinkable e) => Sinkable (Compose f e) where
  sinkabilityProof rename (Compose xs) = Compose (fmap (sinkabilityProof rename) xs)

-- | Move a value into an extended scope. A coercion, see the module header.
sink :: (Sinkable e, DExt n l) => e n -> e l
sink = unsafeCoerce
{-# INLINE sink #-}

-- | Move a container of values into an extended scope, without walking it.
-- Use this rather than @'fmap' 'sink'@.
sink1 :: (Functor f, Sinkable e, DExt n l) => f (e n) -> f (e l)
sink1 = getCompose . sink . Compose
{-# INLINE sink1 #-}

-- | Move a closed value into any scope. Every scope extends the empty scope.
sinkClosed :: forall e n. (Sinkable e, Distinct n) => e 'VoidS -> e n
sinkClosed e = case unsafeExt @'VoidS @n of
  Ext -> sink e
{-# INLINE sinkClosed #-}

-- | Extend a renaming when going under a binder, in a 'sinkabilityProof'. The
-- binder is kept as it is, which is sound, see the module header.
extendRenamingNameBinder
  :: (Name n -> Name n')
  -- ^ Map names from scope @n@ to a (possibly larger) scope @n'@
  -> NameBinder n l
  -- ^ A binder that extends scope @n@ to scope @l@
  -> (forall l'. (Name l -> Name l') -> NameBinder n' l' -> r)
  -- ^ Gets the extended renaming, and the binder, extending @n'@
  -> r
extendRenamingNameBinder _ (UnsafeNameBinder name) cont =
  cont unsafeCoerce (UnsafeNameBinder name)
