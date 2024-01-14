{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017-2019, Myrtle Software Ltd, Google Inc.
                  2019,2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

The Product/Signal isomorphism
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-# LANGUAGE Trustworthy #-}

--{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Signal.Bundle
  ( Bundle (..)
  -- ** Tools to emulate pre Clash 1.0 @Bundle ()@ instance
  , EmptyTuple(..)
  , TaggedEmptyTuple(..)
  -- ** Internal
  , vecBundle#
  )
where

import Data.Functor.Compose
import GHC.Generics
import GHC.TypeLits                 (KnownNat)
import Prelude                      hiding (head, map, tail)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Signal.Bundle.Internal (deriveBundleTuples)
import Clash.Signal.Internal        (Signal (..), Domain)
import Clash.Sized.BitVector        (Bit, BitVector)
import Clash.Sized.Fixed            (Fixed)
import Clash.Sized.Index            (Index)
import Clash.Sized.Signed           (Signed)
import Clash.Sized.Unsigned         (Unsigned)
import Clash.Sized.Vector           (Vec, traverse#, lazyV)
import Clash.Sized.RTree            (RTree, lazyT)

-- | Isomorphism between a 'Clash.Signal.Signal' of a product type (e.g. a tuple) and a
-- product type of 'Clash.Signal.Signal's.
--
-- Instances of 'Bundle' must satisfy the following laws:
--
-- @
-- 'bundle' . 'unbundle' = 'id'
-- 'unbundle' . 'bundle' = 'id'
-- @
--
-- By default, 'bundle' and 'unbundle', are defined as the identity, that is,
-- writing:
--
-- @
-- data D = A | B
--
-- instance Bundle D
-- @
--
-- is the same as:
--
-- @
-- data D = A | B
--
-- instance Bundle D where
--   type 'Unbundled' clk D = 'Signal' clk D
--   'bundle'   s = s
--   'unbundle' s = s
-- @
--
-- For custom product types you'll have to write the instance manually:
--
-- @
-- data Pair a b = MkPair { getA :: a, getB :: b }
--
-- instance Bundle (Pair a b) where
--   type Unbundled dom (Pair a b) = Pair (Signal dom a) (Signal dom b)
--
--   -- bundle :: Pair (Signal dom a) (Signal dom b) -> Signal dom (Pair a b)
--   bundle   (MkPair as bs) = MkPair '<$>' as '<*>' bs
--
--   -- unbundle :: Signal dom (Pair a b) -> Pair (Signal dom a) (Signal dom b)
--   unbundle pairs = MkPair (getA '<$>' pairs) (getB '<$>' pairs)
-- @

class Bundle a where
  type Unbundled (dom :: Domain) a = res | res -> dom a
  type Unbundled dom a = Signal dom a
  -- | Example:
  --
  -- @
  -- __bundle__ :: ('Signal' dom a, 'Signal' dom b) -> 'Signal' dom (a,b)
  -- @
  --
  -- However:
  --
  -- @
  -- __bundle__ :: 'Signal' dom 'Clash.Sized.BitVector.Bit' -> 'Signal' dom 'Clash.Sized.BitVector.Bit'
  -- @
  bundle :: Unbundled dom a -> Signal dom a

  {-# INLINE bundle #-}
  default bundle :: (Signal dom a ~ Unbundled dom a)
                 => Unbundled dom a -> Signal dom a
  bundle s = s
  -- | Example:
  --
  -- @
  -- __unbundle__ :: 'Signal' dom (a,b) -> ('Signal' dom a, 'Signal' dom b)
  -- @
  --
  -- However:
  --
  -- @
  -- __unbundle__ :: 'Signal' dom 'Clash.Sized.BitVector.Bit' -> 'Signal' dom 'Clash.Sized.BitVector.Bit'
  -- @
  unbundle :: Signal dom a -> Unbundled dom a

  {-# INLINE unbundle #-}
  default unbundle :: (Unbundled dom a ~ Signal dom a)
                   => Signal dom a -> Unbundled dom a
  unbundle s = s

instance Bundle ()
instance Bundle Bool
instance Bundle Integer
instance Bundle Int
instance Bundle Float
instance Bundle Double
instance Bundle (Maybe a)
instance Bundle (Either a b)

instance Bundle Bit
instance Bundle (BitVector n)
instance Bundle (Index n)
instance Bundle (Fixed rep int frac)
instance Bundle (Signed n)
instance Bundle (Unsigned n)

-- | __NB__: The documentation only shows instances up to /3/-tuples. By
-- default, instances up to and including /12/-tuples will exist. If the flag
-- @large-tuples@ is set instances up to the GHC imposed limit will exist. The
-- GHC imposed limit is either 62 or 64 depending on the GHC version.
deriveBundleTuples ''Bundle ''Unbundled 'bundle 'unbundle

instance KnownNat n => Bundle (Vec n a) where
  type Unbundled t (Vec n a) = Vec n (Signal t a)
  -- The 'Traversable' instance of 'Vec' is not synthesizable, so we must
  -- define 'bundle' as a primitive.
  bundle   = vecBundle#
  unbundle = sequenceA . fmap lazyV

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE vecBundle# #-}
{-# ANN vecBundle# hasBlackBox #-}
vecBundle# :: Vec n (Signal t a) -> Signal t (Vec n a)
vecBundle# = traverse# id

instance KnownNat d => Bundle (RTree d a) where
  type Unbundled t (RTree d a) = RTree d (Signal t a)
  bundle   = sequenceA
  unbundle = sequenceA . fmap lazyT

instance Bundle ((f :*: g) a) where
  type Unbundled t ((f :*: g) a) = (Compose (Signal t) f :*: Compose (Signal t) g) a
  bundle (Compose l :*: Compose r) = (:*:) <$> l <*> r
  unbundle s = Compose (getL <$> s) :*: Compose (getR <$> s)
   where
    getL (l :*: _) = l
    getR (_ :*: r) = r

-- | See 'TaggedEmptyTuple'
data EmptyTuple = EmptyTuple

-- | Helper type to emulate the "old" behavior of Bundle's unit instance. I.e.,
-- the instance for @Bundle ()@ used to be defined as:
--
-- @
-- class Bundle () where
--   bundle   :: () -> Signal dom ()
--   unbundle :: Signal dom () -> ()
-- @
--
-- In order to have sensible type inference, the 'Bundle' class specifies that
-- the argument type of 'bundle' should uniquely identify the result type, and
-- vice versa for 'unbundle'. The type signatures in the snippet above don't
-- though, as @()@ doesn't uniquely map to a specific domain. In other words,
-- @domain@ should occur in both the argument and result of both functions.
--
-- 'TaggedEmptyTuple' tackles this by carrying the domain in its type. The
-- 'bundle' and 'unbundle' instance now looks like:
--
-- @
-- class Bundle EmptyTuple where
--   bundle   :: TaggedEmptyTuple dom -> Signal dom EmptyTuple
--   unbundle :: Signal dom EmptyTuple -> TaggedEmptyTuple dom
-- @
--
-- @dom@ is now mentioned both the argument and result for both 'bundle' and
-- 'unbundle'.
data TaggedEmptyTuple (dom :: Domain) = TaggedEmptyTuple

-- | See [commit 94b0bff5](https://github.com/clash-lang/clash-compiler/pull/539/commits/94b0bff5770aa4961e04ddce2515130df3fc7863)
-- and documentation for 'TaggedEmptyTuple'.
instance Bundle EmptyTuple where
  type Unbundled dom EmptyTuple = TaggedEmptyTuple dom

  bundle :: TaggedEmptyTuple dom -> Signal dom EmptyTuple
  bundle TaggedEmptyTuple = pure EmptyTuple

  unbundle :: Signal dom EmptyTuple -> TaggedEmptyTuple dom
  unbundle s = seq s TaggedEmptyTuple
