{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017-2019, Myrtle Software Ltd, Google Inc.
                       2019, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

The Product/Signal isomorphism
-}

{-# LANGUAGE CPP                    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Signal.Bundle
  ( Bundle (..)
  -- ** Internal
  , vecBundle#
  )
where

import Data.Functor.Compose
import GHC.Generics
import GHC.TypeLits                 (KnownNat)
import Prelude                      hiding (head, map, tail)

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
-- product type of 'Clash.Signal.Signal''s.
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
-- @
-- data Pair a b = MkPair { getA :: a, getB :: b }
--
-- instance Bundle (Pair a b) where
--   type Unbundled dom (Pair a b) = Pair (Signal dom a) (Signal dom b)
--
--   -- bundle :: Pair (Signal dom a) (Signal dom b) -> Signal dom (Pair a b)
--   bundle   (MkPair as bs) = MkPair <$> as <*> bs
--
--   -- unbundle :: Signal dom (Pair a b) -> Pair (Signal dom a) (Signal dom b)
--   unbundle pairs = MkPair (getA <$> pairs) (getB <$> pairs)
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

deriveBundleTuples ''Bundle ''Unbundled 'bundle 'unbundle

instance KnownNat n => Bundle (Vec n a) where
  type Unbundled t (Vec n a) = Vec n (Signal t a)
  -- The 'Traversable' instance of 'Vec' is not synthesizable, so we must
  -- define 'bundle' as a primitive.
  bundle   = vecBundle#
  unbundle = sequenceA . fmap lazyV

{-# NOINLINE vecBundle# #-}
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
