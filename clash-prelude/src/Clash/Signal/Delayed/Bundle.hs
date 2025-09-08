{-|
  Copyright   :  (C) 2019, Myrtle Software Ltd.
                     2018, @blaxill
                     2018, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Clash.Signal.Delayed.Bundle (
    Bundle(..)
  -- ** Tools to emulate pre Clash 1.0 @Bundle ()@ instance
  , B.EmptyTuple(..)
  , TaggedEmptyTuple(..)
  ) where

#if !MIN_VERSION_base(4,18,0)
import           Control.Applicative           (liftA2)
#endif
import           GHC.TypeLits                  (KnownNat)
import           Prelude                       hiding (head, map, tail)

import           Clash.Signal.Internal         (Domain)
import           Clash.Signal.Delayed (DSignal, toSignal, unsafeFromSignal)
import qualified Clash.Signal.Bundle           as B

import           Clash.Sized.BitVector         (Bit, BitVector)
import           Clash.Sized.Fixed             (Fixed)
import           Clash.Sized.Index             (Index)
import           Clash.Sized.RTree             (RTree, lazyT)
import           Clash.Sized.Signed            (Signed)
import           Clash.Sized.Unsigned          (Unsigned)
import           Clash.Sized.Vector            (Vec, lazyV)

import           GHC.TypeLits                  (Nat)

-- | Isomorphism between a 'DSignal' of a product type
-- (e.g. a tuple) and a product type of 'DSignal's.
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
--   type 'Unbundled' dom delay D = 'DSignal' dom delay D
--   'bundle'   s = s
--   'unbundle' s = s
-- @
--
class Bundle a where
  type Unbundled (dom :: Domain) (d :: Nat) a = res | res -> dom d a
  type Unbundled dom d a = DSignal dom d a

  -- | Example:
  --
  -- @
  -- __bundle__ :: ('DSignal' dom d a, 'DSignal' dom d b) -> 'DSignal' dom d (a,b)
  -- @
  --
  -- However:
  --
  -- @
  -- __bundle__ :: 'DSignal' dom 'Clash.Sized.BitVector.Bit' -> 'DSignal' dom 'Clash.Sized.BitVector.Bit'
  -- @
  bundle :: Unbundled dom d a -> DSignal dom d a
  {-# INLINE bundle #-}
  default bundle :: (DSignal dom d a ~ Unbundled dom d a)
                 => Unbundled dom d a -> DSignal dom d a
  bundle s = s
  -- | Example:
  --
  -- @
  -- __unbundle__ :: 'DSignal' dom d (a,b) -> ('DSignal' dom d a, 'DSignal' dom d b)
  -- @
  --
  -- However:
  --
  -- @
  -- __unbundle__ :: 'DSignal' dom 'Clash.Sized.BitVector.Bit' -> 'DSignal' dom 'Clash.Sized.BitVector.Bit'
  -- @
  unbundle :: DSignal dom d a -> Unbundled dom d a
  {-# INLINE unbundle #-}
  default unbundle :: (Unbundled dom d a ~ DSignal dom d a)
                   => DSignal dom d a -> Unbundled dom d a
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

instance Bundle (a,b) where
  type Unbundled t delay (a,b) = (DSignal t delay a, DSignal t delay b)

  bundle       = uncurry (liftA2 (,))
  unbundle tup = (fmap fst tup, fmap snd tup)

instance Bundle (a,b,c) where
  type Unbundled t delay (a,b,c) =
    ( DSignal t delay a, DSignal t delay b, DSignal t delay c)

  bundle   (a,b,c) = (,,) <$> a <*> b <*> c
  unbundle tup     = (fmap (\(x,_,_) -> x) tup
                      ,fmap (\(_,x,_) -> x) tup
                      ,fmap (\(_,_,x) -> x) tup
                      )
instance Bundle (a,b,c,d) where
  type Unbundled t delay (a,b,c,d) =
    ( DSignal t delay a, DSignal t delay b, DSignal t delay c, DSignal t delay d)

  bundle   (a,b,c,d) = (,,,) <$> a <*> b <*> c <*> d
  unbundle tup     = (fmap (\(x,_,_,_) -> x) tup
                      ,fmap (\(_,x,_,_) -> x) tup
                      ,fmap (\(_,_,x,_) -> x) tup
                      ,fmap (\(_,_,_,x) -> x) tup
                      )

instance Bundle (a,b,c,d,e) where
  type Unbundled t delay (a,b,c,d,e) =
    ( DSignal t delay a, DSignal t delay b, DSignal t delay c, DSignal t delay d
    , DSignal t delay e)

  bundle   (a,b,c,d,e) = (,,,,) <$> a <*> b <*> c <*> d <*> e
  unbundle tup     = (fmap (\(x,_,_,_,_) -> x) tup
                      ,fmap (\(_,x,_,_,_) -> x) tup
                      ,fmap (\(_,_,x,_,_) -> x) tup
                      ,fmap (\(_,_,_,x,_) -> x) tup
                      ,fmap (\(_,_,_,_,x) -> x) tup
                      )

instance Bundle (a,b,c,d,e,f) where
  type Unbundled t delay (a,b,c,d,e,f) =
    ( DSignal t delay a, DSignal t delay b, DSignal t delay c, DSignal t delay d
    , DSignal t delay e, DSignal t delay f)

  bundle   (a,b,c,d,e,f) = (,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f
  unbundle tup           = (fmap (\(x,_,_,_,_,_) -> x) tup
                           ,fmap (\(_,x,_,_,_,_) -> x) tup
                           ,fmap (\(_,_,x,_,_,_) -> x) tup
                           ,fmap (\(_,_,_,x,_,_) -> x) tup
                           ,fmap (\(_,_,_,_,x,_) -> x) tup
                           ,fmap (\(_,_,_,_,_,x) -> x) tup
                           )

instance Bundle (a,b,c,d,e,f,g) where
  type Unbundled t delay (a,b,c,d,e,f,g) =
    ( DSignal t delay a, DSignal t delay b, DSignal t delay c, DSignal t delay d
    , DSignal t delay e, DSignal t delay f, DSignal t delay g)

  bundle   (a,b,c,d,e,f,g) = (,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f
                                      <*> g
  unbundle tup             = (fmap (\(x,_,_,_,_,_,_) -> x) tup
                             ,fmap (\(_,x,_,_,_,_,_) -> x) tup
                             ,fmap (\(_,_,x,_,_,_,_) -> x) tup
                             ,fmap (\(_,_,_,x,_,_,_) -> x) tup
                             ,fmap (\(_,_,_,_,x,_,_) -> x) tup
                             ,fmap (\(_,_,_,_,_,x,_) -> x) tup
                             ,fmap (\(_,_,_,_,_,_,x) -> x) tup
                             )

instance Bundle (a,b,c,d,e,f,g,h) where
  type Unbundled t delay (a,b,c,d,e,f,g,h) =
    ( DSignal t delay a, DSignal t delay b, DSignal t delay c, DSignal t delay d
    , DSignal t delay e, DSignal t delay f ,DSignal t delay g, DSignal t delay h)

  bundle   (a,b,c,d,e,f,g,h) = (,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f
                                         <*> g <*> h
  unbundle tup               = (fmap (\(x,_,_,_,_,_,_,_) -> x) tup
                               ,fmap (\(_,x,_,_,_,_,_,_) -> x) tup
                               ,fmap (\(_,_,x,_,_,_,_,_) -> x) tup
                               ,fmap (\(_,_,_,x,_,_,_,_) -> x) tup
                               ,fmap (\(_,_,_,_,x,_,_,_) -> x) tup
                               ,fmap (\(_,_,_,_,_,x,_,_) -> x) tup
                               ,fmap (\(_,_,_,_,_,_,x,_) -> x) tup
                               ,fmap (\(_,_,_,_,_,_,_,x) -> x) tup
                               )

instance KnownNat n => Bundle (Vec n a) where
  type Unbundled t d (Vec n a) = Vec n (DSignal t d a)
  bundle   = unsafeFromSignal . B.bundle . fmap toSignal
  unbundle = sequenceA . fmap lazyV

instance KnownNat d => Bundle (RTree d a) where
  type Unbundled t delay (RTree d a) = RTree d (DSignal t delay a)
  bundle   = sequenceA
  unbundle = sequenceA . fmap lazyT

-- | Same as 'Clash.Signal.Bundle.TaggedEmptyTuple' in "Clash.Signal.Bundle", but adapted for 'DSignal'.
data TaggedEmptyTuple (dom :: Domain) (d :: Nat) = TaggedEmptyTuple

-- | See [commit 94b0bff5](https://github.com/clash-lang/clash-compiler/pull/539/commits/94b0bff5770aa4961e04ddce2515130df3fc7863)
-- and documentation for 'Clash.Signal.Bundle.TaggedEmptyTuple'.
instance Bundle B.EmptyTuple where
  type Unbundled dom d B.EmptyTuple = TaggedEmptyTuple dom d

  bundle :: TaggedEmptyTuple dom d -> DSignal dom d B.EmptyTuple
  bundle TaggedEmptyTuple = pure B.EmptyTuple

  unbundle :: DSignal dom d B.EmptyTuple -> TaggedEmptyTuple dom d
  unbundle s = seq s TaggedEmptyTuple
