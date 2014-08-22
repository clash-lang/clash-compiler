{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE TypeFamilies      #-}

-- | The Product/Signal isomorphism
module CLaSH.Signal.Wrap
  ( Wrap (..)
  )
where

import Control.Applicative   ((<$>), (<*>), liftA2)
import Data.Traversable      (sequenceA)
import GHC.TypeLits          (KnownNat)
import Prelude               hiding (head, map, tail)

import CLaSH.Signal.Internal (Clock, CSignal (..), SClock)
import CLaSH.Sized.BitVector (BitVector)
import CLaSH.Sized.Fixed     (Fixed)
import CLaSH.Sized.Index     (Index)
import CLaSH.Sized.Signed    (Signed)
import CLaSH.Sized.Unsigned  (Unsigned)
import CLaSH.Sized.Vector    (Vec)

-- | Isomorphism between a 'CSignal' of a product type (e.g. a tuple) and a
-- product type of 'CSignal's.
--
-- Instances of 'Wrap' must satisfy the following laws:
--
-- @
-- unwrap . wrap = 'id'
-- wrap . unwrap = 'id'
-- @
class Wrap a where
  type Wrapped (clk :: Clock) a
  type Wrapped clk a = CSignal clk a
  -- | Example:
  --
  -- > unwrap :: (CSignal clk a, CSignal clk b) -> CSignal clk (a,b)
  --
  -- However:
  --
  -- > unwrap :: CSignal clk Bit -> CSignal clk Bit
  unwrap :: SClock clk -> Wrapped clk a -> CSignal clk a

  default unwrap :: SClock clk ->  CSignal clk a -> CSignal clk a
  unwrap _ s = s
  -- | Example:
  --
  -- > wrap :: CSignal clk (a,b) -> (CSignal clk a, CSignal clk b)
  --
  -- However:
  --
  -- > wrap :: CSignal clk Bit -> CSignal clk Bit
  wrap :: SClock clk -> CSignal clk a -> Wrapped clk a

  default wrap :: SClock clk -> CSignal clk a -> CSignal clk a
  wrap _ s = s

instance Wrap Bool
instance Wrap Integer
instance Wrap Int
instance Wrap Float
instance Wrap Double
instance Wrap ()

instance Wrap (BitVector n)
instance Wrap (Index n)
instance Wrap (Fixed rep int frac)
instance Wrap (Signed n)
instance Wrap (Unsigned n)

instance Wrap (a,b) where
  type Wrapped t (a,b) = (CSignal t a, CSignal t b)
  unwrap _     = uncurry (liftA2 (,))
  wrap   _ tup = (fmap fst tup, fmap snd tup)

instance Wrap (a,b,c) where
  type Wrapped t (a,b,c) = (CSignal t a, CSignal t b, CSignal t c)
  unwrap _ (a,b,c) = (,,) <$> a <*> b <*> c
  wrap   _ tup     = (fmap (\(x,_,_) -> x) tup
                     ,fmap (\(_,x,_) -> x) tup
                     ,fmap (\(_,_,x) -> x) tup
                     )

instance Wrap (a,b,c,d) where
  type Wrapped t (a,b,c,d) = ( CSignal t a, CSignal t b, CSignal t c
                             , CSignal t d
                             )
  unwrap _ (a,b,c,d) = (,,,) <$> a <*> b <*> c <*> d
  wrap   _ tup       = (fmap (\(x,_,_,_) -> x) tup
                       ,fmap (\(_,x,_,_) -> x) tup
                       ,fmap (\(_,_,x,_) -> x) tup
                       ,fmap (\(_,_,_,x) -> x) tup
                       )

instance Wrap (a,b,c,d,e) where
  type Wrapped t (a,b,c,d,e) = ( CSignal t a, CSignal t b, CSignal t c
                               , CSignal t d, CSignal t e
                               )
  unwrap _ (a,b,c,d,e) = (,,,,) <$> a <*> b <*> c <*> d <*> e
  wrap   _ tup         = (fmap (\(x,_,_,_,_) -> x) tup
                         ,fmap (\(_,x,_,_,_) -> x) tup
                         ,fmap (\(_,_,x,_,_) -> x) tup
                         ,fmap (\(_,_,_,x,_) -> x) tup
                         ,fmap (\(_,_,_,_,x) -> x) tup
                         )

instance Wrap (a,b,c,d,e,f) where
  type Wrapped t (a,b,c,d,e,f) = ( CSignal t a, CSignal t b, CSignal t c
                                 , CSignal t d, CSignal t e, CSignal t f
                                 )
  unwrap _ (a,b,c,d,e,f) = (,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f
  wrap   _ tup           = (fmap (\(x,_,_,_,_,_) -> x) tup
                           ,fmap (\(_,x,_,_,_,_) -> x) tup
                           ,fmap (\(_,_,x,_,_,_) -> x) tup
                           ,fmap (\(_,_,_,x,_,_) -> x) tup
                           ,fmap (\(_,_,_,_,x,_) -> x) tup
                           ,fmap (\(_,_,_,_,_,x) -> x) tup
                           )

instance Wrap (a,b,c,d,e,f,g) where
  type Wrapped t (a,b,c,d,e,f,g) = ( CSignal t a, CSignal t b, CSignal t c
                                   , CSignal t d, CSignal t e, CSignal t f
                                   , CSignal t g
                                   )
  unwrap _ (a,b,c,d,e,f,g) = (,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g
  wrap   _ tup             = (fmap (\(x,_,_,_,_,_,_) -> x) tup
                             ,fmap (\(_,x,_,_,_,_,_) -> x) tup
                             ,fmap (\(_,_,x,_,_,_,_) -> x) tup
                             ,fmap (\(_,_,_,x,_,_,_) -> x) tup
                             ,fmap (\(_,_,_,_,x,_,_) -> x) tup
                             ,fmap (\(_,_,_,_,_,x,_) -> x) tup
                             ,fmap (\(_,_,_,_,_,_,x) -> x) tup
                             )

instance Wrap (a,b,c,d,e,f,g,h) where
  type Wrapped t (a,b,c,d,e,f,g,h) = ( CSignal t a, CSignal t b, CSignal t c
                                     , CSignal t d, CSignal t e, CSignal t f
                                     , CSignal t g, CSignal t h
                                     )
  unwrap _ (a,b,c,d,e,f,g,h) = (,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f
                                         <*> g <*> h
  wrap   _ tup               = (fmap (\(x,_,_,_,_,_,_,_) -> x) tup
                               ,fmap (\(_,x,_,_,_,_,_,_) -> x) tup
                               ,fmap (\(_,_,x,_,_,_,_,_) -> x) tup
                               ,fmap (\(_,_,_,x,_,_,_,_) -> x) tup
                               ,fmap (\(_,_,_,_,x,_,_,_) -> x) tup
                               ,fmap (\(_,_,_,_,_,x,_,_) -> x) tup
                               ,fmap (\(_,_,_,_,_,_,x,_) -> x) tup
                               ,fmap (\(_,_,_,_,_,_,_,x) -> x) tup
                               )

instance KnownNat n => Wrap (Vec n a) where
  type Wrapped t (Vec n a) = Vec n (CSignal t a)
  -- The 'Traversable' instances of both 'Vec' and 'CSignal' are not
  -- synthesisable, so we must define these two functions as primitives.
  unwrap = vecUnwrap#
  wrap   = vecWrap#

{-# NOINLINE vecUnwrap# #-}
vecUnwrap# :: KnownNat n => SClock t -> Vec n (CSignal t a)
           -> CSignal t (Vec n a)
vecUnwrap# _ = sequenceA

{-# NOINLINE vecWrap# #-}
vecWrap# :: KnownNat n => SClock t -> CSignal t (Vec n a) -> Vec n (CSignal t a)
vecWrap# _ = sequenceA
