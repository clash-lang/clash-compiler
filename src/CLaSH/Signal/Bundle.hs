{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE TypeFamilies      #-}

-- | The Product/Signal isomorphism
module CLaSH.Signal.Bundle
  ( Bundle (..)
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
-- Instances of 'Bundle' must satisfy the following laws:
--
-- @
-- bundle . unbundle = 'id'
-- unbundle . bundle = 'id'
-- @
class Bundle a where
  type Unbundled (clk :: Clock) a
  type Unbundled clk a = CSignal clk a
  -- | Example:
  --
  -- > bundle :: (CSignal clk a, CSignal clk b) -> CSignal clk (a,b)
  --
  -- However:
  --
  -- > bundle :: CSignal clk Bit -> CSignal clk Bit
  bundle :: SClock clk -> Unbundled clk a -> CSignal clk a

  {-# INLINE bundle #-}
  default bundle :: SClock clk ->  CSignal clk a -> CSignal clk a
  bundle _ s = s
  -- | Example:
  --
  -- > unbundle :: CSignal clk (a,b) -> (CSignal clk a, CSignal clk b)
  --
  -- However:
  --
  -- > unbundle :: CSignal clk Bit -> CSignal clk Bit
  unbundle :: SClock clk -> CSignal clk a -> Unbundled clk a

  {-# INLINE unbundle #-}
  default unbundle :: SClock clk -> CSignal clk a -> CSignal clk a
  unbundle _ s = s

instance Bundle Bool
instance Bundle Integer
instance Bundle Int
instance Bundle Float
instance Bundle Double
instance Bundle ()

instance Bundle (BitVector n)
instance Bundle (Index n)
instance Bundle (Fixed rep int frac)
instance Bundle (Signed n)
instance Bundle (Unsigned n)

instance Bundle (a,b) where
  type Unbundled t (a,b) = (CSignal t a, CSignal t b)
  bundle   _     = uncurry (liftA2 (,))
  unbundle _ tup = (fmap fst tup, fmap snd tup)

instance Bundle (a,b,c) where
  type Unbundled t (a,b,c) = (CSignal t a, CSignal t b, CSignal t c)
  bundle   _ (a,b,c) = (,,) <$> a <*> b <*> c
  unbundle _ tup     = (fmap (\(x,_,_) -> x) tup
                       ,fmap (\(_,x,_) -> x) tup
                       ,fmap (\(_,_,x) -> x) tup
                       )

instance Bundle (a,b,c,d) where
  type Unbundled t (a,b,c,d) = ( CSignal t a, CSignal t b, CSignal t c
                             , CSignal t d
                             )
  bundle   _ (a,b,c,d) = (,,,) <$> a <*> b <*> c <*> d
  unbundle _ tup       = (fmap (\(x,_,_,_) -> x) tup
                         ,fmap (\(_,x,_,_) -> x) tup
                         ,fmap (\(_,_,x,_) -> x) tup
                         ,fmap (\(_,_,_,x) -> x) tup
                         )

instance Bundle (a,b,c,d,e) where
  type Unbundled t (a,b,c,d,e) = ( CSignal t a, CSignal t b, CSignal t c
                               , CSignal t d, CSignal t e
                               )
  bundle   _ (a,b,c,d,e) = (,,,,) <$> a <*> b <*> c <*> d <*> e
  unbundle _ tup         = (fmap (\(x,_,_,_,_) -> x) tup
                           ,fmap (\(_,x,_,_,_) -> x) tup
                           ,fmap (\(_,_,x,_,_) -> x) tup
                           ,fmap (\(_,_,_,x,_) -> x) tup
                           ,fmap (\(_,_,_,_,x) -> x) tup
                           )

instance Bundle (a,b,c,d,e,f) where
  type Unbundled t (a,b,c,d,e,f) = ( CSignal t a, CSignal t b, CSignal t c
                                 , CSignal t d, CSignal t e, CSignal t f
                                 )
  bundle   _ (a,b,c,d,e,f) = (,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f
  unbundle _ tup           = (fmap (\(x,_,_,_,_,_) -> x) tup
                             ,fmap (\(_,x,_,_,_,_) -> x) tup
                             ,fmap (\(_,_,x,_,_,_) -> x) tup
                             ,fmap (\(_,_,_,x,_,_) -> x) tup
                             ,fmap (\(_,_,_,_,x,_) -> x) tup
                             ,fmap (\(_,_,_,_,_,x) -> x) tup
                             )

instance Bundle (a,b,c,d,e,f,g) where
  type Unbundled t (a,b,c,d,e,f,g) = ( CSignal t a, CSignal t b, CSignal t c
                                   , CSignal t d, CSignal t e, CSignal t f
                                   , CSignal t g
                                   )
  bundle   _ (a,b,c,d,e,f,g) = (,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f
                                        <*> g
  unbundle _ tup             = (fmap (\(x,_,_,_,_,_,_) -> x) tup
                               ,fmap (\(_,x,_,_,_,_,_) -> x) tup
                               ,fmap (\(_,_,x,_,_,_,_) -> x) tup
                               ,fmap (\(_,_,_,x,_,_,_) -> x) tup
                               ,fmap (\(_,_,_,_,x,_,_) -> x) tup
                               ,fmap (\(_,_,_,_,_,x,_) -> x) tup
                               ,fmap (\(_,_,_,_,_,_,x) -> x) tup
                               )

instance Bundle (a,b,c,d,e,f,g,h) where
  type Unbundled t (a,b,c,d,e,f,g,h) = ( CSignal t a, CSignal t b, CSignal t c
                                     , CSignal t d, CSignal t e, CSignal t f
                                     , CSignal t g, CSignal t h
                                     )
  bundle   _ (a,b,c,d,e,f,g,h) = (,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f
                                           <*> g <*> h
  unbundle _ tup               = (fmap (\(x,_,_,_,_,_,_,_) -> x) tup
                                 ,fmap (\(_,x,_,_,_,_,_,_) -> x) tup
                                 ,fmap (\(_,_,x,_,_,_,_,_) -> x) tup
                                 ,fmap (\(_,_,_,x,_,_,_,_) -> x) tup
                                 ,fmap (\(_,_,_,_,x,_,_,_) -> x) tup
                                 ,fmap (\(_,_,_,_,_,x,_,_) -> x) tup
                                 ,fmap (\(_,_,_,_,_,_,x,_) -> x) tup
                                 ,fmap (\(_,_,_,_,_,_,_,x) -> x) tup
                                 )

instance KnownNat n => Bundle (Vec n a) where
  type Unbundled t (Vec n a) = Vec n (CSignal t a)
  -- The 'Traversable' instance of 'Vec' is not synthesisable, so we must
  -- define 'bundle' as a primitive.
  bundle     = vecUnwrap#
  unbundle _ = sequenceA

{-# NOINLINE vecUnwrap# #-}
vecUnwrap# :: SClock t -> Vec n (CSignal t a) -> CSignal t (Vec n a)
vecUnwrap# _ = sequenceA
