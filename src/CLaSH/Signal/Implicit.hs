{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}

module CLaSH.Signal.Implicit
  ( -- * Implicitly clocked synchronous signal
    Signal
    -- * Basic circuit functions
  , signal
  , register
  , Pack(..)
  , (<^), (^>)
    -- * Simulation functions
  , simulate
  , simulateP
    -- * List \<-\> Signal conversion
  , sample
  , sampleN
  , fromList
  )
where

import Control.Applicative  (Applicative (..), (<$>), liftA2)

import CLaSH.Sized.Fixed    (Fixed)
import CLaSH.Sized.Signed   (Signed)
import CLaSH.Sized.Unsigned (Unsigned)
import CLaSH.Sized.Vector   (Vec(..), vmap, vhead, vtail)

import CLaSH.Signal.Types

{-# NOINLINE register  #-}

-- | Create a 'Signal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- NB: Simulation only!
--
-- >>> sampleN 2 (fromList [1,2,3,4,5])
-- [1,2]
fromList :: [a] -> Signal a
fromList []     = error "finite list"
fromList (x:xs) = x :- fromList xs

-- | Get an infinite list of samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal' at
-- consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
sample :: Signal a -> [a]
sample ~(x :- xs) = x : sample xs

-- | Get a list of @n@ samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal' at
-- consecutive clock cycles
--
-- > sampleN 3 s == [s0, s1, s2]
sampleN :: Int -> Signal a -> [a]
sampleN 0 _          = []
sampleN n ~(x :- xs) = x : (sampleN (n-1) xs)

-- | 'register' @i s@ delays the values in 'Signal' @s@ for one cycle, and sets
-- the value at time 0 to @i@
--
-- >>> sampleN 3 (register 8 (fromList [1,2,3,4]))
-- [8,1,2]
register :: a -> Signal a -> Signal a
register i s = i :- s

-- | Simulate a (@'Signal' a -> 'Signal' b@) function given a list of samples of
-- type @a@
--
-- >>> simulate (register 8) [1, 2, 3, ...
-- [8, 1, 2, 3, ...
simulate :: (Signal a -> Signal b) -> [a] -> [b]
simulate f = sample . f . fromList

-- | Isomorphism between a 'Signal' of a product type (e.g. a tuple) and a
-- product type of 'Signal's.
--
-- Instances of 'Pack' must satisfy the following laws:
--
-- @
-- pack . unpack = 'id'
-- unpack . pack = 'id'
-- @
class Pack a where
  type SignalP a
  type SignalP a = Signal a
  -- | Example:
  --
  -- > pack :: (Signal a, Signal b) -> Signal (a,b)
  --
  -- However:
  --
  -- > pack :: Signal Bit -> Signal Bit
  pack   :: SignalP a -> Signal a

  default pack :: Signal a -> Signal a
  pack s = s
  -- | Example:
  --
  -- > unpack :: Signal (a,b) -> (Signal a, Signal b)
  --
  -- However:
  --
  -- > unpack :: Signal Bit -> Signal Bit
  unpack :: Signal a -> SignalP a

  default unpack :: Signal a -> Signal a
  unpack s = s

instance Pack Bit
instance Pack (Signed n)
instance Pack (Unsigned n)
instance Pack (Fixed frac rep size)
instance Pack Bool
instance Pack Integer
instance Pack Int
instance Pack Float
instance Pack Double
instance Pack ()
instance Pack (Maybe a)
instance Pack (Either a b)

instance Pack (a,b) where
  type SignalP (a,b) = (Signal a, Signal b)
  pack       = uncurry (liftA2 (,))
  unpack tup = (fmap fst tup, fmap snd tup)

instance Pack (a,b,c) where
  type SignalP (a,b,c) = (Signal a, Signal b, Signal c)
  pack (a,b,c) = (,,) <$> a <*> b <*> c
  unpack tup   = (fmap (\(x,_,_) -> x) tup
                 ,fmap (\(_,x,_) -> x) tup
                 ,fmap (\(_,_,x) -> x) tup
                 )

instance Pack (a,b,c,d) where
  type SignalP (a,b,c,d) = (Signal a, Signal b, Signal c, Signal d)
  pack (a,b,c,d) = (,,,) <$> a <*> b <*> c <*> d
  unpack tup     = (fmap (\(x,_,_,_) -> x) tup
                   ,fmap (\(_,x,_,_) -> x) tup
                   ,fmap (\(_,_,x,_) -> x) tup
                   ,fmap (\(_,_,_,x) -> x) tup
                   )

instance Pack (a,b,c,d,e) where
  type SignalP (a,b,c,d,e) = (Signal a, Signal b, Signal c, Signal d, Signal e)
  pack (a,b,c,d,e) = (,,,,) <$> a <*> b <*> c <*> d <*> e
  unpack tup       = (fmap (\(x,_,_,_,_) -> x) tup
                     ,fmap (\(_,x,_,_,_) -> x) tup
                     ,fmap (\(_,_,x,_,_) -> x) tup
                     ,fmap (\(_,_,_,x,_) -> x) tup
                     ,fmap (\(_,_,_,_,x) -> x) tup
                     )

instance Pack (a,b,c,d,e,f) where
  type SignalP (a,b,c,d,e,f) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f)
  pack (a,b,c,d,e,f) = (,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f
  unpack tup         = (fmap (\(x,_,_,_,_,_) -> x) tup
                       ,fmap (\(_,x,_,_,_,_) -> x) tup
                       ,fmap (\(_,_,x,_,_,_) -> x) tup
                       ,fmap (\(_,_,_,x,_,_) -> x) tup
                       ,fmap (\(_,_,_,_,x,_) -> x) tup
                       ,fmap (\(_,_,_,_,_,x) -> x) tup
                       )

instance Pack (a,b,c,d,e,f,g) where
  type SignalP (a,b,c,d,e,f,g) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g)
  pack (a,b,c,d,e,f,g) = (,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g
  unpack tup           = (fmap (\(x,_,_,_,_,_,_) -> x) tup
                         ,fmap (\(_,x,_,_,_,_,_) -> x) tup
                         ,fmap (\(_,_,x,_,_,_,_) -> x) tup
                         ,fmap (\(_,_,_,x,_,_,_) -> x) tup
                         ,fmap (\(_,_,_,_,x,_,_) -> x) tup
                         ,fmap (\(_,_,_,_,_,x,_) -> x) tup
                         ,fmap (\(_,_,_,_,_,_,x) -> x) tup
                         )

instance Pack (a,b,c,d,e,f,g,h) where
  type SignalP (a,b,c,d,e,f,g,h) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h)
  pack (a,b,c,d,e,f,g,h) = (,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h
  unpack tup             = (fmap (\(x,_,_,_,_,_,_,_) -> x) tup
                           ,fmap (\(_,x,_,_,_,_,_,_) -> x) tup
                           ,fmap (\(_,_,x,_,_,_,_,_) -> x) tup
                           ,fmap (\(_,_,_,x,_,_,_,_) -> x) tup
                           ,fmap (\(_,_,_,_,x,_,_,_) -> x) tup
                           ,fmap (\(_,_,_,_,_,x,_,_) -> x) tup
                           ,fmap (\(_,_,_,_,_,_,x,_) -> x) tup
                           ,fmap (\(_,_,_,_,_,_,_,x) -> x) tup
                           )

instance Pack (Vec n a) where
  type SignalP (Vec n a) = Vec n (Signal a)
  pack vs                = vmap shead vs :- pack (vmap stail vs)
  unpack (Nil :- _)         = Nil
  unpack vs@((_ :> _) :- _) = fmap vhead vs :> (unpack (fmap vtail vs))

-- | Simulate a (@'SignalP' a -> 'SignalP' b@) function given a list of samples
-- of type @a@
--
-- >>> simulateP (unpack . register (8,8) . pack) [(1,1), (2,2), (3,3), ...
-- [(8,8), (1,1), (2,2), (3,3), ...
simulateP :: (Pack a, Pack b) => (SignalP a -> SignalP b) -> [a] -> [b]
simulateP f = simulate (pack . f . unpack)

-- | Operator lifting, use in conjunction with ('^>')
--
-- > add2 :: Signal Int -> Signal Int
-- > add2 x = x <^(+)^> (signal 2)
--
-- >>> simulate add2 [1,2,3,...
-- [3,4,5,...
(<^) :: Applicative f => f a -> (a -> b -> c) -> f b -> f c
v <^ f = liftA2 f v

-- | Operator lifting, use in conjunction with ('<^')
--
-- > add2 :: Signal Int -> Signal Int
-- > add2 x = x <^(+)^> (signal 2)
--
-- >>> simulate add2 [1,2,3,...
-- [3,4,5,...
(^>) :: Applicative f => (f a -> f b) -> f a -> f b
f ^> v = f v
