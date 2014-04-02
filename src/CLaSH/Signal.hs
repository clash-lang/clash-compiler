{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module CLaSH.Signal
  ( Signal
  , Clock (..)
  , sample
  , sampleN
  , fromList
  , signal
  , register
  , simulate
  , Pack(..)
  , simulateP
  , (<^), (^>)
  )
where

import Data.Default               (Default (..))
import Control.Applicative        (Applicative (..), (<$>), liftA2)
import GHC.TypeLits               (Nat)
import Language.Haskell.TH.Syntax (Lift(..))

import CLaSH.Bit                  (Bit)
import CLaSH.Promoted.Nat         (SNat)
import CLaSH.Sized.Fixed          (Fixed)
import CLaSH.Sized.Signed         (Signed)
import CLaSH.Sized.Unsigned       (Unsigned)
import CLaSH.Sized.Vector         (Vec(..), vmap, vhead, vtail)

{-# NOINLINE register  #-}
{-# NOINLINE signal    #-}
{-# NOINLINE mapSignal #-}
{-# NOINLINE appSignal #-}

infixr 5 :-
-- | A synchronized signal with elements of type @a@, synchronized to the
-- relative clock @clk@
data Signal (clk :: Nat) a = a :- Signal clk a

-- | Explicit Clock with relative period @clk@
newtype Clock (clk :: Nat) = Clock (SNat clk)

-- | Create a 'Signal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> sampleN 2 (fromList [1,2,3,4,5])
-- [1,2]
fromList :: [a] -> Signal t a
fromList []     = error "finite list"
fromList (x:xs) = x :- fromList xs

instance Show a => Show (Signal t a) where
  show (x :- xs) = show x ++ " " ++ show xs

instance Lift a => Lift (Signal t a) where
  lift ~(x :- _) = [| signal x |]

instance Default a => Default (Signal t a) where
  def = signal def

-- | Get an infinite list of samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal' at
-- consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
sample :: Signal t a -> [a]
sample ~(x :- xs) = x : sample xs

-- | Get a list of @n@ samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal' at
-- consecutive clock cycles
--
-- > sampleN 3 s == [s0, s1, s2]
sampleN :: Int -> Signal t a -> [a]
sampleN 0 _          = []
sampleN n ~(x :- xs) = x : (sampleN (n-1) xs)

-- | Create a constant 'Signal' from a combinational value
--
-- >>> sample (signal 4)
-- [4, 4, 4, 4, ...
signal :: a -> Signal t a
signal a = a :- signal a

mapSignal :: (a -> b) -> Signal t a -> Signal t b
mapSignal f (a :- as) = f a :- mapSignal f as

appSignal :: Signal t (a -> b) -> Signal t a -> Signal t b
appSignal (f :- fs) ~(a :- as) = f a :- appSignal fs as

instance Functor (Signal t) where
  fmap = mapSignal

instance Applicative (Signal t) where
  pure  = signal
  (<*>) = appSignal

unSignal :: Signal t a -> a
unSignal (a :- _) = a

next :: Signal t a -> Signal t a
next (_ :- as) = as

diag :: Signal t (Signal t a) -> Signal t a
diag (xs :- xss) = unSignal xs :- diag (fmap next xss)

instance Monad (Signal t) where
  return    = signal
  xs >>= f  = diag (fmap f xs)

-- | 'register' @i s@ delays the values in 'Signal' @s@ for one cycle, and sets
-- the value at time 0 to @i@
--
-- >>> sampleN 3 (register 8 (fromList [1,2,3,4]))
-- [8,1,2]
register :: a -> Signal t a -> Signal t a
register i s = i :- s

-- | Simulate a ('Signal' -> 'Signal') function given a list of samples
--
-- >>> simulate (register 8) [1, 2, 3, ...
-- [8, 1, 2, 3, ...
simulate :: (Signal t a -> Signal t b) -> [a] -> [b]
simulate f = sample . f . fromList

-- | Conversion between a 'Signal' of a product type (e.g. a tuple) and a
-- product type of 'Signal's
class Pack a where
  type SignalP (clk :: Nat) a
  type SignalP clk a = Signal clk a
  -- | > pack :: (Signal a, Signal b) -> Signal (a,b)
  -- However:
  --
  -- > pack :: Signal Bit -> Signal Bit
  pack   :: Clock clk -> SignalP clk a -> Signal clk a
  -- | > unpack :: Signal (a,b) -> (Signal a, Signal b)
  -- However:
  --
  -- > unpack :: Signal Bit -> Signal Bit
  unpack :: Clock clk -> Signal clk a -> SignalP clk a

-- | Simulate a ('SignalP' -> 'SignalP') function given a list of samples
--
-- >>> simulateP (unpack . register (8,8) . pack) [(1,1), (2,2), (3,3), ...
-- [(8,8), (1,1), (2,2), (3,3), ...
simulateP :: (Pack a, Pack b) => Clock t -> (SignalP t a -> SignalP t b) -> [a] -> [b]
simulateP clk f = simulate (pack clk . f . unpack clk)

instance Pack Bit where
  pack   _ = id
  unpack _ = id

instance Pack (Signed n) where
  pack   _ = id
  unpack _ = id

instance Pack (Unsigned n) where
  pack   _ = id
  unpack _ = id

instance Pack (Fixed frac rep size) where
  pack   _ = id
  unpack _ = id

instance Pack Bool where
  pack   _ = id
  unpack _ = id

instance Pack Integer where
  pack   _ = id
  unpack _ = id

instance Pack Int where
  pack   _ = id
  unpack _ = id

instance Pack Float where
  pack   _ = id
  unpack _ = id

instance Pack Double where
  pack   _ = id
  unpack _ = id

instance Pack () where
  pack   _ = id
  unpack _ = id

instance Pack (a,b) where
  type SignalP t (a,b) = (Signal t a, Signal t b)
  pack _       = uncurry (liftA2 (,))
  unpack _ tup = (fmap fst tup, fmap snd tup)

instance Pack (a,b,c) where
  type SignalP t (a,b,c) = (Signal t a, Signal t b, Signal t c)
  pack   _ (a,b,c) = (,,) <$> a <*> b <*> c
  unpack _ tup     = (fmap (\(x,_,_) -> x) tup
                     ,fmap (\(_,x,_) -> x) tup
                     ,fmap (\(_,_,x) -> x) tup
                     )

instance Pack (a,b,c,d) where
  type SignalP t (a,b,c,d) = (Signal t a, Signal t b, Signal t c, Signal t d)
  pack   _ (a,b,c,d) = (,,,) <$> a <*> b <*> c <*> d
  unpack _ tup       = (fmap (\(x,_,_,_) -> x) tup
                       ,fmap (\(_,x,_,_) -> x) tup
                       ,fmap (\(_,_,x,_) -> x) tup
                       ,fmap (\(_,_,_,x) -> x) tup
                       )

instance Pack (a,b,c,d,e) where
  type SignalP t (a,b,c,d,e) = (Signal t a, Signal t b, Signal t c, Signal t d, Signal t e)
  pack _ (a,b,c,d,e) = (,,,,) <$> a <*> b <*> c <*> d <*> e
  unpack _ tup       = (fmap (\(x,_,_,_,_) -> x) tup
                       ,fmap (\(_,x,_,_,_) -> x) tup
                       ,fmap (\(_,_,x,_,_) -> x) tup
                       ,fmap (\(_,_,_,x,_) -> x) tup
                       ,fmap (\(_,_,_,_,x) -> x) tup
                       )

instance Pack (a,b,c,d,e,f) where
  type SignalP t (a,b,c,d,e,f) = (Signal t a, Signal t b, Signal t c, Signal t d, Signal t e, Signal t f)
  pack   _ (a,b,c,d,e,f) = (,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f
  unpack _ tup           = (fmap (\(x,_,_,_,_,_) -> x) tup
                         ,fmap (\(_,x,_,_,_,_) -> x) tup
                         ,fmap (\(_,_,x,_,_,_) -> x) tup
                         ,fmap (\(_,_,_,x,_,_) -> x) tup
                         ,fmap (\(_,_,_,_,x,_) -> x) tup
                         ,fmap (\(_,_,_,_,_,x) -> x) tup
                         )

instance Pack (a,b,c,d,e,f,g) where
  type SignalP t (a,b,c,d,e,f,g) = (Signal t a, Signal t b, Signal t c, Signal t d, Signal t e, Signal t f, Signal t g)
  pack   _ (a,b,c,d,e,f,g) = (,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g
  unpack _ tup             = (fmap (\(x,_,_,_,_,_,_) -> x) tup
                             ,fmap (\(_,x,_,_,_,_,_) -> x) tup
                             ,fmap (\(_,_,x,_,_,_,_) -> x) tup
                             ,fmap (\(_,_,_,x,_,_,_) -> x) tup
                             ,fmap (\(_,_,_,_,x,_,_) -> x) tup
                             ,fmap (\(_,_,_,_,_,x,_) -> x) tup
                             ,fmap (\(_,_,_,_,_,_,x) -> x) tup
                             )

instance Pack (a,b,c,d,e,f,g,h) where
  type SignalP t (a,b,c,d,e,f,g,h) = (Signal t a, Signal t b, Signal t c, Signal t d, Signal t e, Signal t f, Signal t g, Signal t h)
  pack   _ (a,b,c,d,e,f,g,h) = (,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h
  unpack _ tup               = (fmap (\(x,_,_,_,_,_,_,_) -> x) tup
                               ,fmap (\(_,x,_,_,_,_,_,_) -> x) tup
                               ,fmap (\(_,_,x,_,_,_,_,_) -> x) tup
                               ,fmap (\(_,_,_,x,_,_,_,_) -> x) tup
                               ,fmap (\(_,_,_,_,x,_,_,_) -> x) tup
                               ,fmap (\(_,_,_,_,_,x,_,_) -> x) tup
                               ,fmap (\(_,_,_,_,_,_,x,_) -> x) tup
                               ,fmap (\(_,_,_,_,_,_,_,x) -> x) tup
                               )

instance Pack (Vec n a) where
  type SignalP t (Vec n a) = Vec n (Signal t a)
  pack   clk vs                 = vmap unSignal vs :- pack clk (vmap next vs)
  unpack _   (Nil :- _)         = Nil
  unpack clk vs@((_ :> _) :- _) = fmap vhead vs :> (unpack clk (fmap vtail vs))

-- | Operator lifting, use in conjunction with '(^>)'
--
-- > add2 :: Signal Int -> Signal Int
-- > add2 x = x <^(+)^> (signal 2)
--
-- >>> simulate add2 [1,2,3,...
-- [3,4,5,...
(<^) :: Applicative f => f a -> (a -> b -> c) -> f b -> f c
v <^ f = liftA2 f v

-- | Operator lifting, use in conjunction with '(<^)'
--
-- > add2 :: Signal Int -> Signal Int
-- > add2 x = x <^(+)^> (signal 2)
--
-- >>> simulate add2 [1,2,3,...
-- [3,4,5,...
(^>) :: Applicative f => (f a -> f b) -> f a -> f b
f ^> v = f v

instance Num a => Num (Signal t a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = signal . fromInteger
