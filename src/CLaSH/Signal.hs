{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module CLaSH.Signal
  ( Signal
  , fromList
  , signal
  , sample
  , register
  , simulate
  , Pack(..)
  , simulateP
  , (<^), (^>)
  )
where

import Control.Applicative
import Language.Haskell.TH.Syntax(Lift(..))

import CLaSH.Class.Default
import CLaSH.Bit            (Bit)
import CLaSH.Sized.Signed   (Signed)
import CLaSH.Sized.Unsigned (Unsigned)
import CLaSH.Sized.Vector   (Vec(..), vmap, vhead, vtail)

{-# NOINLINE register  #-}
{-# NOINLINE signal    #-}
{-# NOINLINE mapSignal #-}
{-# NOINLINE appSignal #-}

infixr 5 :-
data Signal a = a :- Signal a

fromList :: [a] -> Signal a
fromList []     = error "finite list"
fromList (x:xs) = x :- fromList xs

instance Show a => Show (Signal a) where
  show (x :- xs) = show x ++ " " ++ show xs

instance Lift a => Lift (Signal a) where
  lift ~(x :- _) = [| signal x |]

instance Default a => Default (Signal a) where
  def = signal def

sample :: Int -> Signal a -> [a]
sample 0 _         = []
sample n ~(x :- xs) = x : (sample (n-1) xs)

signal :: a -> Signal a
signal a = a :- signal a

mapSignal :: (a -> b) -> Signal a -> Signal b
mapSignal f (a :- as) = f a :- mapSignal f as

appSignal :: Signal (a -> b) -> Signal a -> Signal b
appSignal (f :- fs) ~(a :- as) = f a :- appSignal fs as

instance Functor Signal where
  fmap = mapSignal

instance Applicative Signal where
  pure  = signal
  (<*>) = appSignal

unSignal :: Signal a -> a
unSignal (a :- _) = a

next :: Signal a -> Signal a
next (_ :- as) = as

diag :: Signal (Signal a) -> Signal a
diag (xs :- xss) = unSignal xs :- diag (fmap next xss)

instance Monad Signal where
  return    = signal
  xs >>= f  = diag (fmap f xs)

register :: a -> Signal a -> Signal a
register i s = i :- s

simulate :: (Signal a -> Signal b) -> [a] -> [b]
simulate f as = sample (length as) (f (fromList as))

class Pack a where
  type SignalP a
  pack   :: SignalP a -> Signal a
  unpack :: Signal a -> SignalP a

simulateP :: (Pack a, Pack b) => (SignalP a -> SignalP b) -> [a] -> [b]
simulateP f = simulate (pack . f . unpack)

instance Pack Bit where
  type SignalP Bit = Signal Bit
  pack   = id
  unpack = id

instance Pack (Signed n) where
  type SignalP (Signed n) = Signal (Signed n)
  pack   = id
  unpack = id

instance Pack (Unsigned n) where
  type SignalP (Unsigned n) = Signal (Unsigned n)
  pack   = id
  unpack = id

instance Pack Bool where
  type SignalP Bool = Signal Bool
  pack   = id
  unpack = id

instance Pack Integer where
  type SignalP Integer = Signal Integer
  pack   = id
  unpack = id

instance Pack Int where
  type SignalP Int = Signal Int
  pack   = id
  unpack = id

instance Pack Float where
  type SignalP Float = Signal Float
  pack   = id
  unpack = id

instance Pack Double where
  type SignalP Double = Signal Double
  pack   = id
  unpack = id

instance Pack () where
  type SignalP () = Signal ()
  pack   = id
  unpack = id

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
  pack vs                = vmap unSignal vs :- pack (vmap next vs)
  unpack (Nil :- _)         = Nil
  unpack vs@((_ :> _) :- _) = fmap vhead vs :> (unpack (fmap vtail vs))

(<^) :: Applicative f => f a -> (a -> b -> c) -> f b -> f c
v <^ f = liftA2 f v

(^>) :: Applicative f => (f a -> f b) -> f a -> f b
f ^> v = f v

instance Num a => Num (Signal a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = signal . fromInteger
