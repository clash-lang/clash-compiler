{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module CLaSH.Signal
  ( Signal, fromList, signal
  , sample
  , register
  , Pack(..)
  , (<^), (^>)
  )
where

import Control.Applicative
import Language.Haskell.TH.Syntax(Lift(..))

import CLaSH.Class.Default
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

sample :: Integer -> Signal a -> [a]
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

class Pack a where
  type Packed a
  combine :: Packed a -> Signal a
  split   :: Signal a -> Packed a

instance Pack (Signed n) where
  type Packed (Signed n) = Signal (Signed n)
  combine = id
  split   = id

instance Pack (Unsigned n) where
  type Packed (Unsigned n) = Signal (Unsigned n)
  combine = id
  split   = id

instance Pack Bool where
  type Packed Bool = Signal Bool
  combine = id
  split   = id

instance Pack Integer where
  type Packed Integer = Signal Integer
  combine = id
  split   = id

instance Pack Int where
  type Packed Int = Signal Int
  combine = id
  split   = id

instance Pack () where
  type Packed () = Signal ()
  combine = id
  split   = id

instance Pack (a,b) where
  type Packed (a,b) = (Signal a, Signal b)
  combine  = uncurry (liftA2 (,))
  split ab = (fmap fst ab, fmap snd ab)

instance Pack (a,b,c) where
  type Packed (a,b,c) = (Signal a, Signal b, Signal c)
  combine (a,b,c) = (,,) <$> a <*> b <*> c
  split abc       = (fmap (\(x,_,_) -> x) abc
                    ,fmap (\(_,x,_) -> x) abc
                    ,fmap (\(_,_,x) -> x) abc
                    )

instance Pack (a,b,c,d) where
  type Packed (a,b,c,d) = (Signal a, Signal b, Signal c, Signal d)
  combine (a,b,c,d) = (,,,) <$> a <*> b <*> c <*> d
  split abcd        = (fmap (\(x,_,_,_) -> x) abcd
                      ,fmap (\(_,x,_,_) -> x) abcd
                      ,fmap (\(_,_,x,_) -> x) abcd
                      ,fmap (\(_,_,_,x) -> x) abcd
                      )

instance Pack (Vec n a) where
  type Packed (Vec n a) = Vec n (Signal a)
  combine vs                = vmap unSignal vs :- combine (vmap next vs)
  split (Nil :- _)          = Nil
  split vs@((_ :> _) :- _)  = fmap vhead vs :> (split (fmap vtail vs))

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
