{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

{-# OPTIONS_GHC -fno-expose-all-unfoldings #-}

module CLaSH.Signal
  ( Sync, fromList, sync
  , sample
  , register
  , Pack(..)
  , (<^), (^>)
  )
where

import Control.Applicative
import Language.Haskell.TH.Syntax(Lift(..))
import Unsafe.Coerce

import CLaSH.Class.Default
import CLaSH.Sized.Signed   (Signed)
import CLaSH.Sized.Unsigned (Unsigned)
import CLaSH.Sized.VectorZ  (Vec(..), vmap, vhead, vtail)

{-# NOINLINE register  #-}
{-# NOINLINE sync      #-}
{-# NOINLINE mapSync   #-}
{-# NOINLINE appSync   #-}

infixr 5 :-
data Sync a = a :- Sync a

fromList :: [a] -> Sync a
fromList []     = error "finite list"
fromList (x:xs) = x :- fromList xs

instance Show a => Show (Sync a) where
  show (x :- xs) = show x ++ " " ++ show xs

instance Lift a => Lift (Sync a) where
  lift ~(x :- _) = [| sync x |]

instance Default a => Default (Sync a) where
  def = sync def

sample :: Integer -> Sync a -> [a]
sample 0 _         = []
sample n ~(x :- xs) = x : (sample (n-1) xs)

sync :: a -> Sync a
sync a = a :- sync a

mapSync :: (a -> b) -> Sync a -> Sync b
mapSync f (a :- as) = f a :- mapSync f as

appSync :: Sync (a -> b) -> Sync a -> Sync b
appSync (f :- fs) ~(a :- as) = f a :- appSync fs as

instance Functor Sync where
  fmap = mapSync

instance Applicative Sync where
  pure  = sync
  (<*>) = appSync

unSync :: Sync a -> a
unSync (a :- _) = a

next :: Sync a -> Sync a
next (_ :- as) = as

diag :: Sync (Sync a) -> Sync a
diag (xs :- xss) = unSync xs :- diag (fmap next xss)

instance Monad Sync where
  return    = sync
  xs >>= f  = diag (fmap f xs)

register :: a -> Sync a -> Sync a
register i s = i :- s

class Pack a where
  type Packed a
  type Packed a = Sync a
  {-# NOINLINE combine #-}
  combine :: Packed a -> Sync a
  combine = unsafeCoerce
  {-# NOINLINE split #-}
  split :: Sync a -> Packed a
  split = unsafeCoerce

instance Pack (Signed n)
instance Pack (Unsigned n)
instance Pack Bool
instance Pack Integer
instance Pack ()

instance Pack (a,b) where
  type Packed (a,b) = (Sync a, Sync b)
  combine  = uncurry (liftA2 (,))
  split ab = (fmap fst ab, fmap snd ab)

instance Pack (a,b,c) where
  type Packed (a,b,c) = (Sync a, Sync b, Sync c)
  combine (a,b,c) = (,,) <$> a <*> b <*> c
  split abc       = (fmap (\(x,_,_) -> x) abc
                    ,fmap (\(_,x,_) -> x) abc
                    ,fmap (\(_,_,x) -> x) abc
                    )

instance Pack (a,b,c,d) where
  type Packed (a,b,c,d) = (Sync a, Sync b, Sync c, Sync d)
  combine (a,b,c,d) = (,,,) <$> a <*> b <*> c <*> d
  split abcd        = (fmap (\(x,_,_,_) -> x) abcd
                      ,fmap (\(_,x,_,_) -> x) abcd
                      ,fmap (\(_,_,x,_) -> x) abcd
                      ,fmap (\(_,_,_,x) -> x) abcd
                      )

instance Pack (Vec n a) where
  type Packed (Vec n a) = Vec n (Sync a)
  combine vs                = vmap unSync vs :- combine (vmap next vs)
  split (Nil :- _)          = Nil
  split vs@((_ :> _) :- _)  = fmap vhead vs :> (split (fmap vtail vs))

(<^) :: Applicative f => f a -> (a -> b -> c) -> f b -> f c
v <^ f = liftA2 f v

(^>) :: Applicative f => (f a -> f b) -> f a -> f b
f ^> v = f v

instance Num a => Num (Sync a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = sync . fromInteger
