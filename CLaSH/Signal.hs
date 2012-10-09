-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE UndecidableInstances #-}
module CLaSH.Signal
  ( Sync (..), sync
  , sample
  , register
  , Split(..)
  , (<^>)
  , (<^), (^>)
  , Comp
  , (^^^)
  , registerC
  )
where

import Data.List
import Control.Applicative
import Control.Arrow
import Control.Category
import Prelude hiding (id,(.))

import CLaSH.Sized.VectorZ
import Debug.Trace

infixr 5 :-
data Sync a = a :- Sync a

toList :: Sync a -> [a]
toList (x :- xs) = x : toList xs

fromList :: [a] -> Sync a
fromList []     = error "finite list"
fromList (x:xs) = x :- fromList xs

instance Show a => Show (Sync a) where
  show (x :- xs) = show x ++ " " ++ show xs

sample :: Integer -> Sync a -> [a]
sample 0 _         = []
sample n ~(x :- xs) = x : (sample (n-1) xs)

sync :: a -> Sync a
sync a = a :- sync a

instance Functor Sync where
  fmap f (a :- as)  = f a :- fmap f as

instance Applicative Sync where
  pure                     = sync
  (f :- fs) <*> (a :- as)  = f a :- (fs <*> as)

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

class Split a where
  type SplitSync a
  combine :: SplitSync a -> Sync a
  split   :: Sync a -> SplitSync a

instance Split (a,b) where
  type SplitSync (a,b) = (Sync a, Sync b)
  combine  = uncurry (liftA2 (,))
  split ab = (fmap fst ab, fmap snd ab)

instance Split (Vec n a) where
  type SplitSync (Vec n a) = Vec n (Sync a)
  combine vs                = vmap unSync vs :- combine (vmap next vs)
  split (Nil :- _)          = Nil
  split vs@((_ :> _) :- _)  = fmap vhead vs :> (split (fmap vtail vs))

(<^>) ::
  Split i => Split o
  => (s -> i -> (s,o))
  -> s
  -> (SplitSync i -> SplitSync o)
f <^> iS = \i -> let (s',o) = split $ f <$> s <*> (combine i)
                     s      = register iS s'
                 in split o

(<^) :: Applicative f => f a -> (a -> b -> c) -> f b -> f c
v <^ f = liftA2 f v

(^>) :: Applicative f => (f a -> f b) -> f a -> f b
f ^> v = f v

newtype Comp a b = C (Sync a -> Sync b)

instance Category Comp where
  id            = C id
  (C f) . (C g) = C (f . g)

infixr 8 ><
(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(f >< g) (x,y) = (f x,g y)

instance Arrow Comp where
  arr         = C . fmap
  first (C f) = C $ combine . (f >< id) . split

instance ArrowLoop Comp where
  loop (C f) = C $ simpleLoop (split . f . combine)
    where
      simpleLoop g b = let ~(c,d) = g (b,d)
                       in c

(^^^) :: (s -> i -> (s,o)) -> s -> Comp i o
f ^^^ sI = C $ \i -> let (s',o) = split $ f <$> s <*> i
                         s      = register sI s'
                     in  o

registerC :: a -> Comp a a
registerC = C . register

-- class Idiomatic f g | g -> f where
--   idiomatic :: Sync f -> g
--
-- instance Idiomatic a (Sync a) where
--   idiomatic = id
--
-- instance Idiomatic f g => Idiomatic (s -> f) (Sync s -> g) where
--   idiomatic sfi si = idiomatic (sfi <*> si)
--
-- lift :: Idiomatic f g => f -> g
-- lift f = idiomatic (pure f)

instance Num a => Num (Sync a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = sync . fromInteger
