{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module CLaSH.Signal
  ( Sync (..), fromList, sync
  , sample
  , register
  , Pack(..)
  , registerP
  , (<^>)
  , (<^), (^>)
  , Comp(..)
  , (^^^)
  , registerC
  )
where

import Data.List
import Control.Applicative
import Control.Arrow
import Control.Category
import Language.Haskell.TH.Syntax(Lift(..))
import Prelude hiding (id,(.))

import CLaSH.Class.Default
import CLaSH.Sized.VectorZ (Vec(..), vmap, vhead, vtail)

{-# NOINLINE register  #-}
{-# NOINLINE registerP #-}
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
appSync (f :- fs) (a :- as) = f a :- appSync fs as

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
  combine :: Packed a -> Sync a
  split   :: Sync a -> Packed a

registerP :: Pack a => a -> Packed a -> Packed a
registerP i = split . register i . combine

instance Pack (a,b) where
  type Packed (a,b) = (Sync a, Sync b)
  combine  = uncurry (liftA2 (,))
  split ab = (fmap fst ab, fmap snd ab)

instance Pack (Vec n a) where
  type Packed (Vec n a) = Vec n (Sync a)
  combine vs                = vmap unSync vs :- combine (vmap next vs)
  split (Nil :- _)          = Nil
  split vs@((_ :> _) :- _)  = fmap vhead vs :> (split (fmap vtail vs))

(<^>) ::
  (Pack i, Pack o)
  => (s -> i -> (s,o))
  -> s
  -> (Packed i -> Packed o)
f <^> iS = \i -> let (s',o) = split $ f <$> s <*> (combine i)
                     s      = register iS s'
                 in split o

(<^) :: Applicative f => f a -> (a -> b -> c) -> f b -> f c
v <^ f = liftA2 f v

(^>) :: Applicative f => (f a -> f b) -> f a -> f b
f ^> v = f v

newtype Comp a b = C { asFunction :: Sync a -> Sync b }

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

instance Num a => Num (Sync a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = sync . fromInteger
