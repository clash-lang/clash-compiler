{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module CLaSH.Sized.Vector
  ( Vec(..), (<:)
  , vhead, vtail, vinit, vlast
  , (<++>), (+>>), (<<+), vconcat
  , vmap, vzipWith, vfoldl, vfoldr
  , vindex, vindexM
  )
where

import Unsafe.Coerce(unsafeCoerce)
import GHC.TypeLits

data Vec :: Nat -> * -> * where
  One  :: a -> Vec 1 a
  (:>) :: a -> Vec (n + 1) a ->  Vec (n + 2) a

infixr 5 :>

instance Show a => Show (Vec n a) where
  show vs = "<" ++ punc vs ++ ">"
    where
      punc :: Show a => Vec n a -> String
      punc (One x)   = show x
      punc (x :> xs) = show x ++ "," ++ punc xs

vhead :: Vec n a -> a
vhead (One x)   = x
vhead (x :> xs) = x

vtail :: Vec (n + 2) a -> Vec (n + 1) a
vtail (x :> xs) = xs

vlast :: Vec n a -> a
vlast (One x)   = x
vlast (x :> xs) = vlast xs

vinit :: Vec (n + 2) a -> Vec (n + 1) a
vinit (x :> One _)   = One x
vinit (x :> y :> ys) = x :> vinit (y :> ys)

vmiddle :: Vec (n + 1 + 2) a -> Vec (n + 1) a
vmiddle = vinit . vtail

vappend :: Vec (n + 1) a -> Vec (m + 1) a -> Vec (n + m + 1 + 1) a
vappend (One x)   ys = x :> ys
vappend (x :> xs) ys = x :> (vappend xs ys)

infixr 5 <++>
(<++>) :: Vec (n + 1) a -> Vec (m + 1) a -> Vec (n + m + 1 + 1) a
xs <++> ys = vappend xs ys

snoc :: a -> Vec (n + 1) a -> Vec (n + 2) a
snoc s (One x)   = x :> One s
snoc s (x :> xs) = x :> (snoc s xs)

infixl 5 <:
(<:) :: Vec (n + 1) a -> a -> Vec (n + 2) a
xs <: s = snoc s xs

vconcat :: Vec m (Vec n c) -> Vec (m * n) c
vconcat (One x)   = x
vconcat (x :> xs) = unsafeCoerce
                  $ vappend (unsafeCoerce x)
                  $ unsafeCoerce (vconcat (unsafeCoerce xs))

shiftIntoL :: a -> Vec (n + 1) a -> Vec (n + 1) a
shiftIntoL s (One _)   = One s
shiftIntoL s (x :> xs) = s :> vinit (x :> xs)

infixr 4 +>>
(+>>) :: a -> Vec (n + 1) a -> Vec (n + 1) a
s +>> xs = shiftIntoL s xs

shiftIntoR :: a -> Vec (n + 1) a -> Vec (n + 1) a
shiftIntoR s (One _)   = One s
shiftIntoR s (x :> xs) = (vtail (x :> xs)) <: s

infixl 4 <<+
(<<+) :: Vec (n + 1) a -> a -> Vec (n + 1) a
xs <<+ s = shiftIntoR s xs

vmap :: (a -> b) -> Vec n a -> Vec n b
vmap f (One x)   = One (f x)
vmap f (x :> xs) = (f x) :> vmap f xs

vzipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vzipWith f (One x) (One y)     = One (f x y)
vzipWith f (x :> xs) (y :> ys) = f x y :> vzipWith f xs ys

vfoldr :: (a -> b -> b) -> b -> Vec n a -> b
vfoldr f z (One x)   = f x z
vfoldr f z (x :> xs) = f x (vfoldr f z xs)

vfoldl :: (a -> b -> a) -> a -> Vec n b -> a
vfoldl f z (One x)   = f z x
vfoldl f z (x :> xs) = vfoldl f (f z x) xs

vindexM :: (Num i, Eq i) => Vec n a -> i -> Maybe a
vindexM (One x)   0 = Just x
vindexM (One x)   _ = Nothing
vindexM (x :> _)  0 = Just x
vindexM (_ :> xs) n = vindexM xs (n-1)

vindex :: (Num i, Eq i) => Vec n a -> i -> a
vindex xs i = case vindexM xs i of
  Just a  -> a
  Nothing -> error "index out of bounds"
