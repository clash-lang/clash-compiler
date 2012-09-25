{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module CLaSH.Sized.Vector where

import GHC.TypeLits

import CLaSH.Sized.Index

-- Would like to write:
-- data Vector :: Nat -> * -> * where
--   -- Can't because of the 'snoc' function
--   One  :: a -> Vector 1 a
--   -- Can't because of the 'append' function
--   (:>) :: (1 <= n) => a -> Vector n a -> Vector (n + 1) a

data Vector :: Nat -> * -> * where
 One  :: a -> Vector (0 + 1) a
 -- Type checker hangs on 'v3' when defined as:
 -- (:>) :: a -> Vector (n + 1) a -> Vector (n + 2) a
 (:>) :: a -> Vector (n + 1) a -> Vector (n + 1 + 1) a

infixr 5 :>

instance Show a => Show (Vector n a) where
  show (One x)   = show x
  show (x :> xs) = show x ++ " " ++ show xs

v1 :: Vector 1 Double
v1 = One 1.0

v2 :: Vector 2 Double
v2 = 1.0 :> One 2.0

v3 :: Vector 3 Double
v3 = 1.0 :> 2.0 :> One 3.0

vhead :: Vector n a -> a
vhead (One x)   = x
vhead (x :> xs) = x

vtail :: (1 <= n) => Vector (n + 1) a -> Vector n a
vtail (x :> xs) = xs

vlast :: Vector n a -> a
vlast (One x)   = x
vlast (x :> xs) = vlast xs

vinit :: (1 <= n) => Vector (n + 1) a -> Vector n a
vinit (x :> One _)   = One x
vinit (x :> y :> ys) = x :> vinit (y :> ys)

-- Would like to write:
-- vmiddle :: (1 <= n) => Vector (n + 2) a -> Vector n a
--
-- But I get the following error:
-- CLaSH/Sized/Vector.hs:63:19:
--     Overlapping instances for 1 <= (n + 1)
--       arising from a use of `vtail'
--     Matching instances:
--       instance (m <=? n) ~ 'True => m <= n -- Defined in `GHC.TypeLits'
--     There exists a (perhaps superclass) match:
--       from the context (1 <= n)
--         bound by the type signature for
--                    vmiddle :: 1 <= n => Vector (n + 2) a -> Vector n a
--         at CLaSH/Sized/Vector.hs:62:12-53
--     (The choice depends on the instantiation of `n'
--      To pick the first instance above, use -XIncoherentInstances
--      when compiling the other instance declarations)
--     In the second argument of `(.)', namely `vtail'
--     In the expression: vinit . vtail
--     In an equation for `vmiddle': vmiddle = vinit . vtail
--
-- Additionally, type inferace loops (ghci):
-- *CLaSH.Sized.Vector> middle v3 ==> blinking cursor
-- *CLaSH.Sized.Vector> middle v3 :: Vector 1 Double
-- 2.0
vmiddle :: ((1 <=? n) ~ True) => Vector (n + 2) a -> Vector n a
vmiddle = vinit . vtail

-- Would like to write:
-- append :: Vector n a -> Vector m a -> Vector (n + m) a
--
-- But recursive case won't allow it.
-- Is the reason for the ackwardly defined (:>) constructor:
-- a -> Vector (n + 1) a -> Vector (n + 1 + 1) a
append :: Vector (n + 1) a -> Vector (m + 1) a -> Vector (n + m + 1 + 1) a
append (One x)   ys = x :> ys
append (x :> xs) ys = x :> (append xs ys)

shiftIntoL :: a -> Vector (n + 1) a -> Vector (n + 1) a
shiftIntoL s (One _)   = One s
shiftIntoL s (x :> xs) = s :> vinit (x :> xs)

infixr 4 +>>
(+>>) :: a -> Vector (n + 1) a -> Vector (n + 1) a
s +>> xs = shiftIntoL s xs

-- If 'One' _would_ be of type: a -> Vector 1 a
-- The type checker/inferer hangs on:
-- 'snoc s (One x) = x :> (One s)'
snoc :: a -> Vector n a -> Vector (n + 1) a
snoc s (One x)   = x :> One s
snoc s (x :> xs) = x :> (snoc s xs)

infixl 5 <:
(<:) :: Vector n a -> a -> Vector (n + 1) a
xs <: s = snoc s xs

shiftIntoR :: a -> Vector (n + 1) a -> Vector (n + 1) a
shiftIntoR s (One _)   = One s
shiftIntoR s (x :> xs) = snoc s (vtail (x :> xs))

infixl 4 <<+
(<<+) :: Vector (n + 1) a -> a -> Vector (n + 1) a
xs <<+ s = shiftIntoR s xs

vmap :: (a -> b) -> Vector n a -> Vector n b
vmap f (One x)   = One (f x)
vmap f (x :> xs) = (f x) :> vmap f xs

vzipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
vzipWith f (One x) (One y)     = One (f x y)
vzipWith f (x :> xs) (y :> ys) = f x y :> vzipWith f xs ys

vfoldr :: (a -> b -> b) -> b -> Vector n a -> b
vfoldr f z (One x)   = f x z
vfoldr f z (x :> xs) = f x (vfoldr f z xs)

vfoldl :: (a -> b -> a) -> a -> Vector n b -> a
vfoldl f z (One x)   = f z x
vfoldl f z (x :> xs) = vfoldl f (f z x) xs
