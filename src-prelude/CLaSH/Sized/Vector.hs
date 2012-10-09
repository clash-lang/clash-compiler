{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module CLaSH.Sized.Vector
  ( Vec(..), (<:)
  , vhead, vtail, vinit, vlast
  , (<++>), (+>>), (<<+), vconcat
  , vmap, vzipWith, vfoldl, vfoldr
  , vindex, vindexM, unsafeIndex
  )
where

import Unsafe.Coerce(unsafeCoerce)
import GHC.TypeLits

import CLaSH.Sized.Index
import CLaSH.Sized.Number

-- Would like to write:
-- data Vector :: Nat -> * -> * where
--   -- Can't because of the 'snoc' function
--   One  :: a -> Vec 1 a
--   -- Can't because of the 'append' function
--   (:>) :: (1 <= n) => a -> Vec n a -> Vec (n + 1) a

data Vec :: Nat -> * -> * where
 One  :: a -> Vec (0 + 1) a
 -- Type checker hangs on 'v3' when defined as:
 -- (:>) :: a -> Vec (n + 1) a -> Vec (n + 2) a
 (:>) :: a -> Vec (n + 1) a -> Vec (n + 1 + 1) a

infixr 5 :>

instance Show a => Show (Vec n a) where
  show (One x)   = show x
  show (x :> xs) = show x ++ " " ++ show xs

v1 :: Vec 1 Double
v1 = One 1.0

v2 :: Vec 2 Double
v2 = 1.0 :> One 2.0

v3 :: Vec 3 Double
v3 = 1.0 :> 2.0 :> One 3.0

v4 :: Vec 2 (Vec 2 Double)
v4 = v2 :> One v2

vhead :: Vec n a -> a
vhead (One x)   = x
vhead (x :> xs) = x

vtail :: (1 <= n) => Vec (n + 1) a -> Vec n a
vtail (x :> xs) = xs

vlast :: Vec n a -> a
vlast (One x)   = x
vlast (x :> xs) = vlast xs

vinit :: (1 <= n) => Vec (n + 1) a -> Vec n a
vinit (x :> One _)   = One x
vinit (x :> y :> ys) = x :> vinit (y :> ys)

-- Would like to write:
-- vmiddle :: (1 <= n) => Vec (n + 2) a -> Vec n a
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
--                    vmiddle :: 1 <= n => Vec (n + 2) a -> Vec n a
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
-- *CLaSH.Sized.Vector> middle v3 :: Vec 1 Double
-- 2.0
vmiddle :: ((1 <=? n) ~ True) => Vec (n + 2) a -> Vec n a
vmiddle = vinit . vtail

-- Would like to write:
-- append :: Vec n a -> Vec m a -> Vec (n + m) a
--
-- But recursive case won't allow it.
-- Is the reason for the ackwardly defined (:>) constructor:
-- a -> Vec (n + 1) a -> Vec (n + 1 + 1) a
vappend :: Vec (n + 1) a -> Vec (m + 1) a -> Vec (n + m + 1 + 1) a
vappend (One x)   ys = x :> ys
vappend (x :> xs) ys = x :> (vappend xs ys)

-- Would like to write 'vconcat' without unsafeCoerce
vconcat :: Vec m (Vec n c) -> Vec (m * n) c
vconcat (One x)   = x
vconcat (x :> xs) = unsafeCoerce
                  $ vappend (unsafeCoerce x)
                  $ unsafeCoerce (vconcat (unsafeCoerce xs))

infixr 5 <++>
(<++>) :: Vec (n + 1) a -> Vec (m + 1) a -> Vec (n + m + 1 + 1) a
xs <++> ys = vappend xs ys

shiftIntoL :: a -> Vec (n + 1) a -> Vec (n + 1) a
shiftIntoL s (One _)   = One s
shiftIntoL s (x :> xs) = s :> vinit (x :> xs)

infixr 4 +>>
(+>>) :: a -> Vec (n + 1) a -> Vec (n + 1) a
s +>> xs = shiftIntoL s xs

-- If 'One' _would_ be of type: a -> Vec 1 a
-- The type checker/inferer hangs on:
-- 'snoc s (One x) = x :> (One s)'
snoc :: a -> Vec n a -> Vec (n + 1) a
snoc s (One x)   = x :> One s
snoc s (x :> xs) = x :> (snoc s xs)

infixl 5 <:
(<:) :: Vec n a -> a -> Vec (n + 1) a
xs <: s = snoc s xs

shiftIntoR :: a -> Vec (n + 1) a -> Vec (n + 1) a
shiftIntoR s (One _)   = One s
shiftIntoR s (x :> xs) = snoc s (vtail (x :> xs))

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

vindex :: Vec n a -> Index n -> a
vindex (One x)   O     = x
vindex (x :> xs) O     = x
vindex (x :> xs) (S k) = vindex xs k

vindexM :: Vec n a -> Number s -> Maybe a
vindexM (One x)   (N 0) = Just x
vindexM (One _)   (N _) = Nothing
vindexM (x :> _)  (N 0) = Just x
vindexM (_ :> xs) (N n) = vindexM xs (U (n-1))

unsafeIndex :: Vec n a -> Number s -> a
unsafeIndex xs i = case vindexM xs i of
  Just a  -> a
  Nothing -> error "index out of bounds"

vtake :: Sing ((n + 1) :: Nat) -> Vec (s + n + 1) a -> Vec (n + 1) a
vtake (SNat n) = veryUnsafeLift (take (fromInteger n))

vdrop :: Sing (n :: Nat) -> Vec (s + n + 1) a -> Vec s a
vdrop (SNat n) = veryUnsafeLift (drop (fromInteger n))

-- Helper functions
toList :: Vec n a -> [a]
toList (One x)    = [x]
toList (x :> xs)  = x : (toList xs)

veryUnsafeFromList :: [a] -> Vec n a
veryUnsafeFromList [x]      = unsafeCoerce (One x)
veryUnsafeFromList (x : xs) = unsafeCoerce $ x :> (veryUnsafeFromList xs)

veryUnsafeLift :: ([a] -> [a]) -> Vec n a -> Vec s a
veryUnsafeLift f = veryUnsafeFromList . f . toList
