{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module CLaSH.Sized.VectorZ
  ( Vec(..), (<:)
  , vhead, vtail, vlast, vinit
  , (+>>), (<<+), (<++>), vconcat
  , vmap, vzipWith, vfoldl, vfoldr
  , vindex, vindexM, unsafeIndex
  , vsplit, vsplitI
  , vtake, vtakeI, vdrop, vdropI, vexact
  , vreplicate, vreplicateI
  )
where

import GHC.TypeLits
import Unsafe.Coerce(unsafeCoerce)

import CLaSH.Sized.Index
import CLaSH.Sized.Unsigned

data Vec :: Nat -> * -> * where
  Nil  :: Vec 0 a
  (:>) :: a -> Vec n a -> Vec (n + 1) a

infixr 5 :>

instance Show a => Show (Vec n a) where
  show Nil       = ""
  show (x :> xs) = show x ++ " " ++ show xs

vhead :: Vec (n + 1) a -> a
vhead (x :> xs) = x

vtail :: Vec (n + 1) a -> Vec n a
vtail (x :> xs) = xs

vlast :: Vec (n + 1) a -> a
vlast (x :> Nil)     = x
vlast (_ :> y :> ys) = vlast (y :> ys)

vinit :: Vec (n + 1) a -> Vec n a
vinit (_ :> Nil)     = Nil
vinit (x :> y :> ys) = x :> vinit (y :> ys)

shiftIntoL :: a -> Vec (n + 1) a -> Vec (n + 1) a
shiftIntoL s xs = s :> (vinit xs)

infixr 4 +>>
(+>>) :: a -> Vec (n + 1) a -> Vec (n + 1) a
s +>> xs = shiftIntoL s xs

snoc :: a -> Vec n a -> Vec (n + 1) a
snoc s Nil       = s :> Nil
snoc s (x :> xs) = x :> (snoc s xs)

infixl 5 <:
(<:) :: Vec n a -> a -> Vec (n + 1) a
xs <: s = snoc s xs

shiftIntoR :: a -> Vec (n + 1) a -> Vec (n + 1) a
shiftIntoR s xs = snoc s (vtail xs)

infixl 4 <<+
(<<+) :: Vec (n + 1) a -> a -> Vec (n + 1) a
xs <<+ s = shiftIntoR s xs

vappend :: Vec n a -> Vec m a -> Vec (n + m) a
vappend Nil       ys = ys
vappend (x :> xs) ys = x :> (vappend xs ys)

infixr 5 <++>
(<++>) :: Vec n a -> Vec m a -> Vec (n + m) a
xs <++> ys = vappend xs ys

-- CLaSH/Sized/VectorZ.hs:86:21:
--     Could not deduce ((m + (n1 * m)) ~ (n * m))
--     from the context (n ~ (n1 + 1))
--       bound by a pattern with constructor
--                  :> :: forall a (n :: Nat). a -> Vec n a -> Vec (n + 1) a,
--                in an equation for `vconcat'
--       at CLaSH/Sized/VectorZ.hs:86:10-16
--     Expected type: Vec (n * m) a
--       Actual type: Vec (m + (n1 * m)) a
--     In the expression: x <++> vconcat xs
--     In an equation for `vconcat': vconcat (x :> xs) = x <++> vconcat xs
-- Failed, modules loaded: CLaSH.Sized.Index, CLaSH.Sized.Unsigned.
vconcat :: Vec n (Vec m a) -> Vec (n * m) a
vconcat Nil       = Nil
vconcat (x :> xs) = unsafeCoerce $ x <++> vconcat xs

vmap :: (a -> b) -> Vec n a -> Vec n b
vmap f Nil       = Nil
vmap f (x :> xs) = f x :> vmap f xs

vzipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vzipWith f Nil       Nil       = Nil
vzipWith f (x :> xs) (y :> ys) = f x y :> vzipWith f xs ys

vfoldr :: (a -> b -> b) -> b -> Vec n a -> b
vfoldr f z Nil       = z
vfoldr f z (x :> xs) = f x (vfoldr f z xs)

vfoldl :: (a -> b -> a) -> a -> Vec n b -> a
vfoldl f z Nil       = z
vfoldl f z (x :> xs) = vfoldl f (f z x) xs

vzip :: Vec n a -> Vec n b -> Vec n (a,b)
vzip Nil       Nil       = Nil
vzip (x :> xs) (y :> ys) = (x,y) :> vzip xs ys

vunzip :: Vec n (a,b) -> (Vec n a, Vec n b)
vunzip Nil = (Nil,Nil)
vunzip ((a,b) :> xs) = let (as,bs) = vunzip xs
                       in  (a :> as, b :> bs)

vindex :: Vec n a -> Index n -> a
vindex (x :> xs) O     = x
vindex (x :> xs) (S k) = vindex xs k

vindexM :: Vec n a -> Unsigned s -> Maybe a
vindexM Nil       _     = Nothing
vindexM (x :> _)  (U 0) = Just x
vindexM (_ :> xs) (U n) = vindexM xs (U (n-1))

unsafeIndex :: Vec n a -> Unsigned s -> a
unsafeIndex xs i = case vindexM xs i of
  Just a  -> a
  Nothing -> error "index out of bounds"

-- CLaSH/Sized/VectorZ.hs:142:69:
--     Could not deduce (n3 ~ (n2 + n1))
--     from the context (m1 ~ (n2 + 1))
--       bound by a pattern with constructor
--                  IsSucc :: forall (n :: Nat). Sing Nat n -> IsZero (n + 1),
--                in an equation for vsplit'
--       at CLaSH/Sized/VectorZ.hs:142:14-21
--     or from ((m1 + n1) ~ (n3 + 1))
--       bound by a pattern with constructor
--                  :> :: forall a (n :: Nat). a -> Vec n a -> Vec (n + 1) a,
--                in an equation for vsplit'
--       at CLaSH/Sized/VectorZ.hs:142:25-31
--       `n3' is a rigid type variable bound by
--            a pattern with constructor
--              :> :: forall a (n :: Nat). a -> Vec n a -> Vec (n + 1) a,
--            in an equation for vsplit'
--            at CLaSH/Sized/VectorZ.hs:142:25
--     Expected type: Vec (n2 + n1) a1
--       Actual type: Vec n3 a1
--     In the second argument of vsplit', namely `ys'
--     In the expression: vsplit' (isZero s) ys
--     In a pattern binding: (as, bs) = vsplit' (isZero s) ys
vsplit :: Sing m -> Vec (m + n) a -> (Vec m a, Vec n a)
vsplit n xs = vsplit' (isZero n) xs
  where
    vsplit' :: IsZero m -> Vec (m + n) a -> (Vec m a, Vec n a)
    vsplit' IsZero     ys        = (Nil,ys)
    vsplit' (IsSucc s) (y :> ys) = let (as,bs) = vsplit' (isZero s) (unsafeCoerce ys)
                                   in  (y :> as, bs)

vsplitI :: SingI m => Vec (m + n) a -> (Vec m a, Vec n a)
vsplitI = withSing vsplit

vtake :: Sing m -> Vec (m + n) a -> Vec m a
vtake n = fst . vsplit n

vtakeI :: SingI m => Vec (m + n) a -> Vec m a
vtakeI = withSing vtake

vdrop :: Sing m -> Vec (m + n) a -> Vec n a
vdrop n = snd . vsplit n

vdropI :: SingI m => Vec (m + n) a -> Vec n a
vdropI = withSing vdrop

vexact :: Sing m -> Vec (m + (n + 1)) a -> a
vexact n xs = vhead $ snd $ vsplit n xs

vreplicate :: Sing n -> a -> Vec n a
vreplicate n a = vreplicate' (isZero n) a
  where
    vreplicate' :: IsZero n -> a -> Vec n a
    vreplicate' IsZero     _ = Nil
    vreplicate' (IsSucc s) x = x :> vreplicate' (isZero s) x

vreplicateI :: SingI n => a -> Vec n a
vreplicateI = withSing vreplicate

-- Test vectors
v1 :: Vec 1 Double
v1 = 1.0 :> Nil

v2 :: Vec 2 Double
v2 = 1.0 :> 2.0 :> Nil

v3 :: Vec 3 Double
v3 = 1.0 :> 2.0 :> 3.0 :> Nil

v4 :: Vec 2 (Vec 2 Double)
v4 = v2 :> v2 :> Nil
