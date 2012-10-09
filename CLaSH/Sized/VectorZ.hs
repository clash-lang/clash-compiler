{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module CLaSH.Sized.VectorZ
  ( Vec(..), (<:)
  , vhead, vtail, vlast, vinit
  , (+>>), (<<+), (<++>), vconcat
  , vsplit, vsplitI, vunconcat, vunconcatI
  , vreverse, vmap, vzipWith, vfoldl, vfoldr, vzip, vunzip
  , vindex, vindexM, unsafeIndex
  , vreplace, vreplaceM, unsafeReplace
  , vtake, vtakeI, vdrop, vdropI, vexact, vselect
  , vreplicate, vreplicateI, viterate, viterateI, vgenerate, vgenerateI
  )
where

import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

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
vhead (x :> _) = x

vtail :: Vec (n + 1) a -> Vec n a
vtail (_ :> xs) = xs

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

vsplit :: Sing m -> Vec (m + n) a -> (Vec m a, Vec n a)
vsplit n xs = vsplit' (isZero n) xs
  where
    vsplit' :: IsZero m -> Vec (m + n) a -> (Vec m a, Vec n a)
    vsplit' IsZero     ys        = (Nil,ys)
    vsplit' (IsSucc s) (y :> ys) = let (as,bs) = vsplit' (isZero s) (unsafeCoerce ys)
                                   in  (y :> as, bs)

vsplitI :: SingI m => Vec (m + n) a -> (Vec m a, Vec n a)
vsplitI = withSing vsplit

vconcat :: Vec n (Vec m a) -> Vec (n * m) a
vconcat Nil       = Nil
vconcat (x :> xs) = unsafeCoerce $ x <++> vconcat xs

vunconcat :: Sing n -> Sing m -> Vec (n * m) a -> Vec n (Vec m a)
vunconcat n m xs = vunconcat' (isZero n) m xs
  where
    vunconcat' :: IsZero n -> Sing m -> Vec (n * m) a -> Vec n (Vec m a)
    vunconcat' IsZero      _ _  = Nil
    vunconcat' (IsSucc n') m' ys = let (as,bs) = vsplit m' (unsafeCoerce ys)
                                   in  as :> vunconcat' (isZero n') m' bs

vunconcatI :: (SingI n, SingI m) => Vec (n * m) a -> Vec n (Vec m a)
vunconcatI = (withSing . withSing) vunconcat

vreverse :: Vec n a -> Vec n a
vreverse Nil        = Nil
vreverse (x :> xs)  = vreverse xs <: x

vmap :: (a -> b) -> Vec n a -> Vec n b
vmap _ Nil       = Nil
vmap f (x :> xs) = f x :> vmap f xs

vzipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vzipWith _ Nil       Nil       = Nil
vzipWith f (x :> xs) (y :> ys) = f x y :> vzipWith f xs ys

vfoldr :: (a -> b -> b) -> b -> Vec n a -> b
vfoldr _ z Nil       = z
vfoldr f z (x :> xs) = f x (vfoldr f z xs)

vfoldl :: (a -> b -> a) -> a -> Vec n b -> a
vfoldl _ z Nil       = z
vfoldl f z (x :> xs) = vfoldl f (f z x) xs

vzip :: Vec n a -> Vec n b -> Vec n (a,b)
vzip Nil       Nil       = Nil
vzip (x :> xs) (y :> ys) = (x,y) :> vzip xs ys

vunzip :: Vec n (a,b) -> (Vec n a, Vec n b)
vunzip Nil = (Nil,Nil)
vunzip ((a,b) :> xs) = let (as,bs) = vunzip xs
                       in  (a :> as, b :> bs)

vindex :: Vec n a -> Index n -> a
vindex (x :> _)  O     = x
vindex (_ :> xs) (S k) = vindex xs k

vindexM :: Vec n a -> Unsigned s -> Maybe a
vindexM Nil       _     = Nothing
vindexM (x :> _)  (U 0) = Just x
vindexM (_ :> xs) (U n) = vindexM xs (U (n-1))

unsafeIndex :: Vec n a -> Unsigned s -> a
unsafeIndex xs i = case vindexM xs i of
  Just a  -> a
  Nothing -> error "index out of bounds"

vreplace :: Vec n a -> Index n -> a -> Vec n a
vreplace (_ :> xs) O     y = y :> xs
vreplace (x :> xs) (S k) y = x :> vreplace xs k y

vreplaceM :: Vec n a -> Unsigned s -> a -> Maybe (Vec n a)
vreplaceM Nil _ _           = Nothing
vreplaceM (_ :> xs) (U 0) y = Just (y :> xs)
vreplaceM (x :> xs) (U n) y = case vreplaceM xs (U (n-1)) y of
                                Just xs' -> Just (x :> xs')
                                Nothing  -> Nothing

unsafeReplace :: Vec n a -> Unsigned s -> a -> Vec n a
unsafeReplace xs i a = case vreplaceM xs i a of
  Just ys -> ys
  Nothing -> error "index out of bounds"

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

vselect ::
  Sing f
  -> Sing (s + 1)
  -> Sing n
  -> Vec (f + ((s * n) + i)) a
  -> Vec n a
vselect f s n xs = vselect' (isZero n) $ vdrop f xs
  where
    vselect' :: IsZero n -> Vec m a -> Vec n a
    vselect' IsZero      _          = Nil
    vselect' (IsSucc n') l@(a :> _) = a :> vselect' (isZero n')
                                                    (vdrop s (unsafeCoerce l))

vreplicate :: Sing n -> a -> Vec n a
vreplicate n a = vreplicate' (isZero n) a
  where
    vreplicate' :: IsZero n -> a -> Vec n a
    vreplicate' IsZero     _ = Nil
    vreplicate' (IsSucc s) x = x :> vreplicate' (isZero s) x

vreplicateI :: SingI n => a -> Vec n a
vreplicateI = withSing vreplicate

viterate :: Sing n -> (a -> a) -> a -> Vec n a
viterate n f a = viterate' (isZero n) f a
  where
    viterate' :: IsZero n -> (a -> a) -> a -> Vec n a
    viterate' IsZero     _ _ = Nil
    viterate' (IsSucc s) g x = x :> viterate' (isZero s) g (g x)

viterateI :: SingI n => (a -> a) -> a -> Vec n a
viterateI = withSing viterate

vgenerate :: Sing n -> (a -> a) -> a -> Vec n a
vgenerate n f a = viterate n f (f a)

vgenerateI :: SingI n => (a -> a) -> a -> Vec n a
vgenerateI = withSing vgenerate
