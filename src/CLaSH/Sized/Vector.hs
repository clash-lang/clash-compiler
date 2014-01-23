{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module CLaSH.Sized.Vector
  ( Vec(..), (<:)
  , vhead, vtail, vlast, vinit
  , (+>>), (<<+), (<++>), vconcat
  , vsplit, vsplitI, vunconcat, vunconcatI, vmerge
  , vreverse, vmap, vzipWith
  , vfoldr, vfoldl, vfoldr1, vfoldl1
  , vzip, vunzip
  , (!), vreplace
  , vtake, vtakeI, vdrop, vdropI, vexact, vselect, vselectI
  , vcopy, vcopyI, viterate, viterateI, vgenerate, vgenerateI
  , toList, v
  )
where

import Control.Applicative
import Data.Traversable
import Data.Foldable hiding (toList)
import GHC.TypeLits
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax (Lift(..))

import CLaSH.Promoted.Nat

data Vec :: Nat -> * -> * where
  Nil  :: Vec 0 a
  (:>) :: a -> Vec n a -> Vec (n + 1) a

infixr 5 :>

instance Show a => Show (Vec n a) where
  show vs = "<" ++ punc vs ++ ">"
    where
      punc :: Show a => Vec m a -> String
      punc Nil        = ""
      punc (x :> Nil) = show x
      punc (x :> xs)  = show x ++ "," ++ punc xs

instance Eq a => Eq (Vec n a) where
  v1 == v2 = vfoldr (&&) True (vzipWith (==) v1 v2)

instance KnownNat n => Applicative (Vec n) where
  pure  = vcopyI
  (<*>) = vzipWith ($)

instance Traversable (Vec n) where
  traverse _ Nil       = pure Nil
  traverse f (x :> xs) = (:>) <$> f x <*> traverse f xs

instance Foldable (Vec n) where
  foldMap = foldMapDefault

instance Functor (Vec n) where
  fmap = fmapDefault

{-# NOINLINE vhead #-}
vhead :: Vec (n + 1) a -> a
vhead (x :> _) = x

{-# NOINLINE vtail #-}
vtail :: Vec (n + 1) a -> Vec n a
vtail (_ :> xs) = xs

{-# NOINLINE vlast #-}
vlast :: Vec (n + 1) a -> a
vlast (x :> Nil)     = x
vlast (_ :> y :> ys) = vlast (y :> ys)

{-# NOINLINE vinit #-}
vinit :: Vec (n + 1) a -> Vec n a
vinit (_ :> Nil)     = Nil
vinit (x :> y :> ys) = (x :> vinit (y :> ys))

{-# NOINLINE shiftIntoL #-}
shiftIntoL :: a -> Vec n a -> Vec n a
shiftIntoL _ Nil       = Nil
shiftIntoL s (x :> xs) = s :> (vinit (x:>xs))

infixr 4 +>>
{-# INLINEABLE (+>>) #-}
(+>>) :: a -> Vec n a -> Vec n a
s +>> xs = shiftIntoL s xs

{-# NOINLINE snoc #-}
snoc :: a -> Vec n a -> Vec (n + 1) a
snoc s Nil       = s :> Nil
snoc s (x :> xs) = x :> (snoc s xs)

infixl 5 <:
{-# INLINEABLE (<:) #-}
(<:) :: Vec n a -> a -> Vec (n + 1) a
xs <: s = snoc s xs

{-# NOINLINE shiftIntoR #-}
shiftIntoR :: a -> Vec n a -> Vec n a
shiftIntoR _ Nil     = Nil
shiftIntoR s (x:>xs) = snoc s (vtail (x:>xs))

infixl 4 <<+
{-# INLINE (<<+) #-}
(<<+) :: Vec n a -> a -> Vec n a
xs <<+ s = shiftIntoR s xs

{-# NOINLINE vappend #-}
vappend :: Vec n a -> Vec m a -> Vec (n + m) a
vappend Nil       ys = ys
vappend (x :> xs) ys = (x :> (vappend xs ys))

infixr 5 <++>
{-# INLINE (<++>) #-}
(<++>) :: Vec n a -> Vec m a -> Vec (n + m) a
xs <++> ys = vappend xs ys

{-# NOINLINE vsplit #-}
vsplit :: SNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
vsplit n xs = vsplitU (toUNat n) xs

vsplitU :: UNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
vsplitU UZero     ys        = (Nil,ys)
vsplitU (USucc s) (y :> ys) = let (as,bs) = vsplitU s (ys)
                              in  (y :> as, bs)

{-# INLINEABLE vsplitI #-}
vsplitI :: KnownNat m => Vec (m + n) a -> (Vec m a, Vec n a)
vsplitI = withSNat vsplit

{-# NOINLINE vconcat #-}
vconcat :: Vec n (Vec m a) -> Vec (n * m) a
vconcat Nil       = Nil
vconcat (x :> xs) = vappend x (vconcat xs)

{-# NOINLINE vunconcat #-}
vunconcat :: SNat n -> SNat m -> Vec (n * m) a -> Vec n (Vec m a)
vunconcat n m xs = vunconcatU (toUNat n) (toUNat m) xs

vunconcatU :: UNat n -> UNat m -> Vec (n * m) a -> Vec n (Vec m a)
vunconcatU UZero      _  _  = Nil
vunconcatU (USucc n') m' ys = let (as,bs) = vsplitU m' (ys)
                              in  as :> vunconcatU n' m' bs

{-# INLINEABLE vunconcatI #-}
vunconcatI :: (KnownNat n, KnownNat m) => Vec (n * m) a -> Vec n (Vec m a)
vunconcatI = (withSNat . withSNat) vunconcat

{-# NOINLINE vmerge #-}
vmerge :: Vec n a -> Vec n a -> Vec (n + n) a
vmerge Nil       Nil       = Nil
vmerge (x :> xs) (y :> ys) = (x :> y :> (vmerge xs ys))

{-# NOINLINE vreverse #-}
vreverse :: Vec n a -> Vec n a
vreverse Nil        = Nil
vreverse (x :> xs)  = vreverse xs <: x

{-# NOINLINE vmap #-}
vmap :: (a -> b) -> Vec n a -> Vec n b
vmap _ Nil       = Nil
vmap f (x :> xs) = f x :> vmap f xs

{-# NOINLINE vzipWith #-}
vzipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vzipWith _ Nil       Nil       = Nil
vzipWith f (x :> xs) (y :> ys) = f x y :> (vzipWith f xs ys)

{-# NOINLINE vfoldr #-}
vfoldr :: (a -> b -> b) -> b -> Vec n a -> b
vfoldr _ z Nil       = z
vfoldr f z (x :> xs) = f x (vfoldr f z xs)

{-# NOINLINE vfoldl #-}
vfoldl :: (b -> a -> b) -> b -> Vec n a -> b
vfoldl _ z Nil       = z
vfoldl f z (x :> xs) = vfoldl f (f z x) xs

{-# NOINLINE vfoldr1 #-}
vfoldr1 :: (a -> a -> a) -> Vec (n + 1) a -> a
vfoldr1 _ (x :> Nil)       = x
vfoldr1 f (x :> (y :> ys)) = f x (vfoldr1 f (y :> ys))

{-# INLINEABLE vfoldl1 #-}
vfoldl1 :: (a -> a -> a) -> Vec (n + 1) a -> a
vfoldl1 f xs = vfoldl f (vhead xs) (vtail xs)

{-# NOINLINE vzip #-}
vzip :: Vec n a -> Vec n b -> Vec n (a,b)
vzip Nil       Nil       = Nil
vzip (x :> xs) (y :> ys) = (x,y) :> (vzip xs ys)

{-# NOINLINE vunzip #-}
vunzip :: Vec n (a,b) -> (Vec n a, Vec n b)
vunzip Nil = (Nil,Nil)
vunzip ((a,b) :> xs) = let (as,bs) = vunzip xs
                       in  (a :> as, b :> bs)

{-# NOINLINE vindexM_integer #-}
vindexM_integer :: Vec n a -> Integer -> Maybe a
vindexM_integer Nil       _ = Nothing
vindexM_integer (x :> _)  0 = Just x
vindexM_integer (_ :> xs) n = vindexM_integer xs (n-1)

{-# NOINLINE vindex_integer #-}
vindex_integer :: KnownNat n => Vec n a -> Integer -> a
vindex_integer xs i = case vindexM_integer xs (maxIndex xs - i) of
    Just a  -> a
    Nothing -> error "index out of bounds"

{-# INLINEABLE (!) #-}
(!) :: (KnownNat n, Integral i) => Vec n a -> i -> a
xs ! i = vindex_integer xs (toInteger i)

{-# NOINLINE maxIndex #-}
maxIndex :: forall n a . KnownNat n => Vec n a -> Integer
maxIndex _ = fromSNat (snat :: SNat n) - 1

{-# NOINLINE vreplaceM_integer #-}
vreplaceM_integer :: Vec n a -> Integer -> a -> Maybe (Vec n a)
vreplaceM_integer Nil       _ _ = Nothing
vreplaceM_integer (_ :> xs) 0 y = Just (y :> xs)
vreplaceM_integer (x :> xs) n y = case vreplaceM_integer xs (n-1) y of
                                    Just xs' -> Just (x :> xs')
                                    Nothing  -> Nothing

{-# NOINLINE vreplace_integer #-}
vreplace_integer :: KnownNat n => Vec n a -> Integer -> a -> Vec n a
vreplace_integer xs i a = case vreplaceM_integer xs (maxIndex xs - i) a of
  Just ys -> ys
  Nothing -> error "index out of bounds"

{-# INLINEABLE vreplace #-}
vreplace :: (KnownNat n, Integral i) => Vec n a -> i -> a -> Vec n a
vreplace xs i y = vreplace_integer xs (toInteger i) y

{-# NOINLINE vtake #-}
vtake :: SNat m -> Vec (m + n) a -> Vec m a
vtake n = fst . vsplit n

{-# INLINEABLE vtakeI #-}
vtakeI :: KnownNat m => Vec (m + n) a -> Vec m a
vtakeI = withSNat vtake

{-# NOINLINE vdrop #-}
vdrop :: SNat m -> Vec (m + n) a -> Vec n a
vdrop n = snd . vsplit n

{-# INLINEABLE vdropI #-}
vdropI :: KnownNat m => Vec (m + n) a -> Vec n a
vdropI = withSNat vdrop

{-# NOINLINE vexact #-}
vexact :: SNat m -> Vec (m + (n + 1)) a -> a
vexact n xs = vhead $ snd $ vsplit n xs

{-# NOINLINE vselect #-}
vselect :: SNat k
        -> SNat n
        -> SNat (m + 1)
        -> Vec (k + (n * (m + 1))) a
        -> Vec n a
vselect k n m xs = vselectU k (toUNat n) (toUNat m) xs

vselectI :: KnownNat n
         => SNat k
         -> SNat (m + 1)
         -> Vec (k + (n * (m + 1))) a
         -> Vec n a
vselectI k m1 xs = withSNat (\n -> vselect k n m1 xs)

vselectU :: SNat k -> UNat n -> UNat (m+1) -> Vec (k + (n * (m + 1))) a -> Vec n a
vselectU k n m xs = vmap vhead (vunconcatU n m (vdrop k xs))

{-# NOINLINE vcopy #-}
vcopy :: SNat n -> a -> Vec n a
vcopy n a = vreplicateU (toUNat n) a

vreplicateU :: UNat n -> a -> Vec n a
vreplicateU UZero     _ = Nil
vreplicateU (USucc s) x = x :> vreplicateU s x

{-# INLINEABLE vcopyI #-}
vcopyI :: KnownNat n => a -> Vec n a
vcopyI = withSNat vcopy

{-# NOINLINE viterate #-}
viterate :: SNat n -> (a -> a) -> a -> Vec n a
viterate n f a = viterateU (toUNat n) f a

viterateU :: UNat n -> (a -> a) -> a -> Vec n a
viterateU UZero     _ _ = Nil
viterateU (USucc s) g x = x :> viterateU s g (g x)

{-# INLINEABLE viterateI #-}
viterateI :: KnownNat n => (a -> a) -> a -> Vec n a
viterateI = withSNat viterate

{-# INLINEABLE vgenerate #-}
vgenerate :: SNat n -> (a -> a) -> a -> Vec n a
vgenerate n f a = viterate n f (f a)

{-# INLINEABLE vgenerateI #-}
vgenerateI :: KnownNat n => (a -> a) -> a -> Vec n a
vgenerateI = withSNat vgenerate

{-# NOINLINE toList #-}
toList :: Vec n a -> [a]
toList = vfoldr (:) []

v :: Lift a => [a] -> ExpQ
v []     = [| Nil |]
v (x:xs) = [| x :> $(v xs) |]
