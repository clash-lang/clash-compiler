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

module CLaSH.Sized.VectorZ
  ( Vec(..), (<:)
  , vhead, vtail, vlast, vinit
  , (+>>), (<<+), (<++>), vconcat
  , vsplit, vsplitI, vunconcat, vunconcatI
  , vreverse, vmap, vzipWith
  , vfoldl, vfoldr, vfoldl1
  , vzip, vunzip
  , (!), vindex, vindexM
  , vreplace, vreplaceM
  , vtake, vtakeI, vdrop, vdropI, vexact, vselect, vselectI
  , vcopyE, vcopy, viterateE, viterate, vgenerateE, vgenerate
  , toList, v
  )
where

import GHC.TypeLits
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax (Lift(..))
import Unsafe.Coerce (unsafeCoerce)

import CLaSH.Class.Default

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

instance (SingI n, Default a) => Default (Vec n a) where
  def = vcopy def

instance Eq a => Eq (Vec n a) where
  v1 == v2 = vfoldr (&&) True (vzipWith (==) v1 v2)

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
vinit (x :> y :> ys) = x :> vinit (y :> ys)

{-# NOINLINE shiftIntoL #-}
shiftIntoL :: a -> Vec n a -> Vec n a
shiftIntoL _ Nil       = Nil
shiftIntoL s (x :> xs) = s :> (vinit (x:>xs))

infixr 4 +>>
{-# INLINE (+>>) #-}
(+>>) :: a -> Vec n a -> Vec n a
s +>> xs = shiftIntoL s xs

{-# NOINLINE snoc #-}
snoc :: a -> Vec n a -> Vec (n + 1) a
snoc s Nil       = s :> Nil
snoc s (x :> xs) = x :> (snoc s xs)

infixl 5 <:
{-# INLINE (<:) #-}
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
vappend (x :> xs) ys = x :> (vappend xs ys)

infixr 5 <++>
{-# INLINE (<++>) #-}
(<++>) :: Vec n a -> Vec m a -> Vec (n + m) a
xs <++> ys = vappend xs ys

{-# NOINLINE vsplit #-}
vsplit :: Sing m -> Vec (m + n) a -> (Vec m a, Vec n a)
vsplit n xs = vsplit' (isZero n) xs
  where
    vsplit' :: IsZero m -> Vec (m + n) a -> (Vec m a, Vec n a)
    vsplit' IsZero     ys        = (Nil,ys)
    vsplit' (IsSucc s) (y :> ys) = let (as,bs) = vsplit' (isZero s) (unsafeCoerce ys)
                                   in  (y :> as, bs)

{-# NOINLINE vsplitI #-}
vsplitI :: SingI m => Vec (m + n) a -> (Vec m a, Vec n a)
vsplitI = withSing vsplit

{-# NOINLINE vconcat #-}
vconcat :: Vec n (Vec m a) -> Vec (n * m) a
vconcat Nil       = Nil
vconcat (x :> xs) = unsafeCoerce $ x <++> vconcat xs

{-# NOINLINE vunconcat #-}
vunconcat :: Sing n -> Sing m -> Vec (n * m) a -> Vec n (Vec m a)
vunconcat n m xs = vunconcat' (isZero n) m xs
  where
    vunconcat' :: IsZero n -> Sing m -> Vec (n * m) a -> Vec n (Vec m a)
    vunconcat' IsZero      _ _  = Nil
    vunconcat' (IsSucc n') m' ys = let (as,bs) = vsplit m' (unsafeCoerce ys)
                                   in  as :> vunconcat' (isZero n') m' bs

{-# NOINLINE vunconcatI #-}
vunconcatI :: (SingI n, SingI m) => Vec (n * m) a -> Vec n (Vec m a)
vunconcatI = (withSing . withSing) vunconcat

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
vzipWith f (x :> xs) (y :> ys) = f x y :> vzipWith f xs ys

{-# NOINLINE vfoldr #-}
vfoldr :: (a -> b -> b) -> b -> Vec n a -> b
vfoldr _ z Nil       = z
vfoldr f z (x :> xs) = f x (vfoldr f z xs)

{-# NOINLINE vfoldl #-}
vfoldl :: (b -> a -> b) -> b -> Vec n a -> b
vfoldl _ z Nil       = z
vfoldl f z (x :> xs) = vfoldl f (f z x) xs

{-# NOINLINE vfoldl1 #-}
vfoldl1 :: (a -> a -> a) -> Vec (n + 1) a -> a
vfoldl1 f (x :> xs) = vfoldl f x xs

{-# NOINLINE vzip #-}
vzip :: Vec n a -> Vec n b -> Vec n (a,b)
vzip Nil       Nil       = Nil
vzip (x :> xs) (y :> ys) = (x,y) :> vzip xs ys

{-# NOINLINE vunzip #-}
vunzip :: Vec n (a,b) -> (Vec n a, Vec n b)
vunzip Nil = (Nil,Nil)
vunzip ((a,b) :> xs) = let (as,bs) = vunzip xs
                       in  (a :> as, b :> bs)

{-# NOINLINE vindexM #-}
vindexM :: (Num i, Eq i) => Vec n a -> i -> Maybe a
vindexM Nil       _ = Nothing
vindexM (x :> _)  0 = Just x
vindexM (_ :> xs) n = vindexM xs (n-1)

{-# NOINLINE vindex #-}
vindex :: (SingI n, Num i, Eq i) => Vec n a -> i -> a
vindex xs i = case vindexM xs (maxIndex xs - i) of
    Just a  -> a
    Nothing -> error "index out of bounds"

{-# INLINE (!) #-}
(!) :: (SingI n, Num i, Eq i) => Vec n a -> i -> a
(!) = vindex

{-# NOINLINE maxIndex #-}
maxIndex :: forall i n a . SingRep n => Num i => Vec n a -> i
maxIndex _ = fromInteger (fromSing (sing :: Sing n) - 1)

{-# NOINLINE vreplaceM #-}
vreplaceM :: (Num i, Eq i) => Vec n a -> i -> a -> Maybe (Vec n a)
vreplaceM Nil       _ _ = Nothing
vreplaceM (_ :> xs) 0 y = Just (y :> xs)
vreplaceM (x :> xs) n y = case vreplaceM xs (n-1) y of
                                Just xs' -> Just (x :> xs')
                                Nothing  -> Nothing

{-# NOINLINE vreplace #-}
vreplace :: (SingI n, Num i, Eq i) => Vec n a -> i -> a -> Vec n a
vreplace xs i a = case vreplaceM xs (maxIndex xs - i) a of
  Just ys -> ys
  Nothing -> error "index out of bounds"

{-# NOINLINE vtake #-}
vtake :: Sing m -> Vec (m + n) a -> Vec m a
vtake n = fst . vsplit n

{-# NOINLINE vtakeI #-}
vtakeI :: SingI m => Vec (m + n) a -> Vec m a
vtakeI = withSing vtake

{-# NOINLINE vdrop #-}
vdrop :: Sing m -> Vec (m + n) a -> Vec n a
vdrop n = snd . vsplit n

{-# NOINLINE vdropI #-}
vdropI :: SingI m => Vec (m + n) a -> Vec n a
vdropI = withSing vdrop

{-# NOINLINE vexact #-}
vexact :: Sing m -> Vec (m + (n + 1)) a -> a
vexact n xs = vhead $ snd $ vsplit n xs

{-# NOINLINE vselect #-}
vselect ::
  ((f + (s * n)) <= i)
  => Sing f
  -> Sing s
  -> Sing (n + 1)
  -> Vec i a
  -> Vec (n + 1) a
vselect f s n xs = vselect' (isZero n) $ vdrop f (unsafeCoerce xs)
  where
    vselect' :: IsZero n -> Vec m a -> Vec n a
    vselect' IsZero      _           = Nil
    vselect' (IsSucc n') vs@(x :> _) = x :> vselect' (isZero n') (vdrop s (unsafeCoerce vs))

{-# NOINLINE vselectI #-}
vselectI ::
  ((f + (s * n)) <= i, SingI (n + 1))
  => Sing f
  -> Sing s
  -> Vec i a
  -> Vec (n + 1) a
vselectI f s xs = withSing (\n -> vselect f s n xs)

{-# NOINLINE vcopyE #-}
vcopyE :: Sing n -> a -> Vec n a
vcopyE n a = vreplicate' (isZero n) a
  where
    vreplicate' :: IsZero n -> a -> Vec n a
    vreplicate' IsZero     _ = Nil
    vreplicate' (IsSucc s) x = x :> vreplicate' (isZero s) x

{-# NOINLINE vcopy #-}
vcopy :: SingI n => a -> Vec n a
vcopy = withSing vcopyE

{-# NOINLINE viterateE #-}
viterateE :: Sing n -> (a -> a) -> a -> Vec n a
viterateE n f a = viterate' (isZero n) f a
  where
    viterate' :: IsZero n -> (a -> a) -> a -> Vec n a
    viterate' IsZero     _ _ = Nil
    viterate' (IsSucc s) g x = x :> viterate' (isZero s) g (g x)

{-# NOINLINE viterate #-}
viterate :: SingI n => (a -> a) -> a -> Vec n a
viterate = withSing viterateE

{-# NOINLINE vgenerateE #-}
vgenerateE :: Sing n -> (a -> a) -> a -> Vec n a
vgenerateE n f a = viterateE n f (f a)

{-# NOINLINE vgenerate #-}
vgenerate :: SingI n => (a -> a) -> a -> Vec n a
vgenerate = withSing vgenerateE

toList :: Vec n a -> [a]
toList = vfoldr (:) []

v :: Lift a => [a] -> ExpQ
v []     = [| Nil |]
v (x:xs) = [| x :> $(v xs) |]
