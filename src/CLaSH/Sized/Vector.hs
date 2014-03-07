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
import Unsafe.Coerce (unsafeCoerce)

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
-- | Extract the first element of a vector
vhead :: Vec (n + 1) a -> a
vhead (x :> _) = x

{-# NOINLINE vtail #-}
-- | Extract the elements after the head of a vector
vtail :: Vec (n + 1) a -> Vec n a
vtail (_ :> xs) = unsafeCoerce xs

{-# NOINLINE vlast #-}
-- | Extract the last element of a vector
vlast :: Vec (n + 1) a -> a
vlast (x :> Nil)     = x
vlast (_ :> y :> ys) = vlast (y :> ys)

{-# NOINLINE vinit #-}
-- | Extract all the elements of a vector except the last element
vinit :: Vec (n + 1) a -> Vec n a
vinit (_ :> Nil)     = unsafeCoerce Nil
vinit (x :> y :> ys) = unsafeCoerce (x :> vinit (y :> ys))

{-# NOINLINE shiftIntoL #-}
-- | Add an element to the head of the vector, and extract all elements of the
-- resulting vector except the last element
shiftIntoL :: a -> Vec n a -> Vec n a
shiftIntoL _ Nil       = Nil
shiftIntoL s (x :> xs) = s :> (vinit (x:>xs))

infixr 4 +>>
{-# INLINEABLE (+>>) #-}
-- | Add an element to the head of the vector, and extract all elements of the
-- resulting vector except the last element
(+>>) :: a -> Vec n a -> Vec n a
s +>> xs = shiftIntoL s xs

{-# NOINLINE snoc #-}
-- | Add an element to the tail of the vector
snoc :: a -> Vec n a -> Vec (n + 1) a
snoc s Nil       = s :> Nil
snoc s (x :> xs) = x :> (snoc s xs)

infixl 5 <:
{-# INLINEABLE (<:) #-}
-- | Add an element to the tail of the vector
(<:) :: Vec n a -> a -> Vec (n + 1) a
xs <: s = snoc s xs

{-# NOINLINE shiftIntoR #-}
-- | Add an element to the tail of the vector, and extract all elements of the
-- resulting vector except the first element
shiftIntoR :: a -> Vec n a -> Vec n a
shiftIntoR _ Nil     = Nil
shiftIntoR s (x:>xs) = snoc s (vtail (x:>xs))

infixl 4 <<+
{-# INLINE (<<+) #-}
-- | Add an element to the tail of the vector, and extract all elements of the
-- resulting vector except the first element
(<<+) :: Vec n a -> a -> Vec n a
xs <<+ s = shiftIntoR s xs

{-# NOINLINE vappend #-}
-- | Append two vectors
vappend :: Vec n a -> Vec m a -> Vec (n + m) a
vappend Nil       ys = ys
vappend (x :> xs) ys = unsafeCoerce (x :> (vappend xs ys))

infixr 5 <++>
{-# INLINE (<++>) #-}
-- | Append two vectors
(<++>) :: Vec n a -> Vec m a -> Vec (n + m) a
xs <++> ys = vappend xs ys

{-# NOINLINE vsplit #-}
-- | Split a vector into two vectors at the given point
vsplit :: SNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
vsplit n xs = vsplitU (toUNat n) xs

vsplitU :: UNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
vsplitU UZero     ys        = (Nil,ys)
vsplitU (USucc s) (y :> ys) = let (as,bs) = vsplitU s (unsafeCoerce ys)
                              in  (y :> as, bs)

{-# INLINEABLE vsplitI #-}
-- | Split a vector into two vectors where the length of the two is determined
-- by the context
vsplitI :: KnownNat m => Vec (m + n) a -> (Vec m a, Vec n a)
vsplitI = withSNat vsplit

{-# NOINLINE vconcat #-}
-- | Concatenate a vector of vectors
vconcat :: Vec n (Vec m a) -> Vec (n * m) a
vconcat Nil       = Nil
vconcat (x :> xs) = unsafeCoerce (vappend x (vconcat xs))

{-# NOINLINE vunconcat #-}
-- | Split a vector of (n * m) elements into a vector of vectors with length m,
-- where m is given
vunconcat :: KnownNat n => SNat m -> Vec (n * m) a -> Vec n (Vec m a)
vunconcat n xs = vunconcatU (withSNat toUNat) (toUNat n) xs

vunconcatU :: UNat n -> UNat m -> Vec (n * m) a -> Vec n (Vec m a)
vunconcatU UZero      _ _  = Nil
vunconcatU (USucc n') m ys = let (as,bs) = vsplitU m (unsafeCoerce ys)
                             in  as :> vunconcatU n' m bs

{-# INLINEABLE vunconcatI #-}
-- | Split a vector of (n * m) elements into a vector of vectors with length m,
-- where m is determined by the context
vunconcatI :: (KnownNat n, KnownNat m) => Vec (n * m) a -> Vec n (Vec m a)
vunconcatI = withSNat vunconcat

{-# NOINLINE vmerge #-}
-- | Merge two vectors, alternating their elements, i.e.,
--
-- > vmerge <xn, ..., x2, x1>  <yn, ..., y2, y1> == <xn, yn, ..., x2, y2, x1, y1>
--
vmerge :: Vec n a -> Vec n a -> Vec (n + n) a
vmerge Nil       Nil       = Nil
vmerge (x :> xs) (y :> ys) = unsafeCoerce (x :> y :> (vmerge xs (unsafeCoerce ys)))

{-# NOINLINE vreverse #-}
-- | Returns the elements in a list in reverse order
vreverse :: Vec n a -> Vec n a
vreverse Nil        = Nil
vreverse (x :> xs)  = vreverse xs <: x

{-# NOINLINE vmap #-}
-- | 'vmap' @f xs@ is the list obtained by applying @f@ to each element
-- of @xs@, i.e.,
--
-- > vmap f <xn, ..., x2, x1> == <f xn, ..., f x2, f x1>
vmap :: (a -> b) -> Vec n a -> Vec n b
vmap _ Nil       = Nil
vmap f (x :> xs) = f x :> vmap f xs

{-# NOINLINE vzipWith #-}
-- | 'vzipWith' generalises 'vzip' by zipping with the function given
-- as the first argument, instead of a tupling function.
-- For example, @'vzipWith' (+)@ is applied to two vectors to produce the
-- vector of corresponding sums.
vzipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vzipWith _ Nil       Nil       = Nil
vzipWith f (x :> xs) (y :> ys) = f x y :> (vzipWith f xs (unsafeCoerce ys))

{-# NOINLINE vfoldr #-}
-- | 'vfoldr', applied to a binary operator, a starting value (typically
-- the right-identity of the operator), and a vector, reduces the vector
-- using the binary operator, from right to left:
--
-- > foldr f z <xn, ..., x2, x1> == xn `f` (... (x2 `f` (x1 `f` z))...)
vfoldr :: (a -> b -> b) -> b -> Vec n a -> b
vfoldr _ z Nil       = z
vfoldr f z (x :> xs) = f x (vfoldr f z xs)

{-# NOINLINE vfoldl #-}
-- | 'vfoldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a vector, reduces the vector
-- using the binary operator, from left to right:
--
-- > vfoldl f z <xn, ..., x2, x1> == (...((z `f` xn)... `f` x2) `f` x1
vfoldl :: (b -> a -> b) -> b -> Vec n a -> b
vfoldl _ z Nil       = z
vfoldl f z (x :> xs) = vfoldl f (f z x) xs

{-# NOINLINE vfoldr1 #-}
-- | 'vfoldr1' is a variant of 'vfoldr' that has no starting value argument,
-- and thus must be applied to non-empty vectors.
vfoldr1 :: (a -> a -> a) -> Vec (n + 1) a -> a
vfoldr1 _ (x :> Nil)       = x
vfoldr1 f (x :> (y :> ys)) = f x (vfoldr1 f (y :> ys))

{-# INLINEABLE vfoldl1 #-}
-- | 'vfoldl1' is a variant of 'vfoldl' that has no starting value argument,
-- and thus must be applied to non-empty vectors.
vfoldl1 :: (a -> a -> a) -> Vec (n + 1) a -> a
vfoldl1 f xs = vfoldl f (vhead xs) (vtail xs)

{-# NOINLINE vzip #-}
-- | 'vzip' takes two lists and returns a list of corresponding pairs.
vzip :: Vec n a -> Vec n b -> Vec n (a,b)
vzip Nil       Nil       = Nil
vzip (x :> xs) (y :> ys) = (x,y) :> (vzip xs (unsafeCoerce ys))

{-# NOINLINE vunzip #-}
-- | 'vunzip' transforms a list of pairs into a list of first components
-- and a list of second components.
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
-- | Vector index (subscript) operator, descending from 'maxIndex', where the
-- last element has subscript 0.
--
-- > <1,2,3,4,5> ! 4 == 1
-- > <1,2,3,4,5> ! maxIndex == 1
-- > <1,2,3,4,5> ! 1 == 4
(!) :: (KnownNat n, Integral i) => Vec n a -> i -> a
xs ! i = vindex_integer xs (toInteger i)

{-# NOINLINE maxIndex #-}

-- | Index (subscript) of the head of the vector
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
-- | Replace an element of a vector at the given index (subscript), NB: vector
-- elements have a descending subscript starting from 'maxIndex' and ending at 0
--
-- > vreplace <1,2,3,4,5> 3 7 == <1,7,3,4,5>
vreplace :: (KnownNat n, Integral i) => Vec n a -> i -> a -> Vec n a
vreplace xs i y = vreplace_integer xs (toInteger i) y

{-# NOINLINE vtake #-}
-- | 'vtake' @n@, applied to a vector @xs@, returns the @n@-length prefix of @xs@
--
-- > vtake (snat :: SNat 3) <1,2,3,4,5> == <1,2,3>
-- > vtake (snat :: SNat 0) <1,2> == <>
-- > vtake (snat :: SNat 4) <1,2> == TYPE ERROR
vtake :: SNat m -> Vec (m + n) a -> Vec m a
vtake n = fst . vsplit n

{-# INLINEABLE vtakeI #-}
-- | 'vtakeI' @xs@, returns the prefix of @xs@ as demanded by the context
vtakeI :: KnownNat m => Vec (m + n) a -> Vec m a
vtakeI = withSNat vtake

{-# NOINLINE vdrop #-}
-- | 'vdrop' @n xs@ returns the suffix of @xs@ after the first @n@ elements
--
-- > vdrop (snat :: SNat 3) <1,2,3,4,5> == <4,5>
-- > vdrop (snat :: SNat 0) <1,2> == <1,2>
-- > vdrop (snat :: SNat 4) <1,2> == TYPE ERROR
vdrop :: SNat m -> Vec (m + n) a -> Vec n a
vdrop n = snd . vsplit n

{-# INLINEABLE vdropI #-}
-- | 'vdropI' @xs@, returns the suffix of @xs@ as demanded by the context
vdropI :: KnownNat m => Vec (m + n) a -> Vec n a
vdropI = withSNat vdrop

{-# NOINLINE vexact #-}
-- | 'vexact' @n xs@ returns @n@'th element of @xs@, NB: vector elements
-- have a descending subscript starting from 'maxIndex' and ending at 0
--
-- > vexact (snat :: SNat 1) <1,2,3,4,5> == 4
vexact :: SNat m -> Vec (m + (n + 1)) a -> a
vexact n xs = vhead $ snd $ vsplit n (vreverse xs)

{-# NOINLINE vselect #-}
-- | 'vselect' @f s n xs@ selects @n@ elements with stepsize @s@ and
-- offset @f@ from @xs@
--
-- vselect (snat :: SNat 1) (snat :: SNat 2) (snat :: SNat 3) <1,2,3,4,5,6,7,8> == <2,4,6>
vselect :: ((f + (s * n) + 1) <= i)
        => SNat f
        -> SNat s
        -> SNat (n + 1)
        -> Vec i a
        -> Vec (n + 1) a
vselect f s n xs = vselect' (toUNat n) $ vdrop f (unsafeCoerce xs)
  where
    vselect' :: UNat n -> Vec m a -> Vec n a
    vselect' UZero      _           = Nil
    vselect' (USucc n') vs@(x :> _) = x :> vselect' n' (vdrop s (unsafeCoerce vs))

{-# NOINLINE vselectI #-}
-- | 'vselectI' @f s xs@ selects as many elements as demanded by the context
-- with stepsize @s@ and offset @f@ from @xs@
vselectI :: ((f + (s * n) + 1) <= i, KnownNat (n + 1))
         => SNat f
         -> SNat s
         -> Vec i a
         -> Vec (n + 1) a
vselectI f s xs = withSNat (\n -> vselect f s n xs)

{-# NOINLINE vcopy #-}
-- | 'vcopy' @n a@ returns a vector that has @n@ copies of @a@
vcopy :: SNat n -> a -> Vec n a
vcopy n a = vreplicateU (toUNat n) a

vreplicateU :: UNat n -> a -> Vec n a
vreplicateU UZero     _ = Nil
vreplicateU (USucc s) x = x :> vreplicateU s x

{-# INLINEABLE vcopyI #-}
-- | 'vcopy' @a@ creates a vector with as many copies of @a@ as demanded by the
-- context
vcopyI :: KnownNat n => a -> Vec n a
vcopyI = withSNat vcopy

{-# NOINLINE viterate #-}
-- | 'viterate' @n f x@ returns a vector starting with @x@ followed by @n@
-- repeated applications of @f@ to @x@
--
-- > viterate (snat :: SNat 4) f x = <x, f x, f (f x), f (f (f x))>
viterate :: SNat n -> (a -> a) -> a -> Vec n a
viterate n f a = viterateU (toUNat n) f a

viterateU :: UNat n -> (a -> a) -> a -> Vec n a
viterateU UZero     _ _ = Nil
viterateU (USucc s) g x = x :> viterateU s g (g x)

{-# INLINEABLE viterateI #-}
-- | 'viterate' @f x@ returns a vector starting with @x@ followed by @n@
-- repeated applications of @f@ to @x@, where @n@ is determined by the context
viterateI :: KnownNat n => (a -> a) -> a -> Vec n a
viterateI = withSNat viterate

{-# INLINEABLE vgenerate #-}
-- | 'vgenerate' @n f x@ returns a vector with @n@ repeated applications of @f@
-- to @x@
--
-- > vgenerate (snat :: SNat 4) f x = <f x, f (f x), f (f (f x)), f (f (f (f x)))>
vgenerate :: SNat n -> (a -> a) -> a -> Vec n a
vgenerate n f a = viterate n f (f a)

{-# INLINEABLE vgenerateI #-}
-- | 'vgenerate' @f x@ returns a vector with @n@ repeated applications of @f@
-- to @x@, where @n@ is determined by the context
vgenerateI :: KnownNat n => (a -> a) -> a -> Vec n a
vgenerateI = withSNat vgenerate

{-# INLINEABLE toList #-}
-- | Convert a vector to a list
toList :: Vec n a -> [a]
toList = vfoldr (:) []

-- | Create a vector literal from a list literal
--
-- > $(v [1::Signed 8,2,3,4,5]) == <1,2,3,4,5> :: Vec 5 (Signed 8)
v :: Lift a => [a] -> ExpQ
v []     = [| Nil |]
v (x:xs) = [| x :> $(v xs) |]
