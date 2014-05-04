{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module CLaSH.Sized.Vector
  ( -- * 'Vec'tor constructors
    Vec(..), (<:)
    -- * Standard 'Vec'tor functions
    -- ** Extracting sub-'Vec'tors
  , vhead, vtail, vlast, vinit
  , vtake, vtakeI, vdrop, vdropI, vexact, vselect, vselectI
    -- ** Combining 'Vec'tors
  , (+>>), (<<+), (<++>), vconcat, vzip, vunzip
    -- ** Splitting 'Vec'tors
  , vsplit, vsplitI, vunconcat, vunconcatI, vmerge
    -- ** Applying functions to 'Vec'tor elements
  , vmap, vzipWith
  , vfoldr, vfoldl, vfoldr1, vfoldl1
  , vscanl, vscanl1, vscanr, vscanr1
  , vmapAccumL, vmapAccumR
    -- ** Indexing 'Vec'tors
  , (!), vreplace, maxIndex, vlength
    -- ** Generating 'Vec'tors
  , vcopy, vcopyI, viterate, viterateI, vgenerate, vgenerateI
    -- ** Misc
  , vreverse, toList, v, lazyV, asNatProxy
    -- * Alternative 'Vec'tor functions
  , vhead'
  )
where

import Control.Applicative
-- import Data.Traversable
import Data.Foldable              hiding (toList)
import Data.Proxy
import GHC.TypeLits
import Language.Haskell.TH        (ExpQ)
import Language.Haskell.TH.Syntax (Lift(..))
import Unsafe.Coerce              (unsafeCoerce)

import CLaSH.Promoted.Nat

-- | Fixed size vectors
--
-- * Lists with their length encoded in their type
-- * 'Vec'tor elements have a descending subscript starting from 'maxIndex' ('vlength' - 1)
--   and ending at 0
--
-- >>> (3:>4:>5:>Nil)
-- <3,4,5>
-- >>> :t (3:>4:>5:>Nil)
-- (3:>4:>5:>Nil) :: Num a => Vec 3 a
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

-- instance Traversable (Vec n) where
--   traverse _ Nil       = pure Nil
--   traverse f (x :> xs) = (:>) <$> f x <*> traverse f xs

instance Foldable (Vec n) where
  foldr = vfoldr

instance Functor (Vec n) where
  fmap = vmap

{-# NOINLINE vhead #-}
-- | Extract the first element of a vector
--
-- >>> vhead (1:>2:>3:>Nil)
-- 1
-- >>> vhead Nil
--   <interactive>
--       Couldn't match type ‘1’ with ‘0’
--       Expected type: Vec (0 + 1) a
--         Actual type: Vec 0 a
--       In the first argument of ‘vhead’, namely ‘Nil’
--       In the expression: vhead Nil
vhead :: Vec (n + 1) a -> a
vhead (x :> _) = x

{-# NOINLINE vtail #-}
-- | Extract the elements after the head of a vector
--
-- >>> vtail (1:>2:>3:>Nil)
-- <2,3>
-- >>> vtail Nil
--   <interactive>
--       Couldn't match type ‘1’ with ‘0’
--       Expected type: Vec (0 + 1) a
--         Actual type: Vec 0 a
--       In the first argument of ‘vtail’, namely ‘Nil’
--       In the expression: vtail Nil
vtail :: Vec (n + 1) a -> Vec n a
vtail (_ :> xs) = unsafeCoerce xs

{-# NOINLINE vlast #-}
-- | Extract the last element of a vector
--
-- >>> vlast (1:>2:>3:>Nil)
-- 3
-- >>> vlast Nil
--   <interactive>
--       Couldn't match type ‘1’ with ‘0’
--       Expected type: Vec (0 + 1) a
--         Actual type: Vec 0 a
--       In the first argument of ‘vlast’, namely ‘Nil’
--       In the expression: vlast Nil
vlast :: Vec (n + 1) a -> a
vlast (x :> Nil)     = x
vlast (_ :> y :> ys) = vlast (y :> ys)

{-# NOINLINE vinit #-}
-- | Extract all the elements of a vector except the last element
--
-- >>> vinit (1:>2:>3:>Nil)
-- <1,2>
-- >>> vinit Nil
--   <interactive>
--       Couldn't match type ‘1’ with ‘0’
--       Expected type: Vec (0 + 1) a
--         Actual type: Vec 0 a
--       In the first argument of ‘vinit’, namely ‘Nil’
--       In the expression: vinit Nil
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
--
-- >>> 1 +>> (3:>4:>5:>Nil)
-- <1,3,4>
-- >>> 1 +>> Nil
-- <>
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
--
-- >>> (3:>4:>5:>Nil) <: 1
-- <3,4,5,1>
-- >>> :t (3:>4:>5:>Nil) <: 1
-- (3:>4:>5:>Nil) <: 1 :: Num a => Vec 4 a
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
--
-- >>> (3:>4:>5:>Nil) <<+ 1
-- <4,5,1>
-- >>> Nil <<+ 1
-- <>
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
--
-- >>> (1:>2:>3:>Nil) <++> (7:>8:>Nil)
-- <1,2,3,7,8>
(<++>) :: Vec n a -> Vec m a -> Vec (n + m) a
xs <++> ys = vappend xs ys

{-# NOINLINE vsplit #-}
-- | Split a vector into two vectors at the given point
--
-- >>> vsplit (snat :: SNat 3) (1:>2:>3:>7:>8:>Nil)
-- (<1,2,3>, <7,8>)
-- >>> vsplit d3 (1:>2:>3:>7:>8:>Nil)
-- (<1,2,3>, <7,8>)
vsplit :: SNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
vsplit n xs = vsplitU (toUNat n) xs

vsplitU :: UNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
vsplitU UZero     ys        = (Nil,ys)
vsplitU (USucc s) (y :> ys) = let (as,bs) = vsplitU s (unsafeCoerce ys)
                              in  (y :> as, bs)

{-# INLINEABLE vsplitI #-}
-- | Split a vector into two vectors where the length of the two is determined
-- by the context
--
-- >>> vsplitI (1:>2:>3:>7:>8:>Nil) :: (Vec 2 Int, Vec 3 Int)
-- (<1,2>,<3,7,8>)
vsplitI :: KnownNat m => Vec (m + n) a -> (Vec m a, Vec n a)
vsplitI = withSNat vsplit

{-# NOINLINE vconcat #-}
-- | Concatenate a vector of vectors
--
-- >>> vconcat ((1:>2:>3:>Nil) :> (4:>5:>6:>Nil) :> (7:>8:>9:>Nil) :> (10:>11:>12:>Nil) :> Nil)
-- <1,2,3,4,5,6,7,8,9,10,11,12>
vconcat :: Vec n (Vec m a) -> Vec (n * m) a
vconcat Nil       = Nil
vconcat (x :> xs) = unsafeCoerce (vappend x (vconcat xs))

{-# NOINLINE vunconcat #-}
-- | Split a vector of (n * m) elements into a vector of vectors with length m,
-- where m is given
--
-- >>> vunconcat d4 (1:>2:>3:>4:>5:>6:>7:>8:>9:>10:>11:>12:>Nil)
-- <<1,2,3,4>,<5,6,7,8>,<9,10,11,12>>
vunconcat :: KnownNat n => SNat m -> Vec (n * m) a -> Vec n (Vec m a)
vunconcat n xs = vunconcatU (withSNat toUNat) (toUNat n) xs

vunconcatU :: UNat n -> UNat m -> Vec (n * m) a -> Vec n (Vec m a)
vunconcatU UZero      _ _  = Nil
vunconcatU (USucc n') m ys = let (as,bs) = vsplitU m (unsafeCoerce ys)
                             in  as :> vunconcatU n' m bs

{-# INLINEABLE vunconcatI #-}
-- | Split a vector of (n * m) elements into a vector of vectors with length m,
-- where m is determined by the context
--
-- >>> vunconcatI (1:>2:>3:>4:>5:>6:>7:>8:>9:>10:>11:>12:>Nil) :: Vec 2 (Vec 6 Int)
-- <<1,2,3,4,5,6>,<7,8,9,10,11,12>>
vunconcatI :: (KnownNat n, KnownNat m) => Vec (n * m) a -> Vec n (Vec m a)
vunconcatI = withSNat vunconcat

{-# NOINLINE vmerge #-}
-- | Merge two vectors, alternating their elements, i.e.,
--
-- >>> vmerge (1 :> 2 :> 3 :> 4 :> Nil) (5 :> 6 :> 7 :> 8 :> Nil)
-- <1,5,2,6,3,7,4,8>
vmerge :: Vec n a -> Vec n a -> Vec (n + n) a
vmerge Nil       Nil       = Nil
vmerge (x :> xs) (y :> ys) = unsafeCoerce (x :> y :> (vmerge xs (unsafeCoerce ys)))

{-# NOINLINE vreverse #-}
-- | Returns the elements in a list in reverse order
--
-- >>> vreverse (1:>2:>3:>4:>Nil)
-- <4,3,2,1>
vreverse :: Vec n a -> Vec n a
vreverse Nil        = Nil
vreverse (x :> xs)  = vreverse xs <: x

{-# NOINLINE vmap #-}
-- | 'vmap' @f xs@ is the list obtained by applying @f@ to each element
-- of @xs@, i.e.,
--
-- > vmap f (xn :> ... :> x2 :> x1 :> Nil) == (f xn :> ... :> f x2 :> f x1 :> Nil)
vmap :: (a -> b) -> Vec n a -> Vec n b
vmap _ Nil       = Nil
vmap f (x :> xs) = f x :> vmap f xs

{-# NOINLINE vzipWith #-}
-- | 'vzipWith' generalises 'vzip' by zipping with the function given
-- as the first argument, instead of a tupling function.
-- For example, @'vzipWith' (+)@ is applied to two vectors to produce the
-- vector of corresponding sums.
--
-- > vzipWith f (xn :> ... :> x2 :> x1 :> Nil) (yn :> ... :> y2 :> y1 :> Nil) == (f xn yn :> ... :> f x2 y2 :> f x1 y1 :> Nil)
vzipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vzipWith _ Nil       Nil       = Nil
vzipWith f (x :> xs) (y :> ys) = f x y :> (vzipWith f xs (unsafeCoerce ys))

{-# NOINLINE vfoldr #-}
-- | 'vfoldr', applied to a binary operator, a starting value (typically
-- the right-identity of the operator), and a vector, reduces the vector
-- using the binary operator, from right to left:
--
-- > vfoldr f z (xn :> ... :> x2 :> x1 :> Nil) == xn `f` (... (x2 `f` (x1 `f` z))...)
-- > vfoldr r z Nil                            == z
vfoldr :: (a -> b -> b) -> b -> Vec n a -> b
vfoldr _ z Nil       = z
vfoldr f z (x :> xs) = f x (vfoldr f z xs)

{-# NOINLINE vfoldl #-}
-- | 'vfoldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a vector, reduces the vector
-- using the binary operator, from left to right:
--
-- > vfoldl f z (xn :> ... :> x2 :> x1 :> Nil) == (...((z `f` xn)... `f` x2) `f` x1
-- > vfoldl f z Nil                            == z
vfoldl :: (b -> a -> b) -> b -> Vec n a -> b
vfoldl _ z Nil       = z
vfoldl f z (x :> xs) = vfoldl f (f z x) xs

{-# NOINLINE vfoldr1 #-}
-- | 'vfoldr1' is a variant of 'vfoldr' that has no starting value argument,
-- and thus must be applied to non-empty vectors.
--
-- > vfoldr1 f (xn :> ... :> x3 :> x2 :> x1 :> Nil) == xn `f` (... (x3 `f` (x2 `f` x1))...)
-- > vfoldr1 f (x1 :> Nil)                          == x1
-- > vfoldr1 f Nil                                  == TYPE ERROR
vfoldr1 :: (a -> a -> a) -> Vec (n + 1) a -> a
vfoldr1 _ (x :> Nil)       = x
vfoldr1 f (x :> (y :> ys)) = f x (vfoldr1 f (y :> ys))

{-# INLINEABLE vfoldl1 #-}
-- | 'vfoldl1' is a variant of 'vfoldl' that has no starting value argument,
-- and thus must be applied to non-empty vectors.
--
-- > vfoldl f (xn :> xn1 :> ... :> x2 :> x1 :> Nil) == (...((xn `f` xn1)... `f` x2) `f` x1
-- > vfoldl f (x1 :> Nil)                           == x1
-- > vfoldl f Nil                                   == TYPE ERROR
vfoldl1 :: (a -> a -> a) -> Vec (n + 1) a -> a
vfoldl1 f xs = vfoldl f (vhead xs) (vtail xs)

{-# NOINLINE vscanl #-}
vscanl :: (b -> a -> b) -> b -> Vec n a -> Vec (n + 1) b
vscanl f z vs = z :> case vs of
                       Nil       -> Nil
                       (x :> xs) -> vscanl f (f z x) xs

{-# NOINLINE vscanl1 #-}
vscanl1 :: (a -> a -> a) -> Vec n a -> Vec n a
vscanl1 _ Nil       = Nil
vscanl1 f (x :> xs) = vscanl f x xs

{-# NOINLINE vscanr #-}
vscanr :: (a -> b -> b) -> b -> Vec n a -> Vec (n + 1) b
vscanr _ z Nil       = z :> Nil
vscanr f z (x :> xs) = case vscanr f z xs of
                         (q :> qs) -> f x q :> q :> qs

{-# NOINLINE vscanr1 #-}
vscanr1 :: (a -> a -> a) -> Vec n a -> Vec n a
vscanr1 _ Nil            = Nil
vscanr1 _ (x :> Nil)     = x :> Nil
vscanr1 f (x :> y :> ys) = case vscanr1 f (y :> ys) of
                             (q :> qs) -> f x q :> q :> qs

{-# NOINLINE vmapAccumL #-}
vmapAccumL :: (acc -> x -> (acc,y)) -> acc -> Vec n x -> (acc, Vec n y)
vmapAccumL _ acc Nil       = (acc,Nil)
vmapAccumL f acc (x :> xs) = (acc'',y :> ys)
  where
    (acc',y)   = f acc x
    (acc'',ys) = vmapAccumL f acc' xs

{-# NOINLINE vmapAccumR #-}
vmapAccumR :: (acc -> x -> (acc,y)) -> acc -> Vec n x -> (acc, Vec n y)
vmapAccumR _ acc Nil       = (acc,Nil)
vmapAccumR f acc (x :> xs) = (acc'',y :> ys)
  where
    (acc'',y) = f acc' x
    (acc',ys) = vmapAccumL f acc xs

{-# NOINLINE vzip #-}
-- | 'vzip' takes two lists and returns a list of corresponding pairs.
--
-- >>> vzip (1:>2:>3:>4:>Nil) (4:>3:>2:>1:>Nil)
-- <(1,4),(2,3),(3,2),(4,1)>
vzip :: Vec n a -> Vec n b -> Vec n (a,b)
vzip Nil       Nil       = Nil
vzip (x :> xs) (y :> ys) = (x,y) :> (vzip xs (unsafeCoerce ys))

{-# NOINLINE vunzip #-}
-- | 'vunzip' transforms a list of pairs into a list of first components
-- and a list of second components.
--
-- >>> vunzip ((1,4):>(2,3):>(3,2):>(4,1):>Nil)
-- (<1,2,3,4>,<4,3,2,1>)
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
    Nothing -> error ("(!): Index " ++ show i ++ " is out of bounds 0 and " ++ show (maxIndex xs))

{-# INLINEABLE (!) #-}
-- | Vector index (subscript) operator, descending from 'maxIndex', where the
-- last element has subscript 0.
--
-- >>> (1:>2:>3:>4:>5:>Nil) ! 4
-- 1
-- >>> (1:>2:>3:>4:>5:>Nil) ! maxIndex
-- 1
-- >>> (1:>2:>3:>4:>5:>Nil) ! 1
-- 4
-- >>> (1:>2:>3:>4:>5:>Nil) ! 14
-- *** Exception: index out of bounds
(!) :: (KnownNat n, Integral i) => Vec n a -> i -> a
xs ! i = vindex_integer xs (toInteger i)

{-# NOINLINE maxIndex #-}
-- | Index (subscript) of the head of the 'Vec'tor
--
-- >>> maxIndex (6 :> 7 :> 8 :> Nil)
-- 2
maxIndex :: KnownNat n => Vec n a -> Integer
maxIndex = subtract 1 . vlength

{-# NOINLINE vlength #-}
-- | Length of a 'Vec'tor as an Integer
--
-- >>> vlength (6 :> 7 :> 8 :> Nil)
-- 3
vlength :: KnownNat n => Vec n a -> Integer
vlength = natVal . asNatProxy

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
  Nothing -> error ("vreplace: Index " ++ show i ++ " is out of bounds 0 and " ++ show (maxIndex xs))

{-# INLINEABLE vreplace #-}
-- | Replace an element of a vector at the given index (subscript).
--
-- NB: vector elements have a descending subscript starting from 'maxIndex' and
-- ending at 0
--
-- >>> vreplace (1:>2:>3:>4:>5:>Nil) 3 7
-- <1,7,3,4,5>
-- >>> vreplace (1:>2:>3:>4:>5:>Nil) 0 7
-- <1,2,3,4,7>
-- >>> vreplace (1:>2:>3:>4:>5:>Nil) 9 7
-- <*** Exception: index out of bounds
vreplace :: (KnownNat n, Integral i) => Vec n a -> i -> a -> Vec n a
vreplace xs i y = vreplace_integer xs (toInteger i) y

{-# NOINLINE vtake #-}
-- | 'vtake' @n@, applied to a vector @xs@, returns the @n@-length prefix of @xs@
--
-- >>> vtake (snat :: SNat 3) (1:>2:>3:>4:>5:>Nil)
-- <1,2,3>
-- >>> vtake d3               (1:>2:>3:>4:>5:>Nil)
-- <1,2,3>
-- >>> vtake d0               (1:>2:>Nil)
-- <>
-- >>> vtake d4               (1:>2:>Nil)
--   <interactive>
--       Couldn't match type ‘4 + n0’ with ‘2’
--       The type variable ‘n0’ is ambiguous
--       Expected type: Vec (4 + n0) a
--         Actual type: Vec (1 + 1) a
--       In the second argument of ‘vtake’, namely ‘(1 :> 2 :> Nil)’
--       In the expression: vtake d4 (1 :> 2 :> Nil)
--       In an equation for ‘it’: it = vtake d4 (1 :> 2 :> Nil)
vtake :: SNat m -> Vec (m + n) a -> Vec m a
vtake n = fst . vsplit n

{-# INLINEABLE vtakeI #-}
-- | 'vtakeI' @xs@, returns the prefix of @xs@ as demanded by the context
--
-- >>> vtakeI (1:>2:>3:>4:>5:>Nil) :: Vec 2 Int
-- <1,2>
vtakeI :: KnownNat m => Vec (m + n) a -> Vec m a
vtakeI = withSNat vtake

{-# NOINLINE vdrop #-}
-- | 'vdrop' @n xs@ returns the suffix of @xs@ after the first @n@ elements
--
-- >>> vdrop (snat :: SNat 3) (1:>2:>3:>4:>5:>Nil)
-- <4,5>
-- >>> vdrop d3               (1:>2:>3:>4:>5:>Nil)
-- <4,5>
-- >>> vdrop d0               (1:>2:>Nil)
-- <1,2>
-- >>> vdrop d4               (1:>2:>Nil)
--   <interactive>
--       Couldn't match expected type ‘2’ with actual type ‘4 + n0’
--       The type variable ‘n0’ is ambiguous
--       In the first argument of ‘print’, namely ‘it’
--       In a stmt of an interactive GHCi command: print it
vdrop :: SNat m -> Vec (m + n) a -> Vec n a
vdrop n = snd . vsplit n

{-# INLINEABLE vdropI #-}
-- | 'vdropI' @xs@, returns the suffix of @xs@ as demanded by the context
--
-- >>> vdropI (1:>2:>3:>4:>5:>Nil) :: Vec 2 Int
-- <4,5>
vdropI :: KnownNat m => Vec (m + n) a -> Vec n a
vdropI = withSNat vdrop

{-# NOINLINE vexact #-}
-- | 'vexact' @n xs@ returns @n@'th element of @xs@
--
-- NB: vector elements have a descending subscript starting from 'maxIndex' and
-- ending at 0
--
-- >>> vexact (snat :: SNat 1) (1:>2:>3:>4:>5:>Nil)
-- 4
-- >>> vexact d1               (1:>2:>3:>4:>5:>Nil)
-- 4
vexact :: SNat m -> Vec (m + (n + 1)) a -> a
vexact n xs = vhead $ snd $ vsplit n (vreverse xs)

{-# NOINLINE vselect #-}
-- | 'vselect' @f s n xs@ selects @n@ elements with stepsize @s@ and
-- offset @f@ from @xs@
--
-- >>> vselect (snat :: SNat 1) (snat :: SNat 2) (snat :: SNat 3) (1:>2:>3:>4:>5:>6:>7:>8:>Nil)
-- <2,4,6>
-- >>> vselect d1 d2 d3 (1:>2:>3:>4:>5:>6:>7:>8:>Nil)
-- <2,4,6>
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
--
-- >>> vselectI d1 d2 (1:>2:>3:>4:>5:>6:>7:>8:>Nil) :: Vec 2 Int
-- <2,4>
vselectI :: ((f + (s * n) + 1) <= i, KnownNat (n + 1))
         => SNat f
         -> SNat s
         -> Vec i a
         -> Vec (n + 1) a
vselectI f s xs = withSNat (\n -> vselect f s n xs)

{-# NOINLINE vcopy #-}
-- | 'vcopy' @n a@ returns a vector that has @n@ copies of @a@
--
-- >>> vcopy (snat :: SNat 3) 6
-- <6,6,6>
-- >>> vcopy d3 6
-- <6,6,6>
vcopy :: SNat n -> a -> Vec n a
vcopy n a = vreplicateU (toUNat n) a

vreplicateU :: UNat n -> a -> Vec n a
vreplicateU UZero     _ = Nil
vreplicateU (USucc s) x = x :> vreplicateU s x

{-# INLINEABLE vcopyI #-}
-- | 'vcopyI' @a@ creates a vector with as many copies of @a@ as demanded by the
-- context
--
-- >>> vcopy 6 :: Vec 5 Int
-- <6,6,6,6,6>
vcopyI :: KnownNat n => a -> Vec n a
vcopyI = withSNat vcopy

{-# NOINLINE viterate #-}
-- | 'viterate' @n f x@ returns a vector starting with @x@ followed by @n@
-- repeated applications of @f@ to @x@
--
-- > viterate (snat :: SNat 4) f x == (x :> f x :> f (f x) :> f (f (f x)) :> Nil)
-- > viterate d4 f x               == (x :> f x :> f (f x) :> f (f (f x)) :> Nil)
viterate :: SNat n -> (a -> a) -> a -> Vec n a
viterate n f a = viterateU (toUNat n) f a

viterateU :: UNat n -> (a -> a) -> a -> Vec n a
viterateU UZero     _ _ = Nil
viterateU (USucc s) g x = x :> viterateU s g (g x)

{-# INLINEABLE viterateI #-}
-- | 'viterate' @f x@ returns a vector starting with @x@ followed by @n@
-- repeated applications of @f@ to @x@, where @n@ is determined by the context
--
-- > viterateI f x :: Vec 3 a == (x :> f x :> f (f x) :> Nil)
viterateI :: KnownNat n => (a -> a) -> a -> Vec n a
viterateI = withSNat viterate

{-# INLINEABLE vgenerate #-}
-- | 'vgenerate' @n f x@ returns a vector with @n@ repeated applications of @f@
-- to @x@
--
-- > vgenerate (snat :: SNat 4) f x == (f x :> f (f x) :> f (f (f x)) :> f (f (f (f x))) :> Nil)
-- > vgenerate d4 f x               == (f x :> f (f x) :> f (f (f x)) :> f (f (f (f x))) :> Nil)
vgenerate :: SNat n -> (a -> a) -> a -> Vec n a
vgenerate n f a = viterate n f (f a)

{-# INLINEABLE vgenerateI #-}
-- | 'vgenerate' @f x@ returns a vector with @n@ repeated applications of @f@
-- to @x@, where @n@ is determined by the context
--
-- > vgenerateI f x :: Vec 3 a == (f x :> f (f x) :> f (f (f x)) :> Nil)
vgenerateI :: KnownNat n => (a -> a) -> a -> Vec n a
vgenerateI = withSNat vgenerate

{-# INLINEABLE toList #-}
-- | Convert a vector to a list
--
-- >>> toList (1:>2:>3:>Nil)
-- [1,2,3]
toList :: Vec n a -> [a]
toList = vfoldr (:) []

-- | Create a vector literal from a list literal
--
-- > $(v [1::Signed 8,2,3,4,5]) == (8:>2:>3:>4:>5:>Nil) :: Vec 5 (Signed 8)
--
-- >>> [1 :: Signed 8,2,3,4,5]
-- [1,2,3,4,5]
-- >>> $(v [1::Signed 8,2,3,4,5])
-- <1,2,3,4,5>
v :: Lift a => [a] -> ExpQ
v []     = [| Nil |]
v (x:xs) = [| x :> $(v xs) |]

-- | 'Vec'tor as a 'Proxy' for 'Nat'
asNatProxy :: Vec n a -> Proxy n
asNatProxy _ = Proxy

{-# NOINLINE lazyV #-}
-- | For when your vector functions are too strict in their arguments
--
-- For example:
--
-- > -- Bubble sort for 1 iteration
-- > sortV xs = vmap fst sorted <: (snd (vlast sorted))
-- >  where
-- >    lefts  = vhead xs :> vmap snd (vinit sorted)
-- >    rights = vtail xs
-- >    sorted = vzipWith compareSwapL lefts rights
-- >
-- > -- Compare and swap
-- > compareSwapL a b = if a < b then (a,b)
-- >                             else (b,a)
--
-- Will not terminate because 'vzipWith' is too strict in its second argument:
--
-- >>> sortV (4 :> 1 :> 2 :> 3 :> Nil)
-- <*** Exception: <<loop>>
--
-- In this case, adding 'lazyV' on 'vzipWith's second argument:
--
-- > sortVL xs = vmap fst sorted <: (snd (vlast sorted))
-- >  where
-- >    lefts  = vhead xs :> vmap snd (vinit sorted)
-- >    rights = vtail xs
-- >    sorted = vzipWith compareSwapL (lazyV lefts) rights
--
-- Results in a successful computation:
--
-- >>> sortVL (4 :> 1 :> 2 :> 3 :> Nil)
-- <1,2,3,4>
lazyV :: KnownNat n
      => Vec n a
      -> Vec n a
lazyV = lazyV' (vcopyI undefined)
  where
    lazyV' :: Vec n a -> Vec n a -> Vec n a
    lazyV' Nil       _  = Nil
    lazyV' (_ :> xs) ys = vhead ys :> lazyV' xs (vtail ys)

{-# NOINLINE vhead' #-}
-- | Same as 'vhead', but with a "@(1 <= n)@" constraint and "@Vec n a@" argument,
-- instead of a "@Vec (n + 1) a@" argument
vhead' :: (1 <= n)
       => Vec n a
       -> a
vhead' (x :> _) = x
