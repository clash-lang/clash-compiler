{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module CLaSH.Sized.Vector
  ( -- * 'Vec'tor constructors
    Vec(..), (<:), singleton
    -- * Standard 'Vec'tor functions
    -- ** Extracting sub-'Vec'tors
  , head, tail, last, init
  , take, takeI, drop, dropI, exact, select, selectI
    -- ** Combining 'Vec'tors
  , (++), (+>>), (<<+), concat, zip, unzip, shiftInAt0, shiftInAtN
  , shiftOutFrom0, shiftOutFromN
    -- ** Splitting 'Vec'tors
  , splitAt, splitAtI, unconcat, unconcatI, merge
    -- ** Applying functions to 'Vec'tor elements
  , map, zipWith
  , foldr, foldl, foldr1, foldl1, fold
  , scanl, scanr, scanl1, scanr1
  , mapAccumL, mapAccumR
    -- ** Indexing 'Vec'tors
  , (!!), replace, maxIndex, length
    -- ** Generating 'Vec'tors
  , replicate, replicateU, repeat, iterate, iterateI, generate, generateI
    -- ** Misc
  , reverse, toList, v, lazyV, asNatProxy
    -- ** 'Bits'
  , concatBitVector#
  , unconcatBitVector#
  )
where

import Control.Applicative        (Applicative (..))
import Data.Default               (Default (..))
import qualified Data.Foldable    as F
import Data.Proxy                 (Proxy (..))
import GHC.TypeLits               (KnownNat, Nat, type (+), type (*), type (<=),
                                   natVal)
import Language.Haskell.TH        (ExpQ)
import Language.Haskell.TH.Syntax (Lift(..))
import Prelude                    hiding ((++), (!!), concat, drop, foldl,
                                          foldl1, foldr, foldr1, head, init,
                                          iterate, last, length, map, repeat,
                                          replicate, reverse, scanl, scanl1,
                                          scanr, scanr1, splitAt, tail, take,
                                          unzip, zip, zipWith)
import qualified Prelude          as P
import Unsafe.Coerce              (unsafeCoerce)

import CLaSH.Class.BitIndex       (split)
import CLaSH.Promoted.Nat         (SNat, UNat (..), withSNat, toUNat)
import CLaSH.Sized.BitVector      (BitVector, (++#))

-- | Fixed size vectors
--
-- * Lists with their length encoded in their type
-- * 'Vec'tor elements have an __ASCENDING__ subscript starting from 0 and
--   ending at 'maxIndex' (== 'length' - 1).
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
  show vs = "<" P.++ punc vs P.++ ">"
    where
      punc :: Show a => Vec m a -> String
      punc Nil        = ""
      punc (x :> Nil) = show x
      punc (x :> xs)  = show x P.++ "," P.++ punc xs

instance Eq a => Eq (Vec n a) where
  (==) = eq#
  (/=) = neq#

{-# NOINLINE eq# #-}
eq# :: Eq a => Vec n a -> Vec n a -> Bool
eq# v1 v2  = foldr (&&) True (zipWith (==) v1 v2)

{-# NOINLINE neq# #-}
neq# :: Eq a => Vec n a -> Vec n a -> Bool
neq# v1 v2 = not (foldr (&&) True (zipWith (==) v1 v2))

instance KnownNat n => Applicative (Vec n) where
  pure  = repeat
  (<*>) = zipWith ($)

instance F.Foldable (Vec n) where
  foldr = foldr

instance Functor (Vec n) where
  fmap = map

instance (Default a, KnownNat n) => Default (Vec n a) where
  def = repeat def

{-# INLINABLE singleton #-}
singleton :: a -> Vec 1 a
singleton = (:> Nil)

{-# NOINLINE head #-}
-- | Extract the first element of a vector
--
-- >>> head (1:>2:>3:>Nil)
-- 1
-- >>> head Nil
--   <interactive>
--       Couldn't match type ‘1’ with ‘0’
--       Expected type: Vec (0 + 1) a
--         Actual type: Vec 0 a
--       In the first argument of ‘vhead’, namely ‘Nil’
--       In the expression: vhead Nil
head :: Vec (n + 1) a -> a
head (x :> _) = x

{-# NOINLINE tail #-}
-- | Extract the elements after the head of a vector
--
-- >>> tail (1:>2:>3:>Nil)
-- <2,3>
-- >>> tail Nil
--   <interactive>
--       Couldn't match type ‘1’ with ‘0’
--       Expected type: Vec (0 + 1) a
--         Actual type: Vec 0 a
--       In the first argument of ‘vtail’, namely ‘Nil’
--       In the expression: vtail Nil
tail :: Vec (n + 1) a -> Vec n a
tail (_ :> xs) = unsafeCoerce xs

{-# NOINLINE last #-}
-- | Extract the last element of a vector
--
-- >>> last (1:>2:>3:>Nil)
-- 3
-- >>> last Nil
--   <interactive>
--       Couldn't match type ‘1’ with ‘0’
--       Expected type: Vec (0 + 1) a
--         Actual type: Vec 0 a
--       In the first argument of ‘vlast’, namely ‘Nil’
--       In the expression: vlast Nil
last :: Vec (n + 1) a -> a
last (x :> Nil)     = x
last (_ :> y :> ys) = last (y :> ys)

{-# NOINLINE init #-}
-- | Extract all the elements of a vector except the last element
--
-- >>> init (1:>2:>3:>Nil)
-- <1,2>
-- >>> init Nil
--   <interactive>
--       Couldn't match type ‘1’ with ‘0’
--       Expected type: Vec (0 + 1) a
--         Actual type: Vec 0 a
--       In the first argument of ‘vinit’, namely ‘Nil’
--       In the expression: vinit Nil
init :: Vec (n + 1) a -> Vec n a
init (_ :> Nil)     = unsafeCoerce Nil
init (x :> y :> ys) = unsafeCoerce (x :> init (y :> ys))

{-# INLINEABLE shiftInAt0 #-}
-- | Shift in elements to the head of a vector, bumping out elements at the
-- tail. The result is a tuple containing:
--
-- * The new vector
-- * The shifted out elements
--
-- >>> shiftInAt0 (1 :> 2 :> 3 :> 4 :> Nil) ((-1) :> 0 :> Nil)
-- (<-1,0,1,2,>,<3,4>)
-- >>> shiftInAt0 (1 :> Nil) ((-1) :> 0 :> Nil)
-- (<-1>,<0,1>)
shiftInAt0 :: KnownNat n
           => Vec n a -- ^ The old vector
           -> Vec m a -- ^ The elements to shift in at the head
           -> (Vec n a, Vec m a) -- ^ (The new vector, shifted out elements)
shiftInAt0 xs ys = splitAtI (unsafeCoerce zs)
  where
    zs = ys ++ xs

{-# INLINEABLE shiftInAtN #-}
-- | Shift in element to the tail of a vector, bumping out elements at the head.
-- The result is a tuple containing:
--
-- * The new vector
-- * The shifted out elements
--
-- >>> shiftInAtN (1 :> 2 :> 3 :> 4 :> Nil) (5 :> 6 :> Nil)
-- (<3,4,5,6>,<1,2>)
-- >>> shiftInAtN (1 :> Nil) (2 :> 3 :> Nil)
-- (<3>,<1,2>)
shiftInAtN :: KnownNat m
           => Vec n a -- ^ The old vector
           -> Vec m a -- ^ The elements to shift in at the tail
           -> (Vec n a,Vec m a) -- ^ (The new vector, shifted out elements)
shiftInAtN xs ys = (zsR, zsL)
  where
    zs        = xs ++ ys
    (zsL,zsR) = splitAtI (unsafeCoerce zs)

infixl 5 <:
{-# INLINEABLE (<:) #-}
-- | Add an element to the tail of a vector.
--
-- >>> (3:>4:>5:>Nil) <: 1
-- <3,4,5,1>
-- >>> :t (3:>4:>5:>Nil) <: 1
-- (3:>4:>5:>Nil) <: 1 :: Num a => Vec 4 a
(<:) :: Vec n a -> a -> Vec (n + 1) a
xs <: x = xs ++ singleton x

infixr 4 +>>
{-# INLINEABLE (+>>) #-}
-- | Add an element to the head of a vector, and extract all but the last
-- element.
--
-- >>> 1 +>> (3:>4:>5:>Nil)
-- <1,3,4>
-- >>> 1 +>> Nil
-- <>
(+>>) :: KnownNat n => a -> Vec n a -> Vec n a
s +>> xs = fst (shiftInAt0 xs (singleton s))

infixl 4 <<+
{-# INLINEABLE (<<+) #-}
-- | Add an element to the tail of a vector, and extract all but the first
-- element.
--
-- >>> (3:>4:>5:>Nil) <<+ 1
-- <4,5,1>
-- >>> Nil <<+ 1
-- <>
(<<+) :: KnownNat n => Vec n a -> a -> Vec n a
xs <<+ s = fst (shiftInAtN xs (singleton s))

{-# INLINEABLE shiftOutFrom0 #-}
-- | Shift @m@ elements out from the head of a vector, filling up the tail with
-- 'Default' values. The result is a tuple containing:
--
-- * The new vector
-- * The shifted out values
--
-- >>> shiftOutFrom0 d2 ((1 :> 2 :> 3 :> 4 :> 5 :> Nil) :: Vec 5 Integer)
-- (<3,4,5,0,0>,<1,2>)
shiftOutFrom0 :: (Default a, KnownNat m)
              => SNat m        -- ^ @m@, the number of elements to shift out
              -> Vec (m + n) a -- ^ The old vector
              -> (Vec (m + n) a, Vec m a)
              -- ^ (The new vector, shifted out elements)
shiftOutFrom0 m xs = shiftInAtN xs (replicate m def)

{-# INLINEABLE shiftOutFromN #-}
-- | Shift @m@ elements out from the tail of a vector, filling up the head with
-- 'Default' values. The result is a tuple containing:
--
-- * The new vector
-- * The shifted out values
--
-- >>> shiftOutFromN d2 ((1 :> 2 :> 3 :> 4 :> 5 :> Nil) :: Vec 5 Integer)
-- (<0,0,1,2,3>,<4,5>)
shiftOutFromN :: (Default a, KnownNat (m + n))
              => SNat m        -- ^ @m@, the number of elements to shift out
              -> Vec (m + n) a -- ^ The old vector
              -> (Vec (m + n) a, Vec m a)
              -- ^ (The new vector, shifted out elements)
shiftOutFromN m xs = shiftInAt0 xs (replicate m def)

infixr 5 ++
{-# NOINLINE (++) #-}
-- | Append two vectors
--
-- >>> (1:>2:>3:>Nil) ++ (7:>8:>Nil)
-- <1,2,3,7,8>
(++) :: Vec n a -> Vec m a -> Vec (n + m) a
Nil       ++ ys = ys
(x :> xs) ++ ys = unsafeCoerce (x :> (xs ++ ys))

{-# NOINLINE splitAt #-}
-- | Split a vector into two vectors at the given point
--
-- >>> splitAt (snat :: SNat 3) (1:>2:>3:>7:>8:>Nil)
-- (<1,2,3>, <7,8>)
-- >>> splitAt d3 (1:>2:>3:>7:>8:>Nil)
-- (<1,2,3>, <7,8>)
splitAt :: SNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
splitAt n xs = splitAtU (toUNat n) xs

splitAtU :: UNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
splitAtU UZero     ys        = (Nil,ys)
splitAtU (USucc s) (y :> ys) = let (as,bs) = splitAtU s (unsafeCoerce ys)
                               in  (y :> as, bs)

{-# INLINEABLE splitAtI #-}
-- | Split a vector into two vectors where the length of the two is determined
-- by the context
--
-- >>> splitAtI (1:>2:>3:>7:>8:>Nil) :: (Vec 2 Int, Vec 3 Int)
-- (<1,2>,<3,7,8>)
splitAtI :: KnownNat m => Vec (m + n) a -> (Vec m a, Vec n a)
splitAtI = withSNat splitAt

{-# NOINLINE concat #-}
-- | Concatenate a vector of vectors
--
-- >>> vconcat ((1:>2:>3:>Nil) :> (4:>5:>6:>Nil) :> (7:>8:>9:>Nil) :> (10:>11:>12:>Nil) :> Nil)
-- <1,2,3,4,5,6,7,8,9,10,11,12>
concat :: Vec n (Vec m a) -> Vec (n * m) a
concat Nil       = Nil
concat (x :> xs) = unsafeCoerce (x ++ (concat xs))

{-# NOINLINE unconcat #-}
-- | Split a vector of (n * m) elements into a vector of vectors with length m,
-- where m is given
--
-- >>> vunconcat d4 (1:>2:>3:>4:>5:>6:>7:>8:>9:>10:>11:>12:>Nil)
-- <<1,2,3,4>,<5,6,7,8>,<9,10,11,12>>
unconcat :: KnownNat n => SNat m -> Vec (n * m) a -> Vec n (Vec m a)
unconcat n xs = unconcatU (withSNat toUNat) (toUNat n) xs

unconcatU :: UNat n -> UNat m -> Vec (n * m) a -> Vec n (Vec m a)
unconcatU UZero      _ _  = Nil
unconcatU (USucc n') m ys = let (as,bs) = splitAtU m (unsafeCoerce ys)
                            in  as :> unconcatU n' m bs

{-# INLINEABLE unconcatI #-}
-- | Split a vector of (n * m) elements into a vector of vectors with length m,
-- where m is determined by the context
--
-- >>> vunconcatI (1:>2:>3:>4:>5:>6:>7:>8:>9:>10:>11:>12:>Nil) :: Vec 2 (Vec 6 Int)
-- <<1,2,3,4,5,6>,<7,8,9,10,11,12>>
unconcatI :: (KnownNat n, KnownNat m) => Vec (n * m) a -> Vec n (Vec m a)
unconcatI = withSNat unconcat

{-# NOINLINE merge #-}
-- | Merge two vectors, alternating their elements, i.e.,
--
-- >>> merge (1 :> 2 :> 3 :> 4 :> Nil) (5 :> 6 :> 7 :> 8 :> Nil)
-- <1,5,2,6,3,7,4,8>
merge :: Vec n a -> Vec n a -> Vec (n + n) a
merge Nil       Nil       = Nil
merge (x :> xs) (y :> ys) = unsafeCoerce
                              (x :> y :> (merge xs (unsafeCoerce ys)))

{-# NOINLINE reverse #-}
-- | Returns the elements in a vector in reverse order
--
-- >>> reverse (1:>2:>3:>4:>Nil)
-- <4,3,2,1>
reverse :: Vec n a -> Vec n a
reverse Nil        = Nil
reverse (x :> xs)  = reverse xs <: x

{-# NOINLINE map #-}
-- | 'map' @f xs@ is the vector obtained by applying @f@ to each element
-- of @xs@, i.e.,
--
-- > map f (x1 :> x2 :>  ... :> xn :> Nil) == (f x1 :> f x2 :> ... :> f xn :> Nil)
map :: (a -> b) -> Vec n a -> Vec n b
map _ Nil       = Nil
map f (x :> xs) = f x :> map f xs

{-# NOINLINE zipWith #-}
-- | 'zipWith' generalises 'zip' by zipping with the function given
-- as the first argument, instead of a tupling function.
-- For example, @'zipWith' (+)@ is applied to two vectors to produce the
-- vector of corresponding sums.
--
-- > zipWith f (x1 :> x2 :> ... xn :> Nil) (y1 :> y2 :> ... :> yn :> Nil) == (f x1 y1 :> f x2 y2 :> ... :> f xn yn :> Nil)
zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith _ Nil       Nil       = Nil
zipWith f (x :> xs) (y :> ys) = f x y :> (zipWith f xs (unsafeCoerce ys))

{-# NOINLINE foldr #-}
-- | 'vfoldr', applied to a binary operator, a starting value (typically
-- the right-identity of the operator), and a vector, reduces the vector
-- using the binary operator, from right to left:
--
-- > foldr f z (x1 :> ... :> xn1 :> xn :> Nil) == x1 `f` (... (xn1 `f` (xn `f` z))...)
-- > foldr r z Nil                             == z
--
-- >>> foldr (/) 1 (5 :> 4 :> 3 :> 2 :> Nil)
-- 1.875
--
-- __NB__: @"'foldr' f z xs"@ produces a linear structure, which has a depth, or
-- delay, of O(@'length' xs@). Use 'fold' if your binary operator @f@ is
-- associative, as @"'fold' f xs"@ produces a structure with a depth of
-- O(log_2(@'length' xs@)).
foldr :: (a -> b -> b) -> b -> Vec n a -> b
foldr _ z Nil       = z
foldr f z (x :> xs) = f x (foldr f z xs)

{-# NOINLINE foldl #-}
-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a vector, reduces the vector
-- using the binary operator, from left to right:
--
-- > foldl f z (x1 :> x2 :> ... :> xn :> Nil) == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- > foldl f z Nil                            == z
--
-- >>> foldl (/) 1 (5 :> 4 :> 3 :> 2 :> Nil)
-- 8.333333333333333e-3
--
-- __NB__: @"'foldl' f z xs"@ produces a linear structure, which has a depth, or
-- delay, of O(@'length' xs@). Use 'fold' if your binary operator @f@ is
-- associative, as @"'fold' f xs"@ produces a structure with a depth of
-- O(log_2(@'length' xs@)).
foldl :: (b -> a -> b) -> b -> Vec n a -> b
foldl _ z Nil       = z
foldl f z (x :> xs) = foldl f (f z x) xs

{-# INLINABLE foldr1 #-}
-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty vectors.
--
-- > foldr1 f (x1 :> ... :> xn2 :> xn1 :> xn :> Nil) == x1 `f` (... (xn2 `f` (xn1 `f` xn))...)
-- > foldr1 f (x1 :> Nil)                            == x1
-- > foldr1 f Nil                                    == TYPE ERROR
--
-- >>> foldr1 (/) (5 :> 4 :> 3 :> 2 :> 1 :> Nil)
-- 1.875
--
-- __NB__: @"'foldr1' f z xs"@ produces a linear structure, which has a depth,
-- or delay, of O(@'length' xs@). Use 'fold' if your binary operator @f@ is
-- associative, as @"'fold' f xs"@ produces a structure with a depth of
-- O(log_2(@'length' xs@)).
foldr1 :: (a -> a -> a) -> Vec (n + 1) a -> a
foldr1 f xs = foldr f (last xs) (init xs)

{-# INLINEABLE foldl1 #-}
-- | 'foldl1' is a variant of 'foldl' that has no starting value argument,
-- and thus must be applied to non-empty vectors.
--
-- > foldl1 f (x1 :> x2 :> x3 :> ... :> xn :> Nil) == (...((x1 `f` x2) `f` x3) `f`...) `f` xn
-- > foldl1 f (x1 :> Nil)                          == x1
-- > foldl1 f Nil                                  == TYPE ERROR
--
-- >>> foldl1 (/) (1 :> 5 :> 4 :> 3 :> 2 :> Nil)
-- 8.333333333333333e-3
--
-- __NB__: @"'foldl1' f z xs"@ produces a linear structure, which has a depth,
-- or delay, of O(@'length' xs@). Use 'fold' if your binary operator @f@ is
-- associative, as @"'fold' f xs"@ produces a structure with a depth of
-- O(log_2(@'length' xs@)).
foldl1 :: (a -> a -> a) -> Vec (n + 1) a -> a
foldl1 f xs = foldl f (head xs) (tail xs)

{-# NOINLINE fold #-}
-- | 'fold' is a variant of 'foldr1' and 'foldl1', but instead of reducing from
-- right to left, or left to right, it reduces a vector using a tree-like
-- structure. The depth, or delay, of the structure produced by @'fold' f xs@,
-- is hence @O(log_2('length' xs))@, and not @O('length' xs)@.
--
-- __NB__: The binary operator @f@ in @'fold' f xs@ must be associative.
--
-- > fold f (x1 :> x2 :> ... :> xn1 :> xn :> Nil) == ((x1 `f` x2) `f` ...) `f` (... `f` (xn1 `f` xn))
-- > fold f (x1 :> Nil)                           == x1
-- > fold f Nil                                   == TYPE ERROR
--
-- >>> fold (+) (5 :> 4 :> 3 :> 2 :> 1 :> Nil)
-- 15
fold :: (a -> a -> a) -> Vec (n + 1) a -> a
fold f vs = fold' (toList vs)
  where
    fold' [x] = x
    fold' xs  = fold' ys `f` fold' zs
      where
        (ys,zs) = P.splitAt (P.length xs `div` 2) xs

{-# INLINEABLE scanl #-}
-- | 'scanl' is similar to 'foldl', but returns a vector of successive reduced
-- values from the left:
--
-- > scanl f z (x1 :> x2 :> ... :> Nil) == z :> (z `f` x1) :> ((z `f` x1) `f` x2) :> ... :> Nil
--
-- >>> scanl (+) 0 (5 :> 4 :> 3 :> 2 :> Nil)
-- <0,5,9,12,14>
--
-- __NB__:
--
-- > last (scanl f z xs) == foldl f z xs
scanl :: KnownNat n => (b -> a -> b) -> b -> Vec n a -> Vec (n + 1) b
scanl f z xs = ws
  where
    ws = z :> zipWith f (lazyV (init ws)) xs

{-# INLINEABLE scanl1 #-}
-- | 'scanl1' is a variant of 'scanl' that has no starting element:
--
-- > scanl1 f (x1 :> x2 :> x3 :> ... :> Nil) == x1 :> (x1 `f` x2) :> ((x1 `f` x2) `f` x3) :> ... :> Nil
--
-- >>> scanl1 (+) (5 :> 4 :> 3 :> 2 :> Nil)
-- <5,9,12,14>
--
-- __NB__:
--
-- > last (scanl1 f xs) == foldl1 f xs
scanl1 :: KnownNat n => (a -> a -> a) -> Vec n a -> Vec n a
scanl1 f xs = init (scanl f (head xs') (tail xs'))
  where
    xs' = xs <: undefined

{-# INLINEABLE scanr #-}
-- | 'scanr' is similar to 'foldr', but returns a vector of successive reduced
-- values from the right:
--
-- > scanr f z (... :> xn1 :> xn :> Nil) == ... :> (xn1 `f` (xn `f` z)) :> (xn `f` z) :> z :> Nil
--
-- >>> scanr (+) 0 (5 :> 4 :> 3 :> 2 :> Nil)
-- <14,9,5,2,0>
--
-- __NB__:
--
-- > head (scanr f z xs) == foldr f z xs
scanr :: KnownNat n => (a -> b -> b) -> b -> Vec n a -> Vec (n + 1) b
scanr f z xs = ws
  where
    ws = zipWith f xs (lazyV (tail ws)) <: z

{-# INLINEABLE scanr1 #-}
-- | 'scanr1' is a variant of 'scanr' that has no starting element:
--
-- > scanr f (... :> xn2 :> xn1 :> xn :> Nil) == ... :> (xn3 `f` (xn2 `f` xn1)) :> (xn1 `f` xn) :> xn :> Nil
--
-- >>> scanr1 (+) (5 :> 4 :> 3 :> 2 :> Nil)
-- <14,9,5,2>
--
-- __NB__:
--
-- > head (scanr1 f xs) == foldr1 f xs
scanr1 :: KnownNat n => (a -> a -> a) -> Vec n a -> Vec n a
scanr1 f xs = tail (scanr f (last xs') (init xs'))
  where
    xs' = undefined :> xs

{-# INLINEABLE mapAccumL #-}
-- | The 'mapAccumL' function behaves like a combination of 'map' and 'foldl';
-- it applies a function to each element of a vector, passing an accumulating
-- parameter from left to right, and returning a final value of this accumulator
-- together with the new vector.
--
-- >>> mapAccumL (\acc x -> (acc + x,acc + 1)) 0 (1 :> 2 :> 3 :> 4 :> Nil)
-- (10,<1,2,4,7>)
mapAccumL :: KnownNat n => (acc -> x -> (acc,y)) -> acc -> Vec n x
          -> (acc,Vec n y)
mapAccumL f acc xs = (acc',ys)
  where
    ws   = scanl (\l r -> f (fst l) r) (acc,undefined) xs
    acc' = fst (last ws)
    ys   = map snd (tail ws)

{-# INLINEABLE mapAccumR #-}
-- | The 'mapAccumR' function behaves like a combination of 'map' and 'foldr';
-- it applies a function to each element of a vector, passing an accumulating
-- parameter from right to left, and returning a final value of this accumulator
-- together with the new vector.
--
-- >>> mapAccumR (\acc x -> (acc + x,acc + 1)) 0 (1 :> 2 :> 3 :> 4 :> Nil)
-- (10,<10,8,5,1>)
mapAccumR :: KnownNat n => (acc -> x -> (acc,y)) -> acc -> Vec n x
          -> (acc, Vec n y)
mapAccumR f acc xs = (acc',ys)
  where
    ws   = scanr (\l r -> f (fst r) l) (acc,undefined) xs
    acc' = fst (head ws)
    ys   = map snd (init ws)

{-# INLINEABLE zip #-}
-- | 'zip' takes two vectors and returns a vector of corresponding pairs.
--
-- >>> zip (1:>2:>3:>4:>Nil) (4:>3:>2:>1:>Nil)
-- <(1,4),(2,3),(3,2),(4,1)>
zip :: Vec n a -> Vec n b -> Vec n (a,b)
zip = zipWith (,)

{-# INLINEABLE unzip #-}
-- | 'unzip' transforms a vector of pairs into a vector of first components
-- and a vector of second components.
--
-- >>> unzip ((1,4):>(2,3):>(3,2):>(4,1):>Nil)
-- (<1,2,3,4>,<4,3,2,1>)
unzip :: Vec n (a,b) -> (Vec n a, Vec n b)
unzip xs = (map fst xs, map snd xs)

indexM_integer :: Vec n a -> Integer -> Maybe a
indexM_integer Nil       _ = Nothing
indexM_integer (x :> _)  0 = Just x
indexM_integer (_ :> xs) n = indexM_integer xs (n-1)

{-# NOINLINE index_integer #-}
index_integer :: KnownNat n => Vec n a -> Integer -> a
index_integer xs i = case indexM_integer xs i of
    Just a  -> a
    Nothing -> error (P.concat [ "(!!): Index "
                               , show i
                               , " is out of bounds [0.."
                               , show (maxIndex xs)
                               , "]"
                               ])

{-# INLINEABLE (!!) #-}
-- | Vector index (subscript) operator.
--
-- __NB__: vector elements have an __ASCENDING__ subscript starting from 0 and
-- ending at 'maxIndex'.
--
-- >>> (1:>2:>3:>4:>5:>Nil) !! 4
-- 5
-- >>> (1:>2:>3:>4:>5:>Nil) !! maxIndex
-- 5
-- >>> (1:>2:>3:>4:>5:>Nil) !! 1
-- 2
-- >>> (1:>2:>3:>4:>5:>Nil) !! 14
-- *** Exception: (!!): Index 14 is out of bounds [0..4]
(!!) :: (KnownNat n, Integral i) => Vec n a -> i -> a
xs !! i = index_integer xs (toInteger i)

{-# NOINLINE maxIndex #-}
-- | Index (subscript) of the last element in a 'Vec'tor
--
-- >>> maxIndex (6 :> 7 :> 8 :> Nil)
-- 2
maxIndex :: KnownNat n => Vec n a -> Integer
maxIndex = subtract 1 . length

{-# NOINLINE length #-}
-- | Length of a 'Vec'tor as an Integer
--
-- >>> length (6 :> 7 :> 8 :> Nil)
-- 3
length :: KnownNat n => Vec n a -> Integer
length = natVal . asNatProxy

replaceM_integer :: Vec n a -> Integer -> a -> Maybe (Vec n a)
replaceM_integer Nil       _ _ = Nothing
replaceM_integer (_ :> xs) 0 y = Just (y :> xs)
replaceM_integer (x :> xs) n y = case replaceM_integer xs (n-1) y of
                                    Just xs' -> Just (x :> xs')
                                    Nothing  -> Nothing

{-# NOINLINE replace_integer #-}
replace_integer :: KnownNat n => Vec n a -> Integer -> a -> Vec n a
replace_integer xs i a = case replaceM_integer xs i a of
  Just ys -> ys
  Nothing -> error (P.concat [ "replace: Index "
                             , show i
                             , " is out of bounds [0.."
                             , show (maxIndex xs)
                             , "]"
                             ])

{-# INLINEABLE replace #-}
-- | Replace an element of a vector at the given index (subscript).
--
-- __NB__: vector elements have an __ASCENDING__ subscript starting from 0 and
-- ending at 'maxIndex'.
--
-- >>> replace (1:>2:>3:>4:>5:>Nil) 3 7
-- <1,2,3,7,5>
-- >>> replace (1:>2:>3:>4:>5:>Nil) 0 7
-- <7,2,3,4,5>
-- >>> replace (1:>2:>3:>4:>5:>Nil) 9 7
-- <*** Exception: replace: Index 9 is out of bounds [0..4]
replace :: (KnownNat n, Integral i) => Vec n a -> i -> a -> Vec n a
replace xs i y = replace_integer xs (toInteger i) y

{-# NOINLINE take #-}
-- | 'take' @n@, applied to a vector @xs@, returns the @n@-length prefix of @xs@
--
-- >>> take (snat :: SNat 3) (1:>2:>3:>4:>5:>Nil)
-- <1,2,3>
-- >>> take d3               (1:>2:>3:>4:>5:>Nil)
-- <1,2,3>
-- >>> take d0               (1:>2:>Nil)
-- <>
-- >>> take d4               (1:>2:>Nil)
--   <interactive>
--       Couldn't match type ‘4 + n0’ with ‘2’
--       The type variable ‘n0’ is ambiguous
--       Expected type: Vec (4 + n0) a
--         Actual type: Vec (1 + 1) a
--       In the second argument of ‘vtake’, namely ‘(1 :> 2 :> Nil)’
--       In the expression: vtake d4 (1 :> 2 :> Nil)
--       In an equation for ‘it’: it = vtake d4 (1 :> 2 :> Nil)
take :: SNat m -> Vec (m + n) a -> Vec m a
take n = fst . splitAt n

{-# INLINEABLE takeI #-}
-- | 'takeI' @xs@, returns the prefix of @xs@ as demanded by the context
--
-- >>> takeI (1:>2:>3:>4:>5:>Nil) :: Vec 2 Int
-- <1,2>
takeI :: KnownNat m => Vec (m + n) a -> Vec m a
takeI = withSNat take

{-# NOINLINE drop #-}
-- | 'drop' @n xs@ returns the suffix of @xs@ after the first @n@ elements
--
-- >>> drop (snat :: SNat 3) (1:>2:>3:>4:>5:>Nil)
-- <4,5>
-- >>> drop d3               (1:>2:>3:>4:>5:>Nil)
-- <4,5>
-- >>> drop d0               (1:>2:>Nil)
-- <1,2>
-- >>> drop d4               (1:>2:>Nil)
--   <interactive>
--       Couldn't match expected type ‘2’ with actual type ‘4 + n0’
--       The type variable ‘n0’ is ambiguous
--       In the first argument of ‘print’, namely ‘it’
--       In a stmt of an interactive GHCi command: print it
drop :: SNat m -> Vec (m + n) a -> Vec n a
drop n = snd . splitAt n

{-# INLINEABLE dropI #-}
-- | 'dropI' @xs@, returns the suffix of @xs@ as demanded by the context
--
-- >>> dropI (1:>2:>3:>4:>5:>Nil) :: Vec 2 Int
-- <4,5>
dropI :: KnownNat m => Vec (m + n) a -> Vec n a
dropI = withSNat drop

{-# NOINLINE exact #-}
-- | 'exact' @n xs@ returns @n@'th element of @xs@
--
-- __NB__: vector elements have an __ASCENDING__ subscript starting from 0 and
-- ending at 'maxIndex'.
--
-- >>> exact (snat :: SNat 1) (1:>2:>3:>4:>5:>Nil)
-- 4
-- >>> exact d1               (1:>2:>3:>4:>5:>Nil)
-- 4
exact :: SNat m -> Vec (m + (n + 1)) a -> a
exact n xs = head $ snd $ splitAt n xs

{-# NOINLINE select #-}
-- | 'select' @f s n xs@ selects @n@ elements with stepsize @s@ and
-- offset @f@ from @xs@
--
-- >>> select (snat :: SNat 1) (snat :: SNat 2) (snat :: SNat 3) (1:>2:>3:>4:>5:>6:>7:>8:>Nil)
-- <2,4,6>
-- >>> select d1 d2 d3 (1:>2:>3:>4:>5:>6:>7:>8:>Nil)
-- <2,4,6>
select :: ((f + (s * n) + 1) <= i)
       => SNat f
       -> SNat s
       -> SNat (n + 1)
       -> Vec i a
       -> Vec (n + 1) a
select f s n xs = select' (toUNat n) $ drop f (unsafeCoerce xs)
  where
    select' :: UNat n -> Vec m a -> Vec n a
    select' UZero      _           = Nil
    select' (USucc n') vs@(x :> _) = x :> select' n' (drop s (unsafeCoerce vs))

{-# INLINEABLE selectI #-}
-- | 'selectI' @f s xs@ selects as many elements as demanded by the context
-- with stepsize @s@ and offset @f@ from @xs@
--
-- >>> selectI d1 d2 (1:>2:>3:>4:>5:>6:>7:>8:>Nil) :: Vec 2 Int
-- <2,4>
selectI :: ((f + (s * n) + 1) <= i, KnownNat (n + 1))
        => SNat f
        -> SNat s
        -> Vec i a
        -> Vec (n + 1) a
selectI f s xs = withSNat (\n -> select f s n xs)

{-# NOINLINE replicate #-}
-- | 'replicate' @n a@ returns a vector that has @n@ copies of @a@
--
-- >>> replicate (snat :: SNat 3) 6
-- <6,6,6>
-- >>> replicate d3 6
-- <6,6,6>
replicate :: SNat n -> a -> Vec n a
replicate n a = replicateU (toUNat n) a

replicateU :: UNat n -> a -> Vec n a
replicateU UZero     _ = Nil
replicateU (USucc s) x = x :> replicateU s x

{-# INLINEABLE repeat #-}
-- | 'repeat' @a@ creates a vector with as many copies of @a@ as demanded by the
-- context
--
-- >>> repeat 6 :: Vec 5 Int
-- <6,6,6,6,6>
repeat :: KnownNat n => a -> Vec n a
repeat = withSNat replicate

{-# NOINLINE iterate #-}
-- | 'iterate' @n f x@ returns a vector starting with @x@ followed by @n@
-- repeated applications of @f@ to @x@
--
-- > iterate (snat :: SNat 4) f x == (x :> f x :> f (f x) :> f (f (f x)) :> Nil)
-- > iterate d4 f x               == (x :> f x :> f (f x) :> f (f (f x)) :> Nil)
--
-- >>> iterate d4 (+1) 1
-- <1,2,3,4>
iterate :: SNat n -> (a -> a) -> a -> Vec n a
iterate n f a = iterateU (toUNat n) f a

iterateU :: UNat n -> (a -> a) -> a -> Vec n a
iterateU UZero     _ _ = Nil
iterateU (USucc s) g x = x :> iterateU s g (g x)

{-# INLINEABLE iterateI #-}
-- | 'iterate' @f x@ returns a vector starting with @x@ followed by @n@
-- repeated applications of @f@ to @x@, where @n@ is determined by the context
--
-- > iterateI f x :: Vec 3 a == (x :> f x :> f (f x) :> Nil)
--
-- >>> iterateI (+1) 1 :: Vec 3 Int
-- <1,2,3>
iterateI :: KnownNat n => (a -> a) -> a -> Vec n a
iterateI = withSNat iterate

{-# INLINEABLE generate #-}
-- | 'generate' @n f x@ returns a vector with @n@ repeated applications of @f@
-- to @x@
--
-- > generate (snat :: SNat 4) f x == (f x :> f (f x) :> f (f (f x)) :> f (f (f (f x))) :> Nil)
-- > generate d4 f x               == (f x :> f (f x) :> f (f (f x)) :> f (f (f (f x))) :> Nil)
--
-- >>> generate d4 (+1) 1
-- <2,3,4,5>
generate :: SNat n -> (a -> a) -> a -> Vec n a
generate n f a = iterate n f (f a)

{-# INLINEABLE generateI #-}
-- | 'generate' @f x@ returns a vector with @n@ repeated applications of @f@
-- to @x@, where @n@ is determined by the context
--
-- > generateI f x :: Vec 3 a == (f x :> f (f x) :> f (f (f x)) :> Nil)
--
-- >>> generateI (+1) 1 :: Vec 3 Int
-- <2,3,4>
generateI :: KnownNat n => (a -> a) -> a -> Vec n a
generateI = withSNat generate

{-# INLINEABLE toList #-}
-- | Convert a vector to a list
--
-- >>> toList (1:>2:>3:>Nil)
-- [1,2,3]
--
-- __NB__: Not synthesisable
toList :: Vec n a -> [a]
toList = foldr (:) []

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
-- > sortV xs = map fst sorted <: (snd (last sorted))
-- >  where
-- >    lefts  = head xs :> map snd (init sorted)
-- >    rights = tail xs
-- >    sorted = zipWith compareSwapL lefts rights
-- >
-- > -- Compare and swap
-- > compareSwapL a b = if a < b then (a,b)
-- >                             else (b,a)
--
-- Will not terminate because 'zipWith' is too strict in its second argument:
--
-- >>> sortV (4 :> 1 :> 2 :> 3 :> Nil)
-- <*** Exception: <<loop>>
--
-- In this case, adding 'lazyV' on 'zipWith's second argument:
--
-- > sortVL xs = map fst sorted <: (snd (last sorted))
-- >  where
-- >    lefts  = head xs :> map snd (init sorted)
-- >    rights = tail xs
-- >    sorted = zipWith compareSwapL (lazyV lefts) rights
--
-- Results in a successful computation:
--
-- >>> sortVL (4 :> 1 :> 2 :> 3 :> Nil)
-- <1,2,3,4>
lazyV :: KnownNat n
      => Vec n a
      -> Vec n a
lazyV = lazyV' (repeat undefined)
  where
    lazyV' :: Vec n a -> Vec n a -> Vec n a
    lazyV' Nil       _  = Nil
    lazyV' (_ :> xs) ys = head ys :> lazyV' xs (tail ys)

{-# NOINLINE concatBitVector# #-}
concatBitVector# :: KnownNat m
                 => Vec n (BitVector m)
                 -> BitVector (n * m)
concatBitVector# Nil       = 0
concatBitVector# (x :> xs) = unsafeCoerce (concatBitVector# xs ++# x)

{-# NOINLINE unconcatBitVector# #-}
unconcatBitVector# :: (KnownNat n, KnownNat m)
                   => BitVector (n * m)
                   -> Vec n (BitVector m)
unconcatBitVector# bv = withSNat (\s -> ucBV (toUNat s) bv)

{-# INLINE ucBV #-}
ucBV :: forall n m . KnownNat m
     => UNat n -> BitVector (n * m) -> Vec n (BitVector m)
ucBV UZero     _  = Nil
ucBV (USucc n) bv = let (bv',x :: BitVector m) = split (unsafeCoerce bv)
                    in  x :> ucBV n bv'
