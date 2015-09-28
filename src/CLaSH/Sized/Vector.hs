{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module CLaSH.Sized.Vector
  ( -- * 'Vec'tor data type
    Vec(..)
    -- * Accessors
    -- ** Length information
  , length, maxIndex, lengthS
    -- ** Indexing
  , (!!), head, last, at
    -- ** Extracting subvectors (slicing)
  , tail, init
  , take, takeI, drop, dropI
  , select, selectI
    -- *** Splitting
  , splitAt, splitAtI
  , unconcat, unconcatI
    -- * Construction
    -- ** Initialisation
  , singleton
  , replicate, replicateI, repeat
  , iterate, iterateI, generate, generateI
    -- *** Initialisation from a list
  , v
    -- ** Concatenation
  , pattern (:>), pattern (:<)
  , (++), (+>>), (<<+), concat
  , shiftInAt0, shiftInAtN , shiftOutFrom0, shiftOutFromN
  , merge
    -- * Modifying vectors
  , replace
    -- ** Permutations
  , permute, backpermute, scatter, gather
    -- *** Specialised permutations
  , reverse, transpose, interleave
  , rotateLeft, rotateRight, rotateLeftS, rotateRightS
    -- * Element-wise operations
    -- ** Mapping
  , map
    -- ** Zipping
  , zipWith, zipWith3
  , zip, zip3
    -- ** Unzipping
  , unzip, unzip3
    -- * Folding
  , foldr, foldl, foldr1, foldl1, fold
    -- ** Specialised folds
  , dfold, vfold
    -- * Prefix sums (scans)
  , scanl, scanr, postscanl, postscanr
  , mapAccumL, mapAccumR
    -- * Stencil computations
  , stencil1d, stencil2d
  , windows1d, windows2d
    -- * Misc
  , toList, lazyV, asNatProxy
    -- * Primitives
    -- ** 'Eq' instance
  , eq#
  , neq#
    -- ** 'Traversable' instance
  , traverse#
    -- ** 'BitPack' instance
  , concatBitVector#
  , unconcatBitVector#
  )
where

import Control.Lens               (Index, Ixed (..), IxValue)
import Data.Default               (Default (..))
import qualified Data.Foldable    as F
import Data.Proxy                 (Proxy (..))
import Data.Singletons.Prelude    (TyFun,Apply,type ($))
import GHC.TypeLits               (CmpNat, KnownNat, Nat, type (+), type (*),
                                   natVal)
import GHC.Base                   (Int(I#),Int#,isTrue#)
import GHC.Prim                   ((==#),(<#),(-#))
import Language.Haskell.TH        (ExpQ)
import Language.Haskell.TH.Syntax (Lift(..))
import Prelude                    hiding ((++), (!!), concat, drop, foldl,
                                          foldl1, foldr, foldr1, head, init,
                                          iterate, last, length, map, repeat,
                                          replicate, reverse, scanl, scanr,
                                          splitAt, tail, take, unzip, unzip3,
                                          zip, zip3, zipWith, zipWith3)
import qualified Prelude          as P
import Test.QuickCheck            (Arbitrary (..), CoArbitrary (..))
import Unsafe.Coerce              (unsafeCoerce)

import CLaSH.Promoted.Nat         (SNat (..), UNat (..), snat, withSNat, toUNat)
import CLaSH.Sized.Internal.BitVector (BitVector, (++#), split#)

import CLaSH.Class.BitPack (BitPack (..))

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeFamilies
-- >>> :set -XTypeOperators
-- >>> :set -XTemplateHaskell
-- >>> :set -XFlexibleContexts
-- >>> :set -fplugin GHC.TypeLits.Normalise
-- >>> import CLaSH.Prelude
-- >>> let compareSwapL a b = if a < b then (a,b) else (b,a)
-- >>> :{
-- let sortV xs = map fst sorted :< (snd (last sorted))
--       where
--         lefts  = head xs :> map snd (init sorted)
--         rights = tail xs
--         sorted = zipWith compareSwapL lefts rights
-- :}
--
-- >>> :{
-- let sortVL xs = map fst sorted :< (snd (last sorted))
--       where
--         lefts  = head xs :> map snd (init sorted)
--         rights = tail xs
--         sorted = zipWith compareSwapL (lazyV lefts) rights
-- :}
--
-- >>> :{
-- let sortV_flip xs = map fst sorted :< (snd (last sorted))
--       where
--         lefts  = head xs :> map snd (init sorted)
--         rights = tail xs
--         sorted = zipWith (flip compareSwapL) rights lefts
-- :}
--
-- >>> import Data.Singletons.Prelude
-- >>> data Append (m :: Nat) (a :: *) (f :: TyFun Nat *) :: *
-- >>> type instance Apply (Append m a) l = Vec (l + m) a
-- >>> let append' xs ys = dfold (Proxy :: Proxy (Append m a)) (const (:>)) ys xs
-- >>> let cs a b     = if a > b then (a,b) else (b,a)
-- >>> let csRow y xs = let (y',xs') = mapAccumL cs y xs in xs' :< y'
-- >>> let csSort     = vfold csRow

infixr 5 `Cons`
-- | Fixed size vectors.
--
-- * Lists with their length encoded in their type
-- * 'Vec'tor elements have an __ASCENDING__ subscript starting from 0 and
--   ending at 'maxIndex' (== 'length' - 1).
data Vec :: Nat -> * -> * where
  Nil  :: Vec 0 a
  Cons :: a -> Vec n a -> Vec (n + 1) a
{-# WARNING Cons "Use ':>' instead of 'Cons'" #-}

-- | Add an element to the head of a vector.
--
-- >>> 3:>4:>5:>Nil
-- <3,4,5>
-- >>> let x = 3:>4:>5:>Nil
-- >>> :t x
-- x :: Num a => Vec 3 a
--
-- Can be used as a pattern:
--
-- >>> let f (x :> y :> _) = x + y
-- >>> :t f
-- f :: Num a => Vec ((n + 1) + 1) a -> a
-- >>> f (3:>4:>5:>6:>7:>Nil)
-- 7
--
-- Also in conjunctions with ':<':
--
-- >>> let g (a :> b :> (_ :< y :< x)) = a + b +  x + y
-- >>> :t g
-- g :: Num a => Vec ((((n + 1) + 1) + 1) + 1) a -> a
-- >>> g (1:>2:>3:>4:>5:>Nil)
-- 12
pattern (:>) :: a -> Vec n a -> Vec (n + 1) a
pattern (:>) x xs <- ((\ys -> (head ys,tail ys)) -> (x,xs))
  where
    (:>) x xs = Cons x xs

infixr 5 :>

instance Show a => Show (Vec n a) where
  show vs = "<" P.++ punc vs P.++ ">"
    where
      punc :: Vec m a -> String
      punc Nil        = ""
      punc (x `Cons` Nil) = show x
      punc (x `Cons` xs)  = show x P.++ "," P.++ punc xs

instance Eq a => Eq (Vec n a) where
  (==) = eq#
  (/=) = neq#

{-# NOINLINE eq# #-}
eq# :: Eq a => Vec n a -> Vec n a -> Bool
eq# v1 v2  = foldr (&&) True (zipWith (==) v1 v2)

{-# NOINLINE neq# #-}
neq# :: Eq a => Vec n a -> Vec n a -> Bool
neq# v1 v2 = not (eq# v1 v2)

instance Ord a => Ord (Vec n a) where
  compare x y = foldr f EQ $ zipWith compare x y
    where f EQ   keepGoing = keepGoing
          f done _         = done

instance KnownNat n => Applicative (Vec n) where
  pure      = repeat
  fs <*> xs = zipWith ($) fs xs

instance (KnownNat m, m ~ (n+1)) => F.Foldable (Vec m) where
  foldr   = foldr
  foldl   = foldl
  foldr1  = foldr1
  foldl1  = foldl1
  toList  = toList
  null _  = False
  length  = length
  maximum = foldr1 (\x y -> if x >= y then x else y)
  minimum = foldr1 (\x y -> if x <= y then x else y)
  sum     = foldr1 (+)
  product = foldr1 (*)

instance Functor (Vec n) where
  fmap = map

instance (KnownNat m, m ~ (n+1)) => Traversable (Vec m) where
  traverse = traverse#

{-# NOINLINE traverse# #-}
traverse# :: Applicative f => (a -> f b) -> Vec n a -> f (Vec n b)
traverse# _ Nil           = pure Nil
traverse# f (x `Cons` xs) = Cons <$> f x <*> traverse# f xs

instance (Default a, KnownNat n) => Default (Vec n a) where
  def = repeat def

{-# INLINE singleton #-}
-- | Create a vector of one element
--
-- >>> singleton 5
-- <5>
singleton :: a -> Vec 1 a
singleton = (`Cons` Nil)

{-# NOINLINE head #-}
-- | Extract the first element of a vector
--
-- >>> head (1:>2:>3:>Nil)
-- 1
-- >>> head Nil
-- <BLANKLINE>
-- <interactive>:...
--     Couldn't match type ‘...’ with ‘0’
--     Expected type: Vec ... a
--       Actual type: Vec 0 a
--     In the first argument of ‘head’, namely ‘Nil’
--     In the expression: head Nil
head :: Vec (n + 1) a -> a
head (x `Cons` _) = x

{-# NOINLINE tail #-}
-- | Extract the elements after the head of a vector
--
-- >>> tail (1:>2:>3:>Nil)
-- <2,3>
-- >>> tail Nil
-- <BLANKLINE>
-- <interactive>:...
--     Couldn't match type ‘...’ with ‘0’
--     Expected type: Vec ... a
--       Actual type: Vec 0 a
--     In the first argument of ‘tail’, namely ‘Nil’
--     In the expression: tail Nil
tail :: Vec (n + 1) a -> Vec n a
tail (_ `Cons` xs) = xs

{-# NOINLINE last #-}
-- | Extract the last element of a vector
--
-- >>> last (1:>2:>3:>Nil)
-- 3
-- >>> last Nil
-- <BLANKLINE>
-- <interactive>:...
--     Couldn't match type ‘...’ with ‘0’
--     Expected type: Vec ... a
--       Actual type: Vec 0 a
--     In the first argument of ‘last’, namely ‘Nil’
--     In the expression: last Nil
last :: Vec (n + 1) a -> a
last (x `Cons` Nil)         = x
last (_ `Cons` y `Cons` ys) = last (y `Cons` ys)

{-# NOINLINE init #-}
-- | Extract all the elements of a vector except the last element
--
-- >>> init (1:>2:>3:>Nil)
-- <1,2>
-- >>> init Nil
-- <BLANKLINE>
-- <interactive>:...
--     Couldn't match type ‘...’ with ‘0’
--     Expected type: Vec ... a
--       Actual type: Vec 0 a
--     In the first argument of ‘init’, namely ‘Nil’
--     In the expression: init Nil
init :: Vec (n + 1) a -> Vec n a
init (_ `Cons` Nil)         = Nil
init (x `Cons` y `Cons` ys) = x `Cons` init (y `Cons` ys)

{-# INLINE shiftInAt0 #-}
-- | Shift in elements to the head of a vector, bumping out elements at the
-- tail. The result is a tuple containing:
--
-- * The new vector
-- * The shifted out elements
--
-- >>> shiftInAt0 (1 :> 2 :> 3 :> 4 :> Nil) ((-1) :> 0 :> Nil)
-- (<-1,0,1,2>,<3,4>)
-- >>> shiftInAt0 (1 :> Nil) ((-1) :> 0 :> Nil)
-- (<-1>,<0,1>)
shiftInAt0 :: KnownNat n
           => Vec n a -- ^ The old vector
           -> Vec m a -- ^ The elements to shift in at the head
           -> (Vec n a, Vec m a) -- ^ (The new vector, shifted out elements)
shiftInAt0 xs ys = splitAtI zs
  where
    zs = ys ++ xs

{-# INLINE shiftInAtN #-}
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
    (zsL,zsR) = splitAtI zs

infixl 5 :<
-- | Add an element to the tail of a vector.
--
-- >>> (3:>4:>5:>Nil) :< 1
-- <3,4,5,1>
-- >>> let x = (3:>4:>5:>Nil) :< 1
-- >>> :t x
-- x :: Num a => Vec 4 a
--
-- Can be used as a pattern:
--
-- >>> let f (_ :< y :< x) = y + x
-- >>> :t f
-- f :: Num a => Vec ((n + 1) + 1) a -> a
-- >>> f (3:>4:>5:>6:>7:>Nil)
-- 13
--
-- Also in conjunctions with ':>':
--
-- >>> let g (a :> b :> (_ :< y :< x)) = a + b +  x + y
-- >>> :t g
-- g :: Num a => Vec ((((n + 1) + 1) + 1) + 1) a -> a
-- >>> g (1:>2:>3:>4:>5:>Nil)
-- 12
pattern (:<) :: Vec n a -> a -> Vec (n+1) a
pattern (:<) xs x <- ((\ys -> (init ys,last ys)) -> (xs,x))
  where
    (:<) xs x = xs ++ singleton x

infixr 4 +>>
-- | Add an element to the head of a vector, and extract all but the last
-- element.
--
-- >>> 1 +>> (3:>4:>5:>Nil)
-- <1,3,4>
-- >>> 1 +>> Nil
-- <>
(+>>) :: KnownNat n => a -> Vec n a -> Vec n a
s +>> xs = fst (shiftInAt0 xs (singleton s))
{-# INLINE (+>>) #-}


infixl 4 <<+
-- | Add an element to the tail of a vector, and extract all but the first
-- element.
--
-- >>> (3:>4:>5:>Nil) <<+ 1
-- <4,5,1>
-- >>> Nil <<+ 1
-- <>
(<<+) :: Vec n a -> a -> Vec n a
xs <<+ s = fst (shiftInAtN xs (singleton s))
{-# INLINE (<<+) #-}

-- | Shift /m/ elements out from the head of a vector, filling up the tail with
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
{-# INLINE shiftOutFrom0 #-}

-- | Shift /m/ elements out from the tail of a vector, filling up the head with
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
{-# INLINE shiftOutFromN #-}

infixr 5 ++
-- | Append two vectors.
--
-- >>> (1:>2:>3:>Nil) ++ (7:>8:>Nil)
-- <1,2,3,7,8>
(++) :: Vec n a -> Vec m a -> Vec (n + m) a
Nil           ++ ys = ys
(x `Cons` xs) ++ ys = x `Cons` xs ++ ys
{-# NOINLINE (++) #-}

-- | Split a vector into two vectors at the given point.
--
-- >>> splitAt (snat :: SNat 3) (1:>2:>3:>7:>8:>Nil)
-- (<1,2,3>,<7,8>)
-- >>> splitAt d3 (1:>2:>3:>7:>8:>Nil)
-- (<1,2,3>,<7,8>)
splitAt :: SNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
splitAt n xs = splitAtU (toUNat n) xs
{-# NOINLINE splitAt #-}

splitAtU :: UNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
splitAtU UZero     ys            = (Nil,ys)
splitAtU (USucc s) (y `Cons` ys) = let (as,bs) = splitAtU s ys
                                   in  (y `Cons` as, bs)

-- | Split a vector into two vectors where the length of the two is determined
-- by the context.
--
-- >>> splitAtI (1:>2:>3:>7:>8:>Nil) :: (Vec 2 Int, Vec 3 Int)
-- (<1,2>,<3,7,8>)
splitAtI :: KnownNat m => Vec (m + n) a -> (Vec m a, Vec n a)
splitAtI = withSNat splitAt
{-# INLINE splitAtI #-}

-- | Concatenate a vector of vectors.
--
-- >>> concat ((1:>2:>3:>Nil) :> (4:>5:>6:>Nil) :> (7:>8:>9:>Nil) :> (10:>11:>12:>Nil) :> Nil)
-- <1,2,3,4,5,6,7,8,9,10,11,12>
concat :: Vec n (Vec m a) -> Vec (n * m) a
concat Nil           = Nil
concat (x `Cons` xs) = x ++ concat xs
{-# NOINLINE concat #-}

-- | Split a vector of \(n * m)\ elements into a vector of \"vectors of length
-- /m/\", where the length /m/ is given.
--
-- >>> unconcat d4 (1:>2:>3:>4:>5:>6:>7:>8:>9:>10:>11:>12:>Nil)
-- <<1,2,3,4>,<5,6,7,8>,<9,10,11,12>>
unconcat :: KnownNat n => SNat m -> Vec (n * m) a -> Vec n (Vec m a)
unconcat n xs = unconcatU (withSNat toUNat) (toUNat n) xs
{-# NOINLINE unconcat #-}

unconcatU :: UNat n -> UNat m -> Vec (n * m) a -> Vec n (Vec m a)
unconcatU UZero      _ _  = Nil
unconcatU (USucc n') m ys = let (as,bs) = splitAtU m ys
                            in  as `Cons` unconcatU n' m bs

-- | Split a vector of /(n * m)/ elements into a vector of \"vectors of length
-- /m/\", where the length /m/ is determined by the context.
--
-- >>> unconcatI (1:>2:>3:>4:>5:>6:>7:>8:>9:>10:>11:>12:>Nil) :: Vec 2 (Vec 6 Int)
-- <<1,2,3,4,5,6>,<7,8,9,10,11,12>>
unconcatI :: (KnownNat n, KnownNat m) => Vec (n * m) a -> Vec n (Vec m a)
unconcatI = withSNat unconcat
{-# INLINE unconcatI #-}

-- | Merge two vectors, alternating their elements, i.e.,
--
-- >>> merge (1 :> 2 :> 3 :> 4 :> Nil) (5 :> 6 :> 7 :> 8 :> Nil)
-- <1,5,2,6,3,7,4,8>
merge :: KnownNat n => Vec n a -> Vec n a -> Vec (2 * n) a
merge x y = concat (transpose (x :> singleton y))
{-# INLINE merge #-}

-- | The elements in a vector in reverse order.
--
-- >>> reverse (1:>2:>3:>4:>Nil)
-- <4,3,2,1>
reverse :: Vec n a -> Vec n a
reverse Nil           = Nil
reverse (x `Cons` xs) = reverse xs :< x
{-# NOINLINE reverse #-}

-- | \"'map' @f xs@\" is the vector obtained by applying /f/ to each element
-- of /xs/, i.e.,
--
-- > map f (x1 :> x2 :>  ... :> xn :> Nil) == (f x1 :> f x2 :> ... :> f xn :> Nil)
--
-- and corresponds to the following circuit layout:
--
-- <<doc/map.svg>>
map :: (a -> b) -> Vec n a -> Vec n b
map _ Nil           = Nil
map f (x `Cons` xs) = f x `Cons` map f xs
{-# NOINLINE map #-}

-- | 'zipWith' generalises 'zip' by zipping with the function given
-- as the first argument, instead of a tupling function.
-- For example, \"'zipWith' @(+)@\" applied to two vectors produces the
-- vector of corresponding sums.
--
-- > zipWith f (x1 :> x2 :> ... xn :> Nil) (y1 :> y2 :> ... :> yn :> Nil) == (f x1 y1 :> f x2 y2 :> ... :> f xn yn :> Nil)
--
-- \"'zipWith' @f xs ys@\" corresponds to the following circuit layout:
--
-- <<doc/zipWith.svg>>
--
-- __NB:__ 'zipWith' is /strict/ in its second argument, and /lazy/ in its
-- third. This matters when 'zipWith' is used in a recursive setting. See
-- 'lazyV' for more information.
zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith _ Nil           _  = Nil
zipWith f (x `Cons` xs) ys = f x (head ys) `Cons` zipWith f xs (tail ys)
{-# NOINLINE zipWith #-}

-- | 'zipWith3' generalises 'zip3' by zipping with the function given
-- as the first argument, instead of a tupling function.
--
-- > zipWith3 f (x1 :> x2 :> ... xn :> Nil) (y1 :> y2 :> ... :> yn :> Nil) (z1 :> z2 :> ... :> zn :> Nil) == (f x1 y1 z1 :> f x2 y2 z2 :> ... :> f xn yn zn :> Nil)
--
-- \"'zipWith3' @f xs ys zs@\" corresponds to the following circuit layout:
--
-- <<doc/zipWith3.svg>>
--
-- __NB:__ 'zipWith3' is /strict/ in its second argument, and /lazy/ in its
-- third and fourth. This matters when 'zipWith3' is used in a recursive setting.
-- See 'lazyV' for more information.
zipWith3 :: (a -> b -> c -> d) -> Vec n a -> Vec n b -> Vec n c -> Vec n d
zipWith3 f us vs ws = zipWith (\a (b,c) -> f a b c) us (zip vs ws)
{-# INLINE zipWith3 #-}

-- | 'foldr', applied to a binary operator, a starting value (typically
-- the right-identity of the operator), and a vector, reduces the vector
-- using the binary operator, from right to left:
--
-- > foldr f z (x1 :> ... :> xn1 :> xn :> Nil) == x1 `f` (... (xn1 `f` (xn `f` z))...)
-- > foldr r z Nil                             == z
--
-- >>> foldr (/) 1 (5 :> 4 :> 3 :> 2 :> Nil)
-- 1.875
--
-- \"'foldr' @f z xs@\" corresponds to the following circuit layout:
--
-- <<doc/foldr.svg>>
--
-- __NB__: @"'foldr' f z xs"@ produces a linear structure, which has a depth, or
-- delay, of O(@'length' xs@). Use 'fold' if your binary operator @f@ is
-- associative, as @"'fold' f xs"@ produces a structure with a depth of
-- O(log_2(@'length' xs@)).
foldr :: (a -> b -> b) -> b -> Vec n a -> b
foldr f z xs = head (scanr f z xs)
{-# INLINE foldr #-}

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
-- \"'foldl' @f z xs@\" corresponds to the following circuit layout:
--
-- <<doc/foldl.svg>>
--
-- __NB__: @"'foldl' f z xs"@ produces a linear structure, which has a depth, or
-- delay, of O(@'length' xs@). Use 'fold' if your binary operator @f@ is
-- associative, as @"'fold' f xs"@ produces a structure with a depth of
-- O(log_2(@'length' xs@)).
foldl :: (b -> a -> b) -> b -> Vec n a -> b
foldl f z xs = last (scanl f z xs)
{-# INLINE foldl #-}

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
-- \"'foldr1' @f xs@\" corresponds to the following circuit layout:
--
-- <<doc/foldr1.svg>>
--
-- __NB__: @"'foldr1' f z xs"@ produces a linear structure, which has a depth,
-- or delay, of O(@'length' xs@). Use 'fold' if your binary operator @f@ is
-- associative, as @"'fold' f xs"@ produces a structure with a depth of
-- O(log_2(@'length' xs@)).
foldr1 :: (a -> a -> a) -> Vec (n + 1) a -> a
foldr1 f xs = foldr f (last xs) (init xs)
{-# INLINE foldr1 #-}

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
-- \"'foldl1' @f xs@\" corresponds to the following circuit layout:
--
-- <<doc/foldl1.svg>>
--
-- __NB__: @"'foldl1' f z xs"@ produces a linear structure, which has a depth,
-- or delay, of O(@'length' xs@). Use 'fold' if your binary operator @f@ is
-- associative, as @"'fold' f xs"@ produces a structure with a depth of
-- O(log_2(@'length' xs@)).
foldl1 :: (a -> a -> a) -> Vec (n + 1) a -> a
foldl1 f xs = foldl f (head xs) (tail xs)
{-# INLINE foldl1 #-}

-- | 'fold' is a variant of 'foldr1' and 'foldl1', but instead of reducing from
-- right to left, or left to right, it reduces a vector using a tree-like
-- structure. The depth, or delay, of the structure produced by
-- \"@'fold' f xs@\", is hence @O(log_2('length' xs))@, and not
-- @O('length' xs)@.
--
-- __NB__: The binary operator \"@f@\" in \"@'fold' f xs@\" must be associative.
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
{-# NOINLINE fold #-}
{-# WARNING fold "CLaSH.Sized.Vector.fold is not synthesisable by the CLaSH compiler." #-}

-- | 'scanl' is similar to 'foldl', but returns a vector of successive reduced
-- values from the left:
--
-- > scanl f z (x1 :> x2 :> ... :> Nil) == z :> (z `f` x1) :> ((z `f` x1) `f` x2) :> ... :> Nil
--
-- >>> scanl (+) 0 (5 :> 4 :> 3 :> 2 :> Nil)
-- <0,5,9,12,14>
--
-- \"'scanl' @f z xs@\" corresponds to the following circuit layout:
--
-- <<doc/scanl.svg>>
--
-- __NB__:
--
-- > last (scanl f z xs) == foldl f z xs
scanl :: (b -> a -> b) -> b -> Vec n a -> Vec (n + 1) b
scanl f z xs = ws
  where
    ws = z `Cons` zipWith (flip f) xs (init ws)
{-# INLINE scanl #-}

-- | 'postscanl' is a variant of 'scanl' where the first result is dropped:
--
-- > postscanl f z (x1 :> x2 :> ... :> Nil) == (z `f` x1) :> ((z `f` x1) `f` x2) :> ... :> Nil
--
-- >>> postscanl (+) 0 (5 :> 4 :> 3 :> 2 :> Nil)
-- <5,9,12,14>
--
-- \"'postscanl' @f z xs@\" corresponds to the following circuit layout:
--
-- <<doc/sscanl.svg>>
postscanl :: (b -> a -> b) -> b -> Vec n a -> Vec n b
postscanl f z xs = tail (scanl f z xs)
{-# INLINE postscanl #-}

-- | 'scanr' is similar to 'foldr', but returns a vector of successive reduced
-- values from the right:
--
-- > scanr f z (... :> xn1 :> xn :> Nil) == ... :> (xn1 `f` (xn `f` z)) :> (xn `f` z) :> z :> Nil
--
-- >>> scanr (+) 0 (5 :> 4 :> 3 :> 2 :> Nil)
-- <14,9,5,2,0>
--
-- \"'scanr' @f z xs@\" corresponds to the following circuit layout:
--
-- <<doc/scanr.svg>>
--
-- __NB__:
--
-- > head (scanr f z xs) == foldr f z xs
scanr :: (a -> b -> b) -> b -> Vec n a -> Vec (n + 1) b
scanr f z xs = ws
  where
    ws = zipWith f xs ((tail ws)) :< z
{-# INLINE scanr #-}

-- | 'postscanr' is a variant of 'scanr' that where the last result is dropped:
--
-- > postscanr f z (... :> xn1 :> xn :> Nil) == ... :> (xn1 `f` (xn `f` z)) :> (xn `f` z) :> Nil
--
-- >>> postscanr (+) 0 (5 :> 4 :> 3 :> 2 :> Nil)
-- <14,9,5,2>
--
-- \"'postscanr' @f z xs@\" corresponds to the following circuit layout:
--
-- <<doc/sscanr.svg>>
postscanr :: (a -> b -> b) -> b -> Vec n a -> Vec n b
postscanr f z xs = init (scanr f z xs)
{-# INLINE postscanr #-}

-- | The 'mapAccumL' function behaves like a combination of 'map' and 'foldl';
-- it applies a function to each element of a vector, passing an accumulating
-- parameter from left to right, and returning a final value of this accumulator
-- together with the new vector.
--
-- >>> mapAccumL (\acc x -> (acc + x,acc + 1)) 0 (1 :> 2 :> 3 :> 4 :> Nil)
-- (10,<1,2,4,7>)
--
-- \"'mapAccumL' @f acc xs@\" corresponds to the following circuit layout:
--
-- <<doc/mapAccumL.svg>>
mapAccumL :: (acc -> x -> (acc,y)) -> acc -> Vec n x -> (acc,Vec n y)
mapAccumL f acc xs = (acc',ys)
  where
    accs  = acc `Cons` accs'
    ws    = zipWith (flip f) xs (init accs)
    accs' = map fst ws
    ys    = map snd ws
    acc'  = last accs
{-# INLINE mapAccumL #-}

-- | The 'mapAccumR' function behaves like a combination of 'map' and 'foldr';
-- it applies a function to each element of a vector, passing an accumulating
-- parameter from right to left, and returning a final value of this accumulator
-- together with the new vector.
--
-- >>> mapAccumR (\acc x -> (acc + x,acc + 1)) 0 (1 :> 2 :> 3 :> 4 :> Nil)
-- (10,<10,8,5,1>)
--
-- \"'mapAccumR' @f acc xs@\" corresponds to the following circuit layout:
--
-- <<doc/mapAccumR.svg>>
mapAccumR :: (acc -> x -> (acc,y)) -> acc -> Vec n x -> (acc, Vec n y)
mapAccumR f acc xs = (acc',ys)
  where
    accs  = accs' :< acc
    ws    = zipWith (flip f) xs (tail accs)
    accs' = map fst ws
    ys    = map snd ws
    acc'  = head accs
{-# INLINE mapAccumR #-}

-- | 'zip' takes two vectors and returns a vector of corresponding pairs.
--
-- >>> zip (1:>2:>3:>4:>Nil) (4:>3:>2:>1:>Nil)
-- <(1,4),(2,3),(3,2),(4,1)>
zip :: Vec n a -> Vec n b -> Vec n (a,b)
zip = zipWith (,)
{-# INLINE zip #-}

-- | 'zip' takes three vectors and returns a vector of corresponding triplets.
--
-- >>> zip3 (1:>2:>3:>4:>Nil) (4:>3:>2:>1:>Nil) (5:>6:>7:>8:>Nil)
-- <(1,4,5),(2,3,6),(3,2,7),(4,1,8)>
zip3 :: Vec n a -> Vec n b -> Vec n c -> Vec n (a,b,c)
zip3 = zipWith3 (,,)
{-# INLINE zip3 #-}

-- | 'unzip' transforms a vector of pairs into a vector of first components
-- and a vector of second components.
--
-- >>> unzip ((1,4):>(2,3):>(3,2):>(4,1):>Nil)
-- (<1,2,3,4>,<4,3,2,1>)
unzip :: Vec n (a,b) -> (Vec n a, Vec n b)
unzip xs = (map fst xs, map snd xs)
{-# INLINE unzip #-}

-- | 'unzip3' transforms a vector of triplets into a vector of first components,
-- a vector of second components, and a vector of third components.
--
-- >>> unzip3 ((1,4,5):>(2,3,6):>(3,2,7):>(4,1,8):>Nil)
-- (<1,2,3,4>,<4,3,2,1>,<5,6,7,8>)
unzip3 :: Vec n (a,b,c) -> (Vec n a, Vec n b, Vec n c)
unzip3 xs = ( map (\(x,_,_) -> x) xs
            , map (\(_,y,_) -> y) xs
            , map (\(_,_,z) -> z) xs
            )
{-# INLINE unzip3 #-}

index_int :: KnownNat n => Vec n a -> Int -> a
index_int xs i@(I# n0)
  | isTrue# (n0 <# 0#) = error "CLaSH.Sized.Vector.(!!): negative index"
  | otherwise          = sub xs n0
  where
    sub :: Vec m a -> Int# -> a
    sub Nil     _ = error (P.concat [ "CLaSH.Sized.Vector.(!!): index "
                                    , show i
                                    , " is larger than maximum index "
                                    , show (maxIndex xs)
                                    ])
    sub (y `Cons` (!ys)) n = if isTrue# (n ==# 0#)
                                then y
                                else sub ys (n -# 1#)
{-# NOINLINE index_int #-}

-- | \"@xs@ '!!' @n@\" returns the /n/'th element of /xs/.
--
-- __NB__: vector elements have an __ASCENDING__ subscript starting from 0 and
-- ending at 'maxIndex'.
--
-- >>> (1:>2:>3:>4:>5:>Nil) !! 4
-- 5
-- >>> (1:>2:>3:>4:>5:>Nil) !! maxIndex (1:>2:>3:>4:>5:>Nil)
-- 5
-- >>> (1:>2:>3:>4:>5:>Nil) !! 1
-- 2
-- >>> (1:>2:>3:>4:>5:>Nil) !! 14
-- *** Exception: CLaSH.Sized.Vector.(!!): index 14 is larger than maximum index 4
(!!) :: (KnownNat n, Enum i) => Vec n a -> i -> a
xs !! i = index_int xs (fromEnum i)
{-# INLINE (!!) #-}

-- | The index (subscript) of the last element in a 'Vec'tor as an 'Int'
-- value.
--
-- >>> maxIndex (6 :> 7 :> 8 :> Nil)
-- 2
maxIndex :: KnownNat n => Vec n a -> Int
maxIndex = subtract 1 . length
{-# NOINLINE maxIndex #-}

-- | The length of a 'Vec'tor as an 'Int' value.
--
-- >>> length (6 :> 7 :> 8 :> Nil)
-- 3
length :: KnownNat n => Vec n a -> Int
length = fromInteger . natVal . asNatProxy
{-# NOINLINE length #-}

replace_int :: KnownNat n => Vec n a -> Int -> a -> Vec n a
replace_int xs i@(I# n0) a
  | isTrue# (n0 <# 0#) = error "CLaSH.Sized.Vector.replace: negative index"
  | otherwise          = sub xs n0 a
  where
    sub :: Vec m b -> Int# -> b -> Vec m b
    sub Nil     _ _ = error (P.concat [ "CLaSH.Sized.Vector.replace: index "
                                      , show i
                                      , " is larger than maximum index "
                                      , show (maxIndex xs)
                                      ])
    sub (y `Cons` (!ys)) n b = if isTrue# (n ==# 0#)
                                 then b `Cons` ys
                                 else y `Cons` sub ys (n -# 1#) b
{-# NOINLINE replace_int #-}

-- | \"'replace' @n a xs@\" returns the vector /xs/ where the /n/'th element is
-- replaced by /a/.
--
-- __NB__: vector elements have an __ASCENDING__ subscript starting from 0 and
-- ending at 'maxIndex'.
--
-- >>> replace 3 7 (1:>2:>3:>4:>5:>Nil)
-- <1,2,3,7,5>
-- >>> replace 0 7 (1:>2:>3:>4:>5:>Nil)
-- <7,2,3,4,5>
-- >>> replace 9 7 (1:>2:>3:>4:>5:>Nil)
-- <1,2,3,4,*** Exception: CLaSH.Sized.Vector.replace: index 9 is larger than maximum index 4
replace :: (KnownNat n, Enum i) => i -> a -> Vec n a -> Vec n a
replace i y xs = replace_int xs (fromEnum i) y
{-# INLINE replace #-}

-- | \"'take' @n xs@\" returns the /n/-length prefix of /xs/.
--
-- >>> take (snat :: SNat 3) (1:>2:>3:>4:>5:>Nil)
-- <1,2,3>
-- >>> take d3               (1:>2:>3:>4:>5:>Nil)
-- <1,2,3>
-- >>> take d0               (1:>2:>Nil)
-- <>
-- >>> take d4               (1:>2:>Nil)
-- <BLANKLINE>
-- <interactive>:...
--     Couldn't match type ‘4 + n0’ with ‘2’
--     The type variable ‘n0’ is ambiguous
--     Expected type: Vec (4 + n0) a
--       Actual type: Vec (1 + 1) a
--     In the second argument of ‘take’, namely ‘(1 :> 2 :> Nil)’
--     In the expression: take d4 (1 :> 2 :> Nil)
--     In an equation for ‘it’: it = take d4 (1 :> 2 :> Nil)
take :: SNat m -> Vec (m + n) a -> Vec m a
take n = fst . splitAt n
{-# INLINE take #-}

-- | \"'takeI' @xs@\" returns the prefix of /xs/ as demanded by the context.
--
-- >>> takeI (1:>2:>3:>4:>5:>Nil) :: Vec 2 Int
-- <1,2>
takeI :: KnownNat m => Vec (m + n) a -> Vec m a
takeI = withSNat take
{-# INLINE takeI #-}

-- | \"'drop' @n xs@\" returns the suffix of /xs/ after the first /n/ elements.
--
-- >>> drop (snat :: SNat 3) (1:>2:>3:>4:>5:>Nil)
-- <4,5>
-- >>> drop d3               (1:>2:>3:>4:>5:>Nil)
-- <4,5>
-- >>> drop d0               (1:>2:>Nil)
-- <1,2>
-- >>> drop d4               (1:>2:>Nil)
-- <BLANKLINE>
-- <interactive>:...
--     Couldn't match expected type ‘2’ with actual type ‘4 + n0’
--     The type variable ‘n0’ is ambiguous
--     In the first argument of ‘print’, namely ‘it’
--     In a stmt of an interactive GHCi command: print it
drop :: SNat m -> Vec (m + n) a -> Vec n a
drop n = snd . splitAt n
{-# INLINE drop #-}

-- | \"'dropI' @xs@\" returns the suffix of /xs/ as demanded by the context.
--
-- >>> dropI (1:>2:>3:>4:>5:>Nil) :: Vec 2 Int
-- <4,5>
dropI :: KnownNat m => Vec (m + n) a -> Vec n a
dropI = withSNat drop
{-# INLINE dropI #-}

-- | \"'at' @n xs@\" returns /n/'th element of /xs/
--
-- __NB__: vector elements have an __ASCENDING__ subscript starting from 0 and
-- ending at 'maxIndex'.
--
-- >>> at (snat :: SNat 1) (1:>2:>3:>4:>5:>Nil)
-- 2
-- >>> at d1               (1:>2:>3:>4:>5:>Nil)
-- 2
at :: SNat m -> Vec (m + (n + 1)) a -> a
at n xs = head $ snd $ splitAt n xs
{-# INLINE at #-}

-- | \"'select' @f s n xs@\" selects /n/ elements with step-size /s/ and
-- offset @f@ from /xs/.
--
-- >>> select (snat :: SNat 1) (snat :: SNat 2) (snat :: SNat 3) (1:>2:>3:>4:>5:>6:>7:>8:>Nil)
-- <2,4,6>
-- >>> select d1 d2 d3 (1:>2:>3:>4:>5:>6:>7:>8:>Nil)
-- <2,4,6>
select :: (CmpNat (i + s) (s * n) ~ 'GT)
       => SNat f
       -> SNat s
       -> SNat n
       -> Vec (f + i) a
       -> Vec n a
select f s n xs = select' (toUNat n) $ drop f xs
  where
    select' :: UNat n -> Vec i a -> Vec n a
    select' UZero      _               = Nil
    select' (USucc n') vs@(x `Cons` _) = x `Cons`
                                         select' n' (drop s (unsafeCoerce vs))
{-# NOINLINE select #-}

-- | \"'selectI' @f s xs@\" selects as many elements as demanded by the context
-- with step-size /s/ and offset /f/ from /xs/.
--
-- >>> selectI d1 d2 (1:>2:>3:>4:>5:>6:>7:>8:>Nil) :: Vec 2 Int
-- <2,4>
selectI :: (CmpNat (i + s) (s * n) ~ 'GT, KnownNat n)
        => SNat f
        -> SNat s
        -> Vec (f + i) a
        -> Vec n a
selectI f s xs = withSNat (\n -> select f s n xs)
{-# INLINE selectI #-}

-- | \"'replicate' @n a@\" returns a vector that has /n/ copies of /a/.
--
-- >>> replicate (snat :: SNat 3) 6
-- <6,6,6>
-- >>> replicate d3 6
-- <6,6,6>
replicate :: SNat n -> a -> Vec n a
replicate n a = replicateU (toUNat n) a
{-# NOINLINE replicate #-}

replicateU :: UNat n -> a -> Vec n a
replicateU UZero     _ = Nil
replicateU (USucc s) x = x `Cons` replicateU s x

-- | \"'replicateI' @a@\" creates a vector with as many copies of /a/ as
-- demanded by the context.
--
-- >>> replicateI 6 :: Vec 5 Int
-- <6,6,6,6,6>
replicateI :: KnownNat n => a -> Vec n a
replicateI = withSNat replicate
{-# INLINE replicateI #-}
{-# WARNING replicateI "Use 'repeat' instead of 'replicateI'" #-}

-- | \"'repeat' @a@\" creates a vector with as many copies of /a/ as demanded
-- by the context.
--
-- >>> repeat 6 :: Vec 5 Int
-- <6,6,6,6,6>
repeat :: KnownNat n => a -> Vec n a
repeat = withSNat replicate
{-# INLINE repeat #-}

-- | \"'iterate' @n f x@\" returns a vector starting with /x/ followed by
-- /n/ repeated applications of /f/ to /x/.
--
-- > iterate (snat :: SNat 4) f x == (x :> f x :> f (f x) :> f (f (f x)) :> Nil)
-- > iterate d4 f x               == (x :> f x :> f (f x) :> f (f (f x)) :> Nil)
--
-- >>> iterate d4 (+1) 1
-- <1,2,3,4>
--
-- \"'interate' @n f z@\" corresponds to the following circuit layout:
--
-- <<doc/iterate.svg>>
iterate :: SNat n -> (a -> a) -> a -> Vec n a
iterate (SNat _) = iterateI
{-# INLINE iterate #-}

-- | \"'iterate' @f x@\" returns a vector starting with @x@ followed by @n@
-- repeated applications of @f@ to @x@, where @n@ is determined by the context.
--
-- > iterateI f x :: Vec 3 a == (x :> f x :> f (f x) :> Nil)
--
-- >>> iterateI (+1) 1 :: Vec 3 Int
-- <1,2,3>
--
-- \"'interateI' @f z@\" corresponds to the following circuit layout:
--
-- <<doc/iterate.svg>>
iterateI :: KnownNat n => (a -> a) -> a -> Vec n a
iterateI f a = xs
  where
    xs = init (a `Cons` ws)
    ws = map f (lazyV xs)
{-# INLINE iterateI #-}

-- | \"'generate' @n f x@\" returns a vector with @n@ repeated applications of
-- @f@ to @x@.
--
-- > generate (snat :: SNat 4) f x == (f x :> f (f x) :> f (f (f x)) :> f (f (f (f x))) :> Nil)
-- > generate d4 f x               == (f x :> f (f x) :> f (f (f x)) :> f (f (f (f x))) :> Nil)
--
-- >>> generate d4 (+1) 1
-- <2,3,4,5>
--
-- \"'generate' @n f z@\" corresponds to the following circuit layout:
--
-- <<doc/generate.svg>>
generate :: SNat n -> (a -> a) -> a -> Vec n a
generate (SNat _) f a = iterateI f (f a)
{-# INLINE generate #-}

-- | \"'generateI' @f x@\" returns a vector with @n@ repeated applications of
-- @f@ to @x@, where @n@ is determined by the context.
--
-- > generateI f x :: Vec 3 a == (f x :> f (f x) :> f (f (f x)) :> Nil)
--
-- >>> generateI (+1) 1 :: Vec 3 Int
-- <2,3,4>
--
-- \"'generateI' @f z@\" corresponds to the following circuit layout:
--
-- <<doc/generate.svg>>
generateI :: KnownNat n => (a -> a) -> a -> Vec n a
generateI f a = iterateI f (f a)
{-# INLINE generateI #-}

-- | Transpose a matrix: go from row-major to column-major
--
-- >>> let xss = (1:>2:>Nil):>(3:>4:>Nil):>(5:>6:>Nil):>Nil
-- >>> xss
-- <<1,2>,<3,4>,<5,6>>
-- >>> transpose xss
-- <<1,3,5>,<2,4,6>>
transpose :: KnownNat n => Vec m (Vec n a) -> Vec n (Vec m a)
transpose = traverse# id
{-# NOINLINE transpose #-}

-- | 1-dimensional stencil computations
--
-- \"'stencil1d' @stX f xs@\", where /xs/ has /stX + n/ elements, applies the
-- stencil computation /f/ on: /n + 1/ overlapping (1D) windows of length /stX/,
-- drawn from /xs/. The resulting vector has /n + 1/ elements.
--
-- >>> let xs = (1:>2:>3:>4:>5:>6:>Nil)
-- >>> :t xs
-- xs :: Num a => Vec 6 a
-- >>> :t stencil1d d2 sum xs
-- stencil1d d2 sum xs :: Num b => Vec 5 b
-- >>> stencil1d d2 sum xs
-- <3,5,7,9,11>
stencil1d :: KnownNat (n + 1)
          => SNat (stX + 1) -- ^ Windows length /stX/, at least size 1
          -> (Vec (stX + 1) a -> b) -- ^ The stencil (function)
          -> Vec ((stX + n) + 1) a
          -> Vec (n + 1) b
stencil1d stX f xs = map f (windows1d stX xs)
{-# INLINE stencil1d #-}

-- | 2-dimensional stencil computations
--
-- \"'stencil2d' @stY stX f xss@\", where /xss/ is a matrix of /stY + m/ rows
-- of /stX + n/ elements, applies the stencil computation /f/ on:
-- /(m + 1) * (n + 1)/ overlapping (2D) windows of /stY/ rows of /stX/ elements,
-- drawn from /xss/. The result matrix has /m + 1/ rows of /n + 1/ elements.
--
-- >>> let xss = ((1:>2:>3:>4:>Nil):>(5:>6:>7:>8:>Nil):>(9:>10:>11:>12:>Nil):>(13:>14:>15:>16:>Nil):>Nil)
-- >>> :t xss
-- xss :: Num a => Vec 4 (Vec 4 a)
-- >>> :t stencil2d d2 d2 (sum . map sum) xss
-- stencil2d d2 d2 (sum . map sum) xss :: Num a => Vec 3 (Vec 3 a)
-- >>> stencil2d d2 d2 (sum . map sum) xss
-- <<14,18,22>,<30,34,38>,<46,50,54>>
stencil2d :: (KnownNat (n + 1), KnownNat (m+1))
          => SNat (stY + 1) -- ^ Window hight /stY/, at least size 1
          -> SNat (stX + 1) -- ^ Window width /stX/, at least size 1
          -> (Vec (stY + 1) (Vec (stX + 1) a) -> b) -- ^ The stencil (function)
          -> Vec ((stY + m) + 1) (Vec ((stX + n) + 1) a)
          -> Vec (m + 1) (Vec (n + 1) b)
stencil2d stY stX f xss = (map.map) f (windows2d stY stX xss)
{-# INLINE stencil2d #-}

-- | \"'windows1d' @stX xs@\", where the vector /xs/ has /stX + n/ elements,
-- returns a vector of /n + 1/ overlapping (1D) windows of /xs/ of length /stX/.
--
-- >>> let xs = (1:>2:>3:>4:>5:>6:>Nil)
-- >>> :t xs
-- xs :: Num a => Vec 6 a
-- >>> :t windows1d d2 xs
-- windows1d d2 xs :: Num a => Vec 5 (Vec 2 a)
-- >>> windows1d d2 xs
-- <<1,2>,<2,3>,<3,4>,<4,5>,<5,6>>
windows1d :: KnownNat (n + 1)
          => SNat (stX + 1) -- ^ Length of the window, at least size 1
          -> Vec ((stX + n) + 1) a
          -> Vec (n + 1) (Vec (stX + 1) a)
windows1d stX xs = map (take stX) (rotations xs)
  where
    rotateL ys   = tail ys :< head ys
    rotations ys = iterateI rotateL ys
{-# INLINE windows1d #-}

-- | \"'windows2d' @stY stX xss@\", where matrix /xss/ has /stY + m/ rows of
-- /stX + n/, returns a matrix of /m+1/ rows of /n+1/ elements. The elements
-- of this new matrix are the overlapping (2D) windows of /xss/, where every
-- window has /stY/ rows of /stX/ elements.
--
-- >>> let xss = ((1:>2:>3:>4:>Nil):>(5:>6:>7:>8:>Nil):>(9:>10:>11:>12:>Nil):>(13:>14:>15:>16:>Nil):>Nil)
-- >>> :t xss
-- xss :: Num a => Vec 4 (Vec 4 a)
-- >>> :t windows2d d2 d2 xss
-- windows2d d2 d2 xss :: Num a => Vec 3 (Vec 3 (Vec 2 (Vec 2 a)))
-- >>> windows2d d2 d2 xss
-- <<<<1,2>,<5,6>>,<<2,3>,<6,7>>,<<3,4>,<7,8>>>,<<<5,6>,<9,10>>,<<6,7>,<10,11>>,<<7,8>,<11,12>>>,<<<9,10>,<13,14>>,<<10,11>,<14,15>>,<<11,12>,<15,16>>>>
windows2d :: (KnownNat (n+1),KnownNat (m+1))
          => SNat (stY + 1) -- ^ Window hight /stY/, at least size 1
          -> SNat (stX + 1) -- ^ Window width /stX/, at least size 1
          -> Vec ((stY + m) + 1) (Vec (stX + n + 1) a)
          -> Vec (m + 1) (Vec (n + 1) (Vec (stY + 1) (Vec (stX + 1) a)))
windows2d stY stX xss = map (transpose . (map (windows1d stX)))
                            (windows1d stY xss)
{-# INLINE windows2d #-}

-- | Forward permutation specified by an index mapping, /ix/. The result vector
-- is initialised by the given defaults, /def/, and an further values that are
-- permuted into the result are added to the current value using the given
-- combination function, /f/.
--
-- The combination function must be /associative/ and /commutative/.
permute :: (Enum i, KnownNat n, KnownNat m)
        => (a -> a -> a)  -- ^ Combination function, /f/
        -> Vec n a        -- ^ Default values, /def/
        -> Vec m i        -- ^ Index mapping, /is/
        -> Vec (m + k) a  -- ^ Vector to be permuted, /xs/
        -> Vec n a
permute f defs is xs = ys
  where
    ixs = zip is (takeI xs)
    ys  = foldl (\ks (i,x) -> let ki = ks!!i in replace i (f x ki) ks) defs ixs
{-# INLINE permute #-}

-- | Backwards permutation specified by an index mapping, /is/, from the
-- destination vector specifying which element of the source vector /xs/ to
-- read.
--
-- \"'backpermute' @xs is@\" is equivalent to \"'map' @(xs '!!') is@\".
--
-- For example:
--
-- >>> let input = 1:>9:>6:>4:>4:>2:>0:>1:>2:>Nil
-- >>> let from  = 1:>3:>7:>2:>5:>3:>Nil
-- >>> backpermute input from
-- <9,4,1,6,2,4>
backpermute :: (Enum i, KnownNat n)
            => Vec n a  -- ^ Source vector, /xs/
            -> Vec m i  -- ^ Index mapping, /is/
            -> Vec m a
backpermute xs = map (xs!!)
{-# INLINE backpermute #-}

-- | Copy elements from the source vector, /xs/, to the destination vector
-- according to an index mapping /is/. This is a forward permute operation where
-- a /to/ vector encodes an input to output index mapping. Output elements for
-- indices that are not mapped assume the value in the default vector /def/.
--
-- For example:
--
-- >>> let defVec = 0:>0:>0:>0:>0:>0:>0:>0:>0:>Nil
-- >>> let to = 1:>3:>7:>2:>5:>8:>Nil
-- >>> let input = 1:>9:>6:>4:>4:>2:>5:>Nil
-- >>> scatter defVec to input
-- <0,1,4,9,0,4,0,6,2>
--
-- __NB__: If the same index appears in the index mapping more than once, the
-- latest mapping is chosen.
scatter :: (Enum i, KnownNat n, KnownNat m)
        => Vec n a       -- ^ Default values, /def/
        -> Vec m i       -- ^ Index mapping, /is/
        -> Vec (m + k) a -- ^ Vector to be scattered, /xs/
        -> Vec n a
scatter = permute const
{-# INLINE scatter #-}

-- | Backwards permutation specified by an index mapping, /is/, from the
-- destination vector specifying which element of the source vector /xs/ to
-- read.
--
-- \"'gather' @xs is@\" is equivalent to \"'map' @(xs '!!') is@\".
--
-- For example:
--
-- >>> let input = 1:>9:>6:>4:>4:>2:>0:>1:>2:>Nil
-- >>> let from  = 1:>3:>7:>2:>5:>3:>Nil
-- >>> gather input from
-- <9,4,1,6,2,4>
gather :: (Enum i, KnownNat n)
       => Vec n a  -- ^ Source vector, /xs/
       -> Vec m i  -- ^ Index mapping, /is/
       -> Vec m a
gather xs = map (xs!!)
{-# INLINE gather #-}

-- | \"'interleave' @d xs@\" creates a vector:
--
-- @
-- \<x_0,x_d,x_(2d),...,x_1,x_(d+1),x_(2d+1),...,x_(d-1),x_(2d-1),x_(3d-1)\>
-- @
--
-- >>> let xs = 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> Nil
-- >>> interleave d3 xs
-- <1,4,7,2,5,8,3,6,9>
interleave :: (KnownNat n, KnownNat d)
           => SNat d -- ^ Interleave step, /d/
           -> Vec (n * d) a
           -> Vec (d * n) a
interleave d = concat . transpose . unconcat d
{-# INLINE interleave #-}

-- | /Dynamically/ rotate a 'Vec'tor to the left:
--
-- >>> let xs = 1 :> 2 :> 3 :> 4 :> Nil
-- >>> rotateLeft xs 1
-- <2,3,4,1>
-- >>> rotateLeft xs 2
-- <3,4,1,2>
-- >>> rotateLeft xs (-1)
-- <4,1,2,3>
--
-- __NB:__ use `rotateLeftS` if you want to rotate left by a /static/ amount.
rotateLeft :: (Enum i, KnownNat n)
           => Vec n a
           -> i
           -> Vec n a
rotateLeft xs i = map ((xs !!) . (`mod` len)) (iterateI (+1) i')
  where
    i'  = fromEnum i
    len = length xs
{-# INLINE rotateLeft #-}

-- | /Dynamically/ rotate a 'Vec'tor to the right:
--
-- >>> let xs = 1 :> 2 :> 3 :> 4 :> Nil
-- >>> rotateRight xs 1
-- <4,1,2,3>
-- >>> rotateRight xs 2
-- <3,4,1,2>
-- >>> rotateRight xs (-1)
-- <2,3,4,1>
--
-- __NB:__ use `rotateRightS` if you want to rotate right by a /static/ amount.
rotateRight :: (Enum i, KnownNat n)
            => Vec n a
            -> i
            -> Vec n a
rotateRight xs i = map ((xs !!) . (`mod` len)) (iterateI (+1) i')
  where
    i'  = negate (fromEnum i)
    len = length xs
{-# INLINE rotateRight #-}

-- | /Statically/ rotate a 'Vec'tor to the left:
--
-- >>> let xs = 1 :> 2 :> 3 :> 4 :> Nil
-- >>> rotateLeftS xs d1
-- <2,3,4,1>
--
-- __NB:__ use `rotateLeft` if you want to rotate left by a /dynamic/ amount.
rotateLeftS :: KnownNat (d + n)
            => Vec (d + n) a
            -> SNat d
            -> Vec (n + d) a
rotateLeftS xs d = let (l,r) = splitAt d xs in r ++ l
{-# INLINE rotateLeftS #-}

-- | /Statically/ rotate a 'Vec'tor to the right:
--
-- >>> let xs = 1 :> 2 :> 3 :> 4 :> Nil
-- >>> rotateRightS xs d1
-- <4,1,2,3>
--
-- __NB:__ use `rotateRight` if you want to rotate right by a /dynamic/ amount.
rotateRightS :: forall n d a . (KnownNat n)
             => Vec (n + d) a
             -> SNat d
             -> Vec (d + n) a
rotateRightS xs _ = let (l,r) = splitAtI xs :: (Vec n a, Vec d a) in r ++ l
{-# INLINE rotateRightS #-}

-- | Convert a vector to a list.
--
-- >>> toList (1:>2:>3:>Nil)
-- [1,2,3]
--
-- __NB__: Not synthesisable
toList :: Vec n a -> [a]
toList = foldr (:) []
{-# INLINE toList #-}

-- | Create a vector literal from a list literal.
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

-- | Length of a 'Vec'tor as an 'SNat' value
lengthS :: KnownNat n => Vec n a -> SNat n
lengthS _ = snat
{-# INLINE lengthS #-}

-- | What you should use when your vector functions are too strict in their
-- arguments.
--
-- For example:
--
-- @
-- -- Bubble sort for 1 iteration
-- sortV xs = 'map' fst sorted ':<' (snd ('last' sorted))
--  where
--    lefts  = 'head' xs :> 'map' snd ('init' sorted)
--    rights = 'tail' xs
--    sorted = 'zipWith' compareSwapL lefts rights
--
-- -- Compare and swap
-- compareSwapL a b = if a < b then (a,b)
--                             else (b,a)
-- @
--
-- Will not terminate because 'zipWith' is too strict in its second argument:
--
-- >>> sortV (4 :> 1 :> 2 :> 3 :> Nil)
-- <*** Exception: <<loop>>
--
-- In this case, adding 'lazyV' on 'zipWith's second argument:
--
-- @
-- sortVL xs = 'map' fst sorted ':<' (snd ('last' sorted))
--  where
--    lefts  = 'head' xs :> map snd ('init' sorted)
--    rights = 'tail' xs
--    sorted = 'zipWith' compareSwapL ('lazyV' lefts) rights
-- @
--
-- Results in a successful computation:
--
-- >>> sortVL (4 :> 1 :> 2 :> 3 :> Nil)
-- <1,2,3,4>
--
-- __NB__: There is also a solution using 'flip', but it slightly obfuscates the
-- meaning of the code:
--
-- @
-- sortV_flip xs = 'map' fst sorted ':<' (snd ('last' sorted))
--  where
--    lefts  = 'head' xs :> 'map' snd ('init' sorted)
--    rights = 'tail' xs
--    sorted = 'zipWith' ('flip' compareSwapL) rights lefts
-- @
--
-- >>> sortV_flip (4 :> 1 :> 2 :> 3 :> Nil)
-- <1,2,3,4>
lazyV :: KnownNat n
      => Vec n a
      -> Vec n a
lazyV = lazyV' (repeat undefined)
  where
    lazyV' :: Vec n a -> Vec n a -> Vec n a
    lazyV' Nil           _  = Nil
    lazyV' (_ `Cons` xs) ys = head ys `Cons` lazyV' xs (tail ys)
{-# NOINLINE lazyV #-}

-- | A /dependently/ typed fold.
--
-- Using lists, we can define @append@ ('Prelude.++') using 'Prelude.foldr':
--
-- >>> import qualified Prelude
-- >>> let append xs ys = Prelude.foldr (:) ys xs
-- >>> append [1,2] [3,4]
-- [1,2,3,4]
--
-- However, when we try to do the same for 'Vec':
--
-- @
-- append' xs ys = 'foldr' (:>) ys xs
-- @
--
-- We get a type error
--
-- >>> let append' xs ys = foldr (:>) ys xs
-- <BLANKLINE>
-- <interactive>:...
--     Occurs check: cannot construct the infinite type: ... ~ ... + 1
--     Expected type: a -> Vec ... a -> Vec ... a
--       Actual type: a -> Vec ... a -> Vec (... + 1) a
--     Relevant bindings include
--       ys :: Vec ... a (bound at ...)
--       append' :: Vec n a -> Vec ... a -> Vec ... a
--         (bound at ...)
--     In the first argument of ‘foldr’, namely ‘(:>)’
--     In the expression: foldr (:>) ys xs
--
-- The reason is that the type of 'foldr' is:
--
-- >>> :t foldr
-- foldr :: (a -> b -> b) -> b -> Vec n a -> b
--
-- While the type of (':>') is:
--
-- >>> :t (:>)
-- (:>) :: a -> Vec n a -> Vec (n + 1) a
--
-- We thus need a @fold@ function that can handle the growing vector type:
-- 'dfold'. Compared to 'foldr', 'dfold' takes an extra parameter, called the
-- /motive/, that allows the folded function to have an argument and result type
-- that /depends/ on the current index into the vector. Using 'dfold', we can
-- now correctly define ('++'):
--
-- @
-- import Data.Singletons.Prelude
-- import Data.Proxy
--
-- data Append (m :: Nat) (a :: *) (f :: 'TyFun' Nat *) :: *
-- type instance 'Apply' (Append m a) l = 'Vec' (l + m) a
--
-- append' xs ys = 'dfold' (Proxy :: Proxy (Append m a)) (const (':>')) ys xs
-- @
--
-- We now see that @append'@ has the appropriate type:
--
-- >>> :t append'
-- append' :: Vec k a -> Vec m a -> Vec (k + m) a
--
-- And that it works:
--
-- >>> append' (1 :> 2 :> Nil) (3 :> 4 :> Nil)
-- <1,2,3,4>
dfold :: Proxy (p :: TyFun Nat * -> *) -- ^ The /motive/
      -> (forall l . Proxy l -> a -> (p $ l) -> (p $ (l + 1))) -- ^ Function to fold
      -> (p $ 0) -- ^ Initial element
      -> Vec k a -- ^ Vector to fold over
      -> (p $ k)
dfold _ _ z Nil                        = z
dfold p f z (x `Cons` (xs :: Vec l a)) = f (Proxy :: Proxy l) x (dfold p f z xs)
{-# NOINLINE dfold #-}
{-# WARNING dfold "CLaSH.Sized.Vector.dfold is not synthesisable by the CLaSH compiler." #-}

data V (a :: *) (f :: TyFun Nat *) :: *
type instance Apply (V a) l = Vec l a

-- | Specialised version of 'dfold' that builds a triangular computational
-- structure.
--
-- Example:
--
-- @
-- cs a b     = if a > b then (a,b) else (b,a)
-- csRow y xs = let (y',xs') = 'mapAccumL' cs y xs in xs' ':<' y'
-- csSort     = 'vfold' csRow
-- @
--
-- Builds a triangular structure of compare and swaps to sort a row.
--
-- >>> csSort (7 :> 3 :> 9 :> 1 :> Nil)
-- <1,3,7,9>
vfold :: (forall l . a -> Vec l b -> Vec (l + 1) b)
      -> Vec k a
      -> Vec k b
vfold f xs = dfold (Proxy :: Proxy (V a)) (const f) Nil xs
{-# NOINLINE vfold #-}
{-# WARNING vfold "CLaSH.Sized.Vector.vfold is not synthesisable by the CLaSH compiler." #-}

instance (KnownNat n, KnownNat (BitSize a), BitPack a) => BitPack (Vec n a) where
  type BitSize (Vec n a) = n * (BitSize a)
  pack   = concatBitVector# . map pack
  unpack = map unpack . unconcatBitVector#

concatBitVector# :: KnownNat m
                 => Vec n (BitVector m)
                 -> BitVector (n * m)
concatBitVector# = concatBitVector' . reverse
  where
    concatBitVector' :: KnownNat m
                     => Vec n (BitVector m)
                     -> BitVector (n * m)
    concatBitVector' Nil           = 0
    concatBitVector' (x `Cons` xs) = concatBitVector' xs ++# x
{-# NOINLINE concatBitVector# #-}

unconcatBitVector# :: (KnownNat n, KnownNat m)
                   => BitVector (n * m)
                   -> Vec n (BitVector m)
unconcatBitVector# bv = withSNat (\s -> ucBV (toUNat s) bv)
{-# NOINLINE unconcatBitVector# #-}

ucBV :: forall n m . KnownNat m
     => UNat n -> BitVector (n * m) -> Vec n (BitVector m)
ucBV UZero     _  = Nil
ucBV (USucc n) bv = let (bv',x :: BitVector m) = split# bv
                    in  ucBV n bv' :< x
{-# INLINE ucBV #-}

instance Lift a => Lift (Vec n a) where
  lift Nil           = [| Nil |]
  lift (x `Cons` xs) = [| x :> $(lift xs) |]

instance (KnownNat n, Arbitrary a) => Arbitrary (Vec n a) where
  arbitrary = traverse# id $ repeat arbitrary
  shrink    = traverse# id . fmap shrink

instance CoArbitrary a => CoArbitrary (Vec n a) where
  coarbitrary = coarbitrary . toList

type instance Index   (Vec n a) = Int
type instance IxValue (Vec n a) = a
instance KnownNat n => Ixed (Vec n a) where
  ix i f xs = replace_int xs i <$> f (index_int xs i)
