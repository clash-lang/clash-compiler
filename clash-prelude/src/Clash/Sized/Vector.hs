{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-redundant-constraints #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Sized.Vector
  ( -- * 'Vec'tor data type
    Vec(Nil,(:>),(:<),Cons)
    -- * Accessors
    -- ** Length information
  , length, lengthS
    -- ** Indexing
  , (!!), head, last, at
  , indices, indicesI
  , findIndex, elemIndex
    -- ** Extracting sub-vectors (slicing)
  , tail, init
  , take, takeI, drop, dropI
  , select, selectI
    -- *** Splitting
  , splitAt, splitAtI
  , unconcat, unconcatI
    -- * Construction
    -- ** Initialisation
  , singleton
  , replicate, repeat
  , iterate, iterateI, generate, generateI
  , unfoldr, unfoldrI
    -- *** Initialisation from a list
  , listToVecTH
    -- ** Concatenation
  , (++), (+>>), (<<+), concat, concatMap
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
  , map, imap, smap
    -- ** Zipping
  , zipWith, zipWith3, zipWith4, zipWith5, zipWith6, zipWith7
  , zip, zip3, zip4, zip5, zip6, zip7
  , izipWith
    -- ** Unzipping
  , unzip, unzip3, unzip4, unzip5, unzip6, unzip7
    -- * Folding
  , foldr, foldl, foldr1, foldl1, fold
  , ifoldr, ifoldl
    -- ** Specialised folds
  , dfold, dtfold, vfold
    -- * Prefix sums (scans)
  , scanl, scanr, postscanl, postscanr
  , mapAccumL, mapAccumR
    -- * Stencil computations
  , stencil1d, stencil2d
  , windows1d, windows2d
    -- * Conversions
  , toList
  , fromList
  , unsafeFromList
  , bv2v
  , v2bv
    -- * Misc
  , lazyV, VCons, asNatProxy, seqV, forceV, seqVX, forceVX
    -- * Primitives
    -- ** 'Traversable' instance
  , traverse#
    -- ** 'BitPack' instance
  , concatBitVector#
  , unconcatBitVector#
  )
where

import Control.DeepSeq            (NFData (..))
import qualified Control.Lens     as Lens hiding (pattern (:>), pattern (:<))
import Data.Bits                  ((.|.), shiftL)
import Data.Constraint            ((:-)(..), Dict (..))
import Data.Constraint.Nat        (leZero)
import Data.Data
  (Data (..), Constr, DataType, Fixity (..), Typeable, mkConstr, mkDataType)
import Data.Either                (isLeft)
import Data.Default.Class         (Default (..))
import qualified Data.Foldable    as F
import Data.Kind                  (Type)
import Data.Proxy                 (Proxy (..))
import Data.Singletons            (TyFun,Apply,type (@@))
import GHC.TypeLits               (CmpNat, KnownNat, Nat, type (+), type (-), type (*),
                                   type (^), type (<=), natVal)
import GHC.Base                   (Int(I#),Int#,isTrue#)
import GHC.Generics               hiding (Fixity (..))
import qualified GHC.Magic
import GHC.Prim                   ((==#),(<#),(-#))
import Language.Haskell.TH        (ExpQ)
import Language.Haskell.TH.Syntax (Lift(..))
#if MIN_VERSION_template_haskell(2,16,0)
import Language.Haskell.TH.Compat
#endif
import Prelude                    hiding ((++), (!!), concat, concatMap, drop,
                                          foldl, foldl1, foldr, foldr1, head,
                                          init, iterate, last, length, map,
                                          repeat, replicate, reverse, scanl,
                                          scanr, splitAt, tail, take, unzip,
                                          unzip3, zip, zip3, zipWith, zipWith3)
import qualified Data.String.Interpolate as I
import qualified Prelude          as P
import Test.QuickCheck
  (Arbitrary(arbitrary, shrink), CoArbitrary(coarbitrary))
import Unsafe.Coerce              (unsafeCoerce)

import Clash.Annotations.Primitive
  (Primitive(InlinePrimitive), HDL(..), dontTranslate)
import Clash.Promoted.Nat
  (SNat (..), SNatLE (..), UNat (..), compareSNat, leToPlus, pow2SNat,
   snatProxy, snatToInteger, subSNat, withSNat, toUNat, natToInteger)
import Clash.Promoted.Nat.Literals (d1)
import Clash.Sized.Internal.BitVector (Bit, BitVector (..), split#)
import Clash.Sized.Index          (Index)

import Clash.Class.BitPack        (packXWith, BitPack (..))
import Clash.XException
  (ShowX (..), NFDataX (..), showsX, seqX, isX)

{- $setup
>>> :set -XDataKinds
>>> :set -XTypeFamilies
>>> :set -XTypeOperators
>>> :set -XTemplateHaskell
>>> :set -XFlexibleContexts
>>> :set -XTypeApplications
>>> :set -fplugin GHC.TypeLits.Normalise
>>> import Clash.Prelude
>>> import Data.Kind
>>> import Data.Proxy
>>> import qualified Clash.Sized.Vector as Vec
>>> let compareSwapL a b = if a < b then (a,b) else (b,a)
>>> :{
let sortV xs = map fst sorted :< (snd (last sorted))
      where
        lefts  = head xs :> map snd (init sorted)
        rights = tail xs
        sorted = zipWith compareSwapL lefts rights
:}

>>> :{
let sortVL xs = map fst sorted :< (snd (last sorted))
      where
        lefts  = head xs :> map snd (init sorted)
        rights = tail xs
        sorted = zipWith compareSwapL (lazyV lefts) rights
:}

>>> :{
let sortV_flip xs = map fst sorted :< (snd (last sorted))
      where
        lefts  = head xs :> map snd (init sorted)
        rights = tail xs
        sorted = zipWith (flip compareSwapL) rights lefts
:}

>>> data Append (m :: Nat) (a :: Type) (f :: TyFun Nat Type) :: Type
>>> type instance Apply (Append m a) l = Vec (l + m) a
>>> let append' xs ys = dfold (Proxy :: Proxy (Append m a)) (const (:>)) ys xs
>>> let compareSwap a b = if a > b then (a,b) else (b,a)
>>> let insert y xs     = let (y',xs') = mapAccumL compareSwap y xs in xs' :< y'
>>> let insertionSort   = vfold (const insert)
>>> data IIndex (f :: TyFun Nat Type) :: Type
>>> :set -XUndecidableInstances
>>> type instance Apply IIndex l = Index ((2^l)+1)
>>> :{
let populationCount' :: (KnownNat k, KnownNat (2^k)) => BitVector (2^k) -> Index ((2^k)+1)
    populationCount' bv = dtfold (Proxy @IIndex)
                                 fromIntegral
                                 (\_ x y -> add x y)
                                 (bv2v bv)
:}

-}

infixr 5 `Cons`
-- | Fixed size vectors.
--
-- * Lists with their length encoded in their type
-- * 'Vec'tor elements have an __ASCENDING__ subscript starting from 0 and
--   ending at @'length' - 1@.
data Vec :: Nat -> Type -> Type where
  Nil  :: Vec 0 a
  Cons :: a -> Vec n a -> Vec (n + 1) a

-- | In many cases, this Generic instance only allows generic
-- functions/instances over vectors of at least size 1, due to the
-- /n-1/ in the /Rep (Vec n a)/ definition.
--
-- We'll have to wait for things like
-- https://ryanglscott.github.io/2018/02/11/how-to-derive-generic-for-some-gadts/
-- before we can work around this limitation
instance KnownNat n => Generic (Vec n a) where
  type Rep (Vec n a) =
    D1 ('MetaData "Vec" "Clash.Data.Vector" "clash-prelude" 'False)
      (C1 ('MetaCons "Nil" 'PrefixI 'False) U1 :+:
       C1 ('MetaCons "Cons" 'PrefixI 'False)
        (S1 ('MetaSel 'Nothing
                'NoSourceUnpackedness
                'NoSourceStrictness
                'DecidedLazy)
            (Rec0 a) :*:
         S1 ('MetaSel 'Nothing
                'NoSourceUnpackedness
                'NoSourceStrictness
                'DecidedLazy)
            (Rec0 (Vec (n-1) a))))
  from Nil         = M1 (L1 (M1 U1))
  from (Cons x xs) = M1 (R1 (M1 (M1 (K1 x) :*: M1 (K1 xs))))
  to (M1 g) = case compareSNat (SNat @n) (SNat @0) of
    SNatLE -> case leZero @n of
      Sub Dict -> Nil
    SNatGT -> case g of
      R1 (M1 (M1 (K1 p) :*: M1 (K1 q))) -> Cons p q

instance (KnownNat n, Typeable a, Data a) => Data (Vec n a) where
  gunfold k z _ = case compareSNat (SNat @n) (SNat @0) of
    SNatLE -> case leZero @n of
      Sub Dict -> z Nil
    SNatGT -> k (k (z @(a -> Vec (n-1) a -> Vec n a) Cons))
  toConstr Nil        = cNil
  toConstr (Cons _ _) = cCons
  dataTypeOf _        = tVec

  gfoldl
    :: (forall d b. Data d => c (d -> b) -> d -> c b)
    -> (forall g. g -> c g)
    -> Vec n a
    -> c (Vec n a)
  gfoldl f z xs = case compareSNat (SNat @n) (SNat @0) of
    SNatLE -> case leZero @n of
                  Sub Dict -> z Nil
    SNatGT -> let (y :> ys) = xs
              in (z @(a -> Vec (n-1) a -> Vec n a) (:>) `f` y `f` ys)

tVec :: DataType
tVec = mkDataType "Vec" [cNil, cCons]

cNil :: Constr
cNil = mkConstr tVec "Nil" [] Prefix

cCons :: Constr
cCons = mkConstr tVec "Cons" [] Prefix

instance NFData a => NFData (Vec n a) where
  rnf = foldl (\() -> rnf) ()

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
-- Also in conjunctions with (':<'):
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
  showsPrec _ vs = \s -> '<':punc vs ('>':s)
    where
      punc :: Vec m a -> ShowS
      punc Nil            = id
      punc (x `Cons` Nil) = shows x
      punc (x `Cons` xs)  = \s -> shows x (',':punc xs s)

instance ShowX a => ShowX (Vec n a) where
  showsPrecX _n vs = showString "<" . punc vs
   where
    punc :: Vec m a -> ShowS
    punc (isX -> Left _) = showString "X"
    punc Nil = showString ">"
    punc (Cons x (isX -> Right Nil)) = showsX x . ('>':)
    punc (Cons x xs) = showsX x . showString "," . punc xs

instance (KnownNat n, Eq a) => Eq (Vec n a) where
  (==) Nil _            = True
  (==) v1@(Cons _ _) v2 = fold (&&) (zipWith (==) v1 v2)

instance (KnownNat n, Ord a) => Ord (Vec n a) where
  compare x y = foldr f EQ $ zipWith compare x y
    where f EQ   keepGoing = keepGoing
          f done _         = done

instance (KnownNat n, Semigroup a) => Semigroup (Vec n a) where
  (<>) = zipWith (<>)

instance (KnownNat n, Monoid a) => Monoid (Vec n a) where
  mempty = repeat mempty
  mappend = (<>)

instance KnownNat n => Applicative (Vec n) where
  pure      = repeat
  fs <*> xs = zipWith ($) fs xs

{-# RULES
"zipWith$map" forall f xs ys. zipWith (\g a -> g a) (map f xs) ys = zipWith f xs ys
  #-}

instance (KnownNat n, 1 <= n) => F.Foldable (Vec n) where
  fold      = leToPlus @1 @n $ fold mappend
  foldMap f = leToPlus @1 @n $ fold mappend . map f
  foldr     = foldr
  foldl     = foldl
  foldr1 f  = leToPlus @1 @n $ foldr1 f
  foldl1 f  = leToPlus @1 @n $ foldl1 f
  toList    = toList
  null _    = False
  length    = length
  maximum   = leToPlus @1 @n $ fold (\x y -> if x >= y then x else y)
  minimum   = leToPlus @1 @n $ fold (\x y -> if x <= y then x else y)
  sum       = leToPlus @1 @n $ fold (+)
  product   = leToPlus @1 @n $ fold (*)

instance Functor (Vec n) where
  fmap = map

instance (KnownNat n, 1 <= n) => Traversable (Vec n) where
  traverse = traverse#

{-# NOINLINE traverse# #-}
traverse# :: forall a f b n . Applicative f => (a -> f b) -> Vec n a -> f (Vec n b)
traverse# _ Nil           = pure Nil
traverse# f (x `Cons` xs) = Cons <$> f x <*> traverse# f xs

instance (Default a, KnownNat n) => Default (Vec n a) where
  def = repeat def

instance (NFDataX a, KnownNat n) => NFDataX (Vec n a) where
  deepErrorX x = repeat (deepErrorX x)

  rnfX v =
    -- foldl will fail if the spine of the vector is undefined, so we need to
    -- seqX the result of it. We need to use foldl so Clash won't treat it as
    -- a recursive function.
    seqX (foldl (\() -> rnfX) () v) ()

  hasUndefined v =
    if isLeft (isX v) then True else go v
   where
    go :: forall m b . (NFDataX b, KnownNat m) => Vec m b -> Bool
    go Nil = False
    go (x `Cons` xs) = hasUndefined x || hasUndefined xs

  ensureSpine = map ensureSpine . lazyV

{-# INLINE singleton #-}
-- | Create a vector of one element
--
-- >>> singleton 5
-- <5>
singleton :: a -> Vec 1 a
singleton = (`Cons` Nil)

{-# NOINLINE head #-}
{- | Extract the first element of a vector

>>> head (1:>2:>3:>Nil)
1

#if __GLASGOW_HASKELL__ >= 900
>>> head Nil
<BLANKLINE>
<interactive>:...
    • Couldn't match type ‘1’ with ‘0’
      Expected: Vec (0 + 1) a
        Actual: Vec 0 a
    • In the first argument of ‘head’, namely ‘Nil’
      In the expression: head Nil
      In an equation for ‘it’: it = head Nil

#else
>>> head Nil
<BLANKLINE>
<interactive>:...
    • Couldn't match type ‘1’ with ‘0’
      Expected type: Vec (0 + 1) a
        Actual type: Vec 0 a
    • In the first argument of ‘head’, namely ‘Nil’
      In the expression: head Nil
      In an equation for ‘it’: it = head Nil

#endif
-}
head :: Vec (n + 1) a -> a
head (x `Cons` _) = x

{-# NOINLINE tail #-}
{- | Extract the elements after the head of a vector

>>> tail (1:>2:>3:>Nil)
<2,3>

#if __GLASGOW_HASKELL__ >= 900
>>> tail Nil
<BLANKLINE>
<interactive>:...
    • Couldn't match type ‘1’ with ‘0’
      Expected: Vec (0 + 1) a
        Actual: Vec 0 a
    • In the first argument of ‘tail’, namely ‘Nil’
      In the expression: tail Nil
      In an equation for ‘it’: it = tail Nil

#else
>>> tail Nil
<BLANKLINE>
<interactive>:...
    • Couldn't match type ‘1’ with ‘0’
      Expected type: Vec (0 + 1) a
        Actual type: Vec 0 a
    • In the first argument of ‘tail’, namely ‘Nil’
      In the expression: tail Nil
      In an equation for ‘it’: it = tail Nil

#endif
-}
tail :: Vec (n + 1) a -> Vec n a
tail (_ `Cons` xs) = xs

{-# NOINLINE last #-}
{- | Extract the last element of a vector

>>> last (1:>2:>3:>Nil)
3

#if __GLASGOW_HASKELL__ >= 900
>>> last Nil
<BLANKLINE>
<interactive>:...
    • Couldn't match type ‘1’ with ‘0’
      Expected: Vec (0 + 1) a
        Actual: Vec 0 a
    • In the first argument of ‘last’, namely ‘Nil’
      In the expression: last Nil
      In an equation for ‘it’: it = last Nil

#else
>>> last Nil
<BLANKLINE>
<interactive>:...
    • Couldn't match type ‘1’ with ‘0’
      Expected type: Vec (0 + 1) a
        Actual type: Vec 0 a
    • In the first argument of ‘last’, namely ‘Nil’
      In the expression: last Nil
      In an equation for ‘it’: it = last Nil

#endif
-}
last :: Vec (n + 1) a -> a
last (x `Cons` Nil)         = x
last (_ `Cons` y `Cons` ys) = last (y `Cons` ys)

{-# NOINLINE init #-}
{- | Extract all the elements of a vector except the last element

>>> init (1:>2:>3:>Nil)
<1,2>

#if __GLASGOW_HASKELL__ >= 900
>>> init Nil
<BLANKLINE>
<interactive>:...
    • Couldn't match type ‘1’ with ‘0’
      Expected: Vec (0 + 1) a
        Actual: Vec 0 a
    • In the first argument of ‘init’, namely ‘Nil’
      In the expression: init Nil
      In an equation for ‘it’: it = init Nil

#else
>>> init Nil
<BLANKLINE>
<interactive>:...
    • Couldn't match type ‘1’ with ‘0’
      Expected type: Vec (0 + 1) a
        Actual type: Vec 0 a
    • In the first argument of ‘init’, namely ‘Nil’
      In the expression: init Nil
      In an equation for ‘it’: it = init Nil

#endif
-}
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
-- Also in conjunctions with (':>'):
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
shiftOutFromN :: (Default a, KnownNat n)
              => SNat m        -- ^ @m@, the number of elements to shift out
              -> Vec (m + n) a -- ^ The old vector
              -> (Vec (m + n) a, Vec m a)
              -- ^ (The new vector, shifted out elements)
shiftOutFromN m@SNat xs = shiftInAt0 xs (replicate m def)
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
-- >>> splitAt (SNat :: SNat 3) (1:>2:>3:>7:>8:>Nil)
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

-- | Map a function over all the elements of a vector and concatentate the resulting vectors.
--
-- >>> concatMap (replicate d3) (1:>2:>3:>Nil)
-- <1,1,1,2,2,2,3,3,3>
concatMap :: (a -> Vec m b) -> Vec n a -> Vec (n * m) b
concatMap f xs = concat (map f xs)
{-# INLINE concatMap #-}

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

-- | Apply a function of every element of a vector and its index.
--
-- >>> :t imap (+) (2 :> 2 :> 2 :> 2 :> Nil)
-- imap (+) (2 :> 2 :> 2 :> 2 :> Nil) :: Vec 4 (Index 4)
-- >>> imap (+) (2 :> 2 :> 2 :> 2 :> Nil)
-- <2,3,*** Exception: X: Clash.Sized.Index: result 4 is out of bounds: [0..3]
-- ...
-- >>> imap (\i a -> fromIntegral i + a) (2 :> 2 :> 2 :> 2 :> Nil) :: Vec 4 (Unsigned 8)
-- <2,3,4,5>
--
-- \"'imap' @f xs@\" corresponds to the following circuit layout:
--
-- <<doc/imap.svg>>
imap :: forall n a b . KnownNat n => (Index n -> a -> b) -> Vec n a -> Vec n b
imap f = go 0
  where
    go :: Index n -> Vec m a -> Vec m b
    go _ Nil           = Nil
    go n (x `Cons` xs) = f n x `Cons` go (n+1) xs
{-# NOINLINE imap #-}

{- | Zip two vectors with a functions that also takes the elements' indices.

#if __GLASGOW_HASKELL__ >= 900
>>> izipWith (\i a b -> i + a + b) (2 :> 2 :> Nil)  (3 :> 3:> Nil)
<*** Exception: X: Clash.Sized.Index: result 2 is out of bounds: [0..1]
...
#else
>>> izipWith (\i a b -> i + a + b) (2 :> 2 :> Nil)  (3 :> 3:> Nil)
<*** Exception: X: Clash.Sized.Index: result 3 is out of bounds: [0..1]
...
#endif
>>> izipWith (\i a b -> fromIntegral i + a + b) (2 :> 2 :> Nil) (3 :> 3 :> Nil) :: Vec 2 (Unsigned 8)
<5,6>

\"'imap' @f xs@\" corresponds to the following circuit layout:

<<doc/izipWith.svg>>

__NB:__ 'izipWith' is /strict/ in its second argument, and /lazy/ in its
third. This matters when 'izipWith' is used in a recursive setting. See
'lazyV' for more information.
-}
izipWith :: KnownNat n => (Index n -> a -> b -> c) -> Vec n a -> Vec n b
         -> Vec n c
izipWith f xs ys = imap (\i -> uncurry (f i)) (zip xs ys)
{-# INLINE izipWith #-}

-- | Right fold (function applied to each element and its index)
--
-- >>> let findLeftmost x xs = ifoldr (\i a b -> if a == x then Just i else b) Nothing xs
-- >>> findLeftmost 3 (1:>3:>2:>4:>3:>5:>6:>Nil)
-- Just 1
-- >>> findLeftmost 8 (1:>3:>2:>4:>3:>5:>6:>Nil)
-- Nothing
--
-- \"'ifoldr' @f z xs@\" corresponds to the following circuit layout:
--
-- <<doc/ifoldr.svg>>
ifoldr :: KnownNat n => (Index n -> a -> b -> b) -> b -> Vec n a -> b
ifoldr f z xs = head ws
  where
    ws = izipWith f xs ((tail ws)) :< z
{-# INLINE ifoldr #-}

-- | Left fold (function applied to each element and its index)
--
-- >>> let findRightmost x xs = ifoldl (\a i b -> if b == x then Just i else a) Nothing xs
-- >>> findRightmost 3 (1:>3:>2:>4:>3:>5:>6:>Nil)
-- Just 4
-- >>> findRightmost 8 (1:>3:>2:>4:>3:>5:>6:>Nil)
-- Nothing
--
-- \"'ifoldl' @f z xs@\" corresponds to the following circuit layout:
--
-- <<doc/ifoldl.svg>>
ifoldl :: KnownNat n => (a -> Index n -> b -> a) -> a -> Vec n b -> a
ifoldl f z xs = last ws
  where
    ws = z `Cons` izipWith (\i b a -> f a i b) xs (init ws)
{-# INLINE ifoldl #-}

-- | Generate a vector of indices.
--
-- >>> indices d4
-- <0,1,2,3>
indices :: KnownNat n => SNat n -> Vec n (Index n)
indices _ = indicesI
{-# INLINE indices #-}

-- | Generate a vector of indices, where the length of the vector is determined
-- by the context.
--
-- >>> indicesI :: Vec 4 (Index 4)
-- <0,1,2,3>
indicesI :: KnownNat n => Vec n (Index n)
indicesI = imap const (repeat ())
{-# INLINE indicesI #-}

-- | \"'findIndex' @p xs@\" returns the index of the /first/ element of /xs/
-- satisfying the predicate /p/, or 'Nothing' if there is no such element.
--
-- >>> findIndex (> 3) (1:>3:>2:>4:>3:>5:>6:>Nil)
-- Just 3
-- >>> findIndex (> 8) (1:>3:>2:>4:>3:>5:>6:>Nil)
-- Nothing
findIndex :: KnownNat n => (a -> Bool) -> Vec n a -> Maybe (Index n)
findIndex f = ifoldr (\i a b -> if f a then Just i else b) Nothing
{-# INLINE findIndex #-}

-- | \"'elemIndex' @a xs@\" returns the index of the /first/ element which is
-- equal (by '==') to the query element /a/, or 'Nothing' if there is no such
-- element.
--
-- >>> elemIndex 3 (1:>3:>2:>4:>3:>5:>6:>Nil)
-- Just 1
-- >>> elemIndex 8 (1:>3:>2:>4:>3:>5:>6:>Nil)
-- Nothing
elemIndex :: (KnownNat n, Eq a) => a -> Vec n a -> Maybe (Index n)
elemIndex x = findIndex (x ==)
{-# INLINE elemIndex #-}

-- | 'zipWith' generalizes 'zip' by zipping with the function given
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

-- | 'zipWith3' generalizes 'zip3' by zipping with the function given
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

-- 'zipWith4' is analogous to 'zipWith3', but with four vectors.
--
-- __NB:__ 'zipWith4' is /strict/ in its second argument, and /lazy/ its following
-- arguments. This matters when 'zipWith4' is used in a recursive setting. See
-- 'lazyV' for more information.
zipWith4
  :: (a -> b -> c -> d -> e)
  -> Vec n a
  -> Vec n b
  -> Vec n c
  -> Vec n d
  -> Vec n e
zipWith4 f us vs ws xs =
  zipWith (\a (b,c,d) -> f a b c d) us (zip3 vs ws xs)
{-# INLINE zipWith4 #-}

-- 'zipWith5' is analogous to 'zipWith3', but with five vectors.
--
-- __NB:__ 'zipWith5' is /strict/ in its second argument, and /lazy/ its following
-- arguments. This matters when 'zipWith5' is used in a recursive setting. See
-- 'lazyV' for more information.
zipWith5
  :: (a -> b -> c -> d -> e -> f)
  -> Vec n a
  -> Vec n b
  -> Vec n c
  -> Vec n d
  -> Vec n e
  -> Vec n f
zipWith5 f us vs ws xs ys =
  zipWith (\a (b,c,d,e) -> f a b c d e) us (zip4 vs ws xs ys)
{-# INLINE zipWith5 #-}

-- 'zipWith6' is analogous to 'zipWith3', but with six vectors.
--
-- __NB:__ 'zipWith6' is /strict/ in its second argument, and /lazy/ its following
-- arguments. This matters when 'zipWith6' is used in a recursive setting. See
-- 'lazyV' for more information.
zipWith6
  :: (a -> b -> c -> d -> e -> f -> g)
  -> Vec n a
  -> Vec n b
  -> Vec n c
  -> Vec n d
  -> Vec n e
  -> Vec n f
  -> Vec n g
zipWith6 f us vs ws xs ys zs =
  zipWith (\u (v,w,x,y,z) -> f u v w x y z) us (zip5 vs ws xs ys zs)
{-# INLINE zipWith6 #-}

-- 'zipWith7' is analogous to 'zipWith3', but with seven vectors.
--
-- __NB:__ 'zipWith7' is /strict/ in its second argument, and /lazy/ its following
-- arguments. This matters when 'zipWith7' is used in a recursive setting. See
-- 'lazyV' for more information.
zipWith7
  :: (a -> b -> c -> d -> e -> f -> g -> h)
  -> Vec n a
  -> Vec n b
  -> Vec n c
  -> Vec n d
  -> Vec n e
  -> Vec n f
  -> Vec n g
  -> Vec n h
zipWith7 f ts us vs ws xs ys zs =
  zipWith (\t (u,v,w,x,y,z) -> f t u v w x y z) ts (zip6 us vs ws xs ys zs)
{-# INLINE zipWith7 #-}

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
foldr _ z Nil           = z
foldr f z (x `Cons` xs) = f x (foldr f z xs)
{-# NOINLINE foldr #-}

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
--
-- \"'fold' @f xs@\" corresponds to the following circuit layout:
--
-- <<doc/fold.svg>>
fold :: forall n a . (a -> a -> a) -> Vec (n + 1) a -> a
fold f vs = fold' (toList vs)
  where
    fold' [x] = x
    fold' xs  = fold' ys `f` fold' zs
      where
        (ys,zs) = P.splitAt (P.length xs `div` 2) xs
{-# NOINLINE fold #-}
{-# ANN fold (InlinePrimitive [VHDL,Verilog,SystemVerilog] "[ { \"BlackBoxHaskell\" : { \"name\" : \"Clash.Sized.Vector.fold\", \"templateFunction\" : \"Clash.Primitives.Sized.Vector.foldBBF\"}} ]") #-}

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

-- | 'zip3' takes three vectors and returns a vector of corresponding triplets.
--
-- >>> zip3 (1:>2:>3:>4:>Nil) (4:>3:>2:>1:>Nil) (5:>6:>7:>8:>Nil)
-- <(1,4,5),(2,3,6),(3,2,7),(4,1,8)>
zip3 :: Vec n a -> Vec n b -> Vec n c -> Vec n (a,b,c)
zip3 = zipWith3 (,,)
{-# INLINE zip3 #-}

-- | 'zip4' takes four vectors and returns a list of quadruples, analogous
-- to 'zip'.
zip4 :: Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n (a,b,c,d)
zip4 = zipWith4 (,,,)
{-# INLINE zip4 #-}

-- | 'zip5' takes five vectors and returns a list of five-tuples, analogous
-- to 'zip'.
zip5 :: Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n (a,b,c,d,e)
zip5 = zipWith5 (,,,,)
{-# INLINE zip5 #-}

-- | 'zip6' takes six vectors and returns a list of six-tuples, analogous
-- to 'zip'.
zip6
  :: Vec n a
  -> Vec n b
  -> Vec n c
  -> Vec n d
  -> Vec n e
  -> Vec n f
  -> Vec n (a,b,c,d,e,f)
zip6 = zipWith6 (,,,,,)
{-# INLINE zip6 #-}

-- | 'zip7' takes seven vectors and returns a list of seven-tuples, analogous
-- to 'zip'.
zip7
  :: Vec n a
  -> Vec n b
  -> Vec n c
  -> Vec n d
  -> Vec n e
  -> Vec n f
  -> Vec n g
  -> Vec n (a,b,c,d,e,f,g)
zip7 = zipWith7 (,,,,,,)
{-# INLINE zip7 #-}

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

-- | 'unzip4' takes a vector of quadruples and returns four vectors, analogous
-- to 'unzip'.
unzip4 :: Vec n (a,b,c,d) -> (Vec n a, Vec n b, Vec n c, Vec n d)
unzip4 xs = ( map (\(w,_,_,_) -> w) xs
            , map (\(_,x,_,_) -> x) xs
            , map (\(_,_,y,_) -> y) xs
            , map (\(_,_,_,z) -> z) xs
            )
{-# INLINE unzip4 #-}

-- | 'unzip5' takes a vector of five-tuples and returns five vectors, analogous
-- to 'unzip'.
unzip5 :: Vec n (a,b,c,d,e) -> (Vec n a, Vec n b, Vec n c, Vec n d, Vec n e)
unzip5 xs = ( map (\(v,_,_,_,_) -> v) xs
            , map (\(_,w,_,_,_) -> w) xs
            , map (\(_,_,x,_,_) -> x) xs
            , map (\(_,_,_,y,_) -> y) xs
            , map (\(_,_,_,_,z) -> z) xs
            )
{-# INLINE unzip5 #-}

-- | 'unzip6' takes a vector of six-tuples and returns six vectors, analogous
-- to 'unzip'.
unzip6
  :: Vec n (a,b,c,d,e,f)
  -> (Vec n a, Vec n b, Vec n c, Vec n d, Vec n e, Vec n f)
unzip6 xs = ( map (\(u,_,_,_,_,_) -> u) xs
            , map (\(_,v,_,_,_,_) -> v) xs
            , map (\(_,_,w,_,_,_) -> w) xs
            , map (\(_,_,_,x,_,_) -> x) xs
            , map (\(_,_,_,_,y,_) -> y) xs
            , map (\(_,_,_,_,_,z) -> z) xs
            )
{-# INLINE unzip6 #-}

-- | 'unzip7' takes a vector of seven-tuples and returns seven vectors, analogous
-- to 'unzip'.
unzip7
  :: Vec n (a,b,c,d,e,f,g)
  -> (Vec n a, Vec n b, Vec n c, Vec n d, Vec n e, Vec n f, Vec n g)
unzip7 xs = ( map (\(t,_,_,_,_,_,_) -> t) xs
            , map (\(_,u,_,_,_,_,_) -> u) xs
            , map (\(_,_,v,_,_,_,_) -> v) xs
            , map (\(_,_,_,w,_,_,_) -> w) xs
            , map (\(_,_,_,_,x,_,_) -> x) xs
            , map (\(_,_,_,_,_,y,_) -> y) xs
            , map (\(_,_,_,_,_,_,z) -> z) xs
            )
{-# INLINE unzip7 #-}


index_int :: KnownNat n => Vec n a -> Int -> a
index_int xs i@(I# n0)
  | isTrue# (n0 <# 0#) = error "Clash.Sized.Vector.(!!): negative index"
  | otherwise          = sub xs n0
  where
    sub :: Vec m a -> Int# -> a
    sub Nil     _ = error (P.concat [ "Clash.Sized.Vector.(!!): index "
                                    , show i
                                    , " is larger than maximum index "
                                    , show ((length xs)-1)
                                    ])
    sub (y `Cons` (!ys)) n = if isTrue# (n ==# 0#)
                                then y
                                else sub ys (n -# 1#)
{-# NOINLINE index_int #-}

-- | \"@xs@ '!!' @n@\" returns the /n/'th element of /xs/.
--
-- __NB__: vector elements have an __ASCENDING__ subscript starting from 0 and
-- ending at @'length' - 1@.
--
-- >>> (1:>2:>3:>4:>5:>Nil) !! 4
-- 5
-- >>> (1:>2:>3:>4:>5:>Nil) !! (length (1:>2:>3:>4:>5:>Nil) - 1)
-- 5
-- >>> (1:>2:>3:>4:>5:>Nil) !! 1
-- 2
-- >>> (1:>2:>3:>4:>5:>Nil) !! 14
-- *** Exception: Clash.Sized.Vector.(!!): index 14 is larger than maximum index 4
-- ...
(!!) :: (KnownNat n, Enum i) => Vec n a -> i -> a
xs !! i = index_int xs (fromEnum i)
{-# INLINE (!!) #-}

-- | The length of a 'Vec'tor as an 'Int' value.
--
-- >>> length (6 :> 7 :> 8 :> Nil)
-- 3
length :: KnownNat n => Vec n a -> Int
length = fromInteger . natVal . asNatProxy
{-# NOINLINE length #-}

replace_int :: KnownNat n => Vec n a -> Int -> a -> Vec n a
replace_int xs i@(I# n0) a
  | isTrue# (n0 <# 0#) = error "Clash.Sized.Vector.replace: negative index"
  | otherwise          = sub xs n0 a
  where
    sub :: Vec m b -> Int# -> b -> Vec m b
    sub Nil     _ _ = error (P.concat [ "Clash.Sized.Vector.replace: index "
                                      , show i
                                      , " is larger than maximum index "
                                      , show (length xs - 1)
                                      ])
    sub (y `Cons` (!ys)) n b = if isTrue# (n ==# 0#)
                                 then b `Cons` ys
                                 else y `Cons` sub ys (n -# 1#) b
{-# NOINLINE replace_int #-}

-- | \"'replace' @n a xs@\" returns the vector /xs/ where the /n/'th element is
-- replaced by /a/.
--
-- __NB__: vector elements have an __ASCENDING__ subscript starting from 0 and
-- ending at @'length' - 1@.
--
-- >>> replace 3 7 (1:>2:>3:>4:>5:>Nil)
-- <1,2,3,7,5>
-- >>> replace 0 7 (1:>2:>3:>4:>5:>Nil)
-- <7,2,3,4,5>
-- >>> replace 9 7 (1:>2:>3:>4:>5:>Nil)
-- <1,2,3,4,*** Exception: Clash.Sized.Vector.replace: index 9 is larger than maximum index 4
-- ...
replace :: (KnownNat n, Enum i) => i -> a -> Vec n a -> Vec n a
replace i y xs = replace_int xs (fromEnum i) y
{-# INLINE replace #-}

{- | \"'take' @n xs@\" returns the /n/-length prefix of /xs/.

>>> take (SNat :: SNat 3) (1:>2:>3:>4:>5:>Nil)
<1,2,3>
>>> take d3               (1:>2:>3:>4:>5:>Nil)
<1,2,3>
>>> take d0               (1:>2:>Nil)
<>

#if __GLASGOW_HASKELL__ >= 900
>>> take d4               (1:>2:>Nil)
<BLANKLINE>
<interactive>:...
    • Couldn't match type ‘4 + n0’ with ‘2’
      Expected: Vec (4 + n0) a
        Actual: Vec (1 + 1) a
      The type variable ‘n0’ is ambiguous
    • In the second argument of ‘take’, namely ‘(1 :> 2 :> Nil)’
      In the expression: take d4 (1 :> 2 :> Nil)
      In an equation for ‘it’: it = take d4 (1 :> 2 :> Nil)

#else
>>> take d4               (1:>2:>Nil)
<BLANKLINE>
<interactive>:...
    • Couldn't match type ‘4 + n0’ with ‘2’
      Expected type: Vec (4 + n0) a
        Actual type: Vec (1 + 1) a
      The type variable ‘n0’ is ambiguous
    • In the second argument of ‘take’, namely ‘(1 :> 2 :> Nil)’
      In the expression: take d4 (1 :> 2 :> Nil)
      In an equation for ‘it’: it = take d4 (1 :> 2 :> Nil)

#endif
-}
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
-- >>> drop (SNat :: SNat 3) (1:>2:>3:>4:>5:>Nil)
-- <4,5>
-- >>> drop d3               (1:>2:>3:>4:>5:>Nil)
-- <4,5>
-- >>> drop d0               (1:>2:>Nil)
-- <1,2>
-- >>> drop d4               (1:>2:>Nil)
-- <BLANKLINE>
-- <interactive>:...: error:
--     • Couldn't match...type ‘4 + n0...
--       The type variable ‘n0’ is ambiguous
--     • In the first argument of ‘print’, namely ‘it’
--       In a stmt of an interactive GHCi command: print it
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
-- ending at @'length' - 1@.
--
-- >>> at (SNat :: SNat 1) (1:>2:>3:>4:>5:>Nil)
-- 2
-- >>> at d1               (1:>2:>3:>4:>5:>Nil)
-- 2
at :: SNat m -> Vec (m + (n + 1)) a -> a
at n xs = head $ snd $ splitAt n xs
{-# INLINE at #-}

-- | \"'select' @f s n xs@\" selects /n/ elements with step-size /s/ and
-- offset @f@ from /xs/.
--
-- >>> select (SNat :: SNat 1) (SNat :: SNat 2) (SNat :: SNat 3) (1:>2:>3:>4:>5:>6:>7:>8:>Nil)
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
-- >>> replicate (SNat :: SNat 3) 6
-- <6,6,6>
-- >>> replicate d3 6
-- <6,6,6>
replicate :: SNat n -> a -> Vec n a
replicate n a = replicateU (toUNat n) a
{-# NOINLINE replicate #-}

replicateU :: UNat n -> a -> Vec n a
replicateU UZero     _ = Nil
replicateU (USucc s) x = x `Cons` replicateU s x

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
-- > iterate (SNat :: SNat 4) f x == (x :> f x :> f (f x) :> f (f (f x)) :> Nil)
-- > iterate d4 f x               == (x :> f x :> f (f x) :> f (f (f x)) :> Nil)
--
-- >>> iterate d4 (+1) 1
-- <1,2,3,4>
--
-- \"'iterate' @n f z@\" corresponds to the following circuit layout:
--
-- <<doc/iterate.svg>>
iterate :: SNat n -> (a -> a) -> a -> Vec n a
iterate SNat = iterateI
{-# INLINE iterate #-}

-- | \"'iterate' @f x@\" returns a vector starting with @x@ followed by @n@
-- repeated applications of @f@ to @x@, where @n@ is determined by the context.
--
-- > iterateI f x :: Vec 3 a == (x :> f x :> f (f x) :> Nil)
--
-- >>> iterateI (+1) 1 :: Vec 3 Int
-- <1,2,3>
--
-- \"'iterateI' @f z@\" corresponds to the following circuit layout:
--
-- <<doc/iterate.svg>>
iterateI :: forall n a. KnownNat n => (a -> a) -> a -> Vec n a
iterateI f a = xs
  where
    xs = init (a `Cons` ws)
    ws = map f (lazyV xs)
{-# NOINLINE iterateI #-}
{-# ANN iterateI (InlinePrimitive [VHDL,Verilog,SystemVerilog] [I.i| [{
    "BlackBoxHaskell": {
        "name": "Clash.Sized.Vector.iterateI"
      , "templateFunction": "Clash.Primitives.Sized.Vector.iterateBBF"
    }}] |]) #-}

-- | \"'unfoldr @n f s@\" builds a vector of length @n@ from a seed value @s@,
-- where every element @a@ is created by successive calls of @f@ on @s@. Unlike
-- 'Data.List.unfoldr' from "Data.List" the generating function @f@ cannot
-- dictate the length of the resulting vector, it must be statically known.
--
-- a simple use of 'unfoldr':
--
-- >>> unfoldr d10 (\s -> (s,s-1)) 10
-- <10,9,8,7,6,5,4,3,2,1>
unfoldr :: SNat n -> (s -> (a,s)) -> s -> Vec n a
unfoldr SNat = unfoldrI
{-# INLINE unfoldr #-}

-- | \"'unfoldr @f s@\" builds a vector from a seed value @s@, where every
-- element @a@ is created by successive calls of @f@ on @s@; the length of the
-- vector is inferred from the context. Unlike 'Data.List.unfoldr' from
-- "Data.List" the generating function @f@ cannot  dictate the length of the
-- resulting vector, it must be statically known.
--
-- a simple use of 'unfoldrI':
--
-- >>> unfoldrI (\s -> (s,s-1)) 10 :: Vec 10 Int
-- <10,9,8,7,6,5,4,3,2,1>
unfoldrI :: KnownNat n => (s -> (a,s)) -> s -> Vec n a
unfoldrI f s0 = map fst xs
 where
  xs = init (f s0 `Cons` ws)
  ws = map (f . snd) (lazyV xs)
{-# INLINE unfoldrI #-}

-- | \"'generate' @n f x@\" returns a vector with @n@ repeated applications of
-- @f@ to @x@.
--
-- > generate (SNat :: SNat 4) f x == (f x :> f (f x) :> f (f (f x)) :> f (f (f (f x))) :> Nil)
-- > generate d4 f x               == (f x :> f (f x) :> f (f (f x)) :> f (f (f (f x))) :> Nil)
--
-- >>> generate d4 (+1) 1
-- <2,3,4,5>
--
-- \"'generate' @n f z@\" corresponds to the following circuit layout:
--
-- <<doc/generate.svg>>
generate :: SNat n -> (a -> a) -> a -> Vec n a
generate SNat f a = iterateI f (f a)
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
stencil1d :: KnownNat n
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
-- stencil2d d2 d2 (sum . map sum) xss :: Num b => Vec 3 (Vec 3 b)
-- >>> stencil2d d2 d2 (sum . map sum) xss
-- <<14,18,22>,<30,34,38>,<46,50,54>>
stencil2d :: (KnownNat n, KnownNat m)
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
windows1d :: KnownNat n
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
windows2d :: (KnownNat n,KnownNat m)
          => SNat (stY + 1) -- ^ Window hight /stY/, at least size 1
          -> SNat (stX + 1) -- ^ Window width /stX/, at least size 1
          -> Vec ((stY + m) + 1) (Vec (stX + n + 1) a)
          -> Vec (m + 1) (Vec (n + 1) (Vec (stY + 1) (Vec (stX + 1) a)))
windows2d stY stX xss = map (transpose . (map (windows1d stX))) (windows1d stY xss)
{-# INLINE windows2d #-}

-- | Forward permutation specified by an index mapping, /ix/. The result vector
-- is initialized by the given defaults, /def/, and an further values that are
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
rotateLeftS :: KnownNat n
            => Vec n a
            -> SNat d
            -> Vec n a
rotateLeftS xs d = go (snatToInteger d `mod` natVal (asNatProxy xs)) xs
  where
    go :: Integer -> Vec k a -> Vec k a
    go _ Nil           = Nil
    go 0 ys            = ys
    go n (y `Cons` ys) = go (n-1) (ys :< y)
{-# NOINLINE rotateLeftS #-}

-- | /Statically/ rotate a 'Vec'tor to the right:
--
-- >>> let xs = 1 :> 2 :> 3 :> 4 :> Nil
-- >>> rotateRightS xs d1
-- <4,1,2,3>
--
-- __NB:__ use `rotateRight` if you want to rotate right by a /dynamic/ amount.
rotateRightS :: KnownNat n
             => Vec n a
             -> SNat d
             -> Vec n a
rotateRightS xs d = go (snatToInteger d `mod` natVal (asNatProxy xs)) xs
  where
    go _ Nil            = Nil
    go 0 ys             = ys
    go n ys@(Cons _ _)  = go (n-1) (last ys :> init ys)
{-# NOINLINE rotateRightS #-}

-- | Convert a vector to a list.
--
-- >>> toList (1:>2:>3:>Nil)
-- [1,2,3]
toList :: Vec n a -> [a]
toList = foldr (:) []
{-# INLINE toList #-}

-- | Convert a list to a vector. This function returns Nothing if the size of
-- the list is not equal to the size of the resulting vector.
--
-- >>> Vec.fromList [1,2,3,4,5] :: Maybe (Vec 5 Int)
-- Just <1,2,3,4,5>
--
-- >>> Vec.fromList [1,2,3,4,5] :: Maybe (Vec 3 Int)
-- Nothing
--
-- >>> Vec.fromList [1,2,3,4,5] :: Maybe (Vec 10 Int)
-- Nothing
--
-- __NB:__ use `listToVecTH` if you want to make a /statically known/ vector
-- __NB:__ this function is not synthesizable
--
fromList :: forall n a. (KnownNat n) => [a] -> Maybe (Vec n a)
fromList xs
  | exactLength (natToInteger @n) xs = Just (unsafeFromList xs)
  | otherwise = Nothing
 where
  exactLength 0 acc = null acc
  exactLength _ []  = False
  exactLength i (_:ys) = exactLength (i - 1) ys
{-# NOINLINE fromList #-}
{-# ANN fromList dontTranslate #-}

-- | Convert a list to a vector. This function always returns a vector of the
-- desired length, by either truncating the list or padding the vector with
-- undefined elements.
--
-- >>> Vec.unsafeFromList [1,2,3,4,5] :: Vec 5 Int
-- <1,2,3,4,5>
--
-- >>> Vec.unsafeFromList [1,2,3,4,5] :: Vec 3 Int
-- <1,2,3>
--
-- >>> Vec.unsafeFromList [1,2,3,4,5] :: Vec 10 Int
-- <1,2,3,4,5,*** Exception: Clash.Sized.Vector.unsafeFromList: vector larger than list
-- ...
--
-- __NB:__ use `listToVecTH` if you want to make a /statically known/ vector
-- __NB:__ this function is not synthesizable
--
unsafeFromList :: forall n a. (KnownNat n) => [a] -> Vec n a
unsafeFromList = unfoldr SNat go
 where
  go :: [a] -> (a, [a])
  go (x:xs) = (x, xs)
  go [] =
    let item = error "Clash.Sized.Vector.unsafeFromList: vector larger than list"
     in (item, [])
{-# NOINLINE unsafeFromList #-}
{-# ANN unsafeFromList dontTranslate #-}

-- | Create a vector literal from a list literal.
--
-- > $(listToVecTH [1::Signed 8,2,3,4,5]) == (8:>2:>3:>4:>5:>Nil) :: Vec 5 (Signed 8)
--
-- >>> [1 :: Signed 8,2,3,4,5]
-- [1,2,3,4,5]
-- >>> $(listToVecTH [1::Signed 8,2,3,4,5])
-- <1,2,3,4,5>
listToVecTH :: Lift a => [a] -> ExpQ
listToVecTH []     = [| Nil |]
listToVecTH (x:xs) = [| x :> $(listToVecTH xs) |]

-- | 'Vec'tor as a 'Proxy' for 'Nat'
asNatProxy :: Vec n a -> Proxy n
asNatProxy _ = Proxy

-- | Length of a 'Vec'tor as an 'SNat' value
lengthS :: KnownNat n => Vec n a -> SNat n
lengthS _ = SNat
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
-- Will not terminate because 'zipWith' is too strict in its second argument.
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
lazyV = lazyV' (repeat ())
  where
    lazyV' :: Vec n () -> Vec n a -> Vec n a
    lazyV' Nil           _  = Nil
    lazyV' (_ `Cons` xs) ys = head ys `Cons` lazyV' xs (tail ys)
{-# NOINLINE lazyV #-}

-- | A /dependently/ typed fold.
--
-- Using lists, we can define /append/ (a.k.a. @Data.List.@'Data.List.++') in
-- terms of @Data.List.@'Data.List.foldr':
--
-- >>> import qualified Data.List
-- >>> let append xs ys = Data.List.foldr (:) ys xs
-- >>> append [1,2] [3,4]
-- [1,2,3,4]
--
-- However, when we try to do the same for 'Vec', by defining /append'/ in terms
-- of @Clash.Sized.Vector.@'foldr':
--
-- @
-- append' xs ys = 'foldr' (:>) ys xs
-- @
--
-- we get a type error:
--
-- @
-- __>>> let append' xs ys = foldr (:>) ys xs__
--
-- \<interactive\>:...
--     • Occurs check: cannot construct the infinite type: ... ~ ... + 1
--       Expected type: a -> Vec ... a -> Vec ... a
--         Actual type: a -> Vec ... a -> Vec (... + 1) a
--     • In the first argument of ‘foldr’, namely ‘(:>)’
--       In the expression: foldr (:>) ys xs
--       In an equation for ‘append'’: append' xs ys = foldr (:>) ys xs
--     • Relevant bindings include
--         ys :: Vec ... a (bound at ...)
--         append' :: Vec n a -> Vec ... a -> Vec ... a
--           (bound at ...)
-- @
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
-- that /depends/ on the current length of the vector. Using 'dfold', we can
-- now correctly define /append'/:
--
-- @
-- import Data.Singletons
-- import Data.Proxy
--
-- data Append (m :: Nat) (a :: Type) (f :: 'TyFun' Nat Type) :: Type
-- type instance 'Apply' (Append m a) l = 'Vec' (l + m) a
--
-- append' xs ys = 'dfold' (Proxy :: Proxy (Append m a)) (const (':>')) ys xs
-- @
--
-- We now see that /append'/ has the appropriate type:
--
-- >>> :t append'
-- append' :: KnownNat k => Vec k a -> Vec m a -> Vec (k + m) a
--
-- And that it works:
--
-- >>> append' (1 :> 2 :> Nil) (3 :> 4 :> Nil)
-- <1,2,3,4>
--
-- __NB__: \"@'dfold' m f z xs@\" creates a linear structure, which has a depth,
-- or delay, of O(@'length' xs@). Look at 'dtfold' for a /dependently/ typed
-- fold that produces a structure with a depth of O(log_2(@'length' xs@)).
dfold :: forall p k a . KnownNat k
      => Proxy (p :: TyFun Nat Type -> Type) -- ^ The /motive/
      -> (forall l . SNat l -> a -> (p @@ l) -> (p @@ (l + 1)))
      -- ^ Function to fold.
      --
      -- __NB__: The @SNat l@ is __not__ the index (see (`!!`)) to the
      -- element /a/. @SNat l@ is the number of elements that occur to the
      -- right of /a/.
      -> (p @@ 0) -- ^ Initial element
      -> Vec k a -- ^ Vector to fold over
      -> (p @@ k)
dfold _ f z xs = go (snatProxy (asNatProxy xs)) xs
  where
    go :: SNat n -> Vec n a -> (p @@ n)
    go _ Nil                        = z
    go s (y `Cons` (ys :: Vec z a)) =
      let s' = s `subSNat` d1
      in  f s' y (go s' ys)
{-# NOINLINE dfold #-}

{- | A combination of 'dfold' and 'fold': a /dependently/ typed fold that
reduces a vector in a tree-like structure.

As an example of when you might want to use 'dtfold' we will build a
population counter: a circuit that counts the number of bits set to '1' in
a 'BitVector'. Given a vector of /n/ bits, we only need we need a data type
that can represent the number /n/: 'Index' @(n+1)@. 'Index' @k@ has a range
of @[0 .. k-1]@ (using @ceil(log2(k))@ bits), hence we need 'Index' @n+1@.
As an initial attempt we will use 'sum', because it gives a nice (@log2(n)@)
tree-structure of adders:

@
populationCount :: (KnownNat (n+1), KnownNat (n+2))
                => 'BitVector' (n+1) -> 'Index' (n+2)
populationCount = sum . map fromIntegral . 'bv2v'
@

The \"problem\" with this description is that all adders have the same
bit-width, i.e. all adders are of the type:

@
(+) :: 'Index' (n+2) -> 'Index' (n+2) -> 'Index' (n+2).
@

This is a \"problem\" because we could have a more efficient structure:
one where each layer of adders is /precisely/ wide enough to count the number
of bits at that layer. That is, at height /d/ we want the adder to be of
type:

@
'Index' ((2^d)+1) -> 'Index' ((2^d)+1) -> 'Index' ((2^(d+1))+1)
@

We have such an adder in the form of the 'Clash.Class.Num.add' function, as
defined in the instance 'Clash.Class.Num.ExtendingNum' instance of 'Index'.
However, we cannot simply use 'fold' to create a tree-structure of
'Clash.Class.Num.add'es:

#if __GLASGOW_HASKELL__ >= 900
>>> :{
let populationCount' :: (KnownNat (n+1), KnownNat (n+2))
                     => BitVector (n+1) -> Index (n+2)
    populationCount' = fold add . map fromIntegral . bv2v
:}
<BLANKLINE>
<interactive>:...
    • Couldn't match type: ((n + 2) + (n + 2)) - 1
                     with: n + 2
      Expected: Index (n + 2) -> Index (n + 2) -> Index (n + 2)
        Actual: Index (n + 2)
                -> Index (n + 2) -> AResult (Index (n + 2)) (Index (n + 2))
    • In the first argument of ‘fold’, namely ‘add’
      In the first argument of ‘(.)’, namely ‘fold add’
      In the expression: fold add . map fromIntegral . bv2v
    • Relevant bindings include
        populationCount' :: BitVector (n + 1) -> Index (n + 2)
          (bound at ...)

#else
>>> :{
let populationCount' :: (KnownNat (n+1), KnownNat (n+2))
                     => BitVector (n+1) -> Index (n+2)
    populationCount' = fold add . map fromIntegral . bv2v
:}
<BLANKLINE>
<interactive>:...
    • Couldn't match type ‘((n + 2) + (n + 2)) - 1’ with ‘n + 2’
      Expected type: Index (n + 2) -> Index (n + 2) -> Index (n + 2)
        Actual type: Index (n + 2)
                     -> Index (n + 2) -> AResult (Index (n + 2)) (Index (n + 2))
    • In the first argument of ‘fold’, namely ‘add’
      In the first argument of ‘(.)’, namely ‘fold add’
      In the expression: fold add . map fromIntegral . bv2v
    • Relevant bindings include
        populationCount' :: BitVector (n + 1) -> Index (n + 2)
          (bound at ...)

#endif

because 'fold' expects a function of type \"@a -> a -> a@\", i.e. a function
where the arguments and result all have exactly the same type.

In order to accommodate the type of our 'Clash.Class.Num.add', where the
result is larger than the arguments, we must use a dependently typed fold in
the form of 'dtfold':

@
{\-\# LANGUAGE UndecidableInstances \#-\}
import Data.Singletons
import Data.Proxy

data IIndex (f :: 'TyFun' Nat Type) :: Type
type instance 'Apply' IIndex l = 'Index' ((2^l)+1)

populationCount' :: (KnownNat k, KnownNat (2^k))
                 => BitVector (2^k) -> Index ((2^k)+1)
populationCount' bv = 'dtfold' (Proxy @IIndex)
                             fromIntegral
                             (\\_ x y -> 'Clash.Class.Num.add' x y)
                             ('bv2v' bv)
@

And we can test that it works:

>>> :t populationCount' (7 :: BitVector 16)
populationCount' (7 :: BitVector 16) :: Index 17
>>> populationCount' (7 :: BitVector 16)
3

Some final remarks:

  * By using 'dtfold' instead of 'fold', we had to restrict our 'BitVector'
    argument to have bit-width that is a power of 2.
  * Even though our original /populationCount/ function specified a structure
    where all adders had the same width. Most VHDL/(System)Verilog synthesis
    tools will create a more efficient circuit, i.e. one where the adders
    have an increasing bit-width for every layer, from the
    VHDL/(System)Verilog produced by the Clash compiler.

__NB__: The depth, or delay, of the structure produced by
\"@'dtfold' m f g xs@\" is O(log_2(@'length' xs@)).
-}
dtfold :: forall p k a . KnownNat k
       => Proxy (p :: TyFun Nat Type -> Type) -- ^ The /motive/
       -> (a -> (p @@ 0)) -- ^ Function to apply to every element
       -> (forall l . SNat l -> (p @@ l) -> (p @@ l) -> (p @@ (l + 1)))
       -- ^ Function to combine results.
       --
       -- __NB__: The @SNat l@ indicates the depth/height of the node in the
       -- tree that is created by applying this function. The leafs of the tree
       -- have depth\/height /0/, and the root of the tree has height /k/.
       -> Vec (2^k) a
       -- ^ Vector to fold over.
       --
       -- __NB__: Must have a length that is a power of 2.
       -> (p @@ k)
dtfold _ f g = go (SNat :: SNat k)
  where
    go :: forall n . SNat n -> Vec (2^n) a -> (p @@ n)
    go _  (x `Cons` Nil) = f x
    go sn xs@(Cons _ (Cons _ _)) =
      let sn' :: SNat (n - 1)
          sn'       = sn `subSNat` d1
          (xsL,xsR) = splitAt (pow2SNat sn') xs
      in  g sn' (go sn' xsL) (go sn' xsR)
{-# NOINLINE dtfold #-}

-- | To be used as the motive /p/ for 'dfold', when the /f/ in \"'dfold' @p f@\"
-- is a variation on (':>'), e.g.:
--
-- @
-- map' :: forall n a b . KnownNat n => (a -> b) -> Vec n a -> Vec n b
-- map' f = 'dfold' (Proxy @('VCons' b)) (\_ x xs -> f x :> xs)
-- @
data VCons (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (VCons a) l = Vec l a

-- | Specialised version of 'dfold' that builds a triangular computational
-- structure.
--
-- Example:
--
-- @
-- compareSwap a b = if a > b then (a,b) else (b,a)
-- insert y xs     = let (y',xs') = 'mapAccumL' compareSwap y xs in xs' ':<' y'
-- insertionSort   = 'vfold' (const insert)
-- @
--
-- Builds a triangular structure of compare and swaps to sort a row.
--
-- >>> insertionSort (7 :> 3 :> 9 :> 1 :> Nil)
-- <1,3,7,9>
--
-- The circuit layout of @insertionSort@, build using 'vfold', is:
--
-- <<doc/csSort.svg>>
vfold :: forall k a b . KnownNat k
      => (forall l . SNat l -> a -> Vec l b -> Vec (l + 1) b)
      -> Vec k a
      -> Vec k b
vfold f xs = dfold (Proxy @(VCons b)) f Nil xs
{-# INLINE vfold #-}

-- | Apply a function to every element of a vector and the element's position
-- (as an 'SNat' value) in the vector.
--
-- >>> let rotateMatrix = smap (flip rotateRightS)
-- >>> let xss = (1:>2:>3:>Nil):>(1:>2:>3:>Nil):>(1:>2:>3:>Nil):>Nil
-- >>> xss
-- <<1,2,3>,<1,2,3>,<1,2,3>>
-- >>> rotateMatrix xss
-- <<1,2,3>,<3,1,2>,<2,3,1>>
smap :: forall k a b . KnownNat k => (forall l . SNat l -> a -> b) -> Vec k a -> Vec k b
smap f xs = reverse
          $ dfold (Proxy @(VCons b))
                  (\sn x xs' -> f sn x :> xs')
                  Nil (reverse xs)
{-# INLINE smap #-}

instance (KnownNat n, BitPack a) => BitPack (Vec n a) where
  type BitSize (Vec n a) = n * (BitSize a)
  pack   = packXWith (concatBitVector# . map pack)
  unpack = map unpack . unconcatBitVector#

concatBitVector#
  :: forall n m
   . (KnownNat n, KnownNat m)
  => Vec n (BitVector m)
  -> BitVector (n * m)
concatBitVector# = go 0
 where
  go :: BitVector (n*m) -> Vec p (BitVector m) -> BitVector (n * m)
  go acc Nil = acc
  go (BV accMsk accVal) ((BV xMsk xVal) `Cons` xs) =
    let sh = fromInteger (natVal (Proxy @m)) :: Int in
    go (BV (shiftL accMsk sh .|. xMsk) (shiftL accVal sh .|. xVal)) xs
{-# NOINLINE concatBitVector# #-}

unconcatBitVector#
  :: forall n m
   . (KnownNat n, KnownNat m)
  => BitVector (n * m)
  -> Vec n (BitVector m)
unconcatBitVector# orig = snd (go (toUNat (SNat @n)))
  where
    go :: forall p . (p <= n) => UNat p -> (BitVector ((n-p)*m), Vec p (BitVector m))
    go UZero = (orig,Nil)
    go (USucc (n :: UNat (p-1))) =
      let (bv,xs) = go n
          (l,x) = (GHC.Magic.noinline split#) bv
      in  (l,x :> xs)
{-# NOINLINE unconcatBitVector# #-}

-- | Convert a 'BitVector' to a 'Vec' of 'Bit's.
--
-- >>> let x = 6 :: BitVector 8
-- >>> x
-- 0000_0110
-- >>> bv2v x
-- <0,0,0,0,0,1,1,0>
bv2v :: KnownNat n => BitVector n -> Vec n Bit
bv2v = unpack

-- | Convert a 'Vec' of 'Bit's to a 'BitVector'.
--
-- >>> let x = (0:>0:>0:>1:>0:>0:>1:>0:>Nil) :: Vec 8 Bit
-- >>> x
-- <0,0,0,1,0,0,1,0>
-- >>> v2bv x
-- 0001_0010
v2bv :: KnownNat n => Vec n Bit -> BitVector n
v2bv = pack

-- | Evaluate all elements of a vector to WHNF, returning the second argument
seqV
  :: KnownNat n
  => Vec n a
  -> b
  -> b
seqV v b =
  let s () e = seq e () in
  foldl s () v `seq` b
{-# NOINLINE seqV #-}
infixr 0 `seqV`

-- | Evaluate all elements of a vector to WHNF
forceV
  :: KnownNat n
  => Vec n a
  -> Vec n a
forceV v =
  v `seqV` v
{-# INLINE forceV #-}

-- | Evaluate all elements of a vector to WHNF, returning the second argument.
-- Does not propagate 'Clash.XException.XException's.
seqVX
  :: KnownNat n
  => Vec n a
  -> b
  -> b
seqVX v b =
  let s () e = seqX e () in
  foldl s () v `seqX` b
{-# NOINLINE seqVX #-}
infixr 0 `seqVX`

-- | Evaluate all elements of a vector to WHNF. Does not propagate
-- 'Clash.XException.XException's.
forceVX
  :: KnownNat n
  => Vec n a
  -> Vec n a
forceVX v =
  v `seqVX` v
{-# INLINE forceVX #-}

instance Lift a => Lift (Vec n a) where
  lift Nil           = [| Nil |]
  lift (x `Cons` xs) = [| x `Cons` $(lift xs) |]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedFromUntyped
#endif

instance (KnownNat n, Arbitrary a) => Arbitrary (Vec n a) where
  arbitrary = traverse# id $ repeat arbitrary
  shrink    = traverse# id . fmap shrink

instance CoArbitrary a => CoArbitrary (Vec n a) where
  coarbitrary = coarbitrary . toList

type instance Lens.Index   (Vec n a) = Index n
type instance Lens.IxValue (Vec n a) = a
instance KnownNat n => Lens.Ixed (Vec n a) where
  ix i f xs = replace_int xs (fromEnum i) <$> f (index_int xs (fromEnum i))
