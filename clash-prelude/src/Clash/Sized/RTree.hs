{-|
Copyright  :  (C) 2016, University of Twente
                  2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Clash.Sized.RTree
  ( -- * 'RTree' data type
    RTree (LR, BR, RLeaf, RBranch)
    -- * Construction
  , treplicate
  , trepeat
    -- * Accessors
  , thead
  , tlast
    -- ** Indexing
  , indexTree
  , tindices
    -- * Modifying trees
  , replaceTree
    -- * Element-wise operations
    -- ** Mapping
  , tmap
  , tzipWith
    -- ** Zipping
  , tzip
    -- ** Unzipping
  , tunzip
    -- * Folding
  , tfold
    -- ** Specialised folds
  , tdfold
    -- ** Prefix sums (scans)
    -- $scans
  , scanlPar
  , tscanl
  , scanrPar
  , tscanr
    -- * Conversions
  , v2t
  , t2v
    -- * Misc
  , lazyT
  )
where

import Control.Applicative         (liftA2)
import Control.DeepSeq             (NFData(..))
import qualified Control.Lens      as Lens
import Data.Default.Class          (Default (..))
import Data.Either                 (isLeft)
import Data.Foldable               (toList)
import Data.Kind                   (Type)
import Data.Singletons             (Apply, TyFun, type (@@))
import Data.Proxy                  (Proxy (..))
import GHC.TypeLits                (KnownNat, Nat, type (+), type (^), type (*))
import Language.Haskell.TH.Syntax  (Lift(..))
#if MIN_VERSION_template_haskell(2,16,0)
import Language.Haskell.TH.Compat
#endif
import Prelude                     hiding ((++), (!!), map)
import Test.QuickCheck             (Arbitrary (..), CoArbitrary (..))

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Class.BitPack         (BitPack (..), packXWith)
import Clash.Promoted.Nat          (SNat (..), UNat (..),
                                    pow2SNat, snatToNum, subSNat, toUNat)
import Clash.Promoted.Nat.Literals (d1)
import Clash.Sized.Index           (Index)
import Clash.Sized.Vector          (Vec (..), (!!), (++), dtfold, replace)
import Clash.XException
  (ShowX (..), NFDataX (..), isX, showsX, showsPrecXWith)

{- $setup
>>> :set -XDataKinds
>>> :set -XTypeFamilies
>>> :set -XTypeOperators
>>> :set -XTemplateHaskell
>>> :set -XFlexibleContexts
>>> :set -XTypeApplications
>>> :set -fplugin GHC.TypeLits.Normalise
>>> :set -XUndecidableInstances
>>> import Clash.Prelude
>>> import Data.Kind
>>> import Data.Singletons (Apply, TyFun)
>>> import Data.Proxy
>>> data IIndex (f :: TyFun Nat Type) :: Type
>>> type instance Apply IIndex l = Index ((2^l)+1)
>>> :{
let populationCount' :: (KnownNat k, KnownNat (2^k)) => BitVector (2^k) -> Index ((2^k)+1)
    populationCount' bv = tdfold (Proxy @IIndex)
                                 fromIntegral
                                 (\_ x y -> add x y)
                                 (v2t (bv2v bv))
:}
-}

-- | Perfect depth binary tree.
--
-- * Only has elements at the leaf of the tree
-- * A tree of depth /d/ has /2^d/ elements.
data RTree :: Nat -> Type -> Type where
  RLeaf :: a -> RTree 0 a
  RBranch :: RTree d a -> RTree d a -> RTree (d+1) a

instance NFData a => NFData (RTree d a) where
    rnf (RLeaf x) = rnf x
    rnf (RBranch l r ) = rnf l `seq` rnf r

textract :: RTree 0 a -> a
textract (RLeaf x)   = x
textract (RBranch _ _) = error $ "textract: nodes hold no values"
{-# NOINLINE textract #-}
{-# ANN textract hasBlackBox #-}

tsplit :: RTree (d+1) a -> (RTree d a,RTree d a)
tsplit (RBranch l r) = (l,r)
tsplit (RLeaf _)   = error $ "tsplit: leaf is atomic"
{-# NOINLINE tsplit #-}
{-# ANN tsplit hasBlackBox #-}

-- | RLeaf of a perfect depth tree
--
-- >>> LR 1
-- 1
-- >>> let x = LR 1
-- >>> :t x
-- x :: Num a => RTree 0 a
--
-- Can be used as a pattern:
--
-- >>> let f (LR a) (LR b) = a + b
-- >>> :t f
-- f :: Num a => RTree 0 a -> RTree 0 a -> a
-- >>> f (LR 1) (LR 2)
-- 3
pattern LR :: a -> RTree 0 a
pattern LR x <- (textract -> x)
  where
    LR x = RLeaf x

-- | RBranch of a perfect depth tree
--
-- >>> BR (LR 1) (LR 2)
-- <1,2>
-- >>> let x = BR (LR 1) (LR 2)
-- >>> :t x
-- x :: Num a => RTree 1 a
--
-- Case be used a pattern:
--
-- >>> let f (BR (LR a) (LR b)) = LR (a + b)
-- >>> :t f
-- f :: Num a => RTree 1 a -> RTree 0 a
-- >>> f (BR (LR 1) (LR 2))
-- 3
pattern BR :: RTree d a -> RTree d a -> RTree (d+1) a
pattern BR l r <- ((\t -> (tsplit t)) -> (l,r))
  where
    BR l r = RBranch l r

instance (KnownNat d, Eq a) => Eq (RTree d a) where
  (==) t1 t2 = (==) (t2v t1) (t2v t2)

instance (KnownNat d, Ord a) => Ord (RTree d a) where
  compare t1 t2 = compare (t2v t1) (t2v t2)

instance Show a => Show (RTree n a) where
  showsPrec _ (RLeaf a)   = shows a
  showsPrec _ (RBranch l r) = \s -> '<':shows l (',':shows r ('>':s))

instance ShowX a => ShowX (RTree n a) where
  showsPrecX = showsPrecXWith go
    where
      go :: Int -> RTree d a -> ShowS
      go _ (RLeaf a)   = showsX a
      go _ (RBranch l r) = \s -> '<':showsX l (',':showsX r ('>':s))

instance KnownNat d => Functor (RTree d) where
  fmap = tmap

instance KnownNat d => Applicative (RTree d) where
  pure  = trepeat
  (<*>) = tzipWith ($)

instance KnownNat d => Foldable (RTree d) where
  foldMap f = tfold f mappend

data TraversableTree (g :: Type -> Type) (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (TraversableTree f a) d = f (RTree d a)

instance KnownNat d => Traversable (RTree d) where
  traverse :: forall f a b . Applicative f => (a -> f b) -> RTree d a -> f (RTree d b)
  traverse f = tdfold (Proxy @(TraversableTree f b))
                      (fmap LR . f)
                      (const (liftA2 BR))

instance (KnownNat d, BitPack a) =>
  BitPack (RTree d a) where
  type BitSize (RTree d a) = (2^d) * (BitSize a)
  pack   = packXWith (pack . t2v)
  unpack = v2t . unpack

type instance Lens.Index   (RTree d a) = Int
type instance Lens.IxValue (RTree d a) = a
instance KnownNat d => Lens.Ixed (RTree d a) where
  ix i f t = replaceTree i <$> f (indexTree t i) <*> pure t

instance (KnownNat d, Default a) => Default (RTree d a) where
  def = trepeat def

instance Lift a => Lift (RTree d a) where
  lift (RLeaf a)     = [| RLeaf a |]
  lift (RBranch t1 t2) = [| RBranch $(lift t1) $(lift t2) |]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedFromUntyped
#endif

instance (KnownNat d, Arbitrary a) => Arbitrary (RTree d a) where
  arbitrary = sequenceA (trepeat arbitrary)
  shrink    = sequenceA . fmap shrink

instance (KnownNat d, CoArbitrary a) => CoArbitrary (RTree d a) where
  coarbitrary = coarbitrary . toList

instance (KnownNat d, NFDataX a) => NFDataX (RTree d a) where
  deepErrorX x = pure (deepErrorX x)

  rnfX t = if isLeft (isX t) then () else go t
   where
    go :: RTree d a -> ()
    go (RLeaf x)   = rnfX x
    go (RBranch l r) = rnfX l `seq` rnfX r

  hasUndefined t = if isLeft (isX t) then True else go t
   where
    go :: RTree d a -> Bool
    go (RLeaf x)   = hasUndefined x
    go (RBranch l r) = hasUndefined l || hasUndefined r

  ensureSpine = fmap ensureSpine . lazyT


{- | A /dependently/ typed fold over trees.

As an example of when you might want to use 'dtfold' we will build a
population counter: a circuit that counts the number of bits set to '1' in
a 'Clash.Sized.BitVector.BitVector'. Given a vector of /n/ bits, we only need we
need a data type that can represent the number /n/: 'Index' @(n+1)@. 'Index' @k@
has a range of @[0 .. k-1]@ (using @ceil(log2(k))@ bits), hence we need 'Index' @n+1@.
As an initial attempt we will use 'tfold', because it gives a nice (@log2(n)@)
tree-structure of adders:

@
populationCount :: (KnownNat (2^d), KnownNat d, KnownNat (2^d+1))
                => BitVector (2^d) -> Index (2^d+1)
populationCount = tfold (resize . bv2i . pack) (+) . v2t . bv2v
@

The \"problem\" with this description is that all adders have the same
bit-width, i.e. all adders are of the type:

@
(+) :: 'Index' (2^d+1) -> 'Index' (2^d+1) -> 'Index' (2^d+1).
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
However, we cannot simply use 'Clash.Sized.Vector.fold' to create a tree-structure of
'Clash.Class.Num.add's:
#if __GLASGOW_HASKELL__ >= 900
>>> :{
let populationCount' :: (KnownNat (2^d), KnownNat d, KnownNat (2^d+1))
                     => BitVector (2^d) -> Index (2^d+1)
    populationCount' = tfold (resize . bv2i . pack) add . v2t . bv2v
:}
<BLANKLINE>
<interactive>:...
    • Couldn't match type: (((2 ^ d) + 1) + ((2 ^ d) + 1)) - 1
                     with: (2 ^ d) + 1
      Expected: Index ((2 ^ d) + 1)
                -> Index ((2 ^ d) + 1) -> Index ((2 ^ d) + 1)
        Actual: Index ((2 ^ d) + 1)
                -> Index ((2 ^ d) + 1)
                -> AResult (Index ((2 ^ d) + 1)) (Index ((2 ^ d) + 1))
    • In the second argument of ‘tfold’, namely ‘add’
      In the first argument of ‘(.)’, namely
        ‘tfold (resize . bv2i . pack) add’
      In the expression: tfold (resize . bv2i . pack) add . v2t . bv2v
    • Relevant bindings include
        populationCount' :: BitVector (2 ^ d) -> Index ((2 ^ d) + 1)
          (bound at ...)

#else
>>> :{
let populationCount' :: (KnownNat (2^d), KnownNat d, KnownNat (2^d+1))
                     => BitVector (2^d) -> Index (2^d+1)
    populationCount' = tfold (resize . bv2i . pack) add . v2t . bv2v
:}
<BLANKLINE>
<interactive>:...
    • Couldn't match type ‘(((2 ^ d) + 1) + ((2 ^ d) + 1)) - 1’
                     with ‘(2 ^ d) + 1’
      Expected type: Index ((2 ^ d) + 1)
                     -> Index ((2 ^ d) + 1) -> Index ((2 ^ d) + 1)
        Actual type: Index ((2 ^ d) + 1)
                     -> Index ((2 ^ d) + 1)
                     -> AResult (Index ((2 ^ d) + 1)) (Index ((2 ^ d) + 1))
    • In the second argument of ‘tfold’, namely ‘add’
      In the first argument of ‘(.)’, namely
        ‘tfold (resize . bv2i . pack) add’
      In the expression: tfold (resize . bv2i . pack) add . v2t . bv2v
    • Relevant bindings include
        populationCount' :: BitVector (2 ^ d) -> Index ((2 ^ d) + 1)
          (bound at ...)

#endif

because 'tfold' expects a function of type \"@b -> b -> b@\", i.e. a function
where the arguments and result all have exactly the same type.

In order to accommodate the type of our 'Clash.Class.Num.add', where the
result is larger than the arguments, we must use a dependently typed fold in
the form of 'dtfold':

@
{\-\# LANGUAGE UndecidableInstances \#-\}
import Data.Singletons

data IIndex (f :: 'TyFun' Nat Type) :: Type
type instance 'Apply' IIndex l = 'Index' ((2^l)+1)

populationCount' :: (KnownNat k, KnownNat (2^k))
                 => BitVector (2^k) -> Index ((2^k)+1)
populationCount' bv = 'tdfold' (Proxy @IIndex)
                             (resize . bv2i . pack)
                             (\\_ x y -> 'Clash.Class.Num.add' x y)
                             ('v2t' ('Clash.Sized.Vector.bv2v' bv))
@

And we can test that it works:

>>> :t populationCount' (7 :: BitVector 16)
populationCount' (7 :: BitVector 16) :: Index 17
>>> populationCount' (7 :: BitVector 16)
3
-}
tdfold :: forall p k a . KnownNat k
       => Proxy (p :: TyFun Nat Type -> Type) -- ^ The /motive/
       -> (a -> (p @@ 0)) -- ^ Function to apply to the elements on the leafs
       -> (forall l . SNat l -> (p @@ l) -> (p @@ l) -> (p @@ (l+1)))
       -- ^ Function to fold the branches with.
       --
       -- __NB:__ @SNat l@ is the depth of the two sub-branches.
       -> RTree k a -- ^ Tree to fold over.
       -> (p @@ k)
tdfold _ f g = go SNat
  where
    go :: SNat m -> RTree m a -> (p @@ m)
    go _  (RLeaf a)   = f a
    go sn (RBranch l r) = let sn' = sn `subSNat` d1
                      in  g sn' (go sn' l) (go sn' r)
{-# NOINLINE tdfold #-}
{-# ANN tdfold hasBlackBox #-}

data TfoldTree (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (TfoldTree a) d = a

-- | Reduce a tree to a single element
tfold :: forall d a b .
         KnownNat d
      => (a -> b) -- ^ Function to apply to the leaves
      -> (b -> b -> b) -- ^ Function to combine the results of the reduction
                       -- of two branches
      -> RTree d a -- ^ Tree to fold reduce
      -> b
tfold f g = tdfold (Proxy @(TfoldTree b)) f (const g)

-- | \"'treplicate' @d a@\" returns a tree of depth /d/, and has /2^d/ copies
-- of /a/.
--
-- >>> treplicate (SNat :: SNat 3) 6
-- <<<6,6>,<6,6>>,<<6,6>,<6,6>>>
-- >>> treplicate d3 6
-- <<<6,6>,<6,6>>,<<6,6>,<6,6>>>
treplicate :: forall d a . SNat d -> a -> RTree d a
treplicate sn a = go (toUNat sn)
  where
    go :: UNat n -> RTree n a
    go UZero      = LR a
    go (USucc un) = BR (go un) (go un)
{-# NOINLINE treplicate #-}
{-# ANN treplicate hasBlackBox #-}

-- | \"'trepeat' @a@\" creates a tree with as many copies of /a/ as demanded by
-- the context.
--
-- >>> trepeat 6 :: RTree 2 Int
-- <<6,6>,<6,6>>
trepeat :: KnownNat d => a -> RTree d a
trepeat = treplicate SNat

data MapTree (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (MapTree a) d = RTree d a

-- | \"'tmap' @f t@\" is the tree obtained by apply /f/ to each element of /t/,
-- i.e.,
--
-- > tmap f (BR (LR a) (LR b)) == BR (LR (f a)) (LR (f b))
tmap :: forall d a b . KnownNat d => (a -> b) -> RTree d a -> RTree d b
tmap f = tdfold (Proxy @(MapTree b)) (LR . f) (\_ l r -> BR l r)

-- | Generate a tree of indices, where the depth of the tree is determined by
-- the context.
--
-- >>> tindices :: RTree 3 (Index 8)
-- <<<0,1>,<2,3>>,<<4,5>,<6,7>>>
tindices :: forall d . KnownNat d => RTree d (Index (2^d))
tindices =
  tdfold (Proxy @(MapTree (Index (2^d)))) LR
         (\s@SNat l r -> BR l (tmap (+(snatToNum (pow2SNat s))) r))
         (treplicate SNat 0)

data V2TTree (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (V2TTree a) d = RTree d a

-- | Convert a vector with /2^d/ elements to a tree of depth /d/.
--
-- >>> v2t (1 :> 2 :> 3 :> 4:> Nil)
-- <<1,2>,<3,4>>
v2t :: forall d a . KnownNat d => Vec (2^d) a -> RTree d a
v2t = dtfold (Proxy @(V2TTree a)) LR (const BR)

data T2VTree (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (T2VTree a) d = Vec (2^d) a

-- | Convert a tree of depth /d/ to a vector of /2^d/ elements
--
-- >>> (BR (BR (LR 1) (LR 2)) (BR (LR 3) (LR 4)))
-- <<1,2>,<3,4>>
-- >>> t2v (BR (BR (LR 1) (LR 2)) (BR (LR 3) (LR 4)))
-- 1 :> 2 :> 3 :> 4 :> Nil
t2v :: forall d a . KnownNat d => RTree d a -> Vec (2^d) a
t2v = tdfold (Proxy @(T2VTree a)) (:> Nil) (\_ l r -> l ++ r)

-- | \"'indexTree' @t n@\" returns the /n/'th element of /t/.
--
-- The bottom-left leaf had index /0/, and the bottom-right leaf has index
-- /2^d-1/, where /d/ is the depth of the tree
--
-- >>> indexTree (BR (BR (LR 1) (LR 2)) (BR (LR 3) (LR 4))) 0
-- 1
-- >>> indexTree (BR (BR (LR 1) (LR 2)) (BR (LR 3) (LR 4))) 2
-- 3
-- >>> indexTree (BR (BR (LR 1) (LR 2)) (BR (LR 3) (LR 4))) 14
-- *** Exception: Clash.Sized.Vector.(!!): index 14 is larger than maximum index 3
-- ...
indexTree :: (KnownNat d, Enum i) => RTree d a -> i -> a
indexTree t i = (t2v t) !! i

-- | \"'replaceTree' @n a t@\" returns the tree /t/ where the /n/'th element is
-- replaced by /a/.
--
-- The bottom-left leaf had index /0/, and the bottom-right leaf has index
-- /2^d-1/, where /d/ is the depth of the tree
--
-- >>> replaceTree 0 5 (BR (BR (LR 1) (LR 2)) (BR (LR 3) (LR 4)))
-- <<5,2>,<3,4>>
-- >>> replaceTree 2 7 (BR (BR (LR 1) (LR 2)) (BR (LR 3) (LR 4)))
-- <<1,2>,<7,4>>
-- >>> replaceTree 9 6 (BR (BR (LR 1) (LR 2)) (BR (LR 3) (LR 4)))
-- <<1,2>,<3,*** Exception: Clash.Sized.Vector.replace: index 9 is larger than maximum index 3
-- ...
replaceTree :: (KnownNat d, Enum i) => i -> a -> RTree d a -> RTree d a
replaceTree i a = v2t . replace i a . t2v

data ZipWithTree (b :: Type) (c :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (ZipWithTree b c) d = RTree d b -> RTree d c

-- | 'tzipWith' generalizes 'tzip' by zipping with the function given as the
-- first argument, instead of a tupling function. For example, "tzipWith (+)"
-- applied to two trees produces the tree of corresponding sums.
--
-- > tzipWith f (BR (LR a1) (LR b1)) (BR (LR a2) (LR b2)) == BR (LR (f a1 a2)) (LR (f b1 b2))
tzipWith :: forall a b c d . KnownNat d => (a -> b -> c) -> RTree d a -> RTree d b -> RTree d c
tzipWith f = tdfold (Proxy @(ZipWithTree b c)) lr br
  where
    lr :: a -> RTree 0 b -> RTree 0 c
    lr a t = LR (f a (textract t))

    br :: SNat l
       -> (RTree l b -> RTree l c)
       -> (RTree l b -> RTree l c)
       -> RTree (l+1) b
       -> RTree (l+1) c
    br _ fl fr t = BR (fl l) (fr r)
      where
        (l,r) = tsplit t


-- | 'tzip' takes two trees and returns a tree of corresponding pairs.
tzip :: KnownNat d => RTree d a -> RTree d b -> RTree d (a,b)
tzip = tzipWith (,)

data UnzipTree (a :: Type) (b :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (UnzipTree a b) d = (RTree d a, RTree d b)

-- | 'tunzip' transforms a tree of pairs into a tree of first components and a
-- tree of second components.
tunzip :: forall d a b . KnownNat d => RTree d (a,b) -> (RTree d a,RTree d b)
tunzip = tdfold (Proxy @(UnzipTree a b)) lr br
  where
    lr   (a,b) = (LR a,LR b)

    br _ (l1,r1) (l2,r2) = (BR l1 l2, BR r1 r2)

-- | Given a function @f@ that is strict in its /n/th 'RTree' argument, make it
-- lazy by applying 'lazyT' to this argument:
--
-- > f x0 x1 .. (lazyT xn) .. xn_plus_k
lazyT :: KnownNat d
      => RTree d a
      -> RTree d a
lazyT = tzipWith (flip const) (trepeat ())

-- | Extract the first element of a tree
--
-- The first element is defined to be the bottom-left leaf.
--
-- >>> thead $ BR (BR (LR 1) (LR 2)) (BR (LR 3) (LR 4))
-- 1
thead :: RTree n a -> a
thead (RLeaf x) = x
thead (RBranch x _) = thead x

-- | Extract the last element of a tree
--
-- The last element is defined to be the bottom-right leaf.
--
-- >>> tlast $ BR (BR (LR 1) (LR 2)) (BR (LR 3) (LR 4))
-- 4
tlast :: RTree n a -> a
tlast (RLeaf x) = x
tlast (RBranch _ y) = tlast y

{- $scans #scans#

Scans (`Clash.Sized.Vector.scanl`, `Clash.Sized.Vector.scanr`) are similar to
folds (`Clash.Sized.Vector.foldl`, `Clash.Sized.Vector.foldr`) but return a list
of successive reduced values. When the binary reduction operator @f@ is
associative, the scan functions in this module can be characterized as follows:

> tscanl f [x1, x2, x3, ...] == [x1, x1 `f` x2, x1 `f` x2 `f` x3, ...]

> tscanr f [..., xn2, xn1, xn] == [..., xn2 `f` xn1 `f` xn, xn1 `f` xn, xn]

The scan functions in this module provide a different trade-off between circuit
size and logic depth than the default `Clash.Sized.Vector.scanl` and
`Clash.Sized.Vector.scanr` functions. When \(n\) is the number of elements,
circuit size is \(\mathcal{O}(n \cdot \log n)\), but logic depth is \(\mathcal{O}(\log n)\).
This means the resource usage will likely increase, but the maximum clock
frequency also increases due to the reduced logic depth. The exact amount of
instantiations of @f@ given a tree of depth /d/ is:

> work 0 = 0
> work d = 2 ^ (d - 1) + 2 * work (d - 1)

-}

-- | `tscanl` applied to `Vec`
--
-- >>> scanlPar (+) (1 :> 2 :> 3 :> 4 :> Nil)
-- 1 :> 3 :> 6 :> 10 :> Nil
scanlPar ::
  KnownNat n =>
  -- | Must be associative
  (a -> a -> a) ->
  Vec (2^n) a ->
  Vec (2^n) a
scanlPar op = t2v . tscanl op . v2t
{-# INLINE scanlPar #-}

-- | `tscanr` applied to `Vec`
--
-- >>> scanrPar (+) (1 :> 2 :> 3 :> 4 :> Nil)
-- 10 :> 9 :> 7 :> 4 :> Nil
scanrPar ::
  KnownNat n =>
   -- | Must be associative
  (a -> a -> a) ->
  Vec (2^n) a ->
  Vec (2^n) a
scanrPar op = t2v . tscanr op . v2t
{-# INLINE scanrPar #-}

-- | Low-depth left scan
--
-- `tscanl` is similar to `Clash.Sized.Vector.foldl`, but returns a tree of
-- successive reduced values from the left:
--
-- > tscanl f [x1, x2, x3, ...] == [x1, x1 `f` x2, x1 `f` x2 `f` x3, ...]
--
-- >>> tscanl (+) (v2t (1 :> 2 :> 3 :> 4 :> Nil))
-- <<1,3>,<6,10>>
--
-- <<doc/scanlPar.svg>>
tscanl ::
  forall a n.
  KnownNat n =>
  -- | Must be associative
  (a -> a -> a) ->
  RTree n a ->
  RTree n a
tscanl op tr =
  case tr of
    RLeaf x -> LR x
    RBranch x y ->
      let
        x' = tscanl op x
        y' = tscanl op y
        l = tlast x'
      in BR x' (fmap (l `op`) y')

-- | Low-depth right scan
--
-- `tscanr` is similar to `Clash.Sized.Vector.foldr`, but returns a tree of
-- successive reduced values from the left:
--
-- > tscanr f [..., xn2, xn1, xn] == [..., xn2 `f` xn1 `f` xn, xn1 `f` xn, xn]
--
-- >>> tscanr (+) (v2t (1 :> 2 :> 3 :> 4 :> Nil))
-- <<10,9>,<7,4>>
tscanr ::
  forall a n.
  KnownNat n =>
  (a -> a -> a) ->
  RTree n a ->
  RTree n a
tscanr op tr =
  case tr of
    RLeaf x -> LR x
    RBranch x y ->
        let
          x' = tscanr op x
          y' = tscanr op y
          l = thead y'
        in BR (fmap (l `op`) x') y'
