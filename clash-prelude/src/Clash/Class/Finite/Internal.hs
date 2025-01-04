{-|
Copyright  :  (C) 2024-2025, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module Clash.Class.Finite.Internal
  ( Finite(..)
  , GFinite(..)
  , BoundedEnumEq(..)
  , FiniteDerive(..)
  , ReversedIndexOrder(..)
  , WithUndefined(..)
  )
where

import Prelude hiding ((++), (!!), concatMap, foldl, foldr, repeat, reverse)

import Control.Applicative (Alternative(..))
import Control.Arrow (second)
import Data.Bits (Bits(..), FiniteBits(..))
import Data.Coerce (coerce)
import Data.Constraint (Dict(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product)
import Data.Functor.Sum (Sum)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Ord (Down(..))
import Data.Proxy (Proxy(..))
import Data.Singletons (Apply, TyFun)
import Data.Void (Void)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics
  (Generic(..), Rep, V1, U1(..), M1(..), K1(..), (:+:)(..), (:*:)(..))
import GHC.TypeNats
  (Nat, KnownNat, type (^), type (<=), type (*), type (+), type (-))

import Clash.Class.Num (SaturatingNum(..), SaturationMode(..))
import Clash.Class.Finite.Internal.Dictionaries
  ( powGTZero, powMonotone1, powLawsRewrite
#if !MIN_VERSION_base(4,16,0)
  , powCLogDual, leqOnePlusMinus
#endif
  )
import Clash.Class.Finite.Internal.TH (deriveFiniteTuples)
import Clash.Class.Resize (Resize(..))
import Clash.Class.BitPack (BitPack(..), bitCoerce)
import Clash.Promoted.Nat
  ( SNat(..), UNat(..), toUNat, fromUNat, natToNum, snatToNum
  )
import Clash.Sized.Index (Index)
import Clash.Sized.Internal.BitVector (BitVector(..), Bit(..), high, low)
import Clash.Sized.Internal.Unsigned (Unsigned(..))
import Clash.Sized.Signed (Signed)
import Clash.Sized.RTree (RTree(..), tdfold, tfold, trepeat)
import Clash.Sized.Vector
  ( Vec(..), (++), (!!), concatMap, bv2v, dfold, foldl, foldr
  , ifoldr, indicesI, iterateI, unfoldrI, repeat, reverse, replace
  )
import Clash.XException (ShowX, NFDataX)

import qualified Data.List as List (iterate)

{- $setup
>>> :m -Prelude
>>> :set -XDeriveAnyClass
>>> import Clash.Prelude
-}

-- * Finite Class

-- | The class of types holding only a finite number of elements.
--
-- The class supports generic deriving, i.e., for custom types the
-- class instances can be derived via @derive (Generic, Finite)@
-- requiring that all inner types of the type declaration have
-- @Finite@ instances as well.
--
-- >>> data T = B Bit | D (Index 2) (Signed 1) deriving (Generic, Finite, Show)
-- >>> natToNum @(ElementCount T)
-- 6
-- >>> elements @T
-- B 0 :> B 1 :> D 0 -1 :> D 0 0 :> D 1 -1 :> D 1 0 :> Nil
-- >>> lowest @T
-- Just (B 0)
-- >>> highest @T
-- Just (D 1 0)
-- >>> after (B 1)
-- Just (D 0 -1)
-- >>> before (B 0)
-- Nothing
-- >>> index (D 0 0)
-- 3
-- >>> ith @T 5
-- D 1 0
--
-- Any definition must satisfy the following laws (automatically
-- ensured when generic deriving the instance):
--
-- [Index Order]
--   @ index '<$>' elements = 'indicesI' @
-- [Forward Iterate]
--   @ 'iterateI' ('>>=' after) (lowest \@a) = 'Just' '<$>' (elements \@a) @
-- [Backward Iterate]
--   @ 'iterateI' ('>>=' before) (highest \@a) = 'Just' '<$>' (elements \@a) @
-- [Index Isomorphism]
--   @ith (index x) = x@
-- [Minimum Predecessor]
--   @ lowest '>>=' before = 'Nothing' @
-- [Maximum Successor]
--   @ highest '>>=' after = 'Nothing' @
-- [No Uninhabited Extremes]
--   @ lowest \@a = 'Nothing' /and/ highest \@a = 'Nothing'
--     /if and only if/ ElementCount a = 0 @
--
-- Furthermore, if the type also has a 'BitPack' instance, then it is
-- recommended to choose an index order that is compatible with the
-- one over the bit packed representations, i.e., an order satisfying
--
-- [BitPack Compatibility (optional)]
--   @ index x <= index y /if and only if/ pack x <= pack y @
--
class KnownNat (ElementCount a) => Finite a where
  -- | The number of elements of the type.
  type ElementCount a :: Nat
  type ElementCount a = GElementCount (Rep a)

  -- | The elements of the type.
  elements :: Vec (ElementCount a) a
  default elements ::
    ( Generic a, GFinite (Rep a)
    , ElementCount a ~ GElementCount (Rep a)
    ) => Vec (ElementCount a) a
  elements = to <$> gElements

  -- | Just the @0@ indexed element. Nothing if @ElementCount a = 0@.
  lowest :: Maybe a
  default lowest ::
    ( Generic a, GFinite (Rep a)
    ) => Maybe a
  lowest = to <$> gLowest

  -- | Just the @(ElementCount a - 1)@ indexed element. Nothing if
  -- @ElementCount a = 0@.
  highest :: Maybe a
  default highest ::
    ( Generic a, GFinite (Rep a)
    ) => Maybe a
  highest = to <$> gHighest

  -- | Just the element before the given one according to the
  -- associated index order with the lowest one being the only element
  -- that has no predecessor.
  before :: a -> Maybe a
  default before ::
    ( Generic a, GFinite (Rep a)
    ) => a -> Maybe a
  before = fmap to . gBefore . from

  -- | Just the element after the given one according to the
  -- associated index order with the highest one being the only
  -- element that has no successor.
  after :: a -> Maybe a
  default after ::
    ( Generic a, GFinite (Rep a)
    ) => a -> Maybe a
  after = fmap to . gAfter . from

  -- | Maps from an index to the associated element.
  ith :: Index (ElementCount a) -> a
  default ith ::
    ( Generic a, GFinite (Rep a)
    , ElementCount a ~ GElementCount (Rep a)
    ) => Index (ElementCount a) -> a
  ith = to . gIth

  -- | Maps an element of the type to it's associated index.
  index :: a -> Index (ElementCount a)
  default index ::
    ( Generic a, GFinite (Rep a)
    , ElementCount a ~ GElementCount (Rep a)
    ) => a -> Index (ElementCount a)
  index = gIndex . from

  -- | Returns the suffix slice of 'elements' starting at the index
  -- provided via the @SNat@ argument.
  elementsFrom ::
    n + 1 <= ElementCount a =>
    SNat n -> Vec (ElementCount a - n) a
  elementsFrom sn@SNat =
    iterateI (fromJust . after) (ith $ snatToNum sn)

  -- | Returns the infix slice of 'elements' from the index provided
  -- via the first @SNat@ argument to the index provided via the
  -- second one.
  elementsFromTo ::
    (n + 1 <= ElementCount a, n <= m, m + 1 <= ElementCount a) =>
    SNat n -> SNat m -> Vec (m - n + 1) a
  elementsFromTo sn@SNat SNat =
    iterateI (fromJust . after) (ith $ snatToNum sn)

class KnownNat (GElementCount rep) => GFinite rep where
  type GElementCount rep :: Nat
  gElements :: Vec (GElementCount rep) (rep a)
  gLowest   :: Maybe (rep a)
  gHighest  :: Maybe (rep a)
  gBefore   :: rep a -> Maybe (rep a)
  gAfter    :: rep a -> Maybe (rep a)
  gIth      :: Index (GElementCount rep) -> rep a
  gIndex    :: rep a -> Index (GElementCount rep)

instance GFinite V1 where
  type GElementCount V1 = 0
  gElements = Nil
  gLowest   = Nothing
  gHighest  = Nothing
  gBefore   = const Nothing
  gAfter    = const Nothing
  -- GHC has no knowledge about Index 0 being isomorphic to Void,
  -- i.e., being an uninhabited type. Hence, we need to throw an error
  -- here although there provably are no values that can ever be
  -- passed to gIth.
  gIth a    = error $ "Index 0 cannot contain values like " <> show a
  gIndex    = \case {}

instance GFinite U1 where
  type GElementCount U1 = 1
  gElements = U1 :> Nil
  gLowest   = Just U1
  gHighest  = Just U1
  gBefore   = const Nothing
  gAfter    = const Nothing
  gIth      = const U1
  gIndex    = const 0

instance Finite a => GFinite (K1 i a) where
  type GElementCount (K1 _ a)  = ElementCount a
  gElements = K1 <$> elements
  gLowest   = K1 <$> lowest
  gHighest  = K1 <$> highest
  gBefore   = fmap K1 . before . unK1
  gAfter    = fmap K1 . after . unK1
  gIth      = K1 . ith
  gIndex    = index . unK1

instance GFinite a => GFinite (M1 i v a) where
  type GElementCount (M1 _ _ a) = GElementCount a
  gElements = M1 <$> gElements
  gLowest   = M1 <$> gLowest
  gHighest  = M1 <$> gHighest
  gBefore   = fmap M1 . gBefore . unM1
  gAfter    = fmap M1 . gAfter . unM1
  gIth      = M1 . gIth
  gIndex    = gIndex . unM1

instance (GFinite a, GFinite b) => GFinite (a :*: b) where
  type GElementCount (a :*: b) = GElementCount a * GElementCount b
  gElements = concatMap (\a -> (a :*:) <$> gElements @b) (gElements @a)
  gLowest   = (:*:) <$> gLowest  <*> gLowest
  gHighest  = (:*:) <$> gHighest <*> gHighest

  gBefore (a :*: b) =
        (:*:)             a <$> gBefore b
    <|> (:*:) <$> gBefore a <*> gHighest
  gAfter (a :*: b) =
        (:*:)            a <$> gAfter b
    <|> (:*:) <$> gAfter a <*> gLowest

  gIth x = gIth (resize $ x `div` m) :*: gIth (resize $ x `mod` m)
   where
    m = natToNum @(GElementCount b)

  gIndex (a :*: b) =
      resize (gIndex a) * natToNum @(GElementCount b)
    + resize (gIndex b)

instance (GFinite a, GFinite b) => GFinite (a :+: b) where
  type GElementCount (a :+: b) = GElementCount a + GElementCount b
  gElements = (L1 <$> gElements @a) ++ (R1 <$> gElements @b)
  gLowest   = L1 <$> gLowest  @a <|> R1 <$> gLowest  @b
  gHighest  = R1 <$> gHighest @b <|> L1 <$> gHighest @a

  gBefore = \case
    L1 x -> L1 <$> gBefore x
    R1 x -> R1 <$> gBefore x <|> L1 <$> gHighest

  gAfter = \case
    R1 x -> R1 <$> gAfter x
    L1 x -> L1 <$> gAfter x <|> R1 <$> gLowest

  gIth x
    | x < n     = L1 $ gIth $ truncateB x
    | otherwise = R1 $ gIth $ truncateB $ x - n
   where
    n = natToNum @(GElementCount a)

  gIndex = \case
    L1 x -> extend (gIndex x)
    R1 x -> extend (gIndex x) + natToNum @(GElementCount a)

instance Finite Void
instance Finite ()
instance Finite Bool
instance Finite Ordering

deriving via BoundedEnumEq 0x110000         Char   instance Finite Char
deriving via BoundedEnumEq (2^BitSize Int)  Int    instance Finite Int
deriving via BoundedEnumEq (2^8)            Int8   instance Finite Int8
deriving via BoundedEnumEq (2^16)           Int16  instance Finite Int16
deriving via BoundedEnumEq (2^32)           Int32  instance Finite Int32
deriving via BoundedEnumEq (2^64)           Int64  instance Finite Int64
deriving via BoundedEnumEq (2^BitSize Word) Word   instance Finite Word
deriving via BoundedEnumEq (2^8)            Word8  instance Finite Word8
deriving via BoundedEnumEq (2^16)           Word16 instance Finite Word16
deriving via BoundedEnumEq (2^32)           Word32 instance Finite Word32
deriving via BoundedEnumEq (2^64)           Word64 instance Finite Word64

deriving newtype instance Finite a         => Finite (Const a b)
deriving newtype instance Finite a         => Finite (Down a)
deriving newtype instance Finite a         => Finite (Identity a)
deriving newtype instance Finite (f (g a)) => Finite (Compose f g a)

instance  Finite a                    => Finite (Maybe a)
instance (Finite a,     Finite b    ) => Finite (Either a b)
instance (Finite (f a), Finite (g a)) => Finite (Product f g a)
instance (Finite (f a), Finite (g a)) => Finite (Sum f g a)

instance KnownNat n => Finite (Index n) where
  type ElementCount (Index n) = n
  elements = indicesI
  lowest   = case toUNat (SNat @n) of
    UZero -> Nothing
    _     -> Just minBound
  highest  = case toUNat (SNat @n) of
    UZero -> Nothing
    _     -> Just maxBound
  before   = case toUNat (SNat @n) of
    UZero -> const Nothing
    _     -> \n -> if n == minBound then Nothing else Just $ n - 1
  after    = case toUNat (SNat @n)of
    UZero -> const Nothing
    _     -> \n -> if n == maxBound then Nothing else Just $ n + 1
  ith      = id
  index    = id

instance KnownNat n => Finite (Signed n) where
  type ElementCount (Signed n) = 2^n
  elements = iterateI (+1) minBound
  lowest   = Just minBound
  highest  = Just maxBound
  before n = if n == minBound then Nothing else Just $ n - 1
  after n  = if n == maxBound then Nothing else Just $ n + 1
  ith      = unpack . xor (complement (complement 0 `shiftR` 1)) . pack
  index    = unpack . xor (complement (complement 0 `shiftR` 1)) . pack

instance KnownNat n => Finite (Unsigned n) where
  type ElementCount (Unsigned n) = 2^n
  elements = iterateI (+1) minBound
  lowest   = Just minBound
  highest  = Just maxBound
  before n = if n == minBound then Nothing else Just $ n - 1
  after n  = if n == maxBound then Nothing else Just $ n + 1
  ith      = bitCoerce
  index    = bitCoerce

instance Finite Bit where
  type ElementCount Bit = 2
  elements = low :> high :> Nil
  lowest   = Just low
  highest  = Just high
  before b = if b == low then Nothing else Just low
  after b  = if b == high then Nothing else Just high
  ith      = \case { 0 -> low; _ -> high }
  index b  = if b == low then 0 else 1

instance KnownNat n => Finite (BitVector n) where
  type ElementCount (BitVector n) = 2^n
  elements  = iterateI (+1) 0
  lowest    = Just minBound
  highest   = Just maxBound
  before bv = if bv == minBound then Nothing else Just $ bv - 1
  after bv  = if bv == maxBound then Nothing else Just $ bv + 1
  ith       = pack
  index     = unpack

data PowV (k :: Nat) (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (PowV k a) n = Vec (k^n) (Vec n a)

instance (KnownNat n, Finite a) => Finite (Vec n a) where
  type ElementCount (Vec n a) = ElementCount a^n

  elements = dfold
    (Proxy @(PowV (ElementCount a) a))
    (\_ _ -> concatMap ((<$> elements) . (:<)))
    (Nil :> Nil)
    (repeat @n ())

  lowest = repeat <$> lowest

  highest = repeat <$> highest

  before v = do
    h <- highest
    either Just (const Nothing)
      $ ifoldr
          (\i x a -> case before x of
              Nothing -> replace i h <$> a
              Just y  -> a >>= Left . replace i y
          ) (Right v) v

  after v = do
    l <- lowest
    either Just (const Nothing)
      $ ifoldr
          (\i x a -> case after x of
              Nothing -> replace i l <$> a
              Just y  -> a >>= Left . replace i y
          ) (Right v) v

  ith = (reverse .) . unfoldrI $ \i ->
    ( ith $ resize $ i `mod` natToNum @(ElementCount a)
    , i `div` natToNum @(ElementCount a)
    )

  index = (fst .) . (`foldr` (0, 1))
    $ \a (n, p) ->
        ( p * resize (index a) + n
        , natToNum @(ElementCount a) * p
        )

data PowT (k :: Nat) (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (PowT k a) d = Vec (k^(2^d)) (RTree d a)

instance (KnownNat d, Finite a) => Finite (RTree d a) where
  type ElementCount (RTree d a) = ElementCount a^(2^d)

  elements = tdfold
    (Proxy @(PowT (ElementCount a) a))
    (const $ RLeaf <$> (elements @a))
    (\(_ :: SNat m) l r -> case powLawsRewrite @(ElementCount a) @m of
       Dict -> concatMap ((<$> r) . RBranch) l
    )
    (trepeat @d ())

  lowest  = trepeat <$> lowest
  highest = trepeat <$> highest

  before t = highest >>= beforeAfterT# t before
  after t = lowest  >>= beforeAfterT# t after

  ith = case toUNat (SNat @d) of
    UZero -> RLeaf . ith
    USucc (_ :: UNat p) -> \i -> RBranch
      (ith @(RTree p a) $ resize $ i `div` m)
      (ith @(RTree p a) $ resize $ i `mod` m)
     where
      m = natToNum @(ElementCount a^(2^p))

  index =
    fst . tfold
      ((, natToNum @(ElementCount a)) . resize . index)
      (\(nL, pL) (nR, pR) -> (nR + pR * nL, pL * pR))

data IterT (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (IterT a) d = (RTree d a, (Bool, RTree d a))

beforeAfterT# :: forall n a. KnownNat n =>
  RTree n a -> (a -> Maybe a) -> a -> Maybe (RTree n a)
beforeAfterT# t op o
  | hasAfter  = return t'
  | otherwise = Nothing
 where
  (hasAfter, t') = snd $ tdfold (Proxy @(IterT a)) fLeaf fBranch t

  fLeaf x = (RLeaf x, ) $ case op x of
    Nothing -> (False, RLeaf o)
    Just y  -> (True,  RLeaf y)

  fBranch _ (lO, (lF, lM)) (rO, (rF, rM)) =
    (RBranch lO rO, )
      $ if rF then (rF, RBranch lO rM)
              else (lF, RBranch lM rM)

instance (Finite a, Finite b) => Finite (a -> b) where
  type ElementCount (a -> b) = ElementCount b^ElementCount a
  elements = fmap ((. index) . (!!)) $ elements @(Vec (ElementCount a) b)
  lowest   = const <$> lowest
  highest  = const <$> highest

  before f = do
    h <- highest
    either Just (const Nothing)
      $ foldr (\i -> (=<<) $ \g -> do
                 let g' y x = if index x == i then y else g x
                 maybe (Right $ g' h) (Left . g') $ before $ g $ ith i
              ) (Right f) $ indicesI @(ElementCount a)

  after f = do
    l <- lowest
    either Just (const Nothing)
      $ foldr (\i -> (=<<) $ \g -> do
                 let g' y x = if index x == i then y else g x
                 maybe (Right $ g' l) (Left . g') $ after $ g $ ith i
              ) (Right f) $ indicesI @(ElementCount a)

  ith      = ((. index) . (!!)) . ith @(Vec (ElementCount a) b)
  index f  = index (f . ith <$> indicesI)

-- | A newtype wrapper, which reverses the index order used by the
-- finite instance of the inner type.
--
-- >>> elements @(Maybe Bool)
-- Nothing :> Just False :> Just True :> Nil
--
-- >>> elements @(ReversedIndexOrder (Maybe Bool))
-- Just True :> Just False :> Nothing :> Nil
newtype ReversedIndexOrder a = ReversedIndexOrder { getReversedIndexOrder :: a }
  deriving newtype ( Bits, BitPack, Bounded, Enum, Eq, FiniteBits
                   , Generic, Integral, NFDataX, Num, Ord, Real, Read
                   , Show, ShowX
                   )

-- | see 'ReversedIndexOrder'
instance Finite a => Finite (ReversedIndexOrder a) where
  type ElementCount (ReversedIndexOrder a) = ElementCount a
  elements = ReversedIndexOrder <$> reverse elements
  lowest   = ReversedIndexOrder <$> highest
  highest  = ReversedIndexOrder <$> lowest
  before   = fmap ReversedIndexOrder . after . getReversedIndexOrder
  after    = fmap ReversedIndexOrder . before . getReversedIndexOrder
  ith      = ReversedIndexOrder . ith . (maxBound -)
  index    = (maxBound -) . index . getReversedIndexOrder

-- | The elements of the 'Bit' and 'BitVector' types may have
-- undefined bits, which are not in scope when using their default
-- 'Finite' class instances. The default instances only consider the
-- synthesizable fragment of the types, while for simulation or
-- testing purposes, it may be useful to have access to the range of
-- undefined inhabitants as well.
--
-- The @Finite@ instances of @WithUndefined Bit@ and @WithUndefined
-- (BitVector n)@ also add the elements containing undefined bits, but
-- are __not synthesizable__ as a consequence. They make use of a
-- special index order, that first enumerates all well-defined values,
-- i.e., those that have no undefined bits, and then continues with
-- the non-well-defined ones.
--
-- >>> elements @(BitVector 2)
-- 0b00 :> 0b01 :> 0b10 :> 0b11 :> Nil
--
-- >>> elements @(WithUndefined (BitVector 2))
-- 0b00 :> 0b01 :> 0b10 :> 0b11 :> 0b0. :> 0b1. :> 0b.0 :> 0b.1 :> 0b.. :> Nil
newtype WithUndefined a = WithUndefined { getWithUndefined :: a }
  deriving newtype ( Bits, BitPack, Bounded, Enum, Eq, FiniteBits
                   , Generic, Integral, NFDataX, Num, Ord, Real, Read
                   , Show, ShowX
                   )

-- | __NB__: not synthesizable (see 'WithUndefined')
instance Finite (WithUndefined Bit) where
  type ElementCount (WithUndefined Bit) = 3
  elements = coerce <$> Bit 0 0 :> Bit 0 1 :> Bit 1 0 :> Nil
  lowest   = Just $ coerce $ Bit 0 0
  highest  = Just $ coerce $ Bit 1 0
  before b   = fmap coerce $ case coerce b of
    Bit 0 0 -> Nothing
    Bit 0 _ -> Just $ Bit 0 0
    _       -> Just $ Bit 0 1
  after b   = fmap coerce $ case coerce b of
    Bit 0 0 -> Just $ Bit 0 1
    Bit 0 _ -> Just $ Bit 1 0
    _       -> Nothing
  ith      = coerce . \case
    0 -> Bit 0 0
    1 -> Bit 0 1
    _ -> Bit 1 0
  index b  = case coerce b of
    Bit 0 0 -> 0
    Bit 0 _ -> 1
    _       -> 2

-- | __NB__: not synthesizable (see 'WithUndefined')
instance KnownNat n => Finite (WithUndefined (BitVector n)) where
  type ElementCount (WithUndefined (BitVector n)) = 3^n

  elements = coerce <$> iterateI bvwuAfter# (BV 0 0)

  lowest = Just $ coerce $ BV 0 0

  highest = Just $ coerce $ BV mb mb
   where
    BV _ mb = maxBound :: BitVector n

  before bv = case coerce bv of
    BV 0 0 -> Nothing
    BV m n -> Just $ coerce $
      if ((m `xor` mb) .&. n) == 0
      then BV (m - 1) ((m - 1) `xor` mb)
      else BV m ((m `xor` mb) .&. (n - 1))
   where
    BV _ mb = maxBound :: BitVector n

  after (coerce -> bv@(BV m _))
    | m < mb    = Just $ coerce $ bvwuAfter# bv
    | otherwise = Nothing
   where
    BV _ mb = maxBound :: BitVector n

  ith i = coerce $ BV
    (toNat $ complement nMask)
    (toNat $ snd $ foldr stretch (remaining, 0) $ unpack nMask)
   where
    nMask, remaining :: BitVector n
    (nMask, remaining)
      | Dict <- powMonotone1 @2 @3 @n
      , Dict <- powGTZero @2 @n
      , Dict <- powGTZero @3 @n
#if !MIN_VERSION_base(4,16,0)
      , Dict <- powCLogDual @2 @n
      , Dict <- leqOnePlusMinus @(2^n) @(3^n)
#endif
      = second (pack . complement . truncateB @Index @(2^n) @(3^n - 2^n))
      $ fst
      $ foldl
         ( \(((`shiftL` 1) -> m, r), x2) x3 ->
             if r < x2 * x3
             then ((m,            r          ),     x2)
             else ((m `setBit` 0, r - x2 * x3), 2 * x2)
         )
         ((0, negate $ satSucc SatWrap i), 1)
      $ reverse
      $ iterateI @n (3 *) 1

    stretch negMBit (bv, (`shiftR` 1) -> v)
      | negMBit   = (shiftR bv 1, ) $ if testBit bv 0 then setMsb v else v
      | otherwise = (bv, v)

    toNat = fromInteger . toInteger
    setMsb = (.|.) (complement $ complement 0 `shiftR` 1)

  index (coerce -> bv@(BV mask _))
    | Dict <- powGTZero @3 @n
    = -- compute the mask induced offset
      negate
        ( snd
        $ foldr (\b (p, a) -> (3 * p, if b then p + 2 * a else a)) (1, 0)
        $ bitCoerce @(Unsigned n) @(Vec n Bool)
        $ negate
        $ U mask
        )
    + -- re-align the value bits according to the mask
      foldl
        ( \a (Bit m n) -> if
            | m /= 0    -> a
            | n == 0    -> shiftL a 1
            | otherwise -> shiftL a 1 `setBit` 0
        ) 0 (bv2v bv)

bvwuAfter# :: forall n. KnownNat n => BitVector n -> BitVector n
bvwuAfter# (BV m n)
  | n < mb    = BV m ((n + 1) .|. m)
  | otherwise = BV (m + 1) (m + 1)
 where
  BV _ mb = maxBound :: BitVector n
{-# INLINE bvwuAfter# #-}

-- | A newtype wrapper for deriving Finite instances from existing
-- instances of 'Bounded', 'Enum', and 'Eq', where 'Eq' is only
-- utilized for efficiency reasons although it is not strictly
-- necessary.
newtype BoundedEnumEq (n :: Nat) a = BoundedEnumEq { getBoundedEnumEq :: a }

-- | see 'BoundedEnumEq'
instance
  ( Bounded a, Enum a, Eq a, KnownNat n, 1 <= n
  ) => Finite (BoundedEnumEq n a)
 where
  type ElementCount (BoundedEnumEq n a) = n
  elements = BoundedEnumEq <$> iterateI succ minBound
  lowest = Just $ BoundedEnumEq minBound
  highest = Just $ BoundedEnumEq maxBound
  before (getBoundedEnumEq -> x)
    | x == minBound = Nothing
    | otherwise     = Just $ BoundedEnumEq $ pred x
  after (getBoundedEnumEq -> x)
    | x == maxBound = Nothing
    | otherwise     = Just $  BoundedEnumEq $ succ x
  ith = BoundedEnumEq . toEnum . fromEnum
  index = toEnum . fromEnum . getBoundedEnumEq

-- | A newtype wrapper for implementing deriving strategies of classes
-- whose implementation may follow from 'Finite', e.g., the 'Enum'
-- class.
newtype FiniteDerive a = FiniteDerive { getFiniteDerive :: a }

instance Finite a => Enum (FiniteDerive a) where
  succ = FiniteDerive . fromJust . after . getFiniteDerive
  pred = FiniteDerive . fromJust . before . getFiniteDerive
  toEnum = FiniteDerive . ith . toEnum . (`mod` natToNum @(ElementCount a))
  fromEnum = fromEnum . index . getFiniteDerive
  enumFrom x =
    take (natToNum @(ElementCount a) - fromEnum (index $ getFiniteDerive x))
      $ List.iterate succ x
  enumFromTo x y =
    take (fromEnum (index (getFiniteDerive y) - index (getFiniteDerive x)))
      $ List.iterate succ x
  enumFromThen = case toUNat (SNat @(ElementCount a)) of
    UZero -> const $ const []
    USucc um -> \x y -> FiniteDerive . ith <$>
      [  index (getFiniteDerive x)
      ,  index (getFiniteDerive y)
      .. snatToNum (fromUNat um)
      ]
  enumFromThenTo = case toUNat (SNat @(ElementCount a)) of
    UZero -> const $ const $ const []
    USucc _ -> \x y z -> FiniteDerive . ith <$>
      [  index (getFiniteDerive x)
      ,  index (getFiniteDerive y)
      .. index (getFiniteDerive z)
      ]

instance (Finite a, Finite b) => Finite (a, b)

-- | __NB__: The documentation only shows instances up to /3/-tuples. By
-- default, instances up to and including /12/-tuples will exist. If the flag
-- @large-tuples@ is set instances up to the GHC imposed limit will exist. The
-- GHC imposed limit is either 62 or 64 depending on the GHC version.
#if MIN_VERSION_base(4,16,0)
deriveFiniteTuples ''Finite
#else
deriveFiniteTuples ''Finite ''ElementCount 'elements 'lowest 'highest 'before
  'after 'ith 'index
#endif
