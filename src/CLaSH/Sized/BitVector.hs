{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}
module CLaSH.Sized.BitVector
  ( -- * Datatypes
    BitVector
  , Bit
    -- * Accessors
    -- ** Length information
  , blength
  , bmaxIndex
    -- ** Indexing
  , (!#)
  , bhead
  , blast
    -- * Construction
    -- ** Initialisation
  , high
  , low
    -- ** Concatenation
  , (++#)
    -- * Modifying BitVectors
  , bupdate
  )
where

import Data.Vector.Unboxed (Vector, singleton, (!), update_, (++), unsafeHead,
                            unsafeLast)
import GHC.TypeLits        (KnownNat, Nat, type (+), natVal)
import Prelude             hiding ((++))

newtype BitVector (n :: Nat) = BV (Vector Bool)
type Bit = BitVector 1

-- * Accessors
-- ** Length information
{-# NOINLINE blength #-}
blength :: KnownNat n => BitVector n -> Integer
blength bv = natVal bv

{-# NOINLINE bmaxIndex #-}
bmaxIndex :: KnownNat n => BitVector n -> Integer
bmaxIndex bv = natVal bv - 1

-- ** Indexing
{-# INLINABLE (!#) #-}
-- | Indexing
--
-- __NB:__ Uses a /descending/ index, starting at 'bmaxIndex'
--
-- >>> (1 #> 1 #> 0 #> 0) !# 3
-- 1
-- >>> (1 #> 1 #> 0 #> 0) !# bmaxIndex
-- 1
-- >>> (1 #> 1 #> 0 #> 0) !# 1
-- 0
(!#) :: (Integral i, KnownNat n) => BitVector n -> i -> Bit
bv !# i = bindexInt bv (fromIntegral i)

{-# NOINLINE bindexInt #-}
bindexInt :: KnownNat n => BitVector n -> Int -> Bit
bindexInt bv@(BV v) i = BV (singleton (v ! i'))
  where
    i' = fromInteger (natVal bv - 1) - i

-- | MSB
bhead :: BitVector (n + 1) -> Bit
bhead (BV v) = BV (singleton (unsafeHead v))

-- | LSB
blast :: BitVector (n + 1) -> Bit
blast (BV v) = BV (singleton (unsafeLast v))

-- * Constructions
-- ** Initialisation
{-# NOINLINE high #-}
-- | logic '1'
high :: Bit
high = BV (singleton True)

{-# NOINLINE low #-}
-- | logic '0'
low :: Bit
low = BV (singleton False)

-- ** Concatenation
{-# INLINABLE (#>) #-}
infixr 5 #>
-- | Prepend a 'Bit'
(#>) :: Bit -> BitVector n -> BitVector (1 + n)
(#>) = (++#)

{-# INLINABLE (<#) #-}
infixl 5 <#
-- | Append a 'Bit'
(<#) :: BitVector n -> Bit -> BitVector (n + 1)
(<#) = (++#)

{-# NOINLINE (++#) #-}
-- | Concatenate two 'BitVector's
(++#) :: BitVector n -> BitVector m -> BitVector (n + m)
(BV v1) ++# (BV v2) = BV (v1 ++ v2)

-- * Modifying BitVectors
{-# INLINABLE bupdate #-}
-- | Replace the element in @bv@, at the index @i@, with the value @b@.
--
-- __NB:__ Uses a /descending/ index, starting at 'bmaxIndex'
--
-- >>> bupdate (1 #> 1 #> 0 #> 0) 3 0
-- "0100"
-- >>> bupdate (1 #> 1 #> 0 #> 0) bmaxIndex 0
-- "0100"
-- >>> bupdate (1 #> 1 #> 0 #> 0) 1 1
-- "1110"
bupdate :: (Integral i, KnownNat n)
        => BitVector n -- Initial vector @bv@
        -> i           -- Index @i@
        -> Bit         -- Bit value @b@
        -> BitVector n
bupdate bv i b = bupdateInt bv (fromIntegral i) b

{-# NOINLINE bupdateInt #-}
bupdateInt :: KnownNat n => BitVector n -> Int -> Bit -> BitVector n
bupdateInt bv@(BV v) i (BV b) = BV (update_ v (singleton i') b)
  where
    i' = fromInteger (natVal bv - 1) - i
