{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- Used to improve the performance of derived instances.
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v0 #-}

module Clash.FFI.VPI.Object.Value.Vector
  ( CVector(..)
  , bitVectorToVector
  , vectorToBitVector
  ) where

import           Data.Bits (clearBit, setBit, testBit)
import           Data.Proxy
import           Foreign.C.Types (CInt)
import qualified Foreign.Marshal.Array as FFI (peekArray)
import           Foreign.Storable.Generic (GStorable)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           GHC.TypeNats

import           Clash.Class.BitPack (replaceBit, unpack)
import           Clash.Sized.Internal.BitVector
import           Clash.Sized.Internal.Index (Index)
import           Clash.Sized.Vector (Vec(..))
import qualified Clash.Sized.Vector as Vec
import           Clash.XException (deepErrorX)

import           Clash.FFI.View
import           Clash.FFI.VPI.Object.Value.Scalar

-- | A bit vector encoded using the a/b encoding as specified in the Verilog
-- 2005 standard. This encodes bit vectors as an array of pairs of words, where
-- the value of each bit is given by the value of the A and B bits:
--
-- +---+---+-----------+
-- | A | B | Bit Value |
-- +===+===+===========+
-- | 0 | 0 | 0         |
-- +---+---+-----------+
-- | 1 | 0 | 1         |
-- +---+---+-----------+
-- | 0 | 1 | Z         |
-- +---+---+-----------+
-- | 1 | 1 | X         |
-- +---+---+-----------+
--
data CVector = CVector
  { cvectorA :: CInt
  , cvectorB :: CInt
  }
  deriving stock (Generic, Show)
  deriving anyclass (GStorable)

vectorToCVectorList
  :: forall n
   . HasCallStack
  => KnownNat n
  => Vec n Scalar
  -> [CVector]
vectorToCVectorList vec = go [] 0
 where
  size :: Int
  size = fromIntegral $ natVal (Proxy @n)

  replaceScalar :: Int -> Scalar -> CVector -> CVector
  replaceScalar ix s CVector{..} =
    let
      (aMod, bMod) =
        case s of
          S0 -> (clearBit, clearBit)
          SL -> (clearBit, clearBit)
          S1 -> (  setBit, clearBit)
          SH -> (  setBit, clearBit)
          SZ -> (clearBit,   setBit)
          SX -> (  setBit,   setBit)
          S_ -> (  setBit,   setBit)
    in
      CVector (aMod cvectorA ix) (bMod cvectorB ix)

  go :: [CVector] -> Int -> [CVector]
  go a i =
    let
      new = CVector (-1) (-1)
      upd = replaceScalar (i `mod` 32) (vec Vec.!! (size - i - 1))
    in if
      | i >= size       -> reverse a
      | i `mod` 32 == 0 -> go (upd new : a ) $ i + 1
      | x:xr <- a       -> go (upd   x : xr) $ i + 1
      | otherwise       -> error "vectorToCVectorList"

type instance CRepr (Vec _ Scalar) = CRepr [CVector]

instance (KnownNat n) => UnsafeSend (Vec n Scalar) where
  unsafeSend = unsafeSend . vectorToCVectorList

instance (KnownNat n) => Send (Vec n Scalar) where
  send = send . vectorToCVectorList

cvectorListToVector
  :: forall n
   . HasCallStack
  => KnownNat n
  => [CVector]
  -> Vec n Scalar
cvectorListToVector = go (Vec.repeat SX) 0
 where
  size :: Int
  size = fromIntegral $ natVal (Proxy @n)

  go :: Vec n Scalar -> Int -> [CVector] -> Vec n Scalar
  go acc ix arr
    | ix >= size && length arr <= 1 = acc
    | x:xr <- arr =
        go (Vec.replace ix (getScalar (ix `mod` 32) x) acc) (ix + 1)
          $ if (ix + 1) `mod` 32 == 0 then xr else x:xr
    | otherwise = error "cvectorListToVector: Array is not the specified size"

  getScalar :: Int -> CVector -> Scalar
  getScalar ix CVector{..} =
    case (testBit cvectorA ix, testBit cvectorB ix) of
      (False, False) -> S0
      (True,  False) -> S1
      (False, True)  -> SZ
      (True,  True)  -> SX

instance (KnownNat n) => UnsafeReceive (Vec n Scalar) where
  unsafeReceive =
    let size = fromIntegral $ natVal $ Proxy @n
        len  = div (size - 1) 32 + 1
     in fmap cvectorListToVector . FFI.peekArray len

instance (KnownNat n) => Receive (Vec n Scalar) where
  receive =
    let size = fromIntegral $ natVal $ Proxy @n
        len  = div (size - 1) 32 + 1
     in fmap cvectorListToVector . FFI.peekArray len


-- | Turns a 'BitVector' into a vector of 'Scalar' values.
bitVectorToVector
  :: forall n
   . KnownNat n
  => BitVector n
  -> Vec n Scalar
bitVectorToVector =
  fmap bitToScalar . unpack

-- | Turns a vector of 'Scalar' values into a 'BitVector'.
vectorToBitVector
  :: forall n
   . KnownNat n
  => Vec n Scalar
  -> BitVector n
vectorToBitVector vec =
  Vec.ifoldr go (deepErrorX "vectorToBitVector") vec
 where
  go :: Index n -> Scalar -> BitVector n -> BitVector n
  go ix s = replaceBit ix $ scalarToBit s

type instance CRepr (BitVector n) = CRepr (Vec n Scalar)

instance (KnownNat n) => UnsafeSend (BitVector n) where
  unsafeSend = unsafeSend . bitVectorToVector

instance (KnownNat n) => Send (BitVector n) where
  send = send . bitVectorToVector

instance (KnownNat n) => UnsafeReceive (BitVector n) where
  unsafeReceive = fmap vectorToBitVector . unsafeReceive

instance (KnownNat n) => Receive (BitVector n) where
  receive = fmap vectorToBitVector . receive
#else
module Clash.FFI.VPI.Object.Value.Vector () where
#endif
