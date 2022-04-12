{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
  ) where

import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.Bits (clearBit, setBit, testBit)
import qualified Data.List as List (replicate)
import           Data.Proxy
import           Foreign.C.Types (CInt)
import qualified Foreign.Marshal.Array as FFI (peekArray)
import           Foreign.Ptr (Ptr)
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
vectorToCVectorList vec =
  let
      size = fromIntegral (natVal (Proxy @n))
      len  = div (size - 1) 32 + 1
   in
      -- Default to all bits being undefined.
      go (List.replicate len (CVector (-1) (-1))) (size - 1) vec
 where
  go :: forall m. [CVector] -> Int -> Vec m Scalar -> [CVector]
  go acc n = \case
    Nil ->
      acc

    Cons x xs ->
      go (replaceScalar n x acc) (n - 1) xs

  replaceScalar :: HasCallStack => Int -> Scalar -> [CVector] -> [CVector]
  replaceScalar ix s [CVector as bs]
    | ix < 32
    = let (f, g) = case s of
                     S0 -> (clearBit, clearBit)
                     S1 -> (setBit,   clearBit)
                     SZ -> (clearBit, setBit)
                     SX -> (setBit,   setBit)
                     SH -> (setBit,   clearBit)
                     SL -> (clearBit, clearBit)
                     S_ -> (setBit,   setBit)
       in [CVector (f as ix) (g bs ix)]

    | otherwise
    = error "replaceScalar: Index out of range"

  replaceScalar ix s (x:xs) =
    x : replaceScalar (ix - 32) s xs

  replaceScalar _ _ _ =
    error "replaceScalar: Index and list not consistent"

instance (KnownNat n) => UnsafeSend (Vec n Scalar) where
  type Sent (Vec n Scalar) = Sent [CVector]

  unsafeSend =
    unsafeSend . vectorToCVectorList

instance (KnownNat n) => Send (Vec n Scalar) where
  send =
    send . vectorToCVectorList

cvectorListToVector
  :: forall n
   . HasCallStack
  => KnownNat n
  => [CVector]
  -> Vec n Scalar
cvectorListToVector =
  let size = fromIntegral (natVal (Proxy @n))
   in go (Vec.repeat SX) size 0
 where
  go :: Vec n Scalar -> Int -> Int -> [CVector] -> Vec n Scalar
  go acc size ix arr
    | size == ix
    = acc

    | ix < 32
    , [x] <- arr
    = go (Vec.replace ix (getScalar ix x) acc) size (ix + 1) arr

    | (_:xs) <- arr
    = go acc (size - 32) 0 xs

    | otherwise
    = error "cvectorListToVector: Array is not the specified size"

  getScalar :: Int -> CVector -> Scalar
  getScalar ix (CVector as bs) =
    case (testBit as ix, testBit bs ix) of
      (False, False) -> S0
      (True,  False) -> S1
      (False, True)  -> SZ
      (True,  True)  -> SX

instance (KnownNat n) => UnsafeReceive (Vec n Scalar) where
  type Received (Vec n Scalar) = Ptr CVector

  unsafeReceive =
    let size = fromIntegral (natVal (Proxy @n))
        len  = div (size - 1) 32 + 1
     in fmap cvectorListToVector . IO.liftIO . FFI.peekArray len

instance (KnownNat n) => Receive (Vec n Scalar) where
  receive =
    let size = fromIntegral (natVal (Proxy @n))
        len  = div (size - 1) 32 + 1
     in fmap cvectorListToVector . IO.liftIO . FFI.peekArray len

bitVectorToVector
  :: forall n
   . KnownNat n
  => BitVector n
  -> Vec n Scalar
bitVectorToVector =
  fmap bitToScalar . unpack

vectorToBitVector
  :: forall n
   . KnownNat n
  => Vec n Scalar
  -> BitVector n
vectorToBitVector vec =
  Vec.ifoldr go (deepErrorX "vectorToBitVector") vec
 where
  go :: Index n -> Scalar -> BitVector n -> BitVector n
  go ix s = replaceBit ix (scalarToBit s)

instance (KnownNat n) => UnsafeSend (BitVector n) where
  type Sent (BitVector n) = Sent (Vec n Scalar)

  unsafeSend =
    unsafeSend . bitVectorToVector

instance (KnownNat n) => Send (BitVector n) where
  send =
    send . bitVectorToVector

instance (KnownNat n) => UnsafeReceive (BitVector n) where
  type Received (BitVector n) = Received (Vec n Scalar)

  unsafeReceive =
    fmap vectorToBitVector . unsafeReceive

instance (KnownNat n) => Receive (BitVector n) where
  receive =
    fmap vectorToBitVector . receive
#else
module Clash.FFI.VPI.Object.Value.Vector () where
#endif

