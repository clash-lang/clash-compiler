{-|
Copyright  :  (C) 2019, QBayLogic
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

#include "MachDeps.h"

module Clash.Class.Parity
  ( Parity (..) )
where

import Prelude hiding                 (even, odd)

import Data.Int
import Data.Word
import Foreign.C.Types                (CUShort)
import GHC.TypeLits                   (KnownNat)

import Clash.Class.BitPack            (pack)
import Clash.Sized.Internal.BitVector (BitVector, high, low, lsb#)
import Clash.Promoted.Nat             (SNat(..), snatToNum)

{- $setup
>>> :m -Prelude
>>> import Clash.Prelude
>>> import Clash.Class.Parity
-}

-- | Determine whether value is odd or even
class Parity a where
  -- | Check if value is even
  --
  -- >>> even (4 :: Unsigned 4)
  -- True
  even :: a -> Bool
  even = not . odd

  -- | Check if value is odd
  --
  -- >>> odd (4 :: Unsigned 4)
  -- False
  odd :: a -> Bool
  odd = not . even
  {-# MINIMAL even | odd #-}

instance Parity Integer
  where
    even a = a `mod` 2 == 0
    odd a = a `mod` 2 == 1

instance KnownNat n => Parity (BitVector n) where
  even a =
    case snatToNum @Integer (SNat @n) of
      0 -> True
      _ -> (==low) $ lsb# a
  odd a =
    case snatToNum @Integer (SNat @n) of
      0 -> False
      _ -> (==high) $ lsb# a

instance Parity Bool where
  even = even . pack
  odd = odd . pack

instance Parity CUShort where
  even = even . pack
  odd = odd . pack

instance Parity Word where
  even = even . pack
  odd = odd . pack

instance Parity Word8 where
  even = even . pack
  odd = odd . pack

instance Parity Word16 where
  even = even . pack
  odd = odd . pack

instance Parity Word32 where
  even = even . pack
  odd = odd . pack

instance Parity Word64 where
  even = even . pack
  odd = odd . pack

instance Parity Int where
  even = even . pack
  odd = odd . pack

instance Parity Int8 where
  even = even . pack
  odd = odd . pack

instance Parity Int16 where
  even = even . pack
  odd = odd . pack

instance Parity Int32 where
  even = even . pack
  odd = odd . pack

instance Parity Int64 where
  even = even . pack
  odd = odd . pack
