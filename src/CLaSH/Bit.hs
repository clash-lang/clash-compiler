{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module CLaSH.Bit
  (Bit(..))
where

import Data.Bits
import Data.Default
import Language.Haskell.TH.Lift

-- | Two-level logic
data Bit = H | L

instance Eq Bit where
  (==) = eqBit

{-# NOINLINE eqBit #-}
eqBit :: Bit -> Bit -> Bool
eqBit L L = True
eqBit H H = True
eqBit _ _ = False

instance Show Bit where
  show H = "1"
  show L = "0"

instance Default Bit where
  def = L

deriveLift ''Bit

{-# NOINLINE bAnd #-}
bAnd :: Bit -> Bit -> Bit
bAnd H H = H
bAnd _ _ = L

{-# NOINLINE bOr #-}
bOr :: Bit -> Bit -> Bit
bOr L L  = L
bOr _ _  = H

{-# NOINLINE bXor #-}
bXor :: Bit -> Bit -> Bit
bXor L L = L
bXor H H = L
bXor _ _ = H

{-# NOINLINE bNot #-}
bNot :: Bit -> Bit
bNot L = H
bNot H = L

instance Bits Bit where
  (.&.)        = bAnd
  (.|.)        = bOr
  xor          = bXor
  complement   = bNot
  bit          = const H
  testBit H _  = True
  testBit _ _  = False
  bitSizeMaybe = const (Just 1)
  isSigned     = const False
  popCount H   = 1
  popCount _   = 0
