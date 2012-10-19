{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module CLaSH.Bit
  (Bit(..))
where

import Data.Bits

import CLaSH.Class.Default

data Bit = H | L
  deriving (Eq)

instance Show Bit where
  show H = "1"
  show L = "0"

instance Default Bit where
  def = L

bAnd :: Bit -> Bit -> Bit
bAnd H H = H
bAnd _ _ = L

bOr :: Bit -> Bit -> Bit
bOr L L  = L
bOr _ _  = H

bXor :: Bit -> Bit -> Bit
bXor L L = L
bXor H H = L
bXor _ _ = H

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
