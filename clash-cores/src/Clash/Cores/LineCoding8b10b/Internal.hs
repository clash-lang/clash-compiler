module Clash.Cores.LineCoding8b10b.Internal where

import Clash.Prelude

data SubBlockDisparity
  = SubBlockPositive
  | SubBlockNeutral
  | SubBlockNegative
  deriving (Eq, Show)

-- | Data type that contains a 'BitVector' with the corresponding error
--   condition of the decode function
data Symbol8b10b
  = -- | Correct data word
    Dw (BitVector 8)
  | -- | Correct control word
    Cw (BitVector 8)
  | -- | Incorrect data word
    DwError (BitVector 8)
  | -- | Running disparity error
    RdError (BitVector 8)
  deriving (Generic, NFDataX, Eq, Show)

-- | Function to convert a 'Symbol8b10b' to a plain 'BitVector'
fromSymbol :: Symbol8b10b -> BitVector 8
fromSymbol sym = case sym of
  Dw w -> w
  Cw w -> w
  DwError w -> w
  RdError w -> w

-- | Function to check whether a 'Symbol8b10b' results in a control word
isCw :: Symbol8b10b -> Bool
isCw (Cw _) = True
isCw _ = False

-- | Compute the disparity for a 6b sub-block
subBlockDisparity6b ::
  BitVector 6 ->
  (Bool, SubBlockDisparity)
subBlockDisparity6b inp = (err, disp)
 where
  popCount0 = popCount6 inp
  disp
    | popCount0  < 3 = SubBlockNegative
    | popCount0 == 3 = SubBlockNeutral
    | otherwise      = SubBlockPositive
  err = popCount0 < 2 || popCount0 > 4

-- | Compute the disparity for a 4b sub-block
subBlockDisparity4b ::
  BitVector 4 ->
  (Bool, SubBlockDisparity)
subBlockDisparity4b inp = (err, disp)
 where
  popCount0 = popCount4 inp
  disp
    | popCount0  < 2 = SubBlockNegative
    | popCount0 == 2 = SubBlockNeutral
    | otherwise      = SubBlockPositive
  err = popCount0 < 1 || popCount0 > 3

-- | Specialized population count optimized for operand size
popCount6 :: BitVector 6 -> Unsigned 3
popCount6 inp = popCount1
 where
  popCount0 :: Vec 3 (Unsigned 2)
  popCount0 =
    map (uncurry add) $ bitCoerce @_ @(Vec 3 (Unsigned 1, Unsigned 1)) inp
  popCount1
    | (e0 :> e1 :> e2 :> Nil) <- popCount0
    = extend e0 + add e1 e2
    | otherwise
    = error "Absurd"

-- | Specialized population count optimized for operand size
popCount4 :: BitVector 4 -> Unsigned 3
popCount4 inp = popCount1
 where
  popCount0 :: Vec 2 (Unsigned 2)
  popCount0 =
    map (uncurry add) $ bitCoerce @_ @(Vec 2 (Unsigned 1, Unsigned 1)) inp
  popCount1
    | (e0 :> e1 :> Nil) <- popCount0
    = e0 `add` e1
    | otherwise
    = error "Absurd"
