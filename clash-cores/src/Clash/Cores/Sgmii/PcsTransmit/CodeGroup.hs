{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Sgmii.PcsTransmit.CodeGroup where

import Clash.Cores.LineCoding8b10b
import Clash.Cores.Sgmii.Common
import Clash.Prelude
import Data.Maybe (fromMaybe)

-- | State type of 'codeGroupT' as defined in IEEE 802.3 Clause 36, with the
--   exception of @GENERATE_CODE_GROUPS@ and @IDLE_DISPARITY_TEST@ as these
--   states does not act upon the 125 MHz @cg_timer@ timer
data CodeGroupState
  = SpecialGo
      { _rd :: Bool
      , _txConfReg :: ConfReg
      , _txEven :: Even
      , _txOSet :: OrderedSet
      }
  | DataGo {_rd :: Bool, _txConfReg :: ConfReg, _txEven :: Even}
  | IdleDisparityWrong {_rd :: Bool, _txConfReg :: ConfReg}
  | IdleI1B {_rd :: Bool, _txConfReg :: ConfReg}
  | IdleDisparityOk {_rd :: Bool, _txConfReg :: ConfReg}
  | IdleI2B {_rd :: Bool, _txConfReg :: ConfReg}
  | ConfigurationC1A {_rd :: Bool, _txConfReg :: ConfReg}
  | ConfigurationC1B {_rd :: Bool, _txConfReg :: ConfReg}
  | ConfigurationC1C {_rd :: Bool, _txConfReg :: ConfReg}
  | ConfigurationC1D {_rd :: Bool, _txConfReg :: ConfReg}
  | ConfigurationC2A {_rd :: Bool, _txConfReg :: ConfReg}
  | ConfigurationC2B {_rd :: Bool, _txConfReg :: ConfReg}
  | ConfigurationC2C {_rd :: Bool, _txConfReg :: ConfReg}
  | ConfigurationC2D {_rd :: Bool, _txConfReg :: ConfReg}
  deriving (Generic, NFDataX, Eq, Show)

-- | State transition function for the states as defined in IEEE 802.3 Clause
--   36, specifically Figure 36-6. This function receives an ordered set from
--   the ordered set state machine, a @TXD@ value from the outside world and
--   then encodes it.
codeGroupT ::
  -- | State variable
  CodeGroupState ->
  -- | Input 'DataWord' from the ordered set, new input value and the config
  --   register
  (OrderedSet, BitVector 8, Maybe ConfReg) ->
  -- | The new state and the new output values
  (CodeGroupState, (CodeGroupState, BitVector 10, Even, Bool))
codeGroupT self@SpecialGo{..} (txOSet, _, txConfReg) = (nextState, out)
 where
  nextState
    | txOSet == OSetD = DataGo rd txConfReg' txEven
    | txOSet == OSetI && rd = IdleDisparityWrong rd txConfReg'
    | txOSet == OSetI && not rd = IdleDisparityOk rd txConfReg'
    | txOSet == OSetC = ConfigurationC1A rd txConfReg'
    | otherwise = SpecialGo rd txConfReg' txEven txOSet

  dw
    | _txOSet == OSetS = cwS
    | _txOSet == OSetT = cwT
    | _txOSet == OSetR = cwR
    | _txOSet == OSetV = cwV
    | otherwise = Cw 0

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd dw
  txEven = nextEven _txEven

  out = (self, cg, txEven, True)
codeGroupT self@DataGo{..} (txOSet, dw, txConfReg) = (nextState, out)
 where
  nextState
    | txOSet == OSetD = DataGo rd txConfReg' txEven
    | txOSet == OSetI && rd = IdleDisparityWrong rd txConfReg'
    | txOSet == OSetI && not rd = IdleDisparityOk rd txConfReg'
    | txOSet == OSetC = ConfigurationC1A rd txConfReg'
    | otherwise = SpecialGo rd txConfReg' txEven txOSet

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd (Dw dw)
  txEven = nextEven _txEven

  out = (self, cg, txEven, True)
codeGroupT self@IdleDisparityWrong{..} (_, _, txConfReg) = (nextState, out)
 where
  nextState = IdleI1B rd txConfReg'

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd cwK28_5
  txEven = Even

  out = (self, cg, txEven, False)
codeGroupT self@IdleI1B{..} (txOSet, _, txConfReg) = (nextState, out)
 where
  nextState
    | txOSet == OSetD = DataGo rd txConfReg' txEven
    | txOSet == OSetI && rd = IdleDisparityWrong rd txConfReg'
    | txOSet == OSetI && not rd = IdleDisparityOk rd txConfReg'
    | txOSet == OSetC = ConfigurationC1A rd txConfReg'
    | otherwise = SpecialGo rd txConfReg' txEven txOSet

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd dwD05_6
  txEven = Odd

  out = (self, cg, txEven, True)
codeGroupT self@IdleDisparityOk{..} (_, _, txConfReg) = (nextState, out)
 where
  nextState = IdleI2B rd txConfReg'

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd cwK28_5
  txEven = Even

  out = (self, cg, txEven, False)
codeGroupT self@IdleI2B{..} (txOSet, _, txConfReg) = (nextState, out)
 where
  nextState
    | txOSet == OSetD = DataGo rd txConfReg' txEven
    | txOSet == OSetI && rd = IdleDisparityWrong rd txConfReg'
    | txOSet == OSetI && not rd = IdleDisparityOk rd txConfReg'
    | txOSet == OSetC = ConfigurationC1A rd txConfReg'
    | otherwise = SpecialGo rd txConfReg' txEven txOSet

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd dwD16_2
  txEven = Odd

  out = (self, cg, txEven, True)
codeGroupT self@ConfigurationC1A{..} (_, _, txConfReg) = (nextState, out)
 where
  nextState = ConfigurationC1B rd txConfReg'

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd cwK28_5
  txEven = Even

  out = (self, cg, txEven, False)
codeGroupT self@ConfigurationC1B{..} (_, _, txConfReg) = (nextState, out)
 where
  nextState = ConfigurationC1C rd txConfReg'

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd dwD21_5
  txEven = Odd

  out = (self, cg, txEven, False)
codeGroupT self@ConfigurationC1C{..} (_, _, txConfReg) = (nextState, out)
 where
  nextState = ConfigurationC1D rd txConfReg'

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd (Dw (resize txConfReg'))
  txEven = Even

  out = (self, cg, txEven, False)
codeGroupT self@ConfigurationC1D{..} (txOSet, _, txConfReg) = (nextState, out)
 where
  nextState
    | txOSet == OSetD = DataGo rd txConfReg' txEven
    | txOSet == OSetI && rd = IdleDisparityWrong rd txConfReg'
    | txOSet == OSetI && not rd = IdleDisparityOk rd txConfReg'
    | txOSet == OSetC = ConfigurationC2A rd txConfReg'
    | otherwise = SpecialGo rd txConfReg' txEven txOSet

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd (Dw (resize $ rotateR _txConfReg 8))
  txEven = Odd

  out = (self, cg, txEven, True)
codeGroupT self@ConfigurationC2A{..} (_, _, txConfReg) = (nextState, out)
 where
  nextState = ConfigurationC2B rd txConfReg'

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd cwK28_5
  txEven = Even

  out = (self, cg, txEven, False)
codeGroupT self@ConfigurationC2B{..} (_, _, txConfReg) = (nextState, out)
 where
  nextState = ConfigurationC2C rd txConfReg'

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd dwD02_2
  txEven = Odd

  out = (self, cg, txEven, False)
codeGroupT self@ConfigurationC2C{..} (_, _, txConfReg) = (nextState, out)
 where
  nextState = ConfigurationC2D rd txConfReg'

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd (Dw (resize txConfReg'))
  txEven = Even

  out = (self, cg, txEven, False)
codeGroupT self@ConfigurationC2D{..} (txOSet, _, txConfReg) =
  (nextState, out)
 where
  nextState
    | txOSet == OSetD = DataGo rd txConfReg' txEven
    | txOSet == OSetI && rd = IdleDisparityWrong rd txConfReg'
    | txOSet == OSetI && not rd = IdleDisparityOk rd txConfReg'
    | txOSet == OSetC = ConfigurationC1A rd txConfReg'
    | otherwise = SpecialGo rd txConfReg' txEven txOSet

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd (Dw (resize $ rotateR _txConfReg 8))
  txEven = Odd

  out = (self, cg, txEven, True)

{-# CLASH_OPAQUE codeGroupT #-}
