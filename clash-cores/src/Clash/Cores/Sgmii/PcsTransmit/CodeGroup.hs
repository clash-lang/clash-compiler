{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Sgmii.PcsTransmit.CodeGroup where

import Clash.Cores.LineCoding8b10b
import Clash.Cores.Sgmii.Common
import Clash.Prelude
import Data.Maybe (fromMaybe)

-- | State type of the code group process as defined in IEEE 802.3 Clause 36,
--   with the exception of @GENERATE_CODE_GROUPS@ and @IDLE_DISPARITY_TEST@ as
--   these states does not act upon the 125 MHz @cg_timer@ timer
data CodeGroupState
  = SpecialGo
      { _rd :: Bool
      , _cg :: BitVector 10
      , _txConfReg :: ConfReg
      , _txEven :: Even
      , _txOSet :: OrderedSet
      }
  | DataGo
      { _rd :: Bool
      , _cg :: BitVector 10
      , _txConfReg :: ConfReg
      , _txEven :: Even
      }
  | IdleDisparityWrong {_rd :: Bool, _cg :: BitVector 10, _txConfReg :: ConfReg}
  | IdleI1B {_rd :: Bool, _cg :: BitVector 10, _txConfReg :: ConfReg}
  | IdleDisparityOk {_rd :: Bool, _cg :: BitVector 10, _txConfReg :: ConfReg}
  | IdleI2B {_rd :: Bool, _cg :: BitVector 10, _txConfReg :: ConfReg}
  | ConfigurationC1A {_rd :: Bool, _cg :: BitVector 10, _txConfReg :: ConfReg}
  | ConfigurationC1B {_rd :: Bool, _cg :: BitVector 10, _txConfReg :: ConfReg}
  | ConfigurationC1C {_rd :: Bool, _cg :: BitVector 10, _txConfReg :: ConfReg}
  | ConfigurationC1D {_rd :: Bool, _cg :: BitVector 10, _txConfReg :: ConfReg}
  | ConfigurationC2A {_rd :: Bool, _cg :: BitVector 10, _txConfReg :: ConfReg}
  | ConfigurationC2B {_rd :: Bool, _cg :: BitVector 10, _txConfReg :: ConfReg}
  | ConfigurationC2C {_rd :: Bool, _cg :: BitVector 10, _txConfReg :: ConfReg}
  | ConfigurationC2D {_rd :: Bool, _cg :: BitVector 10, _txConfReg :: ConfReg}
  deriving (Generic, NFDataX, Eq, Show)

-- | State transition function for the states as defined in IEEE 802.3 Clause
--   36, specifically Figure 36-6. This function receives an ordered set from
--   the ordered set state machine, a @TXD@ value from the outside world and
--   then encodes it.
codeGroupT ::
  -- | State variable
  CodeGroupState ->
  -- | Input data word from the ordered set, new input value and the config
  --   register
  (OrderedSet, BitVector 8, Maybe ConfReg) ->
  -- | The new state
  CodeGroupState
codeGroupT SpecialGo{..} (txOSet, _, txConfReg) = nextState
 where
  nextState
    | txOSet == OSetD = DataGo rd cg txConfReg' txEven
    | txOSet == OSetI && rd = IdleDisparityWrong rd cg txConfReg'
    | txOSet == OSetI && not rd = IdleDisparityOk rd cg txConfReg'
    | txOSet == OSetC = ConfigurationC1A rd cg txConfReg'
    | otherwise = SpecialGo rd cg txConfReg' txEven txOSet

  dw
    | _txOSet == OSetS = cwS
    | _txOSet == OSetT = cwT
    | _txOSet == OSetR = cwR
    | _txOSet == OSetV = cwV
    | otherwise = Cw 0

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd dw
  txEven = nextEven _txEven
codeGroupT DataGo{..} (txOSet, dw, txConfReg) = nextState
 where
  nextState
    | txOSet == OSetD = DataGo rd cg txConfReg' txEven
    | txOSet == OSetI && rd = IdleDisparityWrong rd cg txConfReg'
    | txOSet == OSetI && not rd = IdleDisparityOk rd cg txConfReg'
    | txOSet == OSetC = ConfigurationC1A rd cg txConfReg'
    | otherwise = SpecialGo rd cg txConfReg' txEven txOSet

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd (Dw dw)
  txEven = nextEven _txEven
codeGroupT IdleDisparityWrong{..} (_, _, txConfReg) = nextState
 where
  nextState = IdleI1B rd cg txConfReg'

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd cwK28_5
codeGroupT IdleI1B{..} (txOSet, _, txConfReg) = nextState
 where
  nextState
    | txOSet == OSetD = DataGo rd cg txConfReg' txEven
    | txOSet == OSetI && rd = IdleDisparityWrong rd cg txConfReg'
    | txOSet == OSetI && not rd = IdleDisparityOk rd cg txConfReg'
    | txOSet == OSetC = ConfigurationC1A rd cg txConfReg'
    | otherwise = SpecialGo rd cg txConfReg' txEven txOSet

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd dwD05_6
  txEven = Odd
codeGroupT IdleDisparityOk{..} (_, _, txConfReg) = nextState
 where
  nextState = IdleI2B rd cg txConfReg'

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd cwK28_5
codeGroupT IdleI2B{..} (txOSet, _, txConfReg) = nextState
 where
  nextState
    | txOSet == OSetD = DataGo rd cg txConfReg' txEven
    | txOSet == OSetI && rd = IdleDisparityWrong rd cg txConfReg'
    | txOSet == OSetI && not rd = IdleDisparityOk rd cg txConfReg'
    | txOSet == OSetC = ConfigurationC1A rd cg txConfReg'
    | otherwise = SpecialGo rd cg txConfReg' txEven txOSet

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd dwD16_2
  txEven = Odd
codeGroupT ConfigurationC1A{..} (_, _, txConfReg) = nextState
 where
  nextState = ConfigurationC1B rd cg txConfReg'

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd cwK28_5
codeGroupT ConfigurationC1B{..} (_, _, txConfReg) = nextState
 where
  nextState = ConfigurationC1C rd cg txConfReg'

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd dwD21_5
codeGroupT ConfigurationC1C{..} (_, _, txConfReg) = nextState
 where
  nextState = ConfigurationC1D rd cg txConfReg'

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd (Dw (resize txConfReg'))
codeGroupT ConfigurationC1D{..} (txOSet, _, txConfReg) = nextState
 where
  nextState
    | txOSet == OSetD = DataGo rd cg txConfReg' txEven
    | txOSet == OSetI && rd = IdleDisparityWrong rd cg txConfReg'
    | txOSet == OSetI && not rd = IdleDisparityOk rd cg txConfReg'
    | txOSet == OSetC = ConfigurationC2A rd cg txConfReg'
    | otherwise = SpecialGo rd cg txConfReg' txEven txOSet

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd (Dw (resize $ rotateR _txConfReg 8))
  txEven = Odd
codeGroupT ConfigurationC2A{..} (_, _, txConfReg) = nextState
 where
  nextState = ConfigurationC2B rd cg txConfReg'

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd cwK28_5
codeGroupT ConfigurationC2B{..} (_, _, txConfReg) = nextState
 where
  nextState = ConfigurationC2C rd cg txConfReg'

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd dwD02_2
codeGroupT ConfigurationC2C{..} (_, _, txConfReg) = nextState
 where
  nextState = ConfigurationC2D rd cg txConfReg'

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd (Dw (resize txConfReg'))
codeGroupT ConfigurationC2D{..} (txOSet, _, txConfReg) = nextState
 where
  nextState
    | txOSet == OSetD = DataGo rd cg txConfReg' txEven
    | txOSet == OSetI && rd = IdleDisparityWrong rd cg txConfReg'
    | txOSet == OSetI && not rd = IdleDisparityOk rd cg txConfReg'
    | txOSet == OSetC = ConfigurationC1A rd cg txConfReg'
    | otherwise = SpecialGo rd cg txConfReg' txEven txOSet

  txConfReg' = fromMaybe _txConfReg txConfReg
  (rd, cg) = encode8b10b _rd (Dw (resize $ rotateR _txConfReg 8))
  txEven = Odd

{-# CLASH_OPAQUE codeGroupT #-}

-- | Output transition function for the states as defined in IEEE 802.3 Clause
--   36, specifically Figure 36-6. This function receives an ordered set from
--   the ordered set state machine, a @TXD@ value from the outside world and
--   then encodes it.
codeGroupO ::
  -- | Current state
  CodeGroupState ->
  -- | New output values
  (CodeGroupState, BitVector 10, Even, Bool)
codeGroupO self@SpecialGo{..} = (self, _cg, txEven, True)
 where
  txEven = nextEven _txEven
codeGroupO self@DataGo{..} = (self, _cg, txEven, True)
 where
  txEven = nextEven _txEven
codeGroupO self@IdleI1B{..} = (self, _cg, Odd, True)
codeGroupO self@IdleI2B{..} = (self, _cg, Odd, True)
codeGroupO self@ConfigurationC1B{..} = (self, _cg, Odd, False)
codeGroupO self@ConfigurationC1D{..} = (self, _cg, Odd, True)
codeGroupO self@ConfigurationC2B{..} = (self, _cg, Odd, False)
codeGroupO self@ConfigurationC2D{..} = (self, _cg, Odd, True)
codeGroupO self = (self, self._cg, Even, False)

{-# CLASH_OPAQUE codeGroupO #-}
