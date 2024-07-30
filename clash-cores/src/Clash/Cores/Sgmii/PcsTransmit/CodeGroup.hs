{-# LANGUAGE CPP #-}

-- |
--   Copyright   :  (C) 2024, QBayLogic B.V.
--   License     :  BSD2 (see the file LICENSE)
--   Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
--
--   Code group process of the PCS transmit block, as defined in IEEE 802.3
--   Figure 36-6
module Clash.Cores.Sgmii.PcsTransmit.CodeGroup
  ( CodeGroupState (..)
  , codeGroupO
  , codeGroupT
  )
where

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
      , _cg :: Cg
      , _txConfReg :: ConfReg
      , _txEven :: Even
      , _txOSet :: OrderedSet
      }
  | DataGo {_rd :: Bool, _cg :: Cg, _txConfReg :: ConfReg, _txEven :: Even}
  | IdleDisparityWrong {_rd :: Bool, _cg :: Cg, _txConfReg :: ConfReg}
  | IdleDisparityOk {_rd :: Bool, _cg :: Cg, _txConfReg :: ConfReg}
  | IdleIB {_rd :: Bool, _cg :: Cg, _txConfReg :: ConfReg, _i :: Index 2}
  | ConfCA {_rd :: Bool, _cg :: Cg, _txConfReg :: ConfReg, _i :: Index 2}
  | ConfCB {_rd :: Bool, _cg :: Cg, _txConfReg :: ConfReg, _i :: Index 2}
  | ConfCC {_rd :: Bool, _cg :: Cg, _txConfReg :: ConfReg, _i :: Index 2}
  | ConfCD {_rd :: Bool, _cg :: Cg, _txConfReg :: ConfReg, _i :: Index 2}
  deriving (Generic, NFDataX, Eq, Show)

-- | State transitions from @GENERATE_CODE_GROUP@ from Figure 36-6, which need
--   to be set in all parent states of @GENERATE_CODE_GROUP@ as this state
--   itself is not implemented as it does not transmit a code group
generateCg :: OrderedSet -> Bool -> Cg -> ConfReg -> Even -> CodeGroupState
generateCg txOSet rd cg txConfReg txEven
  | txOSet == OSetD = DataGo rd cg txConfReg txEven
  | txOSet == OSetI && rd = IdleDisparityWrong rd cg txConfReg
  | txOSet == OSetI && not rd = IdleDisparityOk rd cg txConfReg
  | txOSet == OSetC = ConfCA rd cg txConfReg 0
  | otherwise = SpecialGo rd cg txConfReg txEven txOSet

-- | State transition function for the states as defined in IEEE 802.3 Clause
--   36, specifically Figure 36-6. This function receives an ordered set from
--   the ordered set state machine, a @TXD@ value from the outside world and
--   then sends out the correct code group based on the given ordered set.
codeGroupT ::
  -- | State variable
  CodeGroupState ->
  -- | Input data word from the ordered set, new input value and the config
  --   register
  (OrderedSet, BitVector 8, Maybe ConfReg) ->
  -- | The new state
  CodeGroupState
codeGroupT self (txOSet, dw, txConfReg) = nextState
 where
  (dw', nextState) = case self of
    SpecialGo{} ->
      ( case _txOSet self of
          OSetS -> cwS
          OSetT -> cwT
          OSetR -> cwR
          _ -> cwV
      , generateCg' txEven
      )
    DataGo{} -> (Dw dw, generateCg' txEven)
    IdleDisparityWrong{} -> (cwK28_5, IdleIB rd cg txConfReg' 0)
    IdleDisparityOk{} -> (cwK28_5, IdleIB rd cg txConfReg' 1)
    IdleIB{} -> (if _i self == 0 then dwD05_6 else dwD16_2, generateCg' Odd)
    ConfCA{} -> (cwK28_5, ConfCB rd cg txConfReg' (_i self))
    ConfCB{} ->
      ( if _i self == 0 then dwD21_5 else dwD02_2
      , ConfCC rd cg txConfReg' (_i self)
      )
    ConfCC{} -> (Dw (resize txConfReg'), ConfCD rd cg txConfReg' (_i self))
    ConfCD{} ->
      ( Dw (resize $ rotateR (_txConfReg self) 8)
      , if _i self == 0 && txOSet == OSetC
          then ConfCA rd cg txConfReg' 1
          else generateCg' Odd
      )

  generateCg' = generateCg txOSet rd cg txConfReg'
  txConfReg' = fromMaybe (_txConfReg self) txConfReg
  (rd, cg) = encode8b10b (_rd self) dw'
  txEven = nextEven (_txEven self)

{-# CLASH_OPAQUE codeGroupT #-}

-- | Output transition function for the states as defined in IEEE 802.3 Clause
--   36, specifically Figure 36-6. This function takes the state values that
--   have been determined in 'codeGroupT' and sets the correct outputs.
codeGroupO ::
  -- | Current state
  CodeGroupState ->
  -- | New output values
  (CodeGroupState, Cg, Even, Bool)
codeGroupO self = case self of
  SpecialGo{} -> (self, _cg self, txEven, True)
  DataGo{} -> (self, _cg self, txEven, True)
  IdleIB{} -> (self, _cg self, Odd, True)
  ConfCB{} -> (self, _cg self, Odd, False)
  ConfCD{} -> (self, _cg self, Odd, True)
  _ -> (self, _cg self, Even, False)
 where
  txEven = nextEven (_txEven self)

{-# CLASH_OPAQUE codeGroupO #-}
