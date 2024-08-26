{- |
  Copyright   :  (C) 2024, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Common functions, type definitions and hard-coded settings used in the
  different modules that are defined for SGMII
-}
module Clash.Cores.Sgmii.Common where

import Clash.Cores.LineCoding8b10b
import Clash.Prelude

-- | Format of a single code group, 10-bit
type CodeGroup = BitVector 10

-- | Format of @rxConfReg@ and @txConfReg@, size of two data words
type ConfReg = BitVector 16

-- | Defines the type of the signal that indicates whether the current received
--   code group is at an even or odd index in the sequence
data Even = Even | Odd
  deriving (Generic, NFDataX, Eq, Show)

-- | Function that makes an Even RxEven Odd, and vice-versa
nextEven :: Even -> Even
nextEven Even = Odd
nextEven Odd = Even

-- | Link speed that was communicated by the PHY
data LinkSpeed = Speed10 | Speed100 | Speed1000
  deriving (Generic, NFDataX, BitPack)

-- | Get the current link speed from a 'ConfReg'
toLinkSpeed :: ConfReg -> LinkSpeed
toLinkSpeed confReg =
  unpack $ pack (testBit confReg 11) ++# pack (testBit confReg 10)

-- | Defines the possible different types of ordered sets that can be generated
--   by the 'Sgmii.PcsTransmit.orderedSet' process
data OrderedSet = OSetC | OSetI | OSetR | OSetS | OSetT | OSetV | OSetD
  deriving (Generic, NFDataX, Eq, Show)

-- | Defines the possible values for the RUDI output signal of the PCS Receive
--   block as defined in IEEE 802.3 Clause 36
data Rudi = RudiC ConfReg | RudiI | RudiInvalid
  deriving (Generic, NFDataX, Eq, Show)

-- | Convert a 'Rudi' to a 'ConfReg'
toConfReg :: Rudi -> Maybe ConfReg
toConfReg (RudiC confReg) = Just confReg
toConfReg _ = Nothing

-- | Record that holds the current status of the module, specifically the
--   'Status' from 'Sgmii.sync', the 'ConfReg' that has been received by
--   'Sgmii.pcsReceive', the 'Rudi' that is transmitted by 'Sgmii.pcsReceive'
--   and the 'Xmit' that is transmitted by 'Sgmii.autoNeg'.
data SgmiiStatus = SgmiiStatus
  { _cBsStatus :: Status
  , _cSyncStatus :: Status
  , _cLinkSpeed :: LinkSpeed
  , _cXmit :: Xmit
  , _cRudi :: Rudi
  }

-- | Defines the type of the signal that indicates whether the transmission is
--   in sync ('Ok') or not ('Fail')
data Status = Fail | Ok
  deriving (Generic, NFDataX, Eq, Show)

-- | Signal that is received by the two PCS blocks from the auto-negotiation
--   block to indicate the current state of the auto-negotiation block
data Xmit = Conf | Data | Idle
  deriving (Generic, NFDataX, BitPack, Eq, Show)

-- | Return a 'Just' when the argument is 'True', else return a 'Nothing'
orNothing :: Bool -> a -> Maybe a
orNothing True a = Just a
orNothing False _ = Nothing

-- | Code group that corresponds to K28.5 with negative disparity
cgK28_5N :: CodeGroup
cgK28_5N = 0b0101111100

-- | Code group that corresponds to K28.5 with positive disparity
cgK28_5P :: CodeGroup
cgK28_5P = 0b1010000011

-- | Vector containing the two alternative forms (with opposite running
--   disparity) of K28.5. This is the only relevant comma, as the other commas
--   are set as "reserved" in the list of control words. The order of the commas
--   in this is important, as the first comma returns the negative running
--   disparity when it is decoded and the second comma returns the positive
--   running disparity when it is decoded. This is used in 'Sync.LossOfSync' to
--   recover the correct running disparity from a received comma.
commas :: Vec 2 CodeGroup
commas = cgK28_5N :> cgK28_5P :> Nil

-- | Data word corresponding to the decoded version of code group D00.0, used
--   for early-end detection
dwD00_0 :: Symbol8b10b
dwD00_0 = Dw 0b00000000

-- | Data word corresponding to the decoded version of code group D02.2, used
--   for alternating configuration transmission
dwD02_2 :: Symbol8b10b
dwD02_2 = Dw 0b01000010

-- | Data word corresponding to the decoded version of code group D05.6, used
--   for correcting idle transmission
dwD05_6 :: Symbol8b10b
dwD05_6 = Dw 0b11000101

-- | Data word corresponding to the decoded version of code group D16.2, used
--   for preserving idle transmission
dwD16_2 :: Symbol8b10b
dwD16_2 = Dw 0b01010000

-- | Data word corresponding to the decoded version of code group D21.5, used
--   for alternating configuration transmission
dwD21_5 :: Symbol8b10b
dwD21_5 = Dw 0b10110101

-- | Data word  corresponding to the decoded version of code group K28.5, the
--   most commonly used comma value
cwK28_5 :: Symbol8b10b
cwK28_5 = Cw 0b10111100

-- | Data word corresponding to the decoded version of code group K23.7, used
--   for encapsulation of @Carrier_Extend@ (/R/)
cwR :: Symbol8b10b
cwR = Cw 0b11110111

-- | Data word corresponding to the decoded version of code group K27.7, used
--   for encapsulation of @Start_of_Packet@ (/S/)
cwS :: Symbol8b10b
cwS = Cw 0b11111011

-- | Data word corresponding to the decoded version of code group D29.7, used
--   for encapsulation of @End_of_Packet@ (/T/)
cwT :: Symbol8b10b
cwT = Cw 0b11111101

-- | Data word corresponding to the decoded version of code group K30.7, used
--   for encapsulation of @Error_Propagation@ (/V/)
cwV :: Symbol8b10b
cwV = Cw 0b11111110
