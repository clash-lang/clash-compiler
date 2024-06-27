module Clash.Cores.Sgmii.Common where

import Clash.Cores.LineCoding8b10b
import Clash.Prelude

-- | Format of rxConfReg and txConfReg, size of two data words
type ConfReg = BitVector 16

-- | Defines the possible values for the RUDI output signal of the PCS Receive
--   block as defined in IEEE 802.3 Clause 36
data Rudi = C | I | Invalid
  deriving (Generic, NFDataX, Eq, Show)

-- | Defines the type of the signal that indicates whether the current received
--   code group is at an even or odd index in the sequence
data Even = Even | Odd
  deriving (Generic, NFDataX, Eq, Show)

-- | Function that makes an Even RxEven Odd, and vice-versa
nextEven :: Even -> Even
nextEven Even = Odd
nextEven Odd = Even

-- | Defines the possible different types of ordered sets
data OrderedSet
  = OSetC
  | OSetI
  | OSetR
  | OSetS
  | OSetT
  | OSetV
  | OSetD
  deriving (Generic, NFDataX, Eq, Show)

-- | Defines the type of the signal that indicates whether the transmission is
--   in sync (Ok) or not (Fail)
data SyncStatus = Ok | Fail
  deriving (Generic, NFDataX, Eq, Show)

-- | Signal that is received by the two PCS blocks from the auto-negotiation
--   block to indicate the current state of the auto-negotiation block
data Xmit = Conf | Data | Idle
  deriving (Generic, NFDataX, Eq, Show, BitPack)

-- The following data words are used for comparisions in 'pcsReceive', and are
-- defined here to be usable in other places as well.

-- | Data word corresponding to the decoded version of code group D00.0, used
--   for early-end detection
dwD00_0 :: DataWord
dwD00_0 = Dw 0b00000000

-- | Data word corresponding to the decoded version of code group D02.2, used
--   for alternating configuration transmission
dwD02_2 :: DataWord
dwD02_2 = Dw 0b01000010

-- | Data word corresponding to the decoded version of code group D05.6, used
--   for correcting idle transmission
dwD05_6 :: DataWord
dwD05_6 = Dw 0b11000101

-- | Data word corresponding to the decoded version of code group D16.2, used
--   for preserving idle transmission
dwD16_2 :: DataWord
dwD16_2 = Dw 0b01010000

-- | Data word corresponding to the decoded version of code group D21.5, used
--   for alternating configuration transmission
dwD21_5 :: DataWord
dwD21_5 = Dw 0b10110101

-- | Data word  corresponding to the decoded version of code group K28.5, the
--   most commonly used comma value
cwK28_5 :: DataWord
cwK28_5 = Cw 0b10111100

-- | Data word corresponding to the decoded version of code group K23.7, used
--   for encapsulation of @Carrier_Extend@ (/R/)
cwR :: DataWord
cwR = Cw 0b11110111

-- | Data word corresponding to the decoded version of code group K27.7, used
--   for encapsulation of @Start_of_Packet@ (/S/)
cwS :: DataWord
cwS = Cw 0b11111011

-- | Data word corresponding to the decoded version of code group D29.7, used
--   for encapsulation of @End_of_Packet@ (/T/)
cwT :: DataWord
cwT = Cw 0b11111101

-- | Data word corresponding to the decoded version of code group K30.7, used
--   for encapsulation of @Error_Propagation@ (/V/)
cwV :: DataWord
cwV = Cw 0b11111110
