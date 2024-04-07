{-|
Copyright   :  (C) 2022-2024, Google Inc.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Unisim.DnaPortE2.Internal where

import Clash.Cores.Xilinx.Xpm.Cdc.Internal
import Clash.Explicit.Prelude hiding (Read)

data State
  = Load
  -- ^ Load the DNA register with an identifier unique to the FPGA
  | Shift (BitVector 96)
  -- ^ Shift out a bit from the DNA register
  | Done (BitVector 96)
  -- ^ Valid DNA shifted out
  deriving (Generic, NFDataX)

type Read = Bool
type Shift = Bool

-- | A valid DNA that can be used for simulation purposes
simDna2 :: BitVector 96
simDna2 = 0x4ABABAB55555555DEADBEEF1

-- | Whether a given DNA value is valid. I.e., when the two most significant and
-- two least significant bits are set to @0b01@
isValidDna :: BitVector 96 -> Bool
isValidDna val = slice d95 d94 val == 1 && slice d1 d0 val == 1

-- | Convenience wrapper around the 'dnaPortE2' primitive that reads the DNA value
readDnaPortE2 ::
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Simulation only DNA value, must have bits @[95:94]@ and @[1:0]@ set to @0b01@
  BitVector 96 ->
  Signal dom (Maybe (BitVector 96))
readDnaPortE2 clk rst ena0 simDna = maybeDna
 where
  ena1 = fromEnable ena0
  dout = dnaPortE2 simDna clk (pure 0) (doRead .&&. ena1) (doShift .&&. ena1)
  (doRead, doShift, maybeDna) = mooreB clk rst ena0 goState goOut Load dout

  goOut :: State ->  (Read,  Shift, Maybe (BitVector 96))
  goOut Load       = (True,  False, Nothing)
  goOut (Shift _ ) = (False, True,  Nothing)
  goOut (Done dna) = (False, False, Just dna)

  goState :: State -> Bit -> State
  goState state input =
    case state of
      Load -> Shift 0
      Shift dna
        | isValidDna dna -> Done dna
        | otherwise      -> Shift (input +>>. dna)
      Done _ -> state

-- | From https://docs.xilinx.com/r/2021.2-English/ug974-vivado-ultrascale-libraries/DNA_PORTE2:
--
-- The DNA_PORT allows access to a dedicated shift register that can be loaded
-- with the Device DNA data bits (factory-programmed, read-only unique ID) for a
-- given UltraScale device. In addition to shifting out the DNA data bits, this
-- component allows for the inclusion of supplemental bits of your data, or
-- allows for the DNA data to rollover (repeat DNA data after initial data has
-- been shifted out). This component is primarily used in conjunction with other
-- circuitry to build added copy protection for the device bitstream from possible
-- theft.
--
-- To access the Device DNA data, you must first load the shift register by setting
-- the active-High READ signal for one clock cycle. After the shift register is
-- loaded, the data can be synchronously shifted out by enabling the active-High
-- SHIFT input and capturing the data out the DOUT output port. Additional data can
-- be appended to the end of the 96-bit shift register by connecting the appropriate
-- logic to the DIN port. If DNA data rollover is desired, connect the DOUT port
-- directly to the DIN port to allow for the same data to be shifted out after
-- completing the 96-bit shift operation. If no additional data is necessary, the
-- DIN port can be tied to a logic zero. The attribute SIM_DNA_VALUE can be
-- optionally set to allow for simulation of a possible DNA data sequence. By
-- default, the Device DNA data bits are all zeros in the simulation model.
dnaPortE2 ::
  forall dom .
  KnownDomain dom =>
  -- | Simulation only DNA value, must have bits @[95:94]@ and @[1:0]@ set to @0b01@
  "SIM_DNA_VALUE" ::: BitVector 96 ->
  -- | Incoming clock signal
  "CLK" ::: Clock dom ->
  -- | Shift register input pin
  "DIN" ::: Signal dom Bit ->
  -- | Active high load DNA, active low read input
  "READ" ::: Signal dom Bool ->
  -- | Active high shift enable
  "SHIFT" ::: Signal dom Bool ->
  -- | DNA output pin
  "DOUT" ::: Signal dom Bit
dnaPortE2 dnaSimValue clk din doRead doShift
  | slice d95 d94 dnaSimValue /= 0b01 || slice d1 d0 dnaSimValue /= 0b01 =
    clashCompileError "dnaPortE2: Supplied simulation DNA must have bits [95:94] and [1:0] set to 0b01"
  | clashSimulation = unpack . resize <$> regOut
  | otherwise =
    unPort @(Port "DOUT" dom Bit) $
      inst
        (instConfig "DNA_PORTE2")
          { library = Just "UNISIM"
          , libraryImport = Just "UNISIM.vcomponents.all" }

        (Param @"SIM_DNA_VALUE" dnaSimValue)

        (ClockPort @"CLK"   clk)
        (Port      @"DIN"   din)
        (Port      @"READ"  (boolToBit <$> doRead))
        (Port      @"SHIFT" (boolToBit <$> doShift))
 where
  regOut = register clk noReset enableGen 0 $
    mux
      doRead
      (pure dnaSimValue)
      (mux
         doShift
         ((+>>.) <$> din <*> regOut)
         regOut
      )
