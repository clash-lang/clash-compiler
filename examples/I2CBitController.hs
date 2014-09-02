{-# LANGUAGE RecordWildCards, DataKinds, MagicHash, TypeOperators#-}
module I2CBitController where

import qualified Prelude
import CLaSH.Prelude
import Control.Applicative
-- import DE1Types
-- import I2CController.I2CTypes

type I2CIn       = (Bit,Bit)
type I2COut      = (Bit,Bool,Bit,Bool)

data I2CCommand = I2Cstart | I2Cstop | I2Cwrite | I2Cread | I2Cnop
  deriving (Eq, Ord)

type BitCtrlSig = (I2CCommand,Bit)
type BitRespSig = (Bool,Bool,Bit)

data BitStateMachine = BITidle |
                       BITstartA | BITstartB | BITstartC | BITstartD | BITstartE |
                       BITstopA | BITstopB | BITstopC | BITstopD |
                       BITrdA | BITrdB | BITrdC | BITrdD |
                       BITwrA | BITwrB | BITwrC | BITwrD
  deriving Eq

data MachineState =
  MachineState
    { bitStateM :: BitStateMachine -- State Machine
    , cmdAck    :: Bool            -- Command acknowledged register
    , sclOen    :: Bool            -- i2c clock output enable register
    , sdaOen    :: Bool            -- i2c data output enable register
    , sdaChk    :: Bool            -- check SDA statur (multi-master arbitration)
    }

isRising :: Bit
         -> Signal Bit
         -> Signal Bool
isRising is s = isRisingT <$> delayed <*> s
  where
    isRisingT old new = old == 0 && new == 1
    delayed = register is s

isFalling :: Bit
          -> Signal Bit
          -> Signal Bool
isFalling is s = isFallingT <$> delayed <*> s
  where
    isFallingT old new = old == 1 && new == 0
    delayed = register is s

isFallingB :: Bool
           -> Signal Bool
           -> Signal Bool
isFallingB is s = isFallingT <$> delayed <*> s
  where
    isFallingT old new = old && not new
    delayed = register is s

topEntity = i2cMasterBitCtrl

i2cMasterBitCtrl :: Signal (Unsigned 16)
                 -> Signal I2CCommand
                 -> Signal Bit
                 -> Signal (Bit, Bit)
                 -> ((Signal Bool,
                      Signal Bool,
                      Signal Bit),
                     Signal Bool,
                     (Signal Bit, Signal Bool, Signal Bit, Signal Bool))
i2cMasterBitCtrl clkCnt cmd dIn i2cIn = ((icmdAck,al,dout),busy,i2cOut)
  where
    -- whenever the slave is not ready it can delay the cycle by pulling SCL low
    -- slave_wait is asserted when master wants to drive SCL high, but the slave pulls it low
    -- slave_wait remains asserted until the slave releases SCL
    isclOenFalling = isFallingB False isclOen
    slaveWait  = register False slaveWait'
    slaveWait' = syncSCL ==& 0 &&$ (isclOenFalling ||$ slaveWait)

    -- master drives SCL high, but another master pulls it low
    -- master start counting down its low cycle now (clock synchronization)
    sclFalling = isFalling 1 syncSCL
    sclSync    = sclFalling &&$ isclOen

    -- generate clk enable signal
    clkEn      = genClkEn (sclSync, slaveWait, clkCnt)

    -- generate bus status controller
    (busy,al,dout,syncSCL) = busStatusControl i2cIn clkCnt clkEn cmd cState isdaChk isdaOen

    -- generate next state
    sm  = register startState sm'
    sm' = nextStateDecoder <$> sm <*> al <*> clkEn <*> cmd <*> dIn

    -- Extract command information
    icmdAck = cmdAck <$> sm
    isdaChk = sdaChk <$> sm
    cState  = bitStateM <$> sm
    isclOen = sclOen <$> sm
    isdaOen = sdaOen <$> sm

    i2cOut = (0,isclOen,0,isdaOen)

genClkEn :: ( Signal Bool
            , Signal Bool
            , Signal (Unsigned 16)
            )
         -> Signal Bool
genClkEn = genClkEnT <^> (0,True)
  where
    genClkEnT (cnt,clkEn) (sclSync,slaveWait,clkCnt) = ((cnt',clkEn'), clkEn)
      where
        (cnt',clkEn') = if cnt == 0 || sclSync
                           then (clkCnt,True)
                           else if slaveWait
                                   then (cnt  ,False)
                                   else (cnt-1,False)

busStatusControl :: Signal (Bit,Bit)
                 -> Signal (Unsigned 16)
                 -> Signal Bool
                 -> Signal I2CCommand
                 -> Signal BitStateMachine
                 -> Signal Bool
                 -> Signal Bool
                 -> (Signal Bool, Signal Bool, Signal Bit, Signal Bit)
busStatusControl i2cIn clkCnt clkEn cmd cState sdaChk iSdaOen =
    (busy,al,dout,syncSCL)
  where
    -- capture scl and sda
    i2cCaptured = captureSclSda i2cIn

    -- filter SCL and SDA; (attempt to) remove glitches
    filterCnt   = filterDivider clkCnt
    i2cFiltered = filterSclSda filterCnt i2cCaptured

    -- generate filtered/synchronised SCL and SDA signals
    (syncSCL, syncSDA) = syncSclSda i2cFiltered

    -- detect start condition => detect falling edge on SDA while SCL is high
    -- detect stop condition  => detect rising edge on SDA while SCL is high
    sdaFalling = isFalling 1 syncSDA
    sdaRising  = isRising 1 syncSDA

    (startCondition,stopCondition) = detectStartStop sdaFalling sdaRising syncSCL

    -- generate i2c-bus busy signal
    busy = genBusy startCondition stopCondition

    -- generate arbitration lost signal
    -- aribitration lost when:
    -- 1) master drives SDA high, but the i2c bus is low
    -- 2) stop detected while not requested (detect during 'idle' state)
    al = genArbitrationLost (clkEn,cmd,cState,sdaChk,syncSDA,iSdaOen,stopCondition)

    -- generate dout signal, store dout on rising edge of SCL
    sclRising = isRising 1 syncSCL
    dout      = genDout sclRising syncSDA

-- capture SCL and SDA
captureSclSda :: Signal (Bit,Bit)
              -> (Signal (BitVector 2, BitVector 2))
captureSclSda i2cIn = i2cCaptured
  where
    i2cCaptured  = register (0,0) i2cCaptured'
    i2cCaptured' = captureSclSdaT <$> i2cIn <*> i2cCaptured

    captureSclSdaT (sclI,sdaI) (cSCL,cSDA)
      = ( msb cSCL ++# sclI
        , msb cSDA ++# sdaI
        )

-- filter SCL and SDA; (attempt to) remove glitches
filterDivider :: Signal (Unsigned 16)
              -> Signal (Unsigned 14)
filterDivider clkCnt = filterCnt
  where
    filterCnt  = register 0 filterCnt'
    filterCnt' = liftA2 (\c f -> if f == 0 then unpack (fst (split c)) else f - 1)
                        clkCnt filterCnt

(<<#) :: KnownNat n => BitVector (1 + n) -> Bit -> BitVector (n + 1)
bv <<# b = snd (split bv) ++# b

filterSclSda :: Signal (Unsigned 14)
             -> Signal (BitVector 2, BitVector 2)
             -> Signal (BitVector 3, BitVector 3)
filterSclSda filterCnt i2cCaptured = i2cFiltered
  where
    i2cFiltered   = register (maxBound,maxBound) i2cFiltered'
    i2cFiltered'  = filterSclSdaT <$> filterCnt <*> i2cFiltered <*> i2cCaptured
    filterSclSdaT cnt (fSCL,fSDA) (cSCL,cSDA) =
      if cnt == 0
         then (fSCL <<# msb cSCL, fSDA <<# msb cSDA)
         else (fSCL,fSDA)

-- generate filtered SCL and SDA signals
syncSclSda :: Signal (BitVector 3, BitVector 3)
           -> (Signal Bit, Signal Bit)
syncSclSda i2cFiltered = (syncSCL, syncSDA)
  where
    syncSCL = register maxBound (filterT <$> (fst <$> i2cFiltered))
    syncSDA = register maxBound (filterT <$> (snd <$> i2cFiltered))

    filterT f = (f!2 .&. f!1) .|.
                (f!2 .&. f!0) .|.
                (f!1 .&. f!0)

-- detect start condition => detect falling edge on SDA while SCL is high
-- detect stop condition  => detect rising edge on SDA while SCL is high
detectStartStop :: Signal Bool
                -> Signal Bool
                -> Signal Bit
                -> (Signal Bool, Signal Bool)
detectStartStop sdaFalling sdaRising syncSCL =
    ( register False startCondition
    , register False stopCondition
    )
  where
    startCondition = sdaFalling &&$ syncSCL ==& 1
    stopCondition  = sdaRising  &&$ syncSCL ==& 1

genBusy :: Signal Bool
        -> Signal Bool
        -> Signal Bool
genBusy startCondition stopCondition = busy
  where
    busy  = register False busy'
    busy' = (startCondition ||$ busy) &&$ (not1 stopCondition)

-- generate arbitration lost signal
-- aribitration lost when:
-- 1) master drives SDA high, but the i2c bus is low
-- 2) stop detected while not requested (detect during 'idle' state)
genArbitrationLost :: ( Signal Bool
                      , Signal I2CCommand
                      , Signal BitStateMachine
                      , Signal Bool
                      , Signal Bit
                      , Signal Bool
                      , Signal Bool
                      )
                   -> Signal Bool
genArbitrationLost = genArbitrationLostT <^> (False,False)
  where
    genArbitrationLostT (cmdStop,al) (clkEn,cmd,cState,sdaChk,sSDA,iSdaOen,stopCondition)
        = ((cmdStop',al'),al)
      where
        cmdStop' = if clkEn
                      then cmd == I2Cstop
                      else cmdStop

        driveHighWhileLow = sdaChk && sSDA == 0 && iSdaOen

        al'      = if cState == BITidle
                      then driveHighWhileLow || (stopCondition && not cmdStop)
                      else driveHighWhileLow

-- generate dout signal, store dout on rising edge of SCL
genDout :: Signal Bool
        -> Signal Bit
        -> Signal Bit
genDout sclRising syncSDA = dout
  where
    dout  = register 0 dout'
    dout' = mux sclRising syncSDA dout

startState :: MachineState
startState = MachineState BITidle False True True False

nextStateDecoder :: MachineState
                   -> Bool
                   -> Bool
                   -> I2CCommand
                   -> Bit
                   -> MachineState
nextStateDecoder (MachineState {..}) al clkEn cmd din
  | al    = startState
  | clkEn = case bitStateM of
      -- idle
      BITidle   -> MachineState
                      { bitStateM = case cmd of
                            I2Cstart  -> BITstartA
                            I2Cstop   -> BITstopA
                            I2Cwrite  -> BITwrA
                            I2Cread   -> BITrdA
                            otherwise -> BITidle
                      , cmdAck = False
                      , sclOen = sclOen
                      , sdaOen = sdaOen
                      , sdaChk = False
                      }

      -- start
      BITstartA -> MachineState
                      { bitStateM = BITstartB
                      , cmdAck    = False
                      , sclOen    = sclOen
                      , sdaOen    = True   -- set SDA high
                      , sdaChk    = False  -- don't check SDA
                      }
      BITstartB -> MachineState
                      { bitStateM = BITstartC
                      , cmdAck    = False
                      , sclOen    = True   -- set SCL high
                      , sdaOen    = True   -- keep SDA high
                      , sdaChk    = False  -- don't check SDA
                      }
      BITstartC -> MachineState
                      { bitStateM = BITstartD
                      , cmdAck    = False
                      , sclOen    = True   -- keep SCL high
                      , sdaOen    = False  -- set SDA low
                      , sdaChk    = False  -- don't check SDA
                      }
      BITstartD -> MachineState
                      { bitStateM = BITstartE
                      , cmdAck   = False
                      , sclOen   = True   -- keep SCL high
                      , sdaOen   = False  -- keep SDA low
                      , sdaChk   = False  -- don't check SDA
                      }
      BITstartE -> MachineState
                      { bitStateM = BITidle
                      , cmdAck   = True   -- command completed
                      , sclOen   = False  -- set SCL low
                      , sdaOen   = False  -- keep SDA low
                      , sdaChk   = False  -- don't check SDA
                      }

      -- stop
      BITstopA  -> MachineState
                      { bitStateM = BITstopB
                      , cmdAck   = False
                      , sclOen   = False  -- keep SCL Low
                      , sdaOen   = False  -- set SDA low
                      , sdaChk   = False  -- don't check SDA
                      }
      BITstopB  -> MachineState
                      { bitStateM = BITstopC
                      , cmdAck   = False
                      , sclOen   = True   -- set SCL High
                      , sdaOen   = False  -- keep SDA low
                      , sdaChk   = False  -- don't check SDA
                      }
      BITstopC  -> MachineState
                      { bitStateM = BITstopD
                      , cmdAck   = False
                      , sclOen   = True   -- keep SCL High
                      , sdaOen   = False  -- keep SDA low
                      , sdaChk   = False  -- don't check SDA
                      }
      BITstopD  -> MachineState
                      { bitStateM = BITidle
                      , cmdAck   = True   -- command completed
                      , sclOen   = True   -- keep SCL High
                      , sdaOen   = True   -- set SDA high
                      , sdaChk   = False  -- don't check SDA
                      }

      -- read
      BITrdA    -> MachineState
                      { bitStateM = BITrdB
                      , cmdAck   = False
                      , sclOen   = False  -- keep SCL Low
                      , sdaOen   = True   -- tri-state SDA
                      , sdaChk   = False  -- don't check SDA
                      }
      BITrdB    -> MachineState
                      { bitStateM = BITrdC
                      , cmdAck   = False
                      , sclOen   = True   -- set SCL High
                      , sdaOen   = True   -- tri-state SDA
                      , sdaChk   = False  -- don't check SDA
                      }
      BITrdC    -> MachineState
                      { bitStateM = BITrdD
                      , cmdAck   = False
                      , sclOen   = True   -- keep SCL High
                      , sdaOen   = True   -- tri-state SDA
                      , sdaChk   = False  -- don't check SDA
                      }
      BITrdD    -> MachineState
                      { bitStateM = BITidle
                      , cmdAck   = True   -- command completed
                      , sclOen   = False  -- set SCL Low
                      , sdaOen   = True   -- tri-state SDA
                      , sdaChk   = False  -- don't check SDA
                      }

      -- write
      BITwrA    -> MachineState
                      { bitStateM = BITwrB
                      , cmdAck   = False
                      , sclOen   = False         -- keep SCL Low
                      , sdaOen   = (din == high) -- set SDA
                      , sdaChk   = False         -- don't check SDA (SCL low)
                      }
      BITwrB    -> MachineState
                      { bitStateM = BITwrC
                      , cmdAck   = False
                      , sclOen   = True          -- set SCL High
                      , sdaOen   = (din == high) -- keep SDA
                      , sdaChk   = False         -- don't check SDA yet
                      }                          -- Allow some more time for SDA and SCL to settle
      BITwrC    -> MachineState
                      { bitStateM = BITwrD
                      , cmdAck   = False
                      , sclOen   = True          -- keep SCL High
                      , sdaOen   = (din == high) -- keep SDA
                      , sdaChk   = True          -- check SDA
                      }
      BITwrD    -> MachineState
                      { bitStateM = BITidle
                      , cmdAck   = True          -- command completed
                      , sclOen   = False         -- set SCL Low
                      , sdaOen   = (din == high) -- keep SDA
                      , sdaChk   = False         -- don't check SDA (SCL low)
                      }
  | otherwise =
      MachineState {bitStateM = bitStateM, cmdAck = False, sclOen = sclOen,
                    sdaOen = sdaOen, sdaChk = sdaChk}

