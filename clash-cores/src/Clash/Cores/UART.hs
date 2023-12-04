{-|
  Copyright   :  (C) 2021, LUMI GUIDE FIETSDETECTIE B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  UART transmitter and receiver module
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Clash.Cores.UART
  ( BaudGenerator
  , ValidBaud
  , baudGenerator
  , baudGenToSignal
  , uartTxNoBaudGen
  , uartTx
  , uartRxNoBaudGen
  , uartRx
  , uartNoBaudGen
  , uart

  -- * Internal
  , UartBaudGenTick
  , UartBaudGenTick2
  , PeriodToHz
  , DivRound
  , BaudGenCounterWidth
  , Second
  ) where

import Clash.Prelude
import Clash.Class.HasDomain (TryDomain, TryDomainResult(Found))
import Data.Maybe (isJust)

-- | Utility type that represents a second
type Second = 1_000_000_000_000 -- picoseconds

-- | Division that rounds the result
type DivRound (a :: Nat) (b :: Nat) = Div (a + (Div b 2)) b

-- | Calculate frequency from period at type level
type PeriodToHz (period :: Nat) = Div Second period

-- | Calculate period from frequency at type level
type HzToPeriod (freq :: Nat) = Div Second freq

-- | UART baud generator
newtype BaudGenerator (dom :: Domain) = BaudGenerator (Signal dom Bool)

-- | Find clock domain for BaudGenerator
type instance TryDomain t (BaudGenerator dom) = 'Found dom

-- | The width of the counter used for baud generation
type BaudGenCounterWidth = 16

-- | Create a nice type error if it's not possible to generate an accurate
-- enough baud from the clock
type family UartBaudGenTick2
  (clkFastEnough :: Bool)
  (dom :: Domain)
  (baud :: Nat) :: Nat
  where
    UartBaudGenTick2 'False dom baud
      = TypeError
          (('Text "The UART with baud "
              ':<>: 'ShowType baud
              ':<>: 'Text " requires a driving clock with frequency of at least "
              ':<>: 'ShowType (16 * baud)
              ':<>: 'Text " Hz."
          )
          ':$$:
          ( 'Text "  The used clock has frequency "
              ':<>: ('ShowType (PeriodToHz (DomainPeriod dom)))
              ':<>: 'Text " Hz."
          ))
    UartBaudGenTick2 'True dom baud
      = DivRound
          (16 * baud * 2 ^ BaudGenCounterWidth)
          (PeriodToHz (DomainPeriod dom))

-- | Utility type to calculate tick amount
type UartBaudGenTick (dom :: Domain) (baud :: Nat)
   = UartBaudGenTick2
       (16 * baud <=? PeriodToHz (DomainPeriod dom))
       dom
       baud

-- | Any function that instantiates a baud generator
-- with polymorphic arguments requires this constraint.
type ValidBaud (dom :: Domain) (baud :: Nat)
   = ( KnownNat (UartBaudGenTick dom baud)
     , KnownNat (DomainPeriod dom)
     , 1 <= DomainPeriod dom
     , 1 <= 16 * (DomainPeriod dom)
     , 1 <= baud
     )

-- | Generates an accurate baud for usage with the UART modules
--
-- If multiple UARTs are instantiated with the same baud it's more efficient to
-- have a single `baudGenerator` and manually pass them to
-- the @*NoBaudGen@ functions.
baudGenerator
  :: forall (baud :: Nat)
            (dom :: Domain)
   . HiddenClockResetEnable dom
  => ValidBaud dom baud
  => SNat baud
  -- ^ The desired baud
  -> BaudGenerator dom
  -- ^ The resulting baud generator that can be used by uart transmit and
  -- receive components
baudGenerator SNat = BaudGenerator en
  where
    tick
      :: Unsigned (BaudGenCounterWidth + 1)
      -> Unsigned (BaudGenCounterWidth + 1)
    tick = (+) (snatToNum $ SNat @(UartBaudGenTick dom baud))
    zeroMsb = extend . truncateB @Unsigned @BaudGenCounterWidth
    cnt = register 0 $ fmap (tick . zeroMsb) cnt
    en1, en2, en :: Signal dom Bool
    en1 = fmap (bitToBool . msb) cnt
    en2 = riseEvery $ SNat @(Div (HzToPeriod baud) (16 * (DomainPeriod dom)))
    -- The number 172_500_000 was found empirically by calculating the error
    -- between the baudGenerator and the perfect uart period. At this threshold
    -- the counter overflow method is about as accurate as simply rising every
    -- n clock edges. Since the counter overflow based method has low accuracy
    -- when high baud <<< clock frequency we switch to the riseEvery construct.
    en = case compareSNat (SNat @(DomainPeriod dom * baud)) (SNat @172_500_000) of
      SNatGT -> en1
      SNatLE -> en2

-- | Access the underlying enable signal of the baud generator
baudGenToSignal :: BaudGenerator dom -> Signal dom Bool
baudGenToSignal (BaudGenerator en) = en

-- | State used for the UART transmitter
data UartTxState
   -- | Not transmitting anything
   = UartTxIdle
   -- | Transmitting a word
   | UartTxTransmitting
       { uartTxWord :: BitVector 10
       -- ^ All databits that need to be transmitted
       -- Includes start and stop bit
       , uartTxPeriodCounter :: Unsigned 4
       -- ^ Counter that tracks how many cycles we need to hold the
       -- current bit on the tx line
       }
  deriving (Show, Generic, NFDataX)

-- | Create a word that includes start a stop bits from a raw word
createTxWord
  :: BitVector 8
  -- ^ The word to transmit
  -> BitVector 10
  -- ^ The word including start and stop bit
createTxWord d = (1 :: (BitVector 1)) ++# d ++# (0 :: (BitVector 1))

-- | State transition function for the UART transmitter
uartTxT
  :: UartTxState
  -- ^ The current state
  -> Maybe (BitVector 8)
  -- ^ The word to transmit
  -> (UartTxState, (Bit, Bool))
  -- ^ The next state, the output bit to the tx line and an acknowledgement
  -- that the word to transmit has been received
uartTxT UartTxIdle (Just d) = (nextState, (1, True))
  where
    nextState = UartTxTransmitting (createTxWord d) 0
uartTxT UartTxIdle Nothing = (UartTxIdle, (1, False))
uartTxT UartTxTransmitting{..} mD = (nextState, (lsb uartTxWord, ack))
  where
    periodDone = uartTxPeriodCounter == 15
    lastBit = uartTxWord == 1
    doneTx = periodDone && lastBit
    nextWord = shiftR uartTxWord 1
    nextCounter = uartTxPeriodCounter + 1

    (nextState, ack)
      | doneTx     = case mD of
                       Just d  -> (UartTxTransmitting (createTxWord d) 0, True)
                       Nothing -> (UartTxIdle, False)
      | periodDone = (UartTxTransmitting nextWord 0, False)
      | otherwise  = (UartTxTransmitting uartTxWord nextCounter, False)

-- | The UART transmitter
--
-- This version requires the baud generator to be passed in. Create one
-- using `Clash.Cores.UART.baudGenerator`.
uartTxNoBaudGen
  :: HiddenClockResetEnable dom
  => BaudGenerator dom
  -- ^ The baud generator
  -> Signal dom (Maybe (BitVector 8))
  -- ^ The word to transmit
  -> (Signal dom Bit, Signal dom Bool)
  -- ^ The output bits for the tx line and an acknowledgement that the word
  -- to transmit has been received
uartTxNoBaudGen (BaudGenerator enable) mWord = (tx, ackOut)
  where
    ackOut = register False $ mux (isRising False ack) ack (pure False)
    (tx, ack) = unbundle fsmOut
    (uartS, fsmOut) = unbundle
                        $ regEn (UartTxIdle, (1, False)) enable
                        $ uartTxT <$> uartS <*> mWord

-- | The UART transmitter
uartTx
  :: forall (dom :: Domain)
            (baud :: Nat)
   . HiddenClockResetEnable dom
  => ValidBaud dom baud
  => SNat baud
  -- ^ The UART baud
  -> Signal dom (Maybe (BitVector 8))
  -- ^ The word to transmit
  -> (Signal dom Bit, Signal dom Bool)
  -- ^ The output bits for the tx line and an acknowledgement that the word
  -- to transmit has been received
uartTx baud mWord = uartTxNoBaudGen (baudGenerator baud) mWord

-- | State used for the UART receiver
data UartRxState
   -- | Waiting for a start bit
   = UartRxIdle
   -- | Wait until we are at the center of the start bit
   | UartRxStartBit
       { uartRxSampleCounter :: Unsigned 4
       -- ^ Current supersample
       }
   -- | Receive data bits
   | UartRxDataBits
       { uartRxSampleCounter :: Unsigned 4
       -- ^ Current supersample
       , uartRxWord :: BitVector 8
       -- ^ The data received
       }
   -- | Check the stop bit is correct
   | UartRxStopBit
       { uartRxSampleCounter :: Unsigned 4
       -- ^ Current supersample
       , uartRxWord :: BitVector 8
       -- ^ The received word
       }
  deriving (Show, Generic, NFDataX)

-- | State transition function for the UART receiver
uartRxT
  :: UartRxState
  -- ^ The current state
  -> Bit
  -- ^ The uart receive line
  -> (UartRxState, Maybe (BitVector 8))
  -- ^ The next state and a potential word that has been received
uartRxT UartRxIdle 1 = (UartRxIdle, Nothing)
uartRxT UartRxIdle 0 = (UartRxStartBit 1, Nothing)
uartRxT UartRxStartBit{..} b = (nextState, Nothing)
  where
    middleSample = uartRxSampleCounter == 8
    stillLow = b == 0
    validStartBit = middleSample && stillLow
    nextCounter = uartRxSampleCounter + 1

    nextState | validStartBit  = UartRxDataBits nextCounter 0b1000_0000
              | middleSample   = UartRxIdle
              | otherwise      = UartRxStartBit nextCounter
uartRxT UartRxDataBits{..} b = (nextState, Nothing)
  where
    middleSample = uartRxSampleCounter == 8
    lastBit = bitToBool $ lsb uartRxWord
    finishedReceive = middleSample && lastBit
    nextCounter = uartRxSampleCounter + 1
    nextWord = replaceBit (7 :: Index 8) b (shiftR uartRxWord 1)

    nextState | finishedReceive = UartRxStopBit nextCounter nextWord
              | middleSample    = UartRxDataBits nextCounter nextWord
              | otherwise       = UartRxDataBits nextCounter uartRxWord
uartRxT UartRxStopBit{..} b = (nextState, out)
  where
    middleSample = uartRxSampleCounter == 8
    validStopBit = middleSample && b == 1
    nextCounter = uartRxSampleCounter + 1
    out | validStopBit = Just uartRxWord
        | otherwise    = Nothing

    nextState | middleSample = UartRxIdle
              | otherwise    = UartRxStopBit nextCounter uartRxWord
uartRxT _ _ = (UartRxIdle, Nothing)

-- | The UART receiver
--
-- This version requires the baud generator to be passed in. Create one
-- using `Clash.Cores.UART.baudGenerator`.
uartRxNoBaudGen
  :: HiddenClockResetEnable dom
  => BaudGenerator dom
  -- ^ Baud generator
  -> Signal dom Bit
  -- ^ The UART receive line
  -> Signal dom (Maybe (BitVector 8))
  -- ^ The received words
uartRxNoBaudGen (BaudGenerator enable) b = register Nothing out
  where
    out = mux (isRising False $ isJust <$> fsmOut) fsmOut (pure Nothing)
    (uartS, fsmOut) =
      unbundle $ uartRxT <$> regEn UartRxIdle enable uartS <*> supersampled
    syncB = register 1 $ register 1 b
    -- Supersample according to doc:
    -- https://ww1.microchip.com/downloads/en/DeviceDoc/Atmel-2486-8-bit-AVR-microcontroller-ATmega8_L_datasheet.pdf#page=143
    shiftReg = regEn (repeat 1 :: Vec 3 Bit) enable (liftA2 (+>>) syncB shiftReg)
    majority s = boolToBit $ testBit (popCount $ v2bv s) 1
    supersampled = register 1 $ fmap majority shiftReg

-- | The UART receiver
uartRx
  :: HiddenClockResetEnable dom
  => ValidBaud dom baud
  => SNat baud
  -- ^ The UART baud
  -> Signal dom Bit
  -- ^ The UART receive line
  -> Signal dom (Maybe (BitVector 8))
  -- ^ The received words
uartRx baud rx = uartRxNoBaudGen (baudGenerator baud) rx

-- | The UART module; consists of both a UART transmitter and a UART receiver
--
-- This version requires the baud generator to be passed in. Create one
-- using `Clash.Cores.UART.baudGenerator`.
uartNoBaudGen
  :: HiddenClockResetEnable dom
  => BaudGenerator dom
  -- ^ The baud generator
  -> Signal dom Bit
  -- ^ UART receive line
  -> Signal dom (Maybe (BitVector 8))
  -- ^ transmit request
  -> (Signal dom (Maybe (BitVector 8)), Signal dom Bit, Signal dom Bool)
  -- ^ Words that have been received, the UART transmit line and the
  -- acknowledgement that the word to transmit has been received
uartNoBaudGen baudGen rxB txM = (rxM, txB, ackTx)
  where
    rxM = uartRxNoBaudGen baudGen rxB
    (txB, ackTx) = uartTxNoBaudGen baudGen txM

-- | The UART module; consists of both a UART transmitter and a UART receiver
uart
  :: HiddenClockResetEnable dom
  => ValidBaud dom baud
  => SNat baud
  -- ^ The baud
  -> Signal dom Bit
  -- ^ UART receive line
  -> Signal dom (Maybe (BitVector 8))
  -- ^ transmit request
  -> (Signal dom (Maybe (BitVector 8)), Signal dom Bit, Signal dom Bool)
  -- ^ Words that have been received, the UART transmit line and the
  -- acknowledgement that the word to transmit has been received
uart baud rxB txM = uartNoBaudGen (baudGenerator baud) rxB txM
