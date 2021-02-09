{-|
Copyright : © 2015-2016, Christiaan Baaij,
              2017     , Google Inc.
              2019     , Myrtle Software Ltd
Licence   : Creative Commons 4.0 (CC BY 4.0) (https://creativecommons.org/licenses/by/4.0/)
-}

{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Clash.Examples (
  -- * Decoders and Encoders
  -- $decoders_and_encoders

  -- * Counters
  -- $counters

  -- * Parity and CRC
  -- $parity_and_crc

  -- * UART model
  -- $uart
  )
where

import Clash.Prelude
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State

{- $setup
>>> :set -XDataKinds
>>> :m -Clash.Explicit.Prelude
>>> :m -Clash.Signal.Internal
>>> import Clash.Prelude
>>> import Test.QuickCheck ((===))
>>> import Clash.Examples.Internal
-}

{- $decoders_and_encoders
= Decoder

Using a @case@ statement:

@
decoderCase :: Bool -> BitVector 4 -> BitVector 16
decoderCase enable binaryIn | enable =
  case binaryIn of
    0x0 -> 0x0001
    0x1 -> 0x0002
    0x2 -> 0x0004
    0x3 -> 0x0008
    0x4 -> 0x0010
    0x5 -> 0x0020
    0x6 -> 0x0040
    0x7 -> 0x0080
    0x8 -> 0x0100
    0x9 -> 0x0200
    0xA -> 0x0400
    0xB -> 0x0800
    0xC -> 0x1000
    0xD -> 0x2000
    0xE -> 0x4000
    0xF -> 0x8000
decoderCase _ _ = 0
@

Using the `shiftL` function:

@
decoderShift :: Bool -> BitVector 4 -> BitVector 16
decoderShift enable binaryIn =
  if enable
     then 1 ``shiftL`` ('fromIntegral' binaryIn)
     else 0
@

Examples:

>>> decoderCase True 3
0000_0000_0000_1000
>>> decoderShift True 7
0000_0000_1000_0000

The following property holds:

prop> \enable binaryIn -> decoderShift enable binaryIn === decoderCase enable binaryIn

= Encoder

Using a @case@ statement:

@
encoderCase :: Bool -> BitVector 16 -> BitVector 4
encoderCase enable binaryIn | enable =
  case binaryIn of
    0x0001 -> 0x0
    0x0002 -> 0x1
    0x0004 -> 0x2
    0x0008 -> 0x3
    0x0010 -> 0x4
    0x0020 -> 0x5
    0x0040 -> 0x6
    0x0080 -> 0x7
    0x0100 -> 0x8
    0x0200 -> 0x9
    0x0400 -> 0xA
    0x0800 -> 0xB
    0x1000 -> 0xC
    0x2000 -> 0xD
    0x4000 -> 0xE
    0x8000 -> 0xF
encoderCase _ _ = 0
@

The following property holds:

prop> \en decIn -> en ==> (encoderCase en (decoderCase en decIn) === decIn)
-}

{- $counters
= 8-bit Simple Up Counter

Using `register`:

@
upCounter
  :: HiddenClockResetEnable dom
  => Signal dom Bool
  -> Signal dom (Unsigned 8)
upCounter enable = s
  where
    s = `register` 0 (`mux` enable (s + 1) s)
@

= 8-bit Up Counter With Load

Using `mealy`:

@
upCounterLd
  :: HiddenClockResetEnable dom
  => Signal dom (Bool,Bool,Unsigned 8)
  -> Signal dom (Unsigned 8)
upCounterLd = `mealy` upCounterLdT 0

upCounterLdT s (ld,en,dIn) = (s',s)
  where
    s' | ld        = dIn
       | en        = s + 1
       | otherwise = s
@

= 8-bit Up-Down counter

Using `register` and `mux`:

@
upDownCounter
  :: HiddenClockResetEnable dom
  => Signal dom Bool
  -> Signal dom (Unsigned 8)
upDownCounter upDown = s
  where
    s = `register` 0 (`mux` upDown (s + 1) (s - 1))
@

The following property holds:

prop> \en -> en ==> testFor 1000 (upCounter (pure en) .==. upDownCounter (pure en) :: Signal "System" Bool)

= LFSR

External/Fibonacci LFSR, for @n=16@ and using the primitive polynominal @1 + x^11 + x^13 + x^14 + x^16@

@
lfsrF' :: BitVector 16 -> BitVector 16
lfsrF' s = 'pack' feedback '++#' 'slice' d15 d1 s
  where
    feedback = s'!'5 ``xor`` s'!'3 ``xor`` s'!'2 ``xor`` s'!'0

lfsrF
  :: HiddenClockResetEnable dom
  => BitVector 16
  -> Signal dom Bit
lfsrF seed = 'msb' '<$>' r
  where r = 'register' seed (lfsrF' '<$>' r)
@

We can also build a internal/Galois LFSR which has better timing characteristics.
We first define a Galois LFSR parameterizable in its filter taps:

@
lfsrGP taps regs = 'zipWith' xorM taps (fb '+>>' regs)
  where
    fb  = 'last' regs
    xorM i x | i         = x ``xor`` fb
             | otherwise = x
@

Then we can instantiate a 16-bit LFSR as follows:

@
lfsrG :: HiddenClockResetEnable dom  => BitVector 16 -> Signal dom Bit
lfsrG seed = 'last' ('unbundle' r)
  where r = 'register' ('unpack' seed) (lfsrGP ('unpack' 0b0011010000000000) '<$>' r)
@

The following property holds:

prop> testFor 100 (lfsrF 0xACE1 .==. lfsrG 0x4645 :: Signal "System" Bool)

= Gray counter

Using the previously defined @upCounter@:

@
grayCounter
  :: HiddenClockResetEnable dom
  => Signal dom Bool
  -> Signal dom (BitVector 8)
grayCounter en = gray '<$>' upCounter en
  where gray xs = 'pack' ('msb' xs) '++#' 'xor' ('slice' d7 d1 xs) ('slice' d6 d0 xs)
@

= One-hot counter

Basically a barrel-shifter:

@
oneHotCounter
  :: HiddenClockResetEnable dom
  => Signal dom Bool
  -> Signal dom (BitVector 8)
oneHotCounter enable = s
  where
    s = 'register' 1 ('mux' enable ('rotateL' '<$>' s '<*>' 1) s)
@
-}

{- $parity_and_crc
= Parity

Just 'reduceXor':

@
parity :: Unsigned 8 -> Bit
parity data_in = `reduceXor` data_in
@

= Serial CRC

* Width = 16 bits
* Truncated polynomial = 0x1021
* Initial value = 0xFFFF
* Input data is NOT reflected
* Output CRC is NOT reflected
* No XOR is performed on the output CRC

@
crcT bv dIn = 'replaceBit' 0  dInXor
            $ 'replaceBit' 5  (bv'!'4  ``xor`` dInXor)
            $ 'replaceBit' 12 (bv'!'11 ``xor`` dInXor)
              rotated
  where
    dInXor  = dIn ``xor`` fb
    rotated = 'rotateL' bv 1
    fb      = 'msb' bv

crc
  :: HiddenClockResetEnable dom
  => Signal dom Bool
  -> Signal dom Bool
  -> Signal dom Bit
  -> Signal dom (BitVector 16)
crc enable ld dIn = s
  where
    s = 'register' 0xFFFF ('mux' enable ('mux' ld 0xFFFF (crcT '<$>' s '<*>' dIn)) s)
@
-}

{- $uart
@
{\-\# LANGUAGE RecordWildCards \#-\}

module UART (uart) where

import Clash.Prelude
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State

-- UART RX Logic
data RxReg
  = RxReg
  { _rx_reg        :: BitVector 8
  , _rx_data       :: BitVector 8
  , _rx_sample_cnt :: Unsigned 4
  , _rx_cnt        :: Unsigned 4
  , _rx_frame_err  :: Bool
  , _rx_over_run   :: Bool
  , _rx_empty      :: Bool
  , _rx_d1         :: Bit
  , _rx_d2         :: Bit
  , _rx_busy       :: Bool
  } deriving (Generic, NFDataX)

makeLenses ''RxReg

uartRX r\@(RxReg {..}) rx_in uld_rx_data rx_enable = 'flip' 'execState' r $ do
  -- Synchronize the async signal
  rx_d1 '.=' rx_in
  rx_d2 '.=' _rx_d1
  -- Uload the rx data
  'when' uld_rx_data $ do
    rx_data  '.=' _rx_reg
    rx_empty '.=' True
  -- Receive data only when rx is enabled
  if rx_enable then do
    -- Check if just received start of frame
    'when' (not _rx_busy && _rx_d2 == 0) $ do
      rx_busy       '.=' True
      rx_sample_cnt '.=' 1
      rx_cnt        '.=' 0
    -- Star of frame detected, Proceed with rest of data
    'when' _rx_busy $ do
      rx_sample_cnt '+=' 1
      -- Logic to sample at middle of data
      'when' (_rx_sample_cnt == 7) $ do
        if _rx_d1 == 1 && _rx_cnt == 0 then
          rx_busy '.=' False
        else do
          rx_cnt '+=' 1
          -- start storing the rx data
          'when' (_rx_cnt > 0 && _rx_cnt < 9) $ do
            rx_reg '%=' 'replaceBit' (_rx_cnt - 1) _rx_d2
          'when' (_rx_cnt == 9) $ do
            rx_busy .= False
            -- Check if End of frame received correctly
            if _rx_d2 == 0 then
              rx_frame_err '.=' True
            else do
              rx_empty     '.=' False
              rx_frame_err '.=' False
              -- Check if last rx data was not unloaded
              rx_over_run  '.=' not _rx_empty
  else do
    rx_busy .= False

-- UART TX Logic
data TxReg
  = TxReg
  { _tx_reg      :: BitVector 8
  , _tx_empty    :: Bool
  , _tx_over_run :: Bool
  , _tx_out      :: Bit
  , _tx_cnt      :: Unsigned 4
  } deriving (Generic, NFDataX)

makeLenses ''TxReg

uartTX t\@(TxReg {..}) ld_tx_data tx_data tx_enable = 'flip' 'execState' t $ do
  'when' ld_tx_data $ do
    if not _tx_empty then
      tx_over_run '.=' False
    else do
      tx_reg   '.=' tx_data
      tx_empty '.=' False
  'when' (tx_enable && not _tx_empty) $ do
    tx_cnt '+=' 1
    'when' (_tx_cnt == 0) $
      tx_out '.=' 0
    'when' (_tx_cnt > 0 && _tx_cnt < 9) $
      tx_out '.=' _tx_reg '!' (_tx_cnt - 1)
    'when' (_tx_cnt == 9) $ do
      tx_out   '.=' 1
      tx_cnt   '.=' 0
      tx_empty '.=' True
  'unless' tx_enable $
    tx_cnt '.=' 0

-- Combine RX and TX logic
uart ld_tx_data tx_data tx_enable rx_in uld_rx_data rx_enable =
    ( _tx_out   '<$>' txReg
    , _tx_empty '<$>' txReg
    , _rx_data  '<$>' rxReg
    , _rx_empty '<$>' rxReg
    )
  where
    rxReg     = register rxRegInit (uartRX '<$>' rxReg '<*>' rx_in '<*>' uld_rx_data
                                           '<*>' rx_enable)
    rxRegInit = RxReg { _rx_reg        = 0
                      , _rx_data       = 0
                      , _rx_sample_cnt = 0
                      , _rx_cnt        = 0
                      , _rx_frame_err  = False
                      , _rx_over_run   = False
                      , _rx_empty      = True
                      , _rx_d1         = 1
                      , _rx_d2         = 1
                      , _rx_busy       = False
                      }

    txReg     = register txRegInit (uartTX '<$>' txReg '<*>' ld_tx_data '<*>' tx_data
                                           '<*>' tx_enable)
    txRegInit = TxReg { _tx_reg      = 0
                      , _tx_empty    = True
                      , _tx_over_run = False
                      , _tx_out      = 1
                      , _tx_cnt      = 0
                      }
@
-}
