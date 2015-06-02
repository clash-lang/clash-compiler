{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|
Copyright : © Christiaan Baaij, 2015
Licence   : Creative Commons 4.0 (CC BY 4.0) (http://creativecommons.org/licenses/by/4.0/)
-}
module CLaSH.Examples (
  -- * Decoders and Encoders
  -- $decoders_and_encoders

  -- * Counters
  -- $counters

  -- * Parity and CRC
  -- $parity_and_crc
  )
where

import CLaSH.Prelude
import Test.QuickCheck

-- $setup
-- >>> :set -XDataKinds -XFlexibleContexts -XBinaryLiterals -XTypeFamilies
-- >>> :set -fplugin GHC.TypeLits.Normalise
-- >>> import CLaSH.Prelude
-- >>> import Test.QuickCheck
-- >>> :{
-- let decoderCase :: Bool -> BitVector 4 -> BitVector 16
--     decoderCase enable binaryIn | enable =
--       case binaryIn of
--         0x0 -> 0x0001
--         0x1 -> 0x0002
--         0x2 -> 0x0004
--         0x3 -> 0x0008
--         0x4 -> 0x0010
--         0x5 -> 0x0020
--         0x6 -> 0x0040
--         0x7 -> 0x0080
--         0x8 -> 0x0100
--         0x9 -> 0x0200
--         0xA -> 0x0400
--         0xB -> 0x0800
--         0xC -> 0x1000
--         0xD -> 0x2000
--         0xE -> 0x4000
--         0xF -> 0x8000
--     decoderCase _ _ = 0
-- :}
--
-- >>> :{
-- let decoderShift :: Bool -> BitVector 4 -> BitVector 16
--     decoderShift enable binaryIn =
--       if enable
--          then 1 `shiftL` (fromIntegral binaryIn)
--          else 0
-- :}
--
-- >>> :{
-- let encoderCase :: Bool -> BitVector 16 -> BitVector 4
--     encoderCase enable binaryIn | enable =
--       case binaryIn of
--         0x0001 -> 0x0
--         0x0002 -> 0x1
--         0x0004 -> 0x2
--         0x0008 -> 0x3
--         0x0010 -> 0x4
--         0x0020 -> 0x5
--         0x0040 -> 0x6
--         0x0080 -> 0x7
--         0x0100 -> 0x8
--         0x0200 -> 0x9
--         0x0400 -> 0xA
--         0x0800 -> 0xB
--         0x1000 -> 0xC
--         0x2000 -> 0xD
--         0x4000 -> 0xE
--         0x8000 -> 0xF
--     encoderCase _ _ = 0
-- :}
--
-- >>> :{
-- let upCounter :: Signal Bool -> Signal (Unsigned 8)
--     upCounter enable = s
--       where
--         s = regEn 0 enable (s + 1)
-- :}
--
-- >>> :{
-- let upCounterLdT s (ld,en,dIn) = (s',s)
--       where
--         s' | ld        = dIn
--            | en        = s + 1
--            | otherwise = s
-- :}
--
-- >>> :{
-- let upCounterLd :: Signal (Bool,Bool,Unsigned 8) -> Signal (Unsigned 8)
--     upCounterLd = mealy upCounterLdT 0
-- :}
--
-- >>> :{
-- let upDownCounter :: Signal Bool -> Signal (Unsigned 8)
--     upDownCounter upDown = s
--       where
--         s = register 0 (mux upDown (s + 1) (s - 1))
-- :}
--
-- >>> :{
-- let lfsrF' :: BitVector 16 -> BitVector 16
--     lfsrF' s = feedback ++# slice d15 d1 s
--       where
--         feedback = s!5 `xor` s!3 `xor` s!2 `xor` s!0
-- :}
--
-- >>> :{
-- let lfsrF :: BitVector 16 -> Signal Bit
--     lfsrF seed = msb <$> r
--       where r = register seed (lfsrF' <$> r)
-- :}
--
-- >>> :{
-- let lfsrGP taps regs = zipWith xorM taps (fb +>> regs)
--       where
--         fb = last regs
--         xorM i x | i         =  x `xor` fb
--                  | otherwise = x
-- :}
--
-- >>> :{
-- let lfsrG :: BitVector 16 -> Signal Bit
--     lfsrG seed = last (unbundle r)
--       where r = register (unpack seed) (lfsrGP (unpack 0b0011010000000000) <$> r)
-- :}
--
-- >>> :{
-- let grayCounter :: Signal Bool -> Signal (BitVector 8)
--     grayCounter en = gray <$> upCounter en
--       where gray xs = msb xs ++# xor (slice d7 d1 xs) (slice d6 d0 xs)
-- :}
--
-- >>> :{
-- let oneHotCounter :: Signal Bool -> Signal (BitVector 8)
--     oneHotCounter enable = s
--       where
--         s = regEn 1 enable (rotateL s 1)
-- :}
--
-- >>> :{
-- let parity :: Unsigned 8 -> Bit
--     parity data_in = reduceXor data_in
-- :}
--
-- >>> :{
-- let crcT bv dIn = replaceBit 0  dInXor
--                 $ replaceBit 5  (bv!4  `xor` dInXor)
--                 $ replaceBit 12 (bv!11 `xor` dInXor)
--                   rotated
--       where
--         dInXor  = dIn `xor` fb
--         rotated = rotateL bv 1
--         fb      = msb bv
-- :}
--
-- >>> :{
-- let crc :: Signal Bool -> Signal Bool -> Signal Bit -> Signal (BitVector 16)
--     crc enable ld dIn = s
--       where
--         s = regEn 0xFFFF enable (mux ld 0xFFFF (crcT <$> s <*> dIn))
-- :}

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

prop> decoderShift enable binaryIn === decoderCase enable binaryIn

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

prop> en ==> (encoderCase en (decoderCase en decIn) === decIn)
-}

{- $counters
= 8-bit Simple Up Counter

Using `regEn`:

@
upCounter :: Signal Bool -> Signal (Unsigned 8)
upCounter enable = s
  where
    s = `regEn` 0 enable (s + 1)
@

= 8-bit Up Counter With Load

Using `mealy`:

@
upCounterLd :: Signal (Bool,Bool,Unsigned 8) -> Unsigned 8
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
upDownCounter :: Signal Bool -> Signal (Unsigned 8)
upDownCounter upDown = s
  where
    s = `register` 0 (`mux` upDown (s + 1) (s - 1))
@

The following property holds:

prop> en ==> testFor 1000 (upCounter (signal en) .==. upDownCounter (signal en))

= LFSR

External/Fibonacci LFSR, for @n=16@ and using the primitive polynominal @1 + x^11 + x^13 + x^14 + x^16@

@
lfsrF' :: BitVector 16 -> BitVector 16
lfsrF' s = feedback '++#' 'slice' d15 d1 s
  where
    feedback = s'!'5 ``xor`` s'!'3 ``xor`` s'!'2 ``xor`` s'!'0

lfsrF :: BitVector 16 -> Signal Bit
lfsrF seed = 'msb' '<$>' r
  where r = 'register' seed (lfsrF' '<$>' r)
@

We can also build a internal/Galois LFSR which has better timing characteristics.
We first define a Galois LFSR parametrizable in its filter taps:

@
lfsrGP taps regs = 'zipWith' xorM taps (fb '+>>' regs)
  where
    fb  = 'last' regs
    xorM i x | i         = x ``xor`` fb
             | otherwise = x
@

Then we can instance a 16-bit LFSR as follows:

@
lfsrG :: BitVector 16 -> Signal Bit
lfsrG seed = 'last' ('unbundle' r)
  where r = 'register' ('unpack' seed) (lfsrGP ('unpack' 0b0011010000000000) '<$>' r)
@

The following property holds:

prop> testFor 100 (lfsrF 0xACE1 .==. lfsrG 0x4645)

= Gray counter

Using the previously defined @upCounter@:

@
grayCounter :: Signal Bool -> Signal (BitVector 8)
grayCounter en = gray '<$>' upCounter en
  where gray xs = 'msb' xs '++#' 'xor' ('slice' d7 d1 xs) ('slice' d6 d0 xs)
@

= One-hot counter

Basically a barrel-shifter:

@
oneHotCounter :: Signal Bool -> Signal (BitVector 8)
oneHotCounter enable = s
  where
    s = 'regEn' 1 enable ('rotateL' s 1)
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
* Input date is NOT reflected
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

crc :: Signal Bool -> Signal Bool -> Signal Bit -> Signal (BitVector 16)
crc enable ld dIn = s
  where
    s = 'regEn' 0xFFFF enable ('mux' ld 0xFFFF (crcT '<$>' s '<*>' dIn))
@
-}
