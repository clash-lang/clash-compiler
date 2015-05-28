{-# LANGUAGE NoImplicitPrelude, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|
Copyright : © Christiaan Baaij, 2015
Licence   : Creative Commons 4.0 (CC BY 4.0) (http://creativecommons.org/licenses/by/4.0/)
-}
module CLaSH.Examples (
  -- * Decoders and Encoders
  -- $decoders_and_encoders
  )
where

import CLaSH.Prelude
import Test.QuickCheck

-- $setup
-- >>> :set -XDataKinds
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
