{-|
  Copyright   :  (C) 2024, Rowan Goemans <goemansrowan@gmail.com>
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Utilities for computing cyclic redundancy checks (CRCs) in software and in
hardware.

CRCs are specified using 'CrcParams' type in conjunction with the 'KnownCrc' type class.
'CrcParams' contains settings for CRC width, polynomial, initial value,
input/output reflection, and output XOR. This fully defines a CRC computation.
Many commonly used CRC algorithms are available in the "Clash.Cores.Crc.Catalog"
module, while most other CRC designs can be accommodated by manually constructing
'CrcParams' and writing a 'KnownCrc' instance.

Call the 'mkSoftwareCrc' using a specifc CRC to create an engine to perform software
computations. It's not intended to be used on hardware.

Example usage:

>>> :set -XMultiParamTypeClasses
>>> import Clash.Prelude
>>> import Clash.Sized.Vector (unsafeFromList)
>>> import Data.Char (ord)
>>> import qualified Data.List as List
>>> import Clash.Cores.Crc
>>> import Clash.Cores.Crc.Catalog

First we convert the check characters to @BitVector 8@ for processing.

>>> charToBv = fromIntegral . ord
>>> checkInput = fmap charToBv "123456789"
>>> checkValue = 0xcbf43926

Here we instantiate a software CRC engine for the 32-bit Ethernet CRC
that can handle @BitVector 8@ input and use it to compute the CRC.

>>> softwareCrc = mkSoftwareCrc Crc32_ethernet d8
>>> crcFromSoftware = digest $ List.foldl' feed softwareCrc checkInput
>>> crcFromSoftware == checkValue
True

For a hardware implementation the first thing you have to do is use Template Haskell
to derive a 'HardwareCrc' instance using 'Clash.Cores.Crc.deriveHardwareCrc'.
Here you need to provide a concrete @dataWidth@ and @nLanes@. Up to
@dataWidth * nLanes@ of an input can be handled in a single clock cycle in
@dataWidth@ increments. See the type of 'crcEngine' to see what impact it has
on the CRC circuit.

>>> :{
deriveHardwareCrc Crc32_ethernet d8 d4
dummy = 1
:}

>>> crcEngine' = exposeClockResetEnable crcEngine systemClockGen resetGen enableGen
>>> myEngine = crcEngine' Crc32_ethernet

Note how we hinted to @clashi@ that our multi-line command was a list of
declarations by including a dummy declaration @dummy = 1@. Without this trick,
@clashi@ would expect an expression and the Template Haskell emitted by
@deriveHardwareCrc@ would not work.

We can give up to 4 bytes from @checkInput@ to our hardware CRC in the format
of @Maybe (Index 4, Vec 4 (BitVector 8))@. The @Maybe@ indicates whether
we want to run the CRC engine this clock cycle. The @Index@ inside the @Maybe@
indicates how many @BitVector 8@ are valid inside the @Vec@. 0 means 1 is valid.
3 means 4 are valid.

>>> hwInp0 = Just (3, unsafeFromList $ fmap charToBv "1234" :: Vec 4 (BitVector 8))
>>> hwInp1 = Just (3, unsafeFromList $ fmap charToBv "5678")
>>> hwInp2 = Just (0, unsafeFromList $ fmap charToBv "9___")
>>> hwInp = [Nothing, hwInp0, hwInp1, hwInp2]
>>> hwOut = myEngine (pure False) (fromList hwInp)
>>> crcFromHardware = List.last $ sampleN (1 + List.length hwInp) hwOut
>>> crcFromHardware == checkValue
True

Notice that the 'crcEngine' has a latency of one clock cycle.

-}

module Clash.Cores.Crc
  ( CrcParams(..)
  , KnownCrc(..)
  -- ** Software
  , SoftwareCrc
  , mkSoftwareCrc
  , reset
  , feed
  , digest
  , rawResidue
  , residue
  -- ** Hardware
  , HardwareCrc
  , deriveHardwareCrc
  , crcEngine
  , crcValidator
  ) where

import Clash.Cores.Crc.Internal
