{-|
  Copyright   :  (C) 2024, Rowan Goemans <goemansrowan@gmail.com>
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Utilities for computing cyclic redundancy checks (CRCs) in software and in
hardware.

CRCs are specified using 'CRCParams' type in conjunction with the 'KnownCRC' type class.
'CRCParams' contains settings for CRC width, polynomial, initial value,
input/output reflection, and output XOR. This fully defines a CRC computation.
Many commonly used CRC algorithms are available in the 'Clash.Cores.CRC.Catalog'
module, while most other CRC designs can be accommodated by manually constructing
'CRCParams' and writing a 'KnownCRC' instance.

Call the 'mkSoftwareCRC' using a specifc CRC to create an engine to perform software
computations. It's not intented to be used on hardware.

Example usage:

First we convert the check characters to @BitVector 8@ for processing.

>>> charToBV = fromIntegral . ord
>>> checkInput = fmap charToBv "123456789"
>>> checkValue = 0xcbf43926

Here we instantiate a software CRC engine for the 32-bit Ethernet CRC
that can handle @BitVector 8@ input and use it to compute the CRC.

>>> softwareCRC = mkSoftwareCRC (Proxy @CRC32_ETHERNET) d8
>>> crcFromSoftware = digest $ List.foldl' feed softwareCRC checkInput
>>> crcFromSoftware == checkValue
True

For a hardware implementation the first thing you have to do is use Template Haskell
to derive a 'HardwareCRC' instance using 'Clash.Cores.CRC.Derive.deriveHardwareCRC'.
Here you to provide a concrete @dataWidth@ and @nLanes@. Upto
@dataWidth * nLanes@ of an input can be handled in a single clock cycle in
@dataWidth@ increments. See the type of 'crcEngine' to see what impact it has
on the CRC circuit.

>>> $(deriveHardwareCRC (Proxy @CRC32_ETHERNET) d8 d4)
>>> crcEngine' = exposeClockResetEnable crcEngine systemClockGen resetGen enableGen
>>> myEngine = crcEngine' (Proxy @CRC32_ETHERNET) (Proxy @8) (Proxy @4)

We can give upto 4 bytes from @checkInput@ to our hardware CRC in the format
of @Maybe (Index 4, Vec 4 (BitVector 8))@. The @Maybe@ indicates whether
we want to run the CRC engine this clock cycle. The @Index@ inside the @Maybe@
indicates how many @BitVector 8@ are valid inside the @Vec@. 0 means 1 is valid.
3 means 4 are valid.

>>> hwInp0 = Just (3, unsafeFromList $ fmap charToBV "1234")
>>> hwInp1 = Just (3, unsafeFromList $ fmap charToBV "5678")
>>> hwInp2 = Just (0, unsafeFromList $ fmap charToBV "9___")
>>> hwInp = [Nothing, hwInp0, hwInp1, hwInp2]
>>> hwOut = myEngine $ fromList hwInp
>>> crcFromHardware = List.last $ sampleN (1 + List.length hwInp) hwOut
>>> crcFromHardware == checkValue
>>> True

Notice that the 'crcEngine' has latency of one clock cycle.

-}

module Clash.Cores.CRC
  ( CRCParams(..)
  -- ** Software
  , SoftwareCRC
  , mkSoftwareCRC
  , reset
  , feed
  , digest
  , residue
  -- ** Hardware
  , HardwareCRC
  , CRCHardwareParams
  , crcEngine
  , crcValidator
  ) where

import Clash.Cores.CRC.Internal
