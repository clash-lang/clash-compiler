{-|
  Copyright   :  (C) 2024, Rowan Goemans <goemansrowan@gmail.com>
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Utilities for computing cyclic redundancy checks (CRCs) in software and in
hardware.

$setup
>>> :set -XTypeFamilies
>>> :set -XTypeOperators
>>> :set -XTemplateHaskell
>>> :set -XFlexibleContexts
>>> :set -fplugin GHC.TypeLits.Normalise
>>> :set -fplugin GHC.TypeLits.KnownNat.Solver
>>> :set -fplugin GHC.TypeLits.Extra.Solver
>>> :m -Prelude
>>> import Clash.Prelude
>>> import Clash.Cores.CRC.Catalog
>>> import qualified Data.List as List
>>> import qualified Data.List.Split as List
>>> import Data.Char

CRCs are specified using `CRCParams`, which contains settings for CRC width,
polynomial, initial value, input/output reflection, and output XOR. This
fully defines a CRC computation. Many commonly used CRC algorithms are
available in the `Clash.Cores.CRC.Catalog` module, while most other CRC
designs can be accommodated by manually constructing `CRCParams`.

Call the `mkSoftwareCRC` function to create an engine to perform software
computations. It's not intented to be used on hardware.

Example usage:

First we convert the check characters to @BitVector 8@ for processing.

>>> checkInput = fmap (fromIntegral . ord) "123456789"
>>> checkValue = 0xcbf43926

Here we instantiate a software CRC with @dataWidth ~ 8@ and use it to get the CRC

>>> softwareCRC = mkSoftwareCRC (cRC32_ETHERNET d8)
>>> crcFromSoftware = digest $ List.foldl' feed softwareCRC checkInput
>>> crcFromSoftware == checkValue
True

For a hardware implementation the first thing you have to do is use Template Haskell
to evaluate the construction of `ParallelCRCParams` at compile time. An additional
parameter can be set called @nLanes@. It indicates how wide of an input can be
handled in a single clock cycle. See `mkParallelCRCParamsTH` for an example of
what that means.

>>> hwParams = $(mkParallelCRCParamsTH (cRC32_ETHERNET d8) d4)
>>> myEngine = exposeClockResetEnable crcEngine systemClockEngine resetGen enableGen hwParams

We can give upto 4 bytes from @checkInput@ to our hardware CRC in the format
of @Maybe (Index 4, Vec 4 (BitVector 8))@. The @Maybe@ indicates whether
we want to run the CRC engine this clock cycle. The @Index@ inside the @Maybe@
indicates how many @BitVector 8@ are valid inside the @Vec@. 0 means 1 is valid.
3 means 4 are valid.

>>> hwCheckInput = Just . (3,) <$> List.chunksOf 4 checkInput
>>> hwOut = myEngine (fromList hwCheckInput)
>>> crcFromHardware = List.last $ sampleN (1 + List.length hwInp) hwOut
>>> crcFromHardware == checkValue
>>> True

Notice that the `crcEngine` has latency of one clock cycle.

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
  , ParallelCRCParams
  , mkParallelCRCParamsTH
  , crcEngine
  , crcValidator
  ) where

import Clash.Cores.CRC.Internal
