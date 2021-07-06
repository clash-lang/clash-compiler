{-|
Copyright  :  (C) 2021,      QBayLogic B.V.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE ViewPatterns #-}

module Floating.TH where

import Clash.Prelude (BitPack, natToNum, SNat(..), unpack)
import Clash.Prelude.ROM.File (memFile)

import Prelude
import Language.Haskell.TH
  (appTypeE, conE, ExpQ, litE, litT, numTyLit, stringL, tupE)
import Language.Haskell.TH.Syntax (qRunIO)
import Numeric.IEEE
  (epsilon, infinity, maxFinite, minDenormal, minNormal)

import Clash.Cores.Xilinx.Floating as F
import Clash.Cores.Xilinx.Floating.Internal as F

romDataFromFile
  :: BitPack a
  => FilePath
  -> [a]
  -> ExpQ
romDataFromFile file es =
  qRunIO (writeFile file $ memFile (Just 0) es)
  >> tupE [ appTypeE (conE 'SNat) (litT . numTyLit . toInteger $ length es)
          , litE $ stringL file
          ]

delayOutput
  :: Int
  -> [(Float, Float, Float)]
  -> [(Float, Float, Float)]
delayOutput d es = zip3 xs ys rs
 where
  (xs0, ys0, rs0) = unzip3 es
  xs = xs0 ++ repeat (last xs0)
  ys = ys0 ++ repeat (last ys0)
  rs = replicate d 0 ++ rs0

addBasicSamples :: [(Float, Float, Float)]
addBasicSamples =
  delayOutput (natToNum @F.AddDefDelay) $
  [ (1, 4, 5)
  , (2, 5, 7)
  , (3, 6, 9)
    -- Subnormal positive number is conditioned to plus zero
    --
    -- The unconditioned result is the subnormal of largest magnitude
  , ( -minNormal
    , minNormal + maxDenormal
    , 0
    )
    -- The unconditioned result is the subnormal of smallest magnitude
  , ( -minNormal
    , minNormal + minDenormal
    , 0
    )
    -- Subnormal negative number is conditioned to minus zero
    --
    -- The unconditioned result is the subnormal of largest magnitude
  , ( minNormal
    , -minNormal - maxDenormal
    , -0
    )
    -- The unconditioned result is the subnormal of smallest magnitude
  , ( minNormal
    , -minNormal - minDenormal
    , -0
    )
    -- Subnormals on input are conditioned to zero
    --
    -- The result would normally be the smallest normal number, but due to
    -- conditioning it is zero.
  , ( maxDenormal
    , minDenormal
    , 0
    )
    -- The result would normally be almost twice the smallest normal number,
    -- well within normal range, but due to conditioning it is again zero.
  , ( maxDenormal
    , maxDenormal
    , 0
    )
    -- The result would normally be exact, but the second input is conditioned
    -- to zero.
  , ( minNormal
    , minDenormal
    , minNormal
    )
    -- Idem dito.
  , ( minNormal
    , maxDenormal
    , minNormal
    )
    -- Subnormals on input are conditioned to zero, negative version
    --
    -- The result would normally be the normal number of smallest magnitude,
    -- but due to conditioning it is zero.
  , ( -maxDenormal
    , -minDenormal
    , -0
    )
    -- The result would normally be almost twice the normal number of smallest
    -- magnitude, well within normal range, but due to conditioning it is again
    -- zero.
  , ( -maxDenormal
    , -maxDenormal
    , -0
    )
    -- The result would normally be exact, but the second input is conditioned
    -- to zero.
  , ( -minNormal
    , -minDenormal
    , -minNormal
    )
    -- Idem dito.
  , ( -minNormal
    , -maxDenormal
    , -minNormal
    )
    -- Round to nearest
    --
    -- For a datatype with 4 bits of precision, the significands align as:
    -- 1000
    --     1001
    -- -------- +
    -- 1001
  , ( 2 ^ (digits - 1)
    , encodeFloat (2 ^ (digits - 1) + 1) (-digits)
    , 2 ^ (digits - 1) + 1
    )
    -- 1000
    --     01111
    -- --------- +
    -- 1000
  , ( 2 ^ (digits - 1)
    , encodeFloat (2 ^ digits - 1) (-digits - 1)
    , 2 ^ (digits - 1)
    )
    -- Ties to even
    --
    -- 1000
    --     1000
    -- -------- +
    -- 1000
  , ( 2 ^ (digits - 1)
    , encodeFloat (2 ^ (digits - 1)) (-digits)
    , 2 ^ (digits - 1)
    )
    -- Round to nearest
    --
    -- For a datatype with 4 bits of precision, the significands align as:
    -- 1001
    --     1001
    -- -------- +
    -- 1010
  , ( 2 ^ (digits - 1) + 1
    , encodeFloat (2 ^ (digits - 1) + 1) (-digits)
    , 2 ^ (digits - 1) + 2
    )
    -- 1001
    --     01111
    -- --------- +
    -- 1001
  , ( 2 ^ (digits - 1) + 1
    , encodeFloat (2 ^ digits - 1) (-digits - 1)
    , 2 ^ (digits - 1) + 1
    )
    -- Ties to even
    --
    -- 1001
    --     1000
    -- -------- +
    -- 1010
  , ( 2 ^ (digits - 1) + 1
    , encodeFloat (2 ^ (digits - 1)) (-digits)
    , 2 ^ (digits - 1) + 2
    )
    -- Rounding at maximum exponent
    --
    -- 1111
    --     1000
    -- -------- +
    -- infinity
  , ( maxFinite
    , encodeFloat (2 ^ (digits - 1)) (maxExp - 2*digits)
    , 1/0
    )
    -- 1111
    --     01111
    -- --------- +
    -- 1111
  , ( maxFinite
    , encodeFloat (2 ^ digits - 1) (maxExp - 2*digits - 1)
    , encodeFloat (2 ^ digits - 1) (maxExp - digits)
    )
    -- Infinities
  , (infinity, 1, infinity)
  , (-infinity, 1, -infinity)
  , (infinity, -infinity, F.xilinxNaN)
  ]
  ++ cartesianProductTest model interesting interesting
  ++ nanTest
 where
  digits = floatDigits (undefined :: Float)
  (_, maxExp) = floatRange (undefined :: Float)
  model (conditionFloat -> x) (conditionFloat -> y) = conditionFloat $ x + y

cartesianProductTest
  :: (a -> b -> c)
  -> [a]
  -> [b]
  -> [(a,b,c)]
cartesianProductTest f as bs =
  map (\(a,b) -> (a, b, f a b)) $ cartesianProduct as bs

cartesianProduct
  :: [a]
  -> [b]
  -> [(a,b)]
cartesianProduct as bs =
  concatMap (\a -> map (\b -> (a,b)) bs) as

interesting :: [Float]
interesting =
  [ infinity
  , minDenormal
  , maxDenormal
  , minNormal
  , maxFinite
  , epsilon
  , F.xilinxNaN
    -- Some basic numbers
  , 1
  , 2
  , 42
  ]

nanTest :: [(Float, Float, Float)]
nanTest =
  concatMap testNaN
    [   qNaN0PL
    , negQNaN0PL
    , qNaN1
    , negQNaN1
    , sNaN1
    , negSNaN1
    , qNaNMsb
    , negQNaNMsb
    , sNaNMsb
    , negSNaNMsb
    , qNaNMax
    , negQNaNMax
    , sNaNMax
    , negSNaNMax
    , qNaNR1
    , negQNaNR1
    , sNaNR1
    , negSNaNR1
    , qNaNR2
    , negQNaNR2
    , sNaNR2
    , negSNaNR2
    ]
 where
  testNaN :: Float -> [(Float, Float, Float)]
  testNaN nan =
    [ (nan, 1, F.xilinxNaN)
    , (1, nan, F.xilinxNaN)
    , (nan, nan, F.xilinxNaN)
    ]

-- Maximum subnormal value
maxDenormal :: Float
maxDenormal = minNormal - minDenormal

-- Quiet NaN with no payload
-- Actually, this is equal to F.xilinxNaN
qNaN0PL :: Float
qNaN0PL = unpack 0b0111_1111_1100_0000_0000_0000_0000_0000

-- Negative version
negQNaN0PL :: Float
negQNaN0PL = unpack 0b1111_1111_1100_0000_0000_0000_0000_0000

-- Quiet NaN with payload 1
qNaN1 :: Float
qNaN1 = unpack 0b0111_1111_1100_0000_0000_0000_0000_0001

-- Negative version
negQNaN1 :: Float
negQNaN1 = unpack 0b1111_1111_1100_0000_0000_0000_0000_0001

-- Signaling NaN with payload 1
sNaN1 :: Float
sNaN1 = unpack 0b0111_1111_1000_0000_0000_0000_0000_0001

-- Negative version
negSNaN1 :: Float
negSNaN1 = unpack 0b1111_1111_1000_0000_0000_0000_0000_0001

-- Quiet NaN with payload with only MSB set
qNaNMsb :: Float
qNaNMsb = unpack 0b0111_1111_1110_0000_0000_0000_0000_0000

-- Negative version
negQNaNMsb :: Float
negQNaNMsb = unpack 0b1111_1111_1110_0000_0000_0000_0000_0000

-- Signaling NaN with payload with only MSB set
sNaNMsb :: Float
sNaNMsb = unpack 0b0111_1111_1010_0000_0000_0000_0000_0000

-- Negative version
negSNaNMsb :: Float
negSNaNMsb = unpack 0b1111_1111_1010_0000_0000_0000_0000_0000

-- Quiet NaN with maximum-valued payload
qNaNMax :: Float
qNaNMax = unpack 0b0111_1111_1111_1111_1111_1111_1111_1111

-- Negative version
negQNaNMax :: Float
negQNaNMax = unpack 0b1111_1111_1111_1111_1111_1111_1111_1111

-- Signaling NaN with maximum-valued payload
sNaNMax :: Float
sNaNMax = unpack 0b0111_1111_1011_1111_1111_1111_1111_1111

-- Negative version
negSNaNMax :: Float
negSNaNMax = unpack 0b1111_1111_1011_1111_1111_1111_1111_1111

-- Quiet NaN with random payload
qNaNR1 :: Float
qNaNR1 = unpack 0b0111_1111_1110_0000_1011_0001_0011_1100

-- Negative version
negQNaNR1 :: Float
negQNaNR1 = unpack 0b1111_1111_1110_0000_1011_0001_0011_1100

-- Signaling NaN with random payload
sNaNR1 :: Float
sNaNR1 = unpack 0b0111_1111_1010_0000_1011_0001_0011_1100

-- Negative version
negSNaNR1 :: Float
negSNaNR1 = unpack 0b1111_1111_1010_0000_1011_0001_0011_1100

-- Quiet NaN with random payload
qNaNR2 :: Float
qNaNR2 = unpack 0b0111_1111_1100_0010_0011_0000_1110_0101

-- Negative version
negQNaNR2 :: Float
negQNaNR2 = unpack 0b1111_1111_1100_0010_0011_0000_1110_0101

-- Signaling NaN with random payload
sNaNR2 :: Float
sNaNR2 = unpack 0b0111_1111_1000_0010_0011_0000_1110_0101

-- Negative version
negSNaNR2 :: Float
negSNaNR2 = unpack 0b1111_1111_1000_0010_0011_0000_1110_0101
