{-|
Copyright  :  (C) 2021-2022, QBayLogic B.V.,
                  2022     , Google Inc.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE ViewPatterns #-}

module Floating.TH where

import Clash.Prelude (natToNum, unpack, Unsigned)

import Prelude
import Numeric.IEEE
  (epsilon, infinity, maxFinite, minDenormal, minNormal)

import Clash.Cores.Xilinx.Floating as F
import Clash.Cores.Xilinx.Floating.Internal as F

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
  ]
  ++ addSubBasicSamples
  ++ cartesianProductTest model interesting interesting
  ++ nanTest
 where
  model (conditionFloat -> x) (conditionFloat -> y) = conditionFloat $ x + y

subBasicSamples :: [(Float, Float, Float)]
subBasicSamples =
  delayOutput (natToNum @F.SubDefDelay) $
  [ (1, 6, -5)
  , (2, 5, -3)
  , (3, 4, -1)
  ]
  ++ map (\(a,b,c) -> (a, negate b, c)) addSubBasicSamples
  ++ cartesianProductTest model interesting interesting
  ++ nanTest
 where
  model (conditionFloat -> x) (conditionFloat -> y) = conditionFloat $ x - y

addSubBasicSamples :: [(Float, Float, Float)]
addSubBasicSamples =
  [ -- Subnormal positive number is conditioned to plus zero
    --
    -- The unconditioned result is the subnormal of largest magnitude
    ( -minNormal
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
    -- The result would normally be exact, but the first input is conditioned
    -- to zero.
  , ( maxDenormal
    , minNormal
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
    -- The result would normally be exact, but the first input is conditioned
    -- to zero.
  , ( -maxDenormal
    , -minNormal
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
    , infinity
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
  , (infinity, -maxFinite, infinity)
  , (-infinity, maxFinite, -infinity)
  , (infinity, -infinity, F.xilinxNaN)
  ]
 where
  digits = floatDigits (undefined :: Float)
  (_, maxExp) = floatRange (undefined :: Float)

mulBasicSamples :: [(Float, Float, Float)]
mulBasicSamples =
  delayOutput (natToNum @F.MulDefDelay) $
  [ (1, 4, 4)
  , (2, 5, 10)
  , (3, 6, 18)
    -- Subnormal positive number is conditioned to plus zero
    --
    -- The unconditioned result is the subnormal of largest magnitude
  , ( 1/2
    , encodeFloat (2 ^ digits - 2) (minExp - digits)
    , 0
    )
    -- The unconditioned result is the subnormal of smallest magnitude
  , ( encodeFloat 1 (1 - digits)
    , minNormal
    , 0
    )
    -- Subnormal negative number is conditioned to minus zero
    --
    -- The unconditioned result is the subnormal of largest magnitude
  , ( -1/2
    , encodeFloat (2 ^ digits - 2) (minExp - digits)
    , -0
    )
    -- The unconditioned result is the subnormal of smallest magnitude
  , ( encodeFloat 1 (1 - digits)
    , -minNormal
    , -0
    )
    -- Subnormals on input are conditioned to zero
    --
    -- The result would normally be about four, but due to conditioning it is
    -- zero.
  , ( maxDenormal
    , maxFinite
    , 0
    )
    -- The result would normally be minNormal, but due to conditioning it is
    -- zero.
  , ( encodeFloat 1 (digits - 1)
    , minDenormal
    , 0
    )
    -- Subnormals on input are conditioned to zero, negative version
    --
    --
    -- The result would normally be about -4, but due to conditioning it is
    -- zero.
  , ( -maxDenormal
    , maxFinite
    , -0
    )
    -- The result would normally be -minNormal, but due to conditioning it is
    -- zero.
  , ( encodeFloat 1 (digits - 1)
    , -minDenormal
    , -0
    )
    -- Round to nearest
    --
    -- A small program has been used to determine the two numbers to be
    -- multiplied such that they lead to the desired product. For ease of
    -- comprehension, the result is shown in the comments in two formats.
    --
    -- First, in an easily read format: as if it were the 8-bit result of a
    -- product of two 4-bit mantissa's, to show the desired rounding (cf.
    -- comments in addSubBasicSamples).
    --
    -- If the structure of the full result is exactly equal to the ideal 8-bit
    -- result, that is all. However, if the structure is not ideal, the 8-bit
    -- result is shown with variable placeholders, and there, let XX = xx + 1.
    --
    -- In addition, the precise result is shown for completenes in this case.
    --
    -- 1xx0 1001
    -- -------- round
    -- 1xx1
    --
    -- 0b1000_0000_0000_0000_0000_0110_1000_0000_0000_0000_0000_0001
    -- ------------------------------------------------------------- round
    -- 0b1000_0000_0000_0000_0000_0111
    --
  , ( 14220287
    , 9896959
    , encodeFloat 0b1000_0000_0000_0000_0000_0111 digits
    )
    --
    -- 1000 0111
    -- --------- round
    -- 1000
    --
  , ( 10066329
    , 13981015
    , encodeFloat (2 ^ (digits - 1)) digits
    )
    -- Ties to even
    --
    -- 1000 1000
    -- --------- round
    -- 1000
  , ( 12713984
    , 11069504
    , encodeFloat (2 ^ (digits - 1)) digits
    )
    -- Round to nearest
    --
    -- 1xx1 1001
    -- --------- round
    -- 1XX0
    --
    -- 0b1000_0000_0000_0000_0000_1101_1000_0000_0000_0000_0000_0001
    -- ------------------------------------------------------------- round
    -- 0b1000_0000_0000_0000_0000_1110
    --
  , ( 12427923
    , 11324315
    , encodeFloat 0b1000_0000_0000_0000_0000_1110 digits
    )
    -- 1xx1 0111
    -- --------- round
    -- 1xx1
    --
    -- 0b1000_0000_0000_0000_0000_1001_0111_1111_1111_1111_1111_1111
    -- ------------------------------------------------------------- round
    -- 0b1000_0000_0000_0000_0000_1001
    --
  , ( 10837383
    , 12986313
    , encodeFloat 0b1000_0000_0000_0000_0000_1001 digits
    )
    -- Ties to even
    --
    -- 1001 1000
    -- --------- round
    -- 1010
  , ( 12689408
    , 11090944
    , encodeFloat (2 ^ (digits - 1) + 2) digits
    )
    -- Infinities
  , (infinity, minNormal, infinity)
  , (-infinity, minNormal, -infinity)
  , (infinity, -minNormal, -infinity)
  , (-infinity, -minNormal, infinity)
  , (infinity, 0, F.xilinxNaN)
  , (-infinity, 0, F.xilinxNaN)
  , (infinity, -0, F.xilinxNaN)
  , (-infinity, -0, F.xilinxNaN)
  ]
  ++ cartesianProductTest model interesting interesting
  ++ nanTest
 where
  digits = floatDigits (undefined :: Float)
  (minExp, _) = floatRange (undefined :: Float)
  model (conditionFloat -> x) (conditionFloat -> y) = conditionFloat $ x * y

divBasicSamples :: [(Float, Float, Float)]
divBasicSamples =
  delayOutput (natToNum @F.DivDefDelay) $
  [ (1, 2, 0.5)
  , (3, 4, 0.75)
  , (7, 8, 0.875)
    -- Subnormal positive number is conditioned to plus zero
    --
    -- The unconditioned result is the subnormal of largest magnitude
  , ( encodeFloat (2 ^ digits - 2) (1 - digits)
    , encodeFloat 1 (maxExp - 1)
    , 0
    )
    -- The unconditioned result is the subnormal of smallest magnitude
  , ( encodeFloat 2 (1 - digits)
    , encodeFloat 1 (maxExp - 1)
    , 0
    )
    -- Subnormal negative number is conditioned to minus zero
    --
    -- The unconditioned result is the subnormal of largest magnitude
  , ( -encodeFloat (2 ^ digits - 2) (1 - digits)
    , encodeFloat 1 (maxExp - 1)
    , -0
    )
    -- The unconditioned result is the subnormal of smallest magnitude
  , ( -encodeFloat 2 (1 - digits)
    , encodeFloat 1 (maxExp - 1)
    , -0
    )
    -- Subnormals on input are conditioned to zero
    --
    -- The result would normally be about one, but due to conditioning it is
    -- zero.
  , ( maxDenormal
    , minNormal
    , 0
    )
    -- The result would normally be about maxFinite/2, but due to
    -- conditioning it is division by zero -> infinity.
  , ( encodeFloat 2 (1 - digits)
    , minDenormal
    , infinity
    )
    -- Subnormals on input are conditioned to zero, negative version
    --
    --
    -- The result would normally be about -1, but due to conditioning it is
    -- zero.
  , ( -maxDenormal
    , minNormal
    , -0
    )
    -- The result would normally be about -maxFinite/2, but due to
    -- conditioning it is division by zero -> negative infinity.
  , ( encodeFloat 2 (1 - digits)
    , -minDenormal
    , -infinity
    )
    -- Infinities
  , (infinity, maxFinite, infinity)
  , (-infinity, maxFinite, -infinity)
  , (infinity, -maxFinite, -infinity)
  , (-infinity, -maxFinite, infinity)
  , (1, 0, infinity)
  , (-1, 0, -infinity)
  , (1, -0, -infinity)
  , (-1, -0, infinity)
  , (infinity, infinity, F.xilinxNaN)
  , (-infinity, infinity, F.xilinxNaN)
  , (infinity, -infinity, F.xilinxNaN)
  , (-infinity, -infinity, F.xilinxNaN)
  ]
  ++ cartesianProductTest model interesting interesting
  ++ nanTest
 where
  digits = floatDigits (undefined :: Float)
  (_, maxExp) = floatRange (undefined :: Float)
  model (conditionFloat -> x) (conditionFloat -> y) = conditionFloat $ x / y

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
  , 0
  , 1
  , 2
  , 4
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

fromUBasicSamples :: [(Unsigned 32, Float)]
fromUBasicSamples = delayOutput0 (natToNum @F.FromU32DefDelay) $
  map (\x -> (x, fromIntegral x))
    [ 0
    , 1
    , maxBound

      -- Patterns 0xaa and 0x55, but treating the "sign bit" separately. Floats
      -- are stored in sign/magnitude form so signed and unsigned numbers are
      -- not interchangeable like with two's complement. All these numbers
      -- should be unsigned, verify that they are treated as such.
    , 0b0010_1010_1010_1010_1010_1010_1010_1010
    , 0b0101_0101_0101_0101_0101_0101_0101_0101
    , 0b1010_1010_1010_1010_1010_1010_1010_1010
    , 0b1101_0101_0101_0101_0101_0101_0101_0101

    , -- Longest exactly representable
      0b0000_0000_1111_1111_1111_1111_1111_1111

    , -- Smallest with rounding
      0b0000_0001_0000_0000_0000_0000_0000_0001

      -- More rounding tests
    , 0b0000_0001_0000_0000_0000_0000_0000_0011
    , 0b0000_0010_0000_0000_0000_0000_0000_0001
    ]
 where
  delayOutput0 d es = zip is os
   where
    (is0, os0) = unzip es
    is = is0 ++ repeat (last is0)
    os = replicate d 0 ++ os0

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
