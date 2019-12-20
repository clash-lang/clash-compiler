{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}


{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Integral where
import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Int
import Data.Word

test :: forall a. Integral a => Integer -> Integer -> _
test (fromInteger -> x) (fromInteger -> y) =
  ( quot @a x y
  , rem @a x y
  , div @a x y
  , mod @a x y
  , quotRem @a x y
  , divMod @a x y
  )

topEntity :: (Integer, Integer) -> _
topEntity (x,y) =
  ( ( test @Integer x y
    , test @Int x y
    , test @Int8 x y
    , test @Int16 x y
    , test @Int32 x y
    , test @Int64 x y
    )
  , ( test @Word x y
    , test @Word8 x y
    , test @Word16 x y
    , test @Word32 x y
    , test @Word64 x y
    )
  , ( test @(Signed 8) x y
    , test @(Unsigned 8) x y
    , test @(BitVector 8) x y
    , test @(Index 128) (abs x) (abs y)
    )
  )
{-# NOINLINE topEntity #-}

inputs :: Vec _ (Integer,Integer)
inputs =
  $(let range = [1,2,4,8,9,127,-1,-2,-3,-8,-9,-127] :: [Integer]
    in listToVecTH [(a,b) | a <- range, b <- range, b /= 0])

-- Lift for 8-15 tuples
deriving instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g, Lift h)
      => Lift (a,b,c,d,e,f,g,h)
deriving instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g, Lift h, Lift i)
      => Lift (a,b,c,d,e,f,g,h,i)
deriving instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g, Lift h, Lift i, Lift j)
      => Lift (a,b,c,d,e,f,g,h,i,j)
deriving instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g, Lift h, Lift i, Lift j, Lift k)
      => Lift (a,b,c,d,e,f,g,h,i,j,k)
deriving instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g, Lift h, Lift i, Lift j, Lift k, Lift l)
      => Lift (a,b,c,d,e,f,g,h,i,j,k,l)

