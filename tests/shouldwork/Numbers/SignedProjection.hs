{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- Test for https://github.com/clash-lang/clash-compiler/issues/601
-- part of SignedProjectionTB.hs
module SignedProjection where
import qualified Prelude
import Clash.Prelude

data Complex a = a :+ a deriving (Show, Lift, Generic, ShowX, NFDataX, Functor)

realPart :: Complex a -> a
realPart (x :+ _) = x

imagPart :: Complex a -> a
imagPart (_ :+ y) = y

top
  :: SystemClockResetEnable
  => Signal System (Complex (Signed 3))
  -> Signal System (Signed 4)
top = minimal

minimal
  :: KnownNat b
  => Signal dom (Complex (Signed b))
  -> Signal dom (Signed (b+1))
minimal c = liftA2 add (realPart <$> c) (imagPart <$> c)

input :: Vec _ (Complex (Signed 3))
input = fmap (uncurry (:+)) $(listToVecTH [(a,b) :: (Signed 3,Signed 3) | a <- [minBound..maxBound], b <- [minBound..maxBound]])
