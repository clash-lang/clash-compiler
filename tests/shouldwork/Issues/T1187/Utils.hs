{-# LANGUAGE ScopedTypeVariables #-}
module T1187.Utils
    ( (.==)

    , debounce

    , roundRobin

    , succIdx, moreIdx

    , mealyStateB
    ) where

import Clash.Prelude
import Data.Maybe (fromMaybe)
import Control.Monad.State
import T1187.Clock

unchanged :: (HiddenClockResetEnable dom, Eq a, NFDataX a) => a -> Signal dom a -> Signal dom Bool
unchanged x0 x = x .==. register x0 x

debounce
    :: forall ps a dom. (Eq a, NFDataX a, HiddenClockResetEnable dom, KnownNat (ClockDivider dom ps))
    => SNat ps -> a -> Signal dom a -> Signal dom a
debounce _ initial this = regEn initial stable this
  where
    counter = register (0 :: Index (ClockDivider dom ps)) counter'
    counter' = mux (unchanged initial this) counter 0
    stable = counter' .==. pure maxBound

roundRobin
    :: forall n dom. (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom Bool
    -> (Signal dom (Vec n Bool), Signal dom (Index n))
roundRobin _next = undefined

infix 4 .==
(.==) :: (Eq a, Functor f) => f a -> a -> f Bool
fx .== y = (== y) <$> fx

moreIdx :: (Eq a, Enum a, Bounded a) => a -> a
moreIdx = fromMaybe maxBound . succIdx

succIdx :: (Eq a, Enum a, Bounded a) => a -> Maybe a
succIdx x | x == maxBound = Nothing
          | otherwise = Just $ succ x

mealyState
   :: (HiddenClockResetEnable dom, NFDataX s)
   => (i -> State s o) -> s -> (Signal dom i -> Signal dom o)
mealyState f = mealy step
  where
    step s x = let (y, s') = runState (f x) s in (s', y)

mealyStateB
    :: (HiddenClockResetEnable dom, NFDataX s, Bundle i, Bundle o)
    => (i -> State s o) -> s -> (Unbundled dom i -> Unbundled dom o)
mealyStateB f s0 = unbundle . mealyState f s0 . bundle
