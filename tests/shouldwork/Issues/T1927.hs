{-# LANGUAGE AllowAmbiguousTypes #-}
module T1927 where

import Clash.Prelude
import Control.Monad

topEntity
  :: Signal System (Vec 1 (Signed 1))
  -> Signal System (Maybe (Vec 1 (Signed 1, Signed 1)))
  -> Clock System -> Reset System -> Enable System
  -> Signal System (Maybe (Vec 1 (Signed 1, Signed 1)))
topEntity angles xs c r e = exposeClockResetEnable (machine @8 angles xs) c r e

machine
  :: forall numStages pairs coord angle dom counter
   . HiddenClockResetEnable dom
  => (1 <= numStages)
  => (KnownNat numStages, KnownNat pairs)
  => (Default coord, NFDataX coord)
  => (Default angle, NFDataX angle)
  => counter ~ (Index numStages)
  => Signal dom (Vec pairs angle)
  -> Signal dom (Maybe (Vec pairs (coord, coord)))
  -> Signal dom (Maybe (Vec pairs (coord, coord)))
machine thetas coords = mealy transition def $ liftA2 (liftM2 zip) coords (return <$> thetas)
  where
    transition :: (counter, Vec pairs ((coord, coord), angle))
               -> Maybe (Vec pairs ((coord, coord), angle))
               -> ( (counter, Vec pairs ((coord, coord), angle))
                  , Maybe (Vec pairs (coord, coord))
                  )
    transition (k, xyzs) maybeInputs = case (k == 0, maybeInputs)
                                         of (True, Nothing) -> ((0, xyzs), Just $ map fst xyzs)
                                            (True, Just xyzs0) -> ((0, xyzs0), Just $ map fst xyzs)
                                            (False, _) -> ((satSucc SatWrap k, xyzs), Nothing)
