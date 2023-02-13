{-|
Copyright  :  (C) 2018, Google Inc
                  2019, Myrtle Software Ltd
                  2023,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Generic clock related utilities.
-}

{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC "-Wno-orphans" #-}

module Clash.Clocks (Clocks(..), ClocksSynchronizedReset(..)) where

import Data.Kind (Constraint, Type)
import Unsafe.Coerce (unsafeCoerce)

import Clash.Explicit.Reset (resetSynchronizer)
import Clash.Explicit.Signal (unsafeSynchronizer)
import Clash.Signal.Internal
-- import Clash.Clocks.Deriving (Clocks(..), deriveClocksInstances)
import Clash.Clocks.Deriving (Clocks(..))



-- deriveClocksInstances 16
instance Clocks (Clock c1, Signal pllLock Bool) where
  type ClocksCxt (Clock c1, Signal pllLock Bool) =
    (KnownDomain c1, KnownDomain pllLock)

  clocks clk@(Clock _ Nothing) rst =
    ( unsafeCoerce clk
    , unsafeSynchronizer clockGen clockGen $ unsafeToLowPolarity rst
    )
  clocks _ _ = error "clocks: dynamic clocks unsupported"

instance Clocks (Clock c1, Clock c2, Clock c3, Signal pllLock Bool) where
  type ClocksCxt (Clock c1, Clock c2, Clock c3, Signal pllLock Bool) =
    (KnownDomain c1, KnownDomain c2, KnownDomain c3, KnownDomain pllLock)

  clocks clk@(Clock _ Nothing) rst =
    ( unsafeCoerce clk
    , unsafeCoerce clk
    , unsafeCoerce clk
    , unsafeSynchronizer clockGen clockGen $ unsafeToLowPolarity rst
    )
  clocks _ _ = error "clocks: dynamic clocks unsupported"

class ClocksSynchronizedReset t where
  type ClocksSynchronizedResetCxt t :: Constraint

  type ClocksSynchronizedResetInput t :: Type

  clocksSynchronizedReset
    :: ClocksSynchronizedResetCxt t
    => ClocksSynchronizedResetInput t
    -> t

instance ClocksSynchronizedReset (Clock domIn -> Reset domIn -> (Clock c1, Reset c1)) where

  type ClocksSynchronizedResetCxt (Clock domIn -> Reset domIn -> (Clock c1, Reset c1)) =
    (KnownDomain c1, KnownDomain domIn)

  type ClocksSynchronizedResetInput (Clock domIn -> Reset domIn -> (Clock c1, Reset c1)) =
    (Clock domIn -> Reset domIn -> (Clock c1, Signal domIn Bool))

  clocksSynchronizedReset pll clkIn rstIn =
    let (c1, pllLock) = pll clkIn rstIn
    in ( c1
       , resetSynchronizer c1 $
           unsafeFromLowPolarity $ unsafeSynchronizer clkIn c1 pllLock
       )

instance ClocksSynchronizedReset (Clock domIn -> Reset domIn -> (Clock c1, Reset c1, Clock c2, Reset c2, Clock c3, Reset c3)) where

  type ClocksSynchronizedResetCxt (Clock domIn -> Reset domIn -> (Clock c1, Reset c1, Clock c2, Reset c2, Clock c3, Reset c3)) =
    (KnownDomain c1, KnownDomain c2, KnownDomain c3, KnownDomain domIn)

  type ClocksSynchronizedResetInput (Clock domIn -> Reset domIn -> (Clock c1, Reset c1, Clock c2, Reset c2, Clock c3, Reset c3)) =
    (Clock domIn -> Reset domIn -> (Clock c1, Clock c2, Clock c3, Signal domIn Bool))

  clocksSynchronizedReset pll clkIn rstIn =
    let (c1, c2, c3, pllLock) = pll clkIn rstIn
    in ( c1
       , resetSynchronizer c1 $
           unsafeFromLowPolarity $ unsafeSynchronizer clkIn c1 pllLock
       , c2
       , resetSynchronizer c2 $
           unsafeFromLowPolarity $ unsafeSynchronizer clkIn c2 pllLock
       , c3
       , resetSynchronizer c3 $
           unsafeFromLowPolarity $ unsafeSynchronizer clkIn c3 pllLock
       )
