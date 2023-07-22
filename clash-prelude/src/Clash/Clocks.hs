{-|
Copyright  :  (C) 2018, Google Inc
                  2019, Myrtle Software Ltd
                  2023,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Generic clock related utilities.
-}

{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC "-Wno-orphans" #-}

module Clash.Clocks (Clocks(..), ClocksSync(..), ClocksSyncCxt) where

import Data.Kind (Constraint, Type)

import Clash.Explicit.Reset (resetSynchronizer)
import Clash.Explicit.Signal (unsafeSynchronizer)
import Clash.Promoted.Symbol (SSymbol(..))
import Clash.Signal.Internal
-- import Clash.Clocks.Deriving (Clocks(..), deriveClocksInstances)
import Clash.Clocks.Deriving (Clocks(..))



-- deriveClocksInstances 16
instance Clocks (Clock c1, Signal pllLock Bool) where
  type ClocksCxt (Clock c1, Signal pllLock Bool) =
    (KnownDomain c1, KnownDomain pllLock)

  clocks (Clock _ Nothing) rst =
    ( Clock SSymbol Nothing
    , unsafeSynchronizer clockGen clockGen $ unsafeToActiveLow rst
    )
  clocks _ _ = error "clocks: dynamic clocks unsupported"

instance Clocks (Clock c1, Clock c2, Clock c3, Signal pllLock Bool) where
  type ClocksCxt (Clock c1, Clock c2, Clock c3, Signal pllLock Bool) =
    (KnownDomain c1, KnownDomain c2, KnownDomain c3, KnownDomain pllLock)

  clocks (Clock _ Nothing) rst =
    ( Clock SSymbol Nothing
    , Clock SSymbol Nothing
    , Clock SSymbol Nothing
    , unsafeSynchronizer clockGen clockGen $ unsafeToActiveLow rst
    )
  clocks _ _ = error "clocks: dynamic clocks unsupported"

type ClocksSyncCxt t =
  ( KnownDomain (ClocksSyncDomIn t)
  , ClocksSync t
  , ClocksResetSynchronizerCxt t
  , Clocks (ClocksSyncClocksInst t)
  , ClocksCxt (ClocksSyncClocksInst t)
  ,   ClocksResetSynchronizerInput t
    ~ (Clock (ClocksSyncDomIn t)-> Reset (ClocksSyncDomIn t)-> ClocksSyncClocksInst t)
  )

class ClocksSync t where
  type ClocksSyncClocksInst t :: Type

  type ClocksSyncDomIn t :: Domain

  type ClocksResetSynchronizerCxt t :: Constraint

  type ClocksResetSynchronizerInput t :: Type

  clocksResetSynchronizer
    :: ClocksResetSynchronizerCxt t
    => ClocksResetSynchronizerInput t
    -> t

instance ClocksSync (Clock domIn -> Reset domIn -> (Clock c1, Reset c1)) where

  type ClocksSyncClocksInst (Clock domIn -> Reset domIn -> (Clock c1, Reset c1)) =
    (Clock c1, Signal domIn Bool)

  type ClocksSyncDomIn (Clock domIn -> Reset domIn -> (Clock c1, Reset c1)) = domIn

  type ClocksResetSynchronizerCxt (Clock domIn -> Reset domIn -> (Clock c1, Reset c1)) =
    (KnownDomain c1, KnownDomain domIn)

  type ClocksResetSynchronizerInput (Clock domIn -> Reset domIn -> (Clock c1, Reset c1)) =
    (Clock domIn -> Reset domIn -> (Clock c1, Signal domIn Bool))

  clocksResetSynchronizer pll clkIn rstIn =
    let (c1, pllLock) = pll clkIn rstIn
    in ( c1
       , resetSynchronizer c1 $
           unsafeFromActiveLow $ unsafeSynchronizer clkIn c1 pllLock
       )

instance ClocksSync (Clock domIn -> Reset domIn -> (Clock c1, Reset c1, Clock c2, Reset c2, Clock c3, Reset c3)) where

  type ClocksSyncClocksInst (Clock domIn -> Reset domIn -> (Clock c1, Reset c1, Clock c2, Reset c2, Clock c3, Reset c3)) =
    (Clock c1, Clock c2, Clock c3, Signal domIn Bool)

  type ClocksSyncDomIn (Clock domIn -> Reset domIn -> (Clock c1, Reset c1, Clock c2, Reset c2, Clock c3, Reset c3)) = domIn

  type ClocksResetSynchronizerCxt (Clock domIn -> Reset domIn -> (Clock c1, Reset c1, Clock c2, Reset c2, Clock c3, Reset c3)) =
    (KnownDomain c1, KnownDomain c2, KnownDomain c3, KnownDomain domIn)

  type ClocksResetSynchronizerInput (Clock domIn -> Reset domIn -> (Clock c1, Reset c1, Clock c2, Reset c2, Clock c3, Reset c3)) =
    (Clock domIn -> Reset domIn -> (Clock c1, Clock c2, Clock c3, Signal domIn Bool))

  clocksResetSynchronizer pll clkIn rstIn =
    let (c1, c2, c3, pllLock) = pll clkIn rstIn
    in ( c1
       , resetSynchronizer c1 $
           unsafeFromActiveLow $ unsafeSynchronizer clkIn c1 pllLock
       , c2
       , resetSynchronizer c2 $
           unsafeFromActiveLow $ unsafeSynchronizer clkIn c2 pllLock
       , c3
       , resetSynchronizer c3 $
           unsafeFromActiveLow $ unsafeSynchronizer clkIn c3 pllLock
       )
