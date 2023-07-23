{-|
Copyright  :  (C) 2018, Google Inc
                  2019, Myrtle Software Ltd
                  2023,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Generic clock related utilities.
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC "-Wno-orphans" #-}

module Clash.Clocks (Clocks(..), ClocksSync(..), ClocksSyncCxt) where

import Clash.Explicit.Signal (unsafeSynchronizer)
import Clash.Promoted.Symbol (SSymbol(..))
import Clash.Signal.Internal
-- import Clash.Clocks.Deriving (Clocks(..), deriveClocksInstances)
import Clash.Clocks.Deriving




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

type ClocksSyncCxt t (domIn :: Domain) =
  ( KnownDomain domIn
  , ClocksSync t
  , ClocksResetSynchronizerCxt t
  , Clocks (ClocksSyncClocksInst t domIn)
  , ClocksCxt (ClocksSyncClocksInst t domIn)
  )

deriveClocksSyncInstance 1
deriveClocksSyncInstance 3
