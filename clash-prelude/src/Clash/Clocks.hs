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

import Clash.Clocks.Deriving
  (Clocks(..), ClocksSync(..), deriveClocksInstances, deriveClocksSyncInstances)
import Clash.Signal.Internal (Domain, KnownDomain)

deriveClocksInstances 16

type ClocksSyncCxt t (domIn :: Domain) =
  ( KnownDomain domIn
  , ClocksSync t
  , ClocksResetSynchronizerCxt t
  , Clocks (ClocksSyncClocksInst t domIn)
  , ClocksCxt (ClocksSyncClocksInst t domIn)
  )

deriveClocksSyncInstances 16
