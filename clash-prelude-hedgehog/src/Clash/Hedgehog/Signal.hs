{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of types from "Clash.Signal.Internal".
-}

{-# LANGUAGE CPP #-}

module Clash.Hedgehog.Signal
  ( genSignal
  , genActiveEdge
  , genInitBehavior
  , genResetKind
  , genResetPolarity
  ) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen

import Clash.Signal.Internal

genSignal :: (MonadGen m) => m a -> m (Signal dom a)
genSignal genElem = liftA2 (:-) genElem (genSignal genElem)

genActiveEdge :: (MonadGen m) => m ActiveEdge
genActiveEdge = Gen.element [Rising, Falling]

genInitBehavior :: (MonadGen m) => m InitBehavior
genInitBehavior = Gen.element [Unknown, Defined]

genResetKind :: (MonadGen m) => m ResetKind
genResetKind = Gen.element [Asynchronous, Synchronous]

genResetPolarity :: (MonadGen m) => m ResetPolarity
genResetPolarity = Gen.element [ActiveHigh, ActiveLow]

-- TODO Generate a full domain configuration.
-- We probably want some type like SomeSDomainConfiguration to generate GADTs
