{-# LANGUAGE TypeApplications #-}

module Hedgehog.Extra
  ( throwsException
  , throwsDeepException
  ) where

import Control.DeepSeq (NFData, force)

import Hedgehog (failure, MonadTest, success)
import Hedgehog.Internal.Exception (tryEvaluate)
import Hedgehog.Internal.Source (HasCallStack, withFrozenCallStack)

throwsException
  :: ( MonadTest m
     , HasCallStack
     )
  => a
  -> m ()
throwsException x =
  case (tryEvaluate x) of
    Left _  -> success
    Right _ -> withFrozenCallStack failure

throwsDeepException
  :: ( MonadTest m
     , NFData a
     , HasCallStack
     )
  => a
  -> m ()
throwsDeepException =
  throwsException . force
