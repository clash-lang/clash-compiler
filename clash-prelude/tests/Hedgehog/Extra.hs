module Hedgehog.Extra
  (throwsException) where

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
