{-# LANGUAGE TypeApplications #-}

module Hedgehog.Extra
  ( throwsException
  , throwsDeepException
  , LockstepWalk(..)
  , lockstepWalk
  , combinations
  ) where

import Control.DeepSeq (NFData, force)

import Hedgehog (failure, MonadGen, MonadTest, success)
import Hedgehog.Internal.Exception (tryEvaluate)
import Hedgehog.Internal.Source (HasCallStack, withFrozenCallStack)

import qualified Hedgehog.Gen as Gen

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

{- | Test individual samples of a list through 'lockstepWalk'

@m@ is expected to be a 'MonadTest' monad.

The function gets a boolean indicating if this is the last sample in the list, plus the
current sample. It should return the function to test the next sample with.
-}
newtype LockstepWalk m a
  = LockstepWalk {unLockstepWalk :: Bool -> a -> m (LockstepWalk m a)}

{- | Walk a lazy list testing multiple properties

For each sample, multiple properties are tested. This way multiple tests can share a
walk of the list instead of the usual order where the first test would evaluate the
list fully even before the second test runs.
-}
lockstepWalk ::
  forall m a.
  (MonadTest m) =>
  [LockstepWalk m a] ->
  [a] ->
  m ()
lockstepWalk _ [] = pure ()
lockstepWalk ts (a : as) = do
  ts0 <- sequence $ map (\t -> unLockstepWalk t (null as) a) ts
  lockstepWalk ts0 as

{- | Generate combinations of k elements from a list

The generator shrinks towards the start of the list.
-}

{-
To generate combinations, if we consider the first element, we note that we can either
pick it or skip it. From the n choose k combinations, there are (n-1) choose (k-1)
combinations where we picked the element, and (n-1) choose k combinations where we skipped
the element. So let's define

pick = (n-1) choose (k-1)
skip = (n-1) choose k

We only need to know the ratio pick:skip. Working out the binomial coefficients, we come
to the following equivalent ratio:

pick' = k
skip' = n-k
-}
combinations ::
  (MonadGen m) =>
  [a] ->
  Int ->
  m [a]
combinations es k
  | k < 0 = error $ "combinations: impossible, k < 0. k = " <> show k
  | otherwise = combinations0 n k es
 where
  n = length es
  combinations0 _ 0 _ = pure []
  combinations0 n0 k0 es0 | n0 == k0 = pure es0
  combinations0 n0 k0 (e : es0) =
    Gen.frequency
      [ (k0, (e :) <$> combinations0 (n0 - 1) (k0 - 1) es0)
      , (n0 - k0, combinations0 (n0 - 1) k0 es0)
      ]
  combinations0 _ _ [] =
    error $ "combinations: impossible, k > n. k = " <> show k <> ", n = " <> show n
