{-|
Copyright:    (C) 2023 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>

Use generators to create signal data.
-}

module Clash.Testbench.Generate where

import Hedgehog
import Hedgehog.Gen
import Control.Monad.State.Lazy (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)

import Clash.Prelude (KnownDomain(..), BitPack(..), NFDataX)

import Clash.Testbench.Signal
import Clash.Testbench.Internal.ID
import Clash.Testbench.Internal.Signal hiding (TBSignal, TBClock, TBReset, TBEnable)
import Clash.Testbench.Internal.Monad

-- | Use a generator to create new signal data at every simulation
-- step.
generate ::
  forall dom a.
  (NFDataX a, BitPack a, KnownDomain dom) =>
  a -> Gen a -> TB (TBSignal dom a)
generate def gen = do
  TBDomain{..} <- tbDomain @dom

  vRef <- liftIO $ newIORef def
  simStepCache <- liftIO (readIORef simStepRef >>= newIORef)

  mind SomeSignal IOInput
    { signalId     = NoID
    , signalCurVal = const $ do
        v <- readIORef simStepRef
        v' <- readIORef simStepCache

        if v == v'
        then readIORef vRef
        else do
          x <- sample gen
          writeIORef vRef x
          writeIORef simStepCache v
          return x
    , signalPrint  = Nothing
    }

-- | Extended version of 'generate', which allows to generate a finite
-- sequence of data values, where one value is consumed per simulation
-- step. The generator is repeatedly called after all steps of a
-- generation has been consumed.
generateN ::
  forall dom a.
  (NFDataX a, BitPack a, KnownDomain dom) =>
  a -> Gen [a] -> TB (TBSignal dom a)
generateN def gen = do
  TBDomain{..} <- tbDomain @dom

  vRef <- liftIO $ newIORef [def]
  simStepCache <- liftIO (readIORef simStepRef >>= newIORef)

  mind SomeSignal IOInput
    { signalId     = NoID
    , signalCurVal = const $ do
        v <- readIORef simStepRef
        v' <- readIORef simStepCache

        if v == v'
        then readIORef vRef >>= \case
          x : _ -> return x
          [] -> do
            x : xr <- sample gen
            writeIORef vRef (x : xr)
            return x

        else do
          writeIORef simStepCache v
          readIORef vRef >>= \case
            _ : x : xr -> do
              writeIORef vRef (x : xr)
              return x
            _ -> do
              x : xr <- sample gen
              writeIORef vRef (x : xr)
              return x
    , signalPrint  = Nothing
    , ..
    }

-- | Use an input/output generator to describe an IO relation that
-- specifies valid behavior. The satisfaction of this relation is
-- automatically checked during simulation.
matchIOGen ::
  forall dom i o.
  (NFDataX i, BitPack i, KnownDomain dom, Eq o, Show o) =>
  TBSignal dom o -> Gen (i, o) -> TB (TBSignal dom i)
matchIOGen expectedOutput gen = do
  TBDomain{..} <- tbDomain @dom

  vRef <- liftIO $ newIORef undefined
  simStepCache <- liftIO (readIORef simStepRef >>= newIORef)

  mind SomeSignal $ IOInput
    { signalId     = NoID
    , signalCurVal = const $ do
        global <- readIORef simStepRef
        local <- readIORef simStepCache

        if local == global
        then readIORef vRef
        else do
          (i, o) <- sample gen
          signalExpect expectedOutput $ Expectation (global + 1, verify o)

          writeIORef vRef i
          writeIORef simStepCache global
          return i
    , signalPrint  = Nothing
    }
 where
  verify x y
    | x == y    = Nothing
    | otherwise = Just $ "Expected " <> show x <> " but the output is " <> show y

-- | Extended version of 'matchIOGen', which allows to specify valid
-- IO behavior over a finite amount of simulation steps. The generator
-- is repeatedly called after all steps of a generation have been
-- verified.
matchIOGenN ::
  forall dom i o.
  (NFDataX i, BitPack i, KnownDomain dom, Eq o, Show o, Show i) =>
  TBSignal dom o -> Gen [(i, o)] -> TB (TBSignal dom i)
matchIOGenN expectedOutput gen = do
  TBDomain{..} <- tbDomain @dom

  vRef <- liftIO $ newIORef []
  simStepCache <- liftIO (readIORef simStepRef >>= newIORef)

  mind SomeSignal $ IOInput
    { signalId     = NoID
    , signalCurVal = const $ do
        global <- readIORef simStepRef
        local <- readIORef simStepCache

        if local == global
        then readIORef vRef >>= \case
          (i, _) : _ -> return i
          [] -> do
            (i, o) : xr <- sample gen
            writeIORef vRef ((i, o) : xr)
            Prelude.print $ (i, o) : xr
            return i
        else do
          writeIORef simStepCache global
          readIORef vRef >>= \case
            _ : (i, o) : xr -> do
              writeIORef vRef ((i, o) : xr)
              signalExpect expectedOutput $ Expectation (global + 1, verify o)
              return i
            _ -> do
              (i, o) : xr <- sample gen
              Prelude.print $ (i, o) : xr
              writeIORef vRef ((i, o) : xr)
              signalExpect expectedOutput $ Expectation (global + 1, verify o)
              return i
    , signalPrint  = Nothing
    }
 where
  verify x y
    | x == y    = Nothing
    | otherwise = Just $ "Expected '" <> show x <> "' but the output is '" <> show y <> "'"
