{-|
Copyright:    (C) 2023 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>

Use generators to create signal data.
-}

{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Clash.Testbench.Generate where

import Hedgehog
import Hedgehog.Gen
import Control.Monad.State.Lazy (liftIO, when, modify)
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
  Gen a -> TB (TBSignal dom a)
generate gen = do
  TBDomain{..} <- tbDomain @dom

  vRef <- liftIO $ newIORef undefined
  checkForProgress <- progressCheck simStepRef True
  signalHistory <- newHistory

  mind SomeSignal IOInput
    { signalId     = NoID
    , signalCurVal = const $ do
        progress <- checkForProgress

        if progress
        then do
          x <- sample gen
          writeIORef vRef x
          memorize signalHistory x
          return x
        else
          readIORef vRef
    , signalPrint  = Nothing
    ,..
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
  checkForProgress <- progressCheck simStepRef False
  signalHistory <- newHistory

  mind SomeSignal IOInput
    { signalId     = NoID
    , signalCurVal = const $ do
        progress <- checkForProgress

        if progress
        then
          readIORef vRef >>= \case
            h : x : xr -> do
              memorize signalHistory h
              writeIORef vRef (x : xr)
              return x
            [h] -> do
              memorize signalHistory h
              x : xr <- sample gen
              writeIORef vRef (x : xr)
              return x
            _ -> error "unreachable"
        else readIORef vRef >>= \case
          x : _ -> return x
          [] -> do
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
  checkForProgress <- progressCheck simStepRef False
  signalHistory <- newHistory

  mind SomeSignal $ IOInput
    { signalId     = NoID
    , signalCurVal = const $ do
        progress <- checkForProgress

        if progress
        then do
          (i, o) <- sample gen
          curStep <- readIORef simStepRef
          signalExpect expectedOutput $ Expectation (curStep, verify o)
          writeIORef vRef i

          return i
        else
          readIORef vRef
    , signalPrint  = Nothing
    , ..
    }

 where
  verify x y = do
    when (x /= y)
      $ footnote
      $ "Expected '" <> show x <> "' but the output is '" <> show y <> "'"
    x === x

-- | Extended version of 'matchIOGen', which allows to specify valid
-- IO behavior over a finite amount of simulation steps. During native
-- simulation (no property check), the generator is repeatedly called
-- after all the generated simulation steps have been consumed. The
-- generator is only called once if the test bench is converted to a
-- property instead.
matchIOGenN ::
  forall dom i o.
  (NFDataX i, BitPack i, KnownDomain dom, Eq o, Show o, Show i) =>
  TBSignal dom o -> Gen [(i, o)] -> TB (TBSignal dom i)
matchIOGenN checkedOutput gen = mdo
  TBDomain{..} <- tbDomain @dom

  xs <- liftIO $ sample gen
  modify $ \st@ST{..} -> st { simSteps = max simSteps $ length xs }
  liftIO $ Prelude.print xs

  vRef <- liftIO $ newIORef xs
  checkForProgress <- progressCheck simStepRef False
  signalHistory <- newHistory

  s <- mind SomeSignal $ IOInput
    { signalId = NoID
    , signalCurVal = const $ do
        progress <- checkForProgress

        readIORef vRef >>=
          if progress
          then \case
            (h, _) : (i, o) : xr -> do
              memorize signalHistory h
              writeIORef vRef ((i, o) : xr)
              curStep <- readIORef simStepRef
              signalExpect checkedOutput $ Expectation (curStep, verify s i o)
              return i
            [(h, _)] -> do
              memorize signalHistory h
              (i, o) : xr <- sample gen

              writeIORef vRef ((i, o) : xr)
              curStep <- readIORef simStepRef
              signalExpect checkedOutput $ Expectation (curStep, verify s i o)
              return i
            _ -> error "unreachable"
          else \case
            (i, _) : _ -> return i
            [] -> do
              (i, o) : xr <- sample gen
              writeIORef vRef ((i, o) : xr)
              Prelude.print $ (i, o) : xr
              return i
    , signalPrint = Nothing
    , ..
    }

  return s

 where
  verify generatedInput currentInput expectedOutput observedOutput = do
    when (expectedOutput /= observedOutput) $ do
      xs <-
        (<> [(currentInput, observedOutput)])
          <$> (zip <$> history generatedInput <*> history checkedOutput)

      let
        cHeading = "Cycle"
        iHeading = "Input"
        oHeading = "Output"
        cLen = length cHeading
        iLen = maximum $ (length iHeading :) $ fmap (length . show . fst) xs
        oLen = maximum $ (length oHeading :) $ fmap (length . show . snd) xs

      footnote $ unlines $
        [ "Expected to see the output '" <> show expectedOutput <> "',"
        , "but the observed output is '" <> show observedOutput <> "'."
        , ""
        , "I/O History:"
        , ""
        , cHeading <>
          replicate (iLen - length iHeading + 2) ' ' <> iHeading <>
          replicate (oLen - length oHeading + 2) ' ' <> oHeading
        , replicate (cLen + iLen + oLen + 4) '-'
        ] <>
        [ replicate (cLen - length (show c))     ' ' <> show c <>
          replicate (iLen - length (show i) + 2) ' ' <> show i <>
          replicate (oLen - length (show o) + 2) ' ' <> show o
        | (c, (i, o)) <- zip [0 :: Int,1..] xs
        ]

      failure
