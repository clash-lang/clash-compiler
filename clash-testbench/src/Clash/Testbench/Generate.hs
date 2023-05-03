module Clash.Testbench.Generate where

import Hedgehog
import Hedgehog.Gen
import Control.Monad.State.Lazy (liftIO, get)
import Data.IORef (newIORef, readIORef, writeIORef)

import Clash.Prelude (KnownDomain(..), BitPack(..), NFDataX)

import Clash.Testbench.Signal
import Clash.Testbench.Internal.ID
import Clash.Testbench.Internal.Signal hiding (TBSignal, TBClock, TBReset, TBEnable)
import Clash.Testbench.Internal.Monad

matchIOGen ::
  (NFDataX i, BitPack i, KnownDomain dom, Eq o, Show o) =>
  TBSignal dom o -> Gen (i, o) -> TB (TBSignal dom i)
matchIOGen expectedOutput gen = do
  ST{..} <- get

  vRef <- liftIO $ newIORef undefined
  simStepCache <- liftIO (readIORef simStepRef >>= newIORef)

  mind SomeSignal $ IOInput
    { signalId     = NoID
    , signalCurVal = do
        v <- readIORef simStepRef
        v' <- readIORef simStepCache

        if v == v'
        then readIORef vRef
        else do
          (i, o) <- sample gen
          signalExpect expectedOutput $ Expectation (v + 1, verify o)

          writeIORef vRef i
          writeIORef simStepCache v
          return i
    , signalPrint  = Nothing
    }
 where
  verify x y
    | x == y    = Nothing
    | otherwise = Just $ "Expected " <> show x <> " but the output is " <> show y


matchIOGenN ::
  (NFDataX i, BitPack i, KnownDomain dom, Eq o, Show o) =>
  TBSignal dom o -> Gen [(i, o)] -> TB (TBSignal dom i)
matchIOGenN expectedOutput gen = do
  ST{..} <- get

  vRef <- liftIO $ newIORef []
  simStepCache <- liftIO (readIORef simStepRef >>= newIORef)

  mind SomeSignal $ IOInput
    { signalId     = NoID
    , signalCurVal = do
        v <- readIORef simStepRef
        v' <- readIORef simStepCache

        if v == v'
        then readIORef vRef >>= \case
          (i, _) : _ -> return i
          [] -> do
            (i, o) : xr <- sample gen
            writeIORef vRef ((i, o) : xr)
            return i
        else do
          writeIORef simStepCache v
          readIORef vRef >>= \case
            _ : (i, o) : xr -> do
              writeIORef vRef ((i, o) : xr)
              signalExpect expectedOutput $ Expectation (v, verify o)
              return i
            _ -> do
              (i, o) : xr <- sample gen
              writeIORef vRef ((i, o) : xr)
              signalExpect expectedOutput $ Expectation (v, verify o)
              return i
    , signalPrint  = Nothing
    }
 where
  verify x y
    | x == y    = Nothing
    | otherwise = Just $ "Expected '" <> show x <> "' but the output is '" <> show y <> "'"


generate ::
  (NFDataX a, BitPack a, KnownDomain dom) =>
  a -> Gen a -> TB (TBSignal dom a)
generate def gen = do
  ST{..} <- get

  vRef <- liftIO $ newIORef def
  simStepCache <- liftIO (readIORef simStepRef >>= newIORef)

  mind SomeSignal IOInput
    { signalId     = NoID
    , signalCurVal = do
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

generateN ::
  (NFDataX a, BitPack a, KnownDomain dom) =>
  a -> Gen [a] -> TB (TBSignal dom a)
generateN def gen = do
  ST{..} <- get

  vRef <- liftIO $ newIORef [def]
  simStepCache <- liftIO (readIORef simStepRef >>= newIORef)

  mindSignal IOInput
    { signalId     = NoID
    , signalCurVal = do
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
