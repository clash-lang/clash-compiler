{-|
Copyright:    (C) 2023 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>

Input sources for simulating 'Clash.Testbench.Simulate.TB' defined
test benches.
-}
module Clash.Testbench.Input
  ( fromList
  ) where

import Control.Monad.State.Lazy
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.List (uncons)

import Clash.Prelude (KnownDomain(..), BitPack(..), NFDataX)

import Clash.Testbench.Signal (TBSignal)
import Clash.Testbench.Internal.Signal hiding (TBSignal)
import Clash.Testbench.Internal.Monad
import Clash.Testbench.Internal.ID

-- | Creates an input signal whose values are taken from a finite or
-- infinite list. If the list is finite and the number of simulation
-- steps exceeds the length of the list, then the value of the first
-- argument is used repeatedly.
fromList :: forall dom a.
  (KnownDomain dom, BitPack a, NFDataX a, Show a) =>
  a -> [a] -> TB (TBSignal dom a)
fromList x xs = do
  TBDomain{..} <- tbDomain @dom

  listRef <- liftIO $ newIORef $ x : xs
  simStepCache <- liftIO (readIORef simStepRef >>= newIORef)

  mind SomeSignal $ IOInput
    { signalId     = NoID
    , signalPrint  = Nothing
    , signalCurVal = const $ do
        (r, rs) <- fromMaybe (x, []) . uncons <$> readIORef listRef
        global <- readIORef simStepRef
        local <- readIORef simStepCache

        if local == global
        then return r
        else do
          writeIORef listRef rs
          writeIORef simStepCache global
          return $ case rs of
            []  -> x
            y:_ -> y
    }
