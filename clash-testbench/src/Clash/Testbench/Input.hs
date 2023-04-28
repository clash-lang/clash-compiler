{-|
Copyright:    (C) 2023 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>

Input sources for simulating 'TB' defined testbenches.
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

-- | Generates input that is taken from a finite or infinite list. If
-- the list is finite and the number of simulation steps exceeds the
-- length of the list, then the value of the first argument is
-- used instead.
fromList
  :: (KnownDomain dom, BitPack a, NFDataX a) => a -> [a] -> TB (TBSignal dom a)
fromList x xs = do
  ST{..} <- get

  listRef <- liftIO $ newIORef $ x : xs
  simStepCache <- liftIO (readIORef simStepRef >>= newIORef)

  mindSignal $ IOInput
    { signalId     = NoID
    , signalPrint  = Nothing
    , signalCurVal = do
        (r, rs) <- fromMaybe (x, []) . uncons <$> readIORef listRef
        v <- readIORef simStepRef
        v' <- readIORef simStepCache

        if v == v'
        then return r
        else do
          writeIORef listRef rs
          writeIORef simStepCache v
          return $ case rs of
            []  -> x
            y:_ -> y
    }
