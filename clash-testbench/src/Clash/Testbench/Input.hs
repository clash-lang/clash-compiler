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

import Clash.Prelude (KnownDomain(..), BitPack(..), NFDataX)

import Clash.Testbench.Signal (TBSignal)
import Clash.Testbench.Internal.Signal hiding (TBSignal)
import Clash.Testbench.Internal.Monad
import Clash.Testbench.Internal.ID

-- | The mode defines how to expand finite lists towards infinite
-- ones. If a list is already infinite, then it does not matter which
-- mode is chosen at this point.
data ExpansionMode a =
    Repeat
    -- ^ Repeat a finite list indefinitely. This mode causes an error
    -- if the list to be repeated is the empty list.
  | Default a
    -- ^ Repeat a given default value after the end of a finite list
    -- has been reached.
  | IsInfinite
    -- ^ The list has to be infinite. This mode causes an error if the
    -- end of a finite list is reached.

-- | Creates an input signal whose values are taken from a finite or
-- infinite list. If the list is finite and the number of simulation
-- steps exceeds the length of the list, then the value of the first
-- argument is used repeatedly.
fromList :: forall dom a.
  (KnownDomain dom, BitPack a, NFDataX a) =>
  ExpansionMode a -> [a] -> TB (TBSignal dom a)

fromList Repeat [] =
  error $ "Clash.Testbench.Input.fromList: "
       <> "The empty list cannot be repeated indefinitely."

fromList mode xs = do
  TBDomain{..} <- tbDomain @dom

  vRef <- liftIO $ newIORef xs
  checkForProgress <- progressCheck simStepRef False
  signalHistory <- newHistory

  let
    signalCurVal m = do
      x : xr <- readIORef vRef >>= return . \case
        [] -> case mode of
          Repeat     -> xs
          Default v  -> [v]
          IsInfinite -> error $ "Clash.Testbench.Input.fromList: "
                             <> "end of list reached"
        yr -> yr

      progress <- checkForProgress

      if progress
      then do
        memorize signalHistory x
        writeIORef vRef xr
        signalCurVal m
      else
        return x

  mind SomeSignal $ IOInput
    { signalId     = NoID
    , signalPrint  = Nothing
    , ..
    }
