{-|
Copyright:    (C) 2023 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>

Output processors for post-processing output that results from
simulating 'TB' defined testbenches.
-}
module Clash.Testbench.Output
  ( watch
  , watchWith
  ) where

import Control.Monad.State.Lazy (modify)
import Data.Set (lookupIndex, insert, deleteAt)

import Clash.Prelude (KnownDomain(..), BitPack(..), NFDataX)

import Clash.Testbench.Signal (TBSignal)
import Clash.Testbench.Internal.Signal hiding (TBSignal)
import Clash.Testbench.Internal.Monad

-- | Output the values of the given signal to @stdout@ during
-- simulation using the 'Show' implementation of @a@.
watch ::
  (KnownDomain dom, BitPack a, NFDataX a, Show a) =>
  TBSignal dom a -> TB ()
watch = watchWith show

-- | Output the values of the given signal to @stdout@ during
-- simulation using the provided 'String'-converter for @a@.
watchWith ::
  (KnownDomain dom, BitPack a, NFDataX a) =>
  (a -> String) -> TBSignal dom a -> TB ()
watchWith toStr tbs =
  modify $ \st@ST{..} ->
    st { signals = case lookupIndex tbs' signals of
           Nothing -> insert tbs' signals
           Just i  -> insert tbs' $ deleteAt i signals
       }
 where
   tbs' = SomeSignal $ tbs { signalPrint = Just toStr }
