{-|
  Copyright   :  (C) 2019, Google Inc
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.Netlist.BlackBox.Util where

import Data.Text.Lazy (Text)
import Control.Monad.State (State)
import Clash.Backend (Backend)
import Clash.Netlist.Types (BlackBoxContext)
import Clash.Netlist.BlackBox.Types (BlackBoxTemplate)

renderTemplate
  :: Backend backend
  => BlackBoxContext -- ^ Context used to fill in the hole
  -> BlackBoxTemplate -- ^ Blackbox template
  -> State backend (Int -> Text)
