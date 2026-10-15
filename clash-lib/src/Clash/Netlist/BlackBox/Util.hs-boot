{-|
  Copyright   :  (C) 2019, Google Inc
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
module Clash.Netlist.BlackBox.Util where

import Clash.Backend (Backend)
import Clash.Netlist.BlackBox.Types (BlackBoxTemplate)
import Clash.Netlist.Types (BlackBoxContext)
import Control.Monad.State (State)
import Data.Text.Lazy (Text)

renderTemplate ::
  (Backend backend) =>
  -- | Context used to fill in the hole
  BlackBoxContext ->
  -- | Blackbox template
  BlackBoxTemplate ->
  State backend (Int -> Text)
