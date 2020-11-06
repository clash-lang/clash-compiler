module Clash.Netlist.Id where

import {-# SOURCE #-} Clash.Netlist.Types (Identifier)
import Data.Text (Text)

toText :: Identifier -> Text
