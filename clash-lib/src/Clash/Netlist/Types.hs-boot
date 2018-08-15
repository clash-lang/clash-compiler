{-|
  Copyright   :  (C) 2018, Google Inc
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

module Clash.Netlist.Types where

import Data.Text (Text)

type Identifier = Text

data HWType
data Declaration
data Component
data Expr
data BlackBox
