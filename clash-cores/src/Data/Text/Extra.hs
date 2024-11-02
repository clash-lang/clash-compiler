module Data.Text.Extra
  ( showt
  , showtl
  ) where

import Prelude
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

showt :: (Show a) => a -> TS.Text
showt = TS.pack . show

showtl :: (Show a) => a -> TL.Text
showtl = TL.pack . show
