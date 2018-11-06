module GADTExistential where

import           Clash.Prelude
import           Control.Lens
import           Data.Generics.Product
import           GHC.Generics

data Thing = Thing
 { a  :: Bool
 , a1 :: Bool
 } deriving Generic

topEntity :: Signal System Thing -> Signal System Thing
topEntity a = fmap (field @"a" %~ not) a
