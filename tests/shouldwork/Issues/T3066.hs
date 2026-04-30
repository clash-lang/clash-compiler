{-# LANGUAGE TemplateHaskell #-}
module T3066 where

import Clash.Prelude
import Clash.Annotations.TH (makeTopEntity)

-- makeTopEntity fails when result is wrapped in a newtype.
newtype N a = N a

data T =
  T { a :: "a" ::: Bool
    , b :: "b" ::: Bool
    }

topEntity :: "o" ::: N T
topEntity = N (T True True)

makeTopEntity 'topEntity
