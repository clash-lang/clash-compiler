module Clash.XException where

import Data.Kind (Type)
import GHC.Stack (HasCallStack)

isX :: a -> Either String a

class ShowX (a :: Type)
showsPrecX :: ShowX a => Int -> a -> ShowS

class NFDataX (a :: Type)
deepErrorX :: NFDataX a => HasCallStack => String -> a
hasUndefined :: NFDataX a => a -> Bool
rnfX :: NFDataX a => a -> ()
ensureSpine :: NFDataX a => a -> a

errorX :: HasCallStack => String -> a
