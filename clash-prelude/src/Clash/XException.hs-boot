module Clash.XException where

import GHC.Stack (HasCallStack)

isX :: a -> Either String a

class ShowX a
showsPrecX :: ShowX a => Int -> a -> ShowS

class NFDataX a
deepErrorX :: NFDataX a => HasCallStack => String -> a
hasUndefined :: NFDataX a => a -> Bool
rnfX :: NFDataX a => a -> ()
ensureSpine :: NFDataX a => a -> a

errorX :: HasCallStack => String -> a
