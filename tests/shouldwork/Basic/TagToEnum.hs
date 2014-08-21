{-# LANGUAGE MagicHash #-}
module TagToEnum where

import GHC.Exts
import Prelude

topEntity :: Bool -> (Int,Bool,Int,Bool)
topEntity b = (I# (dataToTag# b),tagToEnum# (dataToTag# b),I# (dataToTag# True), tagToEnum# 1#)
