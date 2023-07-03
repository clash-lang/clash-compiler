{-# LANGUAGE CPP #-}

module T1139 where
import Clash.Prelude

topEntity, otherTopEntity :: Bool -> Bool
topEntity = not
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

otherTopEntity = topEntity
{-# ANN otherTopEntity (defSyn "otherTopEntity") #-}
