{-# LANGUAGE OverloadedStrings #-}
module LocalTopPkgBBTF where

import Clash.Prelude
import Clash.Annotations.Primitive (Primitive(..), HDL(..))

{-# ANN bbTop (InlinePrimitive [VHDL] "[ { \"BlackBox\" : { \"name\" : \"LocalTopPkgBBTF.bbTop\", \"kind\": \"Declaration\", \"format\": \"Haskell\", \"templateFunction\": \"Repro.PublicBB.bbTF\"}} ]") #-}
bbTop :: Signal System Int -> Signal System Int
bbTop = id
{-# OPAQUE bbTop #-}

topEntity :: Signal System Int -> Signal System Int
topEntity = bbTop
{-# OPAQUE topEntity #-}
