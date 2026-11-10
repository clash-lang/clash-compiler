{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module is compiled into the @clash-testsuite@ library, so Clash loads
-- it from its interface file instead of from source. The inline primitive on
-- 'myId' refers to the wrong name, which Clash should report as is, instead of
-- trying to load this module from (nonexistent) local sources.
module T2365 where

import Clash.Prelude
import Clash.Annotations.Primitive (Primitive(..), HDL(..))

{-# ANN myId (InlineYamlPrimitive [VHDL] "BlackBox:\n  kind: Expression\n  name: T2365.myIdX\n  template: ~ARG[0]\n") #-}
myId :: Signal System Int -> Signal System Int
myId = id
{-# OPAQUE myId #-}

topEntity :: Signal System Int -> Signal System Int
topEntity = myId
