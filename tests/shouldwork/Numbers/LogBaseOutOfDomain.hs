-- Regression test: 'GHC.Num.Integer.integerLogBase#' and
-- 'GHC.Num.Natural.naturalLogBase#' must be constant folded for a
-- non-positive second argument too, where they are total but yield a
-- meaningless value. Left unfolded they drag 'Integer' into the netlist, so
-- this test passes @-Werror=clash-non-synthesizable@. See @tests/Main.hs@.
{-# LANGUAGE MagicHash #-}

module LogBaseOutOfDomain where

import Clash.Prelude
import GHC.Word (Word(W#))
import qualified GHC.Num.Integer as Integer
import qualified GHC.Num.Natural as Natural

-- | Kept out of 'topEntity' so GHC does not fold these itself. Clash should.
logBases :: Vec 3 Word
logBases =
     W# (Integer.integerLogBase# 2 0)
  :> W# (Integer.integerLogBase# 3 (-7))
  :> W# (Natural.naturalLogBase# 2 0)
  :> Nil
{-# NOINLINE logBases #-}

topEntity :: Unsigned 64
topEntity = fromIntegral (sum logBases)
{-# OPAQUE topEntity #-}
