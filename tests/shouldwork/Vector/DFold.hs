{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
module DFold where

import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Singletons hiding (type (+))
import Data.Proxy
import Data.Kind (Type)

data Append (m :: Nat) (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (Append m a) l = Vec (l + m) a

append' xs ys = dfold (Proxy :: Proxy (Append m a)) (const (:>)) ys xs

topEntity :: (Vec 3 Int,Vec 7 Int) -> Vec 10 Int
topEntity = uncurry append'
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = pure (7:>8:>9:>Nil,0:>1:>2:>3:>4:>5:>6:>Nil)
    expectedOutput = outputVerifier' clk rst ((7:>8:>9:>0:>1:>2:>3:>4:>5:>6:>Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
