{-# LANGUAGE CPP, KindSignatures #-}
module DFold where

import Clash.Prelude
import Clash.Explicit.Testbench
#if MIN_VERSION_singletons(2,4,0)
import Data.Singletons.Prelude hiding (type (+))
#else
import Data.Singletons.Prelude
#endif
import Data.Proxy

data Append (m :: Nat) (a :: *) (f :: TyFun Nat *) :: *
type instance Apply (Append m a) l = Vec (l + m) a

append' xs ys = dfold (Proxy :: Proxy (Append m a)) (const (:>)) ys xs

topEntity :: (Vec 3 Int,Vec 7 Int) -> Vec 10 Int
topEntity = uncurry append'
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = pure (7:>8:>9:>Nil,0:>1:>2:>3:>4:>5:>6:>Nil)
    expectedOutput = outputVerifier clk rst ((7:>8:>9:>0:>1:>2:>3:>4:>5:>6:>Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
