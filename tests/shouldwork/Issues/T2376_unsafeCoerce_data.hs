module T2376_unsafeCoerce_data where
import Clash.Prelude
import Unsafe.Coerce
import Clash.Explicit.Testbench

data AB a b = A a    | B b
data CD     = C Bool | D Int


topEntity = (f,g)

f :: Int
f = case unsafeCoerce ab of
  C _ -> 123
  D i -> i
 where
  ab :: AB Bool Int
  ab = B 4

g :: Int
g = case unsafeCoerce cd of
  A _ -> 456
  B i -> i
 where
  cd :: CD
  cd = D 5


testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst ((4,5) :> Nil)
    done           = expectedOutput (pure topEntity)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
