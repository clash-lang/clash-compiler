{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Record where

import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Kind (Type)

-- Real-world example of a GADT stolen from:
--   https://stackoverflow.com/questions/21505975/write-gadt-record-with-constrained-type
class HasData (a :: Type) where
    type Data a :: Type

instance HasData Int where
    type Data Int = Maybe Int

data Foo :: Type -> Type where
    Foo :: HasData a => {
        getChar :: Char,
        getData :: Data a,
        getInt :: Int
    } -> Foo a

deriving instance Eq (Foo Int)

instance ShowX (Foo Int) where
  showsPrecX _ _ = undefined

succIntChar :: Foo a -> Foo a
succIntChar (Foo chr dt int) =
  Foo (succ chr) dt (succ int)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE succIntChar #-}

topEntity :: Foo Int -> Foo Int
topEntity foo = Foo chr (succ <$> dt) int
  where
    Foo chr dt int = succIntChar foo
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (Foo 'c' (Just 4) 6 :> Nil)
    expectedOutput = outputVerifier' clk rst (Foo 'd' (Just 5) 7 :> Nil)

    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}
