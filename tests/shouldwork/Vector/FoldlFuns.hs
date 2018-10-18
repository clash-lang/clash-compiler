-- see https://github.com/clash-lang/clash-compiler/issues/364
module FoldlFuns where

import Clash.Prelude

func
    :: BitVector 8
    -> BitVector 8
    -> BitVector 32
func d c = 0

mod'
    :: forall dom gated sync. HiddenClockReset dom gated sync
    => Signal dom (BitVector 32)
mod' = o
    where

    x :: Signal dom (BitVector 8) = pure 0

    f :: Signal dom (BitVector 8) -> Signal dom (BitVector 32)
    f = foldl1 (\x y q -> liftA2 (.|.) (x q) (y q))
        $  liftA2 func x
        :> liftA2 func x
        :> liftA2 func x
        :> Nil

    o :: Signal dom (BitVector 32)
    o = f (pure 0)

topEntity clk rst = withClockReset @System @Source @Synchronous clk rst mod'
