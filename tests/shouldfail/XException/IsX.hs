module IsX where

import Clash.Prelude
import Data.Either (isLeft)

-- 'isX' can only be used in simulation. Clash should report a clear error,
-- instead of crashing with an internal error, when it ends up in synthesizable
-- code.
--
-- See https://github.com/clash-lang/clash-compiler/issues/3432
topEntity :: Signal System (Unsigned 8) -> Signal System Bool
topEntity = fmap (isLeft . isX)
