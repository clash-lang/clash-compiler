{- A dead, non-representable argument of a lifted local function is never
   removed; when the function is inlined during flattening, the argument's
   definition stays behind as a dead let-binding and reaches netlist generation:

     No blackbox found for: GHC.Prim.newMutVar#
-}
module T3407_deadCode_after_appProp where

import Clash.Prelude
import Data.IORef (IORef, newIORef)
import System.IO.Unsafe (unsafePerformIO)

topEntity :: Unsigned 8 -> Unsigned 8 -> Unsigned 8
topEntity k x = step (unsafePerformIO (newIORef 0)) x + step (unsafePerformIO (newIORef 0)) (x + 1)
 where
  -- Needs: (1) a free variable ('k') so it is not the first argument that is
  -- non-representable, (2) two call sites so GHC keeps it as a let-bound
  -- lambda, (3) a body larger than -fclash-inline-function-limit.
  step :: IORef Int -> Unsigned 8 -> Unsigned 8
  step _ a = (a + k) * (a - k) + a * k
