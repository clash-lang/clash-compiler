-- | The evaluator's rules for '&&' and '||' evaluate both arguments to WHNF
-- with a nested call to the evaluator. When the result is the WHNF of one of
-- the arguments, they used to continue in the machine from /before/ that
-- nested evaluation. That machine misses the heap bindings the nested
-- evaluation made, and its unique supply hands out their uniques again.
--
-- Here, the WHNF of @h (c + 1)@ refers to a heap binding for @c + 1@, which
-- ended up free in the result of caseCon.
module AndOrHeapBindings where

import Clash.Prelude
import qualified Prelude as P

h :: Int -> Bool
h c = error ("h: " P.++ show c)
{-# OPAQUE h #-}

-- OPAQUE, so GHC doesn't simplify the conjunction and disjunction away
t :: Bool
t = True
{-# OPAQUE t #-}

f :: Bool
f = False
{-# OPAQUE f #-}

topEntity :: Int -> (Int, Int)
topEntity c =
  ( if t && h (c + 1) then 1 else 2
  , if f || h (c + 2) then 3 else 4
  )
