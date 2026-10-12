{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utility functions for primitives, on scoped Clash Core: ports of those of
  "Clash.Primitives.Util" that look at Clash Core.
-}

module Clash.Primitives.Scoped.Util
  ( getFunctionPlurality
  ) where

import           GHC.Stack                  (HasCallStack)

import           Clash.Core.Scoped.Syntax   (Distinct, Term, Type)
import           Clash.Netlist.Scoped.Types (BlackBoxEnv, callBlackBoxFunction)
import           Clash.Netlist.Types        (NetlistMonad)
import           Clash.Primitives.Types     (CompiledPrimitive)
import           Clash.Primitives.Util      (getFunctionPluralityWith)

-- | Looks up the plurality of a function's function argument, see
-- 'Clash.Primitives.Util.getFunctionPlurality'. If not set, the returned
-- plurality will default to /1/.
getFunctionPlurality
  :: (HasCallStack, Distinct n)
  => BlackBoxEnv n
  -- ^ The context of the arguments
  -> CompiledPrimitive
  -> [Either (Term n) (Type n)]
  -- ^ Arguments passed to blackbox
  -> [Type n]
  -- ^ Result types
  -> Int
  -- ^ Argument number holding the function of interest
  -> NetlistMonad Int
  -- ^ Plurality of function. Defaults to 1. Does not err if argument isn't
  -- a function in the first place. State of monad will not be modified.
getFunctionPlurality env p args resTys =
  getFunctionPluralityWith
    (\f nm -> callBlackBoxFunction env f False nm args resTys) p
