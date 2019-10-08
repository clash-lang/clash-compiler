{-|
  Copyright   :  (C) 2019, Myrtle Software Ltd
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <clash@qbaylogic.com>

Control module instance, and register, names in generated HDL code.
-}

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Clash.Magic where

import Clash.NamedTypes ((:::))
import GHC.TypeLits     (Nat,Symbol)

-- | Prefix instance and register names with the given 'Symbol'
prefixName
  :: forall (name :: Symbol) a . a -> name ::: a
prefixName = id
{-# NOINLINE prefixName #-}

-- | Suffix instance and register names with the given 'Symbol'
suffixName
  :: forall (name :: Symbol) a . a -> name ::: a
suffixName = id
{-# NOINLINE suffixName #-}

-- | Suffix instance and register names with the given 'Nat'
suffixNameFromNat
  :: forall (name :: Nat) a . a -> name ::: a
suffixNameFromNat = id
{-# NOINLINE suffixNameFromNat #-}

-- | Name the instance or register with the given 'Symbol', instead of using
-- an auto-generated name. Pre- and suffixes annotated with 'prefixName' and
-- 'suffixName' will be added to both instances and registers named with
-- 'setName' and instances and registers that are auto-named.
setName
  :: forall (name :: Symbol) a . a -> name ::: a
setName = id
{-# NOINLINE setName #-}
