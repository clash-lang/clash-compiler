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

-- | Suffix instance and register names with the given 'Symbol', but add it
-- in front of other suffixes.
--
-- When you write
--
-- @
-- suffixName \@\"A\" (suffixName \@\"B\" f))
-- @
--
-- you get register and instance names inside /f/ with the suffix: "_B_A"
--
-- However, if you want them in the other order you can write:
--
-- @
-- suffixNameP \@\"A\" (suffixName \@\"B\" f))
-- @
--
-- so that names inside /f/ will have the suffix "_A_B"
suffixNameP
  :: forall (name :: Symbol) a . a -> name ::: a
suffixNameP = id
{-# NOINLINE suffixNameP #-}

-- | Suffix instance and register names with the given 'Nat', but add it in
-- front of other suffixes.
--
-- When you write
--
-- @
-- suffixNameNat \@1 (suffixName \@\"B\" f))
-- @
--
-- you get register and instance names inside /f/ with the suffix: "_B_1"
--
-- However, if you want them in the other order you can write:
--
-- @
-- suffixNameNatP \@1 (suffixName \@\"B\" f))
-- @
--
-- so that names inside /f/ will have the suffix "_1_B"
suffixNameFromNatP
  :: forall (name :: Nat) a . a -> name ::: a
suffixNameFromNatP = id
{-# NOINLINE suffixNameFromNatP #-}

-- | Name the instance or register with the given 'Symbol', instead of using
-- an auto-generated name. Pre- and suffixes annotated with 'prefixName' and
-- 'suffixName' will be added to both instances and registers named with
-- 'setName' and instances and registers that are auto-named.
setName
  :: forall (name :: Symbol) a . a -> name ::: a
setName = id
{-# NOINLINE setName #-}
