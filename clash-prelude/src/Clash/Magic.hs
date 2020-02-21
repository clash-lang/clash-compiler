{-|
  Copyright   :  (C) 2019, Myrtle Software Ltd
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Control module instance, and register, names in generated HDL code.
-}

module Clash.Magic
  (
  -- ** Functions to control names of identifiers in HDL
    prefixName
  , suffixName
  , suffixNameP
  , suffixNameFromNat
  , suffixNameFromNatP
  , setName

  -- ** Functions to control Clash's (de)duplication mechanisms
  , deDup
  , noDeDup
  ) where

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

-- | Force deduplication, i.e. share a function or operator between multiple
-- branches.
--
-- By default Clash converts
--
-- @
-- case x of
--   A -> 3 * y
--   B -> x * x
-- @
--
-- to
--
-- @
-- let f_arg0 = case x of {A -> 3; _ -> x}
--     f_arg1 = case x of {A -> y; _ -> x}
--     f_out  = f_arg0 * f_arg1
-- in  case x of
--       A -> f_out
--       B -> f_out
-- @
--
-- However, it won't do this for:
--
-- @
-- case x of
--   A -> 3 + y
--   B -> x + x
-- @
--
-- Because according to the internal heuristics the multiplexer introduced for
-- the deduplication are more expensive than the addition. This might not be
-- the case for your particular platform.
--
-- In these cases you can force Clash to deduplicate by:
--
-- @
-- case x of
--   A -> 'deDup' (3 + y)
--   B -> 'deDup' (x + x)
-- @
deDup
  :: forall a . a -> a
deDup = id
{-# NOINLINE deDup #-}

-- | Do not deduplicate, i.e. /keep/, an applied function inside a
-- case-alternative; do not try to share the function between multiple
-- branches.
--
-- By default Clash converts
--
-- @
-- case x of
--   A -> f 3 y
--   B -> f x x
--   C -> h x
-- @
--
-- to
--
-- @
-- let f_arg0 = case x of {A -> 3; _ -> x}
--     f_arg1 = case x of {A -> y; _ -> x}
--     f_out  = f f_arg0 f_arg1
-- in  case x of
--       A -> f_out
--       B -> f_out
--       C -> h x
-- @
--
-- i.e. it deduplicates functions (and operators such as multiplication) between
-- case-alternatives to save on area. This comes at the cost of multiplexing the
-- arguments for the deduplicated function.
--
-- There are two reasons you would want to stop Clash from doing this:
--
-- 1. The deduplicated function is in the critical path, and the addition of the
--    multiplexers further increased the propagation delay.
--
-- 2. Clash's heuristics were off, and the addition of the multiplexers actually
--    made the final circuit larger instead of smaller.
--
-- In these cases you want to tell Clash not to deduplicate:
--
-- @
-- case x of
--   A -> 'noDeDup' f 3 y
--   B -> f x x
--   C -> h x
-- @
--
-- Where the application of /f/ in the /A/-alternative is now explicitly not
-- deduplicated, and given that the /f/ in the B-alternative is the only
-- remaining application of /f/ in the case-expression it is also not
-- deduplicated.
--
-- Note that if the /C/-alternative also had an application of /f/, then the
-- applications of /f/ in the /B/- and /C/-alternatives would have been
-- deduplicated; i.e. the final circuit would have had two application of /f/.
noDeDup
  :: forall a . a -> a
noDeDup = id
{-# NOINLINE noDeDup #-}
