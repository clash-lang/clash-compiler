{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Clash uniques for GHC names that do not depend on the values GHC assigned.
  See Note [Deterministic uniques].
-}
module Clash.GHC.Unique
  ( stableUniqueFor
  , globalUnique
  , localUnique
  , maxLocalUniques
  , maxUniqueScopes
  , tyConUniqueScope
  ) where

import Data.Bits (shiftL, xor, (.&.), (.|.))
import Data.Char (ord)
import qualified Data.List as List
import Data.Word (Word64)

import GHC.Builtin.Utils (isKnownKeyName)
import qualified GHC.Types.Name as GHC

import Clash.Unique (Unique, fromGhcUnique)

{- Note [Deterministic uniques]

Clash Core identifies names by their unique, and it used to take those uniques
straight from GHC. GHC hands out uniques from a process-wide counter, so the
values, and more importantly the relative order of any two uniques, depend on
everything the GHC session did before: which interfaces it loaded, in which
order, and how many names it allocated along the way. That order leaks into the
generated HDL. Running Clash with @-dinitial-unique=16777215
-dunique-increment=-1@, which reverses the order of every unique GHC allocates,
changes the HDL of about a quarter of the test suite's designs: declarations
move around, and generated names such as @c$ds_app_arg@ end up on different
binders.

A fresh process compiles the same design to the same HDL every time because it
replays the same allocation sequence. A long-lived GHC session does not: names
loaded for an earlier design keep the uniques they got back then, while the
current design's names get fresh ones. To let one session compile many designs
and still produce the HDL a fresh process would, Clash gives GHC names uniques
that are a function of the design alone:

* Names GHC itself keeps stable across sessions (wired-in and known-key names,
  such as 'True' or the tuple type constructors) keep their GHC unique. Clash
  compares against some of these constants directly, e.g. in
  "Clash.Core.TysPrim".

* Other external names (top-level binders, type constructors, data
  constructors, classes) get a hash of their stable string, which includes the
  unit, module and occurrence name. See 'globalUnique'.

* Local names (lambda- and let-bound variables, type variables) get an index in
  the order they are first encountered within a scope, where a scope is one
  top-level binder group or the type constructor conversion. Scopes are
  numbered from a list sorted by stable name, so the numbering does not depend
  on the order in which GHC happened to hand us the binders. See 'localUnique'.

The tag byte in the top eight bits keeps the three kinds apart and away from the
uniques Clash allocates itself from "Clash.Util.Supply" (which count up from
zero) and from GHC's known-key uniques (tags @'0'@ to @'9'@). Clash's own fresh
uniques must also be replayed per design; see 'Clash.Util.Supply.resetBlockCounter'.
-}

-- | The Clash unique of a GHC name that can be identified without a scope:
-- known-key and wired-in names keep GHC's unique, other external names get a
-- 'globalUnique'. Local names yield 'Nothing'; see 'localUnique'.
stableUniqueFor :: GHC.Name -> Maybe Unique
stableUniqueFor nm
  | GHC.isWiredInName nm || isKnownKeyName nm = Just (fromGhcUnique (GHC.nameUnique nm))
  | GHC.isExternalName nm = Just (globalUnique (GHC.nameStableString nm))
  | otherwise = Nothing

-- | A unique for an external name, computed from its stable string (see
-- 'GHC.nameStableString'): the FNV-1a hash of the string in the low 56 bits,
-- tagged with @'G'@.
globalUnique :: String -> Unique
globalUnique = tagged 'G' . (.&. payloadMask) . fnv1a64

-- | A unique for a local name: the scope number in bits 24 to 55 and the index
-- of the name within the scope in the low 24 bits, tagged with @'L'@.
localUnique
  :: Int
  -- ^ Scope, below 'maxUniqueScopes'
  -> Int
  -- ^ Index within the scope, below 'maxLocalUniques'
  -> Unique
localUnique scope n
  | scope < 0 || scope >= maxUniqueScopes =
      error ("Clash.GHC.Unique.localUnique: scope out of range: " ++ show scope)
  | n < 0 || n >= maxLocalUniques =
      error ("Clash.GHC.Unique.localUnique: too many local names in one scope: " ++ show n)
  | otherwise =
      tagged 'L' ((fromIntegral scope `shiftL` 24) .|. fromIntegral n)

-- | Number of local names one scope can hold.
maxLocalUniques :: Int
maxLocalUniques = 1 `shiftL` 24

-- | Number of scopes available to 'localUnique'.
maxUniqueScopes :: Int
maxUniqueScopes = 1 `shiftL` 32

-- | The scope used for type variables encountered while converting type
-- constructors. Binder groups are numbered from one, so this never collides
-- with them.
tyConUniqueScope :: Int
tyConUniqueScope = maxUniqueScopes - 1

tagged :: Char -> Word64 -> Unique
tagged tag payload =
  fromIntegral ((fromIntegral (ord tag) `shiftL` 56) .|. payload)

payloadMask :: Word64
payloadMask = (1 `shiftL` 56) - 1

-- | 64-bit FNV-1a over the code points of a string.
fnv1a64 :: String -> Word64
fnv1a64 = List.foldl' step 0xcbf29ce484222325
 where
  step h c = (h `xor` fromIntegral (ord c)) * 0x100000001b3
