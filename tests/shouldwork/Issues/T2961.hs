{-# LANGUAGE TypeFamilyDependencies #-}
-- | A cast between @Index 256@ and @Integer@ (introduced by a type family
-- reducing to 'Data.Functor.Identity.Identity') survived normalization and
-- made netlist generation throw "Not in normal form: application of cast".
-- See https://github.com/clash-lang/clash-compiler/issues/2961
module T2961 where

import Clash.Explicit.Prelude
import Data.Data (Proxy)
import Data.Functor.Identity (Identity)
import Data.Kind (Type)

-- | Allows for optional data.
-- Depending on the value of @keep@, the data can either be included or left
-- out. When left out, the data is represented instead as type @()@.
type family KeepType (keep :: Bool) (optionalType :: Type) = t | t -> keep optionalType where
  KeepType 'True optionalType = Identity optionalType
  KeepType 'False optionalType = Proxy optionalType

type BurstLengthType (keep :: Bool) = KeepType keep (Index 256)

data M2S_WriteAddress = M2S_WriteAddress !(BurstLengthType 'True)

topEntity :: M2S_WriteAddress
topEntity = M2S_WriteAddress 1 -- Clash casts this (Index 256) to an Integer
{-# OPAQUE topEntity #-}
