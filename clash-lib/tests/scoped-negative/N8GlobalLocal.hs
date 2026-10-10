{-# LANGUAGE GADTs, RankNTypes, DataKinds, ScopedTypeVariables #-}
-- EXPECT: Couldn't match expected type ‘GlobalId’
-- | Namespaces: look a local variable up in an environment of globals
module N8GlobalLocal where

import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Syntax

lookupLocal :: TmName n -> GlobalEnv a -> Maybe a
lookupLocal n env = lookupGlobalEnv n env
