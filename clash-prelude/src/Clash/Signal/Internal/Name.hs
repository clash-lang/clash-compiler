{-|
  Copyright   : (C) 2026 QBayLogic B.V.
  License     : BSD2 (see the file LICENSE)
  Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

  Clock-domain names shared by runtime reflection and the Clash compiler.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

-- Clash normally exposes all Prelude unfoldings. Keep runtime reflection out
-- of the interface closure loaded for synthesis.
{-# OPTIONS_GHC -fno-expose-all-unfoldings #-}

module Clash.Signal.Internal.Name
  ( DomainName(..)
  , renderDomainName
  , typeRepDomainName
  ) where

import Type.Reflection
  (TypeRep, SomeTypeRep(..),
#if __GLASGOW_HASKELL__ >= 914
   data App, data Con',
#else
   pattern App, pattern Con',
#endif
   tyConPackage, tyConModule, tyConName)
import Clash.Annotations.Primitive (dontTranslate)

-- | A nominal type name. Applications include kind arguments, so e.g.
-- @Proxy :: Type -> Type@ and @Proxy :: Bool -> Type@ remain distinct.
data DomainName
  = DomainName String
  | DomainApp DomainName DomainName

-- | Render without compiler uniques or pretty-printer settings. Literal names
-- use Haskell escaping; constructor names include their unit and module.
renderDomainName :: DomainName -> String
renderDomainName name = go False name ""
 where
  go nested (DomainApp f a) =
    showParen nested (go False f . showChar ' ' . go True a)
  go _ (DomainName atom) = showString atom

-- | Runtime half of the clock-name protocol. This is ordinary Haskell
-- reflection, not a synthesizable operation.
typeRepDomainName :: TypeRep a -> DomainName
typeRepDomainName (App f a) =
  DomainApp (typeRepDomainName f) (typeRepDomainName a)
typeRepDomainName (Con' tc kinds) =
  foldl DomainApp (DomainName name)
    [typeRepDomainName kind | SomeTypeRep kind <- kinds]
 where
  name
    -- TypeRep exposes type-level literals as synthetic TyCons, with their
    -- already escaped spelling as the name.
    | tyConPackage tc == "base"
    , tyConModule tc `elem` ["GHC.TypeLits", "GHC.Internal.TypeLits"] = tyConName tc
    | '\'':occ <- tyConName tc = '\'' : qualified occ
    | otherwise = qualified (tyConName tc)
  qualified occ = "$" ++ tyConPackage tc ++ "$" ++ tyConModule tc ++ "$" ++ occ
{-# OPAQUE typeRepDomainName #-}
{-# ANN typeRepDomainName dontTranslate #-}
