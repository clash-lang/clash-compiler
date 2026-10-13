{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  For the modules of Clash Core that "Clash.Core.Scoped.Convert" depends on,
  and that make data constructors and type constructors of Clash Core, such
  as "Clash.Core.TysPrim", and for the builtin type constructors of
  "Clash.Core.Scoped.TysPrim".
-}

{-# LANGUAGE DataKinds #-}

module Clash.Core.Scoped.Convert where

import Data.Text (Text)
import GHC.Stack (HasCallStack)

import {-# SOURCE #-} Clash.Core.DataCon (ConTag, DataCon, DcName, DcStrictness)
import {-# SOURCE #-} Clash.Core.TyCon (AlgTyConRhs, TyCon, TyConName)
import {-# SOURCE #-} Clash.Core.Type (Kind, Type)
import Clash.Core.Var (TyVar)
import Clash.Unique (Unique)

import Clash.Core.Scoped.Sig (DataConSig)
import qualified Clash.Core.Scoped.Syntax as S

mkDataConFromClash
  :: HasCallStack
  => DcName -> Unique -> ConTag -> Type -> [TyVar] -> [TyVar] -> [Type]
  -> [DcStrictness] -> [Text] -> DataCon

mkAlgTyConFromClash
  :: HasCallStack => Unique -> TyConName -> Kind -> Int -> AlgTyConRhs -> Bool -> TyCon

mkPromotedDataConFromClash
  :: HasCallStack => Unique -> TyConName -> Kind -> Int -> DataCon -> TyCon

mkFunTyConFromClash
  :: HasCallStack => Unique -> TyConName -> Kind -> Int -> [([Type], Type)] -> TyCon

mkPrimTyConFromClash :: HasCallStack => Unique -> TyConName -> Kind -> Int -> TyCon

mkNewTyConRhsFromClash :: HasCallStack => DataCon -> ([TyVar], Type) -> AlgTyConRhs

mkDataCon
  :: DcName -> ConTag -> S.Type 'S.VoidS -> DataConSig -> [DcStrictness] -> [Text]
  -> DataCon

mkAlgTyCon :: TyConName -> S.Kind 'S.VoidS -> Int -> AlgTyConRhs -> Bool -> TyCon

mkPrimTyCon :: TyConName -> S.Kind 'S.VoidS -> Int -> TyCon
