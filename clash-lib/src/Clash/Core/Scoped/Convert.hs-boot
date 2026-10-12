{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  For the transitional 'Data.Binary.Binary' instances of "Clash.Core.DataCon"
  and "Clash.Core.TyCon", which decode the types of Clash Core and convert
  them.
-}

module Clash.Core.Scoped.Convert where

import Data.Text (Text)
import GHC.Stack (HasCallStack)

import {-# SOURCE #-} Clash.Core.DataCon (ConTag, DataCon, DcName, DcStrictness)
import {-# SOURCE #-} Clash.Core.TyCon (AlgTyConRhs, TyCon, TyConName)
import {-# SOURCE #-} Clash.Core.Type (Kind, Type)
import Clash.Core.Var (TyVar)
import Clash.Unique (Unique)

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
