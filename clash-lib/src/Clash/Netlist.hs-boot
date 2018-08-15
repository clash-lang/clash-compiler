{-|
  Copyright   :  (C) 2015-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

module Clash.Netlist
  (genComponent
  ,mkExpr
  ,mkDcApplication
  ,mkDeclarations
  ,mkNetDecl
  ,mkProjection
  ,mkSelection
  ) where

import Clash.Core.DataCon   (DataCon)
import Clash.Core.Term      (Alt,LetBinding,Term)
import Clash.Core.Type      (Type)
import Clash.Core.Var       (Id)
import Clash.Netlist.Types  (Expr, HWType, Identifier, NetlistMonad, Component,
                             Declaration)
import SrcLoc               (SrcSpan)


genComponent :: Id
             -> NetlistMonad (SrcSpan,[Identifier],Component)

mkExpr :: Bool
       -> Either Identifier Id
       -> Type
       -> Term
       -> NetlistMonad (Expr,[Declaration])

mkDcApplication :: HWType
                -> Either Identifier Id
                -> DataCon
                -> [Term]
                -> NetlistMonad (Expr,[Declaration])

mkProjection
  :: Bool
  -> Either Identifier Id
  -> Term
  -> Type
  -> Alt
  -> NetlistMonad (Expr, [Declaration])

mkSelection
  :: Id
  -> Term
  -> Type
  -> [Alt]
  -> NetlistMonad [Declaration]

mkNetDecl :: LetBinding -> NetlistMonad (Maybe Declaration)

mkDeclarations :: Id -> Term -> NetlistMonad [Declaration]
