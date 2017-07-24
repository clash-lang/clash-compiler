{-|
  Copyright   :  (C) 2015-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

module CLaSH.Netlist
  (genComponent
  ,mkExpr
  ,mkDcApplication
  ,mkDeclarations
  ,mkNetDecl
  ,mkProjection
  ,mkSelection
  ) where

import CLaSH.Core.DataCon   (DataCon)
import CLaSH.Core.Term      (Alt,LetBinding,Term,TmOccName)
import CLaSH.Core.Type      (Type)
import CLaSH.Core.Var       (Id)
import CLaSH.Driver.Types   (SrcSpan)
import CLaSH.Netlist.Types  (Expr, HWType, Identifier, NetlistMonad, Component,
                             Declaration)

genComponent :: TmOccName
             -> NetlistMonad (SrcSpan,Component)

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

mkNetDecl :: LetBinding -> NetlistMonad Declaration

mkDeclarations :: Id -> Term -> NetlistMonad [Declaration]
