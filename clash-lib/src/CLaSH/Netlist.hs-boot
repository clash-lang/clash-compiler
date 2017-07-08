{-|
  Copyright   :  (C) 2015-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

module CLaSH.Netlist (genComponent,mkExpr,mkDcApplication) where

import CLaSH.Core.DataCon   (DataCon)
import CLaSH.Core.Term      (Term,TmName)
import CLaSH.Core.Type      (Type)
import CLaSH.Core.Var       (Id)
import CLaSH.Driver.Types   (SrcSpan)
import CLaSH.Netlist.Types  (Expr, HWType, Identifier, NetlistMonad, Component,
                             Declaration)

genComponent :: TmName
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
