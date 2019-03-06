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

import Data.DList           (DList)
import Data.HashSet         (HashSet)
import Clash.Core.DataCon   (DataCon)
import Clash.Core.Term      (Alt,LetBinding,Term)
import Clash.Core.Type      (Type)
import Clash.Core.Var       (Id)
import Clash.Netlist.Types  (Expr, HWType, Identifier, NetlistMonad, Component,
                             Declaration)
import SrcLoc               (SrcSpan)


genComponent :: Id
             -> NetlistMonad ([Bool],SrcSpan,HashSet Identifier,Component)

mkExpr :: Bool
       -> Either Identifier Id
       -> Type
       -> Term
       -> NetlistMonad (Expr,DList Declaration)

mkDcApplication :: HWType
                -> Either Identifier Id
                -> DataCon
                -> [Term]
                -> NetlistMonad (Expr,DList Declaration)

mkProjection
  :: Bool
  -> Either Identifier Id
  -> Term
  -> Type
  -> Alt
  -> NetlistMonad (Expr, DList Declaration)

mkSelection
  :: Id
  -> Term
  -> Type
  -> [Alt]
  -> NetlistMonad (DList Declaration)

mkNetDecl :: LetBinding -> NetlistMonad (Maybe Declaration)

mkDeclarations :: Id -> Term -> NetlistMonad (DList Declaration)
