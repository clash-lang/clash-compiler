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
  ,mkFunApp
  ) where

import Data.HashMap.Strict  (HashMap)
import Clash.Core.DataCon   (DataCon)
import Clash.Core.Term      (Alt,LetBinding,Term)
import Clash.Core.Type      (Type)
import Clash.Core.Var       (Id)
import Clash.Netlist.Types  (Expr, HWType, Identifier, NetlistMonad, Component,
                             Declaration)
import SrcLoc               (SrcSpan)

import GHC.Stack (HasCallStack)

genComponent :: HasCallStack
             => Id
             -> NetlistMonad ([Bool],SrcSpan,HashMap Identifier Word,Component)

mkExpr :: HasCallStack
       => Bool
       -> Either Identifier Id
       -> Type
       -> Term
       -> NetlistMonad (Expr,[Declaration])

mkDcApplication :: HasCallStack
                => HWType
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
  :: Either Identifier Id
  -> Term
  -> Type
  -> [Alt]
  -> [Declaration]
  -> NetlistMonad [Declaration]

mkNetDecl :: LetBinding -> NetlistMonad (Maybe Declaration)

mkDeclarations :: HasCallStack => Id -> Term -> NetlistMonad [Declaration]

mkFunApp
  :: HasCallStack
  => Identifier -- ^ LHS of the let-binder
  -> Id -- ^ Name of the applied function
  -> [Term] -- ^ Function arguments
  -> [Declaration] -- ^ Tick declarations
  -> NetlistMonad [Declaration]
