{-|
  Copyright   :  (C) 2015-2016, University of Twente
                     2022     , Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE CPP #-}

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

import Clash.Core.DataCon   (DataCon)
import Clash.Core.Term      (Alt,LetBinding,Term)
import Clash.Core.Type      (Type)
import Clash.Core.Var       (Id)
import Clash.Netlist.Types
  (Expr, HWType, Identifier, NetlistMonad, Declaration, NetlistId,
   ComponentMeta, Component)

import GHC.Stack (HasCallStack)

genComponent :: HasCallStack
             => Id
             -> NetlistMonad (ComponentMeta, Component)

mkExpr :: HasCallStack
       => Bool
       -> NetlistId
       -> Term
       -> NetlistMonad (Expr,[Declaration])

mkDcApplication :: HasCallStack
                => [HWType]
                -> NetlistId
                -> DataCon
                -> [Term]
                -> NetlistMonad (Expr,[Declaration])

mkProjection
  :: Bool
  -> NetlistId
  -> Term
  -> Type
  -> Alt
  -> NetlistMonad (Expr, [Declaration])

mkSelection
  :: NetlistId
  -> Term
  -> Type
  -> [Alt]
  -> [Declaration]
  -> NetlistMonad [Declaration]

mkNetDecl :: LetBinding -> NetlistMonad [Declaration]

mkDeclarations :: HasCallStack => Id -> Term -> NetlistMonad [Declaration]

mkFunApp
  :: HasCallStack
  => Identifier -- ^ LHS of the let-binder
  -> Id -- ^ Name of the applied function
  -> [Term] -- ^ Function arguments
  -> [Declaration] -- ^ Tick declarations
  -> NetlistMonad [Declaration]
