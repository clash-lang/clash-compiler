module CLaSH.Netlist (genComponent,mkExpr,mkDcApplication) where

import CLaSH.Backend

import CLaSH.Core.DataCon   (DataCon)
import CLaSH.Core.Term      (Term,TmName)
import CLaSH.Core.Type      (Type)
import CLaSH.Netlist.Types  (Expr, HWType, NetlistMonad, Component,
                             Declaration)

genComponent :: Backend backend
  => TmName
  -> Maybe Int
  -> NetlistMonad backend Component

mkExpr :: Backend backend
       => Bool
       -> Type
       -> Term
       -> NetlistMonad backend (Expr,[Declaration])

mkDcApplication :: Backend backend
  => HWType
  -> DataCon
  -> [Term]
  -> NetlistMonad backend (Expr,[Declaration])
