module CLaSH.Netlist (genComponent,mkExpr,mkDcApplication) where

import CLaSH.Core.DataCon   (DataCon)
import CLaSH.Core.Term      (Term,TmName)
import CLaSH.Core.Type      (Type)
import CLaSH.Netlist.Types  (Expr, HWType, NetlistMonad, Component,
                             Declaration)
import CLaSH.Netlist.VHDL   (VHDLState)

genComponent ::
  TmName
  -> Maybe Int
  -> NetlistMonad VHDLState Component

mkExpr :: Bool
       -> Type
       -> Term
       -> NetlistMonad VHDLState (Expr,[Declaration])

mkDcApplication ::
  HWType
  -> DataCon
  -> [Term]
  -> NetlistMonad VHDLState (Expr,[Declaration])
