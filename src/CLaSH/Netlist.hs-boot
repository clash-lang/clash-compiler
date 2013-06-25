module CLaSH.Netlist (genComponent,mkDcApplication) where

import CLaSH.Core.DataCon   (DataCon)
import CLaSH.Core.Term      (Term,TmName)
import CLaSH.Netlist.Types  (Expr,HWType,NetlistMonad,Component,Declaration)

genComponent ::
  TmName
  -> Maybe Int
  -> NetlistMonad Component

mkDcApplication ::
  HWType
  -> DataCon
  -> [Term]
  -> NetlistMonad (Expr,[Declaration])
