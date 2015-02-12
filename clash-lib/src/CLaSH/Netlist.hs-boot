module CLaSH.Netlist (genComponent,mkExpr,mkDcApplication) where

import CLaSH.Core.DataCon   (DataCon)
import CLaSH.Core.Term      (Term,TmName)
import CLaSH.Core.Type      (Type)
import CLaSH.Netlist.Types  (Expr, HWType, NetlistMonad, Component,
                             Declaration)

genComponent :: TmName
             -> Maybe Int
             -> NetlistMonad Component

mkExpr :: Bool
       -> Type
       -> Term
       -> NetlistMonad (Expr,[Declaration])

mkDcApplication :: HWType
                -> DataCon
                -> [Term]
                -> NetlistMonad (Expr,[Declaration])
