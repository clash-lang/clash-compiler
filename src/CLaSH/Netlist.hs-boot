module CLaSH.Netlist (genComponent) where

import CLaSH.Core.Term      (TmName)
import CLaSH.Netlist.Types  (NetlistMonad,Component)

genComponent ::
  TmName
  -> Maybe Integer
  -> NetlistMonad Component
