module CLaSH.Driver.Types where

import Data.HashMap.Lazy (HashMap)

import CLaSH.Core.Term   (Term,TmName)
import CLaSH.Core.Type   (Type,TyName)

type DFunMap    = HashMap TmName (Type,(([TyName],[TmName]),[Term]))
type ClassOpMap = HashMap TmName (Type,Int)
type BindingMap = HashMap TmName (Type,Term)
