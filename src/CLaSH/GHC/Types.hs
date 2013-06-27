module CLaSH.GHC.Types where

import CoreSyn
import Var

type CoreDFUN = (CoreBndr,(([Var],[Var]),[CoreExpr]))
