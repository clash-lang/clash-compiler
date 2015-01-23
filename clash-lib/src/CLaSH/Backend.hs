module CLaSH.Backend where

import           Data.HashSet               (HashSet)
import Control.Monad.State                  (State)
import Text.PrettyPrint.Leijen.Text.Monadic (Doc)

import CLaSH.Netlist.Types

class Backend state where
  init             :: state
  genVHDL          :: Component    -> State state (String, Doc)
  mkTyPackage      :: [HWType]     -> State state Doc
  vhdlType         :: HWType       -> State state Doc
  vhdlTypeErrValue :: HWType       -> State state Doc
  vhdlTypeMark     :: HWType       -> State state Doc
  inst             :: Declaration  -> State state (Maybe Doc)
  expr             :: Bool -> Expr -> State state Doc
  extractTypes     :: state -> HashSet HWType
  --expr             :: Declaration  -> State state Doc
