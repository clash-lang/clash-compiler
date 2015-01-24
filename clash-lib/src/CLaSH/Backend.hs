module CLaSH.Backend where

import           Data.HashSet               (HashSet)
import Control.Monad.State                  (State)
import Text.PrettyPrint.Leijen.Text.Monadic (Doc)

import CLaSH.Netlist.Types

class Backend state where
  init             :: state
  -- | Generate VHDL for a Netlist component
  genVHDL          :: Component    -> State state (String, Doc)
  -- | Generate a VHDL package containing type definitions for the given HWTypes
  mkTyPackage      :: [HWType]     -> State state Doc
  -- | Convert a Netlist HWType to a VHDL type
  vhdlType         :: HWType       -> State state Doc
  -- | Convert a Netlist HWType to an error VHDL value for that type
  vhdlTypeErrValue :: HWType       -> State state Doc
  -- | Convert a Netlist HWType to the root of a VHDL type
  vhdlTypeMark     :: HWType       -> State state Doc
  -- | Turn a Netlist Declaration to a VHDL concurrent block
  inst             :: Declaration  -> State state (Maybe Doc)
  -- | Turn a Netlist expression into a VHDL expression
  expr             :: Bool -> Expr -> State state Doc
  extractTypes     :: state -> HashSet HWType
  --expr             :: Declaration  -> State state Doc
