module CLaSH.Backend where

import           Data.HashSet               (HashSet)
import Control.Monad.State                  (State)
import Text.PrettyPrint.Leijen.Text.Monadic (Doc)

import CLaSH.Netlist.Types

class Backend state where
  -- | Initial state for state monad
  init :: state

  -- | name of backend, used for directory to put output files in
  -- | Should be constant function / ignore value
  name :: state -> String

  -- | get set of types out of state
  extractTypes     :: state -> HashSet HWType

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
