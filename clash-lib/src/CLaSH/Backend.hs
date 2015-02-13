module CLaSH.Backend where

import           Data.HashSet               (HashSet)
import           Data.Text.Lazy                       (Text)
import Control.Monad.State                  (State)
import Text.PrettyPrint.Leijen.Text.Monadic (Doc)

import CLaSH.Netlist.Types

class Backend state where
  -- | Initial state for state monad
  initBackend :: state

  -- | Location for the primitive definitions
  primDir :: state -> IO FilePath

  -- | Name of backend, used for directory to put output files in. Should be
  -- | constant function / ignore argument.
  name :: state -> String

  -- | File extension for target langauge
  extension :: state -> String

  -- | Get the set of types out of state
  extractTypes     :: state -> HashSet HWType

  -- | Generate HDL for a Netlist component
  genHDL           :: Component    -> State state (String, Doc)
  -- | Generate a HDL package containing type definitions for the given HWTypes
  mkTyPackage      :: [HWType]     -> State state Doc
  -- | Convert a Netlist HWType to a target HDL type
  hdlType          :: HWType       -> State state Doc
  -- | Convert a Netlist HWType to an HDL error value for that type
  hdlTypeErrValue  :: HWType       -> State state Doc
  -- | Convert a Netlist HWType to the root of a target HDL type
  hdlTypeMark      :: HWType       -> State state Doc
  -- | Create a signal declaration from an identifier (Text) and Netlist HWType
  hdlSig           :: Text -> HWType -> State state Doc
  -- | Turn a Netlist Declaration to a HDL concurrent block
  inst             :: Declaration  -> State state (Maybe Doc)
  -- | Turn a Netlist expression into a HDL expression
  expr             :: Bool -> Expr -> State state Doc
