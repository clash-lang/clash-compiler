{-|
  Copyright  :  (C) 2015-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

module CLaSH.Backend where

import Data.HashSet                         (HashSet)
import Data.Text.Lazy                       (Text)
import Control.Monad.State                  (State)
import Text.PrettyPrint.Leijen.Text.Monadic (Doc)

import CLaSH.Netlist.Types
import CLaSH.Netlist.BlackBox.Types

class Backend state where
  -- | Initial state for state monad
  initBackend :: Int -> HdlSyn -> state

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
  genHDL           :: String -> Component -> State state (String, Doc)
  -- | Generate a HDL package containing type definitions for the given HWTypes
  mkTyPackage      :: String -> [HWType] -> State state [(String, Doc)]
  -- | Convert a Netlist HWType to a target HDL type
  hdlType          :: HWType       -> State state Doc
  -- | Convert a Netlist HWType to an HDL error value for that type
  hdlTypeErrValue  :: HWType       -> State state Doc
  -- | Convert a Netlist HWType to the root of a target HDL type
  hdlTypeMark      :: HWType       -> State state Doc
  -- | Create a signal declaration from an identifier (Text) and Netlist HWType
  hdlSig           :: Text -> HWType -> State state Doc
  -- | Create a generative block statement marker
  genStmt          :: Bool -> State state Doc
  -- | Turn a Netlist Declaration to a HDL concurrent block
  inst             :: Declaration  -> State state (Maybe Doc)
  -- | Turn a Netlist expression into a HDL expression
  expr             :: Bool -> Expr -> State state Doc
  -- | Bit-width of Int/Word/Integer
  iwWidth          :: State state Int
  -- | Convert to a bit-vector
  toBV             :: HWType -> Text -> State state Doc
  -- | Convert from a bit-vector
  fromBV           :: HWType -> Text -> State state Doc
  -- | Synthesis tool we're generating HDL for
  hdlSyn           :: State state HdlSyn
  -- | mkBasicId
  mkBasicId        :: State state (String -> String)
