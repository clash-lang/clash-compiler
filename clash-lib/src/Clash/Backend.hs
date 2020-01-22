{-|
  Copyright  :  (C) 2015-2016, University of Twente,
                    2017     , Myrtle Software Ltd, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Backend where

import Control.Lens                         (Lens')
import qualified  Control.Lens              as Lens
import Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import Data.HashSet                         (HashSet)
import Data.Maybe                           (fromMaybe)
import Data.Semigroup.Monad                 (Mon (..))
import qualified Data.Text                  as T
import Data.Text                            (Text)
import qualified Data.Text.Lazy             as LT
import Control.Monad.State                  (State)
import Data.Text.Prettyprint.Doc.Extra      (Doc)

import SrcLoc (SrcSpan)

import Clash.Netlist.Id
import {-# SOURCE #-} Clash.Netlist.Types
import Clash.Netlist.BlackBox.Types

import Clash.Annotations.Primitive          (HDL)

#ifdef CABAL
import qualified Paths_clash_lib
import qualified Data.Version
#else
import qualified System.FilePath
#endif

primsRoot :: IO FilePath
#ifdef CABAL
primsRoot = Paths_clash_lib.getDataFileName "prims"
#else
primsRoot = return ("clash-lib" System.FilePath.</> "prims")
#endif

clashVer :: String
#ifdef CABAL
clashVer = Data.Version.showVersion Paths_clash_lib.version
#else
clashVer = "development"
#endif

type ModName = Identifier

-- | Is a type used for internal or external use
data Usage
  = Internal
  -- ^ Internal use
  | External Text
  -- ^ External use, field indicates the library name

class Backend state where
  -- | Initial state for state monad
  initBackend :: Int -> HdlSyn -> Bool -> Maybe (Maybe Int) -> state

  -- | What HDL is the backend generating
  hdlKind :: state -> HDL

  -- | Location for the primitive definitions
  primDirs :: state -> IO [FilePath]

  -- | Name of backend, used for directory to put output files in. Should be
  --   constant function / ignore argument.
  name :: state -> String

  -- | File extension for target langauge
  extension :: state -> String

  -- | Get the set of types out of state
  extractTypes     :: state -> HashSet HWType

  -- | Generate HDL for a Netlist component
  genHDL           :: Identifier -> SrcSpan -> HashMap Identifier Word -> Component -> Mon (State state) ((String, Doc),[(String,Doc)])
  -- | Generate a HDL package containing type definitions for the given HWTypes
  mkTyPackage      :: Identifier -> [HWType] -> Mon (State state) [(String, Doc)]
  -- | Convert a Netlist HWType to a target HDL type
  hdlType          :: Usage -> HWType -> Mon (State state) Doc
  -- | Convert a Netlist HWType to an HDL error value for that type
  hdlTypeErrValue  :: HWType       -> Mon (State state) Doc
  -- | Convert a Netlist HWType to the root of a target HDL type
  hdlTypeMark      :: HWType       -> Mon (State state) Doc
  -- | Create a record selector
  hdlRecSel        :: HWType -> Int -> Mon (State state) Doc
  -- | Create a signal declaration from an identifier (Text) and Netlist HWType
  hdlSig           :: LT.Text -> HWType -> Mon (State state) Doc
  -- | Create a generative block statement marker
  genStmt          :: Bool -> State state Doc
  -- | Turn a Netlist Declaration to a HDL concurrent block
  inst             :: Declaration  -> Mon (State state) (Maybe Doc)
  -- | Turn a Netlist expression into a HDL expression
  expr             :: Bool -- ^ Enclose in parentheses?
                   -> Expr -- ^ Expr to convert
                   -> Mon (State state) Doc
  -- | Bit-width of Int,Word,Integer
  iwWidth          :: State state Int
  -- | Convert to a bit-vector
  toBV             :: HWType -> LT.Text -> Mon (State state) Doc
  -- | Convert from a bit-vector
  fromBV           :: HWType -> LT.Text -> Mon (State state) Doc
  -- | Synthesis tool we're generating HDL for
  hdlSyn           :: State state HdlSyn
  -- | mkIdentifier
  mkIdentifier     :: State state (IdType -> Identifier -> Identifier)
  -- | mkIdentifier
  extendIdentifier :: State state (IdType -> Identifier -> Identifier -> Identifier)
  -- | setModName
  setModName       :: ModName -> state -> state
  -- | setSrcSpan
  setSrcSpan       :: SrcSpan -> State state ()
  -- | getSrcSpan
  getSrcSpan       :: State state SrcSpan
  -- | Block of declarations
  blockDecl        :: Text -> [Declaration] -> Mon (State state) Doc
  -- | unextend/unescape identifier
  unextend         :: State state (Identifier -> Identifier)
  addIncludes      :: [(String, Doc)] -> State state ()
  addLibraries     :: [LT.Text] -> State state ()
  addImports       :: [LT.Text] -> State state ()
  addAndSetData    :: FilePath -> State state String
  getDataFiles     :: State state [(String,FilePath)]
  addMemoryDataFile  :: (String,String) -> State state ()
  getMemoryDataFiles :: State state [(String,String)]
  seenIdentifiers  :: Lens' state (HashMap Identifier Word)
  ifThenElseExpr :: state -> Bool

-- | Replace a normal HDL template placeholder with an unescaped/unextended
-- template placeholder.
--
-- Needed when the the place-holder is filled with an escaped/extended identifier
-- inside an escaped/extended identifier and we want to strip the escape
-- /extension markers. Otherwise we end up with illegal identifiers.
escapeTemplate :: Identifier -> Identifier
escapeTemplate "~RESULT" = "~ERESULT"
escapeTemplate t = fromMaybe t $ do
  t1 <- T.stripPrefix "~ARG[" t
  n  <- T.stripSuffix "]" t1
  pure (T.concat ["~EARG[",n,"]"])

mkUniqueIdentifier
  :: Backend s
  => IdType
  -> Identifier
  -> State s Identifier
mkUniqueIdentifier typ nm = do
  mkId     <- mkIdentifier
  extendId <- extendIdentifier
  seen     <- Lens.use seenIdentifiers
  let i = mkId typ nm
  case HashMap.lookup i seen of
    Just n -> go extendId n seen i
    Nothing -> do
     seenIdentifiers Lens.%= (HashMap.insert i 0)
     return i
 where
  go extendId n seen i = do
    let i' = extendId typ i (T.pack ('_':show n))
    case HashMap.lookup i' seen of
       Just _ -> go extendId (n+1) seen i
       Nothing -> do
        seenIdentifiers Lens.%= (HashMap.insert i' (n+1))
        return i'

preserveSeen
  :: Backend s
  => Mon (State s) a
  -> Mon (State s) a
preserveSeen m = do
  s <- Mon (Lens.use seenIdentifiers)
  a <- m
  Mon (seenIdentifiers Lens..= s)
  return a
