{-|
Functions to read, write, and handle manifest files.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Driver.Manifest where

import           Control.Exception (tryJust)
import           Control.Monad (guard)
import           Control.Monad.State (evalState)
import qualified Crypto.Hash.SHA256 as Sha256
import qualified Data.ByteString.Lazy as ByteStringLazy
import           Data.ByteString (ByteString)
import           Data.Hashable (hash)
import           Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Text (Text)
import qualified Data.Text.Lazy as LText
import           Data.Text.Prettyprint.Doc.Extra (renderOneLine)
import           Data.Time (UTCTime)
import qualified Data.Set as Set
import           Data.Semigroup.Monad (getMon)
import           Text.Read (readMaybe)
import           System.IO.Error (isDoesNotExistError)
import           System.FilePath (takeDirectory, (</>))
import           System.Directory (listDirectory, doesFileExist)

import           Clash.Annotations.TopEntity.Extra ()
import           Clash.Backend (Backend (hdlType), Usage (External))
import           Clash.Driver.Types
import           Clash.Primitives.Types
import           Clash.Core.Var (Id)
import           Clash.Netlist.Types (TopEntityT, Component(..), HWType)
import qualified Clash.Netlist.Types as Netlist
import qualified Clash.Netlist.Id as Id
import           Clash.Primitives.Util (hashCompiledPrimMap)
import           Clash.Util.Graph (callGraphBindings)

#if MIN_VERSION_ghc(9,0,0)
import GHC.Utils.Misc (OverridingBool(..))
#else
import Util (OverridingBool(..))
#endif

data ManifestPort = ManifestPort
  { mpName :: Text
  -- ^ Port name (as rendered in HDL)
  , mpTypeName :: Text
  -- ^ Type name (as rendered in HDL)
  } deriving (Show,Read)

-- | Information about the generated HDL between (sub)runs of the compiler
data Manifest
  = Manifest
  { manifestHash :: Int
    -- ^ Hash of the TopEntity and all its dependencies
  , successFlags  :: (Int,Int,Bool)
    -- ^ Compiler flags used to achieve successful compilation:
    --
    --   * opt_inlineLimit
    --   * opt_specLimit
    --   * opt_floatSupport
  , inPorts :: [ManifestPort]
  , outPorts :: [ManifestPort]
  , componentNames :: [Text]
    -- ^ Names of all the generated components for the @TopEntity@ (does not
    -- include the names of the components of the @TestBench@ accompanying
    -- the @TopEntity@).
  , topComponent :: Text
    -- ^ Design entry point. This is usually the component annotated with a
    -- @TopEntity@ annotation.
  , fileNames :: [(FilePath, ByteString)]
    -- ^ Names and hashes of all the generated files for the @TopEntity@. Hashes
    -- are SHA256.
  } deriving (Show,Read)

data UnexpectedModification
  -- | Clash generated file was modified
  = Modified FilePath
  -- | Non-clash generated file was added
  | Added FilePath
  -- | Clash generated file was removed
  | Removed FilePath
  deriving (Show)

mkManifestPort ::
  Backend backend =>
  -- | Backend used to lookup port type names
  backend ->
  -- | Port name
  Id.Identifier ->
  -- | Port type
  HWType ->
  ManifestPort
mkManifestPort backend portId portType = ManifestPort{..}
 where
  mpName = Id.toText portId
  mpTypeName = flip evalState backend $ getMon $ do
     LText.toStrict . renderOneLine <$> hdlType (External mpName) portType

mkManifest ::
  Backend backend =>
  -- | Backend used to lookup port type names
  backend ->
  -- | Options Clash was run with
  ClashOpts ->
  -- | Component of top entity
  Component ->
  -- | All other entities
  [Component] ->
  -- | Files and  their hashes
  [(FilePath, ByteString)] ->
  -- | Hash returned by 'readFreshManifest'
  Int ->
  -- | New manifest
  Manifest
mkManifest backend ClashOpts{..} Component{..} components files topHash = Manifest
  { manifestHash = topHash
  , inPorts = [mkManifestPort backend pName pType | (pName, pType) <- inputs]
  , outPorts = [mkManifestPort backend pName pType | (_, (pName, pType), _) <- outputs]
  , componentNames = map Id.toText compNames
  , topComponent = Id.toText componentName
  , fileNames = files
  , successFlags = (opt_inlineLimit, opt_specLimit, opt_floatSupport)
  }
 where
  compNames = map Netlist.componentName components

-- | Pretty print an unexpected modification as a list item.
pprintUnexpectedModification :: UnexpectedModification -> String
pprintUnexpectedModification = \case
  Modified p -> "Unexpected modification in " <> p
  Added p -> "Unexpected extra file " <> p
  Removed p -> "Unexpected removed file " <> p

-- | Pretty print a list of unexpected modifications. Print a maximum of /n/
-- modifications.
pprintUnexpectedModifications :: Int -> [UnexpectedModification] -> String
pprintUnexpectedModifications 0 us = pprintUnexpectedModifications maxBound us
pprintUnexpectedModifications _ [] = []
pprintUnexpectedModifications _ [u] = "* " <> pprintUnexpectedModification u
pprintUnexpectedModifications 1 (u:us) =
  "* and " <> show (length (u:us)) <> " more unexpected changes"
pprintUnexpectedModifications n (u:us) =
  "* " <> pprintUnexpectedModification u
        <> "\n" <> pprintUnexpectedModifications (n-1) us

-- | Reads a manifest file. Does not return manifest file if:
--
--  * Caching is disabled through @-fclash-no-cache@.
--  * Manifest could not be found.
--  * Cache is stale. This could be triggered by any of the given arguments.
--
-- Raises an exception if the manifest file or any of the files it is referring
-- to was inaccessible.
--
readFreshManifest ::
  -- | "This" top entity plus all that depend on it.
  [TopEntityT] ->
  -- | Core expressions and entry point. Any changes in the call graph will
  -- trigger a recompile.
  (BindingMap, Id) ->
  -- | Any changes in any primitive will trigger a recompile.
  CompiledPrimMap ->
  -- | Certain options will trigger recompiles if changed
  ClashOpts ->
  -- | Clash modification date
  UTCTime ->
  -- | Path to manifest file.
  FilePath ->
  -- | ( Nothing if no manifest file was found
  --   , Nothing on stale cache, disabled cache, or not manifest file found )
  IO (Maybe [UnexpectedModification], Maybe Manifest, Int)
readFreshManifest tops (bindingsMap, topId) primMap opts@(ClashOpts{..}) clashModDate path = do
  manifestM <- readManifest path
  modificationsM <- traverse (isUserModified path) manifestM

  pure
    ( modificationsM
    , checkManifest =<< if opt_cachehdl then manifestM else Nothing
    , topHash
    )

 where
  optsHash = hash opts {
      -- Ignore the following settings, they don't affect the generated HDL:

      -- 1. Debug
      opt_dbgLevel = DebugNone
    , opt_dbgTransformations = Set.empty
    , opt_dbgRewriteHistoryFile = Nothing

      -- 2. Caching
    , opt_cachehdl = True

      -- 3. Warnings
    , opt_primWarn = True
    , opt_color = Auto
    , opt_errorExtra = False
    , opt_checkIDir = True

      -- 4. Optional output
    , opt_edalize = False

      -- Ignore the following settings, they don't affect the generated HDL. However,
      -- they do influence whether HDL can be generated at all.
      --
      -- We therefore check whether the new flags changed in such a way that
      -- they could affect successful compilation, and use that information
      -- to decide whether to use caching or not (see: XXXX).
      --
      -- 1. termination measures
    , opt_inlineLimit = 20
    , opt_specLimit = 20

      -- 2. Float support
    , opt_floatSupport = False

      -- Finally, also ignore the HDL dir setting, because when a user moves the
      -- entire dir with generated HDL, they probably still want to use that as
      -- a cache
    , opt_hdlDir = Nothing
    }

  topHash = hash
    ( tops
    , hashCompiledPrimMap primMap
    , show clashModDate
    , callGraphBindings bindingsMap topId
    , optsHash
    )

  checkManifest manifest@Manifest{manifestHash,successFlags}
    | (cachedInline, cachedSpec, cachedFloat) <- successFlags

    -- Higher limits shouldn't affect HDL
    , cachedInline <= opt_inlineLimit
    , cachedSpec <= opt_specLimit

    -- /Enabling/ float support should compile more designs. Of course, keeping
    -- the same value for float support shouldn't invalidate caches either.
    , ((cachedFloat && not opt_floatSupport) || (cachedFloat == opt_floatSupport))

    -- Callgraph hashes should correspond
    , manifestHash == topHash
    = Just manifest

    -- One or more checks failed
    | otherwise = Nothing

-- | Determines whether the HDL directory the given 'LocatedManifest' was found
-- in contains any user made modifications. This is used by Clash to protect the
-- user against lost work.
isUserModified :: FilePath -> Manifest -> IO [UnexpectedModification]
isUserModified (takeDirectory -> topDir) Manifest{fileNames,topComponent} = do
  let
    manifestFiles = Set.fromList (map fst fileNames)
    manFile = Text.unpack topComponent <> ".manifest"

  currentFiles <- (Set.delete manFile . Set.fromList) <$> listDirectory topDir

  let
    removedFiles = Set.toList (manifestFiles `Set.difference` currentFiles)
    addedFiles = Set.toList (currentFiles `Set.difference` manifestFiles)

  changedFiles <- catMaybes <$> mapM detectModification fileNames

  pure
    (  map Removed removedFiles
    <> map Added addedFiles
    <> map Modified changedFiles )
 where
  detectModification :: (FilePath, ByteString) -> IO (Maybe FilePath)
  detectModification (filename, manifestDigest) = do
    let fullPath = topDir </> filename
    fileExists <- doesFileExist fullPath
    if fileExists then do
      contents <- ByteStringLazy.readFile fullPath
      if manifestDigest == Sha256.hashlazy contents
      then pure Nothing
      else pure (Just filename)
    else
      -- Will be caught by @removedFiles@
      pure Nothing

-- | Read a manifest file from disk. Returns 'Nothing' if file does not exist.
-- Any other IO exception is re-raised.
readManifest :: FilePath -> IO (Maybe Manifest)
readManifest path = do
  contentsE <- tryJust (guard . isDoesNotExistError) (readFile path)
  pure (either (const Nothing) readMaybe contentsE)

-- | Write manifest file to disk
writeManifest :: Manifest -> FilePath -> IO ()
writeManifest man path = Text.writeFile path (serializeManifest man)

-- | Serialize a manifest.
--
-- TODO: This should really yield a 'ByteString'.
serializeManifest :: Manifest -> Text
serializeManifest = Text.pack . show
