{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.GHCi.Common
  ( checkImportDirs
  , checkMonoLocalBinds
  , checkMonoLocalBindsMod
  , checkClashDynamic
  , getMainTopEntity
  ) where

-- Clash
import           Clash.Driver.Types     (ClashOpts (..), BindingMap)
import           Clash.Netlist.Types    (TopEntityT(..))

-- The GHC interface
import qualified DynFlags
import qualified EnumSet                as GHC (member)
import qualified GHC                    (DynFlags, ModSummary (..), Module (..),
                                         extensionFlags, moduleNameString)
import           Clash.Core.Name        (nameOcc)
import           Clash.Core.Var         (varName)
import           Clash.Normalize.Util   (collectCallGraphUniques, callGraph)
import qualified Clash.Util.Interpolate as I
import           Clash.Util             (ClashException(..), HasCallStack, noSrcSpan)
import           Clash.Unique           (getUnique)
import           Control.Exception      (throw)
import           Data.List              (isSuffixOf)
import qualified Data.Text              as Text
import qualified Data.HashSet           as HashSet
import qualified GHC.LanguageExtensions as LangExt (Extension (..))
import           Panic                  (GhcException (..), throwGhcException)

import           Control.Monad          (forM_, unless, when)
import           System.Directory       (doesDirectoryExist)
import           System.IO              (hPutStrLn, stderr)

getMainTopEntity
  :: HasCallStack
  => String
  -- ^ Module name
  -> BindingMap
  -- ^ Map of global binders
  -> [TopEntityT]
  -- ^ List of top entities loaded by LoadModules
  -> String
  -- ^ string passed with -main-is
  -> IO (TopEntityT, [TopEntityT])
  -- ^ Throws exception if -main-is was set, but no such top entity was found.
  -- Otherwise, returns main top entity and all top entities (transitively) used
  -- in the main top entity.
getMainTopEntity modName bindingMap topEnts nm =
  case filter isNm topEnts of
    [] -> throw $ ClashException noSrcSpan [I.i|
      Could not find top entity called #{show nm} in #{show modName}
    |] Nothing
    [t] ->
      let
        closure0 = collectCallGraphUniques (callGraph bindingMap (topId t))
        closure1 = HashSet.delete (getUnique (topId t)) closure0
      in
        pure (t, filter ((`HashSet.member` closure1) . getUnique . topId) topEnts)
    ts ->
      error $ [I.i|
        Internal error: multiple top entities called #{nm} (#{map topId ts})
        found in #{modName}.
      |]
 where
  isNm (TopEntityT{topId}) =
    let topIdNm = Text.unpack (nameOcc (varName topId)) in
    topIdNm == nm || ('.':nm) `isSuffixOf` topIdNm

-- | Checks whether MonoLocalBinds language extension is enabled or not in
-- modules.
checkMonoLocalBindsMod :: GHC.ModSummary -> IO ()
checkMonoLocalBindsMod x =
  unless (active . GHC.ms_hspp_opts $ x) (hPutStrLn stderr $ msg x)
  where
    msg = messageWith . GHC.moduleNameString . GHC.moduleName . GHC.ms_mod

-- | Checks whether MonoLocalBinds language extension is enabled when generating
-- the HDL directly e.g. in GHCi. modules.
checkMonoLocalBinds :: GHC.DynFlags -> IO ()
checkMonoLocalBinds dflags =
  unless (active dflags) (hPutStrLn stderr $ messageWith "")

messageWith :: String -> String
messageWith srcModule
  | srcModule == []  = msgStem ++ "."
  | otherwise = msgStem ++ " in module: " ++ srcModule
  where
    msgStem = "Warning: Extension MonoLocalBinds disabled. This might lead to unexpected logic duplication"

active :: GHC.DynFlags -> Bool
active = GHC.member LangExt.MonoLocalBinds . GHC.extensionFlags

checkImportDirs :: Foldable t => ClashOpts -> t FilePath -> IO ()
checkImportDirs opts idirs = when (opt_checkIDir opts) $
  forM_ idirs $ \dir -> do
    doesDirectoryExist dir >>= \case
      False -> throwGhcException (CmdLineError $ "Missing directory: " ++ dir)
      _     -> return ()

checkClashDynamic :: GHC.DynFlags -> IO ()
checkClashDynamic dflags = do
  let isStatic = case lookup "GHC Dynamic" (DynFlags.compilerInfo dflags) of
        Just "YES" -> False
        _          -> True
  when isStatic
    (hPutStrLn stderr (unlines
      ["WARNING: Clash is linked statically, which can lead to long startup times."
      ,"See https://gitlab.haskell.org/ghc/ghc/issues/15524"
      ]))
