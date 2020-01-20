{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Clash.GHCi.Common
  ( checkImportDirs
  , checkMonoLocalBinds
  , checkMonoLocalBindsMod
  , checkClashDynamic
  ) where

-- Clash
import           Clash.Driver.Types     (ClashOpts (..))

-- The GHC interface
import qualified DynFlags
#if MIN_VERSION_base(4,11,0)
import qualified EnumSet                as GHC (member) -- ghc84, ghc86
#else
import qualified Data.IntSet            as IntSet -- ghc82
#endif
import qualified GHC                    (DynFlags, ModSummary (..), Module (..),
                                         extensionFlags, moduleNameString)
import qualified GHC.LanguageExtensions as LangExt (Extension (..))
import           Panic                  (GhcException (..), throwGhcException)

import           Control.Monad          (forM_, unless, when)
import           System.Directory       (doesDirectoryExist)
import           System.IO              (hPutStrLn, stderr)

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
#if MIN_VERSION_base(4,11,0)
-- ghc84, ghc86
active = GHC.member LangExt.MonoLocalBinds . GHC.extensionFlags
#else
-- ghc82
active = member LangExt.MonoLocalBinds . GHC.extensionFlags

member :: Enum a => a -> IntSet.IntSet -> Bool
member = IntSet.member . fromEnum
#endif

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
