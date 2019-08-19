{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module Clash.GHCi.Common
  ( checkImportDirs,
    checkMonoLocalBinds
  ) where

-- Clash
import           Clash.Driver.Types     (ClashOpts (..))

-- The GHC interface
#if MIN_VERSION_base(4,11,0)
import qualified EnumSet                as GHC (member) -- ghc84, ghc86
#else
import qualified Data.IntSet            as IntSet -- ghc82
#endif
import qualified GHC                    (DynFlags, extensionFlags)
import qualified GHC.LanguageExtensions as LangExt (Extension (..))
import           Panic                  (GhcException (..), throwGhcException)

import           Control.Monad          (forM_, unless, when)
import           System.Directory       (doesDirectoryExist)
import           System.IO              (hPutStrLn, stderr)

checkMonoLocalBinds :: GHC.DynFlags -> IO ()
checkMonoLocalBinds dflags =
  unless (active dflags) (hPutStrLn stderr msg)
  where
    msg = "Warning: Extension MonoLocalBinds disabled. This might lead to unexpected logic duplication."
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
