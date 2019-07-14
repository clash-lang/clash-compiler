{-# LANGUAGE LambdaCase #-}

module Clash.GHCi.Common
  ( checkImportDirs )
where

import           Clash.Driver.Types (ClashOpts(..))

import           Control.Monad      (when, forM_)
import           Panic              (throwGhcException, GhcException(..))
import           System.Directory   (doesDirectoryExist)

checkImportDirs :: Foldable t => ClashOpts -> t FilePath -> IO ()
checkImportDirs opts idirs = when (opt_checkIDir opts) $
  forM_ idirs $ \dir -> do
    doesDirectoryExist dir >>= \case
      False -> throwGhcException (CmdLineError $ "Missing directory: " ++ dir)
      _     -> return ()
