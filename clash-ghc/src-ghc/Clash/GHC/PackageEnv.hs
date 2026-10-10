{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Make the packages Clash needs to compile designs available when no package
  environment provides them, e.g. when Clash was installed using
  @cabal install@.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.GHC.PackageEnv
  ( preludePkgId
  , addInstallationPackageEnv
  ) where

import           Control.Monad                   (filterM)
import qualified Data.Set                        as Set
import           Language.Haskell.TH.Syntax      (lift, namePackage)
import           System.Directory                (doesDirectoryExist, doesFileExist)
import           System.FilePath                 ((<.>), (</>))

import           GHC.Driver.Session
  (DynFlags, GeneralFlag (Opt_HideAllPackages), ModRenaming (..),
   PackageArg (..), PackageDBFlag (..), PackageFlag (..), PkgDbRef (..),
   gopt_set)
import qualified GHC.Driver.Session              as DynFlags
import           GHC.Unit.Info                   (unitPackageNameString)
import           GHC.Unit.State
  (UnitState (..), initUnits, lookupUnit, unwireUnit)
import           GHC.Unit.Types
  (Definite (..), GenUnit (..), Unit, stringToUnitId, unitString)
import           GHC.Utils.Error                 (compilationProgressMsg)
import           GHC.Utils.Logger                (Logger)
import           GHC.Utils.Outputable            (text)

import qualified GHC.TypeLits.Extra.Solver
import qualified GHC.TypeLits.KnownNat.Solver
import qualified GHC.TypeLits.Normalise

import           Clash.Annotations.TopEntity     (TopEntity)
import           Clash.Util                      (pkgIdFromTypeable)

import           PackageDbs_clash_ghc            (packageDbs)

-- | The package id of the clash-prelude we were built with
preludePkgId :: String
preludePkgId = $(lift $ pkgIdFromTypeable (undefined :: TopEntity))

-- | The unit ids of the packages Clash needs to compile designs: the
-- clash-prelude and type checker plugins we were built with.
clashUnitIds :: [String]
clashUnitIds =
  [ preludePkgId
  , $(maybe (fail "No unit id") lift (namePackage 'GHC.TypeLits.Normalise.plugin))
  , $(maybe (fail "No unit id") lift (namePackage 'GHC.TypeLits.Extra.Solver.plugin))
  , $(maybe (fail "No unit id") lift (namePackage 'GHC.TypeLits.KnownNat.Solver.plugin))
  ]

-- | Make the packages Clash was built with available if the given flags do not
-- make any clash-prelude visible.
--
-- GHC only finds packages through its global package database, package
-- environment files, and flags such as @-package-db@. When Clash is installed
-- using @cabal install@, nothing points GHC at Cabal's store, where the
-- packages Clash needs are installed. In that case we add the package
-- databases Clash was built against, and expose the clash-prelude and plugins
-- Clash was built with.
--
-- Package databases contain all kinds of packages, often multiple instances
-- of the same one. To prevent those from becoming visible, we hide all
-- packages and expose the ones that were visible before explicitly. This
-- mimics GHC's package environment files.
--
-- The flags are left alone if they make any clash-prelude visible, so package
-- environments keep working as before. That includes reporting an error if
-- they provide a different clash-prelude than the one Clash was built with.
--
-- This must be called before the flags are first set in the session, as GHC
-- does not reread package databases afterwards.
addInstallationPackageEnv :: Logger -> DynFlags -> IO DynFlags
addInstallationPackageEnv logger dflags = do
  (_, unitState, _, _) <-
    initUnits logger dflags Nothing (Set.singleton (DynFlags.homeUnitId_ dflags))
  let visible = explicitUnits unitState
  if any (isPrelude unitState . fst) visible then
    pure dflags
  else builtWithPackageDbs >>= \case
    [] -> pure dflags
    dbs -> do
      let loaded db = "Loaded package database Clash was built against from " <> db
      mapM_ (compilationProgressMsg logger . text . loaded) dbs
      let
        -- Units that were exposed by flags will be exposed by the same flags
        -- again, so we only have to deal with the ones exposed by default.
        defaults = [unwireUnit unitState u | (u, Nothing) <- visible]
        clashUnits = map (RealUnit . Definite . stringToUnitId) clashUnitIds

      -- Both lists are stored in reverse command line order. We put the
      -- package databases last so a @clear-package-db@ cannot remove them,
      -- and expose packages first so a @-hide-package@ can still hide them.
      pure (gopt_set dflags Opt_HideAllPackages)
        { DynFlags.packageDBFlags =
            reverse (map (PackageDB . PkgDbPath) dbs) ++ DynFlags.packageDBFlags dflags
        , DynFlags.packageFlags =
            DynFlags.packageFlags dflags ++ map exposeUnit (defaults ++ clashUnits)
        }
 where
  isPrelude unitState u =
    maybe False ((== "clash-prelude") . unitPackageNameString) (lookupUnit unitState u)

-- | Equivalent of passing @-package-id@ on the command line
exposeUnit :: Unit -> PackageFlag
exposeUnit u =
  ExposePackage ("-package-id " <> unitString u) (UnitIdArg u) (ModRenaming True [])

-- | The package databases Clash was built against, if they contain the
-- packages Clash needs. These are recorded by @SetupHooks.hs@ when building
-- @clash-ghc@.
builtWithPackageDbs :: IO [FilePath]
builtWithPackageDbs = do
  dbs <- filterM doesDirectoryExist packageDbs
  let inDbs uid = or <$> mapM (\db -> doesFileExist (db </> uid <.> "conf")) dbs
  found <- and <$> mapM inDbs clashUnitIds
  pure (if found then dbs else [])
