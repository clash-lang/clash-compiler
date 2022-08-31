{-|
Copyright  : (C) 2022, Google LLC
License    : BSD2 (see the file LICENSE)
Maintainer : QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.Driver.LocatedManifest where

import Clash.Driver.Manifest (Manifest, manifestFilename)
import System.FilePath ((</>), dropFileName)

import qualified Language.Haskell.TH  as TH

data LocatedManifest = LocatedManifest
  { -- | Path pointing to the manifest file itself
    lmPath :: FilePath

    -- | Manifest file corresponding to the one at 'lmPath'
  , lmManifest :: Manifest
  }

-- | Get manifest location from the HDL output directory and top entity name
manifestLocation :: FilePath -> TH.Name -> FilePath
manifestLocation hdlDir funcName = manifestLocationFromStrName hdlDir (show funcName)

-- | Get manifest location from the HDL output directory and top entity name
manifestLocationFromStrName :: FilePath -> String -> FilePath
manifestLocationFromStrName hdlDir funcName = hdlDir </> funcName </> manifestFilename

-- | Directory where all entities are stored. Probably equal to the value of
-- @-fclash-hdldir@ passed to Clash.
projectDirectory :: LocatedManifest -> FilePath
projectDirectory manifest = entityDirectory manifest </> ".."

-- | Directory where all HDL files are located for given entity
entityDirectory :: LocatedManifest -> FilePath
entityDirectory manifest = dropFileName (lmPath manifest)
