{-|
Copyright  : (C) 2022, Google LLC
License    : BSD2 (see the file LICENSE)
Maintainer : QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.Driver.LocatedManifest where

import Clash.Driver.Manifest (Manifest)
import System.FilePath ((</>), dropFileName)

data LocatedManifest = LocatedManifest
  { -- | Path pointing to the manifest file itself
    lmPath :: FilePath

    -- | Manifest file corresponding to the one at 'lmPath'
  , lmManifest :: Manifest
  }

-- | Directory where all entities are stored. Probably equal to the value of
-- @-fclash-hdldir@ passed to Clash.
projectDirectory :: LocatedManifest -> FilePath
projectDirectory manifest = entityDirectory manifest </> ".."

-- | Directory where all HDL files are located for given entity
entityDirectory :: LocatedManifest -> FilePath
entityDirectory manifest = dropFileName (lmPath manifest)
