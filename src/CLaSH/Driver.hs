{-# LANGUAGE FlexibleInstances #-}
module CLaSH.Driver where

import qualified Data.HashMap.Lazy as HashMap

import qualified CoreSyn
import qualified HscTypes

--import CLaSH.Core
import CLaSH.GHC.GHC2Core
import CLaSH.GHC.LoadModules
import CLaSH.Core.Pretty

generateVHDL ::
  String
  -> IO ()
generateVHDL modName = do
  coreModGuts <- loadModules modName
  let allBindings = concatMap (CoreSyn.flattenBinds . HscTypes.mg_binds) coreModGuts
  let convertedBindings = map (\(x,e) -> (coreToId x,coreToTerm e)) allBindings
  let printedBindings   = showDoc HashMap.empty convertedBindings
  putStr printedBindings
