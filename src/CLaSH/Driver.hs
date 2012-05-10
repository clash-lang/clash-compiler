{-# LANGUAGE FlexibleInstances #-}
module CLaSH.Driver where

import qualified Data.HashMap.Lazy as HashMap

import CLaSH.GHC.GHC2Core
import CLaSH.GHC.LoadModules
import CLaSH.Core.Pretty
import CLaSH.Core.Util

generateVHDL ::
  String
  -> IO ()
generateVHDL modName = do
  allBindings <- loadModules modName
  let convertedBindings = map (\(x,e) -> (coreToBndr x,coreToTerm e)) allBindings
  let printedBindings   = showDoc HashMap.empty convertedBindings
  putStrLn printedBindings
