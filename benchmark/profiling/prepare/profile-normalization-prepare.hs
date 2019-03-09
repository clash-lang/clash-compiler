{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import           System.Environment           (getArgs)
import           System.FilePath              (FilePath)

import           Clash.Driver                 (createTemporaryClashDirectory)
import           Control.Exception            (finally)
import           Data.Binary (encode)
import qualified Data.ByteString.Lazy as B
import           Data.List                    (partition)
import           System.Directory             (removeDirectoryRecursive)

import           SerialiseInstances
import           BenchmarkCommon

main :: IO ()
main = do
  args <- getArgs
  let (idirs0,tests0) = partition ((== "-i") . take 2) args
  let tests1 | null tests0 = defaultTests
             | otherwise   = tests0
      idirs1 = ".":map (drop 2) idirs0

  tmpDir <- createTemporaryClashDirectory

  finally (do
    mapM_ (prepareFile tmpDir idirs1) tests1
   ) (
    removeDirectoryRecursive tmpDir
   )


prepareFile :: FilePath -> [FilePath] -> FilePath -> IO ()
prepareFile tmpDir idirs fIn = do
  putStrLn $ "Preparing: " ++ fIn
  let fOut = fIn ++ ".bin"
  inp <- runInputStage tmpDir idirs fIn
  let (bindingsMap,tcm,tupTcm,_topEntities,primMap,reprs,topEntityNames,topEntity) = inp
      inp' = (bindingsMap,tcm,tupTcm,_topEntities, fmap (fmap removeBBfunc) primMap, reprs,topEntityNames,topEntity)
  putStrLn $ "Serialising to : " ++ fOut
  B.writeFile fOut $ encode inp'
  putStrLn "Done"
