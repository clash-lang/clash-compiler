{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import           System.Environment           (getArgs)
import           System.FilePath              (FilePath)

import           Clash.Driver                 (createTemporaryClashDirectory)
import           Control.Exception            (finally)
import           Data.Binary (encode)
import qualified Data.ByteString.Lazy as B
import           System.Directory             (removeDirectoryRecursive)

import           SerialiseInstances
import           BenchmarkCommon

main :: IO ()
main = do
  args <- getArgs
  let tests | null args = defaultTests
            | otherwise = args

  tmpDir <- createTemporaryClashDirectory

  finally (do
    mapM_ (prepareFile tmpDir) tests
   ) (
    removeDirectoryRecursive tmpDir
   )


prepareFile :: FilePath -> FilePath -> IO ()
prepareFile tmpDir fIn = do
  putStrLn $ "Preparing: " ++ fIn
  let fOut = fIn ++ ".bin"
  inp <- runInputStage tmpDir fIn
  let (bindingsMap,tcm,tupTcm,_topEntities,primMap,reprs,topEntityNames,topEntity) = inp
      inp' = (bindingsMap,tcm,tupTcm,_topEntities, fmap (fmap removeBBfunc) primMap, reprs,topEntityNames,topEntity)
  putStrLn $ "Serialising to : " ++ fOut
  B.writeFile fOut $ encode inp'
  putStrLn "Done"
