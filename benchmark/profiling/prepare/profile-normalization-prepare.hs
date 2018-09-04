{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import           System.Environment           (getArgs)
import           System.FilePath              (FilePath)

import           Data.Binary (encode)
import qualified Data.ByteString.Lazy as B

import           SerialiseInstances
import           BenchmarkCommon

main :: IO ()
main = do
  args <- getArgs
  let tests | null args = defaultTests
            | otherwise = args
  mapM_ prepareFile tests

prepareFile :: FilePath -> IO ()
prepareFile fIn = do
  putStrLn $ "Preparing: " ++ fIn
  let fOut = fIn ++ ".bin"
  inp <- runInputStage fIn
  let (bindingsMap,tcm,tupTcm,_topEntities,primMap,reprs,topEntityNames,topEntity) = inp
      inp' = (bindingsMap,tcm,tupTcm,_topEntities, fmap removeBBfunc primMap, reprs,topEntityNames,topEntity)
  putStrLn $ "Serialising to : " ++ fOut
  B.writeFile fOut $ encode inp'
  putStrLn "Done"
