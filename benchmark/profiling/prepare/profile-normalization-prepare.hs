{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import           System.Environment           (getArgs)

import           Data.Binary (encode)
import qualified Data.ByteString.Lazy as B
import           Data.List                    (partition)

import           SerialiseInstances
import           BenchmarkCommon

main :: IO ()
main = do
  args <- getArgs
  let (idirs0,tests0) = partition ((== "-i") . take 2) args
  let tests1 | null tests0 = defaultTests
             | otherwise   = tests0
      idirs1 = ".":map (drop 2) idirs0

  mapM_ (prepareFile idirs1) tests1

prepareFile :: [FilePath] -> FilePath -> IO ()
prepareFile idirs fIn = do
  putStrLn $ "Preparing: " ++ fIn
  let fOut = fIn ++ ".bin"
  inp <- runInputStage idirs fIn
  let (bindingsMap,tcm,tupTcm,_topEntities,primMap,reprs,topEntityNames,topEntity) = inp
      inp' :: NormalizationInputs
      inp' = (bindingsMap,tcm,tupTcm, fmap (fmap removeBBfunc) primMap, reprs,topEntityNames,topEntity)
  putStrLn $ "Serialising to : " ++ fOut
  B.writeFile fOut $ encode inp'
  putStrLn "Done"
