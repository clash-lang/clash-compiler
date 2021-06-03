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
  let fOut = fIn ++ ".net.bin"
  inp <- runNormalisationStage idirs fIn
  let (transformedBindings,topEntities,primMap,tcm,reprs,topEntity) = inp
      inp' :: NetlistInputs
      inp' = (transformedBindings,topEntities,fmap (fmap removeBBfunc) primMap,tcm,reprs,topEntity)
  putStrLn $ "Serialising to : " ++ fOut
  B.writeFile fOut $ encode inp'
  putStrLn "Done"
