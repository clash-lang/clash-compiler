{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import           System.Environment           (getArgs)

import           Data.Binary (encode)
import qualified Data.ByteString.Lazy as B
import           Data.List                    (partition)

import           Clash.Driver.Types           (ClashEnv(..), ClashDesign(..))
import           Clash.Netlist.Types          (TopEntityT(topId))

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
  (clashEnv, clashDesign) <- runInputStage idirs fIn

  let topNames = fmap topId (designEntities clashDesign)

  case topNames of
    topName:_ -> do
      let inputs = ( designBindings clashDesign
                   , envTyConMap clashEnv
                   , envTupleTyCons clashEnv
                   , fmap (fmap removeBBfunc) (envPrimitives clashEnv)
                   , envCustomReprs clashEnv
                   , topNames
                   , topName
                   , envDomains clashEnv
                   )

      putStrLn $ "Serialising to : " ++ fOut
      B.writeFile fOut $ encode inputs
      putStrLn "Done"
    _ -> error "no top entities"
