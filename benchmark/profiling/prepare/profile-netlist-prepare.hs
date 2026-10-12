{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import           System.Environment           (getArgs)

import           Data.Binary (encode)
import qualified Data.ByteString.Lazy as B
import           Data.List                    (partition)

import           Clash.Driver.Types (ClashEnv(..), ClashDesign(..))
import           Clash.Rewrite.Scoped.Globals (toClashBindings)
import qualified Clash.Util.Supply as Supply

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
  (clashEnv, clashDesign, top) <- runNormalisationStage idirs fIn
  supply <- Supply.newSupply

  -- The bindings are serialized as bindings of Clash Core
  let inputs = ( toClashBindings supply (designBindings clashDesign)
               , designEntities clashDesign
               , fmap (fmap removeBBfunc) (envPrimitives clashEnv)
               , envTyConMap clashEnv
               , envCustomReprs clashEnv
               , top
               , envDomains clashEnv
               )

  putStrLn $ "Serialising to : " ++ fOut
  B.writeFile fOut $ encode (inputs :: NetlistInputs)
  putStrLn "Done"
