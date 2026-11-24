module AltPll where

import Clash.Explicit.Prelude
import Clash.Intel.ClockGen (altpllSync)
import Control.Monad (unless)
import Data.List (isInfixOf)
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>), takeExtension)

createDomain vSystem{vName="PllIn", vPeriod=8000.25}
createDomain vSystem{vName="PllOut", vPeriod=5333.5}

topEntity :: Clock PllIn -> Reset PllIn -> (Clock PllOut, Reset PllOut)
topEntity = altpllSync

mainVerilog :: IO ()
mainVerilog = do
  [root] <- getArgs
  let dir = root </> show 'topEntity
  files <- listDirectory dir
  [qsys] <- pure [f | f <- files, takeExtension f == ".qsys"]
  xml <- readFile (dir </> qsys)
  let check name value =
        unless (("name=" <> show name <> " value=" <> show value) `isInfixOf` xml) $
          fail ("Expected " <> name <> " = " <> value <> " in " <> qsys)
  check "CLK0_MULTIPLY_BY" "3"
  check "CLK0_DIVIDE_BY" "2"
  check "INCLK0_INPUT_FREQUENCY" "8000"
