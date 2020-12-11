-- This tests the special, verlog only, code-path for index_int,
-- which generates special verilog when the index is constant.
module IndexInt2 where

import qualified Prelude as P
import Control.Monad (when)
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

import Clash.Prelude
import Clash.Netlist.Types

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

topEntity :: Vec 4 Bool -> Signed 8 -> (Bool,Bool)
topEntity xs ix0 = (xs !! ix0, xs !! ix1)
  where
    ix1 :: Signed 8
    ix1 = fold (+) (1 :> (-1) :> 1 :> Nil)

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- readFile (takeDirectory topDir </> "topEntity.v")
  let indexBegins = P.length $ filter (isInfixOf " index begin") $ lines content
  when (indexBegins /= 1) $
    error ("Expected 1 index blackbox, but found " <> show indexBegins)
