module T2472 where

import Clash.Prelude
import Clash.Annotations.SynthesisAttributes

import Data.List (isInfixOf)
import qualified Prelude as P
import System.Environment (getArgs)
import System.FilePath ((</>))

-- BiSignalIn must be present at the top level.
-- Putting it inside a multifield record caused the assign to drive an internal wire.
type FtdiIO dom = "FTDI_D" ::: BiSignalIn 'Floating dom 8
                  `Annotate` 'StringAttr "LOC" "P1, P2, P4, N5, P5, M6, N6, M7"
                  `Annotate` 'StringAttr "IOSTANDARD" "LVTTL"

topEntity
  :: Clock System -- -> Reset System -> Enable System
  -> FtdiIO System -> BiSignalOut 'Floating System 8
topEntity clk io
  = withClockResetEnable clk rst en $ writeToBiSignal io (pure out)
  where
    out = Nothing :: Maybe (BitVector 8)
    rst = unsafeToReset $ pure False -- resetGen
    en = enableGen

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

-- VHDL test
main :: FilePath -> IO ()
main topFile = do
  content <- readFile topFile

  assertIn "io  : inout std_logic_vector(7 downto 0)" content

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  main (topDir </> show 'topEntity </> "topEntity.vhdl")
