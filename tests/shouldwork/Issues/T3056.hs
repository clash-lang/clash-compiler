module T3056 where

-- Preludes
import Clash.Prelude hiding (transpose)
import qualified Prelude as P

-- Other imports
import Data.List (isInfixOf, transpose)
import System.Environment (getArgs)
import System.FilePath ((</>))

topEntity
  :: SystemClockResetEnable
  => Signal System (BitVector 5)
  -> Signal System (Vec 4 Bit)
topEntity bv
  = bundle
  $ ((! 4) <$> bv)
  :> pure (bvLit ! 3)
  :> ((!! 2) <$> v)
  :> pure (vLit !! 1)
  :> Nil
 where
  v :: Signal System (Vec 5 Bit)
  v = bv2v <$> bv

  bvLit :: BitVector 5
  bvLit = 0b01010

  vLit :: Vec 5 Bit
  vLit = low :> high :> low :> high :> low :> Nil

assertNotIn :: String -> String -> IO ()
assertNotIn needle haystack =
  if needle `isInfixOf` haystack
    then let
      prefixLinesWith :: String -> String -> String
      prefixLinesWith pfx str = unlines $ (pfx P.++) <$> lines str

      needleFmt = prefixLinesWith "  " needle
      haystackFmt = prefixLinesWith "  " haystack
    in P.error $ P.concat ["Unexpected:\n\n", needleFmt, "In:\n\n", haystackFmt]
    else return ()

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.v")
  assertNotIn "[(64'" content
