module T1074 where
import Clash.Explicit.Prelude
import Data.List
import System.Environment
import System.FilePath
import qualified Prelude as P
import GHC.Stack

{-# ANN topEntity
  (Synthesize
    { t_name   = "top"
    , t_inputs =
        [ PortName "dom1 ~ System", PortName "HasCallStack", PortName "dom2 ~ System"
        , PortProduct "" [PortName "theClock", PortName "theReset"]
        , PortName "theInput"
        ]
    , t_output = PortName "theResult"
    }
  )#-}
topEntity
  :: forall dom1
   . dom1 ~ System
  => HasCallStack
  => forall dom2
   . dom2 ~ System
  => (Clock dom1, Reset dom2)
  -> Signal dom1 Int
  -> Signal dom2 Int
topEntity (clk, rst) = id

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

main :: FilePath -> IO ()
main topFile = do
  content <- readFile topFile

  assertIn " theClock" content
  assertIn " theReset" content
  assertIn " theInput" content
  assertIn " theResult" content

mainVHDL :: IO ()
mainVHDL = do
  [topFile] <- getArgs
  main (replaceFileName topFile "top/top.vhdl")

mainVerilog :: IO ()
mainVerilog = do
  [topFile] <- getArgs
  main (replaceFileName topFile "top/top.v")

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  [topFile] <- getArgs
  main (replaceFileName topFile "top/top.sv")
