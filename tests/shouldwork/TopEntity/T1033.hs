module Test2 where

import Clash.Prelude
import Data.List
import System.Environment
import qualified Prelude as P

{-# ANN topEntity
  (Synthesize
    { t_name   = "top"
    , t_inputs =
        [ PortName "theHiddenClock"
        , PortName "theHiddenReset"
        , PortName "theInput"
        ]
    , t_output = PortName "theOutput"
    }
  )#-}
topEntity
  :: ( HiddenClock System
     , HiddenReset System )
  => Signal System Int
  -> Signal System Int
topEntity = withEnable enableGen register 0

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

-- VHDL test
mainVHDL :: IO ()
mainVHDL = do
  [topFile] <- getArgs
  content <- readFile topFile

  assertIn "theHiddenClock" content
  assertIn "theHiddenReset" content
  assertIn "theInput" content
  assertIn "theOutput" content

mainVerilog :: IO ()
mainVerilog = mainVHDL

mainSystemVerilog :: IO ()
mainSystemVerilog = mainVHDL
