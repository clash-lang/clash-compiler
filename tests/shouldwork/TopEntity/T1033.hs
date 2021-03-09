module T1033 where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClock, hasClock)
import Data.List
import System.Environment
import System.FilePath
import qualified Prelude as P

{-# ANN topEntity
  (Synthesize
    { t_name   = "top"
    , t_inputs =
        [ PortName "theHiddenClock"
        , PortProduct "en_int" [PortName "theEnable", PortName "a"]
        , PortProduct "int_int_rst" [PortName "b", PortProduct "int_rst" []]
        ]
    , t_output = PortName "theOutput"
    }
  )#-}
topEntity
  :: ( dom ~ System        -- gets the 1st name, just like it would be ~ARG[0] in a blackbox
     , HiddenClock dom )   -- Hidden* consists of some construct and a
                           -- KnownDomain. We don't want them to split off.
  => (Enable dom, Signal dom Int)
  -- ^ We do want Enable/Signal to be split
  -> (Signal dom Int, (Signal dom Int, Reset dom))
  -- ^ And we want to do that recursively
  -> Signal dom Int
topEntity (en, a) (b, (c, rst)) =
  register hasClock rst en 0 (a + b + c)

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

-- VHDL test
main :: FilePath -> IO ()
main topFile = do
  content <- readFile topFile

  assertIn "theHiddenClock" content
  assertIn "en_int_theEnable" content
  assertIn "en_int_a" content
  assertIn "int_int_rst_b" content
  assertIn "int_int_rst_int_rst_0" content
  assertIn "int_int_rst_int_rst_1" content

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  main (topDir </> show 'topEntity </> "top.vhdl")

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  main (topDir </> show 'topEntity </> "top.v")

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  [topDir] <- getArgs
  main (topDir </> show 'topEntity </> "top.sv")
