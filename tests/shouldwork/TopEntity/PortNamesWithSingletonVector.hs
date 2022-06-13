module PortNamesWithSingletonVector where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

import Clash.Prelude
import Clash.Explicit.Testbench

{-# ANN topEntity
  (Synthesize
    { t_name     = "PortNamesWithSingletonVector_topEntity"
    , t_inputs   =
        [ PortName "inp0"
        , PortName "inp1"
        , PortName "inp2"
        , PortName "inp3"
        , PortName "inp4"
        , PortName "inp5"
        , PortName "inp6"
        ]
    , t_output = PortName "outp0"
    }) #-}
topEntity
  :: Bit
  -> BitVector 1
  -> Unsigned 1
  -> Vec 1 Bit
  -> Vec 1 (BitVector 1)
  -> Vec 1 (Unsigned 1)
  -> Vec 1 (Signed 1)
  -> Bool
topEntity _ _ _ _ _ _ _ = True

-- File content test
assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content  <- readFile (topDir </> show 'topEntity </> "PortNamesWithSingletonVector_topEntity.v")

  mapM_ (`assertIn` content)
    [ "input wire  inp0"
    , "input wire [0:0] inp1"
    , "input wire [0:0] inp2"
    , "input wire [0:0] inp3"
    , "input wire [0:0] inp4"
    , "input wire [0:0] inp5"
    , "input wire [0:0] inp6"
    ]
