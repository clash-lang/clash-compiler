module AnnotateReg where

import qualified Prelude as P

import Clash.Explicit.Prelude
import Clash.Annotations.SynthesisAttributes
import Data.Proxy


import Data.List
import System.Environment (getArgs)
import System.FilePath ((</>))


annotateFlipflop ::
  forall (name :: Symbol) n dom a .
  (KnownDomain dom, NFDataX a) =>
  Proxy name ->
  Vec n (Attr String) -> Clock dom -> Signal dom a -> Signal dom a
annotateFlipflop _ attrs clk x = setName @name (annotateReg attrs (dflipflop clk x))

topEntity ::
  Clock System ->
  Signal System Bool ->
  Signal System Bool
topEntity clk = fmap not . annotateFlipflop (Proxy @"my_signal") (Attr "ASYNC_REG" :> Nil) clk

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- P.readFile (topDir </> show 'topEntity </> "topEntity.vhdl")

  assertIn "attribute ASYNC_REG : boolean;" content
  assertIn "attribute ASYNC_REG of my_signal : signal is true;" content

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- P.readFile (topDir </> show 'topEntity </> "topEntity.v")

  assertIn "(* ASYNC_REG *) reg  my_signal" content

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  [topDir] <- getArgs
  content <- P.readFile (topDir </> show 'topEntity </> "topEntity.sv")

  assertIn "(* ASYNC_REG *) logic my_signal" content
