{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module MultiResult where

import qualified Prelude as P

import Clash.Prelude
import Clash.Explicit.Testbench

import qualified Clash.Netlist.Ast.Type as N
import qualified Clash.Netlist.Types as N
import qualified Clash.Netlist.BlackBox.Types as N
import           Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, BlackBoxMeta (..), TemplateKind (TDecl), emptyBlackBoxMeta)
import Clash.Primitives.DSL

import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

-- | Ties off sh_ddr on AWS.
tieOffShDdr :: Clock dom -> Reset dom -> Signal dom Int -> (Signal dom Int, Signal dom Int)
tieOffShDdr !_clk !_rst !_ = (undefined, undefined)
{-# NOINLINE tieOffShDdr #-}
{-# ANN tieOffShDdr (blackBoxHaskell 'tieOffShDdr 'tieOffShDdrBBF def{bo_multiResult=True}) #-}

tieOffShDdrBBF :: BlackBoxFunction
tieOffShDdrBBF _isD _primName _args _ty = pure (Right (meta, bb))
  where
    bb = N.BBFunction (show 'tieOffShDdr) 0 tieOffShDdrTF
    meta = emptyBlackBoxMeta
      { bbKind = TDecl
      , bbResultNames = [ N.BBTemplate [N.Text "foo"]
                        , N.BBTemplate [N.Text "foo"] ]
      }

tieOffShDdrTF :: N.TemplateFunction
tieOffShDdrTF = N.TemplateFunction [] (const True) $ \bbCtx -> do
  let [(clk, N.Clock _), (reset, N.Reset _), (inp, inpType)] = tInputs bbCtx
  declarationReturn bbCtx "myBlock" (pure [inp, inp])

topEntity ::
  Clock System ->
  Reset System ->
  Signal System Int ->
  Signal System (Int, Int, Int)
topEntity clk rst x = bundle (a, b, pure 5)
 where
  (a, b) = tieOffShDdr @System clk rst x
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (1 :> 2 :> Nil)
    expectedOutput = outputVerifier' clk rst ((1, 1, 5) :> (2, 2, 5) :> Nil)
    done           = expectedOutput (topEntity clk rst testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.vhdl")
  assertIn "foo" content
  assertIn "foo_0" content
