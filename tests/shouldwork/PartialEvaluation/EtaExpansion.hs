{-# LANGUAGE OverloadedStrings #-}

-- Data constructor and primitives are only considered to be values when
-- they are fully applied. However, this creates a problem: what does it mean
-- when one of these is partially applied?
--
-- The solution to this problem taken is to eta-expand partially applied data
-- or primitives during evaluation. This means that we can expect something
-- like
--
--   Just x     ~> Data "Just" [x]
--   Just       ~> Lam eta (Data "Just" [eta])
--
-- another example, a binary primop:
--
--   x + y      ~> Prim "+" [x, y]
--   (x +)      ~> Lam eta (Prim "+" [x, eta])
--   (+)        ~> Lam eta1 (Lam eta2 (Prim "+" [eta1, eta2]))
--
-- A side-effect of this is that the following definitions should yield the
-- same Clash Core (even though the GHC core is potentially different):
--
--   foo   = Just
--   bar x = Just x
--
module EtaExpansion where

import Control.Monad (unless)

import Clash.Prelude

import Clash.Backend
import Clash.Core.Evaluator.Models
import Clash.Core.Subst
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Var
import Clash.Core.VarEnv
import Clash.Driver.Types

import Clash.GHC.PartialEval

import Test.Tasty.Clash
import Test.Tasty.Clash.CoreTest

{-# NOINLINE etaReducedData #-}
{-# ANN etaReducedData (Synthesize
          { t_name   = "etaReducedData"
          , t_inputs = [PortName "x"]
          , t_output = PortName "res"
          })
  #-}
etaReducedData :: Integer -> Maybe Integer
etaReducedData = Just

{-# NOINLINE etaExpandedData #-}
{-# ANN etaExpandedData (Synthesize
          { t_name   = "etaExpandedData"
          , t_inputs = [PortName "x"]
          , t_output = PortName "res"
          })
  #-}
etaExpandedData :: Integer -> Maybe Integer
etaExpandedData x = Just x

{-# NOINLINE etaReducedPrim1 #-}
{-# ANN etaReducedPrim1 (Synthesize
          { t_name   = "etaReducedPrim1"
          , t_inputs = [PortName "x", PortName "y"]
          , t_output = PortName "res"
          })
  #-}
etaReducedPrim1 :: Integer -> Integer -> Integer
etaReducedPrim1 = (+)

{-# NOINLINE etaReducedPrim2 #-}
{-# ANN etaReducedPrim2 (Synthesize
          { t_name   = "etaReducedPrim2"
          , t_inputs = [PortName "x", PortName "y"]
          , t_output = PortName "res"
          })
  #-}
etaReducedPrim2 :: Integer -> Integer -> Integer
etaReducedPrim2 x = (x+)

{-# NOINLINE etaExpandedPrim #-}
{-# ANN etaExpandedPrim (Synthesize
          { t_name   = "etaExpandedPrim"
          , t_inputs = [PortName "x", PortName "y"]
          , t_output = PortName "res"
          })
  #-}
etaExpandedPrim :: Integer -> Integer -> Integer
etaExpandedPrim x y = x + y

testPath :: FilePath
testPath = "tests/shouldwork/PartialEvaluation/EtaExpansion.hs"

mainCommon
  :: (Backend (TargetToState target))
  => SBuildTarget target
  -> IO ()
mainCommon hdl = do
  entities <- runToCoreStage hdl id testPath

  -- Eta Expansion of Data Constructors
  let data1 = findBinding "EtaExpansion.etaReducedData" entities
      data2 = findBinding "EtaExpansion.etaExpandedData" entities

  unless (aeqTerm (asTerm data1) (asTerm data2)) $
    error ("Not alpha equivalent: " <> show data1 <> "\n\n" <> show data2)

  -- Eta Expansion of Primitive Operations
  let prim1 = findBinding "EtaExpansion.etaReducedPrim1" entities
      prim2 = findBinding "EtaExpansion.etaReducedPrim2" entities
      prim3 = findBinding "EtaExpansion.etaExpandedPrim" entities

  unless (aeqTerm (asTerm prim1) (asTerm prim2) && aeqTerm (asTerm prim1) (asTerm prim3)) $
    error ("Not alpha equivalent: " <> show prim1 <> "\n\n" <> show prim2 <> "\n\n" <> show prim3)

mainVHDL :: IO ()
mainVHDL = mainCommon SVHDL

mainVerilog :: IO ()
mainVerilog = mainCommon SVerilog

mainSystemVerilog :: IO ()
mainSystemVerilog = mainCommon SSystemVerilog

