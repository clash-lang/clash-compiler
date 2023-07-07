{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module SplitOutHiddenClkRstEna where
import Prelude as P

import Clash.Prelude
import qualified Clash.Explicit.Prelude as E
import Clash.Netlist.Types
import qualified Clash.Util.Interpolate as I
import qualified Clash.Netlist.Id as Id

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest
import Control.Monad

--
-- {- # ANN test0
--   (Synthesize
--     { t_name   = "test0"
--     , t_inputs = [ PortProduct "" [PortName "clk", PortName "rst", PortName "ena"], PortName "theInput"]
--     , t_output = PortName "theOutput"
--     }
--   )#-}

-- topEntity :: HiddenClockResetEnable System => Signal System (Unsigned 8) -> Signal System (Unsigned 8)
-- topEntity = test1_subcomp

{-# NOINLINE test1_subcomp #-}
test1_subcomp :: HiddenClockResetEnable dom => Signal dom (Unsigned 8) -> Signal dom (Unsigned 8)
test1_subcomp = register 1

{-# ANN test1 (defSyn "test1") #-}
test1 :: HiddenClockResetEnable System => Signal System (Unsigned 8) -> Signal System (Unsigned 8)
test1 = test1_subcomp

--
--
--
{- # ANN test2 (defSyn "test2") #-}

-- TODO uncommenting the following 2 lines changes the type of the test1_subcomp
-- test2 :: (Clock System, Reset System, Enable System) -> Signal System (Unsigned 8) -> Signal System (Unsigned 8)
-- test2 (clk,rst,ena) = E.register clk rst ena 2
--
-- data DataTup1 a = DataTup1 a
-- newtype NewTyTup1 a = NewTyTup1 a
--
-- {-  # ANN test3 (defSyn "test3") #-}
-- test3 :: DataTup1 (Clock System, Reset System, Enable System) -> Signal System (Unsigned 8) -> Signal System (Unsigned 8)
-- test3 (DataTup1 (clk,rst,ena)) = E.register clk rst ena 3
--
-- {- # ANN test4 (defSyn "test4") #-}
-- test4 :: NewTyTup1 (Clock System, Reset System, Enable System) -> Signal System (Unsigned 8) -> Signal System (Unsigned 8)
-- test4 (NewTyTup1 (clk,rst,ena)) = E.register clk rst ena 4
-- --
--
-- testPath :: FilePath
-- testPath = __FILE__
--
-- assertArgTys :: Component -> IO ()
-- assertArgTys (Component compNm args _ _) = do
--   unless (argTys == expected)
--     $ error $ "Unexpected arguments for component " <> show (Id.toText compNm) <> ": " <> show argTys
--               <> "\nExpected: " <> show expected
--  where
--   argTys = P.map snd args
--   expected = [Clock "System", Reset "System", Enable "System",(Unsigned 8)]
-- -- TODO check that we're not creating intermediate signals?
--
-- mainVHDL :: IO ()
-- mainVHDL = do
--   components <- runToNetlistStage SVHDL id testPath
--   let nrComponents = P.length components
--       nrComponentsExpected = 5-3
--   mapM_ (assertArgTys . snd) components
--   unless (nrComponents == nrComponentsExpected) $ error $ "Unexpected number of components: " <> show nrComponents <> ", expected: " <> show nrComponentsExpected <> "\nGot: " <> show (fmap (Id.toText . componentName . snd) components)
