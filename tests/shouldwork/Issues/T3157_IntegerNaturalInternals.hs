module T3157_IntegerNaturalInternals where

import Clash.Prelude

import Clash.Backend
import Clash.Netlist.Types (Declaration(..), declarations, Seq (..))
import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

import Control.Monad
import Data.Monoid

topEntity :: Integer -> Natural -> (Bool,Bool, Integer)
topEntity int nat = (is4 int, is4 nat, foldableSNat (SNat @0x1_0000_0000_0000_0000))
 where
  is4 :: (Num a, Eq a) => a -> Bool
  is4 x = case x of
    4 -> True
    _ -> False

-- If elimCaseBigNumInternals runs too early in the pipeline this will fail
-- when applied to an SNat that doesn't fit in 64bits
foldableSNat :: SNat n -> Integer
foldableSNat n = snatToInteger (minSNat d123 n)
-- it needs NOINLINE so GHC doesn't evaluate the minSNat for us already
{-# NOINLINE foldableSNat #-}

expectedCases = 2  -- each call to is4 should create one case

testPath :: FilePath
testPath = "tests/shouldwork/Issues/T3157_IntegerNaturalInternals.hs"

mainCommon
  :: (Backend (TargetToState target))
  => SBuildTarget target
  -> IO ()
mainCommon hdl = do
  netlist <- runToNetlistStage hdl id testPath
  let [(_,comp)] = netlist
      decls = declarations comp
      totalChoices = getSum (mconcat (fmap countChoices decls))
  when (totalChoices /= expectedCases) $
    error ("Expected " <> show expectedCases <> " cases, but got " <> show totalChoices)

countChoices :: Declaration -> Sum Int
countChoices d = case d of
  CondAssignment {} -> Sum 1
  Seq xs -> mconcat (fmap countChoicesSeq xs)
  ConditionalDecl _ xs -> mconcat (fmap countChoices xs)
  _ -> mempty

countChoicesSeq :: Seq -> Sum Int
countChoicesSeq s = case s of
  AlwaysClocked _ _ xs -> mconcat (fmap countChoicesSeq xs)
  Initial xs -> mconcat (fmap countChoicesSeq xs)
  AlwaysComb xs -> mconcat (fmap countChoicesSeq xs)
  SeqDecl d -> countChoices d
  Branch _ _ xss -> Sum 1 <> mconcat (fmap (mconcat . fmap countChoicesSeq . snd) xss)

mainVHDL :: IO ()
mainVHDL = mainCommon SVHDL

mainVerilog :: IO ()
mainVerilog = mainCommon SVerilog

mainSystemVerilog :: IO ()
mainSystemVerilog = mainCommon SSystemVerilog
