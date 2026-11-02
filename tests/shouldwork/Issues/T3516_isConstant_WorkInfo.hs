{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -fno-local-float-out #-}

module T3516_isConstant_WorkInfo where

import Clash.Prelude
import Clash.Annotations.Primitive (Primitive (InlineYamlPrimitive), HDL (..))

import Data.List (isPrefixOf, tails)
import System.Environment (getArgs)
import System.FilePath ((</>))
import qualified Prelude as P

-- | A black box that always does work, even when its arguments are constant.
foo :: BitVector 8 -> Signal System (BitVector 8)
foo !x = pure x
{-# OPAQUE foo #-}
{-# ANN foo (InlineYamlPrimitive [Verilog] $ unlines
  [ "BlackBox:"
  , "  name: " <> show 'foo
  , "  kind: Declaration"
  , "  workInfo: Always"
  , "  template: foo-instantiation ~ARG[0];"
  ]) #-}

-- | A black box that ignores its arguments and outputs a constant, even when
-- its arguments are not constant.
baz :: Signal System (BitVector 8) -> Signal System (BitVector 8)
baz !_ = pure 3
{-# OPAQUE baz #-}
{-# ANN baz (InlineYamlPrimitive [Verilog] $ unlines
  [ "BlackBox:"
  , "  name: " <> show 'baz
  , "  kind: Declaration"
  , "  workInfo: Constant"
  , "  template: baz-instantiation ~ARG[0];"
  ]) #-}

-- | A black box with the default work info (Variable): it does work unless
-- its arguments are constant.
bar :: Signal System (BitVector 8) -> Signal System (BitVector 8)
bar !x = x
{-# OPAQUE bar #-}
{-# ANN bar (InlineYamlPrimitive [Verilog] $ unlines
  [ "BlackBox:"
  , "  name: " <> show 'bar
  , "  kind: Declaration"
  , "  template: bar-instantiation ~ARG[0];"
  ]) #-}

-- | Because 'foo' always does work, @s@ is not a constant and must not be
-- duplicated into both tuple fields. Because 'baz' outputs a constant, @t@ is
-- a constant and is duplicated into both tuple fields.
topEntity ::
  Signal System (BitVector 8) ->
  ( Signal System (BitVector 8), Signal System (BitVector 8)
  , Signal System (BitVector 8), Signal System (BitVector 8) )
topEntity x = (s, s, t, t)
 where
  s = bar (foo 3)
  t = bar (baz x)

assertOccurrences :: Int -> String -> String -> IO ()
assertOccurrences expected needle haystack
  | actual == expected = pure ()
  | otherwise = P.error $ P.concat
      [ "Expected ", P.show expected, " occurrence(s) of ", P.show needle
      , ", but found ", P.show actual, " in:\n\n", haystack ]
 where
  actual = P.length (P.filter (needle `isPrefixOf`) (tails haystack))

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- P.readFile (topDir </> P.show 'topEntity </> "topEntity.v")
  assertOccurrences 1 "foo-instantiation" content
  assertOccurrences 2 "baz-instantiation" content
  assertOccurrences 3 "bar-instantiation" content
