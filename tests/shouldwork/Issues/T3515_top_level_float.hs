{-# LANGUAGE TemplateHaskellQuotes #-}

module T3515_top_level_float where

import qualified Prelude as P

import Clash.Prelude
import Clash.Annotations.Primitive (Primitive (InlineYamlPrimitive), HDL (..))

import System.Environment (getArgs)
import System.FilePath ((</>))

-- | A black box that instantiates a component, so every use of a shared
-- result of it should lead to a single instance.
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

topEntity :: (Signal System (BitVector 8), Signal System (BitVector 8))
topEntity = (s, s)
 where
  s = foo 3

countOccurrences :: String -> String -> Int
countOccurrences needle = go
 where
  n = P.length needle
  go [] = 0
  go s@(_:rest)
    | needle == P.take n s = 1 + go rest
    | otherwise = go rest

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- P.readFile (topDir </> show 'topEntity </> "topEntity.v")

  let n = countOccurrences "foo-instantiation" content
  if n == 1
    then return ()
    else P.error (P.concat ["Expected exactly 1 instance of foo, found ", P.show n, ":\n\n", content])
