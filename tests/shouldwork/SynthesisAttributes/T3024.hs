{-# LANGUAGE  CPP #-}
module T3024 where

import qualified Prelude as P

import qualified Data.List as List
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

import Clash.Prelude
import Clash.Annotations.TopEntity

f :: Unsigned 8 -> Unsigned 8
f x = x + 1
{-# ANN f (Synthesize {t_name = "f", t_inputs = [PortName "x"], t_output = PortName "y"}) #-}
{-# CLASH_OPAQUE f #-}

g :: (Unsigned 8 -> Unsigned 8) -> Unsigned 8 -> Unsigned 8
g h x = (h x + h (x * 8))
{-# CLASH_OPAQUE g #-}

q :: Unsigned 8 -> Unsigned 8
q = g f
{-# ANN q (Synthesize {t_name = "q", t_inputs = [PortName "a"], t_output = PortName "b"}) #-}
{-# CLASH_OPAQUE q #-}

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | List.isInfixOf needle haystack = pure ()
  | otherwise = P.error $ mconcat
      [ "Expected:\n\n  ", needle, "\n\nIn:\n\n", haystack]

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'q </> "T3024_q_g_f.vhdl")

  assertIn "entity f.f" content
