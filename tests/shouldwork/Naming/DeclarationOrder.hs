-- | Signals in the HDL are declared before the signals that use them.
--
-- This used to be partially reversed for two reasons: ANF collected binders
-- of case alternatives in reverse order, and removing unused binders ordered
-- the remaining binders by their 'Unique'.
module DeclarationOrder where

import qualified Prelude as P
import Data.List (isPrefixOf, tails)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Prelude

data ABC = A (Unsigned 8) | B (Unsigned 8) (Unsigned 8) | C (Unsigned 8)

f :: Unsigned 8 -> Unsigned 8 -> Unsigned 8
f x y = x * y + 3
{-# OPAQUE f #-}

topEntity :: ABC -> Unsigned 8
topEntity i =
  case i of
    A a -> f a a
    B b0 b1 -> f (b0 + b1) (b0 - b1)
    C c -> c

-- | Assert that the needles occur in the haystack, in the given order
assertInOrder :: [String] -> String -> IO ()
assertInOrder needles haystack = go 0 needles
 where
  go _ [] = return ()
  go n (needle:rest) =
    case [ i | (i, t) <- P.zip [0..] (tails haystack), needle `isPrefixOf` t ] of
      [] -> P.error $ P.concat [ "Expected:\n\n  ", needle
                               , "\n\nIn:\n\n", haystack ]
      (i:_)
        | i < (n :: Int) -> P.error $ P.concat
            [ "Expected:\n\n  ", needle, "\n\nAfter:\n\n  "
            , P.unlines (P.takeWhile (/= needle) needles), "\nIn:\n\n", haystack ]
        | otherwise -> go i rest

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.v")
  assertInOrder
    [ "assign a = i[15:8];"
    , "assign b0 = i[15:8];"
    , "assign b1 = i[7:0];"
    , "c$f_tupIn = {a,"
    , "assign result_fun_arg = c$f_tupIn"
    , "f f_result_1"
    , "assign c = i[15:8];"
    , "default : result = c;"
    ] content
