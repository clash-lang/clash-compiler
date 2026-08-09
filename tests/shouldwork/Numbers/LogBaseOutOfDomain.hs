-- Regression test: 'GHC.Num.Integer.integerLogBase#' and
-- 'GHC.Num.Natural.naturalLogBase#' must be constant folded to the same value
-- GHC computes, including outside of the domain of 'logBase', and to
-- 'undefined' where GHC throws an arithmetic exception.
{-# LANGUAGE MagicHash #-}

module LogBaseOutOfDomain where

import qualified Prelude as P

import Clash.Prelude
import Data.Char (isDigit)
import Data.List (stripPrefix)
import GHC.Word (Word(W#))
import System.Environment (getArgs)
import System.FilePath ((</>))
import qualified GHC.Num.Integer as Integer
import qualified GHC.Num.Natural as Natural

-- | Kept out of 'topEntity' so GHC does not rewrite these itself. In
-- particular, it would turn 'Integer.integerLogBase#' applied to literals into
-- 'Natural.naturalLogBase#'.
integerLogBase :: Integer -> Integer -> Word
integerLogBase b i = W# (Integer.integerLogBase# b i)
{-# NOINLINE integerLogBase #-}

naturalLogBase :: Natural -> Natural -> Word
naturalLogBase b i = W# (Natural.naturalLogBase# b i)
{-# NOINLINE naturalLogBase #-}

topEntity :: Vec 16 Word
topEntity =
     integerLogBase 2 1
  :> integerLogBase 2 1023
  :> integerLogBase 2 1024
  :> integerLogBase 10 100000000000000000000 -- 10^20 > maxBound :: Word
  :> integerLogBase 10 99999999999999999999
  :> integerLogBase 3 1267650600228229401496703205376 -- 2^100
  :> integerLogBase (-3) 7
  :> integerLogBase 2 0 -- outside of the domain of 'logBase'
  :> integerLogBase 3 (-7) -- outside of the domain of 'logBase'
  :> integerLogBase 1 7 -- arithmetic overflow
  :> naturalLogBase 2 18446744073709551616 -- 2^64
  :> naturalLogBase 2 18446744073709551615
  :> naturalLogBase 7 48
  :> naturalLogBase 2 0 -- outside of the domain of 'logBase'
  :> naturalLogBase 3 0 -- outside of the domain of 'logBase'
  :> naturalLogBase 0 7 -- arithmetic overflow
  :> Nil
{-# OPAQUE topEntity #-}

-- | What GHC computes for 'topEntity', 'Nothing' where it throws
expected :: [Maybe Integer]
expected =
  [ Just 0, Just 9, Just 10, Just 20, Just 19, Just 63, Just 1, maxWord, Just 0
  , Nothing
  , Just 64, Just 63, Just 1, maxWord, Just 0
  , Nothing ]
 where
  maxWord = Just (P.toInteger (maxBound :: Word))

-- | Collects literals like @64'd9@, and @{64 {1'bx}}@ as 'Nothing'
mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- P.readFile (topDir </> P.show 'topEntity </> "topEntity.v")
  let actual = literals content
  if actual == expected
    then pure ()
    else P.error $ "Expected literals " <> P.show expected <> ", got "
                <> P.show actual <> " in:\n\n" <> content
 where
  literals s@(_:rest)
    | Just r <- stripPrefix "64'd" s
    , (ds@(_:_), r') <- P.span isDigit r
    = Just (P.read ds) : literals r'
    | Just r <- stripPrefix "{64 {1'bx}}" s
    = Nothing : literals r
    | otherwise = literals rest
  literals [] = []
