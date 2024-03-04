{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module T2508 where

import Clash.Explicit.Prelude
import qualified Prelude as P

import Control.Exception (AssertionFailed(..), throwIO)
import Control.Monad (when)
import Data.List (sort)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.FilePath.Glob (globDir1)

accum :: Unsigned 64 -> Unsigned 64 -> (Unsigned 64, Unsigned 64)
accum s i = (s + i, s)

opaqueAccum :: Unsigned 64 -> Unsigned 64 -> (Unsigned 64, Unsigned 64)
opaqueAccum s i = accum s i
{-# CLASH_OPAQUE opaqueAccum #-}

noAnnotationMealy clk rst en f iS i =
  let
    (s', o) = unbundle $ f <$> s <*> i
    s       = register clk rst en iS s'
  in
    o

opaqueMealy clk rst en f iS =
  noAnnotationMealy clk rst en f iS
{-# CLASH_OPAQUE opaqueMealy #-}

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Unsigned 64) ->
  Signal System (Unsigned 64)
topEntity clk rst ena i =
    noAnnotationMealy clk rst ena accum       0 i
  + noAnnotationMealy clk rst ena opaqueAccum 0 i
  + opaqueMealy       clk rst ena accum       0 i
  + opaqueMealy       clk rst ena opaqueAccum 0 i
{-# CLASH_OPAQUE topEntity #-}

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs

  let hdlDir = topDir </> show 'topEntity
  actual0 <- sort <$> globDir1 "*.vhdl" hdlDir
  let
    actual1 = P.map (P.drop (P.length hdlDir + 1)) actual0
    expected =
      [ "T2508_topEntity_types.vhdl"
      , "opaqueAccum.vhdl"
      , "opaqueMealy.vhdl"
      , "opaqueMealy_opaqueAccum.vhdl"
      , "topEntity.vhdl"
      ]
  when
    (actual1 /= expected)
    (throwIO $ AssertionFailed $ "Expected " <> show expected <> " got " <> show actual1)

  pure ()
