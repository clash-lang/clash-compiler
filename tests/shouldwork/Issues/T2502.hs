{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module T2502 where

import qualified Prelude as P

import Data.String.Interpolate (__i)
import Clash.Explicit.Prelude
import Clash.Annotations.Primitive

import Control.Exception (AssertionFailed(..), throwIO)
import Control.Monad (when)
import Data.List (sort)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.FilePath.Glob (globDir1)

class X a where
  x :: a

instance X (Signal System Int) where
  x = pure 3

instance X a => X (Signal System Int -> a) where
  x !_i = x

bb :: X a => Int -> a
bb !_ = x
{-# CLASH_OPAQUE bb #-}
{-# ANN bb hasBlackBox #-}
{-# ANN bb (
  let bbName = show 'bb
  in InlineYamlPrimitive [minBound..] [__i|
    BlackBox:
      name: "#{bbName}"
      kind: Expression
      template: "~ARG[2]"
|]) #-}

bbWrapper :: X a => Int -> a
bbWrapper i = bb i
{-# CLASH_OPAQUE bbWrapper #-}

topEntity :: Int -> Signal System Int -> Signal System Int
topEntity i0 i1 = bbWrapper i0 i1
{-# CLASH_OPAQUE topEntity #-}

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs

  -- 'bbWrapper' should get its own file, so we expect two: one for 'topEntity',
  -- one for 'bbWrapper'.
  let hdlDir = topDir </> show 'topEntity
  actual0 <- sort <$> globDir1 "*.vhdl" hdlDir
  let
    actual1 = P.map (P.drop (P.length hdlDir + 1)) actual0
    expected =
      [ "T2502_topEntity_types.vhdl"
      , "bbWrapper.vhdl"
      , "topEntity.vhdl"
      ]
  when
    (actual1 /= expected)
    (throwIO $ AssertionFailed $ "Expected " <> show expected <> " got " <> show actual1)

  pure ()
