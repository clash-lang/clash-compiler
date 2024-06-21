{-# LANGUAGE MagicHash #-}

-- Tests that ~LIT's are rendered as bare literals in HDL

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module LITrendering where
import Clash.Prelude
import Control.Monad (when)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Clash.Annotations.Primitive (Primitive(..), HDL(..))

import Data.Int
import Data.Word
import qualified Data.List as L
import qualified Data.String.Interpolate as I

import GHC.Natural
import GHC.Prim

topEntity = foo @0 (SNat @1)
                2 3
                4 5 6 7 8
                9 10 11 12 13
                14 15 16
                17# 18##

foo :: KnownNat x0
    => SNat x1
    -> Integer -> Natural
    -> Int -> Int8 -> Int16 -> Int32 -> Int64
    -> Word -> Word8 -> Word16 -> Word32 -> Word64
    -> Signed 6 -> Unsigned 5 -> Index 20
    -> Int# -> Word#
    -> Bool
foo !x1
    !x2 !x3
    !x4 !x5 !x6 !x7 !x8
    !x9 !x10 !x11 !x12 !x13
    !x14 !x15 !x16
    !x17 !x18
    = False

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE foo #-}
{-# ANN foo (InlinePrimitive [Verilog] $ [I.i|
  [ { "BlackBox" :
      { "name"      : "LITrendering.foo"
      , "kind"      : "Declaration"
      , "template"  : "// #{L.intercalate "," ["~LIT[" <> show n <> "]" | n <- [0..18]]}"
      }
    }
  ]
  |]) #-}

mainHDL :: String -> IO ()
mainHDL topFile = do
  [topDir] <- getArgs
  content <- readFile (topDir </> "LITrendering.topEntity" </> topFile)
  let needle = L.intercalate "," [show n | n <- [0..18]]
  when (not $ needle `L.isInfixOf` content)
    (error $ unlines
      [ ""
      , "Error: couldn't find " <> show needle <> " in: "
      , content
      ])

mainVerilog       = mainHDL "topEntity.v"
