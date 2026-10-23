-- | Alternatives of case statements in the HDL mention the constructors they
-- match in a comment.
module ConstructorComments where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Prelude

data Op = Inc | Dec | Hold
  deriving (Generic, NFDataX)

topEntity :: Op -> Maybe (Unsigned 8) -> Unsigned 8 -> Unsigned 8
topEntity op m x = case op of
  Inc -> x + 1
  Dec -> x - 1
  Hold -> case m of
    Just y -> y
    Nothing -> x
{-# OPAQUE topEntity #-}

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise = P.error $ P.concat [ "Expected:\n\n  ", needle
                                   , "\n\nIn:\n\n", haystack ]

readHdl :: FilePath -> IO String
readHdl file = do
  [topDir] <- getArgs
  readFile (topDir </> show 'topEntity </> file)

mainVerilog :: IO ()
mainVerilog = do
  content <- readHdl "topEntity.v"
  assertIn "2'b00 : result = x + 8'd1; // Inc" content
  assertIn "2'b01 : result = x - 8'd1; // Dec" content
  -- The last alternative is rendered as the default
  assertIn "default : result = c$case_Hold; // Hold" content

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  content <- readHdl "topEntity.sv"
  assertIn "2'b00 : result = x + 8'd1; // Inc" content
  assertIn "default : result = c$case_Hold; // Hold" content

mainVHDL :: IO ()
mainVHDL = do
  content <- readHdl "topEntity.vhdl"
  assertIn "x when \"0\", -- Nothing" content
  assertIn "y when others; -- Just" content
  -- Patterns of enumerations already mention the constructor
  assertIn "x + to_unsigned(1,8) when Op_1'(Inc),\n" content
  assertIn "\\c$case_Hold\\ when others; -- Hold" content
