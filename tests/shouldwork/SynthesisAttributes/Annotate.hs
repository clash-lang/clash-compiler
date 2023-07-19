module Annotate where

import qualified Prelude as P

import Clash.Prelude
import Clash.Annotations.SynthesisAttributes

import Data.List
import System.Environment (getArgs)
import System.FilePath ((</>))

topEntity :: Signal System Int -> Signal System Int
topEntity = setName @"my_signal" $ annotate $
     BoolAttr "bool_attr" True
  :> IntegerAttr "int_attr" 3
  :> StringAttr "str_attr" "foo"
  :> Attr "attr"
  :> Nil

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- P.readFile (topDir </> show 'topEntity </> "topEntity.vhdl")

  assertIn "attribute bool_attr : boolean;" content
  assertIn "attribute bool_attr of my_signal : signal is true;" content

  assertIn "attribute int_attr : integer;" content
  assertIn "attribute int_attr of my_signal : signal is 3;" content

  assertIn "attribute str_attr : string;" content
  assertIn "attribute str_attr of my_signal : signal is \"foo\";" content

  assertIn "attribute attr : boolean;" content
  assertIn "attribute attr of my_signal : signal is true;" content
