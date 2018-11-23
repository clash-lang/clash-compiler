{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module BlackBoxFunction where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Prelude
import Clash.Netlist.Types
import Clash.Netlist.BlackBox.Types
import Clash.Annotations.Primitive (Primitive(..), HDL(..))

myMultiplyTF :: BlackBoxFunction
myMultiplyTF isD resId primName args ty =
  Right ( emptyBlackBoxMeta
        , BBTemplate [C "123456 * ", Arg 0 0, C " * ", Arg 0 1]
        )

{-# ANN myMultiply (InlinePrimitive VHDL "[ { \"BlackBoxHaskell\" : { \"name\" : \"BlackBoxFunction.myMultiply\", \"templateFunction\" : \"BlackBoxFunction.myMultiplyTF\"}} ]") #-}
myMultiply
  :: Signal System Int
  -> Signal System Int
  -> Signal System Int
myMultiply a b =
  a * b
{-# NOINLINE myMultiply #-}

topEntity
  :: HiddenClockReset System Source Asynchronous
  => Signal System Int
  -> Signal System Int
topEntity a = myMultiply a a

-- Output tests
assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]
mainVHDL :: IO ()
mainVHDL = do
  [modDir, topFile] <- getArgs
  content <- readFile (modDir </> topFile)

  assertIn "123456" content

mainVerilog = mainVHDL
mainSystemVerilog = mainVHDL
