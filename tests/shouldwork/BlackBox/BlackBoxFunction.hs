{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module BlackBoxFunction where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Prelude hiding (Text)
import Clash.Netlist.Types
import Clash.Netlist.BlackBox.Types
import Clash.Annotations.Primitive (Primitive(..), HDL(..))

myMultiplyTF :: BlackBoxFunction
myMultiplyTF isD primName args ty = pure $
  Right ( emptyBlackBoxMeta
        , BBTemplate [Text "123456 * ", ArgGen 0 0, Text " * ", ArgGen 0 1]
        )

{-# ANN myMultiply (InlinePrimitive [VHDL] "[ { \"BlackBoxHaskell\" : { \"name\" : \"BlackBoxFunction.myMultiply\", \"templateFunction\" : \"BlackBoxFunction.myMultiplyTF\"}} ]") #-}
myMultiply
  :: Signal System Int
  -> Signal System Int
  -> Signal System Int
myMultiply a b =
  a * b
{-# NOINLINE myMultiply #-}

topEntity
  :: SystemClockResetEnable
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
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.vhdl")
  assertIn "123456" content
