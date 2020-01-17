{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module TemplateFunction where

import qualified Prelude as P

import Control.Monad.State (State)
import Data.List (isInfixOf)
import Data.Semigroup.Monad (getMon)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Backend (mkUniqueIdentifier, blockDecl)
import Clash.Netlist.Id (IdType(Basic))
import Clash.Netlist.Types

import Clash.Prelude
import Clash.Backend (Backend)
import Clash.Annotations.Primitive (Primitive(..), HDL(..))

myMultiplyTF :: TemplateFunction
myMultiplyTF = TemplateFunction used valid myMultiplyTemplate
 where
  used    = [0,1]
  valid _ = True

myMultiplyTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
myMultiplyTemplate bbCtx = do
  x <- mkUniqueIdentifier Basic "x123456"
  y <- mkUniqueIdentifier Basic "y123456"
  getMon $ blockDecl x [NetDecl Nothing y Bool]


{-# ANN myMultiply (InlinePrimitive [VHDL] "[ { \"BlackBox\" : { \"name\" : \"TemplateFunction.myMultiply\", \"kind\": \"Declaration\", \"format\": \"Haskell\", \"templateFunction\": \"TemplateFunction.myMultiplyTF\"}} ]") #-}
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
  [topFile] <- getArgs
  content <- readFile topFile
  assertIn "123456" content
