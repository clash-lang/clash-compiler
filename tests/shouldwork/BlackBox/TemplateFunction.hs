{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module TemplateFunction where

import qualified Prelude as P

import Control.Monad.State (State)
import Data.List (isInfixOf)
import Data.Semigroup.Monad (getMon)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Backend (blockDecl)
import qualified Clash.Netlist.Id as Id
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
  x <- Id.make "x123456"
  y <- Id.make "y123456"
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
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.vhdl")
  assertIn "123456" content
