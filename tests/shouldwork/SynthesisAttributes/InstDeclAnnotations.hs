{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module InstDeclAnnotations where

import           Clash.Annotations.Primitive     (HDL (..), Primitive (..))
import           Clash.Backend
import           Clash.Core.Var                  (Attr' (..))
import qualified Clash.Netlist.Id                as Id
import           Clash.Netlist.Types
import           Clash.Prelude
import           Control.Monad.State
import           GHC.Stack
import           Data.List                       (isInfixOf)
import           Data.Monoid                     (Ap(getAp))
import           Data.String.Interpolate         (__i)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc.Extra (Doc (..))
import qualified Prelude as P
import           System.Environment              (getArgs)
import           System.FilePath                 ((</>))


myTF :: TemplateFunction
myTF = TemplateFunction used valid myTemplate
 where
  used    = []
  valid _ = True

myTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
myTemplate bbCtx = do
  blkName  <- Id.makeBasic "blkName"
  compInst <- Id.makeBasic "test_inst"
  compName <- Id.makeBasic "TEST"
  let
    attrs =
      [ IntegerAttr' "my_int_attr"    7
      , StringAttr'  "my_string_attr" "Hello World!"
      ]
  getAp
    $ blockDecl blkName [InstDecl Comp Nothing attrs compName compInst [] (NamedPortMap []) ]


myBlackBox
  :: Signal System Int
  -> Signal System Int
myBlackBox _ = pure (errorX "not implemented")
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE myBlackBox #-}
{-# ANN myBlackBox (InlinePrimitive [VHDL,Verilog,SystemVerilog] [__i|
   [ { "BlackBox" :
        { "name" : "InstDeclAnnotations.myBlackBox",
          "kind" : "Declaration",
          "format": "Haskell",
          "templateFunction": "InstDeclAnnotations.myTF"
        }
     }
   ]
   |]) #-}


topEntity
  :: SystemClockResetEnable
  => Signal System Int
  -> Signal System Int
topEntity = myBlackBox


--------------- Actual tests for generated HDL -------------------
assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

-- VHDL test
mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- P.readFile (topDir </> show 'topEntity </> "topEntity.vhdl")

  assertIn "attribute my_int_attr : integer;" content
  assertIn "attribute my_int_attr of TEST : component is 7;" content

  assertIn "attribute my_string_attr : string;" content
  assertIn "attribute my_string_attr of TEST : component is \"Hello World!\";" content

-- Verilog test
mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- P.readFile (topDir </> show 'topEntity </> "topEntity.v")

  assertIn "(* my_int_attr = 7, my_string_attr = \"Hello World!\" *)" content

-- Verilog and SystemVerilog should share annotation syntax
mainSystemVerilog = do
  [topDir] <- getArgs
  content <- P.readFile (topDir </> show 'topEntity </> "topEntity.sv")

  assertIn "(* my_int_attr = 7, my_string_attr = \"Hello World!\" *)" content
