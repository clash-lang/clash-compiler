{-# LANGUAGE BangPatterns #-}

module ZeroWidth where

import qualified Prelude                      as P
import           Control.Monad                (forM_)
import           Clash.Annotations.Primitive
import           Clash.Prelude
import           GHC.Exts
import           Data.String.Interpolate      (i)
import           Data.String.Interpolate.Util (unindent)
import           Data.List                    (isInfixOf)
import           System.Environment           (getArgs)
import           System.FilePath              ((</>), takeDirectory)


luckyNumber, question, answer :: String
luckyNumber = "Your lucky number is 3552664958674928."
question = "What lies on the bottom of the ocean and twitches?"
answer = "A nervous wreck."


-- | Inserts given comment in HDL. Returns "nothing".
comment :: String -> ()
comment !_s = ()
{-# NOINLINE comment #-}
{-# ANN comment (InlinePrimitive [VHDL] $ unindent [i|
  [ { "BlackBox" :
      { "name"      : "ZeroWidth.comment"
      , "kind"      : "Declaration"
      , "renderVoid": "RenderVoid"
      , "template"  : "~NAME[0]"
      }
    }
  ] |]
  ) #-}

implicitComment :: Int -> ()
implicitComment n =
  case n of
    5 -> ()
    _ ->
      comment question
{-# NOINLINE implicitComment #-}

topEntity :: Int -> (Int, (), ())
topEntity a = (succ a, comment luckyNumber, implicitComment a)

mainHDL :: String -> String -> IO ()
mainHDL topFile implFile = do
  [topDir] <- getArgs
  contentTopEntity <- readFile (topDir </> show 'topEntity </> topFile)
  contentImplicitComment <- readFile (topDir </> show 'topEntity </> implFile)

  if luckyNumber `isInfixOf` contentTopEntity then
    pure ()
  else
    error $ "Expected:\n\n" P.++ luckyNumber
       P.++ "\n\nBut did not find it in:\n\n" P.++ contentTopEntity

  if question `isInfixOf` contentImplicitComment then
    pure ()
  else
    error $ "Expected:\n\n" P.++ question
       P.++ "\n\nBut did not find it in:\n\n" P.++ contentImplicitComment

mainSystemVerilog, mainVerilog, mainVHDL :: IO ()
mainSystemVerilog = error "NYI"
mainVerilog       = error "NYI"
mainVHDL          = mainHDL "topEntity.vhdl" "implicitComment.vhdl"
