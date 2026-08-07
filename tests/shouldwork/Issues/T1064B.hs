{-# LANGUAGE CPP #-}

-- | Regression test for the keep-all-casts reimplementation of #1064: an
-- OPAQUE binder whose type is a newtype of a function type (like
-- clash-protocols' @Circuit@) must remain a separate component. Call sites
-- look like @(comp |> Circuit .. ~ (.. -> ..)) arg@; such
-- newtype-unwrapping casts in function position have no HDL significance
-- and must not force inlining of the OPAQUE binder.
--
-- The newtype is deliberately eta-reducible (@Circuit a = (->) a@ in GHC),
-- which also exercises the conversion of a partially applied FUN tycon to
-- Clash's Arrow in GHC2Core.
module T1064B where

import Clash.Prelude
import qualified Prelude as P

import Control.Exception (AssertionFailed(..), throwIO)
import Control.Monad (when)
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.FilePath.Glob (compile, globDir1)

newtype Circuit a b = Circuit (a -> b)

comp :: Circuit (Signal System Bool) (Signal System Bool)
comp = Circuit (fmap not)
{-# OPAQUE comp #-}

runCircuit :: Circuit a b -> a -> b
runCircuit (Circuit f) = f

topEntity :: Signal System Bool -> Signal System Bool
topEntity i = runCircuit comp (runCircuit comp i)
{-# OPAQUE topEntity #-}

assertCompExists :: String -> IO ()
assertCompExists ext = do
  [topDir] <- getArgs
  let hdlDir = topDir </> show 'topEntity
  files <- globDir1 (compile ("*." P.<> ext)) hdlDir
  when (P.not (P.any ("comp" `isInfixOf`) files)) $
    throwIO $ AssertionFailed $ P.unlines
      [ "Expected a separate component for OPAQUE binder 'comp', but the"
      , "generated files are:"
      , P.unlines files
      ]

mainVHDL :: IO ()
mainVHDL = assertCompExists "vhdl"

mainVerilog :: IO ()
mainVerilog = assertCompExists "v"

mainSystemVerilog :: IO ()
mainSystemVerilog = assertCompExists "sv"
