{-# LANGUAGE TemplateHaskellQuotes #-}

{-# OPTIONS_GHC -fno-local-float-out #-}

module T3517_inlineCleanup_oneAlt where

import Clash.Prelude
import Clash.Annotations.Primitive (Primitive (InlineYamlPrimitive), HDL (..))

import qualified Prelude as P

import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

data Wrap = Wrap (Signal System (BitVector 8))

unWrap :: Wrap -> Signal System (BitVector 8)
unWrap (Wrap s) = s

-- | A black box with the default work info (Variable). As its argument is
-- constant, ANF does not let-bind @bar 3@, leaving a case expression with a
-- non-variable subject.
bar :: BitVector 8 -> Wrap
bar !x = Wrap (pure x)
{-# OPAQUE bar #-}
{-# ANN bar (InlineYamlPrimitive [Verilog] $ unlines
  [ "BlackBox:"
  , "  name: " <> show 'bar
  , "  kind: Declaration"
  , "  template: \"// bar-instantiation ~ARG[0]\""
  ]) #-}

dup ::
  Bool ->
  Signal System (BitVector 8) ->
  (Signal System (BitVector 8), Signal System (BitVector 8))
dup False _ = (0, 0)
dup True x = (x, x)

-- | The projection out of @bar 3@ is shared, so 'bar' should be instantiated
-- only once.
topEntity :: (Signal System (BitVector 8), Signal System (BitVector 8))
topEntity = dup True (unWrap (bar 3))

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.v")
  let n = P.length (P.filter ("bar-instantiation" `isInfixOf`) (lines content))
  if n == 1
    then pure ()
    else P.error ("Expected 1 instantiation of bar, got " <> show n <> " in:\n\n" <> content)
