{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
The SystemVerilog backend used to compute the size of an 'RTree' half as
@(d-1)^2@ instead of @2^(d-1)@ -- base and exponent flipped. An @RTree d@ has
@2^d@ elements, so for @d=3@ the right half was emitted as @[4:8]@ on an array
whose valid indices are @0..7@.

@nestM@ merges every RTree-then-RTree nesting before the backend runs, so this
path is not reachable from ordinary Haskell. It is reached here with a blackbox
template function that renders an 'Identifier' carrying a 'Nested' modifier whose
outer component is a 'Vec' projection -- a pairing @nestM@ leaves unmerged.

See https://github.com/clash-lang/clash-compiler/issues/3359
-}
module T3359 where

import qualified Prelude as P

import Control.Monad.State (State)
import Data.List (isInfixOf)
import Data.Monoid (Ap(getAp))
import Data.Text.Prettyprint.Doc.Extra (Doc)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Prelude
import Clash.Backend (Backend, expr)
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
import Clash.Annotations.Primitive (Primitive(..), HDL(..))

treeSelTF :: TemplateFunction
treeSelTF = TemplateFunction [0] (const True) treeSelTemplate

treeSelTemplate :: Backend s => BlackBoxContext -> State s Doc
treeSelTemplate _ = do
  nm <- Id.make "tree_sel"
  let rt   = RTree 3 Bool
      vec  = Vector 2 rt
      -- Outer: element 0 of the vector. Inner: right half of the RTree.
      modf = Nested (Indexed (vec, 10, 0)) (Indexed (rt, 1, 1))
  getAp (expr False (Identifier nm (Just modf)))

{-# ANN treeSel (InlinePrimitive [SystemVerilog] "[ { \"BlackBox\" : { \"name\" : \"T3359.treeSel\", \"kind\": \"Expression\", \"format\": \"Haskell\", \"templateFunction\": \"T3359.treeSelTF\"}} ]") #-}
treeSel :: Signal System Bool -> Signal System Bool
treeSel a = a
{-# OPAQUE treeSel #-}

topEntity :: Signal System Bool -> Signal System Bool
topEntity = treeSel

-- Output tests
assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

assertNotIn :: String -> String -> IO ()
assertNotIn needle haystack
  | needle `isInfixOf` haystack = P.error $ P.concat [ "Did not expect:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]
  | otherwise                   = return ()

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.sv")
  -- An RTree 3 has 8 elements, so the right half is [4:7]. Before the fix this
  -- was emitted as [4:8], which is out of bounds.
  assertIn    "tree_sel[0][4:7]" content
  assertNotIn "tree_sel[0][4:8]" content
