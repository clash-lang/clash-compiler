{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module T3008 where

import Prelude

import Clash.Annotations.Primitive
import Data.List
import Data.String.Interpolate (__i)
import Language.Haskell.TH.Syntax
import System.Directory
import System.Environment (getArgs)
import System.FilePath

-- | Primitive that should result in a blackbox instantiation that
-- copies a file to the output directory.
prim !_srcPath = True
{-# CLASH_OPAQUE prim #-}
{-# ANN
  prim
  (
    InlineYamlPrimitive
        [Verilog, VHDL, SystemVerilog]
        [__i|
  BlackBox:
    name: T3008.prim
    kind: Declaration
    template: |-

      ~DEVNULL[~FILE[~LIT[0]]]
    |]
  )
  #-}

-- | Top entity that references the same primitive twice, but with different paths.
-- We expect that since they point to the same file, only one copy of the file
-- ends up in the output directory.
topEntity = (prim "T3008.hs", prim "../Issues/T3008.hs")
{-# CLASH_OPAQUE topEntity #-}

verify :: IO ()
verify = do
  [topDir] <- getArgs
  let outDir = topDir </> show 'topEntity
  files <- getDirectoryContents outDir

  let hsFiles = filter (".hs" `isSuffixOf`) files
  case hsFiles of
    [] -> error $ "No *.hs files found in output directory: " <> outDir <> "\nFiles found: " <> show files
    [fn] -> if fn == "T3008.hs"
              then return ()
              else error $ "Expected T3008.hs, but found: " <> fn
    files -> error $ "Expected only T3008.hs, but found: " <> show files

mainVHDL = verify
mainSystemVerilog = verify
mainVerilog = verify
