{- Utility executable to convert "old-style" JSON primitives to "new-style"
   YAML ones. See https://github.com/clash-lang/clash-compiler/pull/2009.
-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Aeson.Extra as AesonExtra
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Set as Set

import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import System.Directory (removeFile)
import System.Environment (getArgs)
import System.FilePath.Glob (glob)

help :: String
help = unlines
  [ "Convert JSON primitive files into YAML ones. YAML files will be written to "
  , "the original filename with '.yaml' appended."
  , ""
  , "Usage:"
  , "  v16-upgrade-primitives [options]... <file>..."
  , ""
  , "Options:"
  , "  --dry-run        Do not write YAML files."
  , "  --delete         Delete JSON files after writing."
  , "  --help | -h      Show this screen."
  , ""
  , "Example:"
  , "  v16-upgrade-primitives --dry-run prims/**/*.primitives"
  ]

-- | Same as 'glob', but errors on patterns matching no files.
globOrErr :: FilePath -> IO [FilePath]
globOrErr pattern = do
  files <- glob pattern
  when (null files) (error ("Pattern does not match any files: " <> pattern))
  pure files

-- | 'concatMap', but its monadic cousin
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

-- | Read file and output YAML ByteString
jsonToYaml :: FilePath -> IO ByteString
jsonToYaml path = do
  contents <- ByteStringLazy.readFile path
  let decoded = AesonExtra.decodeOrErr path contents
  pure (Yaml.encode (decoded :: Aeson.Value))

main :: IO ()
main = do
  args0 <- Set.fromList <$> getArgs

  let
    doDryRun = Set.member "--dry-run" args0
    doWrite = not doDryRun
    doDelete = Set.member "--delete" args0 && doWrite
    doHelp = Set.member "-h" args0 || Set.member "--help" args0 || Set.null args1
    args1 = foldr Set.delete args0 ["--dry-run", "--delete", "--help", "-h"]

  if doHelp then
    putStrLn help
  else do
    files <- concatMapM globOrErr (Set.toList args1)
    forM_ files $ \path -> do
      let newPath = path <> ".yaml"
      putStrLn $ "Converting " <> path <> ".."
      decoded <- jsonToYaml path
      when doWrite $ ByteString.writeFile newPath decoded
      when doDelete $ removeFile path
