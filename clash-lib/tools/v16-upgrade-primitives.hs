{- Utility executable to convert "old-style" JSON primitives to "new-style"
   YAML ones. See https://github.com/clash-lang/clash-compiler/pull/2009.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as Aeson
import Data.ByteString.Lazy.Search (replace)
import Data.String (IsString)
#endif

import qualified Data.Aeson.Extra as AesonExtra
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Set as Set

import Control.Monad (forM_, when)
import Data.ByteString.Lazy (ByteString)
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
  contents <- ByteString.readFile path
  let decoded = AesonExtra.decodeOrErrJson path contents
  pure . removeTempKey . ByteString.fromStrict . Yaml.encode . customSortOutput $ decoded

{- NOTE [Sorting YAML object keys]

'Yaml.encode' encodes object with their keys in alphabetical order.
For readability we like `name` to be at the top, and `type` to be just above `template`.

We accomplice this here by renaming those keys to something there sorts where
we like them to be. And find-and-replace those temporary names back
in the resulting ByteString.
-}
#if MIN_VERSION_aeson(2,0,0)
keySortingRenames :: IsString str => [(str,str)]
keySortingRenames =
  [ ("name", "aaaa_really_should_be_name_but_renamed_to_get_the_sorting_we_like")
  , ("type", "really_should_be_type_but_renamed_to_get_the_sorting_we_like")
  ]

customSortOutput :: Aeson.Value -> Aeson.Value
customSortOutput x = case x of
  Aeson.Object o -> Aeson.Object $ fmap customSortOutput $ renameKeys $ o
  Aeson.Array xs -> Aeson.Array $ fmap customSortOutput xs
  _ -> x
 where
  renameKeys obj = foldl renameKey obj keySortingRenames
  renameKey obj (kOld,kNew) =
    case Aeson.lookup kOld obj of
      Nothing -> obj
      Just val -> Aeson.insert kNew val (Aeson.delete kOld obj)

removeTempKey :: ByteString -> ByteString
removeTempKey inp = foldl go inp keySortingRenames
 where
  go bs (orig,temp) = replace (ByteString.toStrict temp) orig bs
#else
-- < aeson-2.0 stores keys in HashMaps, whose order we can't possibly predict.
removeTempKey :: ByteString -> ByteString
removeTempKey = id

customSortOutput:: Aeson.Value -> Aeson.Value
customSortOutput = id
#endif


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
