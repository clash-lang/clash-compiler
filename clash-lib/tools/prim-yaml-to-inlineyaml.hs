{-|
  Utility that can convert blackboxes from clash-lib .primitives.yaml files
  to InlineYamlPrimitive ANNotations.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main(main) where

import Control.Lens
  (Getter, Traversal', (^.), foldMapOf, over, preview, set, to)
import Control.Monad (forM, when)

import qualified Data.Aeson              as Aeson
import           Data.Aeson              (Value(..))
import           Data.Aeson.Extra        (decodeOrErrYaml)
import qualified Data.Aeson.Key          as AesonKey
import           Data.Aeson.Lens
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import           Data.Either             (partitionEithers)
import           Data.List               (sortOn)
import           Data.List.Extra         (split)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Monoid             (Ap(getAp))
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import qualified Data.Text.Encoding      as Text
import           Data.Text.Lazy          (fromStrict,toStrict)
import qualified Data.Vector             as Vector (fromList)
import qualified Data.Yaml as Yaml

#if MIN_VERSION_prettyprinter(1,7,0 )
import Prettyprinter
import Prettyprinter.Render.Text
#else
import Data.Text.Prettyprint.Doc
#endif

import System.Directory (doesFileExist, removeFile)
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>), splitPath, takeDirectory, takeFileName)
import System.FilePath.Glob (glob)

import Clash.Annotations.Primitive  (HDL(..))
import Clash.Netlist.BlackBox.Util  (parseFail, prettyBlackBox)
import Clash.Netlist.BlackBox.Types hiding (bbImports)

#include "BlackBoxSorting.hs"

help :: String
help = unlines
  [ "Convert blackboxes from YAML files into InlineYamlPrimitive"
  , ""
  , "Usage:"
  , "  prim-yaml-to-inlineyaml [options]... <primitive>..."
  , ""
  , "Options:"
  , "  --dry-run | -n   Do not write files."
  , "  --help | -h      Show this screen."
  , ""
  , "Example:"
  , "  prim-yaml-to-inlineyaml --dry-run 'Clash.Signal.Internal.register#'"
  ]

type PrimName = Text
type PrimSet = Set PrimName
type Module = [String]

main :: IO ()
main = do
  args0 <- Set.fromList . map Text.pack <$> getArgs

  let
    doDryRun = Set.member "-n" args0 || Set.member "--dry-run" args0
    doWrite = not doDryRun
    doHelp = Set.member "-h" args0 || Set.member "--help" args0 || Set.null args1
    args1 = foldr Set.delete args0 ["--dry-run", "-n", "--help", "-h"]

  if doHelp then
    putStrLn help
  else do
    files <- glob "./clash-lib/prims/*/*.primitives.yaml"

    bbs <- foldr (Map.unionWith mappend) mempty <$> forM files (goFile doWrite args1)
    let bbs' = fmap (map snd . sortOn fst) bbs
    let go = if doWrite then writeResult else printResult
    go bbs'

writeResult :: Map Module [Text] -> IO ()
writeResult = mapM_ go . Map.toList
 where
  go :: (Module,[Text]) -> IO ()
  go (modNm,xs) = do
    let path = modPath modNm
    exists <- doesFileExist path
    if not exists then do
      error $ path <> " doesn't exist"
    else do
      Text.appendFile path (Text.unlines (header : xs))

printResult :: Map Module [Text] -> IO ()
printResult = mapM_ go . Map.toList
 where
  go :: (Module,[Text]) -> IO ()
  go (modNm,xs) = do
    let path = modPath modNm
    exists <- doesFileExist path
    when (not exists) $ putStrLn $ "error: " <> path <> " doesn't exist"
    putStrLn $ "Would append to " <> path <> ":"
    Text.putStrLn $ Text.unlines (header : xs)
    putStrLn "-------------"

header :: Text
header = Text.unlines $ "" : map ("-- " <>)
 [ "These generated InlineYamlPrimitive annotations need the following pragma and imports:"
 , ""
 , "{-# LANGUAGE QuasiQuotes #-}"
 , "{-# LANGUAGE TemplateHaskellQuotes #-}"
 , ""
 , "import Clash.Annotations.Primitive(Primitive (InlineYamlPrimitive), HDL(..))"
 , "import Data.List.Infinite (Infinite((:<)), (...))"
 , "import Data.String.Interpolate (__i)"
 ]

modPath :: Module -> FilePath
modPath xs = "clash-prelude/src/" </> foldr1 (</>) xs <.> "hs"

escapeValueWith :: (Text -> Text) -> Aeson.Value -> Aeson.Value
escapeValueWith f v = case v of
  Object o -> goObject o
  Array xs -> goArray xs
  String str -> goString str
  Number {} -> v
  Bool {} -> v
  Null -> v
 where
   goObject = Object . Aeson.mapKeyVal goKey (escapeValueWith f)
   goArray = Array . fmap (escapeValueWith f)
   goString = String . f
   goKey = AesonKey.fromText . f . AesonKey.toText

escapeHash :: Aeson.Value -> Aeson.Value
escapeHash = escapeValueWith (Text.replace "#" "\\#")

escapeBracketEscapes :: Aeson.Value -> Aeson.Value
escapeBracketEscapes = escapeValueWith (Text.replace "[\\" "[\\\\" . Text.replace "\\]" "\\\\]")


goFile :: Bool -> PrimSet -> FilePath -> IO (Map Module [(PrimName,Text)])
goFile doRemove wantedPrims path = do
  -- putStrLn $ "Processing " <> path
  contents <- BL.readFile path
  let prims = decodeOrErrYaml @[Value] path contents
  let (rest,found) = partitionEithers $ map lookingFor prims

  if (null found)
  then return mempty
  else do
    putStrLn $ "Found in " <> path
    when doRemove $ do
      if rest == []
        then
          removeFile path
        else
          BS.writeFile path $ customYamlEncode $ mkArray rest
    let convertedTemplates = map (\p -> let nm = getName p in (nm, genInlineYamlAnn nm (getHdls path) p)) found
    return $ Map.singleton (getModule path) convertedTemplates
 where
  mkArray = Aeson.Array . Vector.fromList
  lookingFor :: Value -> Either Value Value
  lookingFor prim = case preview bbName prim  of
    Just nm | Set.member nm wantedPrims -> Right prim
    _ -> Left prim

  getName :: Value -> PrimName
  getName v = v ^. bbName

getHdls :: FilePath -> [HDL]
getHdls path = case lastPart path of
  "common" -> [SystemVerilog,Verilog,VHDL]
  "commonverilog" -> [SystemVerilog,Verilog]
  "systemverilog" -> [SystemVerilog]
  "verilog" -> [Verilog]
  "vhdl" -> [VHDL]
  _ -> [SystemVerilog,Verilog,VHDL]
 where
  lastPart p = last $ splitPath $ takeDirectory p

getModule :: FilePath -> Module
getModule p = case split (== '.') $ takeFileName p of
  [modNm,"primitives","yaml"] -> split (== '_') modNm
  _ -> error $ p <> "  doesn't end in \".primitives.yaml\""

-- optics to fields in the yaml value that contain templates
bbTemplateFields :: [Traversal' Value Text]
bbTemplateFields = [bbTemplate,bbResultInit,bbResultName,bbImports,bbLibraries,bbIncludesTemplate]
 where
  bbTemplate,bbResultInit,bbResultName,bbImports,bbLibraries,bbIncludesTemplate
    :: Traversal' Value Text
  bbTemplate   = bbObj . key "template" . _String
  bbResultInit = bbObj . key "resultInit" . key "template" . _String
  bbResultName = bbObj . key "resultName" . key "template" . _String
  bbLibraries  = bbObj . key "libraries" . values . _String
  bbImports    = bbObj . key "imports" . values . _String
  bbIncludesTemplate = bbObj . key "includes" . values . key "template" . _String

bbObj :: Traversal' Value Value
bbObj = key "BlackBox"

bbName :: Traversal' Value Text
bbName = bbObj . key "name" . _String



getUsedArgs :: Value -> Set Int
getUsedArgs val = Set.unions $ map go bbTemplateFields
 where
  go :: Traversal' Value Text -> Set Int
  go l = foldMapOf (l . getParsedTemplate . traverse @[] . traverse @Element) Set.singleton val


updateArgs :: Value -> Value
updateArgs val0 = set bbName "#{bbName}" $ escapeBracketEscapes $ foldl go (escapeHash val0) bbTemplateFields
 where
  go :: Value -> Traversal' Value Text -> Value
  go val t = over t convertToSplicedArgs val
  convertToSplicedArgs :: Text -> Text
  convertToSplicedArgs = unparse . map (fmap SplicedArgN) . parse

getParsedTemplate :: Getter Text [Element Int]
getParsedTemplate = to parse

newtype SplicedArgN a = SplicedArgN a deriving Show

instance Pretty (SplicedArgN Int) where
  pretty (SplicedArgN n) = string "#{arg" <> pretty n <> string "}"

instance Pretty (SplicedArgN String) where
  pretty (SplicedArgN str) = string "#{" <> string str <> string "}"


parse
 :: Text
 -> [Element Int] -- BlackBoxTemplate
parse = parseFail . fromStrict

unparse
 :: (Pretty arg, Show arg)
 => [Element arg]
 -> Text
unparse elems = case toStrict <$> getAp (prettyBlackBox elems) of
  Just t -> t
  Nothing -> error "unparse failed"

genInlineYamlAnn :: PrimName -> [HDL] -> Value -> Text
genInlineYamlAnn fqNm hdls val = renderStrict $ layoutPretty (LayoutOptions Unbounded) go
 where
  lNm = last $ Text.splitOn "." fqNm
  go = string "{-# ANN" <+> pretty lNm <+> parens (hardline <> indent 2 annValue) <+> string "#-}"
  annValue = vsep [ string "let"
                  , indent 2 (vsep $ map pprBinder binders)
                  , string "in"
                  , indent 2 body]
  body = vsep
          [ string "InlineYamlPrimitive" <+> list (map viaShow hdls) <+> string "[__i|"
          , indent 2 (vsep (map pretty (Text.lines blackbox)))
          , string "|]"
          ]

  val' = updateArgs val
  blackbox = Text.decodeUtf8 $ customYamlEncode val'
  usedArgs = getUsedArgs val
  binders =
    [ (string "bbName", string "show" <+> squote <> pretty lNm)
    , (hcat $ punctuate (string " :< ") ((pprUsedArgs usedArgs ++ [string "_"])), parens (string "(0 :: Int)" <> string "..."))
    ]

pprBinder :: (Doc ann, Doc ann) -> Doc ann
pprBinder (pat,val) = pat <+> equals <+> val

-- | Turns
-- >  Set.fromList [1,3])
-- into
-- >  ["_arg0","arg1","_arg2","arg3"]
pprUsedArgs :: Set Int -> [Doc ann]
pprUsedArgs = snd . Set.foldl (flip go) (-1,[])
 where
  go arg (lastUsed,out)
    | arg > lastUsed = (arg, out ++ [pprUnused n | n <- [lastUsed+1..arg-1]] ++ [pprUsed arg])
    | otherwise = (lastUsed,out)
  pprUsed n = string "arg" <> pretty n
  pprUnused n = pretty '_' <>  pprUsed n


-- | `pretty` specialised to 'String'
string :: String -> Doc ann
string = pretty
