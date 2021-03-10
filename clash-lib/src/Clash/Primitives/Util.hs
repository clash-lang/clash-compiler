{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd
                    2018     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utility functions to generate Primitives
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Primitives.Util
  ( generatePrimMap
  , hashCompiledPrimMap
  , constantArgs
  , decodeOrErr
  , getFunctionPlurality
  ) where

import           Control.DeepSeq        (force)
import           Data.Aeson.Extra       (decodeOrErr)
import qualified Data.ByteString.Lazy   as LZ
import qualified Data.HashMap.Lazy      as HashMap
import qualified Data.HashMap.Strict    as HashMapStrict
import qualified Data.Set               as Set
import           Data.Hashable          (hash)
import           Data.List              (isSuffixOf, sort, find)
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as TS
import           Data.Text.Lazy         (Text)
import qualified Data.Text.Lazy.IO      as T
import           GHC.Stack              (HasCallStack)
import qualified System.Directory       as Directory
import qualified System.FilePath        as FilePath
import           System.IO.Error        (tryIOError)

import           Clash.Annotations.Primitive
  ( PrimitiveGuard(HasBlackBox, DontTranslate)
  , PrimitiveWarning(WarnNonSynthesizable)
  , extractPrim, extractWarnings)
import           Clash.Core.Term        (Term)
import           Clash.Core.Type        (Type)
import           Clash.Primitives.Types
  ( Primitive(BlackBox), CompiledPrimitive, ResolvedPrimitive, ResolvedPrimMap
  , includes, template, TemplateSource(TFile, TInline), Primitive(..)
  , UnresolvedPrimitive, CompiledPrimMap, GuardedResolvedPrimitive)
import           Clash.Netlist.Types    (BlackBox(..), NetlistMonad)
import           Clash.Netlist.Util     (preserveState)
import           Clash.Netlist.BlackBox.Util
  (walkElement)
import           Clash.Netlist.BlackBox.Types
  (Element(Const, Lit), BlackBoxMeta(..))

hashCompiledPrimitive :: CompiledPrimitive -> Int
hashCompiledPrimitive (Primitive {name, primSort}) = hash (name, primSort)
hashCompiledPrimitive (BlackBoxHaskell {function}) = fst function
hashCompiledPrimitive (BlackBox {name, kind, outputReg, libraries, imports, includes, template}) =
  hash (name, kind, outputReg, libraries, imports, includes', hashBlackbox template)
    where
      includes' = map (\(nms, bb) -> (nms, hashBlackbox bb)) includes
      hashBlackbox (BBTemplate bbTemplate) = hash bbTemplate
      hashBlackbox (BBFunction bbName bbHash _bbFunc) = hash (bbName, bbHash)

-- | Hash a compiled primitive map. It needs a separate function (as opposed to
-- just 'hash') as it might contain (obviously unhashable) Haskell functions. This
-- function takes the hash value stored with the function instead.
hashCompiledPrimMap :: CompiledPrimMap -> Int
hashCompiledPrimMap cpm = hash (map (fmap hashCompiledPrimitive) orderedValues)
  where
    -- TODO: switch to 'normal' map instead of hashmap?
    orderedKeys   = sort (HashMap.keys cpm)
    orderedValues = map (cpm HashMapStrict.!) orderedKeys

resolveTemplateSource
  :: HasCallStack
  => FilePath
  -> TemplateSource
  -> IO Text
resolveTemplateSource _metaPath (TInline text) =
  return text
resolveTemplateSource metaPath (TFile path) =
  let path' = FilePath.replaceFileName metaPath path in
  either (error . show) id <$> (tryIOError $ T.readFile path')

-- | Replace file pointers with file contents
resolvePrimitive'
  :: HasCallStack
  => FilePath
  -> UnresolvedPrimitive
  -> IO (TS.Text, GuardedResolvedPrimitive)
resolvePrimitive' _metaPath (Primitive name wf primType) =
  return (name, HasBlackBox [] (Primitive name wf primType))
resolvePrimitive' metaPath BlackBox{template=t, includes=i, resultNames=r, resultInits=ri, ..} = do
  let resolveSourceM = traverse (traverse (resolveTemplateSource metaPath))
  bb <- BlackBox name workInfo renderVoid multiResult kind () outputReg libraries imports functionPlurality
          <$> mapM (traverse resolveSourceM) i
          <*> traverse resolveSourceM r
          <*> traverse resolveSourceM ri
          <*> resolveSourceM t
  case warning of
    Just w  -> pure (name, HasBlackBox [WarnNonSynthesizable (TS.unpack w)] bb)
    Nothing -> pure (name, HasBlackBox [] bb)
resolvePrimitive' metaPath (BlackBoxHaskell bbName wf usedArgs multiRes funcName t) =
  (bbName,) . HasBlackBox [] . BlackBoxHaskell bbName wf usedArgs multiRes funcName <$>
    (mapM (resolveTemplateSource metaPath) t)

-- | Interprets contents of json file as list of @Primitive@s. Throws
-- exception if it fails.
resolvePrimitive
  :: HasCallStack
  => FilePath
  -> IO [(TS.Text, GuardedResolvedPrimitive)]
resolvePrimitive fileName = do
  prims <- decodeOrErr fileName <$> LZ.readFile fileName
  mapM (resolvePrimitive' fileName) prims

addGuards
  :: ResolvedPrimMap
  -> [(TS.Text, PrimitiveGuard ())]
  -> ResolvedPrimMap
addGuards = foldl go
 where
  lookupPrim
    :: TS.Text
    -> ResolvedPrimMap
    -> Maybe ([PrimitiveWarning], ResolvedPrimitive)
  lookupPrim nm primMap = do
    guardedPrim <- HashMapStrict.lookup nm primMap
    prim <- extractPrim guardedPrim
    return (extractWarnings guardedPrim, prim)

  go primMap (nm, guard) =
    HashMapStrict.insert
      nm
      (case (lookupPrim nm primMap, guard) of
        (Nothing, DontTranslate) -> DontTranslate
        (Nothing, HasBlackBox _ ()) ->
          error $ "No BlackBox definition for '" ++ TS.unpack nm ++ "' even"
               ++ " though this value was annotated with 'HasBlackBox'."
        (Just _, DontTranslate) ->
          error (TS.unpack nm ++ " was annotated with DontTranslate, but a "
                              ++ "BlackBox definition was found anyway.")
        (Just (ws1, p), HasBlackBox ws2 ()) ->
          HasBlackBox (ws1 ++ ws2) p
      )
      primMap

-- | Generate a set of primitives that are found in the primitive definition
-- files in the given directories.
generatePrimMap
  :: HasCallStack
  => [UnresolvedPrimitive]
  -- ^ unresolved primitives found in annotations (in LoadModules and
  -- LoadInterfaceFiles)
  -> [(TS.Text, PrimitiveGuard ())]
  -> [FilePath]
  -- ^ Directories to search for primitive definitions
  -> IO ResolvedPrimMap
generatePrimMap unresolvedPrims primGuards filePaths = do
  primitiveFiles <- fmap concat $ mapM
     (\filePath -> do
         fpExists <- Directory.doesDirectoryExist filePath
         if fpExists
           then
             fmap ( map (FilePath.combine filePath)
                  . filter (isSuffixOf ".primitives")
                  ) (Directory.getDirectoryContents filePath)
           else
             return []
     ) filePaths

  primitives0 <- concat <$> mapM resolvePrimitive primitiveFiles
  let metapaths = map (TS.unpack . name) unresolvedPrims
  primitives1 <- sequence $ zipWith resolvePrimitive' metapaths unresolvedPrims
  let primMap = HashMap.fromList (primitives0 ++ primitives1)
  return (force (addGuards primMap primGuards))
{-# SCC generatePrimMap #-}

-- | Determine what argument should be constant / literal
constantArgs :: TS.Text -> CompiledPrimitive -> Set.Set Int
constantArgs nm BlackBox {template = templ@(BBTemplate _), resultInits = tRIM} =
  Set.fromList (concat [ fromIntForce
                       , concatMap walkTemplate tRIM
                       , walkTemplate templ
                       ])
 where
  walkTemplate (BBTemplate t) = concatMap (walkElement getConstant) t
  walkTemplate _ = []

  getConstant (Lit i)      = Just i
  getConstant (Const i)    = Just i
  getConstant _            = Nothing

  -- Ensure that if the 'Integer' arguments are constants, that they are reduced
  -- to literals, so that the buildin rules can properly fire.
  --
  -- Only in the the case that 'Integer' arguments are truly variables should
  -- the blackbox rules fire.
  fromIntForce
    | nm == "Clash.Sized.Internal.BitVector.fromInteger#"  = [2]
    | nm == "Clash.Sized.Internal.BitVector.fromInteger##" = [0,1]
    | nm == "Clash.Sized.Internal.Index.fromInteger#"      = [1]
    | nm == "Clash.Sized.Internal.Signed.fromInteger#"     = [1]
    | nm == "Clash.Sized.Internal.Unsigned.fromInteger#"   = [1]
    -- There is a special code-path for `index_int` in the Verilog backend in
    -- case the index is a variable. But this code path only works when the
    -- vector is (a projection of) a variable. By forcing the arguments of
    -- index_int we can be sure that arguments are either:
    --
    -- Constant Variable
    -- Variable Constant
    -- Variable Variable
    --
    -- As all other cases would be reduced by the evaluator, and even expensive
    -- primitives under index_int are fully unrolled.
    | nm == "Clash.Sized.Vector.index_int"                 = [1,2]
    | nm == "Clash.Sized.Vector.replace_int"               = [1,2]
    | otherwise = []
constantArgs _ _ = Set.empty

-- | Helper function of 'getFunctionPlurality'
getFunctionPlurality' :: [(Int, Int)] -> Int -> Int
getFunctionPlurality' functionPlurality n =
  fromMaybe 1 (snd <$> (find ((== n) . fst) functionPlurality))

-- | Looks up the plurality of a function's function argument. See
-- 'functionPlurality' for more information. If not set, the returned plurality
-- will default to /1/.
getFunctionPlurality
  :: HasCallStack
  => CompiledPrimitive
  -> [Either Term Type]
  -- ^ Arguments passed to blackbox
  -> [Type]
  -- ^ Result types
  -> Int
  -- ^ Argument number holding the function of interest
  -> NetlistMonad Int
  -- ^ Plurality of function. Defaults to 1. Does not err if argument isn't
  -- a function in the first place. State of monad will not be modified.
getFunctionPlurality (Primitive {}) _args _resTys _n = pure 1
getFunctionPlurality (BlackBoxHaskell {name, function, functionName}) args resTys n = do
  errOrMeta <- preserveState ((snd function) False name args resTys)
  case errOrMeta of
    Left err ->
      error $ concat [ "Tried to determine function plurality for "
                     , TS.unpack name, " by quering ", show functionName
                     , ". Function returned an error message instead:\n\n"
                     , err ]
    Right (BlackBoxMeta {bbFunctionPlurality}, _bb) ->
      pure (getFunctionPlurality' bbFunctionPlurality n)
getFunctionPlurality (BlackBox {functionPlurality}) _args _resTy n =
  pure (getFunctionPlurality' functionPlurality n)
