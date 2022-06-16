{-|
  Copyright   :  (C) 2015-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2021-2022, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Clash command line flag definitions.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Clash.GHC.ClashFlags
  ( parseClashFlags
  , ghcClashFlags
  , renderClashFlags
  ) where

#if MIN_VERSION_ghc(9,0,0)
import GHC.Driver.CmdLine
  (addErr, errorsToGhcException, processArgs, Err(errMsg), EwM, Flag, Warn)
import GHC.Utils.Panic (throwGhcExceptionIO)
import GHC.Types.SrcLoc (Located, unLoc)
#else
import           CmdLineParser
  (addErr, errorsToGhcException, processArgs, Err(errMsg), EwM, Flag, Warn)
import           Panic (throwGhcExceptionIO)
import           SrcLoc (Located, unLoc)
#endif

import           Control.Monad
import           Data.Char (toLower)
import           Data.IORef
import           Data.List (intercalate)
import           Text.Read (readMaybe)

import qualified Data.Set as Set
import           Data.Set (Set)

import qualified Data.Text as Text
import           Data.Text (Text)

import           Control.Lens (Lens', set, (^.), lens)
import           Control.Lens.Extra (iso')

import           Data.List.Split (splitOn)

import           Clash.Driver.Types
import           Clash.Netlist.BlackBox.Types   (HdlSyn (..))
import           Clash.Netlist.Types            (preserveCaseToBool, boolToPreserveCase)

import           Clash.GHC.ClashFlag

parseClashFlags :: IORef ClashOpts -> [Located String] -> IO ([Located String], [Warn])
parseClashFlags r = parseClashFlagsFull (ghcClashFlags r)

renderClashFlags :: ClashOpts -> [String]
renderClashFlags opts =
  renderedClashFlags ++ renderedWError ++ renderedColor
 where
  renderedClashFlags = concatMap (`cfRender` opts) clashFlags

  -- -Werror and -fdiagnostics-color are flags we inherit from GHC. Still, we
  -- should render them.
  --
  -- TODO: This makes testing rather awkward. I think we should move it to a
  --       separate data structure.
  renderedWError = if opts ^. opt_werror then ["-Werror"] else []
  renderedColor
    | show (opts ^. opt_color) == show (defClashOpts ^. opt_color) = []
    | otherwise = ["-fdiagnostics-color=" <> map toLower (show (opts ^. opt_color))]

parseClashFlagsFull :: [Flag IO] -> [Located String] -> IO ([Located String], [Warn])
parseClashFlagsFull flagsAvialable args = do
  (leftovers, errs, warns) <- processArgs flagsAvialable args

  unless (null errs) $ throwGhcExceptionIO $
    errorsToGhcException . map (("on the commandline", ) .  unLoc . errMsg) $ errs

  return (leftovers, warns)

ghcClashFlags :: IORef ClashOpts -> [Flag IO]
ghcClashFlags ref = concatMap (`cfFlags` ref) clashFlags

clashFlags :: [ClashFlag]
clashFlags =
  -- TODO: Depending on the type of flag, users need to specify arguments differently. That
  --       is:
  --
  --          * int/word:    -fclash-foo=3
  --          * str/enum/..: -fclash-foo s
  --
  --       Even worse, there's a couple of exceptions to this rule too:
  --
  --          * -fclash-debug-history=history.dat
  --          * -fclash-force-undefined 1
  --
  --       We should be liberal in what we accept and allow both types for all flags.
  --
  [ debugFlag "debug"
  , debugInfoFlag "debug-info"
  , boolFlag "debug-invariants" (opt_debug . dbg_invariants)
  , boolFlag "count-transformations" (opt_debug . dbg_countTransformations)
  , debugTransformationsFlag "debug-transformations"
  , maybeWordFlag "debug-transformations-from" (opt_debug . dbg_transformationsFrom)
  , maybeWordFlag "debug-transformations-limit" (opt_debug . dbg_transformationsLimit)
  , maybeStringWithDefaultFlag "debug-history" "history.dat" (opt_debug . dbg_historyFile)
  , maybeStringFlag "hdldir" opt_hdlDir
  , hdlSynFlag "hdlsyn"
  , boolFlag "cache" opt_cachehdl
  , boolFlag "check-inaccessible-idirs" opt_checkIDir
  , boolFlag "clear" opt_clear
  , boolFlag "prim-warn" opt_primWarn
  , intFlag  "spec-limit" opt_specLimit
  , intFlag  "inline-limit" opt_inlineLimit
  , wordFlag "inline-function-limit" opt_inlineFunctionLimit
  , wordFlag "inline-constant-limit" opt_inlineConstantLimit
  , wordFlag "evaluator-fuel-limit" opt_evaluatorFuelLimit
  , intWidthFlag "intwidth"
  , boolFlag "error-extra" opt_errorExtra
  , maybeStringFlag "component-prefix" (opt_componentPrefix . packUnpackLens)
  , boolFlag "old-inline-strategy" (opt_newInlineStrat . iso' not not)
  , boolFlag "escaped-identifiers" opt_escapedIds
  , boolFlag "lower-case-basic-identifiers" lowerCaseLens
  , boolFlag "compile-ultra" opt_ultra
  , forceUndefinedFlag "fclash-force-undefined"
  , aggressiveXOptFlag "aggressive-x-optimization"
  , boolFlag "aggressive-x-optimization-blackboxes" opt_aggressiveXOptBB
  , wordFlag "inline-workfree-limit" opt_inlineWFCacheLimit
  , boolFlag "edalize" opt_edalize
  , boolFlag "render-enums" opt_renderEnums

  -- Deprecated/removed flags:
  , deprecated
      (  "-fclash-float-support is always enabled from Clash 1.6 and onwards. Passing "
      <> "this flag will yield an error from Clash 1.10." ) $
      boolFlag "float-support" voidLens

  , deprecated
      (  "-fclash-clean has been removed. Passing this flag will yield an error from "
      <> "Clash 1.10." ) $
      boolFlag "clean" voidLens

  , deprecated
      ( "-fclash-nocache deprecated in favor of -fclash-no-cache. Passing this flag "
      <> "will yield an error from Clash 1.10." ) $
      (boolFlag "nocache" (opt_cachehdl . iso' not not)){ cfRender = const [] }
  ]
 where
  voidLens :: Lens' ClashOpts Bool
  voidLens = lens (const True) const

  lowerCaseLens :: Lens' ClashOpts Bool
  lowerCaseLens = opt_lowerCaseBasicIds . iso' preserveCaseToBool boolToPreserveCase

  packUnpackLens :: Lens' (Maybe Text) (Maybe String)
  packUnpackLens = iso' (fmap Text.unpack) (fmap Text.pack)

hdlSynFlag :: FlagName -> ClashFlag
hdlSynFlag flagName = ClashFlag
  { cfFlags = \ref -> strSuffix flagName ref parser (set opt_hdlSyn)
  , cfRender = \opts ->
      if isDefault opt_hdlSyn opts
      then []
      else renderSepSuffix flagName (show (opts ^. opt_hdlSyn))
  }
 where
  parser :: String -> EwM IO (Maybe HdlSyn)
  parser s = case readMaybe s of
    Just hdlSyn ->
      pure (Just hdlSyn)

    Nothing -> case s of
      -- TODO: Should we add a deprecation warning for these flags? We could
      --       just use 'maybeReadableFlag' and ditch this custom impl if we
      --       once the warning has been up long enough.
      "Xilinx" -> pure (Just Vivado)
      "ISE"    -> pure (Just Vivado)
      "Altera" -> pure (Just Quartus)
      "Intel"  -> pure (Just Quartus)
      _ -> do
        addErr (s ++ " is an unknown hdl synthesis tool")
        pure Nothing

debugTransformationsFlag :: FlagName -> ClashFlag
debugTransformationsFlag flagName = ClashFlag
  { cfFlags = \ref ->
      strSuffix flagName ref parser (set transformations)
  , cfRender = \opts ->
      -- XXX: Assumes default value is the empty set
      if Set.null (opts ^. transformations)
      then []
      else renderSuffix flagName (render (opts ^. transformations))
  }
 where
  transformations :: Lens' ClashOpts (Set String)
  transformations = opt_debug . dbg_transformations

  render :: Set String -> String
  render = intercalate "," . Set.elems

  parser :: String -> EwM IO (Maybe (Set String))
  parser = pure . Just . Set.fromList . splitOn ","

intWidthFlag :: FlagName -> ClashFlag
intWidthFlag flagName = ClashFlag
  { cfFlags = \ref -> intSuffix flagName ref parser (set opt_intWidth)
  , cfRender = \opts ->
    if isDefault opt_intWidth opts
    then []
    else renderSuffix flagName (show (opts ^. opt_intWidth))
  }
 where
  parser :: Int -> EwM IO (Maybe Int)
  parser 32 = pure (Just 32)
  parser 64 = pure (Just 64)
  parser n = do
    addErr ("Invalid value: " <> show n <> ". Pick one of: 32, 64.")
    pure Nothing

debugInfoFlag :: FlagName -> ClashFlag
debugInfoFlag flagName = ClashFlag
  { cfFlags = \ref ->
      strSuffix flagName ref parser setter

  , cfRender = \opts ->
      if isDefault info opts then
        []
      else
        let
          suffixFlag = renderSepSuffix flagName (show (opts ^. info))
        in
          suffixFlag ++ case (opts ^. info, opts ^. opt_cachehdl) of
            -- If user explicitly enabled caching after setting a debug flag, we
            -- need to render it too.
            (None, _) -> []
            (_, True) -> renderNoArg "cache" True
            _ -> []
  }
 where
  info :: Lens' ClashOpts TransformationInfo
  info = opt_debug . dbg_transformationInfo

  -- If a debug option is set, we disable HDL caching
  setter (False, debug) opts = set info debug opts
  setter (True, debug) opts = setter (False, debug) (set opt_cachehdl False opts)

  parser :: String -> EwM IO (Maybe (Bool, TransformationInfo))
  parser s = case readMaybe s of
    Just None -> pure (Just (False, None))
    Just t -> pure (Just (True, t))
    Nothing -> do
     addErr ("Unrecognized option: " <> s)
     pure Nothing

debugFlag :: FlagName -> ClashFlag
debugFlag flagName = ClashFlag
  { cfFlags = \ref -> strSuffix flagName ref parser setter
  , cfRender = const []  -- Alias flag, all rendering handled by other flags
  }
 where
  -- If a debug option is set, we disable HDL caching
  setter (False, debug) opts = set opt_debug debug opts
  setter (True, debug) opts = setter (False, debug) (set opt_cachehdl False opts)

  parser :: String -> EwM IO (Maybe (Bool, DebugOpts))
  parser = \case
   --                            disable cache?  debug info
   "DebugNone"    -> pure (Just (False,          debugNone))
   "DebugSilent"  -> pure (Just (True,           debugSilent))
   "DebugFinal"   -> pure (Just (True,           debugFinal))
   "DebugCount"   -> pure (Just (True,           debugCount))
   "DebugName"    -> pure (Just (True,           debugName))
   "DebugTry"     -> pure (Just (True,           debugTry))
   "DebugApplied" -> pure (Just (True,           debugApplied))
   "DebugAll"     -> pure (Just (True,           debugAll))
   s -> do
     addErr ("Unrecognized option: " <> s)
     pure Nothing

aggressiveXOptFlag :: FlagName -> ClashFlag
aggressiveXOptFlag flagName = ClashFlag
  { cfFlags = \ref -> boolArg flagName ref setBoth
  , cfRender = \opts ->
      if | xOpt opts == xOpt defClashOpts -> []
         | xOpt opts && not (xOptBB opts) ->
            -- Enabling x-optimization implies enabling blackbox x-optimization. If
            -- this is later explicitly disabled by a user, we need to make sure to
            -- replicate that.
               renderNoArg flagName (xOpt opts)
            <> renderNoArg "aggressive-x-optimization-blackboxes" (xOptBB opts)
         | otherwise -> renderNoArg flagName (xOpt opts)
  }
 where
  xOpt opts = opts ^. opt_aggressiveXOpt
  xOptBB opts = opts ^. opt_aggressiveXOptBB

  setBoth b = set opt_aggressiveXOptBB b . set opt_aggressiveXOpt b

forceUndefinedFlag :: FlagName -> ClashFlag
forceUndefinedFlag flagName = ClashFlag
  { cfFlags = \ref ->
         strSuffix flagName ref parser (set opt_forceUndefined)
      ++ unsetArg flagName ref (set opt_forceUndefined)
  , cfRender = \opts ->
      if isDefault opt_forceUndefined opts then
        []
      else
        case opts ^. opt_forceUndefined of
          Nothing -> renderNoArg flagName False
          Just Nothing -> renderSepSuffix flagName "c"
          Just (Just n) -> renderSepSuffix flagName (show n)
  }
 where
  parser :: String -> EwM IO (Maybe (Maybe (Maybe Int)))
  parser = \case
    -- TODO: Replace the maybes with a sum type
    "0" -> pure (Just (Just (Just 0)))
    "1" -> pure (Just (Just (Just 1)))
    "c" -> pure (Just (Just Nothing))
    s -> do
      addErr ("Unrecognized option: " <> show s <> ". Options: 0, 1, c.")
      pure Nothing
