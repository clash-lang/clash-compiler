{-|
  Copyright   :  (C) 2015-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2021,      QBayLogic B.V.,
                     2022,      Google Inc.,
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}

module Clash.GHC.ClashFlags
  ( parseClashFlags
  , flagsClash
  )
where

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Driver.CmdLine
import           GHC.Utils.Panic
import           GHC.Types.SrcLoc
#else
import           CmdLineParser
import           Panic
import           SrcLoc
#endif

import           Control.Monad
import           Data.Char                      (isSpace)
import           Data.IORef
import           Data.List                      (dropWhileEnd)
import           Data.List.Split                (splitOn)
import qualified Data.Set                       as Set
import qualified Data.Text                      as Text
import           Text.Read                      (readMaybe)

import           Clash.Backend.Verilog.Time     (parsePeriod)
import           Clash.Driver.Types
import           Clash.Netlist.BlackBox.Types   (HdlSyn (..))
import           Clash.Netlist.Types            (PreserveCase (ToLower))

parseClashFlags :: IORef ClashOpts -> [Located String]
                -> IO ([Located String],[Warn])
parseClashFlags r = parseClashFlagsFull (flagsClash r)

parseClashFlagsFull :: [Flag IO] -> [Located String]
                    -> IO ([Located String],[Warn])
parseClashFlagsFull flagsAvialable args = do
  (leftovers,errs,warns) <- processArgs flagsAvialable args
#if MIN_VERSION_ghc(9,4,0)
                              parseResponseFile
#endif

  unless (null errs) $ throwGhcExceptionIO $
    errorsToGhcException . map (("on the commandline", ) .  unLoc . errMsg)
                         $ errs

  return (leftovers, warns)

flagsClash :: IORef ClashOpts -> [Flag IO]
flagsClash r = [
    defFlag "fclash-debug"                       $ SepArg (setDebugLevel r)
  , defFlag "fclash-debug-info"                  $ SepArg (setDebugInfo r)
  , defFlag "fclash-debug-invariants"            $ NoArg (liftEwM (setDebugInvariants r))
  , defFlag "fclash-debug-count-transformations" $ NoArg (liftEwM (setDebugCountTransformations r))
  , defFlag "fclash-debug-transformations"       $ SepArg (setDebugTransformations r)
  , defFlag "fclash-debug-transformations-from"  $ IntSuffix (setDebugTransformationsFrom r)
  , defFlag "fclash-debug-transformations-limit" $ IntSuffix (setDebugTransformationsLimit r)
  , defFlag "fclash-debug-history"               $ AnySuffix (liftEwM . (setRewriteHistoryFile r))
  , defFlag "fclash-hdldir"                      $ SepArg (setHdlDir r)
  , defFlag "fclash-hdlsyn"                      $ SepArg (setHdlSyn r)
  , defFlag "fclash-nocache"                     $ NoArg (deprecated "nocache" "no-cache" setNoCache r)
  , defFlag "fclash-no-cache"                    $ NoArg (liftEwM (setNoCache r))
  , defFlag "fclash-no-check-inaccessible-idirs" $ NoArg (liftEwM (setNoIDirCheck r))
  , defFlag "fclash-no-clean"                    $ NoArg (setNoClean r)
  , defFlag "fclash-clear"                       $ NoArg (liftEwM (setClear r))
  , defFlag "fclash-no-prim-warn"                $ NoArg (liftEwM (setNoPrimWarn r))
  , defFlag "fclash-spec-limit"                  $ IntSuffix (liftEwM . setSpecLimit r)
  , defFlag "fclash-inline-limit"                $ IntSuffix (liftEwM . setInlineLimit r)
  , defFlag "fclash-inline-function-limit"       $ IntSuffix (liftEwM . setInlineFunctionLimit r)
  , defFlag "fclash-inline-constant-limit"       $ IntSuffix (liftEwM . setInlineConstantLimit r)
  , defFlag "fclash-evaluator-fuel-limit"        $ IntSuffix (liftEwM . setEvaluatorFuelLimit r)
  , defFlag "fclash-intwidth"                    $ IntSuffix (setIntWidth r)
  , defFlag "fclash-error-extra"                 $ NoArg (liftEwM (setErrorExtra r))
  , defFlag "fclash-float-support"               $ NoArg (setFloatSupport r)
  , defFlag "fclash-component-prefix"            $ SepArg (liftEwM . setComponentPrefix r)
  , defFlag "fclash-old-inline-strategy"         $ NoArg (liftEwM (setOldInlineStrategy r))
  , defFlag "fclash-no-escaped-identifiers"      $ NoArg (liftEwM (setNoEscapedIds r))
  , defFlag "fclash-lower-case-basic-identifiers"$ NoArg (liftEwM (setLowerCaseBasicIds r))
  , defFlag "fclash-compile-ultra"               $ NoArg (liftEwM (setUltra r))
  , defFlag "fclash-force-undefined"             $ OptIntSuffix (setUndefined r)
  , defFlag "fclash-aggressive-x-optimization"   $ NoArg (liftEwM (setAggressiveXOpt r))
  , defFlag "fclash-aggressive-x-optimization-blackboxes" $ NoArg (liftEwM (setAggressiveXOptBB r))
  , defFlag "fclash-inline-workfree-limit"       $ IntSuffix (liftEwM . setInlineWFLimit r)
  , defFlag "fclash-edalize"                     $ NoArg (liftEwM (setEdalize r))
  , defFlag "fclash-no-render-enums"             $ NoArg (liftEwM (setNoRenderEnums r))
  , defFlag "fclash-timescale-precision"         $ SepArg (setTimescalePrecision r)
  , defFlag "fclash-sanitize-netlist-ids"        $ NoArg (liftEwM (setSanitizeNames r))
  ]

-- | Print deprecated flag warning
deprecated
  :: String
  -- ^ Deprecated flag
  -> String
  -- ^ Use X instead
  -> (a -> IO ())
  -> a
  -> EwM IO ()
deprecated wrong right f a = do
  addWarn ("Using '-fclash-" ++ wrong
                             ++ "' is deprecated. Use '-fclash-"
                             ++ right
                             ++ "' instead.")
  liftEwM (f a)

setInlineLimit :: IORef ClashOpts
               -> Int
               -> IO ()
setInlineLimit r n = modifyIORef r (\c -> c {opt_inlineLimit = n})

setInlineFunctionLimit
  :: IORef ClashOpts
  -> Int
  -> IO ()
setInlineFunctionLimit r n = modifyIORef r (\c -> c {opt_inlineFunctionLimit = toEnum n})

setInlineConstantLimit
  :: IORef ClashOpts
  -> Int
  -> IO ()
setInlineConstantLimit r n = modifyIORef r (\c -> c {opt_inlineConstantLimit = toEnum n})

setEvaluatorFuelLimit
  :: IORef ClashOpts
  -> Int
  -> IO ()
setEvaluatorFuelLimit r n = modifyIORef r (\c -> c {opt_evaluatorFuelLimit = toEnum n})

setInlineWFLimit
  :: IORef ClashOpts
  -> Int
  -> IO ()
setInlineWFLimit r n = modifyIORef r (\c -> c {opt_inlineWFCacheLimit = toEnum n})

setSpecLimit :: IORef ClashOpts
             -> Int
             -> IO ()
setSpecLimit r n = modifyIORef r (\c -> c {opt_specLimit = n})

setDebugInvariants :: IORef ClashOpts -> IO ()
setDebugInvariants r =
  modifyIORef r $ \c ->
    c { opt_debug = (opt_debug c) { dbg_invariants = True } }

setDebugCountTransformations :: IORef ClashOpts -> IO ()
setDebugCountTransformations r =
  modifyIORef r $ \c ->
    c { opt_debug = (opt_debug c) { dbg_countTransformations = True } }

setDebugTransformations :: IORef ClashOpts -> String -> EwM IO ()
setDebugTransformations r s =
  liftEwM (modifyIORef r (setTransformations transformations))
 where
  transformations = Set.fromList (filter (not . null) (map trim (splitOn "," s)))
  trim = dropWhileEnd isSpace . dropWhile isSpace

  setTransformations xs opts =
    opts { opt_debug = (opt_debug opts) { dbg_transformations = xs } }

setDebugTransformationsFrom :: IORef ClashOpts -> Int -> EwM IO ()
setDebugTransformationsFrom r n =
  liftEwM (modifyIORef r (setFrom (fromIntegral n)))
 where
  setFrom from opts =
    opts { opt_debug = (opt_debug opts) { dbg_transformationsFrom = Just from } }

setDebugTransformationsLimit :: IORef ClashOpts -> Int -> EwM IO ()
setDebugTransformationsLimit r n =
  liftEwM (modifyIORef r (setLimit (fromIntegral n)))
 where
  setLimit limit opts =
    opts { opt_debug = (opt_debug opts) { dbg_transformationsLimit = Just limit } }

setDebugLevel :: IORef ClashOpts -> String -> EwM IO ()
setDebugLevel r s =
  case s of
    "DebugNone" ->
      liftEwM $ modifyIORef r (setLevel debugNone)
    "DebugSilent" ->
      liftEwM $ do
        modifyIORef r (setLevel debugSilent)
        setNoCache r
    "DebugFinal" ->
      liftEwM $ do
        modifyIORef r (setLevel debugFinal)
        setNoCache r
    "DebugCount" ->
      liftEwM $ do
        modifyIORef r (setLevel debugCount)
        setNoCache r
    "DebugName" ->
      liftEwM $ do
        modifyIORef r (setLevel debugName)
        setNoCache r
    "DebugTry" ->
      liftEwM $ do
        modifyIORef r (setLevel debugTry)
        setNoCache r
    "DebugApplied" ->
      liftEwM $ do
        modifyIORef r (setLevel debugApplied)
        setNoCache r
    "DebugAll" ->
      liftEwM $ do
        modifyIORef r (setLevel debugAll)
        setNoCache r
    _ ->
      addWarn (s ++ " is an invalid debug level")
 where
  setLevel lvl opts =
    opts { opt_debug = lvl }

setDebugInfo :: IORef ClashOpts -> String -> EwM IO ()
setDebugInfo r s =
  case readMaybe s of
    Just info ->
      liftEwM $ do
        modifyIORef r (setInfo info)
        when (info /= None) (setNoCache r)

    Nothing ->
      addWarn (s ++ " is an invalid debug info")
 where
  setInfo info opts =
    opts { opt_debug = (opt_debug opts) { dbg_transformationInfo = info } }

setNoCache :: IORef ClashOpts -> IO ()
setNoCache r = modifyIORef r (\c -> c {opt_cachehdl = False})

setNoIDirCheck :: IORef ClashOpts -> IO ()
setNoIDirCheck r = modifyIORef r (\c -> c {opt_checkIDir = False})

setNoClean :: a -> EwM IO ()
setNoClean _ = addWarn "-fclash-no-clean has been removed"

setClear :: IORef ClashOpts -> IO ()
setClear r = modifyIORef r (\c -> c {opt_clear = True})

setNoPrimWarn :: IORef ClashOpts -> IO ()
setNoPrimWarn r = modifyIORef r (\c -> c {opt_primWarn = False})

setIntWidth :: IORef ClashOpts
            -> Int
            -> EwM IO ()
setIntWidth r n =
  if n == 32 || n == 64
     then liftEwM $ modifyIORef r (\c -> c {opt_intWidth = n})
     else addWarn (show n ++ " is an invalid Int/Word/Integer bit-width. Allowed widths: 32, 64.")

setHdlDir :: IORef ClashOpts
          -> String
          -> EwM IO ()
setHdlDir r s = liftEwM $ modifyIORef r (\c -> c {opt_hdlDir = Just s})

setHdlSyn :: IORef ClashOpts
          -> String
          -> EwM IO ()
setHdlSyn r s = case readMaybe s of
  Just hdlSyn -> liftEwM $ modifyIORef r (\c -> c {opt_hdlSyn = hdlSyn})
  Nothing -> case s of
    "Xilinx"  -> liftEwM $ modifyIORef r (\c -> c {opt_hdlSyn = Vivado})
    "ISE"     -> liftEwM $ modifyIORef r (\c -> c {opt_hdlSyn = Vivado})
    "Altera"  -> liftEwM $ modifyIORef r (\c -> c {opt_hdlSyn = Quartus})
    "Intel"   -> liftEwM $ modifyIORef r (\c -> c {opt_hdlSyn = Quartus})
    _         -> addWarn (s ++ " is an unknown hdl synthesis tool")

setErrorExtra :: IORef ClashOpts -> IO ()
setErrorExtra r = modifyIORef r (\c -> c {opt_errorExtra = True})

setFloatSupport :: IORef ClashOpts -> EwM IO ()
setFloatSupport _ =
  addWarn "Deprecated flag: -fclash-float-support is always enabled from Clash 1.6 and onwards"

setComponentPrefix
  :: IORef ClashOpts
  -> String
  -> IO ()
setComponentPrefix r s =
  modifyIORef r (\c -> c {opt_componentPrefix = Just (Text.pack s)})

setTimescalePrecision
  :: IORef ClashOpts
  -> String
  -> EwM IO ()
setTimescalePrecision r s =
  case parsePeriod s of
    Just period ->
      liftEwM $ modifyIORef r $ \c ->
        c{opt_timescalePrecision = period}
    Nothing ->
      addWarn $ "Could not parse " <> s <> " as a period."

setOldInlineStrategy :: IORef ClashOpts -> IO ()
setOldInlineStrategy r = modifyIORef r (\c -> c {opt_newInlineStrat = False})

setNoEscapedIds :: IORef ClashOpts -> IO ()
setNoEscapedIds r = modifyIORef r (\c -> c {opt_escapedIds = False})

setLowerCaseBasicIds :: IORef ClashOpts -> IO ()
setLowerCaseBasicIds r = modifyIORef r (\c -> c {opt_lowerCaseBasicIds = ToLower})

setUltra :: IORef ClashOpts -> IO ()
setUltra r = modifyIORef r (\c -> c {opt_ultra = True})

setUndefined :: IORef ClashOpts -> Maybe Int -> EwM IO ()
setUndefined _ (Just x) | x < 0 || x > 1 =
  addWarn ("-fclash-force-undefined=" ++ show x ++ " ignored, " ++ show x ++
           " not in range [0,1]")
setUndefined r iM =
  liftEwM (modifyIORef r (\c -> c {opt_forceUndefined = Just iM}))

setAggressiveXOpt :: IORef ClashOpts -> IO ()
setAggressiveXOpt r = do
  modifyIORef r (\c -> c { opt_aggressiveXOpt = True })
  setAggressiveXOptBB r


setAggressiveXOptBB :: IORef ClashOpts -> IO ()
setAggressiveXOptBB r = modifyIORef r (\c -> c { opt_aggressiveXOptBB = True })

setEdalize :: IORef ClashOpts -> IO ()
setEdalize r = modifyIORef r (\c -> c { opt_edalize = True })

setRewriteHistoryFile :: IORef ClashOpts -> String -> IO ()
setRewriteHistoryFile r arg = do
  let fileNm = case drop (length "-fclash-debug-history=") arg of
                [] -> "history.dat"
                str -> str
  modifyIORef r (setFile fileNm)
 where
  setFile file opts =
    opts { opt_debug = (opt_debug opts) { dbg_historyFile = Just file } }

setNoRenderEnums :: IORef ClashOpts -> IO ()
setNoRenderEnums r = modifyIORef r (\c -> c { opt_renderEnums = False })

setSanitizeNames :: IORef ClashOpts -> IO ()
setSanitizeNames r = modifyIORef r (\c -> c { opt_sanitizeNames = True })
