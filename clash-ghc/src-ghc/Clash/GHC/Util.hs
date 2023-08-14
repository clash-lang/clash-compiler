{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.GHC.Util
  ( module Clash.GHC.Util
#if MIN_VERSION_ghc(9,2,0)
  , module X
#endif
  )
where

#if MIN_VERSION_ghc(9,8,0)
import GHC.Driver.Errors.Types (GhcMessage(GhcUnknownMessage))
import GHC.Utils.Outputable as X (showPprUnsafe, showSDocUnsafe)
import GHC.Utils.Outputable (SDoc, neverQualify)
import GHC.Utils.Error (mkErrorMsgEnvelope, mkPlainError)
import GHC.Plugins
  (DynFlags, SourceError, ($$), blankLine, empty, isGoodSrcSpan, liftIO,
   noSrcSpan, text, throwOneError)
import GHC.Types.Error (mkSimpleUnknownDiagnostic)
#elif MIN_VERSION_ghc(9,6,0)
import GHC.Driver.Errors.Types (GhcMessage(GhcUnknownMessage))
import GHC.Utils.Outputable as X (showPprUnsafe, showSDocUnsafe)
import GHC.Utils.Outputable (SDoc, neverQualify)
import GHC.Utils.Error (mkErrorMsgEnvelope, mkPlainError)
import GHC.Plugins
  (DynFlags, SourceError, ($$), blankLine, empty, isGoodSrcSpan, liftIO,
   noSrcSpan, text, throwOneError)
import GHC.Types.Error (UnknownDiagnostic(..))
#elif MIN_VERSION_ghc(9,4,0)
import GHC.Driver.Errors.Types (GhcMessage(GhcUnknownMessage))
import GHC.Utils.Outputable as X (showPprUnsafe, showSDocUnsafe)
import GHC.Utils.Outputable (SDoc, neverQualify)
import GHC.Utils.Error (mkErrorMsgEnvelope, mkPlainError)
import GHC.Plugins
  (DynFlags, SourceError, ($$), blankLine, empty, isGoodSrcSpan, liftIO,
   noSrcSpan, text, throwOneError)
#elif MIN_VERSION_ghc(9,2,0)
import GHC.Utils.Outputable as X (showPprUnsafe, showSDocUnsafe)
import GHC.Utils.Outputable (SDoc, neverQualify)
import GHC.Utils.Error (mkMsgEnvelope)
import GHC.Plugins
  (DynFlags, SourceError, ($$), blankLine, empty, isGoodSrcSpan, liftIO,
   noSrcSpan, text, throwOneError)
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Driver.Session (unsafeGlobalDynFlags)
import GHC.Utils.Outputable (Outputable, SDoc, showPpr, showSDoc)
import GHC.Utils.Error (mkPlainErrMsg)
import GHC.Plugins
  (DynFlags, SourceError, ($$), blankLine, empty, isGoodSrcSpan, liftIO,
   noSrcSpan, text, throwOneError)
#else
import DynFlags           (unsafeGlobalDynFlags)
import Outputable         (Outputable, SDoc, showPpr, showSDoc)
import ErrUtils           (mkPlainErrMsg)
import GhcPlugins         (DynFlags, SourceError, ($$), blankLine, empty, isGoodSrcSpan, liftIO, noSrcSpan, text, throwOneError)
#endif
import GHC                (GhcMonad(..), printException)

import Control.Exception  (Exception(..), ErrorCall(..))
import GHC.Exception      (SomeException)
import System.Exit        (ExitCode(ExitFailure), exitWith)

import Clash.Util         (ClashException(..))
import Clash.Util.Interpolate (i)
import Clash.Driver.Types (ClashOpts(..))

-- | Like 'lines', but returning a horizontally spaced SDoc instead of a list:
--
-- >>> textLines "a\nb"
-- a $$ b
textLines :: String -> SDoc
textLines s = foldl1 ($$) (map text (lines s))

handleClashException
  :: GhcMonad m
  => DynFlags
  -> ClashOpts
  -> SomeException
  -> m a
#if MIN_VERSION_ghc(9,2,0)
handleClashException _df opts e = case fromException e of
#else
handleClashException df opts e = case fromException e of
#endif
  Just (ClashException sp s eM) -> do
    let srcInfo' | isGoodSrcSpan sp = srcInfo
                 | otherwise = empty
    throwOneError
#if MIN_VERSION_ghc(9,8,0)
      (mkErrorMsgEnvelope sp neverQualify $ GhcUnknownMessage $ mkSimpleUnknownDiagnostic $ mkPlainError []
#elif MIN_VERSION_ghc(9,6,0)
      (mkErrorMsgEnvelope sp neverQualify $ GhcUnknownMessage $ UnknownDiagnostic $ mkPlainError []
#elif MIN_VERSION_ghc(9,4,0)
      (mkErrorMsgEnvelope sp neverQualify $ GhcUnknownMessage $ mkPlainError []
#elif MIN_VERSION_ghc(9,2,0)
      (mkMsgEnvelope sp neverQualify
#else
      (mkPlainErrMsg df sp
#endif
        (blankLine $$ textLines s $$ blankLine $$ srcInfo' $$ showExtra (opt_errorExtra opts) eM))
  _ -> case fromException e of
    Just (ErrorCallWithLocation _ _) ->
      throwOneError
#if MIN_VERSION_ghc(9,8,0)
        (mkErrorMsgEnvelope noSrcSpan neverQualify $ GhcUnknownMessage $ mkSimpleUnknownDiagnostic $ mkPlainError []
#elif MIN_VERSION_ghc(9,6,0)
        (mkErrorMsgEnvelope noSrcSpan neverQualify $ GhcUnknownMessage $ UnknownDiagnostic $ mkPlainError []
#elif MIN_VERSION_ghc(9,4,0)
        (mkErrorMsgEnvelope noSrcSpan neverQualify $ GhcUnknownMessage $ mkPlainError []
#elif MIN_VERSION_ghc(9,2,0)
        (mkMsgEnvelope noSrcSpan neverQualify
#else
        (mkPlainErrMsg df noSrcSpan
#endif
        (text "Clash error call:" $$ textLines (show e)))
    _ -> case fromException e of
      Just (e' :: SourceError) -> do
        GHC.printException e'
        liftIO $ exitWith (ExitFailure 1)
      _ -> throwOneError
#if MIN_VERSION_ghc(9,8,0)
              (mkErrorMsgEnvelope noSrcSpan neverQualify $ GhcUnknownMessage $ mkSimpleUnknownDiagnostic $ mkPlainError []
#elif MIN_VERSION_ghc(9,6,0)
              (mkErrorMsgEnvelope noSrcSpan neverQualify $ GhcUnknownMessage $ UnknownDiagnostic $ mkPlainError []
#elif MIN_VERSION_ghc(9,4,0)
              (mkErrorMsgEnvelope noSrcSpan neverQualify $ GhcUnknownMessage  $ mkPlainError []
#elif MIN_VERSION_ghc(9,2,0)
              (mkMsgEnvelope noSrcSpan neverQualify
#else
              (mkPlainErrMsg df noSrcSpan
#endif
              (text "Other error:" $$ textLines (displayException e)))
  where
    srcInfo = textLines [i|
      The source location of the error is not exact, only indicative, as it
      is acquired after optimizations. The actual location of the error can be
      in a function that is inlined. To prevent inlining of those functions,
      annotate them with a NOINLINE pragma.
    |]

    showExtra False (Just _)   =
      blankLine $$
      text "This error contains additional information, rerun with '-fclash-error-extra' to show this information."
    showExtra True  (Just msg) =
      blankLine $$
      text "Additional information:" $$ blankLine $$
      textLines msg
    showExtra _ _ = empty

#if !MIN_VERSION_ghc(9,2,0)
showPprUnsafe :: Outputable a => a -> String
showPprUnsafe = showPpr unsafeGlobalDynFlags

showSDocUnsafe :: SDoc -> String
showSDocUnsafe = showSDoc unsafeGlobalDynFlags
#endif
