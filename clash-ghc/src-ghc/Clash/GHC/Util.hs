{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.GHC.Util
  ( module Clash.GHC.Util
  , module X
  )
where

#if MIN_VERSION_ghc(9,8,0)
import GHC.Driver.Errors.Types (GhcMessage(GhcUnknownMessage))
import GHC.Utils.Outputable as X (showPprUnsafe, showSDocUnsafe)
import GHC.Utils.Outputable (SDoc, neverQualify)
import GHC.Utils.Error (mkErrorMsgEnvelope, mkPlainError)
import GHC.Plugins
  (DynFlags, SourceError, SrcSpan, ($$), blankLine, empty,
   isGoodSrcSpan, liftIO, noSrcSpan, text, throwOneError)
import GHC.Types.Error (mkSimpleUnknownDiagnostic)
#else
import GHC.Driver.Errors.Types (GhcMessage(GhcUnknownMessage))
import GHC.Utils.Outputable as X (showPprUnsafe, showSDocUnsafe)
import GHC.Utils.Outputable (SDoc, neverQualify)
import GHC.Utils.Error (mkErrorMsgEnvelope, mkPlainError)
import GHC.Plugins
  (DynFlags, SourceError, SrcSpan, ($$), blankLine, empty,
   isGoodSrcSpan, liftIO, noSrcSpan, text, throwOneError)
import GHC.Types.Error (UnknownDiagnostic(..))
#endif
import GHC                (GhcMonad(..), printException)

#if !MIN_VERSION_ghc(9,12,0)
import Control.Exception  (ErrorCall(..))
#endif
import Control.Exception  (Exception(..))
import Control.Monad.IO.Class (MonadIO)
import GHC.Exception      (SomeException)
import System.Exit        (ExitCode(ExitFailure), exitWith)

import Clash.Util         (ClashException(..))
import Clash.Util.Exception (exceptionBacktrace)
import Clash.Util.Interpolate (i)
import Clash.Driver.Types (ClashOpts(..))

-- | Like 'lines', but returning a horizontally spaced SDoc instead of a list:
--
-- >>> textLines "a\nb"
-- a $$ b
textLines :: String -> SDoc
textLines s = foldl1 ($$) (map text (lines s))

-- | Throw an error at the given source location. GHC renders it like its own
-- errors, i.e., with a code snippet if the location is known.
throwPlainError :: MonadIO m => SrcSpan -> SDoc -> m a
throwPlainError sp msg =
  throwOneError $ mkErrorMsgEnvelope sp neverQualify $ GhcUnknownMessage $
#if MIN_VERSION_ghc(9,8,0)
    mkSimpleUnknownDiagnostic $ mkPlainError [] msg
#else
    UnknownDiagnostic $ mkPlainError [] msg
#endif

handleClashException
  :: GhcMonad m
  => DynFlags
  -> ClashOpts
  -> SomeException
  -> m a
handleClashException _df opts e = case fromException e of
  Just (ClashException sp s eM) -> do
    let srcInfo' | isGoodSrcSpan sp = srcInfo
                 | otherwise = empty
    throwPlainError sp
      (blankLine $$ textLines s $$ blankLine $$ srcInfo' $$ showExtra (opt_errorExtra opts) eM)
  _ -> case fromException e of
#if !MIN_VERSION_ghc(9,12,0)
    Just (ErrorCallWithLocation _ _) ->
      throwPlainError noSrcSpan
        (text "Clash error call:" $$ textLines (show e))
    _ -> case fromException e of
#endif
      Just (e' :: SourceError) -> do
        GHC.printException e'
        liftIO $ exitWith (ExitFailure 1)
      _ -> throwPlainError noSrcSpan
              (text "Other error:" $$ textLines (displayException e) $$ backtrace)
  where
    -- Since base-4.21 (GHC 9.12), displayException omits the exception context,
    -- so print it separately. On GHC 9.10 it already includes the context.
    backtrace = case exceptionBacktrace e of
      [] -> empty
      ctx -> blankLine $$ textLines ctx

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
