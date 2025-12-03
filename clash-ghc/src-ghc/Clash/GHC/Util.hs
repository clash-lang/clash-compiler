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
  (DynFlags, SourceError, ($$), blankLine, empty, isGoodSrcSpan, liftIO,
   noSrcSpan, text, throwOneError)
import GHC.Types.Error (mkSimpleUnknownDiagnostic)
#else
import GHC.Driver.Errors.Types (GhcMessage(GhcUnknownMessage))
import GHC.Utils.Outputable as X (showPprUnsafe, showSDocUnsafe)
import GHC.Utils.Outputable (SDoc, neverQualify)
import GHC.Utils.Error (mkErrorMsgEnvelope, mkPlainError)
import GHC.Plugins
  (DynFlags, SourceError, ($$), blankLine, empty, isGoodSrcSpan, liftIO,
   noSrcSpan, text, throwOneError)
import GHC.Types.Error (UnknownDiagnostic(..))
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
handleClashException _df opts e = case fromException e of
  Just (ClashException sp s eM) -> do
    let srcInfo' | isGoodSrcSpan sp = srcInfo
                 | otherwise = empty
    throwOneError
#if MIN_VERSION_ghc(9,8,0)
      (mkErrorMsgEnvelope sp neverQualify $ GhcUnknownMessage $ mkSimpleUnknownDiagnostic $ mkPlainError []
#else
      (mkErrorMsgEnvelope sp neverQualify $ GhcUnknownMessage $ UnknownDiagnostic $ mkPlainError []
#endif
        (blankLine $$ textLines s $$ blankLine $$ srcInfo' $$ showExtra (opt_errorExtra opts) eM))
  _ -> case fromException e of
    Just (ErrorCallWithLocation _ _) ->
      throwOneError
#if MIN_VERSION_ghc(9,8,0)
        (mkErrorMsgEnvelope noSrcSpan neverQualify $ GhcUnknownMessage $ mkSimpleUnknownDiagnostic $ mkPlainError []
#else
        (mkErrorMsgEnvelope noSrcSpan neverQualify $ GhcUnknownMessage $ UnknownDiagnostic $ mkPlainError []
#endif
        (text "Clash error call:" $$ textLines (show e)))
    _ -> case fromException e of
      Just (e' :: SourceError) -> do
        GHC.printException e'
        liftIO $ exitWith (ExitFailure 1)
      _ -> throwOneError
#if MIN_VERSION_ghc(9,8,0)
              (mkErrorMsgEnvelope noSrcSpan neverQualify $ GhcUnknownMessage $ mkSimpleUnknownDiagnostic $ mkPlainError []
#else
              (mkErrorMsgEnvelope noSrcSpan neverQualify $ GhcUnknownMessage $ UnknownDiagnostic $ mkPlainError []
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
