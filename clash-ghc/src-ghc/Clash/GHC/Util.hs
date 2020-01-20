module Clash.GHC.Util where

import ErrUtils           (mkPlainErrMsg)
import GHC                (GhcMonad(..), printException)
import GhcPlugins         (DynFlags, SourceError, ($$), blankLine, empty, isGoodSrcSpan, liftIO, noSrcSpan, text, throwOneError)

import Control.Exception  (Exception(..), ErrorCall(..))
import GHC.Exception      (SomeException)
import System.Exit        (ExitCode(ExitFailure), exitWith)

import Clash.Util         (ClashException(..))
import Clash.Driver.Types (ClashOpts(..))

handleClashException
  :: GhcMonad m
  => DynFlags
  -> ClashOpts
  -> SomeException
  -> m a
handleClashException df opts e = case fromException e of
  Just (ClashException sp s eM) -> do
    let srcInfo' | isGoodSrcSpan sp = srcInfo
                 | otherwise = empty
    throwOneError (mkPlainErrMsg df sp (text s $$ srcInfo' $$ showExtra (opt_errorExtra opts) eM))
  _ -> case fromException e of
    Just (ErrorCallWithLocation _ _) ->
      throwOneError (mkPlainErrMsg df noSrcSpan (text "Clash error call:" $$ text (show e)))
    _ -> case fromException e of
      Just (e' :: SourceError) -> do
        GHC.printException e'
        liftIO $ exitWith (ExitFailure 1)
      _ -> throwOneError (mkPlainErrMsg df noSrcSpan (text "Other error:" $$ text (displayException e)))
  where
    srcInfo = text "NB: The source location of the error is not exact, only indicative, as it is acquired after optimisations." $$
              text "The actual location of the error can be in a function that is inlined." $$
              text "To prevent inlining of those functions, annotate them with a NOINLINE pragma."

    showExtra False (Just _)   =
      blankLine $$
      text "This error contains additional information, rerun with '-fclash-error-extra' to show this information."
    showExtra True  (Just msg) =
      blankLine $$
      text "Additional information:" $$ blankLine $$
      text msg
    showExtra _ _ = empty
