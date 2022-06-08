{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.GHC.Util where

#if MIN_VERSION_ghc(9,0,0)
import GHC.Utils.Outputable (SDoc)
import GHC.Utils.Error (mkPlainErrMsg)
import GHC.Plugins
  (DynFlags, SourceError, ($$), blankLine, empty, isGoodSrcSpan, liftIO,
   noSrcSpan, text, throwOneError)
#else
import Outputable         (SDoc)
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
handleClashException df opts e = case fromException e of
  Just (ClashException sp s eM) -> do
    let srcInfo' | isGoodSrcSpan sp = srcInfo
                 | otherwise = empty
    throwOneError (mkPlainErrMsg df sp (blankLine $$ textLines s $$ blankLine $$ srcInfo' $$ showExtra (_opt_errorExtra opts) eM))
  _ -> case fromException e of
    Just (ErrorCallWithLocation _ _) ->
      throwOneError (mkPlainErrMsg df noSrcSpan (text "Clash error call:" $$ textLines (show e)))
    _ -> case fromException e of
      Just (e' :: SourceError) -> do
        GHC.printException e'
        liftIO $ exitWith (ExitFailure 1)
      _ -> throwOneError (mkPlainErrMsg df noSrcSpan (text "Other error:" $$ textLines (displayException e)))
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
