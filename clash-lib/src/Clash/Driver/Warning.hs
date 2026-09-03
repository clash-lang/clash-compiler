{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Emit named warnings, respecting the warning options in 'ClashOpts'. See
  "Clash.Warning" for the warnings and the flags controlling them.

  This module is deliberately kept apart from "Clash.Warning": 'ClashOpts' has
  a 'Clash.Warning.WarningOpts' field, so "Clash.Driver.Types" imports
  "Clash.Warning". Anything mentioning 'ClashOpts' - everything below - would
  therefore introduce an import cycle if it lived in "Clash.Warning".
-}

{-# LANGUAGE DefaultSignatures #-}

module Clash.Driver.Warning
  ( PendingWarning(..)
  , warnAbout
  , warnAboutM
    -- * Monads that can emit warnings
  , CanWarn(..)
  ) where

import Control.Exception (throw)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Console.ANSI
  (Color (Magenta), ColorIntensity (Vivid), ConsoleIntensity (BoldIntensity),
   ConsoleLayer (Foreground), SGR (Reset, SetColor, SetConsoleIntensity),
   hSetSGR)
import System.IO
  (hFlush, hIsTerminalDevice, hPutStrLn, stderr)

import GHC.Types.SrcLoc (SrcSpan, isGoodSrcSpan)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

import Clash.Driver.Bool (OverridingBool (..))
import Clash.Driver.Types (ClashOpts (..), HasClashOpts (..))
import Clash.Util (ClashException (..))
import Clash.Warning (ClashWarning, warningName, wopt, woptFatal)

-- | A warning waiting to be reported. Pure passes collect these; whether the
-- warning is enabled or fatal is decided by 'warnAbout' when it is reported.
data PendingWarning = PendingWarning
  { pending_warning :: ClashWarning
  , pending_srcSpan :: SrcSpan
  , pending_message :: String
  }

-- | Monads Clash can emit warnings from with 'warnAboutM'.
class HasClashOpts m => CanWarn m where
  -- | Deliver a warning. Monads that can do IO report it right away, which is
  -- what the default implementation does. Pure monads collect the warning
  -- instead, leaving it to the driver to report it with 'warnAbout' later.
  reportWarning :: PendingWarning -> m ()

  default reportWarning :: MonadIO m => PendingWarning -> m ()
  reportWarning pw = do
    opts <- askClashOpts
    liftIO (warnAbout opts pw)

-- | Report the given warning: a no-op if the warning is disabled, a (colorized)
-- message on stderr if it is enabled, and a 'ClashException' if it is promoted
-- to an error (@-Werror@ or @-Werror=\<name\>@).
warnAbout :: ClashOpts -> PendingWarning -> IO ()
warnAbout opts (PendingWarning w sp msg) = when (wopt w (opt_warnings opts)) $
  if woptFatal (opt_werror opts) w (opt_warnings opts) then
    throw (ClashException sp (msg <> " [-Werror=" <> warningName w <> "]") Nothing)
  else do
    useColor <-
      case opt_color opts of
        Always -> return True
        Never  -> return False
        Auto   -> hIsTerminalDevice stderr

    hSetSGR stderr [SetConsoleIntensity BoldIntensity]
    when useColor $ hSetSGR stderr [SetColor Foreground Vivid Magenta]
    hPutStrLn stderr ("[WARNING] " <> loc <> msg <> " [-W" <> warningName w <> "]")
    hSetSGR stderr [Reset]
    hFlush stderr
 where
  loc
    | isGoodSrcSpan sp = showSDocUnsafe (ppr sp) <> ": "
    | otherwise = ""

-- | 'warnAbout' in whatever monad Clash is currently running in. Disabled
-- warnings are dropped here, so pure monads don't collect warnings nobody is
-- going to report. Note that 'warnAbout' checks again, as a warning can also
-- be reported without going through this function.
warnAboutM :: CanWarn m => ClashWarning -> SrcSpan -> String -> m ()
warnAboutM w sp msg = do
  opts <- askClashOpts
  when (wopt w (opt_warnings opts)) (reportWarning (PendingWarning w sp msg))
