{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Emit named warnings, respecting the warning options in 'ClashOpts'. See
  "Clash.Warning" for the warnings and the flags controlling them.
-}

module Clash.Driver.Warning
  ( warnAbout
  , warnAboutPure
  ) where

import Control.Exception (throw)
import Control.Monad (when)
import System.Console.ANSI
  (Color (Magenta), ColorIntensity (Vivid), ConsoleIntensity (BoldIntensity),
   ConsoleLayer (Foreground), SGR (Reset, SetColor, SetConsoleIntensity),
   hSetSGR)
import System.IO
  (hFlush, hIsTerminalDevice, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

import GHC.Types.SrcLoc (SrcSpan, isGoodSrcSpan)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

import Clash.Driver.Bool (OverridingBool (..))
import Clash.Driver.Types (ClashOpts (..))
import Clash.Util (ClashException (..))
import Clash.Warning (ClashWarning, warningName, wopt, woptFatal)

-- | Emit the given warning: a no-op if the warning is disabled, a (colorized)
-- message on stderr if it is enabled, and a 'ClashException' if it is promoted
-- to an error (@-Werror@ or @-Werror=\<name\>@).
warnAbout :: ClashOpts -> ClashWarning -> SrcSpan -> String -> IO ()
warnAbout opts w sp msg = when (wopt w (opt_warnings opts)) $
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

-- | 'warnAbout' for pure contexts; emits the warning when the result is
-- forced, like 'Debug.Trace.trace'.
warnAboutPure :: ClashOpts -> ClashWarning -> SrcSpan -> String -> a -> a
warnAboutPure opts w sp msg a =
  unsafePerformIO (warnAbout opts w sp msg >> pure a)
{-# NOINLINE warnAboutPure #-}
