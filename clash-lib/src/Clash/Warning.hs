{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Emitting warnings from the Clash compiler.

  All compiler warnings should go through 'warn' (or one of its wrappers), so
  that @-Werror@ / @-fclash-werror@ consistently turns them into errors. Note
  that this is what makes a warning usable as a regression test: a test can
  pass @-fclash-werror@ in its @clashFlags@ and will then fail when Clash
  starts emitting a warning it did not emit before.
-}

module Clash.Warning
  ( warn
  , warnWhen
  ) where

import           Control.Exception              (throw)
import           Control.Monad                  (when)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import qualified System.Console.ANSI            as ANSI
import           System.Console.ANSI
  ( hSetSGR, SGR(SetConsoleIntensity, SetColor), Color(Magenta, Red)
  , ConsoleIntensity(BoldIntensity), ConsoleLayer(Foreground)
  , ColorIntensity(Vivid))
import           System.IO
  (hPutStrLn, stderr, hFlush, hIsTerminalDevice)

import           GHC.Types.SrcLoc               (noSrcSpan)

import           Clash.Driver.Bool              (OverridingBool(..))
import           Clash.Driver.Types             (ClashOpts(opt_color, opt_werror))
import           Clash.Util                     (ClashException(..))

-- | Emits a (colorized) warning to stderr. If warnings are treated as errors
-- (@-Werror@ or @-fclash-werror@, see 'opt_werror'), a 'ClashException' is
-- thrown instead.
warn
  :: ClashOpts
  -> String
  -> IO ()
warn opts msg = do
  useColor <-
    case opt_color opts of
      Always -> return True
      Never  -> return False
      Auto   -> hIsTerminalDevice stderr

  hSetSGR stderr [SetConsoleIntensity BoldIntensity]

  case opt_werror opts of
    True -> do
      when useColor $ hSetSGR stderr [SetColor Foreground Vivid Red]
      throw (ClashException noSrcSpan msg Nothing)

    False -> do
      when useColor $ hSetSGR stderr [SetColor Foreground Vivid Magenta]
      hPutStrLn stderr $ "[WARNING] " ++ msg
      hSetSGR stderr [ANSI.Reset]
      hFlush stderr

-- | 'warn', but only when the given condition holds, and lifted into any
-- 'MonadIO'. The message is not forced when the condition is 'False', so it is
-- fine to build an expensive one.
warnWhen
  :: MonadIO m
  => Bool
  -- ^ Only warn when 'True'
  -> ClashOpts
  -> String
  -> m ()
warnWhen cond opts msg = when cond (liftIO (warn opts msg))
