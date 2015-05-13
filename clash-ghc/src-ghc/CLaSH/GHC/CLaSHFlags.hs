{-# LANGUAGE TupleSections #-}
module CLaSH.GHC.CLaSHFlags
  ( parseCLaSHFlags
  )
where

import CmdLineParser
import Panic
import SrcLoc

import Data.IORef
import Control.Monad
import CLaSH.Driver.Types
import Text.Read (readMaybe)

parseCLaSHFlags :: IORef CLaSHOpts -> [Located String]
                -> IO ([Located String],[Located String])
parseCLaSHFlags r = parseCLaSHFlagsFull (flagsClash r)

parseCLaSHFlagsFull :: [Flag IO] -> [Located String]
                    -> IO ([Located String],[Located String])
parseCLaSHFlagsFull flagsAvialable args = do
  (leftovers,errs,warns) <- processArgs flagsAvialable args

  unless (null errs) $ throwGhcExceptionIO $
    errorsToGhcException . map (("on the commandline", ) . unLoc) $ errs

  return (leftovers, warns)

flagsClash :: IORef CLaSHOpts -> [Flag IO]
flagsClash r = [
    defFlag "clash-inline-limit" (IntSuffix (liftEwM . setInlineLimit r))
  , defFlag "clash-spec-limit" (IntSuffix (liftEwM . setSpecLimit r))
  , defFlag "clash-debug" (SepArg (setDebugLevel r))
  ]

setInlineLimit :: IORef CLaSHOpts
               -> Int
               -> IO ()
setInlineLimit r n = modifyIORef r (\c -> c {opt_inlineLimit = n})

setSpecLimit :: IORef CLaSHOpts
             -> Int
             -> IO ()
setSpecLimit r n = modifyIORef r (\c -> c {opt_specLimit = n})

setDebugLevel :: IORef CLaSHOpts
              -> String
              -> EwM IO ()
setDebugLevel r s = case readMaybe s of
  Just dbgLvl -> liftEwM $ modifyIORef r (\c -> c {opt_dbgLevel = dbgLvl})
  Nothing     -> addWarn (s ++ " is an invalid debug level")
