{-|
  Copyright   :  (C) 2015-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

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
import CLaSH.Netlist.BlackBox.Types (HdlSyn (..))
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
  , defFlag "clash-inline-function-limit" (IntSuffix (liftEwM . setInlineFunctionLimit r))
  , defFlag "clash-inline-constant-limit" (IntSuffix (liftEwM . setInlineConstantLimit r))
  , defFlag "clash-debug" (SepArg (setDebugLevel r))
  , defFlag "clash-noclean" (NoArg (liftEwM (setNoClean r)))
  , defFlag "clash-intwidth" (IntSuffix (setIntWidth r))
  , defFlag "clash-hdldir" (SepArg (setHdlDir r))
  , defFlag "clash-hdlsyn" (SepArg (setHdlSyn r))
  , defFlag "clash-error-extra" (NoArg (liftEwM (setErrorExtra r)))
  , defFlag "clash-float-support" (NoArg (liftEwM (setFloatSupport r)))
  , defFlag "clash-allow-zero-width" (NoArg (liftEwM (setAllowZeroWidth r)))
  , defFlag "clash-allow-invalid-coercions" (NoArg (liftEwM (setAllowInvalidCoercions r)))
  ]

setInlineLimit :: IORef CLaSHOpts
               -> Int
               -> IO ()
setInlineLimit r n = modifyIORef r (\c -> c {opt_inlineLimit = n})

setInlineFunctionLimit
  :: IORef CLaSHOpts
  -> Int
  -> IO ()
setInlineFunctionLimit r n = modifyIORef r (\c -> c {opt_inlineFunctionLimit = toEnum n})

setInlineConstantLimit
  :: IORef CLaSHOpts
  -> Int
  -> IO ()
setInlineConstantLimit r n = modifyIORef r (\c -> c {opt_inlineConstantLimit = toEnum n})

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

setNoClean :: IORef CLaSHOpts -> IO ()
setNoClean r = modifyIORef r (\c -> c {opt_cleanhdl = False})

setIntWidth :: IORef CLaSHOpts
            -> Int
            -> EwM IO ()
setIntWidth r n =
  if n == 32 || n == 64
     then liftEwM $ modifyIORef r (\c -> c {opt_intWidth = n})
     else addWarn (show n ++ " is an invalid Int/Word/Integer bit-width. Allowed widths: 32, 64.")

setHdlDir :: IORef CLaSHOpts
          -> String
          -> EwM IO ()
setHdlDir r s = liftEwM $ modifyIORef r (\c -> c {opt_hdlDir = Just s})

setHdlSyn :: IORef CLaSHOpts
          -> String
          -> EwM IO ()
setHdlSyn r s = case readMaybe s of
  Just hdlSyn -> liftEwM $ modifyIORef r (\c -> c {opt_hdlSyn = hdlSyn})
  Nothing     -> if s == "Xilinx"
                    then liftEwM $ modifyIORef r (\c -> c {opt_hdlSyn = Vivado})
                    else addWarn (s ++ " is an unknown hdl synthesis tool")

setErrorExtra :: IORef CLaSHOpts -> IO ()
setErrorExtra r = modifyIORef r (\c -> c {opt_errorExtra = True})

setFloatSupport :: IORef CLaSHOpts -> IO ()
setFloatSupport r = modifyIORef r (\c -> c {opt_floatSupport = True})

setAllowZeroWidth :: IORef CLaSHOpts -> IO ()
setAllowZeroWidth r = modifyIORef r (\c -> c {opt_allowZero = True})

setAllowInvalidCoercions :: IORef CLaSHOpts -> IO ()
setAllowInvalidCoercions r = modifyIORef r (\c -> c {opt_errorInvalidCoercions = False})
