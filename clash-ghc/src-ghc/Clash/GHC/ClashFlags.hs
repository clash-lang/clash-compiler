{-|
  Copyright   :  (C) 2015-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE TupleSections #-}

module Clash.GHC.ClashFlags
  ( parseClashFlags
  )
where

import CmdLineParser
import Panic
import SrcLoc

import Data.IORef
import Control.Monad
import Clash.Driver.Types
import Clash.Netlist.BlackBox.Types (HdlSyn (..))
import Text.Read (readMaybe)

parseClashFlags :: IORef ClashOpts -> [Located String]
                -> IO ([Located String],[Located String])
parseClashFlags r = parseClashFlagsFull (flagsClash r)

parseClashFlagsFull :: [Flag IO] -> [Located String]
                    -> IO ([Located String],[Located String])
parseClashFlagsFull flagsAvialable args = do
  (leftovers,errs,warns) <- processArgs flagsAvialable args

  unless (null errs) $ throwGhcExceptionIO $
    errorsToGhcException . map (("on the commandline", ) . unLoc) $ errs

  return (leftovers, warns)

flagsClash :: IORef ClashOpts -> [Flag IO]
flagsClash r = [
    defFlag "fclash-debug"                   $ SepArg (setDebugLevel r)
  , defFlag "fclash-hdldir"                  $ SepArg (setHdlDir r)
  , defFlag "fclash-hdlsyn"                  $ SepArg (setHdlSyn r)
  , defFlag "fclash-nocache"                 $ NoArg (liftEwM (setNoCache r))
  , defFlag "fclash-noclean"                 $ NoArg (liftEwM (setNoClean r))
  , defFlag "fclash-spec-limit"              $ IntSuffix (liftEwM . setSpecLimit r)
  , defFlag "fclash-inline-limit"            $ IntSuffix (liftEwM . setInlineLimit r)
  , defFlag "fclash-inline-function-limit"   $ IntSuffix (liftEwM . setInlineFunctionLimit r)
  , defFlag "fclash-inline-constant-limit"   $ IntSuffix (liftEwM . setInlineConstantLimit r)
  , defFlag "fclash-intwidth"                $ IntSuffix (setIntWidth r)
  , defFlag "fclash-error-extra"             $ NoArg (liftEwM (setErrorExtra r))
  , defFlag "fclash-float-support"           $ NoArg (liftEwM (setFloatSupport r))
  , defFlag "fclash-allow-zero-width"        $ NoArg (liftEwM (setAllowZeroWidth r))
  ]

setInlineLimit :: IORef ClashOpts
               -> Int
               -> IO ()
setInlineLimit r n = modifyIORef r (\c -> c {opt_inlineLimit = n})

setInlineFunctionLimit
  :: IORef ClashOpts
  -> Int
  -> IO ()
setInlineFunctionLimit r n = modifyIORef r (\c -> c {opt_inlineFunctionLimit = toEnum n})

setInlineConstantLimit
  :: IORef ClashOpts
  -> Int
  -> IO ()
setInlineConstantLimit r n = modifyIORef r (\c -> c {opt_inlineConstantLimit = toEnum n})

setSpecLimit :: IORef ClashOpts
             -> Int
             -> IO ()
setSpecLimit r n = modifyIORef r (\c -> c {opt_specLimit = n})

setDebugLevel :: IORef ClashOpts
              -> String
              -> EwM IO ()
setDebugLevel r s = case readMaybe s of
  Just dbgLvl -> liftEwM $ modifyIORef r (\c -> c {opt_dbgLevel = dbgLvl})
  Nothing     -> addWarn (s ++ " is an invalid debug level")

setNoCache :: IORef ClashOpts -> IO ()
setNoCache r = modifyIORef r (\c -> c {opt_cachehdl = False})

setNoClean :: IORef ClashOpts -> IO ()
setNoClean r = modifyIORef r (\c -> c {opt_cleanhdl = False})

setIntWidth :: IORef ClashOpts
            -> Int
            -> EwM IO ()
setIntWidth r n =
  if n == 32 || n == 64
     then liftEwM $ modifyIORef r (\c -> c {opt_intWidth = n})
     else addWarn (show n ++ " is an invalid Int/Word/Integer bit-width. Allowed widths: 32, 64.")

setHdlDir :: IORef ClashOpts
          -> String
          -> EwM IO ()
setHdlDir r s = liftEwM $ modifyIORef r (\c -> c {opt_hdlDir = Just s})

setHdlSyn :: IORef ClashOpts
          -> String
          -> EwM IO ()
setHdlSyn r s = case readMaybe s of
  Just hdlSyn -> liftEwM $ modifyIORef r (\c -> c {opt_hdlSyn = hdlSyn})
  Nothing     -> if s == "Xilinx"
                    then liftEwM $ modifyIORef r (\c -> c {opt_hdlSyn = Vivado})
                    else addWarn (s ++ " is an unknown hdl synthesis tool")

setErrorExtra :: IORef ClashOpts -> IO ()
setErrorExtra r = modifyIORef r (\c -> c {opt_errorExtra = True})

setFloatSupport :: IORef ClashOpts -> IO ()
setFloatSupport r = modifyIORef r (\c -> c {opt_floatSupport = True})

setAllowZeroWidth :: IORef ClashOpts -> IO ()
setAllowZeroWidth r = modifyIORef r (\c -> c {opt_allowZero = True})
