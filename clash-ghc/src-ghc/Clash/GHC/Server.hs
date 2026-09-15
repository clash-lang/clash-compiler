{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  @clash --server@: a long-lived process that compiles designs on request,
  keeping its GHC session warm between them. See Note [Clash compile server].
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Clash.GHC.Server
  ( runServer
  ) where

import           Control.Exception (SomeException, evaluate, finally, throwIO, try)
import           Control.Monad (forM_, unless, when)
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Catch as MC
import           Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import           Data.List (partition)
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Time.Clock as Clock
import           Data.Word (Word64)
import           GHC.IO.Handle (hDuplicate, hDuplicateTo)
import           GHC.Stats (RTSStats (gc), GCDetails (gcdetails_live_bytes), getRTSStats, getRTSStatsEnabled)
import           System.Directory (getTemporaryDirectory, removeFile, setCurrentDirectory)
import           System.Environment (lookupEnv)
import           System.Exit (ExitCode (..))
import           System.Info (os)
import           System.IO
import           System.Mem (performMajorGC)
import           Text.Read (readMaybe)

import           GHC (Ghc, DynFlags)
import qualified GHC
import           GHC.Driver.Env (hsc_tmpfs)
import           GHC.Driver.Monad (Session, reflectGhc, reifyGhc)
import           GHC.Driver.Session
  (GeneralFlag (..), defaultFatalMessager, defaultFlushOut, gopt, packageFlagsChanged,
   pluginModNameOpts, pluginModNames, packageEnv, targetWays_, topDir)
import           GHC.Types.SrcLoc (mkGeneralLocated, unLoc)
import           GHC.Utils.TmpFs (cleanTempFiles)

import           Clash.Backend.SystemVerilog (SystemVerilogState)
import           Clash.Backend.VHDL (VHDLState)
import           Clash.Backend.Verilog (VerilogState)
import           Clash.Driver (clearPrimitiveCaches)
import           Clash.Driver.Types (ClashOpts (..), defClashOpts)
import           Clash.GHC.LoadModules (applySessionDynFlags, normalizeSessionDynFlags)
import           Clash.GHC.ClashFlags (parseClashFlags)
import           Clash.GHC.Util (handleClashException)
import           Clash.GHCi.UI (SessionMode (..), makeHDL)

{- Note [Clash compile server]

Most of the time Clash spends on a small design is per-process setup: dynamic
linking, loading a couple of hundred packages into the interpreter for the
type checker plugins, reading the interfaces of clash-prelude, and compiling
the primitive map. A server process does that once and then compiles one
design after another in the same GHC session, so each request only pays for
the design itself (see Note [Reusing a GHC session] in "Clash.GHC.LoadModules"
and Note [Caching compiled primitives] in "Clash.Driver").

The protocol is one JSON document per line on the server's standard input and
output:

>  {"id": 1, "cwd": "/abs/dir", "args": ["--verilog", "-i/abs/src", "Top", ...]}
>  {"id": 1, "exitCode": 0, "stdout": "...", "stderr": "...", "wallMs": 812, "sessionReset": false}

@args@ is exactly what the @clash@ executable would have received on its
command line, minus RTS options. A request is served as the executable would
serve it: the working directory is switched first (GHC looks for
@.ghc.environment@ files there when it parses the flags), then Clash's flags,
the HDL mode and GHC's flags are parsed from scratch against the flags the
server was started with, and the design is compiled with 'makeHDL'. Whatever
the compilation prints goes to the request's @stdout@ and @stderr@, and its
exit code is what the executable's would have been, including for errors:
they pass through the same handlers ('GHC.defaultErrorHandler',
'prettyPrintGhcErrors', 'GHC.handleSourceError', 'handleClashException').

The server announces itself with @{"ready": true}@ once it accepts requests;
anything before that line is startup output. The control messages
@{"reset": true}@ and @{"shutdown": true}@ rebuild the session and stop the
server. The session is also rebuilt on its own when a
request's package, plugin, way or interpreter flags differ from the session's
(these shape state the session keeps), after @CLASH_SERVER_MAX_REQUESTS@
requests (default 250), when the live heap after a request exceeds
@CLASH_SERVER_MAX_LIVE_BYTES@ (default 1.5 GB), or after a request failed with
something other than a compile error, since the session's state may then be
inconsistent.
-}

-- | What a client can send. See Note [Clash compile server].
data Request
  = Compile
      { reqId :: Int
      , reqCwd :: FilePath
      , reqArgs :: [String]
      }
  | Reset
  | Shutdown

instance Aeson.FromJSON Request where
  parseJSON = Aeson.withObject "request" $ \o -> do
    shutdown <- fromMaybe False <$> o .:? "shutdown"
    reset <- fromMaybe False <$> o .:? "reset"
    if shutdown then pure Shutdown
    else if reset then pure Reset
    else Compile <$> o .: "id" <*> o .: "cwd" <*> o .: "args"

-- | The answer to a 'Compile' request. See Note [Clash compile server].
data Response = Response
  { rspId :: Int
  , rspExitCode :: Int
  , rspStdout :: Text
  , rspStderr :: Text
  , rspWallMs :: Int
  , rspSessionReset :: Bool
  -- ^ Whether the session was rebuilt before serving this request
  }

instance Aeson.ToJSON Response where
  toJSON r = Aeson.object
    [ "id" .= rspId r
    , "exitCode" .= rspExitCode r
    , "stdout" .= rspStdout r
    , "stderr" .= rspStderr r
    , "wallMs" .= rspWallMs r
    , "sessionReset" .= rspSessionReset r
    ]

data ServerState = ServerState
  { ssSession :: Session
  , ssStartAction :: Ghc ()
  -- ^ Run in every fresh session, see 'GHC.defaultMainWithAction'
  , ssBaseDynFlags :: DynFlags
  -- ^ The flags every request's flags are parsed on top of: the executable's
  -- flags before the package environment was read
  , ssLastDynFlags :: IORef (Maybe DynFlags)
  -- ^ The flags of the previous request, to detect changes that need a fresh
  -- session; 'Nothing' right after a reset
  , ssServed :: IORef Int
  -- ^ Requests served by the current session
  , ssTainted :: IORef Bool
  -- ^ Whether the previous request failed in a way that leaves the session
  -- suspect
  , ssMaxRequests :: Int
  , ssMaxLiveBytes :: Word64
  , ssProtocolIn :: Handle
  , ssProtocolOut :: Handle
  }

-- | Serve compile requests until @{"shutdown": true}@ or end of input. See
-- Note [Clash compile server].
runServer
  :: Ghc ()
  -- ^ Action to run in every fresh session, see 'GHC.defaultMainWithAction'
  -> DynFlags
  -- ^ The executable's flags before the package environment was read
  -> Ghc ()
runServer startAction baseDflags = do
  session <- reifyGhc pure
  liftIO $ do
    -- The protocol keeps its own handles: the request's output takes over the
    -- process's standard streams while it is compiled.
    protoIn <- hDuplicate stdin
    protoOut <- hDuplicate stdout
    hSetBinaryMode protoIn True
    hSetBinaryMode protoOut True
    hSetBuffering protoOut LineBuffering
    maxRequests <- envNumber "CLASH_SERVER_MAX_REQUESTS" 250
    maxLiveBytes <- envNumber "CLASH_SERVER_MAX_LIVE_BYTES" (1500 * 1024 * 1024)
    st <- ServerState session startAction baseDflags
            <$> newIORef Nothing <*> newIORef 0 <*> newIORef False
            <*> pure maxRequests <*> pure maxLiveBytes <*> pure protoIn <*> pure protoOut
    -- Startup may already have printed to standard output (GHC announces the
    -- package environment it loaded); clients skip everything before this line.
    BL.hPutStr protoOut (Aeson.encode (Aeson.object ["ready" .= True]))
    BL.hPutStr protoOut "\n"
    hFlush protoOut
    serve st
 where
  envNumber :: (Read a, Num a) => String -> a -> IO a
  envNumber var def = maybe def (fromMaybe def . readMaybe) <$> lookupEnv var

serve :: ServerState -> IO ()
serve st = do
  eof <- hIsEOF (ssProtocolIn st)
  unless eof $ do
    line <- BS.hGetLine (ssProtocolIn st)
    if BS.null (BS.strip line) then serve st else
      case Aeson.eitherDecodeStrict line of
        Left err -> do
          respond st (Response 0 2 "" (Text.pack ("clash --server: malformed request: " ++ err)) 0 False)
          serve st
        Right Shutdown -> pure ()
        Right Reset -> do
          resetSession st
          serve st
        Right req@Compile{} -> do
          compileRequest st req >>= respond st
          serve st

respond :: ServerState -> Response -> IO ()
respond st rsp = do
  BL.hPutStr (ssProtocolOut st) (Aeson.encode rsp)
  BL.hPutStr (ssProtocolOut st) "\n"
  hFlush (ssProtocolOut st)

-- | Rebuild the GHC session from scratch, see Note [Clash compile server].
resetSession :: ServerState -> IO ()
resetSession st = do
  flip reflectGhc (ssSession st) $ do
    GHC.initGhcMonad (Just (topDir (ssBaseDynFlags st)))
    ssStartAction st
  clearPrimitiveCaches
  writeIORef (ssLastDynFlags st) Nothing
  writeIORef (ssServed st) 0
  writeIORef (ssTainted st) False

compileRequest :: ServerState -> Request -> IO Response
compileRequest _ Reset = error "compileRequest: not a compile request"
compileRequest _ Shutdown = error "compileRequest: not a compile request"
compileRequest st Compile{reqId, reqCwd, reqArgs} = do
  t0 <- Clock.getCurrentTime
  -- The working directory comes first: GHC looks for a package environment
  -- file there when the flags are parsed below.
  setCurrentDirectory reqCwd
  reflectGhc GHC.workingDirectoryChanged (ssSession st)

  -- Parse the request's flags the way the executable does, from scratch.
  parsed <- try @SomeException (reflectGhc (parseRequest (ssBaseDynFlags st) reqArgs) (ssSession st))
  case parsed of
    Left err -> finish t0 False (ExitFailure 1) "" (Text.pack (show err))
    Right (mode, dflags, opts, srcs) -> do
      -- Decide whether this request can share the session, see
      -- Note [Clash compile server].
      needFresh <- sessionNeedsReset st dflags
      when needFresh (resetSession st)
      writeIORef (ssLastDynFlags st) (Just dflags)

      (exitCode, out, err) <- withCapturedOutput $
        runLikeTheExecutable (ssSession st) dflags opts $ do
          -- Normalized here already, so the ways GHC settles on stay the same
          -- for the whole session; see 'applySessionDynFlags'.
          applySessionDynFlags (normalizeSessionDynFlags dflags)
          makeHDLFor mode opts srcs
      -- A failure that is not a compile error may have left the session in an
      -- odd state; the next request gets a fresh one.
      case exitCode of
        ExitFailure code | code /= 1 -> writeIORef (ssTainted st) True
        _ -> pure ()

      -- Housekeeping: temporary files of this compilation, and the memory check.
      flip reflectGhc (ssSession st) $ do
        env <- GHC.getSession
        logger <- GHC.getLogger
        unless (gopt Opt_KeepTmpFiles dflags) $
          liftIO (cleanTempFiles logger (hsc_tmpfs env))
      modifyIORef' (ssServed st) (+ 1)
      finish t0 needFresh exitCode out err
 where
  finish t0 wasReset exitCode out err = do
    t1 <- Clock.getCurrentTime
    let ms = round (realToFrac (Clock.diffUTCTime t1 t0) * (1000 :: Double)) :: Int
        code = case exitCode of { ExitSuccess -> 0; ExitFailure n -> n }
    pure (Response reqId code out err ms wasReset)

-- | Which HDL a request asks for.
data HdlMode = ModeVHDL | ModeVerilog | ModeSystemVerilog

-- | Parse a request's arguments: Clash's flags into 'ClashOpts', exactly one
-- HDL mode flag, and the rest as GHC flags plus source files or module names.
parseRequest
  :: DynFlags
  -> [String]
  -> Ghc (HdlMode, DynFlags, IORef ClashOpts, [FilePath])
parseRequest baseDflags args = do
  optsRef <- liftIO (newIORef defClashOpts)
  (argv1, _clashWarnings) <- liftIO (parseClashFlags optsRef (map (mkGeneralLocated "in the request") args))
  let (modeFlags, rest) = partition ((`elem` ["--vhdl", "--verilog", "--systemverilog"]) . unLoc) argv1
  mode <- case map unLoc modeFlags of
    ["--vhdl"] -> pure ModeVHDL
    ["--verilog"] -> pure ModeVerilog
    ["--systemverilog"] -> pure ModeSystemVerilog
    _ -> liftIO (throwIO (userError "clash --server: a request needs exactly one of --vhdl, --verilog and --systemverilog"))
  logger <- GHC.getLogger
  (dflags, fileish, _flagWarnings) <- GHC.parseDynamicFlags logger baseDflags rest
  let srcs = map unLoc fileish
  when (null srcs) $
    liftIO (throwIO (userError "clash --server: no input files in request"))
  -- Propagate -Werror to Clash, as the executable does
  liftIO $ modifyIORef' optsRef $ \opts ->
    opts { opt_werror = gopt Opt_WarnIsError dflags }
  pure (mode, dflags, optsRef, srcs)

-- | Whether the session must be rebuilt before serving a request with the
-- given flags. See Note [Clash compile server].
sessionNeedsReset :: ServerState -> DynFlags -> IO Bool
sessionNeedsReset st dflags = do
  tainted <- readIORef (ssTainted st)
  served <- readIORef (ssServed st)
  lastDflags <- readIORef (ssLastDynFlags st)
  tooBig <- liveBytesExceed (ssMaxLiveBytes st)
  let flagsChanged = case lastDflags of
        Nothing -> False
        Just old ->
             packageFlagsChanged dflags old
          || pluginModNames dflags /= pluginModNames old
          || pluginModNameOpts dflags /= pluginModNameOpts old
          || targetWays_ dflags /= targetWays_ old
          || packageEnv dflags /= packageEnv old
          || gopt Opt_ExternalInterpreter dflags /= gopt Opt_ExternalInterpreter old
  pure (tainted || served >= ssMaxRequests st || tooBig || flagsChanged)

liveBytesExceed :: Word64 -> IO Bool
liveBytesExceed limit = do
  enabled <- getRTSStatsEnabled
  if not enabled then pure False else do
    performMajorGC
    stats <- getRTSStats
    pure (gcdetails_live_bytes (gc stats) > limit)

makeHDLFor :: HdlMode -> IORef ClashOpts -> [FilePath] -> Ghc ()
makeHDLFor mode opts srcs = case mode of
  ModeVHDL -> makeHDL (Proxy @VHDLState) ReuseSession opts srcs
  ModeVerilog -> makeHDL (Proxy @VerilogState) ReuseSession opts srcs
  ModeSystemVerilog -> makeHDL (Proxy @SystemVerilogState) ReuseSession opts srcs

-- | Run a compilation under the handlers the @clash@ executable runs under, and
-- turn the exit it would have made into an 'ExitCode'.
runLikeTheExecutable :: Session -> DynFlags -> IORef ClashOpts -> Ghc () -> IO ExitCode
runLikeTheExecutable session dflags optsRef act = do
  opts <- readIORef optsRef
  logger <- reflectGhc GHC.getLogger session
  outcome <- try @ExitCode $
    GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut $
      GHC.prettyPrintGhcErrors logger $
        flip reflectGhc session $
          GHC.handleSourceError
            (\e -> do
              GHC.printException e
              liftIO (throwIO (ExitFailure 1)))
            (act `MC.catch` handleClashException dflags opts)
  pure (either id (const ExitSuccess) outcome)

-- | Run an action with the process's standard output and error streams sent to
-- temporary files, and return what it wrote. Standard input reads nothing.
withCapturedOutput :: IO a -> IO (a, Text, Text)
withCapturedOutput act = do
  tmp <- getTemporaryDirectory
  (outPath, outH) <- openTempFile tmp "clash-server-stdout.txt"
  (errPath, errH) <- openTempFile tmp "clash-server-stderr.txt"
  hSetEncoding outH utf8
  hSetEncoding errH utf8
  savedOut <- hDuplicate stdout
  savedErr <- hDuplicate stderr
  savedIn <- hDuplicate stdin
  devNull <- openFile (if os == "mingw32" then "NUL" else "/dev/null") ReadMode
  hFlush stdout
  hFlush stderr
  hDuplicateTo outH stdout
  hDuplicateTo errH stderr
  hDuplicateTo devNull stdin
  configureStreams
  result <- act `finally` do
    hFlush stdout
    hFlush stderr
    hDuplicateTo savedOut stdout
    hDuplicateTo savedErr stderr
    hDuplicateTo savedIn stdin
    configureStreams
    forM_ [savedOut, savedErr, savedIn, devNull] hClose
  hClose outH
  hClose errH
  out <- readUtf8 outPath
  err <- readUtf8 errPath
  removeFile outPath
  removeFile errPath
  pure (result, out, err)
 where
  -- 'hDuplicateTo' rebuilds the target handle with default buffering and
  -- encoding; give the streams back what the executable sets at startup.
  configureStreams = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    roundtrip <- mkTextEncoding "UTF-8//ROUNDTRIP"
    hSetEncoding stdout roundtrip
    hSetEncoding stderr roundtrip
  readUtf8 path = do
    h <- openFile path ReadMode
    hSetEncoding h utf8
    t <- Text.hGetContents h
    _ <- evaluate (Text.length t)
    hClose h
    pure t
