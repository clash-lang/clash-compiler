{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  A pool of @clash --server@ workers that compile the test suite's designs, so
  that GHC's session stays warm across tests instead of being set up once per
  test. See Note [Clash compile server] in "Clash.GHC.Server" for the protocol.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Tasty.Clash.Server
  ( ClashServer (..)
  , compileViaServer
  ) where

import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar)
import           Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import           Control.Exception
  (SomeAsyncException (..), SomeException, bracket_, fromException, throwIO, try)
import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.IORef (IORef, atomicModifyIORef', newIORef)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory (findExecutable, getTemporaryDirectory)
import           System.Exit (ExitCode (..))
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           System.Process
import           Test.Tasty.Options

-- | @--clash-server@: compile designs with a pool of @clash --server@ workers,
-- one per test suite thread, instead of one @clash@ process per test.
newtype ClashServer = ClashServer Bool
  deriving (Eq, Ord)

instance IsOption ClashServer where
  defaultValue = ClashServer False
  parseValue = fmap ClashServer . safeReadBool
  optionName = pure "clash-server"
  optionHelp = pure "Compile designs with a pool of `clash --server` workers"
  optionCLParser = flagCLParser Nothing (ClashServer True)

data Worker = Worker
  { workerProcess :: ProcessHandle
  , workerIn :: Handle
  , workerOut :: Handle
  , workerErrFile :: FilePath
  -- ^ The worker's own standard error, for when it dies
  , workerNextId :: IORef Int
  }

data Pool = Pool
  { poolSlots :: QSem
  -- ^ One slot per worker the pool may have
  , poolIdle :: MVar [Worker]
  }

-- | The pool is shared by all tests; there is no tasty resource with that
-- scope. Created on first use with the test suite's thread count.
poolVar :: MVar (Maybe Pool)
poolVar = unsafePerformIO (newMVar Nothing)
{-# NOINLINE poolVar #-}

getPool :: Int -> IO Pool
getPool workers = modifyMVar poolVar $ \case
  Just pool -> pure (Just pool, pool)
  Nothing -> do
    pool <- Pool <$> newQSem (max 1 workers) <*> newMVar []
    pure (Just pool, pool)

-- | Compile a design on an idle worker, spawning one if there is none, and
-- report what the @clash@ executable would have: exit code, stdout, stderr.
-- A worker that dies fails the request with its stderr and is not reused.
compileViaServer
  :: Int
  -- ^ Maximum number of workers (the test suite's thread count)
  -> FilePath
  -- ^ Working directory for the compilation
  -> [String]
  -- ^ The @clash@ command line, without RTS options
  -> IO (ExitCode, Text, Text)
compileViaServer workers workDir args = do
  pool <- getPool workers
  bracket_ (waitQSem (poolSlots pool)) (signalQSem (poolSlots pool)) $ do
    worker <- modifyMVar (poolIdle pool) $ \case
      w : ws -> pure (ws, w)
      [] -> do
        w <- spawnWorker
        pure ([], w)
    outcome <- try (request worker workDir args)
    case outcome of
      Right result -> do
        modifyMVar_ (poolIdle pool) (pure . (worker :))
        pure result
      Left (e :: SomeException) -> do
        -- The worker is in an unknown state (it may be mid-compile after a
        -- timeout, or gone); replace it. An asynchronous exception, such as
        -- the test suite's timeout, still propagates.
        terminateProcess (workerProcess worker)
        _ <- waitForProcess (workerProcess worker)
        case fromException e of
          Just (SomeAsyncException _) -> throwIO e
          Nothing -> do
            err <- T.readFile (workerErrFile worker)
            pure ( ExitFailure 1
                 , ""
                 , T.unlines
                     [ "clash server worker exited unexpectedly: " <> T.pack (show e)
                     , "Worker stderr:"
                     , err ])

spawnWorker :: IO Worker
spawnWorker = do
  clash <- maybe (throwIO (userError "clash-server: cannot find `clash` in the PATH")) pure
             =<< findExecutable "clash"
  tmp <- getTemporaryDirectory
  (errFile, errH) <- openTempFile tmp "clash-server-worker-stderr.txt"
  -- The heap limit the test suite gives every clash process, see 'commonArgs'
  -- in "Test.Tasty.Clash"; here it is per worker.
  (Just hin, Just hout, _, ph) <-
    createProcess (proc clash ["--server", "+RTS", "-M2G", "-RTS"])
      { std_in = CreatePipe, std_out = CreatePipe, std_err = UseHandle errH }
  hSetBinaryMode hin True
  hSetBinaryMode hout True
  worker <- Worker ph hin hout errFile <$> newIORef 1
  -- Startup output precedes the ready line; see Note [Clash compile server].
  waitReady worker
  pure worker
 where
  waitReady worker = do
    eof <- hIsEOF (workerOut worker)
    if eof
      then do
        err <- T.readFile (workerErrFile worker)
        throwIO (userError ("clash --server failed to start:\n" ++ T.unpack err))
      else do
        line <- BS.hGetLine (workerOut worker)
        case Aeson.eitherDecodeStrict line of
          Right (Aeson.Object o) | Just (Aeson.Bool True) <- lookupKey "ready" o -> pure ()
          _ -> waitReady worker
  lookupKey k o = Aeson.parseMaybe (.: k) o

request :: Worker -> FilePath -> [String] -> IO (ExitCode, Text, Text)
request worker workDir args = do
  reqId <- atomicModifyIORef' (workerNextId worker) (\n -> (n + 1, n))
  BL.hPutStr (workerIn worker)
    (Aeson.encode (Aeson.object ["id" .= reqId, "cwd" .= workDir, "args" .= args]))
  BL.hPutStr (workerIn worker) "\n"
  hFlush (workerIn worker)
  line <- BS.hGetLine (workerOut worker)
  case Aeson.eitherDecodeStrict line >>= Aeson.parseEither parseResponse of
    Left err -> throwIO (userError ("clash server: malformed response: " ++ err ++ ": " ++ BS.unpack line))
    Right (rspId, code, out, err)
      | rspId /= reqId -> throwIO (userError "clash server: response id does not match request")
      | otherwise -> pure (if code == 0 then ExitSuccess else ExitFailure code, out, err)
 where
  parseResponse = Aeson.withObject "response" $ \o ->
    (,,,) <$> o .: "id" <*> o .: "exitCode" <*> o .: "stdout" <*> o .: "stderr"
