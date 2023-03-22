{-|
  Copyright   :  (C) 2023, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Utility functions for declaring the test cases.
-}
module Clash.FFI.Test where

import Control.DeepSeq (NFData)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Char8 (pack)
import Data.Coerce (Coercible)
import Data.Proxy (Proxy(..))
import Foreign.C.Types (CInt(..))
import GHC.IO.Handle.FD (fdToHandle)
import System.IO (Handle, hGetLine)

import Test.Tasty.HUnit (assertFailure)
import Test.Tasty.SmallCheck (Reason, Testable, Property, test, monadic)
import Test.SmallCheck.Series (Positive)

import Clash.Prelude (BitSize, SNat(..), Bit, Signed, snatToNum)

import Clash.FFI.Monad (SimAction, SimCont, runSimAction)
import Clash.FFI.VPI.Module (Module(..), findTopModule)
import Clash.FFI.VPI.Net (Net(..))
import Clash.FFI.VPI.Object ( IsObject, Object(..), Value(..)
                            , ObjectType(..), ValueFormat(..)
                            )
import Clash.FFI.VPI.Parameter (Parameter(..))
import Clash.FFI.VPI.Port (Port(..))
import Clash.FFI.VPI.Reg (Reg(..))

import Clash.FFI.Test.Instances (TShow(..))

-- | Creates a POSIX pipe that is used for exchanging data between
-- Haskell and C. The interface serves as an alternative to the
-- FFI. The pipe is created at the C level. The function returns the
-- file descriptor of the created pipe, which can be turned into a
-- Haskell 'Handle' afterwards.
foreign import ccall "vpi_test.h init_pipe"
  initPipe :: IO CInt

-- | Closes the pipe created with 'initPipe'.
foreign import ccall "vpi_test.h close_pipe"
  closePipe :: IO ()

-- | Safely runs an action that has access to the pipe.
withPipe :: ((?pipe :: Handle) => IO ()) -> IO ()
withPipe action = do
  pipe <- initPipe >>= fdToHandle
  let ?pipe = pipe
  action
  closePipe

-- | Passes some bit size value to the C interface. This side channel
-- is used to dynamically adjust value sizes to match the values that
-- are generated at the Haskell side. This way we avoid the necessity
-- to create and manage objects that match the generated
-- sizes. Instead, we can generate the objects independently of the
-- generated values and adapt their sizes such that they match.
foreign import ccall "vpi_test.h enforce_size"
  enforceSize :: CInt ->  IO ()

-- | Reifies the bit size from the given value.
valueSize :: Value -> CInt
valueSize = \case
  BitVal (_ :: Bit)       -> snatToNum (SNat :: SNat (BitSize Bit))
  IntVal (_ :: Signed 32) -> snatToNum (SNat :: SNat (BitSize (Signed 32)))
  RealVal (_ :: Double)   -> snatToNum (SNat :: SNat (BitSize Double))
  BitVectorVal n@SNat _   -> snatToNum n
  StringVal n@SNat _      -> snatToNum n
  TimeVal{}               -> -1

-- | Gives the 'ValueFormat' of the given 'Value'.
valueFormat :: Value -> ValueFormat
valueFormat = \case
  BitVal{}       -> ScalarFmt
  BitVectorVal{} -> VectorFmt
  IntVal{}       -> IntFmt
  RealVal{}      -> RealFmt
  StringVal{}    -> StringFmt
  TimeVal{}      -> TimeFmt

-- | Existential data type wrapper for object types (required for
-- realizing 'objectType').
data SomeObjectType =
  forall a. (TShow a, IsObject a, Coercible Object a, NFData a) =>
    SomeObjectType (Proxy a)

-- | Returns a type proxy of an 'IsObject' instance, which matches the
-- given 'ObjectType'.
objectType :: ObjectType -> SomeObjectType
objectType = \case
  ObjModule    -> SomeObjectType (Proxy @Module)
  ObjNet       -> SomeObjectType (Proxy @Net)
  ObjPort      -> SomeObjectType (Proxy @Port)
  ObjParameter -> SomeObjectType (Proxy @Parameter)
  ObjReg       -> SomeObjectType (Proxy @Reg)
  _            -> SomeObjectType (Proxy @Object)

-- | Returns the top module named @top@.
topModule :: SimCont o Module
topModule = findTopModule $ pack "top"

-- | Returns the special top module named @special@. Calls of
--  @vpi_get@ ('Clash.FFI.VPI.Object.getProperty' /
--  'Clash.FFI.VPI.Object.receiveProperty') produce different output
--  for the return value on this module. If passing @special@, then
--  the values are printed as plain @PLI_INT32@ values instead of
--  their corresponding Haskell representation.
specialTop :: (?pipe :: Handle) => IO Module
specialTop =
  runSimAction (findTopModule (pack "special") %% 3)

-- | Runs tests over lists in the 'monadic' context.
testM :: Testable m [b] => (a -> m b) -> [a] -> Property m
testM xs = monadic . mapM xs

-- | Runs some Clash FFI action and compares the returned result with
-- the output printed at the C side.
receiveAndCompare ::
  (TShow a, ?pipe :: Handle) =>
  SimAction a ->
  Positive Int ->
  IO TestResult
receiveAndCompare action =
  const $ runSimAction action >>= outputEQ

-- | Sends some value to the C side via a Clash FFI action and
-- compares the output printed at the C side with the sent value.
sendAndCompare ::
  (TShow a, ?pipe :: Handle) =>
  (a -> SimAction ()) ->
  a ->
  IO TestResult
sendAndCompare action input = do
  runSimAction $ action input
  inputEQ input

-- | Sends some value to the C side via a Clash FFI action and
-- compares the output printed at the C side with the sent value. The
-- same is done for the value returned by the action.
sendReceiveAndCompare ::
  (TShow a, TShow b, ?pipe :: Handle) =>
  (a -> SimAction b) ->
  a ->
  IO [TestResult]
sendReceiveAndCompare action input = do
  output <- runSimAction $ action input
  sequence
    [ inputEQ input
    , outputEQ output
    ]

-- | Sends two values to the C side via a Clash FFI action and
-- compares the output printed at the C side with the sent value. The
-- same is done for the value returned by the action.
sendReceiveAndCompare2 ::
  (TShow a, TShow b, TShow c, ?pipe :: Handle) =>
  (a -> b -> SimAction c) ->
  (a, b) ->
  IO [TestResult]
sendReceiveAndCompare2 action (i1, i2) = do
  output <- runSimAction $ action i1 i2
  sequence
    [ inputEQ i1
    , inputEQ i2
    , outputEQ output
    ]

-- | Newtype wrapper introducing some reduced smallcheck interface for
-- declaring the corresponding tests.
newtype TestResult =
  TestResult { testResult :: Either Reason () }

instance Monad m => Testable m TestResult where
  test (TestResult x) = case x of
    Left err -> test (Left err :: Either Reason Reason)
    Right () -> test True

instance Monad m => Testable m [TestResult] where
  test = test . TestResult . mapM_ testResult

instance Monad m => Testable m [[TestResult]] where
  test = test . concat

-- | Modified variant of 'Test.Tasty.HUnit.assert' checking properties
-- in any 'MonadIO' context.
--
-- Note: 'Test.Tasty.HUnit.assert' is declared deprecated. Since we
-- do need the original behavior here anyway, its fine to hijack the
-- name.
assert :: MonadIO m => m TestResult -> m ()
assert x = (testResult <$> x) >>= \case
  Left err -> liftIO $ assertFailure err
  Right () -> return ()

-- | Discards the given number of lines on the communication pipe
-- between Haskell and C in case they are not important for a test.
-- Also see the '(%%)' operator for some minimal notation overhead
-- variant of this.
ignoreOutputs :: (MonadIO m, ?pipe :: Handle) => Int -> m ()
ignoreOutputs n = replicateM_ n $ liftIO $ hGetLine ?pipe

infix 1 %%

-- | Ignores lines on the communication pipe between Haskell and C
-- like 'ignoreOutputs'. The interface of the operator is designed
-- such that it can be attached to any 'MonadIO' action producing the
-- lines to be ignored.
(%%) :: (Monad m, MonadIO m, ?pipe :: Handle) => m a -> Int -> m a
(%%) action n = do
  r <- action
  ignoreOutputs n
  return r

-- | Checks that the output returned by the tested Clash FFI action
-- matches the value printed at the C side to the communication pipe.
outputEQ :: (MonadIO m, TShow a, ?pipe :: Handle) => a -> m TestResult
outputEQ output = expectEQ <$> liftIO (hGetLine ?pipe) <*> pure (tShow output)

-- | Checks that the input to the tested Clash FFI action matches the
-- value printed at the C side to the communication pipe.
inputEQ :: (MonadIO m, TShow a, ?pipe :: Handle) => a -> m TestResult
inputEQ input = expectEQ (tShow input) <$> liftIO (hGetLine ?pipe)

-- | Checks that the print of some expected value matches some
-- received value. If the values do not match, then an error message
-- indicating the difference gets returned.
expectEQ :: String -> String -> TestResult
expectEQ expected got =
  TestResult $
    if expected /= got
    then Left $ "the printed values differ"
      <> "\n     expected: \"" <> expected <> "\""
      <> "\n      but got: \"" <> got <> "\""
    else Right ()
