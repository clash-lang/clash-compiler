{-|
  Copyright   :  (C) 2023, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  A test suite that checks for the correct FFI data transfer of Clash
  FFI.

  The tests work via utilizing an FFI-independent POSIX pipe to
  compare the exchanged data in printed format. On the Haskell side,
  the default 'Show' instances are used to print the values that are
  transferred whenever possible. As this is not always an option, the
  wrapper class 'TShow' is utilized to adapt the 'Show' behavior when
  necessary. On the C side the corresponding data structures are
  printed manually such that they match the Haskell 'TShow' behavior.

  The suite tests for FFI calls of Clash FFI that the prints of the
  arguments and return values match at the Haskell and at the C
  side. To this end, the values are printed one after another to the
  pipe, always separated by a newline. For generating the test data,
  the SmallCheck library is used, whenever possible. The remaining
  cases are covered by HUnit tests. The test suite is not thread safe,
  as only a single pipe is used to exchange the data.

  Also note: this test suite only checks that the data (described via
  Haskell data values) is correctly transferred to the C level
  structures and vice versa. Accordingly, these tests are independent
  of any actual VPI simulator implementation. When combined with a
  simulator, it still has to be taken care that the interaction with a
  simulator is setup correctly. These tests only ensure that data does
  not get corrupted when exchanged via the VPI.
-}

{-# LANGUAGE MonoLocalBinds #-}

module Main where

import Prelude hiding (iterate)

import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack)
import Data.Proxy (Proxy(..))
import Foreign.C.Types (CInt(..))
import Foreign.C.String (CString)
import System.Environment (setEnv)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)
import Test.Tasty.SmallCheck (testProperty)
import Test.SmallCheck.Series (Positive)

import Clash.FFI.Monad
import Clash.FFI.VPI.Callback
import Clash.FFI.VPI.Control
import Clash.FFI.VPI.Error
import Clash.FFI.VPI.IO
import Clash.FFI.VPI.Info
import Clash.FFI.VPI.Iterator
import Clash.FFI.VPI.Module
import Clash.FFI.VPI.Net
import Clash.FFI.VPI.Object
import Clash.FFI.VPI.Parameter
import Clash.FFI.VPI.Port
import Clash.FFI.VPI.Reg

import Clash.FFI.Test
import Clash.FFI.Test.Instances

-- | Main entry for 'ffi-interface-tests'.
main :: IO ()
main = withPipe $ do
  -- using only a single joint pipe for all tests is not thread-safe,
  -- which is why w restrict to single-threaded execution for now.
  setEnv "TASTY_NUM_THREADS" "1"

  defaultMain $ testGroup "Clash-FFI"
    [ testGroup "Clash.FFI.VPI.Callback"
        [ testProperty "registerCallback (vpi_register_cb)"
            $ testM $ sendReceiveAndCompare @(CallbackInfo BSNT) registerCallback
        , testProperty "removeCallback (vpi_remove_cb)"
            $ testM $ \cbInfo -> runSimAction $ do
                cb <- registerCallback (cbInfo :: CallbackInfo BSNT) %% 2
                removeCallback cb
                inputEQ cb
        ]
    , testGroup "Clash.FFI.VPI.Control"
        [ testProperty "controlSimulator (vpi_control)"
            $ testM $ sendAndCompare controlSimulator
        ]
    , testGroup "Clash.FFI.VPI.Error"
        [ testProperty "receiveErrorLevel (vpi_chk_error)"
            $ testM $ receiveAndCompare receiveErrorLevel
        , testProperty "unsafeReceiveErrorInfo (vpi_chk_error)"
            $ testM $ receiveAndCompare unsafeReceiveErrorInfo
        , testProperty "receiveErrorInfo (vpi_chk_error)"
            $ testM $ receiveAndCompare receiveErrorInfo
        ]
    , testGroup "Clash.FFI.VPI.IO"
        [ testProperty "simPutStr (vpi_printf)"
            $ testM $ sendAndCompare $ simPutStr . serialBS
        , testProperty "simPutStrLn (vpi_printf)"
            $ testM $ sendAndCompare $ simPutStrLn . bsnl
        , testProperty "simFlushIO (vpi_flush)"
            $ testM $ receiveAndCompare simFlushIO
        ]
    , testGroup "Clash.FFI.VPI.Info"
        [ testProperty "receiveSimulatorInfo (vpi_get_vlog_info)"
            $ testM $ receiveAndCompare receiveSimulatorInfo
        , testProperty "unsafeReceiveSimulatorInfo (vpi_get_vlog_info)"
            $ testM $ receiveAndCompare unsafeReceiveSimulatorInfo
        ]
    , testGroup "Clash.FFI.VPI.Iterator"
        [ testProperty "iterate (vpi_iterate)"
            $ testM $ sendReceiveAndCompare2 @_ @(Maybe Object) iterate
        , testProperty "scan (vpi_scan)"
            $ testM $ \x -> do
                iterator <- runSimAction (iterate ObjModule (Nothing @Object) %% 3)
                receiveAndCompare @(Maybe Module) (scan iterator) x
        , testProperty "iterateAll (vpi_iterate, vpi_scan)"
            $ testM $ \(input, mObj) -> case objectType input of
                SomeObjectType (Proxy :: Proxy t) -> do
                  xs <- runSimAction $ iterateAll input mObj
                  -- check vpi_iterate output
                  cInput <- sequence
                    [ inputEQ input
                    , inputEQ (mObj :: Maybe Object)
                    ]
                  ignoreOutputs 1 -- internal iterator
                  -- check vpi_scan_output
                  cOutputs <- mapM (outputEQ . TS . Just) (xs :: [t])
                  cEnd <- outputEQ (Nothing @Object)
                  return (cInput <> cOutputs <> [cEnd])
        ]
    , testGroup "Clash.FFI.VPI.Module"
        [ testCase "topModules (vpi_iterate, vpi_scan)" $ do
            modules <- runSimAction topModules %% 3
            mapM_ (assert . outputEQ . Just) modules
            assert $ outputEQ (Nothing @Module)
        , testCase "findTopModule (vpi_handle_by_name)" $ do
            let known   = pack "top"
                unknown = pack "unknown"
                empty   = pack ""
            top <- runSimAction $ findTopModule known
            mapM_ assert
              [ inputEQ (SerialBS known)
              , inputEQ (Nothing @Module)
              , outputEQ top
              ]
            catch
              (  runSimAction (findTopModule unknown)
              >> assertFailure "expected Exception"
              ) $ \(_ :: SomeException) -> return ()
            mapM_ assert
              [ inputEQ (SerialBS unknown)
              , inputEQ (Nothing @Module)
              ]
            catch
              (  runSimAction (findTopModule empty)
              >> assertFailure "expected Exception"
              ) $ \(_ :: SomeException) -> return ()
            mapM_ assert
              [ inputEQ (SerialBS empty)
              , inputEQ (Nothing @Module)
              ]
        , testCase "moduleNets (vpi_iterate, vpi_scan)" $
            runSimAction ((topModule >>= moduleNets) %% 6)
              >>= mapM_ (assert . outputEQ . Just)
              >>  assert (outputEQ (Nothing @Net))
        , testCase "moduleParameters (vpi_iterate, vpi_scan)" $
            runSimAction ((topModule >>= moduleParameters) %% 6)
              >>= mapM_ (assert . outputEQ . Just)
              >> assert (outputEQ (Nothing @Parameter))
        , testCase "modulePorts (vpi_iterate, vpi_scan)" $
            runSimAction ((topModule >>= modulePorts) %% 6)
              >>= mapM_ (assert . outputEQ . Just)
              >>  assert (outputEQ (Nothing @Port))
        , testCase "moduleRegs (vpi_iterate, vpi_scan)" $
            runSimAction ((topModule >>= moduleRegs) %% 6)
              >>= mapM_ (assert . outputEQ . Just)
              >>  assert (outputEQ (Nothing @Reg))
        ]
    , testGroup "Clash.FFI.VPI.Object"
        [ testProperty "freeObject (vpi_free_object)"
            $ testM $ sendAndCompare @Object freeObject
        , testProperty "compareObjects (vpi_compare_objects)"
            $ testM $ sendReceiveAndCompare2 @Object @Object compareObjects
        , testCase "getChild (vpi_handle)" $ do
            let none = Nothing @Object
                objTypeTop = ObjModule
                objTypePort = ObjPort
            runSimAction $ do
              top <- getChild objTypeTop none
              mapM_ assert
                [ inputEQ objTypeTop
                , inputEQ none
                , outputEQ (top :: Module)
                ]
              let topRef = Just top
              port <- getChild objTypePort topRef
              mapM_ assert
                [ inputEQ objTypePort
                , inputEQ topRef
                , outputEQ (port :: Port)
                ]
            catch
              ( (runSimAction (getChild objTypePort none) :: IO Port)
                >> assertFailure "expected Exception"
              ) $ \(_ :: SomeException) -> return ()
            mapM_ assert
              [ inputEQ objTypePort
              , inputEQ none
              ]
        , testCase "sendChild (vpi_handle_by_name)" $ do
            let none = Nothing @Object
                topName = pack "top"
                portName = pack "port"
            runSimAction $ do
              top <- sendChildRef topName none
              mapM_ assert
                [ inputEQ (SerialBS topName)
                , inputEQ none
                , outputEQ (top :: Module)
                ]
              let topRef = Just top
              port <- sendChildRef portName topRef
              mapM_ assert
                [ inputEQ (SerialBS portName)
                , inputEQ topRef
                , outputEQ (port :: Port)
                ]
            catch
              ( (runSimAction (sendChildRef portName none) :: IO Port)
                >> assertFailure "expected Exception"
              ) $ \(_ :: SomeException) -> return ()
            mapM_ assert
              [ inputEQ (SerialBS portName)
              , inputEQ none
              ]
        , testCase "unsafeSendChild (vpi_handle_by_name)" $ do
            let none = Nothing @Object
                topName = pack "top"
                portName = pack "port"
            runSimAction $ do
              top <- unsafeSendChildRef topName none
              mapM_ assert
                [ inputEQ (SerialBS topName)
                , inputEQ none
                , outputEQ (top :: Module)
                ]
              let topRef = Just top
              port <- unsafeSendChildRef portName topRef
              mapM_ assert
                [ inputEQ (SerialBS portName)
                , inputEQ topRef
                , outputEQ (port :: Port)
                ]
            catch
              ( (runSimAction (unsafeSendChildRef portName none) :: IO Port)
                >> assertFailure "expected Exception"
              ) $ \(_ :: SomeException) -> return ()
            mapM_ assert
              [ inputEQ (SerialBS portName)
              , inputEQ none
              ]
        , testCase "getChild (vpi_handle_by_index)" $ do
            let none = Nothing @Object
                netNameRef = pack "top.net"
                existingNetBitIdx = (0 :: CInt)
                missingNetBitIdx = (20 :: CInt)
            net <- Just <$> runSimAction (sendChildRef netNameRef none %% 3)
            obj <- runSimAction $ getChild existingNetBitIdx (net :: Maybe Net)
            mapM_ assert
                [ inputEQ existingNetBitIdx
                , inputEQ net
                , outputEQ (obj :: Object)
                ]
            catch
              ( (runSimAction (getChild missingNetBitIdx net) :: IO Port)
                >> assertFailure "expected Exception"
              ) $ \(_ :: SomeException) -> return ()
            mapM_ assert
              [ inputEQ missingNetBitIdx
              , inputEQ net
              ]
        , testCase "getChild (vpi_handle_by_multi_index)" $ do
            let none = Nothing @Object
                netNameRef = pack "top.reg"
                existingRegBit = [0 :: CInt, 1 :: CInt]
                missingRegBit = [1 :: CInt, 2 :: CInt]
            reg <- Just <$> runSimAction (sendChildRef netNameRef none %% 3)
            bit <- runSimAction $ getChild existingRegBit (reg :: Maybe Reg)
            mapM_ assert
                [ inputEQ existingRegBit
                , inputEQ reg
                , outputEQ (bit :: Object)
                ]
            catch
              ( (runSimAction (getChild missingRegBit reg) :: IO Port)
                >> assertFailure "expected Exception"
              ) $ \(_ :: SomeException) -> return ()
            mapM_ assert
              [ inputEQ missingRegBit
              , inputEQ reg
              ]
        , testProperty "getProperty (vpi_get) [CInt]"
            $ testM $ \(prop :: Property CInt) ->
                specialTop >>= sendReceiveAndCompare2 getProperty . (prop, )
        , testProperty "getProperty (vpi_get) [Bool]"
            $ testM $ \(prop :: Property Bool) ->
                specialTop >>= sendReceiveAndCompare2 getProperty . (prop, )
        , testProperty "receiveProperty (vpi_get_str) [CString]"
            $ testM $ \(prop :: Property CString) ->
                specialTop >>= sendReceiveAndCompare2
                  (((SerialBS <$>) .) . receiveProperty) . (prop, )
        , testProperty "receiveTime (vpi_get_time)"
            $ testM $ sendReceiveAndCompare2 @_ @(Maybe Object) receiveTime
        , testProperty "receiveValue (vpi_get_value)"
            $ testM
                ( (%% 3) -- internal call of 'getProperty Size'
                .  sendReceiveAndCompare2 @_ @Object receiveValue
                )
              . filter ((`notElem` [SuppressValue, ObjTypeFmt]) . fst)
        , testProperty "unsafeReceiveValue (vpi_get_value)"
            $ testM
                ( (%% 3) -- internal call of 'getProperty Size'
                .  sendReceiveAndCompare2 @_ @Object unsafeReceiveValue
                )
              . filter ((`notElem` [SuppressValue, ObjTypeFmt]) . fst)
        , testProperty "sendValue (vpi_put_value)"
            $ testM $ \(value, delayMode) -> runSimAction $ do
                let none = Nothing @Object
                    portNameRef = pack "top.port"
                port <- sendChildRef portNameRef none %% 3
                liftIO $ enforceSize $ valueSize value
                sendValue port value delayMode
                sequence
                  [ inputEQ (port :: Port)
                  , inputEQ value
                  , inputEQ delayMode
                  ]
        , testProperty "unsafeSendValue (vpi_put_value)"
            $ testM $ \(value, delayMode) -> runSimAction $ do
                let none = Nothing @Object
                    portNameRef = pack "top.port"
                port <- sendChildRef portNameRef none %% 3
                liftIO $ enforceSize $ valueSize value
                unsafeSendValue port value delayMode
                sequence
                  [ inputEQ (port :: Port)
                  , inputEQ value
                  , inputEQ delayMode
                  ]
        , testProperty "compare send & receive"
            $ testM $ \(value, delayMode) -> runSimAction $ do
                let none = Nothing @Object
                    portNameRef = pack "top.port"
                port <- sendChildRef portNameRef none %% 3
                liftIO $ enforceSize $ valueSize value
                sendValue (port :: Port) value delayMode %% 3
                value' <- receiveValue (valueFormat value) port %% 6
                return $ TestResult $
                  if value /= value'
                  then Left $ "the values differ"
                    <> "\n             sent: \"" <> tShow value <> "\""
                    <> "\n     but received: \"" <> tShow value' <> "\""
                  else Right ()
        ]
    , testGroup "Clash.FFI.VPI.Port"
        [ testProperty "direction (vpi_get)"
            $ testM $ \(_ :: [Positive Int]) ->
                runSimAction $ do
                  let none = Nothing @Object
                      portNameRef = pack "top.port"
                  port <- sendChildRef portNameRef none %% 3
                  value <- direction port
                  sequence
                    [ inputEQ Direction
                    , inputEQ port
                    , outputEQ value
                    ]
        ]
    ]

foreign export ccall "clash_ffi_main"
  ffiMain :: IO ()

-- | Must be declared for Clash FFI.
ffiMain :: IO ()
ffiMain = return ()
