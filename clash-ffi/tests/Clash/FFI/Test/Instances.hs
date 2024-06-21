{-|
  Copyright   :  (C) 2023, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Orphan instances for generating test data for the data types of
  Clash FFI to be utilized by 'Test.SmallCheck'.

  Additionally, a custom 'Show' class called 'TShow' is introduced for
  printing the values exchanged via the POSIX pipe on the Haskell
  side. 'TShow' defaults to 'Show', but can be adapted for individual
  types if necessary. By using 'TShow' instead of 'Show' these
  adaptions do not have an influence on the main library.

  Beside that, the module also defines some newtype wrappers that
  adapt the 'Show' and 'Series' behavior of some predefined types.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Clash.FFI.Test.Instances where

import Prelude hiding (init, length)

import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (malloc)
import Foreign.Ptr (nullPtr)
import GHC.Generics (Generic)
import GHC.TypeNats (SomeNat(..), someNatVal)
import Data.ByteString (ByteString, snoc, init, length, pack, unpack)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

import qualified Data.ByteString.Char8 as C (pack)

import Test.SmallCheck.Series
  (Serial(..), Series, NonZero(..), NonNegative(..), (\/), (<~>), cons1)

import Clash.Promoted.Nat (SNat(..), snatProxy, snatToNum)
import Clash.Sized.Internal.BitVector (Bit(..), low, high)
import Clash.Sized.Vector (toList)

import Clash.FFI.VPI.Callback (Callback, CallbackInfo(..))
import Clash.FFI.VPI.Callback.Reason (CallbackReason(..))
import Clash.FFI.VPI.Control (Control(..), StopValue(..), DiagnosticLevel(..))
import Clash.FFI.VPI.Error (ErrorInfo(..))
import Clash.FFI.VPI.Error.Level (ErrorLevel)
import Clash.FFI.VPI.Error.State (ErrorState)
import Clash.FFI.VPI.Info (Info(..))
import Clash.FFI.VPI.Iterator (Iterator)
import Clash.FFI.VPI.Module (Module)
import Clash.FFI.VPI.Net (Net)
import Clash.FFI.VPI.Object (Object(..))
import Clash.FFI.VPI.Object.Property (Property(..))
import Clash.FFI.VPI.Object.Time (Time(..), TimeType(..))
import Clash.FFI.VPI.Object.Type (ObjectType(..))
import Clash.FFI.VPI.Object.Value (Value(..))
import Clash.FFI.VPI.Object.Value.Delay (DelayMode(..))
import Clash.FFI.VPI.Object.Value.Format (ValueFormat(..))
import Clash.FFI.VPI.Object.Value.Scalar (bitToScalar)
import Clash.FFI.VPI.Object.Value.Vector (bitVectorToVector)
import Clash.FFI.VPI.Parameter (Parameter(..))
import Clash.FFI.VPI.Port (Port)
import Clash.FFI.VPI.Port.Direction (Direction)
import Clash.FFI.VPI.Reg (Reg)
import Clash.FFI.View (UnsafeSend, Send, CRepr)

-- | Custom 'Show' class for printing values on the Haskell side to be
-- compared with the respective prints on the C side.
class TShow a where
  tShow :: a -> String
  default tShow :: Show a => a -> String
  tShow = show

-- | 'String' newtype wrapper for avoiding reprints of already printed
-- values.
newtype S = S String
instance Show S where
  show (S str)
    | ' ' `elem` str = "(" <> str <> ")"
    | otherwise      = str

-- | 'tShow' wrapper that adds parenthesis to the print, if necessary.
pShow :: TShow a => a -> String
pShow = show . S . tShow

-- | Newtype wrapper for mapping 'TShow' to functors utilizing their
-- default 'show' behavior.
newtype TS a = TS a
instance (Functor f, TShow a, Show (f S)) => TShow (TS (f a)) where
  tShow (TS x) = show $ fmap (S . tShow) x

instance TShow ()
instance TShow Bool
instance TShow CInt

deriving via (TS [CInt]) instance TShow [CInt]
deriving via (TS [SerialBS]) instance TShow [SerialBS]

deriving via (TS (Maybe Object)) instance TShow (Maybe Object)
deriving via (TS (Maybe Module)) instance TShow (Maybe Module)
deriving via (TS (Maybe Port)) instance TShow (Maybe Port)
deriving via (TS (Maybe Net)) instance TShow (Maybe Net)
deriving via (TS (Maybe Reg)) instance TShow (Maybe Reg)
deriving via (TS (Maybe Parameter)) instance TShow (Maybe Parameter)

-- | 'ByteString' newtype wrapper, which ensures that the wrapped
-- elements are printed as 'Int' lists and that generated elements do
-- not contain NUL characters.
newtype SerialBS = SerialBS { serialBS :: ByteString }
  deriving stock   (Generic)
  deriving newtype (Show, UnsafeSend, Send)

type instance CRepr SerialBS = CString
instance TShow SerialBS where
  tShow = show . unpack . serialBS
instance Monad m => Serial m SerialBS where
  series = SerialBS . pack . filter (/= 0) <$> series

-- | 'ByteString' newtype wrapper, which ensures that the wrapped
-- elements are printed as 'Int' lists (cf. 'SerialBS') and have a
-- newline character at the end.
newtype BSNL = BSNL { bsnl :: ByteString }
  deriving newtype (Show)

type instance CRepr BSNL = CString
instance TShow BSNL where
  tShow = tShow . SerialBS . (`snoc` 10) . bsnl
instance Monad m => Serial m BSNL where
  series = BSNL . serialBS <$> series

-- | 'ByteString' newtype wrapper, whose generated elements are always
-- NUL terminated. Furthermore, 'BSNT' elements are printed as 'Int'
-- lists (cf. 'SerialBS').
newtype BSNT = BSNT { bsnt :: ByteString }
  deriving newtype (Show, UnsafeSend, Send)

type instance CRepr BSNT = CString
instance TShow BSNT where
  tShow = tShow . SerialBS . init . bsnt
instance Monad m => Serial m BSNT where
  series = BSNT . (`snoc` 0) . serialBS <$> series

-- Additional Clash.FFI.VPI.Callback.Callback Instances

instance TShow Callback

-- Additional Clash.FFI.VPI.Callback.CallbackInfo Instances

instance Show a => Show (CallbackInfo a) where
  show = const "CallbackInfo {..}"

instance TShow a => TShow (CallbackInfo a) where
  tShow CallbackInfo{..} =
    "CallbackInfo {"
      <> "cbReason = " <> tShow cbReason <> ", "
      <> "cbRoutine = <" <> show (unsafePerformIO $ cbRoutine nullPtr) <> ">, "
      <> "cbIndex = " <> show cbIndex <> ", "
      <> "cbData = " <> tShow cbData
      <> "}"

instance (Monad m, Serial m a) => Serial m (CallbackInfo a) where
  series =
    CallbackInfo
      <$> series
      -- we use the return value of the callback to check that the function
      -- passed to the VPI on the Haskell side is the same on the C side.
      <~> (series <&> const . return)
      <~> series
      <~> series

-- Additional Clash.FFI.VPI.Callback.Reason.CallbackReason Instances

instance TShow CallbackReason where
  tShow = \case
    AfterValueChange obj tty vf ->
      "AfterValueChange "
         <> pShow (coerce obj :: Object) <> " "
         <> pShow tty <> " "
         <> pShow vf
    BeforeStatement obj tty     ->
      "BeforeStatement "
         <> pShow (coerce obj :: Object) <> " "
         <> pShow tty
    AfterForce mObj tty vf      ->
      "AfterForce "
         <> pShow ((coerce <$> mObj) :: Maybe Object) <> " "
         <> pShow tty <> " "
         <> pShow vf
    AfterRelease mObj tty vf    ->
      "AfterRelease "
         <> pShow ((coerce <$> mObj) :: Maybe Object) <> " "
         <> pShow tty <> " "
         <> pShow vf
    AtStartOfSimTime mObj t     ->
      "AtStartOfSimTime "
         <> pShow ((coerce <$> mObj) :: Maybe Object) <> " "
         <> pShow t
    ReadWriteSynch mObj t       ->
      "ReadWriteSynch "
         <> pShow ((coerce <$> mObj) :: Maybe Object) <> " "
         <> pShow t
    ReadOnlySynch mObj t        ->
      "ReadOnlySynch "
         <> pShow ((coerce <$> mObj) :: Maybe Object) <> " "
         <> pShow t
    NextSimTime mObj tty        ->
      "NextSimTime "
         <> pShow ((coerce <$> mObj) :: Maybe Object) <> " "
         <> pShow tty
    AfterDelay mObj t           ->
      "AfterDelay "
         <> pShow ((coerce <$> mObj) :: Maybe Object) <> " "
         <> pShow t
    EndOfCompile                -> "EndOfCompile"
    StartOfSimulation           -> "StartOfSimulation"
    EndOfSimulation             -> "EndOfSimulation"
    RuntimeError                -> "RuntimeError"
    TchkViolation               -> "TchkViolation"
    StartOfSave                 -> "StartOfSave"
    EndOfSave                   -> "EndOfSave"
    StartOfRestart              -> "StartOfRestart"
    EndOfRestart                -> "EndOfRestart"
    StartOfReset                -> "StartOfReset"
    EndOfReset                  -> "EndOfReset"
    EnterInteractive            -> "EnterInteractive"
    ExitInteractive             -> "ExitInteractive"
    InteractiveScopeChange      -> "InteractiveScopeChange"
    UnresolvedSysTf             -> "UnresolvedSysTf"
#if defined(VERILOG_2001)
    AfterAssign obj tty vf      ->
      "AfterAssign "
         <> pShow ((coerce obj) :: Object) <> " "
         <> pShow tty <> " "
         <> pShow vf
    AfterDeassign obj tty vf    ->
      "AfterDeassign "
         <> pShow ((coerce obj) :: Object) <> " "
         <> pShow tty <> " "
         <> pShow vf
    AfterDisable obj tty vf     ->
      "AfterDisable "
         <> pShow ((coerce obj) :: Object) <> " "
         <> pShow tty <> " "
         <> pShow vf
    PliError                    -> "PliError"
    Signal                      -> "Signal"
#endif
#if defined(VERILOG_2005)
    NbaSynch mObj t             ->
      "NbaSynch "
         <> pShow ((coerce <$> mObj) :: Maybe Object) <> " "
         <> pShow t
    AtEndOfSimTime mObj t       ->
      "AtEndOfSimTime "
         <> pShow ((coerce <$> mObj) :: Maybe Object) <> " "
         <> pShow t
#endif

{- FOURMOLU_DISABLE -}

instance Monad m => Serial m CallbackReason where
  series =
       (AfterValueChange <$> obj <~> series <~> series)
    \/ (BeforeStatement <$> obj <~> series)
    \/ (AfterForce <$> mObj <~> series <~> series)
    \/ (AfterRelease <$> mObj <~> series <~> series)
    \/ (AtStartOfSimTime <$> mObj <~> series)
    \/ (ReadWriteSynch <$> mObj <~> series)
    \/ (ReadOnlySynch <$> mObj <~> series)
    \/ (NextSimTime <$> mObj <~> series)
    \/ (AfterDelay <$> mObj <~> series)
    \/ pure EndOfCompile
    \/ pure StartOfSimulation
    \/ pure EndOfSimulation
    \/ pure RuntimeError
    \/ pure TchkViolation
    \/ pure StartOfSave
    \/ pure EndOfSave
    \/ pure StartOfRestart
    \/ pure EndOfRestart
    \/ pure StartOfReset
    \/ pure EndOfReset
    \/ pure EnterInteractive
    \/ pure ExitInteractive
    \/ pure InteractiveScopeChange
    \/ pure UnresolvedSysTf
#if defined(VERILOG_2001)
    \/ (AfterAssign <$> obj <~> series <~> series)
    \/ (AfterDeassign <$> obj <~> series <~> series)
    \/ (AfterDisable <$> obj <~> series <~> series)
    \/ pure PliError
    \/ pure Signal
#endif
#if defined(VERILOG_2005)
    \/ (NbaSynch <$> mObj <~> series)
    \/ (AtEndOfSimTime <$> mObj <~> series)
#endif
    where
      obj :: Series m Object
      obj = series

      mObj :: Series m (Maybe Object)
      mObj = series

{- FOURMOLU_ENABLE -}

-- Additional Clash.FFI.VPI.Control.Control Instances

instance TShow Control
deriving instance Generic Control
instance Monad m => Serial m Control where
  series = cons1 Stop \/ cons1 Finish \/
    (Reset <$> series <~> (fmap getNonZero <$> series) <~> series)

-- Additional Clash.FFI.VPI.Control.StopValue Instances

instance TShow StopValue
deriving instance Generic StopValue
instance Monad m => Serial m StopValue

-- Additional Clash.FFI.VPI.Control.DiagnosticLevel Instances

instance TShow DiagnosticLevel
deriving instance Generic DiagnosticLevel
instance Monad m => Serial m DiagnosticLevel

-- Additional Clash.FFI.VPI.Error.ErrorInfo Instances

deriving instance Show ErrorInfo
instance TShow ErrorInfo where
  tShow ErrorInfo{..} =
    "ErrorInfo {"
      <> "errorState = " <> tShow errorState <> ", "
      <> "errorLevel = " <> tShow errorLevel <> ", "
      <> "errorMessage = " <> tShow (SerialBS errorMessage) <> ", "
      <> "errorProduct = " <> tShow (SerialBS errorProduct) <> ", "
      <> "errorCode = " <> tShow (SerialBS errorCode) <> ", "
      <> "errorFile = " <> tShow (SerialBS $ C.pack errorFile) <> ", "
      <> "errorLine = " <> show errorLine <> "}"

-- Additional Clash.FFI.VPI.Error.Level.ErrorLevel Instances

instance TShow ErrorLevel

-- Additional Clash.FFI.VPI.Error.Level.ErrorState Instances

instance TShow ErrorState

-- Additional Clash.FFI.VPI.Info.Info Instances

instance TShow Info where
  tShow Info{..} =
    "Info {"
      <> "infoArgs = " <> tShow (map SerialBS infoArgs) <> ", "
      <> "infoProduct = " <> tShow (SerialBS infoProduct) <> ", "
      <> "infoVersion = " <> tShow (SerialBS infoVersion) <> "}"

instance Monad m => Serial m Info where
  series = Info
    <$> (fmap serialBS <$> series)
    <~> (serialBS <$> series)
    <~> (serialBS <$> series)

-- Additional Clash.FFI.VPI.Iterator.Iterator Instances

instance TShow Iterator

-- Additional Clash.FFI.VPI.Module.Module Instances

instance TShow Module

-- Additional Clash.FFI.VPI.Net.Net Instances

instance TShow Net

-- Additional Clash.FFI.VPI.Object.Object Instances

instance TShow Object
instance Monad m => Serial m Object where
  series = pure $ Object $ unsafePerformIO malloc

-- Additional Clash.FFI.VPI.Object.Property.Property Instances

instance TShow (Property a)

instance Monad m => Serial m (Property CInt) where
  series =
    foldl (\/) (pure TypeOf) $ map pure
      [Size, LineNo, Direction, NetType, PortIndex]

instance Monad m => Serial m (Property CString) where
  series = foldl (\/) (pure Name) $ map pure [FullName, File]

{- FOURMOLU_DISABLE -}
instance Monad m => Serial m (Property Bool) where
  series =
    foldl (\/) (pure IsScalar) $ map pure
      [ IsVector
#if defined(VERILOG_2001)
      , IsSigned
      , IsLocalParam
#endif
      ]
{- FOURMOLU_ENABLE -}

-- Additional Clash.FFI.VPI.Object.Time.TimeType Instances

instance TShow TimeType
deriving instance Generic TimeType
instance Monad m => Serial m TimeType where
  series = pure ScaledReal \/ pure Sim

-- Additional Clash.FFI.VPI.Object.Time.Time Instances

instance TShow Time where
  tShow = \case
    SimTime t  -> "SimTime " <> show t
    RealTime t -> "RealTime " <> printf "%.10f" t

deriving instance Generic Time
instance Monad m => Serial m Time

-- Additional Clash.FFI.VPI.Object.Type.ObjectType Instances

instance TShow ObjectType
deriving instance Generic ObjectType
instance Monad m => Serial m ObjectType

-- Additional Clash.FFI.VPI.Object.Value.Value Instances

instance Eq Value where
  x1 == x2
    | BitVal (bitToScalar -> b1) <- x1
    , BitVal (bitToScalar -> b2) <- x2
    = b1 == b2

    | BitVectorVal n1@SNat (toList . bitVectorToVector -> v1) <- x1
    , BitVectorVal n2@SNat (toList . bitVectorToVector -> v2) <- x2
    , snatToNum n1 == (snatToNum n2 :: Integer)
    = v1 == v2

    | IntVal i1 <- x1
    , IntVal i2 <- x2
    = i1 == i2

    | RealVal r1 <- x1
    , RealVal r2 <- x2
    = r1 == r2

    | StringVal n1@SNat s1 <- x1
    , StringVal n2@SNat s2 <- x2
    , snatToNum n1 == (snatToNum n2 :: Integer)
    = s1 == s2

    | TimeVal t1 <- x1
    , TimeVal t2 <- x2
    = t1 == t2

    | otherwise
    = False

instance Monad m => Serial m Value where
  series =
       (BitVal <$> (pure high \/ pure low \/ pure (Bit 1 0)))
    \/ bitVectorVal
    \/ (IntVal . fromInteger <$> series)
    \/ (RealVal <$> series)
    \/ stringVal
    \/ (TimeVal <$> series)
    where
      bitVectorVal = do
        b <- series
        -- ensure that large vectors are generated as well
        n <- (if b then (+ 45) else id) . getNonNegative <$> series
        m <- series
        return $ case someNatVal (fromIntegral (n :: Integer)) of
          SomeNat proxy ->
            BitVectorVal (snatProxy proxy) (fromInteger m)
      stringVal = do
        bs <- serialBS <$> series
        return $ case someNatVal (fromIntegral (length bs)) of
          SomeNat proxy ->
            StringVal (snatProxy proxy) bs

instance TShow Value where
  tShow = \case
    BitVal bit -> "BitVal " <> show bit
    BitVectorVal SNat bv -> "BitVectorVal " <> show bv
    IntVal int -> "IntVal " <> show int
    RealVal real -> "RealVal " <> printf "%.10f" real
    StringVal _ str -> "StringVal " <> tShow (SerialBS str)
    TimeVal time -> "TimeVal " <> tShow time

-- Additional Clash.FFI.VPI.Object.Value.Delay.DelayMode Instances

deriving instance Show DelayMode
instance TShow DelayMode where
  tShow = \case
    NoDelay              -> "NoDelay"
    InertialDelay t      -> "InertialDelay " <> pShow t
    TransportDelay t     -> "TransportDelay " <> pShow t
    PureTransportDelay t -> "PureTransportDelay " <> pShow t
    Force                -> "Force"
    Release              -> "Release"

deriving instance Generic DelayMode
instance Monad m => Serial m DelayMode

-- Additional Clash.FFI.VPI.Object.Value.Format.ValueFormat Instances

instance TShow ValueFormat
deriving instance Generic ValueFormat
instance Monad m => Serial m ValueFormat

-- Additional Clash.FFI.VPI.Parameter.Parameter Instances

instance TShow Parameter

-- Additional Clash.FFI.VPI.Port.Port Instances

instance TShow Port

-- Additional Clash.FFI.VPI.Port.Direction Instances

instance TShow Direction

-- Additional Clash.FFI.VPI.Reg.Reg Instances

instance TShow Reg
