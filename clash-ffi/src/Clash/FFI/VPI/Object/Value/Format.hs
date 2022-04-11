{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Object.Value.Format
  ( ValueFormat(..)
  , UnknownFormat(..)
  , InvalidFormat(..)
  , InvalidSize(..)
  , ZeroWidthValue(..)
  ) where

import           Control.Exception (Exception)
import qualified Control.Exception as IO (throwIO)
import qualified Control.Monad.IO.Class as IO (liftIO)
import           Foreign.C.Types (CInt)
import qualified Foreign.Ptr as FFI (castPtr)
import           Foreign.Storable (Storable(..))
import           GHC.Natural (Natural)
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

import           Clash.FFI.View

data ValueFormat
  = BinStrFmt
  | OctStrFmt
  | DecStrFmt
  | HexStrFmt
  | ScalarFmt
  | IntFmt
  | RealFmt
  | StringFmt
#if defined(VERILOG_2005) && defined(VPI_VECVAL)
  | VectorFmt
#endif
  | StrengthFmt
  | TimeFmt
  | ObjTypeFmt
  | SuppressFmt
  deriving stock (Eq, Show)

data UnknownFormat
  = UnknownFormat CInt CallStack
  deriving anyclass (Exception)

instance Show UnknownFormat where
  show (UnknownFormat f c) =
    mconcat
      [ "Unknown format constant "
      , show f
      , "\n"
      , prettyCallStack c
      ]

data InvalidFormat
  = InvalidFormat ValueFormat CallStack
  deriving anyclass (Exception)

instance Show InvalidFormat where
  show (InvalidFormat f c) =
    mconcat
      [ "The value format "
      , show f
      , " can not be used in all calls.\n"
      , "Please consult the (System)Verilog specification for details.\n"
      , prettyCallStack c
      ]

data InvalidSize
  = InvalidSize Natural Natural CallStack
  deriving anyclass (Exception)

instance Show InvalidSize where
  show (InvalidSize e a c) =
    mconcat
      [ "Invalid size: expected "
      , show e
      , " but size was "
      , show a
      , "\n"
      , prettyCallStack c
      ]

data ZeroWidthValue
  = ZeroWidthValue CallStack
  deriving anyclass (Exception)

instance Show ZeroWidthValue where
  show (ZeroWidthValue c) =
    mconcat
      [ "Attempt to send / receive a zero-width value\n"
      , prettyCallStack c
      ]

cintToFormat :: HasCallStack => CInt -> IO ValueFormat
cintToFormat = \case
  1 -> pure BinStrFmt
  2 -> pure OctStrFmt
  3 -> pure DecStrFmt
  4 -> pure HexStrFmt
  5 -> pure ScalarFmt
  6 -> pure IntFmt
  7 -> pure RealFmt
  8 -> pure StringFmt
#if defined(VERILOG_2005) && defined(VPI_VECVAL)
  9 -> pure VectorFmt
#endif
  10 -> pure StrengthFmt
  11 -> pure TimeFmt
  12 -> pure ObjTypeFmt
  n -> IO.throwIO (UnknownFormat n callStack)

formatToCInt :: ValueFormat -> CInt
formatToCInt = \case
  BinStrFmt -> 1
  OctStrFmt -> 2
  DecStrFmt -> 3
  HexStrFmt -> 4
  ScalarFmt -> 5
  IntFmt -> 6
  RealFmt -> 7
  StringFmt -> 8
#if defined(VERILOG_2005) && defined(VPI_VECVAL)
  VectorFmt -> 9
#endif
  StrengthFmt -> 10
  TimeFmt -> 11
  ObjTypeFmt -> 12
  SuppressFmt -> 13

instance Storable ValueFormat where
  sizeOf _ = sizeOf (0 :: CInt)
  alignment _ = alignment (0 :: CInt)

  peek ptr =
    peek (FFI.castPtr @_ @CInt ptr) >>= cintToFormat

  poke ptr =
    let cintPtr = FFI.castPtr @_ @CInt ptr
     in poke cintPtr . formatToCInt

instance UnsafeSend ValueFormat where
  type Sent ValueFormat = CInt

  unsafeSend =
    pure . formatToCInt

instance Send ValueFormat where
  send = unsafeSend

instance UnsafeReceive ValueFormat where
  type Received ValueFormat = CInt

  unsafeReceive =
    IO.liftIO . cintToFormat

instance Receive ValueFormat where
  receive = unsafeReceive

