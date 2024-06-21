{- FOURMOLU_DISABLE -}
{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Object.Value.Format
  ( ValueFormat(..)
  , UnknownFormat(..)
  , InvalidFormat(..)
  ) where

import           Control.Exception (Exception, throwIO)
import           Foreign.C.Types (CInt)
import qualified Foreign.Ptr as FFI (castPtr)
import           Foreign.Storable (Storable(..))
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

import           Clash.FFI.View

-- | The format of the value to read / write over VPI. The actual type of the
-- value determines which formats are allowed. The main formats are
--
--   * @BinStrFmt@, @OctStrFmt@, @DecStrFmt@ and @HexStrFmt@, which return a
--     bit vector encoded in the specified base (including @\'x\'@ for
--     undefined and @\'z\'@ for high impedence).
--
--  * @ScalarFmt@, which returns a single bit value.
--
--  * @IntFmt@, which returns a 32-bit integer.
--
--  * @RealFmt@, which returns a 64-bit double-precision float.
--
--  * @StringFmt@, which returns a string.
--
--  * @VectorFmt@ (Verilog-2005 onwards), which returns a bit vector encoded
--    as an array of 2-bit values.
--
--  * @TimeFmt@, which returns a time value.
--
-- Additionally there are two special formats, which can only be used in
-- certain situations:
--
--  * @ObjTypeFmt@, which specifies the simulator is free to choose its
--    preferred format when reading a value. This cannot be used when writing.
--
--  * @SuppressValue@, which specifies no value will be read. This is used for
--    callbacks when the value is not needed in the callback routine.
--
data ValueFormat
  = BinStrFmt
  -- ^ Bit Vector as a binary string [0,1,x,z]
  | OctStrFmt
  -- ^ Bit Vector as an octal string [0-7,x,X,z,Z]
  | DecStrFmt
  -- ^ Bit vector as a decimal string [0-9,x,z]
  | HexStrFmt
  -- ^ Bit vector as a hexadecimal string [0-9,a-f,A-F,x,X,z,Z]
  | ScalarFmt
  -- ^ Bit or VPI scalar value
  | IntFmt
  -- ^ 32-bit signed integer
  | RealFmt
  -- ^ 64-bit double-precision float
  | StringFmt
  -- ^ Latin-1 encoded string
#if defined(VERILOG_2005) && defined(VPI_VECVAL)
  | VectorFmt
  -- ^ Bit vector as a bignum with A/B encoding
#endif
  | TimeFmt
  -- ^ VPI time value
  | ObjTypeFmt
  -- ^ Choose the best format based on the object type
  | SuppressValue
  -- ^ Do not return a value
  deriving stock (Eq, Show)

-- | An exception thrown when decoding a value format if an invalid value is
-- given for the C enum that specifies the constructor of 'ValueFormat'.
--
data UnknownFormat
  = UnknownFormat CInt CallStack
  deriving anyclass (Exception)

instance Show UnknownFormat where
  show = \case
    UnknownFormat f c -> mconcat
      [ "Unknown format constant "
      , show f
      , "\n"
      , prettyCallStack c
      ]

-- | An exception thrown when a 'ValueFormat' is used in an operation which
-- does not support that format, e.g. when attempting to write a value with
-- the format 'ObjTypeFmt'.
--
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
  11 -> pure TimeFmt
  12 -> pure ObjTypeFmt
  n -> throwIO $ UnknownFormat n callStack

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
  TimeFmt -> 11
  ObjTypeFmt -> 12
  SuppressValue -> 13

instance Storable ValueFormat where
  sizeOf _ = sizeOf (0 :: CInt)
  alignment _ = alignment (0 :: CInt)

  peek ptr =
    peek (FFI.castPtr @_ @CInt ptr) >>= cintToFormat

  poke ptr =
    let cintPtr = FFI.castPtr @_ @CInt ptr
     in poke cintPtr . formatToCInt

type instance CRepr ValueFormat = CInt

instance Send ValueFormat where
  send = pure . formatToCInt

instance Receive ValueFormat where
  receive = cintToFormat
