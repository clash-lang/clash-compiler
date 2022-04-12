{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Clash.FFI.View
  ( -- * Views on Data for FFI
    UnsafeSend(..)
  , UnsafeReceive(..)
  , Send(..)
  , Receive(..)
    -- * Pointers
  , unsafePokeSend
  , pokeSend
  , unsafePeekReceive
  , peekReceive
    -- * Arrays
  , unsafeSendArray
  , sendArray
  , unsafeReceiveArray0
  , receiveArray0
  , unsafeReceiveArrayN
  , receiveArrayN
    -- * Strings
  , unsafeSendString
  , sendString
  , receiveString
  ) where

import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS (length, packCString)
import qualified Data.ByteString.Unsafe as BS
import           Data.Typeable (Typeable)
import           Foreign.C.String (CString)
import qualified Foreign.C.String as FFI
import qualified Foreign.Marshal.Alloc as FFI (mallocBytes)
import qualified Foreign.Marshal.Array as FFI
import qualified Foreign.Marshal.Utils as FFI (copyBytes)
import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)
import           Foreign.Storable (Storable)
import qualified Foreign.Storable as FFI (peek, poke)
import           GHC.Stack (HasCallStack)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim

-- | A class for data with raw values which can be unsafely sent over the FFI.
-- Any modification to this value will silently modify the original value,
-- potentially corrupting the Haskell RTS state.
class UnsafeSend a where
  type Sent a

  unsafeSend :: HasCallStack => a -> SimCont b (Sent a)

-- | A class for data with raw values which can be safely sent over the FFI.
-- Safely sending data involves making new copies where necessary, so the
-- original value is not corrupted if the FFI call is impure. Memory allocated
-- by this class must be manually deallocated.
class UnsafeSend a => Send a where
  send :: HasCallStack => a -> SimCont b (Sent a)

instance Storable a => UnsafeSend [a] where
  type Sent [a] = Ptr a

  unsafeSend xs =
    Sim.liftCont (FFI.withArray xs)

instance Storable a => Send [a] where
  send =
    IO.liftIO . FFI.newArray

instance (UnsafeSend a, Storable (Sent a)) => UnsafeSend (Maybe a) where
  type Sent (Maybe a) = Ptr (Sent a)

  unsafeSend =
    maybe (pure FFI.nullPtr) unsafePokeSend

instance (Send a, Storable (Sent a)) => Send (Maybe a) where
  send =
    maybe (pure FFI.nullPtr) pokeSend

unsafePokeSend
  :: (UnsafeSend a, Storable (Sent a))
  => a
  -> SimCont b (Ptr (Sent a))
unsafePokeSend x = do
  raw <- unsafeSend x
  fst <$> Sim.withNewPtr Sim.stackPtr (`FFI.poke` raw)

pokeSend
  :: (Send a, Storable (Sent a))
  => a
  -> SimCont b (Ptr (Sent a))
pokeSend x = do
  raw <- send x
  fst <$> Sim.withNewPtr Sim.heapPtr (`FFI.poke` raw)

-- | Send an array by unsafely sending both the elements and the array itself.
unsafeSendArray
  :: (UnsafeSend a, Storable (Sent a))
  => [a]
  -> SimCont b (Ptr (Sent a))
unsafeSendArray arr =
  traverse unsafeSend arr >>= unsafeSend

-- | Send an array by safely sending both the elements and the array itself.
-- The array and elements will need to be explicitly deallocated.
sendArray
  :: (Send a, Storable (Sent a))
  => [a]
  -> SimCont b (Ptr (Sent a))
sendArray arr =
  traverse send arr >>= send

-- | Send a string by taking a temporary view of the String as a CString.
unsafeSendString :: String -> SimCont b CString
unsafeSendString str =
  Sim.liftCont (FFI.withCString str)

-- | Send a string by allocating a new CString which must be explicitly freed.
sendString :: String -> SimCont b CString
sendString str =
  IO.liftIO (FFI.newCString str)

instance UnsafeSend ByteString where
  type Sent ByteString = CString

  unsafeSend str =
    Sim.liftCont (BS.unsafeUseAsCString str)

instance Send ByteString where
  send str = do
    cstr <- unsafeSend str
    let len = BS.length str + 1

    IO.liftIO $ do
      bytes <- FFI.mallocBytes len
      FFI.copyBytes bytes cstr len

      pure bytes

class UnsafeReceive a where
  type Received a

  unsafeReceive :: (HasCallStack, Typeable b) => Received a -> SimCont b a

class UnsafeReceive a => Receive a where
  receive :: (HasCallStack, Typeable b) => Received a -> SimCont b a

instance (UnsafeReceive a, Storable (Received a)) => UnsafeReceive (Maybe a) where
  type Received (Maybe a) = Ptr (Received a)

  unsafeReceive ptr
    | ptr == FFI.nullPtr
    = pure Nothing

    | otherwise
    = Just <$> unsafePeekReceive ptr

instance (Receive a, Storable (Received a)) => Receive (Maybe a) where
  receive ptr
    | ptr == FFI.nullPtr
    = pure Nothing

    | otherwise
    = Just <$> peekReceive ptr

instance UnsafeReceive ByteString where
  type Received ByteString = CString

  unsafeReceive =
    IO.liftIO . BS.unsafePackCString

instance Receive ByteString where
  receive =
    IO.liftIO . BS.packCString

unsafePeekReceive
  :: (UnsafeReceive a, Storable (Received a), Typeable b)
  => Ptr (Received a)
  -> SimCont b a
unsafePeekReceive ptr =
  IO.liftIO (FFI.peek ptr) >>= unsafeReceive

peekReceive
  :: (Receive a, Storable (Received a), Typeable b)
  => Ptr (Received a)
  -> SimCont b a
peekReceive ptr =
  IO.liftIO (FFI.peek ptr) >>= receive

unsafeReceiveArray0
  :: (UnsafeReceive a, Eq (Received a), Storable (Received a), Typeable b)
  => Received a
  -> Ptr (Received a)
  -> SimCont b [a]
unsafeReceiveArray0 end ptr =
  IO.liftIO (FFI.peekArray0 end ptr) >>= traverse unsafeReceive

receiveArray0
  :: (Receive a, Eq (Received a), Storable (Received a), Typeable b)
  => Received a
  -> Ptr (Received a)
  -> SimCont b [a]
receiveArray0 end ptr =
  IO.liftIO (FFI.peekArray0 end ptr) >>= traverse receive

unsafeReceiveArrayN
  :: (UnsafeReceive a, Storable (Received a), Typeable b)
  => Int
  -> Ptr (Received a)
  -> SimCont b [a]
unsafeReceiveArrayN len ptr =
  IO.liftIO (FFI.peekArray len ptr) >>= traverse unsafeReceive

receiveArrayN
  :: (Receive a, Storable (Received a), Typeable b)
  => Int
  -> Ptr (Received a)
  -> SimCont b [a]
receiveArrayN len ptr =
  IO.liftIO (FFI.peekArray len ptr) >>= traverse receive

receiveString :: CString -> SimCont b String
receiveString =
  IO.liftIO . FFI.peekCString

