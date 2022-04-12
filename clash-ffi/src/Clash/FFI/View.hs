{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

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
  , unsafeReceiveArray0
  , receiveArray0
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

-- TODO Should these classes be `SafeX a => UnsafeX a` instead? For things like
-- C enum tags, there is no unsafe way to convert values, so x == unsafeX.

-- | A class for data with raw values which can be unsafely sent over the FFI.
-- It may be desirable to send values unsafely, as unsafe sending allows data
-- to be sent without needing to create a new copy. Callers are reponsible for
-- making sure that data is never unsafely sent to a VPI function which takes
-- ownership of a pointer, as any modification to this value will silently
-- modify the original value, potentially corrupting the Haskell RTS state.
--
-- For the safe version of this class, see 'Send'. To receive data from the FFI
-- see 'Receive' and 'UnsafeReceive'.
--
class UnsafeSend a where
  type Sent a

  unsafeSend :: HasCallStack => a -> SimCont b (Sent a)

-- | A class for data with raw values which can be safely sent over the FFI.
-- Safely sending data involves making new copies where necessary, so the
-- original value is not corrupted if the FFI call is impure. Memory allocated
-- by this class must be manually deallocated.
--
-- If the data being sent is never mutated by the FFI, consider using
-- 'UnsafeSend' instead as it does not need to copy the data first.
--
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

-- | Unsafely send a value, then allocate a new pointer on the stack and assign
-- this value to the pointer. Since the value pointed to is unsafely sent, it
-- should not need deallocating.
--
unsafePokeSend
  :: (UnsafeSend a, Storable (Sent a))
  => a
  -> SimCont b (Ptr (Sent a))
unsafePokeSend x = do
  raw <- unsafeSend x
  fst <$> Sim.withNewPtr Sim.stackPtr (`FFI.poke` raw)

-- | Safely send a value, then allocate a new pointer on the stack and assign
-- this value to the pointer. The caller is responsible for deallocating the
-- value pointed at by the returned pointer if necessary.
--
pokeSend
  :: (Send a, Storable (Sent a))
  => a
  -> SimCont b (Ptr (Sent a))
pokeSend x = do
  raw <- send x
  fst <$> Sim.withNewPtr Sim.stackPtr (`FFI.poke` raw)

-- | Send a string by taking a temporary view of the String as a CString.
--
unsafeSendString :: String -> SimCont b CString
unsafeSendString str =
  Sim.liftCont (FFI.withCString str)

-- | Send a string by allocating a new CString which must be explicitly freed.
--
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

-- | A class for data with raw values which can be unsafely received over the
-- FFI. It may be desirable to receive values unsafely, as this allows data to
-- be interpreted as some high-level representation without needing to create
-- a new copy. Callers are responsible for making sure data is not unsafely
-- received from an FFI function which will deallocate / mutate the data, as
-- this will silently corrupt the Haskell RTS state.
--
-- For the safe version of this class, see 'Receive'. To send data to the FFI
-- see 'Send' and 'UnsafeSend'.
--
class UnsafeReceive a where
  type Received a

  unsafeReceive :: (HasCallStack, Typeable b) => Received a -> SimCont b a

-- | A class for data with raw values which can be safely received over the FFI.
-- Safely receiving data involves making new copies where necessary, so the
-- received value is not corrupted if the original value is later modified.
-- Memory allocated by this class must be manually deallocated.
--
-- If the data being received is newly allocated by the FFI call, consider
-- using 'UnsafeReceive', provided it will not be later mutated.
--
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

-- | Deference a pointer, then unsafely receive the value of the pointer. Since
-- the value is unsafely received, any change to the pointed to value will
-- corrupt the received value.
--
unsafePeekReceive
  :: (UnsafeReceive a, Storable (Received a), Typeable b)
  => Ptr (Received a)
  -> SimCont b a
unsafePeekReceive ptr =
  IO.liftIO (FFI.peek ptr) >>= unsafeReceive

-- | Dereference a pointer, then safely receive the value of the pointer. The
-- caller is responsible for deallocating the received value if necessary.
--
peekReceive
  :: (Receive a, Storable (Received a), Typeable b)
  => Ptr (Received a)
  -> SimCont b a
peekReceive ptr =
  IO.liftIO (FFI.peek ptr) >>= receive

-- | Unsafely receive an array of values, with the end of the array marked by
-- the given final element. Each element of the array is unsafely received.
--
unsafeReceiveArray0
  :: (UnsafeReceive a, Eq (Received a), Storable (Received a), Typeable b)
  => Received a
  -> Ptr (Received a)
  -> SimCont b [a]
unsafeReceiveArray0 end ptr =
  IO.liftIO (FFI.peekArray0 end ptr) >>= traverse unsafeReceive

-- | Safely receive an array of values, with the end of the array marked by
-- the given final element. The caller is responsible for deallocating the
-- elements of the array if necessary.
--
receiveArray0
  :: (Receive a, Eq (Received a), Storable (Received a), Typeable b)
  => Received a
  -> Ptr (Received a)
  -> SimCont b [a]
receiveArray0 end ptr =
  IO.liftIO (FFI.peekArray0 end ptr) >>= traverse receive

-- | Safely receive a string. Users are recommended to use 'ByteString' instead
-- which supports safe and unsafe sending / receiving.
--
receiveString :: CString -> SimCont b String
receiveString =
  IO.liftIO . FFI.peekCString

