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
    CRepr
  , Send(..)
  , UnsafeSend(..)
  , Receive(..)
  , UnsafeReceive(..)
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
  , peekCStringBound
  , ensureNullTerminated
  ) where

import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS (length, packCString, null, last, snoc)
import qualified Data.ByteString.Unsafe as BS
import           Data.Typeable (Typeable)
import           Foreign.C.String (CString)
import qualified Foreign.C.String as FFI
import           Foreign.C.Types (CChar)
import qualified Foreign.Marshal.Alloc as FFI (mallocBytes)
import qualified Foreign.Marshal.Array as FFI
import qualified Foreign.Marshal.Utils as FFI (copyBytes)
import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)
import           Foreign.Storable (Storable, sizeOf)
import qualified Foreign.Storable as FFI (peek, poke, peekElemOff)
import           GHC.Stack (HasCallStack)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim

-- | A type family for determining the representation of a type @a@ when it is
-- sent / received over FFI. Types are converted to their C representation
-- using 'Send' and converted from their C representation using 'Receive'.
--
type family CRepr a

type instance CRepr ByteString = CString
type instance CRepr [a] = Ptr a
type instance CRepr (Maybe a) = Ptr (CRepr a)

-- TODO Should these classes be `SafeX a => UnsafeX a` instead? For things like
-- C enum tags, there is no unsafe way to convert values, so x == unsafeX.

-- | A class for data with raw values which can be unsafely sent over the FFI.
-- It may be desirable to send values unsafely, as unsafe sending allows data
-- to be sent without needing to create a new copy. Callers are responsible for
-- making sure that data is never unsafely sent to a VPI function which takes
-- ownership of a pointer, as any modification to this value will silently
-- modify the original value, potentially corrupting the Haskell RTS state.
--
-- For the safe version of this class, see 'Send'. To receive data from the FFI
-- see 'Receive' and 'UnsafeReceive'.
--
class UnsafeSend a where
  unsafeSend :: HasCallStack => a -> SimCont b (CRepr a)

-- | A class for data with raw values which can be safely sent over the FFI.
-- Safely sending data involves making new copies where necessary, so the
-- original value is not corrupted if the FFI call is impure. Memory allocated
-- by this class must be manually deallocated.
--
-- If the data being sent is never mutated by the FFI, consider using
-- 'UnsafeSend' instead as it does not need to copy the data first.
--
class Send a where
  send :: HasCallStack => a -> SimCont b (CRepr a)

instance Storable a => UnsafeSend [a] where
  unsafeSend xs = Sim.liftCont (FFI.withArray xs)

instance Storable a => Send [a] where
  send = IO.liftIO . FFI.newArray

instance (UnsafeSend a, Storable (CRepr a)) => UnsafeSend (Maybe a) where
  unsafeSend = maybe (pure FFI.nullPtr) unsafePokeSend

instance (Send a, Storable (CRepr a)) => Send (Maybe a) where
  send = maybe (pure FFI.nullPtr) pokeSend

-- | Unsafely send a value, then allocate a new pointer on the stack and assign
-- this value to the pointer. Since the value pointed to is unsafely sent, it
-- should not need deallocating.
--
unsafePokeSend
  :: (UnsafeSend a, Storable (CRepr a))
  => a
  -> SimCont b (Ptr (CRepr a))
unsafePokeSend x = do
  raw <- unsafeSend x
  fst <$> Sim.withNewPtr Sim.stackPtr (`FFI.poke` raw)

-- | Safely send a value, then allocate a new pointer on the stack and assign
-- this value to the pointer. The caller is responsible for deallocating the
-- value pointed at by the returned pointer if necessary.
--
pokeSend
  :: (Send a, Storable (CRepr a))
  => a
  -> SimCont b (Ptr (CRepr a))
pokeSend x = do
  raw <- send x
  fst <$> Sim.withNewPtr Sim.stackPtr (`FFI.poke` raw)

-- | Send a string by taking a temporary view of the String as a CString.
--
unsafeSendString :: String -> SimCont b CString
unsafeSendString str = Sim.liftCont (FFI.withCString str)

-- | Send a string by allocating a new CString which must be explicitly freed.
--
sendString :: String -> SimCont b CString
sendString str = IO.liftIO (FFI.newCString str)

instance UnsafeSend ByteString where
  unsafeSend str = Sim.liftCont (BS.unsafeUseAsCString str)

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
  unsafeReceive :: (HasCallStack, Typeable b) => CRepr a -> SimCont b a

-- | A class for data with raw values which can be safely received over the FFI.
-- Safely receiving data involves making new copies where necessary, so the
-- received value is not corrupted if the original value is later modified.
-- Memory allocated by this class must be manually deallocated.
--
-- If the data being received is newly allocated by the FFI call, consider
-- using 'UnsafeReceive', provided it will not be later mutated.
--
class Receive a where
  receive :: (HasCallStack, Typeable b) => CRepr a -> SimCont b a

instance (UnsafeReceive a, Storable (CRepr a)) => UnsafeReceive (Maybe a) where
  unsafeReceive ptr
    | ptr == FFI.nullPtr
    = pure Nothing

    | otherwise
    = Just <$> unsafePeekReceive ptr

instance (Receive a, Storable (CRepr a)) => Receive (Maybe a) where
  receive ptr
    | ptr == FFI.nullPtr
    = pure Nothing

    | otherwise
    = Just <$> peekReceive ptr

instance UnsafeReceive ByteString where
  unsafeReceive = IO.liftIO . BS.unsafePackCString

instance Receive ByteString where
  receive = IO.liftIO . BS.packCString

-- | Deference a pointer, then unsafely receive the value of the pointer. Since
-- the value is unsafely received, any change to the pointed to value will
-- corrupt the received value.
--
unsafePeekReceive
  :: (UnsafeReceive a, Storable (CRepr a), Typeable b)
  => Ptr (CRepr a)
  -> SimCont b a
unsafePeekReceive ptr =
  IO.liftIO (FFI.peek ptr) >>= unsafeReceive

-- | Dereference a pointer, then safely receive the value of the pointer. The
-- caller is responsible for deallocating the received value if necessary.
--
peekReceive
  :: (Receive a, Storable (CRepr a), Typeable b)
  => Ptr (CRepr a)
  -> SimCont b a
peekReceive ptr =
  IO.liftIO (FFI.peek ptr) >>= receive

-- | Unsafely receive an array of values, with the end of the array
-- marked by the given final element. The search for the marker is
-- bounded by 'bound'. Each element of the array is unsafely received.
unsafeReceiveArray0
  :: (UnsafeReceive a, Eq (CRepr a), Storable (CRepr a), Typeable b)
  => Int
  -> CRepr a
  -> Ptr (CRepr a)
  -> SimCont b [a]
unsafeReceiveArray0 bound end ptr =
  IO.liftIO (boundedPeekArray0 bound end ptr) >>= traverse unsafeReceive

-- | Safely receive an array of values, with the end of the array
-- marked by the given final element. The search for the marker is
-- bounded by 'bound'. The caller is responsible for deallocating the
-- elements of the array if necessary.
receiveArray0
  :: (Receive a, Eq (CRepr a), Storable (CRepr a), Typeable b)
  => Int
  -> CRepr a
  -> Ptr (CRepr a)
  -> SimCont b [a]
receiveArray0 bound end ptr =
  IO.liftIO (boundedPeekArray0 bound end ptr) >>= traverse receive

-- | Variant of 'Foreign.Marshal.Array.lengthArray0' using an upper
-- bound on the elements when searching for the terminator.
boundedLengthArray0
  :: (Storable a, Eq a)
  => Int
  -> a
  -> Ptr a -> IO Int
boundedLengthArray0 bound marker ptr = loop 0
  where
    loop i
      | i >= bound = return bound
      | otherwise = do
          val <- FFI.peekElemOff ptr i
          if val == marker then return i else loop (i+1)

-- | Variant of 'Foreign.Marshal.Array.peekArray0' using an upper
-- bound on the elements when searching for the terminator.
boundedPeekArray0
  :: (Storable a, Eq a)
  => Int
  -> a
  -> Ptr a
  -> IO [a]
boundedPeekArray0 bound marker ptr = do
  size <- boundedLengthArray0 bound marker ptr
  FFI.peekArray size ptr

-- | Variant of 'Foreign.C.String.peekCString' using an upper bound on the
-- elements when searching for the NUL terminator.
peekCStringBound :: Int -> CString -> IO String
peekCStringBound bound cp = do
  let nNL = (0 :: CChar)
  sz <- boundedLengthArray0 bound nNL cp
  FFI.peekCStringLen (cp, sz * sizeOf nNL)

-- | Safely receive a string. Users are recommended to use
-- 'ByteString' instead which supports safe and unsafe sending /
-- receiving.
receiveString :: CString -> SimCont b String
receiveString =
  IO.liftIO . FFI.peekCString

-- | Ensure that the given 'ByteString' is a null-terminated 'ByteString'
ensureNullTerminated :: ByteString -> ByteString
ensureNullTerminated bs =
  if not (BS.null bs) && BS.last bs == 0 then bs else BS.snoc bs 0
