module Clash.FFI.Monad
  ( SimCont
  , liftCont
  , SimAction
  , runSimAction
  , stackPtr
  , heapPtr
  , withNewPtr
  , readPtr
  , unsafeFreeWith
  , unsafeFreePtr
  , freePtr
  , throw
  ) where

import           Control.Exception (Exception)
import qualified Control.Exception as IO (throwIO)
import qualified Control.Monad as Monad (unless)
import           Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.IO.Class as IO (liftIO)
import           Control.Monad.Cont (ContT(ContT), MonadCont)
import qualified Control.Monad.Cont as Cont (runContT)
import qualified Foreign.Marshal.Alloc as FFI (alloca, free, malloc)
import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)
import           Foreign.Storable (Storable)
import qualified Foreign.Storable as FFI (peek)
import           GHC.Stack (HasCallStack)

newtype SimCont o i = SimCont (ContT o IO i)
  deriving newtype (Applicative, Functor, Monad, MonadCont, MonadIO)

liftCont :: ((i -> IO o) -> IO o) -> SimCont o i
liftCont = SimCont . ContT

type SimAction a = SimCont a a

runSimAction :: SimAction a -> IO a
runSimAction (SimCont cont) = Cont.runContT cont pure

-- | Allocate memory that will be automatically deallocated when the action
-- has finished. For long-lasting allocations, see 'heapPtr'.
stackPtr :: (HasCallStack, Storable a) => SimCont b (Ptr a)
stackPtr = SimCont (ContT FFI.alloca)

-- | Allocate memory that will not be automatically deallocated when the action
-- has finished. This must be deallocated with 'freePtr'. For memory which is
-- only needed temporarily, 'stackPtr' should be preferred.
heapPtr :: (HasCallStack, Storable a) => SimCont b (Ptr a)
heapPtr = IO.liftIO FFI.malloc

withNewPtr
  :: Storable a
  => SimCont c (Ptr a)
  -> (Ptr a -> IO b)
  -> SimCont c (Ptr a, b)
withNewPtr alloc set = do
  ptr <- alloc
  res <- IO.liftIO (set ptr)
  pure (ptr, res)

readPtr :: HasCallStack => Storable a => Ptr a -> SimCont b a
readPtr = IO.liftIO . FFI.peek

-- | Free allocated memory using the provided function. If the memory was
-- allocated with 'heapPtr' then 'unsafeFreePtr' should be used instead.
--
-- This function does not check if the pointer given is NULL, meaning it will
-- panic if given NULL. It should only be used for pointers known to be valid.
unsafeFreeWith :: HasCallStack => (a -> IO ()) -> a -> SimCont b ()
unsafeFreeWith f = IO.liftIO . f

-- | Free allocated memory that was allocated with 'heapPtr'. If the memory
-- requires a destructor other than 'FFI.free', or a type other than 'Ptr a'
-- then 'unsafeFreeWith' should be used instead.
--
-- This function does not check if the pointer given is NULL, meaning it will
-- panic if given NULL. It should only be used for pointers known to be valid.
unsafeFreePtr :: HasCallStack => Ptr a -> SimCont b ()
unsafeFreePtr = unsafeFreeWith FFI.free

-- | Free allocated memory that was allocated with 'heapPtr'. If the memory
-- requires a destructor other than 'FFI.free', or a type other than 'Ptr a'
-- then 'unsafeFreeWith' should be used instead.
freePtr :: HasCallStack => Ptr a -> SimCont b ()
freePtr = unsafeFreeWith (\p -> Monad.unless (p == FFI.nullPtr) (FFI.free p))

-- | Throw an exception in simulation. Unless caught this will cause the GHC
-- RTS to exit, which will cause the simulator to stop / hang / enter a prompt.
throw :: (HasCallStack, Exception e) => e -> SimCont b a
throw ex = IO.liftIO (IO.throwIO ex)

