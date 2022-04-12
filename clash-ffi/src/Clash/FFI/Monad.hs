{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.FFI.Monad
  ( SimCont
  , SimAction
  , runSimAction
  , liftCont
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

{-
NOTE [continuation-based API]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For clash-ffi, the API is designed around a continuation monad. The reason for
this is that we want control over allocated memory, but cannot use `ForeignPtr`
as we do not own a lot of the allocated memory.

We could have stayed in IO directly, however the API given in `base` can
quickly become burdensome to read and write, since `alloca` has the type

  alloca :: Storable a => (Ptr a -> IO b) -> IO b

which can lead to an excess of nesting in functions. Using continuations we
can provide a more consistent API where stack and heap pointers are used in the
same way, e.g.

  do
    x <- stackPtr
    y <- heapPtr
    result <- f x y

    freePtr y
    pure result
-}

-- | The type of FFI actions with an input @i@ and an output @o@. For arbitrary
-- actions to be run in FFI, the type will be @SimCont o A@, where @A@ is the
-- result of the action (since this API is continuation-based the output of
-- one action is the input to the contination).
--
-- For the "main" action performed by @clash-ffi@, there is no input and no
-- output. The 'SimAction' synonym is intended for this case. Consequently,
-- there is no @runSimCont@ function, as actions should be run with
-- 'runSimAction'.
--
newtype SimCont o i = SimCont (ContT o IO i)
  deriving newtype (Applicative, Functor, Monad, MonadCont, MonadIO)

-- | The type of an VPI "main" action run in @clash-ffi@. For the more general
-- type of FFI computations, use 'SimCont'.
--
type SimAction = SimCont () ()

-- | Run a VPI "main" action. See 'SimAction' and 'SimCont' for more
-- information.
--
runSimAction :: SimAction -> IO ()
runSimAction (SimCont cont) = Cont.runContT cont pure

-- | Lift a continuation into a simuation FFI action.
--
liftCont :: ((i -> IO o) -> IO o) -> SimCont o i
liftCont = SimCont . ContT

-- | Allocate memory that will be automatically deallocated when the action
-- has finished. For long-lasting allocations, see 'heapPtr'.
--
stackPtr :: (HasCallStack, Storable a) => SimCont b (Ptr a)
stackPtr = SimCont (ContT FFI.alloca)

-- | Allocate memory that will not be automatically deallocated when the action
-- has finished. This must be deallocated with 'freePtr'. For memory which is
-- only needed temporarily, 'stackPtr' should be preferred.
--
heapPtr :: (HasCallStack, Storable a) => SimCont b (Ptr a)
heapPtr = IO.liftIO FFI.malloc

-- | Allocate memory using the provided strategy (see 'stackPtr' and 'heapPtr')
-- and perform the given action with the newly allocated pointer. Both the
-- allocated pointer and the result of the action are returned, so that the
-- pointer can be deallocated later if necessary.
--
withNewPtr
  :: Storable a
  => SimCont c (Ptr a)
  -> (Ptr a -> IO b)
  -> SimCont c (Ptr a, b)
withNewPtr alloc set = do
  ptr <- alloc
  res <- IO.liftIO (set ptr)
  pure (ptr, res)

-- | Dereference a pointer, returning its current value. The caller is
-- responsible for ensuring the pointer is valid.
--
readPtr :: HasCallStack => Storable a => Ptr a -> SimCont b a
readPtr = IO.liftIO . FFI.peek

-- | Free allocated memory using the provided function. If the memory was
-- allocated with 'heapPtr' then 'unsafeFreePtr' should be used instead.
--
-- This function does not check if the pointer given is NULL, meaning it will
-- panic if given NULL. It should only be used for pointers known to be valid.
--
unsafeFreeWith :: HasCallStack => (a -> IO ()) -> a -> SimCont b ()
unsafeFreeWith f = IO.liftIO . f

-- | Free allocated memory that was allocated with 'heapPtr'. If the memory
-- requires a destructor other than 'FFI.free', or a type other than 'Ptr a'
-- then 'unsafeFreeWith' should be used instead.
--
-- This function does not check if the pointer given is NULL, meaning it will
-- panic if given NULL. It should only be used for pointers known to be valid.
--
unsafeFreePtr :: HasCallStack => Ptr a -> SimCont b ()
unsafeFreePtr = unsafeFreeWith FFI.free

-- | Free allocated memory that was allocated with 'heapPtr'. If the memory
-- requires a destructor other than 'FFI.free', or a type other than 'Ptr a'
-- then 'unsafeFreeWith' should be used instead.
--
freePtr :: HasCallStack => Ptr a -> SimCont b ()
freePtr = unsafeFreeWith (\p -> Monad.unless (p == FFI.nullPtr) (FFI.free p))

-- | Throw an exception in simulation. Unless caught this will cause the GHC
-- RTS to exit, which will cause the simulator to stop / hang / enter a prompt.
--
throw :: (HasCallStack, Exception e) => e -> SimCont b a
throw ex = IO.liftIO (IO.throwIO ex)

-- TODO I should also provide catch here, and probably bracket

