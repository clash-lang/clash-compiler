{-|
  Copyright   :  (C) 2019, Google Inc.,
                     2022, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  I\/O actions that are translatable to HDL
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns, MagicHash, TypeOperators, ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE DataKinds, GADTs, TypeApplications #-}

module Clash.Explicit.SimIO
  ( -- * I\/O environment for simulation
    mealyIO
  , SimIO
  -- * Display on stdout
  , display
  -- * End of simulation
  , finish
  -- * Mutable values
  , Reg
  , reg
  , readReg
  , writeReg
  -- * File I\/O
  , File
  , openFile
  , closeFile
  -- ** Reading and writing characters
  , getChar
  , putChar
  -- ** Reading strings
  , getLine
  -- ** Detecting the end of input
  , isEOF
    -- ** Buffering operations
  , flush
    -- ** Repositioning handles
  , seek
  , rewind
  , tell
  )
where

import Control.Monad (when)
#if __GLASGOW_HASKELL__ < 900
import Data.Coerce
#endif
import Data.IORef
import GHC.TypeLits
import Prelude hiding (getChar, putChar, getLine)
import qualified System.IO as IO
import System.IO.Unsafe

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Promoted.Nat
import Clash.Signal.Internal
import Clash.Sized.Unsigned
import Clash.Sized.Vector (Vec (..))
import Clash.XException (seqX)

-- | Simulation-level I\/O environment; synthesizable to HDL I\/O, which in
-- itself is unlikely to be synthesisable to a digital circuit.
--
-- See 'mealyIO' as to its use.
#if __GLASGOW_HASKELL__ >= 900
data SimIO a = SimIO {unSimIO :: !(IO a)}
#else
newtype SimIO a = SimIO {unSimIO :: IO a}
#endif
{-# ANN unSimIO hasBlackBox #-}

instance Functor SimIO where
  fmap = fmapSimIO#

fmapSimIO# :: (a -> b) -> SimIO a -> SimIO b
fmapSimIO# f (SimIO m) = SimIO (fmap f m)
{-# NOINLINE fmapSimIO# #-}
{-# ANN fmapSimIO# hasBlackBox #-}

instance Applicative SimIO where
  pure  = pureSimIO#
  (<*>) = apSimIO#

pureSimIO# :: a -> SimIO a
pureSimIO# a = SimIO (pure a)
{-# NOINLINE pureSimIO# #-}
{-# ANN pureSimIO# hasBlackBox #-}

apSimIO# :: SimIO (a -> b) -> SimIO a -> SimIO b
apSimIO# (SimIO f) (SimIO m) = SimIO (f <*> m)
{-# NOINLINE apSimIO# #-}
{-# ANN apSimIO# hasBlackBox #-}

instance Monad SimIO where
  return = pureSimIO#
  (>>=)  = bindSimIO#

bindSimIO# :: SimIO a -> (a -> SimIO b) -> SimIO b
#if __GLASGOW_HASKELL__ >= 900
bindSimIO# (SimIO m) k = SimIO (m >>= (\x -> x `seqX` unSimIO (k x)))
#else
bindSimIO# (SimIO m) k = SimIO (m >>= (\x -> x `seqX` coerce k x))
#endif
{-# NOINLINE bindSimIO# #-}
{-# ANN bindSimIO# hasBlackBox #-}

-- | Display a string on /stdout/
display
  :: String
  -- ^ String you want to display
  -> SimIO ()
display s = SimIO (putStrLn s)
{-# NOINLINE display #-}
{-# ANN display hasBlackBox #-}

-- | Finish the simulation with an exit code
finish
  :: Integer
  -- ^ The exit code you want to return at the end of the simulation
  -> SimIO a
finish i = return (error (show i))
{-# NOINLINE finish #-}
{-# ANN finish hasBlackBox #-}

-- | Mutable reference
#if __GLASGOW_HASKELL__ >= 900
data Reg a = Reg !(IORef a)
#else
newtype Reg a = Reg (IORef a)
#endif

-- | Create a new mutable reference with the given starting value
reg
  :: a
  -- ^ The starting value
  -> SimIO (Reg a)
reg a = SimIO (Reg <$> newIORef a)
{-# NOINLINE reg #-}
{-# ANN reg hasBlackBox #-}

-- | Read value from a mutable reference
readReg :: Reg a -> SimIO a
readReg (Reg a) = SimIO (readIORef a)
{-# NOINLINE readReg #-}
{-# ANN readReg hasBlackBox #-}

-- | Write new value to the mutable reference
writeReg
  :: Reg a
  -- ^ The mutable reference
  -> a
  -- ^ The new value
  -> SimIO ()
writeReg (Reg r) a = SimIO (writeIORef r a)
{-# NOINLINE writeReg #-}
{-# ANN writeReg hasBlackBox #-}

-- | File handle
#if __GLASGOW_HASKELL__ >= 900
data File = File !IO.Handle
#else
newtype File = File IO.Handle
#endif

-- | Open a file
openFile
  :: FilePath
  -- ^ File to open
  -> String
  -- ^ File mode:
  --
  -- * "r": Open for reading
  -- * "w": Create for writing
  -- * "a": Append
  -- * "r+": Open for update (reading and writing)
  -- * "w+": Create for update
  -- * "a+": Append, open or create for update at end-of-file
  -> SimIO File
#if __GLASGOW_HASKELL__ >= 900
openFile fp "r"   = SimIO $ fmap File (IO.openFile fp IO.ReadMode)
openFile fp "w"   = SimIO $ fmap File (IO.openFile fp IO.WriteMode)
openFile fp "a"   = SimIO $ fmap File (IO.openFile fp IO.AppendMode)
openFile fp "rb"  = SimIO $ fmap File (IO.openBinaryFile fp IO.ReadMode)
openFile fp "wb"  = SimIO $ fmap File (IO.openBinaryFile fp IO.WriteMode)
openFile fp "ab"  = SimIO $ fmap File (IO.openBinaryFile fp IO.AppendMode)
openFile fp "r+"  = SimIO $ fmap File (IO.openFile fp IO.ReadWriteMode)
openFile fp "w+"  = SimIO $ fmap File (IO.openFile fp IO.WriteMode)
openFile fp "a+"  = SimIO $ fmap File (IO.openFile fp IO.AppendMode)
openFile fp "r+b" = SimIO $ fmap File (IO.openBinaryFile fp IO.ReadWriteMode)
openFile fp "w+b" = SimIO $ fmap File (IO.openBinaryFile fp IO.WriteMode)
openFile fp "a+b" = SimIO $ fmap File (IO.openBinaryFile fp IO.AppendMode)
openFile fp "rb+" = SimIO $ fmap File (IO.openBinaryFile fp IO.ReadWriteMode)
openFile fp "wb+" = SimIO $ fmap File (IO.openBinaryFile fp IO.WriteMode)
openFile fp "ab+" = SimIO $ fmap File (IO.openBinaryFile fp IO.AppendMode)
#else
openFile fp "r"   = coerce (IO.openFile fp IO.ReadMode)
openFile fp "w"   = coerce (IO.openFile fp IO.WriteMode)
openFile fp "a"   = coerce (IO.openFile fp IO.AppendMode)
openFile fp "rb"  = coerce (IO.openBinaryFile fp IO.ReadMode)
openFile fp "wb"  = coerce (IO.openBinaryFile fp IO.WriteMode)
openFile fp "ab"  = coerce (IO.openBinaryFile fp IO.AppendMode)
openFile fp "r+"  = coerce (IO.openFile fp IO.ReadWriteMode)
openFile fp "w+"  = coerce (IO.openFile fp IO.WriteMode)
openFile fp "a+"  = coerce (IO.openFile fp IO.AppendMode)
openFile fp "r+b" = coerce (IO.openBinaryFile fp IO.ReadWriteMode)
openFile fp "w+b" = coerce (IO.openBinaryFile fp IO.WriteMode)
openFile fp "a+b" = coerce (IO.openBinaryFile fp IO.AppendMode)
openFile fp "rb+" = coerce (IO.openBinaryFile fp IO.ReadWriteMode)
openFile fp "wb+" = coerce (IO.openBinaryFile fp IO.WriteMode)
openFile fp "ab+" = coerce (IO.openBinaryFile fp IO.AppendMode)
#endif
openFile _  m     = error ("openFile unknown mode: " ++ show m)
{-# NOINLINE openFile #-}
{-# ANN openFile hasBlackBox #-}

-- | Close a file
closeFile
  :: File
  -> SimIO ()
closeFile (File fp) = SimIO (IO.hClose fp)
{-# NOINLINE closeFile #-}
{-# ANN closeFile hasBlackBox #-}

-- | Read one character from a file
getChar
  :: File
  -- ^ File to read from
  -> SimIO Char
getChar (File fp) = SimIO (IO.hGetChar fp)
{-# NOINLINE getChar #-}
{-# ANN getChar hasBlackBox #-}

-- | Insert a character into a buffer specified by the file
putChar
  :: Char
  -- ^ Character to insert
  -> File
  -- ^ Buffer to insert to
  -> SimIO ()
putChar c (File fp) = SimIO (IO.hPutChar fp c)
{-# NOINLINE putChar #-}
{-# ANN putChar hasBlackBox #-}

-- | Read one line from a file
getLine
  :: forall n
   . KnownNat n
  => File
  -- ^ File to read from
  -> Reg (Vec n (Unsigned 8))
  -- ^ Vector to store the content
  -> SimIO Int
getLine (File fp) (Reg r) = SimIO $ do
  s <- IO.hGetLine fp
  let d = snatToNum (SNat @n) - length s
  when (d < 0) (IO.hSeek fp IO.RelativeSeek (toInteger d))
  modifyIORef r (rep s)
  return 0
 where
   rep :: String -> Vec m (Unsigned 8) -> Vec m (Unsigned 8)
   rep []     vs          = vs
   rep (x:xs) (Cons _ vs) = Cons (toEnum (fromEnum x)) (rep xs vs)
   rep _      Nil         = Nil
{-# NOINLINE getLine #-}
{-# ANN getLine hasBlackBox #-}

-- | Determine whether we've reached the end of the file
isEOF
  :: File
  -- ^ File we want to inspect
  -> SimIO Bool
isEOF (File fp) = SimIO (IO.hIsEOF fp)
{-# NOINLINE isEOF #-}
{-# ANN isEOF hasBlackBox #-}

-- | Set the position of the next operation on the file
seek
  :: File
  -- ^ File to set the position for
  -> Integer
  -- ^ Position
  -> Int
  -- ^ Mode:
  --
  -- * 0: From the beginning of the file
  -- * 1: From the current position
  -- * 2: From the end of the file
  -> SimIO Int
seek (File fp) pos mode = SimIO (IO.hSeek fp (toEnum mode) pos >> return 0)
{-# NOINLINE seek #-}
{-# ANN seek hasBlackBox #-}

-- | Set the position of the next operation to the beginning of the file
rewind
  :: File
  -> SimIO Int
rewind (File fp) = SimIO (IO.hSeek fp IO.AbsoluteSeek 0 >> return 0)
{-# NOINLINE rewind #-}
{-# ANN rewind hasBlackBox #-}

-- | Returns the offset from the beginning of the file (in bytes).
tell
  :: File
  -- ^ File we want to inspect
  -> SimIO Integer
tell (File fp) = SimIO (IO.hTell fp)
{-# NOINLINE tell #-}
{-# ANN tell hasBlackBox #-}

-- | Write any buffered output to file
flush
  :: File
  -> SimIO ()
flush (File fp) = SimIO (IO.hFlush fp)
{-# NOINLINE flush #-}
{-# ANN flush hasBlackBox #-}

-- | Simulation-level I/O environment that can be synthesized to HDL-level I\/O.
-- Note that it is unlikely that the HDL-level I\/O can subsequently be
-- synthesized to a circuit.
--
-- = Example
--
-- @
-- tbMachine :: (File,File) -> Int -> SimIO Int
-- tbMachine (fileIn,fileOut) regOut = do
--   eofFileOut <- 'isEOF' fileOut
--   eofFileIn  <- 'isEOF' fileIn
--   when (eofFileIn || eofFileOut) $ do
--     'display' "success"
--     'finish' 0
--
--   goldenIn  <- 'getChar' fileIn
--   goldenOut <- 'getChar' fileOut
--   res <- if regOut == fromEnum goldenOut then do
--            return (fromEnum goldenIn)
--          else do
--            'display' "Output doesn't match golden output"
--            'finish' 1
--   display ("Output matches golden output")
--   return res
--
-- tbInit :: (File,File)
-- tbInit = do
--   fileIn  <- 'openFile' "./goldenInput00.txt" "r"
--   fileOut <- 'openFile' "./goldenOutput00.txt" "r"
--   return (fileIn,fileOut)
--
-- topEntity :: Signal System Int
-- topEntity = regOut
--   where
--     clk = systemClockGen
--     rst = resetGen
--     ena = enableGen
--
--     regOut = register clk rst ena (fromEnum \'a\') regIn
--     regIn  = 'mealyIO' clk tbMachine tbInit regOut
-- @
mealyIO
  :: KnownDomain dom
  => Clock dom
  -- ^ Clock at which rate the I\/O environment progresses
  -> (s -> i -> SimIO o)
  -- ^ Transition function inside an I\/O environment
  -> SimIO s
  -- ^ I/O action to create the initial state
  -> Signal dom i
  -> Signal dom o
mealyIO !_ f (SimIO i) inp = unsafePerformIO (i >>= go inp)
 where
  go q@(~(k :- ks)) s =
    (:-) <$> unSimIO (f s k) <*> unsafeInterleaveIO ((q `seq` go ks s))
{-# NOINLINE mealyIO #-}
