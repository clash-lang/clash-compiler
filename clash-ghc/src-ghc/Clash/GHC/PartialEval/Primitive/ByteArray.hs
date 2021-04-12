{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Primitive.ByteArray
  ( byteArrayPrims
  ) where

import Control.Monad (when)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Maybe (fromJust)
import Data.Primitive.ByteArray as BA
import Data.Primitive.Types (Prim)
import Data.Text (Text)

import Clash.Core.Literal (Literal(..))
import Clash.Core.PartialEval.Monad
import Clash.Core.PartialEval.NormalForm

import Clash.GHC.PartialEval.Primitive.FromAst
import Clash.GHC.PartialEval.Primitive.Info
import Clash.GHC.PartialEval.Primitive.Strategy
import Clash.GHC.PartialEval.Primitive.ToAst
import Clash.GHC.PartialEval.Primitive.Unboxed

byteArrayPrims :: HashMap Text PrimImpl
byteArrayPrims = HashMap.fromList
  [ ("GHC.Prim.newByteArray#", primNewByteArray)
  , ("GHC.Prim.shrinkMutableByteArray#", primShrinkMutableByteArray)
  , ("GHC.Prim.resizeMutableByteArray#", primResizeMutableByteArray)
  , ("GHC.Prim.unsafeFreezeByteArray#", primUnsafeFreezeByteArray)
  , ("GHC.Prim.sizeofByteArray#", primSizeofByteArray)
  , ("GHC.Prim.getSizeofMutableByteArray#", primGetSizeofMutableByteArray)
  , ("GHC.Prim.indexCharArray#", primIndex UChar)
  , ("GHC.Prim.indexIntArray#", primIndex UInt)
  , ("GHC.Prim.indexWordArray#", primIndex UWord)
  , ("GHC.Prim.indexFloatArray#", primIndex UFloat)
  , ("GHC.Prim.indexDoubleArray#", primIndex UDouble)
  , ("GHC.Prim.readCharArray#", primRead UChar)
  , ("GHC.Prim.readIntArray#", primRead UInt)
  , ("GHC.Prim.readWordArray#", primRead UWord)
  , ("GHC.Prim.readFloatArray#", primRead UFloat)
  , ("GHC.Prim.readDoubleArray#", primRead UDouble)
  , ("GHC.Prim.writeCharArray#", primWrite boxChar)
  , ("GHC.Prim.writeIntArray#", primWrite boxInt)
  , ("GHC.Prim.writeWordArray#", primWrite boxWord)
  , ("GHC.Prim.writeFloatArray#", primWrite boxFloat)
  , ("GHC.Prim.writeDoubleArray#", primWrite boxDouble)
  , ("GHC.Prim.copyByteArray#", primCopyByteArray)
  , ("GHC.Prim.setByteArray#", primSetByteArray)
  ]

{-
NOTE [byteArray primitives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
In ghc-prim, some primitives are on MutableByteArray# instead of ByteArray#.
As there is currently no support for mutable byte array literals in clash,
these arguments are taken to be a reference to a ByteArray#. When implementing
new primtives on mutable byte arrays, it is therefore necessary to read the
byte array argument as a reference, and use unsafeThawByteArray to obtain a
mutable byte array.
-}

primNewByteArray :: PrimImpl
primNewByteArray pr args
  | [Right _, Left x, Left y] <- args
  = do size <- boxInt <$> fromValueForce x
       resTy <- resultType pr args
       ba <- liftIO (BA.newByteArray size >>= BA.unsafeFreezeByteArray)

       toValue (UTuple2 (y, Ref Nothing (UByteArray ba))) resTy

  | otherwise
  = throwM (UnexpectedArgs pr args)

primResizeMutableByteArray :: PrimImpl
primResizeMutableByteArray pr args
  | [Right _, Left x, Left y, Left rw] <- args
  = do !ref <- fromValueForce @(Ref UByteArray) x
       !len <- boxInt <$> fromValueForce y
       resTy <- resultType pr args

       ba <- liftIO $ do
         mba  <- BA.unsafeThawByteArray (boxByteArray (refValue ref))
         mba' <- BA.resizeMutableByteArray mba len
         BA.unsafeFreezeByteArray mba'

       let ref' = ref { refValue = VLiteral (ByteArrayLiteral ba) }
       toValue (UTuple2 (rw, ref')) resTy

  | otherwise
  = throwM (UnexpectedArgs pr args)

primShrinkMutableByteArray :: PrimImpl
primShrinkMutableByteArray pr args
  | [Right _, Left x, Left y, Left rw] <- args
  = do !ref <- fromValueForce @(Ref UByteArray) x
       !len <- boxInt <$> fromValueForce y
       mba <- liftIO $ BA.unsafeThawByteArray (boxByteArray (refValue ref))

       -- Attempting to shrink to a larger value is undefined.
       case compare len (BA.sizeofMutableByteArray mba) of
         GT -> throwM ResultUndefined

         _  -> do
           liftIO (BA.shrinkMutableByteArray mba len)
           ba <- liftIO (BA.unsafeFreezeByteArray mba)

           setRef (fromJust (refAddr ref)) (VLiteral (ByteArrayLiteral ba))
           pure rw

  | otherwise
  = throwM (UnexpectedArgs pr args)

primUnsafeFreezeByteArray :: PrimImpl
primUnsafeFreezeByteArray pr args
  | [Right _, Left x, Left y] <- args
  = do !ba <- fromValueForce @(Ref UByteArray) x
       resTy <- resultType pr args

       toValue (UTuple2 (y, ba)) resTy

  | otherwise
  = throwM (UnexpectedArgs pr args)

primSizeofByteArray :: PrimImpl
primSizeofByteArray pr args
  | [Left x] <- args
  = do !ba <- boxByteArray . refValue <$> fromValueForce x
       resTy <- resultType pr args

       toValue (UInt (BA.sizeofByteArray ba)) resTy

  | otherwise
  = throwM (UnexpectedArgs pr args)

primGetSizeofMutableByteArray :: PrimImpl
primGetSizeofMutableByteArray pr args
  | [Right _, Left x, Left rw] <- args
  = do !ba <- boxByteArray . refValue <$> fromValueForce x
       resTy <- resultType pr args

       size <- liftIO $ do
         mba <- BA.unsafeThawByteArray ba
         BA.getSizeofMutableByteArray mba

       toValue (UTuple2 (rw, UInt size)) resTy

  | otherwise
  = throwM (UnexpectedArgs pr args)

primCopyByteArray :: PrimImpl
primCopyByteArray pr args
  | [Right _, Left x, Left xO, Left y, Left yO, Left n, Left rw] <- args
  = do srcRef <- fromValueForce @(Ref UByteArray) x
       srcOff <- boxInt <$> fromValueForce xO
       dstRef <- fromValueForce @(Ref UByteArray) y
       dstOff <- boxInt <$> fromValueForce yO
       len <- boxInt <$> fromValueForce n

       -- Undefined if the src and dst are the same.
       when (refAddr srcRef == refAddr dstRef) (throwM ResultUndefined)

       -- Undefined if offset + length are out of bounds for either array.
       let src = boxByteArray (refValue srcRef)
           dst = boxByteArray (refValue dstRef)

       when (BA.sizeofByteArray src <= srcOff + len) (throwM ResultUndefined)
       when (BA.sizeofByteArray dst <= dstOff + len) (throwM ResultUndefined)

       -- Otherwise, copy from src into dst.
       dst' <- liftIO $ do
         mba <- BA.unsafeThawByteArray dst
         BA.copyByteArray mba dstOff src srcOff len
         BA.unsafeFreezeByteArray mba

       setRef (fromJust (refAddr dstRef)) (VLiteral (ByteArrayLiteral dst'))
       pure rw

  | otherwise
  = throwM (UnexpectedArgs pr args)

primSetByteArray :: PrimImpl
primSetByteArray pr args
  | [Right _, Left b, Left x, Left y, Left z, Left rw] <- args
  = do !ref <- fromValueForce @(Ref UByteArray) b
       !off <- boxInt <$> fromValueForce x
       !len <- boxInt <$> fromValueForce y
       !c <- boxInt <$> fromValueForce z

       ba <- liftIO $ do
         mba <- BA.unsafeThawByteArray (boxByteArray (refValue ref))
         BA.setByteArray mba off len c
         BA.unsafeFreezeByteArray mba

       -- The return type of setByteArray# is just State# s, so we need to
       -- update the ref here before returning that.
       --
       -- fromJust is safe here, we could only get this ref becuase it already
       -- existed in the heap.
       --
       setRef (fromJust (refAddr ref)) (VLiteral (ByteArrayLiteral ba))
       pure rw

  | otherwise
  = throwM (UnexpectedArgs pr args)

primIndex :: (Prim a, ToAst b) => (a -> b) -> PrimImpl
primIndex f pr args
  | [Left x, Left y] <- args
  = do !ba <- boxByteArray . refValue <$> fromValueForce x
       !ix <- boxInt <$> fromValueForce y
       resTy <- resultType pr args

       toValue (f (BA.indexByteArray ba ix)) resTy

  | otherwise
  = throwM (UnexpectedArgs pr args)

primRead :: (Prim a, ToAst b) => (a -> b) -> PrimImpl
primRead f pr args
  | [Right _, Left x, Left y, Left rw] <- args
  = do !ba <- boxByteArray . refValue <$> fromValueForce x
       !ix <- boxInt <$> fromValueForce y
       resTy <- resultType pr args

       res <- liftIO $ do
         mba <- BA.unsafeThawByteArray ba
         BA.readByteArray mba ix

       toValue (UTuple2 (rw, f res)) resTy

  | otherwise
  = throwM (UnexpectedArgs pr args)

primWrite :: (FromAst a, Prim b) => (a -> b) -> PrimImpl
primWrite f pr args
  | [Right _, Left x, Left y, Left z, Left rw] <- args
  = do !ref <- fromValueForce @(Ref UByteArray) x
       !ix  <- boxInt <$> fromValueForce y
       !val <- f <$> fromValueForce z

       !ba <- liftIO $ do
         mba <- BA.unsafeThawByteArray (boxByteArray (refValue ref))
         BA.writeByteArray mba ix val
         BA.unsafeFreezeByteArray mba

       setRef (fromJust (refAddr ref)) (VLiteral (ByteArrayLiteral ba))
       pure rw

  | otherwise
  = throwM (UnexpectedArgs pr args)
