{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.ByteArray
  ( byteArrayPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Primitive.ByteArray as BA
import Data.Text (Text)
import System.IO.Unsafe

import Clash.Core.Evaluator.Models
import Clash.Core.Term

import Clash.GHC.PartialEval.Internal

import Clash.Debug

byteArrayPrims :: HashMap Text PrimImpl
byteArrayPrims = HashMap.fromList
  [ ("GHC.Prim.newByteArray#", primNewByteArray)
  , ("GHC.Prim.setByteArray#", tryMe)
  , ("GHC.Prim.writeWordArray#", tryMe)
  , ("GHC.Prim.unsafeFreezeByteArray#", primUnsafeFreezeByteArray)
  , ("GHC.Prim.sizeofByteArray#", primSizeofByteArray)
  , ("GHC.Prim.getSizeofMutableByteArray#", tryMe)
  , ("GHC.Prim.indexWordArray#", tryMe)
  , ("GHC.Prim.resizeMutableByteArray#", tryMe)
  , ("GHC.Prim.shrinkMutableByteArray#", tryMe)
  , ("GHC.Prim.copyByteArray#", tryMe)
  , ("GHC.Prim.readWordArrray#", tryMe)
  ]

tryMe :: PrimImpl
tryMe e p args =
  traceM (show (primName p) <> ": " <> show args) >> liftId e p args

primNewByteArray :: PrimImpl
primNewByteArray e p args
  | [Right _s, Left x, Left y] <- args
  = do size <- boxInt <$> fromTermOrValue e x
       resTy <- resultType p args
       rw <- lift (evaluateWhnf e y)
       let ba = unsafeDupablePerformIO $ do
                  mba <- BA.newByteArray size
                  BA.unsafeFreezeByteArray mba

       toValue (UTuple2 (rw, Ref Nothing (UByteArray ba))) resTy

  | otherwise
  = empty

primUnsafeFreezeByteArray :: PrimImpl
primUnsafeFreezeByteArray e p args
  | [Right _s, Left x, Left y] <- args
  = do !ba <- fromTermOrValue @(Ref UByteArray) e x
       !rw <- lift (evaluateWhnf e y)

       resTy <- resultType p args

       toValue (UTuple2 (rw, ba)) resTy

  | otherwise
  = empty

primSizeofByteArray :: PrimImpl
primSizeofByteArray e p args
  | [Left x] <- args
  = do !ba <- boxByteArray . refValue <$> fromTermOrValue @(Ref UByteArray) e x
       resTy <- resultType p args

       toValue (UInt (BA.sizeofByteArray ba)) resTy

  | otherwise
  = empty

