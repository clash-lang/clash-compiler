{-|
Copyright  :  (C) 2021-2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

module Clash.Explicit.BlockRam.Internal where

import Data.Bits ((.&.), (.|.), shiftL, xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder (Builder, toLazyByteString, word8, word64BE)
import qualified Data.ByteString.Unsafe as B
import Data.Foldable (foldl')
import Data.Word (Word64)
import GHC.Exts (Addr#)
import GHC.TypeLits (KnownNat, Nat)
import Numeric.Natural (Natural)
import System.IO.Unsafe (unsafePerformIO)

import Clash.Promoted.Nat (natToNum)
import Clash.Sized.Internal.BitVector (Bit(..), BitVector(..))

-- | Efficient storage of memory content
--
-- It holds @n@ words of @'BitVector' m@.
data MemBlob (n :: Nat) (m :: Nat) where
  MemBlob
    :: ( KnownNat n
       , KnownNat m
       )
    => { memBlobRunsLen :: !Int
       , memBlobRuns :: Addr#
       , memBlobEndsLen :: !Int
       , memBlobEnds :: Addr#
       }
    -> MemBlob n m

instance Show (MemBlob n m) where
  showsPrec _ x@MemBlob{} =
    ("$(memBlobTH @" ++) . shows (natToNum @m @Int) . (" Nothing " ++) .
      shows (unpackMemBlob x) . (')':)

-- | Convert a 'MemBlob' back to a list
--
-- __NB__: Not synthesizable
unpackMemBlob
  :: forall n m
   . MemBlob n m
  -> [BitVector m]
unpackMemBlob = unsafePerformIO . unpackMemBlob0

unpackMemBlob0
  :: forall n m
   . MemBlob n m
  -> IO [BitVector m]
unpackMemBlob0 MemBlob{..} = do
  runsB <- B.unsafePackAddressLen memBlobRunsLen memBlobRuns
  endsB <- B.unsafePackAddressLen memBlobEndsLen memBlobEnds
  return $ map (BV 0) $
    unpackNats (natToNum @n) (natToNum @m) runsB endsB

packBVs
  :: forall m f
   . ( Foldable f
     , KnownNat m
     )
  => Maybe Bit
  -> f (BitVector m)
  -> Either String (Int, L.ByteString, L.ByteString)
packBVs care es =
  case lenOrErr of
    Nothing  -> Left err
    Just len -> let (runs, ends) = packAsNats mI knownBVVal es
                in Right (len, runs, ends)
 where
  lenOrErr = case care of
               Just (Bit 0 _) -> Just $ length es
               _              -> foldl' lenOrErr0 (Just 0) es
  lenOrErr0 (Just len) (BV 0 _) = Just $ len + 1
  lenOrErr0 _          _        = Nothing

  knownBVVal bv@(BV _ val) = case care of
    Just (Bit 0 bm) -> maskBVVal bm bv
    _               -> val

  maskBVVal _ (BV 0    val) = val
  maskBVVal 0 (BV mask val) = val .&. (mask `xor` fullMask)
  maskBVVal _ (BV mask val) = val .|. mask

  mI = natToNum @m @Int
  fullMask = (1 `shiftL` mI) - 1
  err = "packBVs: cannot convert don't care values. " ++
        "Please specify a mapping to a definite value."

packAsNats
  :: forall a f
   . Foldable f
  => Int
  -> (a -> Natural)
  -> f a
  -> (L.ByteString, L.ByteString)
packAsNats width trans es = (toLazyByteString runs0, toLazyByteString ends)
 where
  (runL, endL) = width `divMod` 8
  ends | endC0 > 0 = word64BE endA0 <> ends0
       | otherwise = ends0
  (runs0, ends0, endC0, endA0) = foldr pack0 (mempty, mempty, 0, 0) es

  pack0 :: a -> (Builder, Builder, Int, Word64) ->
           (Builder, Builder, Int, Word64)
  pack0 val (runs1, ends1, endC1, endA1) =
    let (ends2, endC2, endA2) = packEnd val2 ends1 endC1 endA1
        (val2, runs2) = packRun runL (trans val) runs1
    in (runs2, ends2, endC2, endA2)

  packRun :: Int -> Natural -> Builder -> (Natural, Builder)
  packRun 0    val1 runs1 = (val1, runs1)
  packRun runC val1 runs1 = let (val2, runB) = val1 `divMod` 256
                                runs2 = word8 (fromIntegral runB) <> runs1
                            in packRun (runC - 1) val2 runs2

  packEnd :: Natural -> Builder -> Int -> Word64 -> (Builder, Int, Word64)
  packEnd val2 ends1 endC1 endA1
    | endL == 0   = (ends1, endC1, endA1)
    | endC2 <= 64 = let endA2 = endA1 * (2 ^ endL) + valEnd
                    in (ends1, endC2, endA2)
    | otherwise   = let ends2 = word64BE endA1 <> ends1
                    in (ends2, endL, valEnd)
   where
    endC2 = endC1 + endL
    valEnd = fromIntegral val2

unpackNats
  :: Int
  -> Int
  -> B.ByteString
  -> B.ByteString
  -> [Natural]
unpackNats 0 _ _ _ = []
unpackNats len width runBs endBs
  | width < 8 = ends
  | otherwise = go (head ends) runL runBs (tail ends)
 where
  (runL, endL) = width `divMod` 8
  ends = if endL == 0 then
           repeat 0
         else
           unpackEnds endL len $ unpackW64s endBs

  go val 0    runBs0 ~(end0:ends0) = val : go end0 runL runBs0 ends0
  go _   _    runBs0 _             | B.null runBs0 = []
  go val runC runBs0 ends0
    = let Just (runB, runBs1) = B.uncons runBs0
          val0 = val * 256 + fromIntegral runB
      in go val0 (runC - 1) runBs1 ends0

unpackW64s
  :: B.ByteString
  -> [Word64]
unpackW64s = go 8 0
 where
  go :: Int -> Word64 -> B.ByteString -> [Word64]
  go 8 _   endBs | B.null endBs = []
  go 0 val endBs = val : go 8 0 endBs
  go n val endBs = let Just (endB, endBs0) = B.uncons endBs
                       val0 = val * 256 + fromIntegral endB
                   in go (n - 1) val0 endBs0

unpackEnds
  :: Int
  -> Int
  -> [Word64]
  -> [Natural]
unpackEnds _    _   []     = []
unpackEnds endL len (w:ws) = go endCInit w ws
 where
  endPerWord = 64 `div` endL
  leader = len `mod` endPerWord
  endCInit | leader == 0 = endPerWord
           | otherwise   = leader

  go 0 _    []       = []
  go 0 _    (w0:ws0) = go endPerWord w0 ws0
  go n endA ws0      = let (endA0, valEnd) = endA `divMod` (2 ^ endL)
                       in fromIntegral valEnd : go (n - 1) endA0 ws0
