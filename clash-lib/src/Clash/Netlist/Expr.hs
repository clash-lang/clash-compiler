{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd,
                    2017-2018, Google Inc.
                    2020-2022, QBayLogic B.V.
                    2022     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Functions for expression manipulation
-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Netlist.Expr where

import Control.Monad (zipWithM)
import Control.Exception (assert)
import Data.Set (fromList, member)
import Data.Bits (Bits, testBit, setBit, zeroBits)
import Data.Foldable (fold)
import Data.Tree (Tree(..))
import GHC.Stack (HasCallStack)
import Data.Text (unpack)
import Language.Haskell.TH.Quote (dataToPatQ)

import qualified Clash.Sized.Vector as V (replicate)
import qualified Clash.Sized.Internal.Index as I (fromInteger#)
import qualified Clash.Sized.Internal.Signed as S (fromInteger#)
import qualified Clash.Sized.Internal.Unsigned as U (fromInteger#)
import qualified Clash.Sized.Internal.BitVector as BV
  (high, low, fromInteger#, fromInteger##)
import GHC.Int (Int8(I8#), Int16(I16#), Int32(I32#), Int64(I64#))
import GHC.Word (Word8(W8#), Word16(W16#), Word32(W32#), Word64(W64#))

import Clash.Primitives.DSL (tySize)
import Clash.Netlist.Types
  ( Size, Bit(..), Expr(..), HWType(..), Literal(..), Modifier(..)
  , BlackBoxContext(..)
  )

-- | Turns a constant expression of known bitsize into their
-- corresponding bitstream representation, arranged as a tree
-- that corresponds to the structure of the expression.
--
-- NOTE: This conversion serves as a best effort approach and can be
-- considered a hack. Fully featured constant expression evaluation is
-- not available in clash yet and will replace this implementation
-- once it is officially supported.
bits :: HasCallStack => Size -> Expr -> Either Expr (Tree [Bool])
bits size expr = case expr of
  Literal _ lit -> case lit of
    BitLit bLit   -> case bLit of
      H -> leaf [True]
      L -> leaf [False]
      _ -> Left expr
    BoolLit bLit      -> leaf [bLit]
    NumLit nLit       -> leaf $ toBits size nLit
    BitVecLit _ bvLit -> leaf $ toBits size bvLit
    VecLit lits       ->
      mapM (bits (size `div` length lits) . lit2Expr) lits >>= inner
    StringLit{}       -> Left expr
  DataCon ty m subs -> assert (tySize ty == size) $ case ty of
    Vector s t      -> vecBits (tySize t) s subs
    Product _ _ tys -> zipWithM bits (map tySize tys) subs >>= inner
    Sum _ cs        -> spBits expr size m subs $ map (const []) cs
    SP _ xs         -> spBits expr size m subs $ map (map tySize . snd) xs
    _               -> case subs of
      [e] -> bits size e
      []  -> leaf []
      _   -> Left expr
  -- appears in case of complex transformations, e.g.,
  -- >>> (bv2v 0b010) :: Vec 3 Bit
  -- >>> (iterate (SNat @3) not True) :: Vec 3 Bool
  -- >>> (complement <$> (True :> False :> Nil)) :: Vec 2 Bool
  Identifier{} -> Left expr
  DataTag{} -> Left expr
  BlackBoxE bbName _ _ _ _ bbCtx _ -> case unpack bbName of
    $(dataToPatQ (const Nothing) $ show 'BV.low) -> leaf [False]
    $(dataToPatQ (const Nothing) $ show 'BV.high) -> leaf [True]
    $(dataToPatQ (const Nothing) $ show 'V.replicate) -> case bbInputs bbCtx of
      [ (eSize, ty, _), (eValue, _, _) ] -> do
        bs <- bits (tySize ty) eSize
        let s = fromBits $ fold bs
        v <- bits (size `div` s) eValue
        inner $ replicate s v
      _ -> Left expr
    _ ->
      if unpack bbName `member` skippableBBs
      then skippableBBBits expr bbCtx size
      else Left expr
  ToBv _ _ e -> bits size e
  FromBv _ _ e -> bits size e
  IfThenElse cond match alt -> case bits 1 cond of
    Right (Node [True] [])  -> bits size match
    Right (Node [False] []) -> bits size alt
    _ -> Left expr
  Noop -> leaf []

 where
  -- known skippable blackboxes
  skippableBBs = fromList $ map show
    [ 'I.fromInteger#, 'S.fromInteger#, 'U.fromInteger#
    , 'BV.fromInteger#, 'BV.fromInteger##
    , 'I8#, 'I16#, 'I32#, 'I64#
    , 'W8#, 'W16#, 'W32#, 'W64#
    ]

  -- skips the blackbox conversion and obtains the constant result
  -- directly from the last input argument instead
  skippableBBBits e Context{..} n = case reverse bbInputs of
    (x, _, _) : _ -> bits n x
    _             -> Left e

  -- turns sum (& product) expressions into bitstreams (preserving the
  -- expressions' tree layout)
  spBits :: Expr -> Size -> Modifier -> [Expr] -> [[Size]]
         -> Either Expr (Tree [Bool])
  spBits e n m es sizes = case m of
    DC (_, i) -> do
      xs <- zipWithM bits (sizes !! i) es
      bs <- fold <$> inner xs
      l <- leaf $ toBits (n - length bs) i
      r <- leaf bs
      inner [ l, r ]
    _ -> Left e

  -- turns vector expressions into bitstream (preserving the
  -- expressions' tree layout)
  vecBits :: Size -> Int -> [Expr] -> Either Expr (Tree [Bool])
  vecBits elemSize elems = \case
    []   -> assert (elems == 0) $ leaf []
    x:xr -> assert (elems > 0) $ do
      (processedElems, cur) <- case x of
        DataCon t _ xs -> case t of
          Vector subElems (tySize -> subTySize) ->
            assert (subElems <= elems && subTySize == elemSize)
              ((subElems,) <$> vecBits elemSize subElems xs)
          _ -> (1,) <$> bits elemSize x
        _ -> (1,) <$> bits elemSize x
      sub <- vecBits elemSize (elems - processedElems) xr
      inner [cur, sub]

  -- creates a leaf node holding the leaf value
  leaf :: [a] -> Either b (Tree [a])
  leaf x = return $ Node x []

  -- creates an inner node (holding no value) with the given
  -- sub-trees
  inner :: [Tree [a]] -> Either b (Tree [a])
  inner = return . Node []

  -- turns a literal into an expression
  lit2Expr = Literal Nothing

-- | Turns values into bitstreams of known length. If the bit stream
-- requires more bits for representing the given value, then only the
-- suffix of the corresponding bitstream gets returned.
toBits :: Bits a => Int -> a -> [Bool]
toBits n x =
  map (testBit x) [n-1,n-2..0]

-- | Turns bitstreams into values.
fromBits :: Bits a => [Bool] -> a
fromBits xs =
  foldl setBit zeroBits $ map snd $ filter fst $ zip xs [n-1,n-2..0]
 where
  n = length xs
