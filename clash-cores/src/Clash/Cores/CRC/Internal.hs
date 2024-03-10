{-|
  Copyright   :  (C) 2024, Rowan Goemans <goemansrowan@gmail.com>
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Clash.Cores.CRC.Internal where

import           Clash.Prelude
import           Clash.Class.HasDomain       (TryDomain, TryDomainResult(NotFound))
import qualified Clash.Sized.Vector as V     (toList)
import qualified Data.List as L
import qualified Language.Haskell.TH as TH
import           Data.Maybe

import           Data.Constraint             ((:-)(..), Dict (..))
import           Data.Constraint.Nat         (leTrans)
import           Unsafe.Coerce               (unsafeCoerce)

-- | Contains all the parameters to generate a CRC implementation
data CRCParams (crcWidth :: Nat) (dataWidth :: Nat)
  = CRCParams
      { _crcWidth         :: SNat crcWidth
      -- ^ Bit width of CRC word. Also known as "width" in the Williams model.
      , _crcDataWidth     :: SNat dataWidth
      -- ^ Bit width of data words
      , _crcPolynomial    :: BitVector crcWidth
      -- ^ CRC polynomial to use, without the implicit
      --   @x^crc_width@ term. Polynomial is always specified with the highest
      --   order terms in the most significant bit positions; use
      --   `_crcReflectInput` and `_crcReflectOutput` to perform a least
      --   significant bit first computation.
      , _crcInitial       :: BitVector crcWidth
      -- ^ Initial value of CRC register at reset. Most significant bit always
      --   corresponds to the highest order term in the CRC register.
      , _crcReflectInput  :: Bool
      -- ^ If True, the input data words are bit-reflected, so that they are
      --   processed least significant bit first.
      , _crcReflectOutput :: Bool
      -- ^ If True, the output CRC is bit-reflected, so the least-significant bit
      --   of the output is the highest-order bit of the CRC register.
      --   Note that this reflection is performed over the entire CRC register;
      --   for transmission you may want to treat the output as a little-endian
      --   multi-word value, so for example the reflected 16-bit output 0x4E4C
      --   would be transmitted as the two octets 0x4C 0x4E, each transmitted
      --   least significant bit first.
      , _crcXorOutput     :: BitVector crcWidth
      -- ^ The output CRC will be the CRC register XOR'd with this value, applied
      --   after any output bit-reflection.
      }
  deriving (Generic, Show, ShowX, Lift)

-- | No domain in `CRCParams`
type instance TryDomain t (CRCParams crcWidth dataWidth) = 'NotFound

-- | Allow one to compute values for the CRC.
--
-- __N.B.__: NOT for use in hardware.
data SoftwareCRC (crcWidth :: Nat) (dataWidth :: Nat)
  = SoftwareCRC
      { _crcParams      :: CRCParams crcWidth dataWidth
      , _crcTopBitMask  :: BitVector (crcWidth + dataWidth)
      , _crcpolyShifted :: BitVector (crcWidth + dataWidth)
      , _crcCurrent     :: BitVector (crcWidth + dataWidth)
      }
  deriving (Generic, Show, ShowX)

-- | No domain in `SoftwareCRC`
type instance TryDomain t (SoftwareCRC crcWidth dataWidth) = 'NotFound

-- | Apply function n times over input.
nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes n f x = nTimes (n - 1) f (f x)

-- | Apply function only when bool is True.
applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f x = f x
applyWhen False _ x = x

-- | Reverse bitvector.
reverseBV :: KnownNat n => BitVector n -> BitVector n
reverseBV = v2bv . reverse . bv2v

-- Implementation notes:
-- We always compute most-significant bit first, which means the
-- polynomial and initial value may be used as-is, and the `_crcReflectInput`
-- and `_crcReflectOutput` values have their usual sense.
-- However, when computing word-at-a-time and MSbit-first, we must align
-- the input word so its MSbit lines up with the MSbit of the previous
-- CRC value. When the CRC width is smaller than the word width, this
-- would normally truncate data bits.
-- Instead, we shift the initial CRC left by the data width, and the
-- data word left by the crc width, lining up their MSbits no matter
-- the relation between the two widths.
-- The new CRC is then shifted right by the data width before output.

-- | Create a `SoftwareCRC` from `CRCParams`.
--
-- __N.B.__: NOT for use in hardware.
mkSoftwareCRC
  :: forall (crcWidth :: Nat) (dataWidth :: Nat)
   . CRCParams crcWidth dataWidth
  -> SoftwareCRC crcWidth dataWidth
mkSoftwareCRC _crcParams@CRCParams{..} = go _crcParams
  where
    go (CRCParams SNat SNat _ _ _ _ _) = reset $ SoftwareCRC { .. }
      where
        combSNat = addSNat _crcWidth _crcDataWidth
        _crcTopBitMask = shiftL 1 (snatToNum combSNat - 1)
        _crcpolyShifted = shiftL (extend _crcPolynomial) (snatToNum _crcDataWidth)
        _crcCurrent = 0

-- | Reset the `SoftwareCRC`. If you want to re-use it for multiple messages
--   You need to reset it in-between messages.
reset
  :: forall (crcWidth :: Nat) (dataWidth :: Nat)
   . SoftwareCRC crcWidth dataWidth
  -- ^ Current CRC engine.
  -> SoftwareCRC crcWidth dataWidth
  -- ^ CRC engine fresh to be used.
reset engine@(SoftwareCRC {..}) = go _crcParams
  where
    CRCParams {..} = _crcParams
    go (CRCParams SNat SNat _ _ _ _ _) = engine { _crcCurrent = initialCRC }
      where
        initial :: BitVector (crcWidth + dataWidth)
        initial = extend _crcInitial
        initialCRC = shiftL initial (snatToNum _crcDataWidth)

-- | Feed a word to the `SoftwareCRC`.
feed
  :: forall (crcWidth :: Nat) (dataWidth :: Nat)
   . SoftwareCRC crcWidth dataWidth
  -- ^ Current `SoftwareCRC`.
  -> BitVector dataWidth
  -- ^ Update `SoftwareCRC` with this input.
  -> SoftwareCRC crcWidth dataWidth
  -- ^ `SoftwareCRC` with update processed.
feed engine@(SoftwareCRC {..}) word = go _crcParams
  where
    CRCParams {..} = _crcParams
    go (CRCParams SNat SNat _ _ _ _ _) = engine { _crcCurrent = nextCrc }
      where
        applyPolyBit crcB
          | crcB .&. _crcTopBitMask > 0 = (shiftL crcB 1) `xor` _crcpolyShifted
          | otherwise                   = shiftL crcB 1
        applyPoly crcW = nTimes (snatToNum _crcDataWidth) applyPolyBit crcW
        applyCrc crc w = applyPoly $ crc `xor` (shiftL (extend w) (snatToNum _crcWidth))
        nextCrc = applyCrc _crcCurrent $ applyWhen _crcReflectInput reverseBV word

-- | Get current CRC state. This is the value before any output reflection and output XOR
getCRCState
  :: forall (crcWidth :: Nat) (dataWidth :: Nat)
   . SoftwareCRC crcWidth dataWidth
  -> BitVector crcWidth
getCRCState (SoftwareCRC {..}) = go _crcParams
  where
    go (CRCParams SNat SNat _ _ _ _ _) = fst $ split _crcCurrent

-- | Generate the CRC
--   If the `SoftwareCRC` will be re-used `reset` must be called before starting
--   to process the next message.
digest
  :: forall (crcWidth :: Nat) (dataWidth :: Nat)
   . SoftwareCRC crcWidth dataWidth
  -> BitVector crcWidth
digest params@(SoftwareCRC {..}) = go _crcParams
  where
    go (CRCParams SNat SNat _ _ _ _ _) = outputCrcRev `xor` _crcXorOutput
      where
        CRCParams {..} = _crcParams
        outputCrcRev = applyWhen _crcReflectOutput reverseBV $ getCRCState params

-- | Compute the raw residue value for these `CRCParams`, which is the value left in the
--   CRC register after processing any valid codeword. Raw in this case means
--   this is the residue before the output reflection and out xor has been applied.
--
--   Upto @dataWidth@ padding can be given. This is useful during CRC verification
--   in the scenario where where you want to handle more words in a single cycle
--   then is native to the protocol.
--
--   For example ethernet uses a 32-bit CRC, you may want a data width of 32-bit
--   To achieve higher throughput. But Ethernet is byte oriented.
--   This means that it is possible for 8, 16, 24 or 32-bit in the last data
--   word of the stream to be valid.
--   We can calculate a residue for each case and compare it to the CRC result.
--   This saves us from having to instantiate four seperate CRCs for the receive path.
--   The invalid words must be set to zero.
rawResidue
  :: forall (crcWidth :: Nat) (dataWidth :: Nat)
   . CRCParams crcWidth dataWidth
  -- ^ CRC engine
  -> Index dataWidth
  -- ^ How many invalid(zero) bits the stream is padded with
  -> BitVector crcWidth
  -- ^ The residue
rawResidue params@(CRCParams {..}) nExtra = go params
  where
    go (CRCParams SNat SNat _ _ _ _ _) = raw
      where
        bitParams = params { _crcDataWidth = d1 }
        crcVal = applyWhen _crcReflectOutput reverse $ bv2v $ digest $ mkSoftwareCRC bitParams
        bitsToFeed = fmap (v2bv . reverse . singleton) $
                       (V.toList crcVal) L.++ (L.replicate (fromIntegral nExtra) 0)
        raw = getCRCState $ L.foldl' feed (mkSoftwareCRC bitParams) bitsToFeed

-- | Compute the residue value for these CRCParam, which is the value left in the
--   CRC register after processing any valid codeword.
residue
  :: forall (crcWidth :: Nat) (dataWidth :: Nat)
   . CRCParams crcWidth dataWidth
  -- ^ CRC engine
  -> BitVector crcWidth
  -- ^ The residue
residue params@(CRCParams {..}) = go params
  where
    go (CRCParams SNat SNat _ _ _ _ _) = applyWhen _crcReflectOutput reverseBV $ rawResidue params 0

-- | The @F@ and @G@ Matrices for parallel CRC computation, treating
--   the CRC as a linear time-invariant system described by the state
--   relation @x(t+1) = F.x(i) + G.u(i)@, where
--
--     - @x(i)@ and @u(i)@ are column vectors of the bits of the CRC register and input word
--
--     - @F@ is the n-by-n matrix relating the old state to the new state
--
--     - @G@ is the n-by-m matrix relating the new data to the new state
--
--     - @n@ is the CRC width
--
--     - @m@ is the data word width.
--
--   The matrix is used to select which bits are XORd together to compute
--   each bit @i@ of the new state. If @F[i][j]@ is set then bit @j@ of the
--   @old state@ is included in the XOR. If @G[i][j]@ is set then bit @j@ of the
--   @input@ is included in the XOR
--
--   These matrices are not affected by `_crcInitial`, `_crcReflectOutput` or
--   `_crcXorOutput`.
data FGMatrices (crcWidth :: Nat) (dataWidth :: Nat)
  = FGMatrices
      { _fMat :: Vec crcWidth (BitVector crcWidth)
      , _gMat :: Vec crcWidth (BitVector dataWidth)
      }
  deriving (Show, Lift)

-- | No domain in `FGMatrices`
type instance TryDomain t (FGMatrices crcWidth dataWidth) = 'NotFound

-- | Compute the `FGMatrices` from `CRCParams`.
mkFGMatrices
  :: forall (crcWidth :: Nat) (dataWidth :: Nat)
   . CRCParams crcWidth dataWidth
  -> FGMatrices crcWidth dataWidth
mkFGMatrices params@CRCParams{..} = go params
  where
    go (CRCParams SNat SNat _ _ _ _ _) = FGMatrices f g
      where
        newParams = params
                      { _crcReflectOutput = False
                      , _crcXorOutput = 0
                      }
        withInitCRC i = mkSoftwareCRC $ newParams { _crcInitial = i }
        runCrc initial dat = digest $ feed (withInitCRC initial) dat
        onehots n = (shiftL 1) . fromIntegral <$> (indices n)
        postProcess x = v2bv . reverse <$> transpose x
        f = postProcess $ bv2v . (\p -> runCrc p 0) <$> onehots _crcWidth
        g = postProcess $ bv2v . (\p -> runCrc 0 p) <$> onehots _crcDataWidth

-- | Vertically partition a matrix.
partitionMatrix
  :: KnownNat q
  => SNat p
  -> Vec n (BitVector (p * q))
  -> Vec n (Vec p (BitVector q))
partitionMatrix SNat mat = fmap (fmap v2bv . unconcatI . bv2v) mat

-- | Flatten vertically partition matrix.
unPartitionMatrix
  :: KnownNat q
  => SNat p
  -> Vec n (Vec p (BitVector q))
  -> Vec n (BitVector (p * q))
unPartitionMatrix SNat mat = fmap (v2bv . concat . fmap bv2v) mat

-- | Helper type that contains the flattened @F@ @G@ matrices for each lane.
data CRCLaneParams (crcWidth :: Nat) (dataWidth :: Nat) (nLanes :: Nat) where
  -- | The final lane.
  CRCLane1
    :: Vec crcWidth (BitVector (crcWidth + dataWidth))
    -- ^ Flattened @F|G@ matrix for this lane.
    -> CRCLaneParams crcWidth dataWidth 1
  -- | Not the final lane.
  CRCLaneN
    :: 2 <= lane
    => SNat lane
    -- ^ Which lane this is
    -> Vec crcWidth (BitVector (crcWidth + lane * dataWidth))
    -- ^ Flattened @F|G@ matrix for this lane.
    -> CRCLaneParams crcWidth dataWidth (lane - 1)
    -> CRCLaneParams crcWidth dataWidth lane

deriving instance (KnownNat crcWidth, KnownNat dataWidth, KnownNat nLanes)
  => Lift (CRCLaneParams crcWidth dataWidth nLanes)

deriving instance (KnownNat crcWidth, KnownNat dataWidth, KnownNat nLanes)
  => Show (CRCLaneParams crcWidth dataWidth nLanes)

-- | No domain in `CRCLaneParams`
type instance TryDomain t (CRCLaneParams crcWidth dataWidth nLanes) = 'NotFound

-- | Contains all necessary parameters for the hardware CRC implementation.
--
-- __N.B.__: This __MUST__ be constructed at compile time with
--           `mkParallelCRCParamsTH` using Template Haskell.
data ParallelCRCParams (crcWidth :: Nat) (dataWidth :: Nat) (nLanes :: Nat)
  = ParallelCRCParams
      { _crcNlanes :: SNat nLanes
      , _crcBaseParams :: CRCParams crcWidth dataWidth
      , _crcLaneParams :: CRCLaneParams crcWidth dataWidth nLanes
      , _crcResidues :: Vec nLanes (BitVector crcWidth)
      }
  deriving (Show, Lift)

-- | No domain in `ParallelCRCParams`
type instance TryDomain t (ParallelCRCParams crcWidth dataWidth nLanes) = 'NotFound

-- | Similar to `compareSNat` but splits into Lt, Eq and Gt instead of Le and Eq.
data SNatOrdering a b where
  SNatLT2 :: forall a b. a <= (b - 1) => SNatOrdering a b
  SNatEQ2 :: forall a b. a ~ b => SNatOrdering a b
  SNatGT2 :: forall a b. (b + 1) <= a => SNatOrdering a b

-- | No domain in `SNatOrdering`
type instance TryDomain t (SNatOrdering a b) = 'NotFound

-- | Get an ordering relation between two SNats.
compareSNat2 :: forall a b . SNat a -> SNat b -> SNatOrdering a b
compareSNat2 a b
  = case compare (snatToInteger a) (snatToInteger b) of
      LT -> unsafeCoerce (SNatLT2 @0 @1)
      EQ -> unsafeCoerce (SNatEQ2 @0 @0)
      GT -> unsafeCoerce (SNatGT2 @1 @0)

-- | Flattens the @F@ and @G@ matrices into a single @F|G@ matrix.
flattenFGMatrices
  :: KnownNat dataWidth
  => FGMatrices crcWidth dataWidth
  -> Vec crcWidth (BitVector (crcWidth + dataWidth))
flattenFGMatrices (FGMatrices f g) = zipWith (++#) f g

-- | Compute CRC lane parameters given `CRCParams` and the number of lanes.
mkCRCLaneParams
  :: 1 <= nLanes
  => CRCParams crcWidth dataWidth
  -> SNat nLanes
  -> CRCLaneParams crcWidth dataWidth nLanes
mkCRCLaneParams params@(CRCParams SNat SNat _ _ reflectInput _ _) nLanes@SNat
  = case compareSNat2 d1 nLanes of
      SNatEQ2 -> CRCLane1 (flattenFGMatrices $ mkFGMatrices params)
      SNatLT2 -> CRCLaneN nLanes fg (mkCRCLaneParams params $ subSNat nLanes d1)
        where
          params' = params { _crcDataWidth = mulSNat nLanes (_crcDataWidth params) }
          (FGMatrices f g) = mkFGMatrices params'
          reverseLanes = unPartitionMatrix nLanes . fmap reverse . partitionMatrix nLanes
          fg = flattenFGMatrices $ FGMatrices f (applyWhen reflectInput reverseLanes g)
      _ -> clashCompileError "mkCRCLaneParams: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

-- | Get the flattened @F|G@ Matrix for a given lane
getFGMatrix
  :: lane <= lanes
  => 1 <= lane
  => CRCLaneParams crcWidth dataWidth lanes
  -> SNat lane
  -> Vec crcWidth (BitVector (crcWidth + lane * dataWidth))
getFGMatrix (CRCLane1 fg) lane
  = case compareSNat2 d1 lane of
      SNatEQ2 -> fg
      _ -> clashCompileError "getFGMatrix, CRCLane1: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
getFGMatrix (CRCLaneN lanes fg next) lane
  = case compareSNat2 lane lanes of
      SNatLT2 -> getFGMatrix next lane
      SNatEQ2 -> fg
      _ -> clashCompileError "getFGMatrix, CRCLaneN: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

-- | Construct parameters for a parallel hardware CRC implementation
--
--   @nLanes@ indicates the maximum number of @dataWidth@ words you want to
--   process in a single cycle. For example the stream could be byte-oriented,
--   but processing is done @n@-bytes at a time.
mkParallelCRCParams
  :: forall (crcWidth :: Nat)
            (dataWidth :: Nat)
            (nLanes :: Nat)
   . 1 <= nLanes
  => CRCParams crcWidth dataWidth
  -- ^ The CRC to instantiate parallel implementation for
  -> SNat nLanes
  -- ^ The number of lanes.
  -> ParallelCRCParams crcWidth dataWidth nLanes
mkParallelCRCParams params@(CRCParams _ dataWidth@SNat _ _ _ _ _) nLanes@SNat
  = parallelParams
    where
      paramsFullWidth = params { _crcDataWidth = mulSNat nLanes dataWidth }
      computeResidue = rawResidue paramsFullWidth . fromIntegral . (snatToNum dataWidth *) . toInteger

      parallelParams = ParallelCRCParams
        { _crcNlanes = nLanes
        , _crcBaseParams = params
        , _crcLaneParams = mkCRCLaneParams params nLanes
        , _crcResidues = fmap computeResidue $ reverse indicesI
        }

-- | Construct parameters for a parallel hardware CRC implementation
--
--   @nLanes@ indicates the maximum number of @dataWidth@ words you want to
--   process in a single cycle. For example the stream could be byte-oriented,
--   but processing is done @n@-bytes at a time.
mkParallelCRCParamsTH
  :: forall (crcWidth :: Nat)
            (dataWidth :: Nat)
            (nLanes :: Nat)
   . 1 <= nLanes
  => CRCParams crcWidth dataWidth
  -- ^ The CRC to instantiate parallel implementation for
  -> SNat nLanes
  -- ^ The number of lanes.
  -> TH.ExpQ
mkParallelCRCParamsTH params@(CRCParams SNat SNat _ _ _ _ _) nLanes@SNat
  = lift $ mkParallelCRCParams params nLanes

-- | Matrix multiplication in the binary finite field
matVecMul
  :: forall (m :: Nat)
            (n :: Nat)
   . KnownNat m
  => KnownNat n
  -- ^ @n@ columns
  => Vec m (BitVector n)
  -- ^ The Matrix
  -> BitVector n
  -- ^ The column vector
  -> BitVector m
  -- ^ The resulting vector
matVecMul mat vec = v2bv $ fmap (\row -> reduceXor $ vec .&. row) mat

-- | Like `subSNat` but uses a `Data.Type.Ord.<=` constraint
subSNatLe
  :: forall (n :: Nat)
            (m :: Nat)
   . m <= n
  => SNat n
  -> SNat m
  -> SNat (n - m)
subSNatLe a b = leToPlus @m @n $ subSNat a b

-- | Like `last` but uses a `Data.Type.Ord.<=` constraint
lastLe
  :: forall (n :: Nat)
            (a :: Type)
   . 1 <= n
  => Vec n a
  -- ^ input vector
  -> a
lastLe vs = leToPlus @1 @n $ last vs

-- | Like `take` but uses a `Data.Type.Ord.<=` constraint
takeLe
  :: forall (n :: Nat)
            (m :: Nat)
            (a :: Type)
   . n <= m
  => SNat n
  -- ^ How many elements to take
  -> Vec m a
  -- ^ input vector
  -> Vec n a
takeLe SNat vs = leToPlus @n @m $ takeI vs

-- | Like `drop` but uses a `Data.Type.Ord.<=` constraint
dropLe
  :: forall (n :: Nat)
            (m :: Nat)
            (a :: Type)
   . n <= m
  => KnownNat m
  => SNat n
  -- ^ How many elements to drop
  -> Vec m a
  -- ^ input vector
  -> Vec (m - n) a
dropLe SNat vs = leToPlus @n @m $ dropI vs

-- | Get slice between index @p@ and @q@
sliceVec
  :: forall (p :: Nat)
            (q :: Nat)
            (n :: Nat)
            (a :: Type)
   . p <= q
  => q <= n
  => KnownNat n
  => SNat p
  -- ^ Index where the slice starts
  -> SNat q
  -- ^ Index where the slice ends
  -> Vec n a
  -- ^ Input vector
  -> Vec (q - p) a
sliceVec p q vs =
  case leTrans @p @q @n of
    Sub Dict -> takeLe (subSNat q p) $ dropLe p vs

-- | Apply a step to a CRC and input given by @lanePrev@
--   This function crashes if called with out of bounds @lanePrev@
--   meant to be used via `smap`
laneStep
  :: ParallelCRCParams crcWidth dataWidth nLanes
  -> SNat lanePrev
  -> ()
  -> BitVector crcWidth
  -> Vec nLanes (BitVector dataWidth)
  -> BitVector crcWidth
laneStep (ParallelCRCParams nLanes@SNat (CRCParams SNat SNat _ _ _ _ _) laneParams _) lanePrev@SNat _ crc input
  = let lane = addSNat lanePrev d1 in case compareSNat lane nLanes of
    SNatLE -> matVecMul (getFGMatrix laneParams lane) (crc ++# (pack $ takeLe lane input))
    _      -> clashCompileError "lookupLaneStep: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

-- | A parallel multilane CRC engine and validator
--
-- They can be configured using `ParallelCRCParams`,
-- which can be constructed with `mkParallelCRCParams`.
--
-- @dataWidth@ should always cleanly divide @crcWidth@. If this is not the case
-- then it is possible for the last message to be misaligned.
--
-- For example if you have @dataWidth ~ 48@, @crcWidth ~ 32@. Then a possible
-- message could be 120-bits, two words + CRC. But this cannot be safely
-- ran through the CRC circuit. Because the final fragment contains 24 valid bits
-- and 24 invalid bits.
--
-- This rule is __NOT__ checked at the type level because this is fine,
-- as long you know for sure such a misalignment does not occur.
--
crcEngine
  :: forall (dom :: Domain)
            (nLanes :: Nat)
            (crcWidth :: Nat)
            (dataWidth :: Nat)
   . HiddenClockResetEnable dom
  => 1 <= nLanes
  => ParallelCRCParams crcWidth dataWidth nLanes
  -- ^ The CRC to instantiate this circuit for.
  -> Signal dom (Maybe (Index nLanes, Vec nLanes (BitVector dataWidth)))
  -- ^ The input data. @Index nLanes@ indicates how many @dataWidth@ words are
  --   valid in the vector minus 1.
  --
  --     - Ex. 1. @Just (0, 0xDE :> 0xAD :> 0xBE :> 0xEF :> Nil)@ means only the first word @0xDE@ is valid
  --     - Ex. 2. @Just (3, 0xDE :> 0xAD :> 0xBE :> 0xEF :> Nil@ means all words in the vector are valid
  --
  -> Signal dom (BitVector crcWidth)
  -- ^ The resulting CRC. There is a delay of a single cycle from input to output.
crcEngine params@(ParallelCRCParams SNat crcParams@(CRCParams SNat SNat _ _ _ _ _) _ _) inDat
  = crcOut
    where
      steps = smap (laneStep params) (repeat @nLanes ())
      (validLanesX, datX) = unbundle $ fromJustX <$> inDat
      step = fmap (\i -> steps !! i) validLanesX

      nextCrcState = step <*> crcState <*> datX
      crcState = regEn (_crcInitial crcParams) (isJust <$> inDat) nextCrcState
      crcOut = xor (_crcXorOutput crcParams) <$> (applyWhen (_crcReflectOutput crcParams) reverseBV <$> crcState)

-- | The validator, see `crcEngine` for more details
--
-- In contrast to the `crcEngine` the `crcValidator` assumes that __only__ the last
-- transfer of a message contains non-valid words.
crcValidator
  :: forall (dom :: Domain)
            (nLanes :: Nat)
            (crcWidth :: Nat)
            (dataWidth :: Nat)
   . HiddenClockResetEnable dom
  => 1 <= nLanes
  => ParallelCRCParams crcWidth dataWidth nLanes
  -- ^ The CRC
  -> Signal dom (Maybe (Index nLanes, Vec nLanes (BitVector dataWidth)))
  -- ^ The input data.
  -> Signal dom Bool
  -- ^ Whether the CRC is valid. This is valid on the transfer of the last
  --   word in the message + crc. So there is no one clock cycle delay
  --   in contrast with `crcEngine`
crcValidator params@(ParallelCRCParams nLanes@SNat crcParams@(CRCParams SNat SNat _ _ _ _ _) _ residues) inDat
  = out
    where
      step = laneStep params (subSNatLe nLanes d1) ()
      (validLanesX, datX) = unbundle $ fromJustX <$> inDat

      nextCrcState = step <$> crcState <*> datX
      crcState = regEn (_crcInitial crcParams) (isJust <$> inDat) nextCrcState
      checkResidue = fmap (residues !!) validLanesX

      -- Maybe not have this mux? User should not check this if they are not
      -- providing input
      out = mux (isJust <$> inDat) (liftA2 (==) checkResidue nextCrcState) (pure False)
