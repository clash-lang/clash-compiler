{-|
  Copyright   :  (C) 2023, Amaranth HDL contributors
                     2024, Rowan Goemans <goemansrowan@gmail.com>
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

This is partially based on the [Amaranth CRC core](https://github.com/amaranth-lang/amaranth/blob/main/amaranth/lib/crc/__init__.py)

The algorithm for the Software CRC and the computation of the FG Matrices were
rewritten directly from their sources.

-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Crc.Internal where

import           Clash.Prelude
import           Clash.Class.HasDomain       (TryDomain, TryDomainResult(NotFound))
import qualified Clash.Sized.Vector as V     (toList)

import qualified Data.List as L
import           Data.Maybe
import           Data.Proxy                  (Proxy(..))
import           Unsafe.Coerce               (unsafeCoerce)

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import           Type.Reflection

-- | Contains all the parameters to generate a CRC implementation
data CrcParams (crcWidth :: Nat)
  = CrcParams
      { _crcWidth         :: SNat crcWidth
      -- ^ Bit width of CRC word. Also known as "width" in the Williams model.
      --   See http://techref.massmind.org/techref/method/math/crcguide.html
      , _crcPolynomial    :: BitVector crcWidth
      -- ^ CRC polynomial to use, without the implicit
      --   @x^crc_width@ term. Polynomial is always specified with the highest
      --   order terms in the most significant bit positions; use
      --   '_crcReflectInput' and '_crcReflectOutput' to perform a least
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

-- | No domain in 'CrcParams'
type instance TryDomain t (CrcParams crcWidth) = 'NotFound

-- | This class is used to define CRCs
--
-- First make a data declaration without constructors for your CRC and then
-- create a 'KnownCrc' instance for that type which contains your CRC parameters:
--
-- @
-- data MyCrc = MyCrc
-- instance KnownCrc MyCrc where
--   type CrcWidth MyCrc = ...
--   crcParams _ = CrcParams
--     { _crcWidth = SNat
--     , _crcPolynomial = ..
--     , _crcInitial = ..
--     , _crcReflectInput = ..
--     , _crcReflectOutput = ..
--     , _crcXorOutput = ..
--     }
-- @
--
-- See "Clash.Cores.Crc.Catalog" for many definition examples.
class (KnownNat (CrcWidth crc), 1 <= (CrcWidth crc)) => KnownCrc (crc :: Type) where
  type CrcWidth crc :: Nat
  crcParams :: crc -> CrcParams (CrcWidth crc)

-- | Allow one to compute values for the CRC
--
-- __NB__: This data type is not synthesizable
data SoftwareCrc (crcWidth :: Nat) (dataWidth :: Nat)
  = SoftwareCrc
      { _crcParams      :: CrcParams crcWidth
      , _crcDataWidth   :: SNat dataWidth
      , _crcTopBitMask  :: BitVector (crcWidth + dataWidth)
      , _crcpolyShifted :: BitVector (crcWidth + dataWidth)
      , _crcCurrent     :: BitVector (crcWidth + dataWidth)
      }
  deriving (Generic, Show, ShowX)

-- | No domain in 'SoftwareCrc'
type instance TryDomain t (SoftwareCrc crcWidth dataWidth) = 'NotFound

-- | Apply function only when bool is True
applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f x = f x
applyWhen False _ x = x

-- | Reverse bitvector
reverseBV :: KnownNat n => BitVector n -> BitVector n
reverseBV = v2bv . reverse . bv2v

-- Implementation notes:
-- We always compute most-significant bit first, which means the
-- polynomial and initial value may be used as-is, and the '_crcReflectInput'
-- and '_crcReflectOutput' values have their usual sense.
-- However, when computing word-at-a-time and MSbit-first, we must align
-- the input word so its MSbit lines up with the MSbit of the previous
-- CRC value. When the CRC width is smaller than the word width, this
-- would normally truncate data bits.
-- Instead, we shift the initial CRC left by the data width, and the
-- data word left by the crc width, lining up their MSbits no matter
-- the relation between the two widths.
-- The new CRC is then shifted right by the data width before output.

-- | Create a 'SoftwareCrc' from 'CrcParams'
--
-- __NB__: This function is not synthesizable
mkSoftwareCrcFromParams
  :: forall (crcWidth :: Nat) (dataWidth :: Nat)
   . CrcParams crcWidth
  -> SNat dataWidth
  -> SoftwareCrc crcWidth dataWidth
mkSoftwareCrcFromParams _crcParams@CrcParams{..} _crcDataWidth@SNat = go _crcParams
  where
    go (CrcParams SNat _ _ _ _ _) = reset $ SoftwareCrc { .. }
      where
        combSNat = addSNat _crcWidth _crcDataWidth
        _crcTopBitMask = shiftL 1 (snatToNum combSNat - 1)
        _crcpolyShifted = shiftL (extend _crcPolynomial) (snatToNum _crcDataWidth)
        _crcCurrent = 0

-- | Create a 'SoftwareCrc' given 'KnownCrc'
--
-- __NB__: This function is not synthesizable
mkSoftwareCrc
  :: forall (crc :: Type) (dataWidth :: Nat)
   . KnownCrc crc
  => crc
  -- ^ The CRC to use
  -> SNat dataWidth
  -- ^ The @dataWidth@ of the words to feed
  -> SoftwareCrc (CrcWidth crc) dataWidth
mkSoftwareCrc crc dataWidth = mkSoftwareCrcFromParams (crcParams crc) dataWidth

-- | Reset the 'SoftwareCrc'. If you want to reuse it for multiple messages
--   you need to reset it in between messages.
reset
  :: forall (crcWidth :: Nat) (dataWidth :: Nat)
   . SoftwareCrc crcWidth dataWidth
  -- ^ Current CRC engine
  -> SoftwareCrc crcWidth dataWidth
  -- ^ CRC engine fresh to be used
reset engine@(SoftwareCrc {..}) = go _crcDataWidth _crcParams
  where
    CrcParams {..} = _crcParams
    go SNat (CrcParams SNat _ _ _ _ _) = engine { _crcCurrent = initialCrc }
      where
        initial :: BitVector (crcWidth + dataWidth)
        initial = extend _crcInitial
        initialCrc = shiftL initial (snatToNum _crcDataWidth)

-- | Feed a word to the 'SoftwareCrc'
feed
  :: forall (crcWidth :: Nat) (dataWidth :: Nat)
   . SoftwareCrc crcWidth dataWidth
  -- ^ Current 'SoftwareCrc'
  -> BitVector dataWidth
  -- ^ Update 'SoftwareCrc' with this input
  -> SoftwareCrc crcWidth dataWidth
  -- ^ 'SoftwareCrc' with update processed
feed engine@(SoftwareCrc {..}) word = go _crcDataWidth _crcParams
  where
    CrcParams {..} = _crcParams
    go SNat (CrcParams SNat _ _ _ _ _) = engine { _crcCurrent = nextCrc }
      where
        applyPolyBit crcB
          | crcB .&. _crcTopBitMask > 0 = (shiftL crcB 1) `xor` _crcpolyShifted
          | otherwise                   = shiftL crcB 1
        applyPoly crcW = L.iterate applyPolyBit crcW L.!! snatToNum _crcDataWidth
        applyCrc crc w = applyPoly $ crc `xor` shiftL (extend w) (snatToNum _crcWidth)
        nextCrc = applyCrc _crcCurrent $ applyWhen _crcReflectInput reverseBV word

-- | Get current CRC state. This is the value before any output reflection and output XOR.
getCrcState
  :: forall (crcWidth :: Nat) (dataWidth :: Nat)
   . SoftwareCrc crcWidth dataWidth
  -> BitVector crcWidth
getCrcState (SoftwareCrc {..}) = go _crcDataWidth _crcParams
  where
    go SNat (CrcParams SNat _ _ _ _ _) = fst $ split _crcCurrent

-- | Generate the CRC
--
--   If the 'SoftwareCrc' will be reused, 'reset' must be called before starting
--   to process the next message.
digest
  :: forall (crcWidth :: Nat) (dataWidth :: Nat)
   . SoftwareCrc crcWidth dataWidth
  -> BitVector crcWidth
digest params@(SoftwareCrc {..}) = go _crcDataWidth _crcParams
  where
    go SNat (CrcParams SNat _ _ _ _ _) = outputCrcRev `xor` _crcXorOutput
      where
        CrcParams {..} = _crcParams
        outputCrcRev = applyWhen _crcReflectOutput reverseBV $ getCrcState params

-- | Compute the raw residue value for these 'CrcParams', which is the value left in the
--   CRC register after processing any valid codeword. Raw in this case means
--   this is the residue before the output reflection and out xor has been applied.
--
--   A fixed amount of padding can be given. This is useful during CRC verification
--   in the scenario where where you want to handle more words in a single cycle
--   than are native to the protocol.
--
--   For example ethernet is byte-oriented but you may want to handle 32 bit
--   in a single cycle to achieve high throughput.
--   This means that it is possible for 8, 16, 24 or 32 bits in the last data
--   word of the stream to be valid.
--   We can calculate a residue for each case and compare it to the CRC result.
--   This saves us from having to instantiate four seperate CRCs for the receive path.
--   The invalid words must be set to zero.
--
--   To use the output of this function as a constant in your design, you can
--   lift it through Template Haskell:
--
-- @
-- import qualified Data.List as L
--
-- res :: BitVector 32
-- res = $(lift $ rawResidue Crc32_ethernet 0)
--
-- ress :: Vec 4 (BitVector 32)
-- ress = $('Clash.Sized.Vector.listToVecTH' $ L.map (rawResidue Crc32_ethernet) [0..3])
-- @
--
-- __NB__: This function is not synthesizable
rawResidue
  :: forall (crc :: Type)
   . KnownCrc crc
  => crc
  -- ^ The CRC to use
  -> Int
  -- ^ How many invalid(zero) bits the stream is padded with
  -> BitVector (CrcWidth crc)
  -- ^ The residue
rawResidue crc nExtra = rawResi
  where
    dataWidth = d1
    CrcParams {..} = crcParams crc
    swCrc = mkSoftwareCrc crc dataWidth
    crcVal = applyWhen _crcReflectOutput reverse $ bv2v $ digest swCrc
    bitsToFeed = fmap (v2bv . reverse . singleton) $
                    V.toList crcVal L.++ L.replicate nExtra 0
    rawResi = getCrcState $ L.foldl' feed swCrc bitsToFeed

-- | Compute the residue value for these 'CrcParams', which is the value left in the
--   CRC register after processing any valid codeword.
--
--   To use the output of this function as a constant in your design, you can
--   lift it through Template Haskell:
--
-- @
-- res :: BitVector 32
-- res = $(lift $ residue Crc32_ethernet)
-- @
--
-- __NB__: This function is not synthesizable
residue
  :: forall (crc :: Type)
   . KnownCrc crc
  => crc
  -- ^ The CRC to use
  -> BitVector (CrcWidth crc)
  -- ^ The residue
residue crc =  applyWhen _crcReflectOutput reverseBV $ rawResidue crc 0
  where
    CrcParams {..} = crcParams crc

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
--     - @m@ is the data word width
--
--   The matrix is used to select which bits are XOR'd together to compute
--   each bit @i@ of the new state. If @F[i][j]@ is set then bit @j@ of the
--   @old state@ is included in the XOR. If @G[i][j]@ is set then bit @j@ of the
--   @input@ is included in the XOR.
--
--   These matrices are not affected by '_crcInitial', '_crcReflectOutput' or
--   '_crcXorOutput'.
data FGMatrices (crcWidth :: Nat) (dataWidth :: Nat)
  = FGMatrices
      { _fMat :: Vec crcWidth (BitVector crcWidth)
      , _gMat :: Vec crcWidth (BitVector dataWidth)
      }
  deriving (Show, Lift)

-- | No domain in 'FGMatrices'
type instance TryDomain t (FGMatrices crcWidth dataWidth) = 'NotFound

-- | Compute the 'FGMatrices' from 'CrcParams'
mkFGMatrices
  :: forall (crc :: Type) (dataWidth :: Nat)
   . KnownCrc crc
  => crc
  -> SNat dataWidth
  -> FGMatrices (CrcWidth crc) dataWidth
mkFGMatrices crc dataWidth@SNat = FGMatrices f g
  where
    params = crcParams crc
    newParams = params
                  { _crcReflectOutput = False
                  , _crcXorOutput = 0
                  }
    withInitCrc i = mkSoftwareCrcFromParams (newParams { _crcInitial = i }) dataWidth
    runCrc initial dat = digest $ feed (withInitCrc initial) dat
    onehots n = shiftL 1 . fromIntegral <$> indices n
    postProcess x = v2bv . reverse <$> transpose x
    f = postProcess $ bv2v . (\p -> runCrc p 0) <$> onehots (SNat @(CrcWidth crc))
    g = postProcess $ bv2v . (\p -> runCrc 0 p) <$> onehots dataWidth

-- | Vertically partition a matrix
partitionMat
  :: KnownNat q
  => SNat p
  -> Vec n (BitVector (p * q))
  -> Vec n (Vec p (BitVector q))
partitionMat SNat mat = fmap unpack mat

-- | Flatten vertically partitioned matrix
unPartitionMat
  :: KnownNat q
  => KnownNat p
  => Vec n (Vec p (BitVector q))
  -> Vec n (BitVector (p * q))
unPartitionMat mat = fmap pack mat

-- | Helper type that contains the flattened @F@ and @G@ matrices for each lane
data CrcLaneParams (crcWidth :: Nat) (dataWidth :: Nat) (nLanes :: Nat) where
  -- | The final lane
  CrcLane1
    :: Vec crcWidth (BitVector (crcWidth + dataWidth))
    -- ^ Flattened @F|G@ matrix for this lane
    -> CrcLaneParams crcWidth dataWidth 1
  -- | Not the final lane
  CrcLaneN
    :: 2 <= lane
    => SNat lane
    -- ^ Which lane this is
    -> Vec crcWidth (BitVector (crcWidth + lane * dataWidth))
    -- ^ Flattened @F|G@ matrix for this lane
    -> CrcLaneParams crcWidth dataWidth (lane - 1)
    -> CrcLaneParams crcWidth dataWidth lane

deriving instance (KnownNat crcWidth, KnownNat dataWidth, KnownNat nLanes)
  => Lift (CrcLaneParams crcWidth dataWidth nLanes)

deriving instance (KnownNat crcWidth, KnownNat dataWidth, KnownNat nLanes)
  => Show (CrcLaneParams crcWidth dataWidth nLanes)

-- | No domain in 'CrcLaneParams'
type instance TryDomain t (CrcLaneParams crcWidth dataWidth nLanes) = 'NotFound

-- | Contains all necessary parameters for the hardware CRC implementation
data CrcHardwareParams (crcWidth :: Nat) (dataWidth :: Nat) (nLanes :: Nat)
  = CrcHardwareParams
      { _crcDataWidthHW :: SNat dataWidth
      , _crcNlanes :: SNat nLanes
      , _crcBaseParams :: CrcParams crcWidth
      , _crcLaneParams :: CrcLaneParams crcWidth dataWidth nLanes
      , _crcResidues :: Vec nLanes (BitVector crcWidth)
      }
  deriving (Show, Lift)

-- | No domain in 'CrcHardwareParams'
type instance TryDomain t (CrcHardwareParams crcWidth dataWidth nLanes) = 'NotFound

-- | This class is used to indicate a CRC has a derived hardware implementation
--
-- @nLanes@ indicates the maximum number of @dataWidth@ words you want to
-- process in a single cycle. For example the stream could be byte-oriented,
-- but processing is done @n@ bytes at a time.
--
-- Use 'Clash.Cores.Crc.deriveHardwareCrc' to create an instance. No instances
-- should be implemented by hand, because a proper instance requires
-- compile-time evaluation.
class
  ( KnownCrc crc, KnownNat dataWidth, 1 <= dataWidth, KnownNat nLanes, 1 <= nLanes)
  => HardwareCrc (crc :: Type) (dataWidth :: Nat) (nLanes :: Nat) where
  crcHardwareParams
    :: crc
    -- ^ Which CRC
    -> SNat dataWidth
    -- ^ What word size in bits the hardware implementation can handle
    -> SNat nLanes
    -- ^ The number of lanes
    -> CrcHardwareParams (CrcWidth crc) dataWidth nLanes

-- | Similar to 'compareSNat' but splits into Lt, Eq and Gt instead of Le and Gt.
data SNatOrdering a b where
  SNatLT2 :: forall a b. a <= (b - 1) => SNatOrdering a b
  SNatEQ2 :: forall a b. a ~ b => SNatOrdering a b
  SNatGT2 :: forall a b. (b + 1) <= a => SNatOrdering a b

-- | No domain in 'SNatOrdering'
type instance TryDomain t (SNatOrdering a b) = 'NotFound

-- | Get an ordering relation between two SNats
compareSNat2 :: forall a b . SNat a -> SNat b -> SNatOrdering a b
compareSNat2 a b
  = case compare (snatToInteger a) (snatToInteger b) of
      LT -> unsafeCoerce (SNatLT2 @0 @1)
      EQ -> unsafeCoerce (SNatEQ2 @0 @0)
      GT -> unsafeCoerce (SNatGT2 @1 @0)

-- | Flattens the @F@ and @G@ matrices into a single @F|G@ matrix
flattenFGMatrices
  :: KnownNat dataWidth
  => FGMatrices crcWidth dataWidth
  -> Vec crcWidth (BitVector (crcWidth + dataWidth))
flattenFGMatrices (FGMatrices f g) = zipWith (++#) f g

-- | Compute CRC lane parameters given 'CrcParams' and the number of lanes
mkCrcLaneParams
  :: KnownCrc crc
  => 1 <= nLanes
  => crc
  -> SNat dataWidth
  -> SNat nLanes
  -> CrcLaneParams (CrcWidth crc) dataWidth nLanes
mkCrcLaneParams crc dataWidth@SNat nLanes@SNat
  = case compareSNat2 d1 nLanes of
      SNatEQ2 -> CrcLane1 (flattenFGMatrices $ mkFGMatrices crc dataWidth)
      SNatLT2 -> CrcLaneN nLanes fg (mkCrcLaneParams crc dataWidth $ subSNat nLanes d1)
        where
          reflectInput = _crcReflectInput $ crcParams crc
          (FGMatrices f g) = mkFGMatrices crc (mulSNat nLanes dataWidth)
          reverseLanes = unPartitionMat . fmap reverse . partitionMat nLanes
          fg = flattenFGMatrices $ FGMatrices f (applyWhen reflectInput reverseLanes g)
      _ -> clashCompileError "mkCrcLaneParams: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

-- | Get the flattened @F|G@ Matrix for a given lane
getFGMatrix
  :: lane <= lanes
  => 1 <= lane
  => CrcLaneParams crcWidth dataWidth lanes
  -> SNat lane
  -> Vec crcWidth (BitVector (crcWidth + lane * dataWidth))
getFGMatrix (CrcLane1 fg) lane
  = case compareSNat2 d1 lane of
      SNatEQ2 -> fg
      _ -> clashCompileError "getFGMatrix, CrcLane1: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
getFGMatrix (CrcLaneN lanes fg next) lane
  = case compareSNat2 lane lanes of
      SNatLT2 -> getFGMatrix next lane
      SNatEQ2 -> fg
      _ -> clashCompileError "getFGMatrix, CrcLaneN: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

-- | Construct parameters for a parallel hardware CRC implementation
mkCrcHardwareParams
  :: forall (crc :: Type)
            (dataWidth :: Nat)
            (nLanes :: Nat)
   . KnownCrc crc
  => 1 <= nLanes
  => crc
  -- ^ The CRC to instantiate parallel implementation for
  -> SNat dataWidth
  -- ^ The width of the word to feed
  -> SNat nLanes
  -- ^ The number of lanes
  -> CrcHardwareParams (CrcWidth crc) dataWidth nLanes
mkCrcHardwareParams crc dataWidth nLanes@SNat
  = hwParams
    where
      computeResidue = rawResidue crc . (snatToNum dataWidth *) . fromIntegral

      hwParams = CrcHardwareParams
        { _crcDataWidthHW = dataWidth
        , _crcNlanes = nLanes
        , _crcBaseParams = crcParams crc
        , _crcLaneParams = mkCrcLaneParams crc dataWidth nLanes
        , _crcResidues = computeResidue <$> reverse indicesI
        }

typeRepToTHType :: SomeTypeRep -> TH.Type
typeRepToTHType (SomeTypeRep (Con tyCon)) = TH.ConT $ TH.Name nameBase flavor
  where
    nameBase = TH.mkOccName (tyConName tyCon)
    flavor
      = TH.NameG TH.TcClsName
          (TH.mkPkgName $ tyConPackage tyCon)
          (TH.mkModName $ tyConModule tyCon)
typeRepToTHType _ = error "typeRepToTHType: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

-- | Derive an instance for the 'HardwareCrc' class for the given arguments
--
-- For example, the following derives a 'HardwareCrc' instance for the 32-bit Ethernet CRC
-- where you can feed it 8, 16, 24 or 32 bits at a time:
--
-- @
-- {\-\# LANGUAGE MultiParamTypeClasses \#-\}
--
-- deriveHardwareCrc Crc32_ethernet d8 d4
-- @
--
-- For the derivation to work the @MultiParamTypeClasses@ pragma must be enabled in
-- the module that uses 'deriveHardwareCrc'.
--
-- See 'HardwareCrc', 'crcEngine' and 'crcValidator' for more information about what
-- the arguments mean.
deriveHardwareCrc
  :: forall (crc :: Type)
            (dataWidth :: Nat)
            (nLanes :: Nat)
   . Typeable crc
  => KnownCrc crc
  => 1 <= dataWidth
  => 1 <= nLanes
  => crc
  -- ^ For which CRC to derive the instance
  -> SNat dataWidth
  -- ^ The width in bits of the words it can handle every clock cycle
  -> SNat nLanes
  -- ^ The number of lanes
  -> TH.Q [TH.Dec]
deriveHardwareCrc crc dataWidth@SNat nLanes@SNat = do
  let crcTy = pure $ typeRepToTHType $ someTypeRep (Proxy @crc)
      dataWidthTy = pure $ TH.LitT $ TH.NumTyLit $ snatToNum dataWidth
      nLanesTy = pure $ TH.LitT $ TH.NumTyLit $ snatToNum nLanes
      hwParams = lift $ mkCrcHardwareParams crc dataWidth nLanes

  [d| instance HardwareCrc $crcTy $dataWidthTy $nLanesTy where
        crcHardwareParams _ _ _ = $hwParams |]

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

-- | Like 'subSNat' but uses a 'Data.Type.Ord.<=' constraint
subSNatLe
  :: forall (n :: Nat)
            (m :: Nat)
   . m <= n
  => SNat n
  -> SNat m
  -> SNat (n - m)
subSNatLe a b = leToPlus @m @n $ subSNat a b

-- | Like 'take' but uses a 'Data.Type.Ord.<=' constraint
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

-- | Apply a step to a CRC and input given by @lanePrev@
--
-- This function is meant to be used via 'smap' and crashes if called with
-- out-of-bounds @lanePrev@.
laneStep
  :: CrcHardwareParams crcWidth dataWidth nLanes
  -> SNat lanePrev
  -> BitVector crcWidth
  -> Vec nLanes (BitVector dataWidth)
  -> BitVector crcWidth
laneStep (CrcHardwareParams SNat nLanes@SNat (CrcParams SNat _ _ _ _ _) laneParams _) lanePrev@SNat crc input
  = let lane = addSNat lanePrev d1 in case compareSNat lane nLanes of
    SNatLE -> matVecMul (getFGMatrix laneParams lane) (crc ++# (pack $ takeLe lane input))
    _      -> clashCompileError "laneStep: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

-- | A parallel multilane CRC engine and validator
--
-- @dataWidth@ should /always/ cleanly divide @crcWidth@. If this is not the case
-- then it is possible for the last message to be misaligned.
--
-- For example if you have a @dataWidth@ of @48@ and a @crcWidth@ of @32@. Then a possible
-- message could be 128 bits, two words + CRC. But this cannot be safely
-- run through the CRC circuit, because the final fragment contains 32 valid bits
-- and 16 invalid bits.
--
-- This rule is __NOT__ checked at the type level because as long you take
-- care that misalignment does not occur it is not a problem.
crcEngine
  :: forall (dom :: Domain)
            (crc :: Type)
            (dataWidth :: Nat)
            (nLanes :: Nat)
   . HiddenClockResetEnable dom
  => HardwareCrc crc dataWidth nLanes
  => crc
  -- ^ The CRC
  -> Signal dom (Maybe (Bool, Index nLanes, Vec nLanes (BitVector dataWidth)))
  -- ^ The input data
  --
  --   The @Bool@ must be asserted on the first fragment to start a new CRC
  --   computation.
  --
  --   @Index nLanes@ indicates how many @dataWidth@ words are valid in the
  --   vector minus 1.
  --
  --     - Ex. 1. @Just (False, 0, 0xDE :> 0xAD :> 0xBE :> 0xEF :> Nil)@
  --       means this is not the start of a new message and only the first word
  --       @0xDE@ is valid.
  --     - Ex. 2. @Just (True, 3, 0xDE :> 0xAD :> 0xBE :> 0xEF :> Nil@ means
  --       this is the start of a new message and all words in the vector are
  --       valid.
  -> Signal dom (BitVector (CrcWidth crc))
  -- ^ The resulting CRC. There is a delay of a single cycle from input to output.
crcEngine crc = crcEngineFromParams $ crcHardwareParams crc SNat SNat

-- | Like 'crcEngine' but uses 'CrcHardwareParams' instead of a 'HardwareCrc' constraint
crcEngineFromParams
  :: forall (dom :: Domain)
            (crcWidth :: Nat)
            (dataWidth :: Nat)
            (nLanes :: Nat)
   . HiddenClockResetEnable dom
  => CrcHardwareParams crcWidth dataWidth nLanes
  -> Signal dom (Maybe (Bool, Index nLanes, Vec nLanes (BitVector dataWidth)))
  -> Signal dom (BitVector crcWidth)
crcEngineFromParams
  hwParams@(CrcHardwareParams SNat SNat CrcParams{_crcWidth=SNat, ..} _ _)
  inDat
  = crcOut
    where
      steps = smap (\sn _ -> laneStep hwParams sn) (repeat @nLanes ())
      (resetCrcX, validLanesX, datX) = unbundle $ fromJustX <$> inDat
      step = fmap (steps !!) validLanesX

      nextCrcState = step <*> (mux resetCrcX (pure _crcInitial) crcState) <*> datX
      crcState = regEn _crcInitial (isJust <$> inDat) nextCrcState
      crcOut = xor _crcXorOutput <$> (applyWhen _crcReflectOutput reverseBV <$> crcState)

-- | The validator, see 'crcEngine' for more details
--
-- Additionally the 'crcValidator' has two extra requirements:
--
-- - The 'crcValidator' assumes that __only__ the last transfer of a message
--   contains non-valid words.
-- - Any non-valid words must be set to zero.
--
-- Due to these extra requirements the implementation can be more efficient
-- than 'crcEngine'.
crcValidator
  :: forall (dom :: Domain)
            (crc :: Type)
            (dataWidth :: Nat)
            (nLanes :: Nat)
   . (HiddenClock dom, HiddenEnable dom)
  => HardwareCrc crc dataWidth nLanes
  => crc
  -- ^ The CRC
  -> Signal dom (Maybe (Bool, Index nLanes, Vec nLanes (BitVector dataWidth)))
  -- ^ The input data
  --
  --   The @Bool@ must be asserted on the first fragment to start a new CRC
  --   computation.
  --
  --   @Index nLanes@ indicates how many @dataWidth@ words are
  --   valid in the vector minus 1.
  --
  --     - Ex. 1. @Just (False, 0, 0xDE :> 0xAD :> 0xBE :> 0xEF :> Nil)@
  --       means this is not the start of a new message and only the first word
  --       @0xDE@ is valid.
  --     - Ex. 2. @Just (True, 3, 0xDE :> 0xAD :> 0xBE :> 0xEF :> Nil@
  --       means this is the start of a new message and all words in the vector
  --       are valid.
  -> Signal dom Bool
  -- ^ Whether the CRC is valid. There is a delay of a single cycle from input to output.
crcValidator crc = crcValidatorFromParams $ crcHardwareParams crc SNat SNat

-- | Like 'crcValidator' but uses 'CrcHardwareParams' instead of a 'HardwareCrc' constraint
crcValidatorFromParams
  :: forall (dom :: Domain)
            (crcWidth :: Nat)
            (dataWidth :: Nat)
            (nLanes :: Nat)
   . (HiddenClock dom, HiddenEnable dom)
  => 1 <= nLanes
  => CrcHardwareParams crcWidth dataWidth nLanes
  -> Signal dom (Maybe (Bool, Index nLanes, Vec nLanes (BitVector dataWidth)))
  -> Signal dom Bool
crcValidatorFromParams
  hwParams@(CrcHardwareParams SNat SNat CrcParams{_crcWidth=SNat, ..} _ residues)
  inDat
  = match
    where
      step = laneStep hwParams (subSNatLe (SNat @nLanes) d1)
      inValid = isJust <$> inDat
      (resetCrcX, validLanesX, datX) = unbundle $ fromJustX <$> inDat

      nextCrcState = step <$> (mux resetCrcX (pure _crcInitial) crcState) <*> datX
      crcState = regEn _crcInitial inValid nextCrcState
      matches = zipWith (==) residues <$> fmap pure crcState
      lane = regEn 0 inValid validLanesX
      match = (!!) <$> matches <*> lane
