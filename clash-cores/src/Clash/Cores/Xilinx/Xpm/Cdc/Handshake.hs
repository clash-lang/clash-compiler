{-|
  Copyright   :  (C) 2023, Google LLC,
                     2023, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Cdc.Handshake
  ( xpmCdcHandshake
  , XpmCdcHandshakeConfig(..)
  , xpmCdcHandshakeWith
  ) where

import Clash.Explicit.Prelude

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal
import Clash.Cores.Xilinx.Xpm.Cdc.Single (XpmCdcSingleConfig(..), xpmCdcSingleWith)

-- | Synchronizes data from the source clock domain to the destination. For this
-- to function correctly, a full handshake must be completed before another data
-- transfer is initiated. The handshake is considered completed when both sides
-- have acknowledged the transfer and the handshake signals have been reset.
--
-- By default, it uses four synchronization stages in both source and
-- destination domains, and auto-detects whether to use initial values for the
-- synchronization registers. Use 'xpmCdcHandshakeWith' to change these
-- settings. For more information see [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_HANDSHAKE).
--
-- __N.B.__: In order to simulate initial values, both the source and destination
--           domain need to support them. If the source and destination domain
--           disagree on this property, use of this function will fail to
--           simulate and translate to HDL. You can explicitly set it using
--           'xpmCdcHandshakeWith'.
xpmCdcHandshake ::
  forall a src dst.
  ( 1 <= BitSize a, BitSize a <= 1024
  , KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , HasCallStack
  ) =>
  Clock src ->
  Clock dst ->
  -- | Word to synchronize to destination domain. This value should not change
  -- when @src_send@ is asserted.
  "src_in" ::: Signal src a ->

  -- | Assertion of this signal allows the @src_in@ bus to be synchronized to the
  -- destination clock domain. This signal should only be asserted when @src_rcv@
  -- is deasserted, indicating that the previous data transfer is complete. This
  -- signal should only be deasserted once @src_rcv@ is asserted, acknowledging
  -- that the @src_in@ has been received by the destination logic.
  "src_send" ::: Signal src Bool ->

  -- | Asserting this signal indicates that data on @dest_out@ has been captured
  -- by the destination logic. This signal should be deasserted once @dest_req@ is
  -- deasserted, completing the handshake on the destination clock domain and
  -- indicating that the destination logic is ready for a new data transfer.
  "dst_ack" ::: Signal dst Bool ->

  -- | @dest_req@ indicates that @dest_out@ contains valid data. It can be
  -- acknowledges by asserting @dst_ack@. @src_rcv@ indicates that the destination
  -- domain has acknowledged a data transfer.
  ( "dest_out" ::: Signal dst a
  , "dest_req" ::: Signal dst Bool
  , "src_rcv"  ::: Signal src Bool
  )
xpmCdcHandshake = xpmCdcHandshakeWith XpmCdcHandshakeConfig{..}
 where
  srcStages = d4
  dstStages = d4
  initialValues =
    case (initBehavior @src, initBehavior @dst) of
      (SDefined, SDefined) -> True
      (SUnknown, SUnknown) -> False
      _ -> clashCompileError $ "xpmCdcHandshake: domains need to agree on initial value "
                            <> "behavior. To set initial value usage explicitly, "
                            <> "consider using 'xpmCdcHandshakeWith'."
{-# INLINE xpmCdcHandshake #-}

-- | Configuration for 'xpmCdcHandshakeWith'
--
-- Other attributes that are hardcoded:
--
-- +------------------+-------+
-- | Attribute        | Value |
-- +==================+=======+
-- | @DEST_EXT_HSK@   |     1 |
-- +------------------+-------+
-- | @SIM_ASSERT_CHK@ |     0 |
-- +------------------+-------+
data XpmCdcHandshakeConfig srcStages dstStages = XpmCdcHandshakeConfig
  { -- | Number of registers, clocked by the src clock, that are used to synchronize @dest_ack@ to @src_rcv@.
    --
    -- This is what [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_HANDSHAKE)
    -- calls @SRC_SYNC_FF@.
    srcStages :: SNat srcStages

    -- | Number of registers, clocked by the dst clock,
    -- that are used to synchronize between the input register of @src_send@ and the output register of @dest_req@.
    --
    -- This is what [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_HANDSHAKE)
    -- calls @DEST_SYNC_FF@.
  , dstStages :: SNat dstStages

    -- | Initialize registers used within the primitive to /0/. Note that
    -- 'xpmCdcHandshake' will set this to 'True' if both domains support initial
    -- values, to 'False' if neither domain does, and will otherwise emit an
    -- error.
    --
    -- This value is ignored in Clash simulation on domains configured to not
    -- support initial values.
    --
    -- This is what [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_HANDSHAKE)
    -- calls @INIT_SYNC_FF@.
  , initialValues :: Bool
  }

-- | Like 'xpmCdcHandshake', but with a configurable number of stages, initial values,
-- and registered input. Also see 'XpmCdcHandshakeConfig'.
xpmCdcHandshakeWith ::
  forall srcStages dstStages a src dst.
  ( 2 <= srcStages, srcStages <= 10
  , 2 <= dstStages, dstStages <= 10
  , 1 <= BitSize a, BitSize a <= 1024
  , KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , HasCallStack
  ) =>
  XpmCdcHandshakeConfig srcStages dstStages ->
  Clock src ->
  Clock dst ->

  -- | Word to synchronize to destination domain. This value should not change
  -- when @src_send@ is asserted.
  "src_in" ::: Signal src a ->

  -- | Assertion of this signal allows the @src_in@ bus to be synchronized to the
  -- destination clock domain. This signal should only be asserted when @src_rcv@
  -- is deasserted, indicating that the previous data transfer is complete. This
  -- signal should only be deasserted once @src_rcv@ is asserted, acknowledging
  -- that the @src_in@ has been received by the destination logic.
  "src_send" ::: Signal src Bool ->

  -- | Asserting this signal indicates that data on @dest_out@ has been captured
  -- by the destination logic. This signal should be deasserted once @dest_req@ is
  -- deasserted, completing the handshake on the destination clock domain and
  -- indicating that the destination logic is ready for a new data transfer.
  "dst_ack" ::: Signal dst Bool ->

  -- | @dest_req@ indicates that @dest_out@ contains valid data. It can be
  -- acknowledges by asserting @dst_ack@. @src_rcv@ indicates that the destination
  -- domain has acknowledged a data transfer.
  ( "dest_out" ::: Signal dst a
  , "dest_req" ::: Signal dst Bool
  , "src_rcv"  ::: Signal src Bool
  )
xpmCdcHandshakeWith XpmCdcHandshakeConfig{srcStages=srcStages@SNat, dstStages=dstStages@SNat, ..} clkSrc clkDst srcIn srcSend dstAck
  | clashSimulation = sim
  | otherwise = synth
 where
  -- Definition used in for HDL generation
  synth = (unpack <$> unPort go0, bitCoerce <$> unPort go1,  bitCoerce <$> unPort go2)
   where
    (go0,go1,go2) = go
    go :: ( Port "dest_out" dst (BitVector (BitSize a))
          , Port "dest_req" dst Bit
          , Port "src_rcv"  src Bit
          )

    go =
      inst
        (instConfig "xpm_cdc_handshake")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"DEST_EXT_HSK"   @Integer 1)
        (Param @"DEST_SYNC_FF"   @Integer (natToNum @dstStages))

        (Param @"INIT_SYNC_FF"   @Integer (if initialValues then 1 else 0))
        (Param @"SIM_ASSERT_CHK" @Integer 0)
        (Param @"SRC_SYNC_FF"    @Integer (natToNum @srcStages))
        (Param @"WIDTH"          @Integer (natToNum @(BitSize a)))

        (Port      @"dest_ack" (bitCoerce @Bool @Bit <$> dstAck))
        (ClockPort @"dest_clk" clkDst)

        (ClockPort @"src_clk"  clkSrc)
        (Port      @"src_in"   (pack <$> srcIn))
        (Port      @"src_send" (bitCoerce @Bool @Bit <$> srcSend))

  -- Definition used in Clash simulation
  sim = (dstOut, dstReq, srcRcv)

  defOpts :: forall stages. SNat stages -> XpmCdcSingleConfig stages
  defOpts nStages = XpmCdcSingleConfig
    { stages = nStages
    , initialValues = initialValues
    , registerInput = False }

  srcSendFfSynced = xpmCdcSingleWith (defOpts dstStages) clkSrc clkDst srcSendFf
  srcRcv = xpmCdcSingleWith (defOpts srcStages) clkDst clkSrc dstAck

  srcSendFf = delay clkSrc enableGen (initVal False) srcSend
  srcHsDataFf = delay clkSrc (toEnable (not <$> srcSendFf)) (initVal (unpack 0)) srcIn
  dstOutEna = toEnable (srcSendFfSynced .&&. fmap not dstReq)

  dstOut =
    delay
      clkDst dstOutEna (initVal (unpack 0))
      (unsafeSynchronizer clkSrc clkDst srcHsDataFf)

  dstReq = delay clkDst enableGen (initVal False) srcSendFfSynced

  initVal :: forall x . NFDataX x => x -> x
  initVal v
    | initialValues = v
    | otherwise = deepErrorX "xpmCdcHandshake: initial values undefined"

{-# INLINE xpmCdcHandshakeWith #-}
