{-|
  Copyright   :  (C) 2024, Rowan Goemans <goemansrowan@gmail.com>
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  CRC validation
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.CRC where

import qualified Data.List as List
import           Data.Char (ord)
import           Data.Maybe (catMaybes, fromJust)
import           Data.Proxy (Proxy(..))
import           Data.Kind (Type)
import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import qualified Clash.Sized.Vector as V
import           Clash.Prelude (exposeClockResetEnable)
import           Clash.Explicit.Prelude hiding (interleave)

import           Clash.Cores.CRC.Internal
import           Clash.Cores.CRC.Catalog

toVecN
  :: forall n a
   . KnownNat n
  => Default a
  => [Maybe a]
  -> [Maybe (Index n, Vec n a)]
toVecN [] = []
toVecN xs = result:toVecN bs
  where
    n = natToNum @n
    (as, bs) = List.splitAt n xs
    as' = catMaybes as
    valid = List.length as'
    vs = V.unsafeFromList $ as' List.++ List.replicate (n - valid) def
    result = if valid /= 0
               then Just (fromIntegral $ valid - 1, vs)
               else Nothing

proxyToSNat :: KnownNat n => Proxy n -> SNat n
proxyToSNat _ = SNat

nearestCleanDivisor :: Integer -> Integer -> Integer
nearestCleanDivisor a b =  List.head $ List.sortOn (\i -> abs (i - b)) candidates
  where
    candidates = List.filter (\i -> rem a i == 0) [1, 2 .. (a + 1)]

dataWidthCfgtoSomeNat :: forall crcWidth n. KnownNat crcWidth => KnownNat n => Index n -> SomeNat
dataWidthCfgtoSomeNat n = fromJust cleanDiv
  where
    cleanDiv = someNatVal $ nearestCleanDivisor (natToNum @crcWidth) (1 + fromIntegral n)

getCheckInput
  :: forall checkWords dataWidth
   . (checkWords * dataWidth) ~ 72
  => SNat checkWords
  -> SNat dataWidth
  -> Bool
  -> [BitVector dataWidth]
getCheckInput SNat SNat reflectInp = toList $ v2bv <$> (rev $ unconcatI @checkWords @dataWidth checkInpBits)
  where
    rev :: Vec n a -> Vec n a
    rev = applyWhen reflectInp reverse
    checkInpBits :: Vec 72 Bit
    checkInpBits = concat @9 @8 $ rev $ bv2v . fromIntegral . ord <$> V.unsafeFromList "123456789"

checkProp
  :: forall checkWords crc dataWidth
   . (checkWords * dataWidth) ~ 72
  => KnownNat checkWords
  => KnownCRC crc
  => Proxy crc
  -> BitVector (CRCWidth crc)
  -> SNat dataWidth
  -> TestTree
checkProp crc checkVal dataWidth = QC.testProperty testName $ checkVal QC.=== resultCheckVal
  where
    CRCParams {..} = crcParams crc dataWidth
    testName = ("datawidth ~ " List.++ (show (snatToNum dataWidth :: Integer)))
    wordsToFeed = getCheckInput (SNat @checkWords) dataWidth _crcReflectInput
    resultCheckVal = digest $ List.foldl' feed (mkSoftwareCRC crc dataWidth) wordsToFeed

crcEngineEqSoftware
  :: forall crc
   . KnownCRC crc
  => Proxy crc
  -- ^ Which crc
  -> Index 7
  -- ^ nLanes
  -> Index (Div (CRCWidth crc) 2)
  -- ^ n * dataWidth ~ crcWidth
  -> [Maybe Integer]
  -> QC.Property
crcEngineEqSoftware crc nLanesI dataWidthCfg inpI
  = go (dataWidthCfgtoSomeNat @(CRCWidth crc) dataWidthCfg) (fromJust $ someNatVal (1 + fromIntegral nLanesI))
    where
      go :: SomeNat -> SomeNat -> QC.Property
      go (SomeNat dataWidthP) (SomeNat nLanesP)
        = let nLanesSNat = proxyToSNat nLanesP
              dataWidthSNat = proxyToSNat dataWidthP
          in case (compareSNat d1 nLanesSNat, compareSNat d1 dataWidthSNat) of
            (SNatLE, SNatLE) -> prop nLanesSNat dataWidthSNat
            _                -> errorX "crcValidatorEqSoftware: Absurd"

      prop
        :: forall (nLanes :: Nat)
                  (dataWidth :: Nat)
         . 1 <= nLanes
        => 1 <= dataWidth
        => SNat nLanes
        -> SNat dataWidth
        -> QC.Property
      prop nLanes@SNat dataWidth@SNat = hwCRC QC.=== swCRC
        where
          inps :: [Maybe (BitVector dataWidth)]
          inps = fmap (fmap fromIntegral) inpI

          hwParams = mkCRCHardwareParams crc dataWidth nLanes
          crcEngine' = exposeClockResetEnable crcEngineFromParams systemClockGen resetGen enableGen hwParams

          swCRC = digest $ List.foldl' feed (mkSoftwareCRC crc dataWidth) (catMaybes inps)
          hwInp = Nothing : (toVecN inps)
          hwOut = crcEngine' (fromList hwInp)
          hwCRC = List.last $ sampleN (1 + List.length hwInp) hwOut

crcValidatorEqSoftware
  :: forall crc
   . KnownCRC crc
  => Proxy crc
  -- ^ Which crc
  -> Index 7
  -- ^ nLanes
  -> Index (CRCWidth crc)
  -- ^ n * dataWidth ~ crcWidth
  -> [Maybe Integer]
  -> QC.Property
crcValidatorEqSoftware crc nLanesI dataWidthCfg inpI
  = go (dataWidthCfgtoSomeNat @(CRCWidth crc) dataWidthCfg) (fromJust $ someNatVal (1 + fromIntegral nLanesI))
    where
      go :: SomeNat -> SomeNat -> QC.Property
      go (SomeNat dataWidthP) (SomeNat nLanesP)
        = let nLanesSNat = proxyToSNat nLanesP
              dataWidthSNat = proxyToSNat dataWidthP
          in case (compareSNat d1 nLanesSNat, compareSNat d1 dataWidthSNat) of
            (SNatLE, SNatLE) -> prop nLanesSNat dataWidthSNat
            _                -> errorX "crcValidatorEqSoftware: Absurd"

      prop
        :: forall (nLanes :: Nat)
                  (dataWidth :: Nat)
         . 1 <= nLanes
        => 1 <= dataWidth
        => SNat nLanes
        -> SNat dataWidth
        -> QC.Property
      prop nLanes@SNat dataWidth@SNat = hwOk QC.=== True
        where
          CRCParams{..} = crcParams crc dataWidth

          inp :: [Maybe (BitVector dataWidth)]
          inp = fmap (fmap fromIntegral) inpI

          inpJusts = catMaybes inp
          swCRC = digest $ List.foldl' feed (mkSoftwareCRC crc dataWidth) inpJusts
          swCRCVec :: Vec (Div (CRCWidth crc) dataWidth) (BitVector dataWidth)
          swCRCVec = applyWhen _crcReflectInput reverse
                      $ bitCoerce
                      $ applyWhen (xor _crcReflectInput _crcReflectOutput) reverseBV
                      $ resize
                      $ swCRC
          inpsWithCrc = inpJusts List.++ (V.toList swCRCVec)

          hwParams = mkCRCHardwareParams crc dataWidth nLanes
          crcValidator' = exposeClockResetEnable crcValidatorFromParams systemClockGen resetGen enableGen hwParams

          hwInp = Nothing : (toVecN $ fmap Just inpsWithCrc)
          hwOut = crcValidator' (fromList hwInp)
          hwOk = List.last $ sampleN (List.length hwInp) hwOut


mkCRCProps
  :: forall (crc :: Type)
   . KnownCRC crc
  => String
  -> Proxy crc
  -> BitVector (CRCWidth crc)
  -> BitVector (CRCWidth crc)
  -> TestTree
mkCRCProps crcName crc checkVal residueVal = props
  where
    checkProp'
      :: forall checkWords n
       . (checkWords * n) ~ 72
      => KnownNat checkWords
      => SNat n
      -> TestTree
    checkProp' dw'@SNat = checkProp @checkWords crc checkVal dw'
    props
      = testGroup crcName
          [ QC.testProperty "Residue" $ residueVal QC.=== residue crc
          , testGroup "Software check"
              [ checkProp' d1 , checkProp' d2 , checkProp' d3
              , checkProp' d8 , checkProp' d9 , checkProp' d12
              , checkProp' d24, checkProp' d36, checkProp' d72
              ]
          , QC.testProperty "crcEngine ~ SoftwareCRC" $ crcEngineEqSoftware crc
          , QC.testProperty "crcValidator ~ SoftwareCRC" $ crcValidatorEqSoftware crc
          ]

-- All of the check and residue values are taken from
-- https://reveng.sourceforge.io/crc-catalogue/all.htm
tests :: TestTree
tests = testGroup "CRC"
  [ mkCRCProps "CRC3_GSM" (Proxy @CRC3_GSM) 0x4 0x2
  , mkCRCProps "CRC3_ROHC" (Proxy @CRC3_ROHC) 0x6 0x0
  , mkCRCProps "CRC4_G_704" (Proxy @CRC4_G_704) 0x7 0x0
  , mkCRCProps "CRC4_INTERLAKEN" (Proxy @CRC4_INTERLAKEN) 0xb 0x2
  , mkCRCProps "CRC5_EPC_C1G2" (Proxy @CRC5_EPC_C1G2) 0x00 0x00
  , mkCRCProps "CRC5_G_704" (Proxy @CRC5_G_704) 0x07 0x00
  , mkCRCProps "CRC5_USB" (Proxy @CRC5_USB) 0x19 0x06
  , mkCRCProps "CRC6_CDMA2000_A" (Proxy @CRC6_CDMA2000_A) 0x0d 0x00
  , mkCRCProps "CRC6_CDMA2000_B" (Proxy @CRC6_CDMA2000_B) 0x3b 0x00
  , mkCRCProps "CRC6_DARC" (Proxy @CRC6_DARC) 0x26 0x00
  , mkCRCProps "CRC6_G_704" (Proxy @CRC6_G_704) 0x06 0x00
  , mkCRCProps "CRC6_GSM" (Proxy @CRC6_GSM) 0x13 0x3a
  , mkCRCProps "CRC7_MMC" (Proxy @CRC7_MMC) 0x75 0x00
  , mkCRCProps "CRC7_ROHC" (Proxy @CRC7_ROHC) 0x53 0x00
  , mkCRCProps "CRC7_UMTS" (Proxy @CRC7_UMTS) 0x61 0x00
  , mkCRCProps "CRC8_AUTOSAR" (Proxy @CRC8_AUTOSAR) 0xdf 0x42
  , mkCRCProps "CRC8_BLUETOOTH" (Proxy @CRC8_BLUETOOTH) 0x26 0x00
  , mkCRCProps "CRC8_CDMA2000" (Proxy @CRC8_CDMA2000) 0xda 0x00
  , mkCRCProps "CRC8_DARC" (Proxy @CRC8_DARC) 0x15 0x00
  , mkCRCProps "CRC8_DVB_S2" (Proxy @CRC8_DVB_S2) 0xbc 0x00
  , mkCRCProps "CRC8_GSM_A" (Proxy @CRC8_GSM_A) 0x37 0x00
  , mkCRCProps "CRC8_GSM_B" (Proxy @CRC8_GSM_B) 0x94 0x53
  , mkCRCProps "CRC8_HITAG" (Proxy @CRC8_HITAG) 0xb4 0x00
  , mkCRCProps "CRC8_I_432_1" (Proxy @CRC8_I_432_1) 0xa1 0xac
  , mkCRCProps "CRC8_I_CODE" (Proxy @CRC8_I_CODE) 0x7e 0x00
  , mkCRCProps "CRC8_LTE" (Proxy @CRC8_LTE) 0xea 0x00
  , mkCRCProps "CRC8_MAXIM_DOW" (Proxy @CRC8_MAXIM_DOW) 0xa1 0x00
  , mkCRCProps "CRC8_MIFARE_MAD" (Proxy @CRC8_MIFARE_MAD) 0x99 0x00
  , mkCRCProps "CRC8_NRSC_5" (Proxy @CRC8_NRSC_5) 0xf7 0x00
  , mkCRCProps "CRC8_OPENSAFETY" (Proxy @CRC8_OPENSAFETY) 0x3e 0x00
  , mkCRCProps "CRC8_ROHC" (Proxy @CRC8_ROHC) 0xd0 0x00
  , mkCRCProps "CRC8_SAE_J1850" (Proxy @CRC8_SAE_J1850) 0x4b 0xc4
  , mkCRCProps "CRC8_SMBUS" (Proxy @CRC8_SMBUS) 0xf4 0x00
  , mkCRCProps "CRC8_TECH_3250" (Proxy @CRC8_TECH_3250) 0x97 0x00
  , mkCRCProps "CRC8_WCDMA" (Proxy @CRC8_WCDMA) 0x25 0x00
  , mkCRCProps "CRC10_ATM" (Proxy @CRC10_ATM) 0x199 0x000
  , mkCRCProps "CRC10_CDMA2000" (Proxy @CRC10_CDMA2000) 0x233 0x000
  , mkCRCProps "CRC10_GSM" (Proxy @CRC10_GSM) 0x12a 0x0c6
  , mkCRCProps "CRC11_FLEXRAY" (Proxy @CRC11_FLEXRAY) 0x5a3 0x000
  , mkCRCProps "CRC11_UMTS" (Proxy @CRC11_UMTS) 0x061 0x000
  , mkCRCProps "CRC12_CDMA2000" (Proxy @CRC12_CDMA2000) 0xd4d 0x000
  , mkCRCProps "CRC12_DECT" (Proxy @CRC12_DECT) 0xf5b 0x000
  , mkCRCProps "CRC12_GSM" (Proxy @CRC12_GSM) 0xb34 0x178
  , mkCRCProps "CRC12_UMTS" (Proxy @CRC12_UMTS) 0xdaf 0x000
  , mkCRCProps "CRC13_BBC" (Proxy @CRC13_BBC) 0x04fa 0x0000
  , mkCRCProps "CRC14_DARC" (Proxy @CRC14_DARC) 0x082d 0x0000
  , mkCRCProps "CRC14_GSM" (Proxy @CRC14_GSM) 0x30ae 0x031e
  , mkCRCProps "CRC15_CAN" (Proxy @CRC15_CAN) 0x059e 0x0000
  , mkCRCProps "CRC15_MPT1327" (Proxy @CRC15_MPT1327) 0x2566 0x6815
  , mkCRCProps "CRC16_ARC" (Proxy @CRC16_ARC) 0xbb3d 0x0000
  , mkCRCProps "CRC16_CDMA2000" (Proxy @CRC16_CDMA2000) 0x4c06 0x0000
  , mkCRCProps "CRC16_CMS" (Proxy @CRC16_CMS) 0xaee7 0x0000
  , mkCRCProps "CRC16_DDS_110" (Proxy @CRC16_DDS_110) 0x9ecf 0x0000
  , mkCRCProps "CRC16_DECT_R" (Proxy @CRC16_DECT_R) 0x007e 0x0589
  , mkCRCProps "CRC16_DECT_X" (Proxy @CRC16_DECT_X) 0x007f 0x0000
  , mkCRCProps "CRC16_DNP" (Proxy @CRC16_DNP) 0xea82 0x66c5
  , mkCRCProps "CRC16_EN_13757" (Proxy @CRC16_EN_13757) 0xc2b7 0xa366
  , mkCRCProps "CRC16_GENIBUS" (Proxy @CRC16_GENIBUS) 0xd64e 0x1d0f
  , mkCRCProps "CRC16_GSM" (Proxy @CRC16_GSM) 0xce3c 0x1d0f
  , mkCRCProps "CRC16_IBM_3740" (Proxy @CRC16_IBM_3740) 0x29b1 0x0000
  , mkCRCProps "CRC16_IBM_SDLC" (Proxy @CRC16_IBM_SDLC) 0x906e 0xf0b8
  , mkCRCProps "CRC16_ISO_IEC_14443_3_A" (Proxy @CRC16_ISO_IEC_14443_3_A) 0xbf05 0x0000
  , mkCRCProps "CRC16_KERMIT" (Proxy @CRC16_KERMIT) 0x2189 0x0000
  , mkCRCProps "CRC16_LJ1200" (Proxy @CRC16_LJ1200) 0xbdf4 0x0000
  , mkCRCProps "CRC16_M17" (Proxy @CRC16_M17) 0x772b 0x0000
  , mkCRCProps "CRC16_MAXIM_DOW" (Proxy @CRC16_MAXIM_DOW) 0x44c2 0xb001
  , mkCRCProps "CRC16_MCRF4XX" (Proxy @CRC16_MCRF4XX) 0x6f91 0x0000
  , mkCRCProps "CRC16_MODBUS" (Proxy @CRC16_MODBUS) 0x4b37 0x0000
  , mkCRCProps "CRC16_NRSC_5" (Proxy @CRC16_NRSC_5) 0xa066 0x0000
  , mkCRCProps "CRC16_OPENSAFETY_A" (Proxy @CRC16_OPENSAFETY_A) 0x5d38 0x0000
  , mkCRCProps "CRC16_OPENSAFETY_B" (Proxy @CRC16_OPENSAFETY_B) 0x20fe 0x0000
  , mkCRCProps "CRC16_PROFIBUS" (Proxy @CRC16_PROFIBUS) 0xa819 0xe394
  , mkCRCProps "CRC16_RIELLO" (Proxy @CRC16_RIELLO) 0x63d0 0x0000
  , mkCRCProps "CRC16_SPI_FUJITSU" (Proxy @CRC16_SPI_FUJITSU) 0xe5cc 0x0000
  , mkCRCProps "CRC16_T10_DIF" (Proxy @CRC16_T10_DIF) 0xd0db 0x0000
  , mkCRCProps "CRC16_TELEDISK" (Proxy @CRC16_TELEDISK) 0x0fb3 0x0000
  , mkCRCProps "CRC16_TMS37157" (Proxy @CRC16_TMS37157) 0x26b1 0x0000
  , mkCRCProps "CRC16_UMTS" (Proxy @CRC16_UMTS) 0xfee8 0x0000
  , mkCRCProps "CRC16_USB" (Proxy @CRC16_USB) 0xb4c8 0xb001
  , mkCRCProps "CRC16_XMODEM" (Proxy @CRC16_XMODEM) 0x31c3 0x0000
  , mkCRCProps "CRC17_CAN_FD" (Proxy @CRC17_CAN_FD) 0x04f03 0x00000
  , mkCRCProps "CRC21_CAN_FD" (Proxy @CRC21_CAN_FD) 0x0ed841 0x000000
  , mkCRCProps "CRC24_BLE" (Proxy @CRC24_BLE) 0xc25a56 0x000000
  , mkCRCProps "CRC24_FLEXRAY_A" (Proxy @CRC24_FLEXRAY_A) 0x7979bd 0x000000
  , mkCRCProps "CRC24_FLEXRAY_B" (Proxy @CRC24_FLEXRAY_B) 0x1f23b8 0x000000
  , mkCRCProps "CRC24_INTERLAKEN" (Proxy @CRC24_INTERLAKEN) 0xb4f3e6 0x144e63
  , mkCRCProps "CRC24_LTE_A" (Proxy @CRC24_LTE_A) 0xcde703 0x000000
  , mkCRCProps "CRC24_LTE_B" (Proxy @CRC24_LTE_B) 0x23ef52 0x000000
  , mkCRCProps "CRC24_OPENPGP" (Proxy @CRC24_OPENPGP) 0x21cf02 0x000000
  , mkCRCProps "CRC24_OS_9" (Proxy @CRC24_OS_9) 0x200fa5 0x800fe3
  , mkCRCProps "CRC30_CDMA" (Proxy @CRC30_CDMA) 0x04c34abf 0x34efa55a
  , mkCRCProps "CRC31_PHILIPS" (Proxy @CRC31_PHILIPS) 0x0ce9e46c 0x4eaf26f1
  , mkCRCProps "CRC32_AIXM" (Proxy @CRC32_AIXM) 0x3010bf7f 0x00000000
  , mkCRCProps "CRC32_AUTOSAR" (Proxy @CRC32_AUTOSAR) 0x1697d06a 0x904cddbf
  , mkCRCProps "CRC32_BASE91_D" (Proxy @CRC32_BASE91_D) 0x87315576 0x45270551
  , mkCRCProps "CRC32_BZIP2" (Proxy @CRC32_BZIP2) 0xfc891918 0xc704dd7b
  , mkCRCProps "CRC32_CD_ROM_EDC" (Proxy @CRC32_CD_ROM_EDC) 0x6ec2edc4 0x00000000
  , mkCRCProps "CRC32_CKSUM" (Proxy @CRC32_CKSUM) 0x765e7680 0xc704dd7b
  , mkCRCProps "CRC32_ISCSI" (Proxy @CRC32_ISCSI) 0xe3069283 0xb798b438
  , mkCRCProps "CRC32_ISO_HDLC" (Proxy @CRC32_ISO_HDLC) 0xcbf43926 0xdebb20e3
  , mkCRCProps "CRC32_JAMCRC" (Proxy @CRC32_JAMCRC) 0x340bc6d9 0x00000000
  , mkCRCProps "CRC32_MEF" (Proxy @CRC32_MEF) 0xd2c22f51 0x00000000
  , mkCRCProps "CRC32_MPEG_2" (Proxy @CRC32_MPEG_2) 0x0376e6e7 0x00000000
  , mkCRCProps "CRC32_XFER" (Proxy @CRC32_XFER) 0xbd0be338 0x00000000
  , mkCRCProps "CRC40_GSM" (Proxy @CRC40_GSM) 0xd4164fc646 0xc4ff8071ff
  , mkCRCProps "CRC64_ECMA_182" (Proxy @CRC64_ECMA_182) 0x6c40df5f0b497347 0x0000000000000000
  , mkCRCProps "CRC64_GO_ISO" (Proxy @CRC64_GO_ISO) 0xb90956c775a41001 0x5300000000000000
  , mkCRCProps "CRC64_MS" (Proxy @CRC64_MS) 0x75d4b74f024eceea 0x0000000000000000
  , mkCRCProps "CRC64_REDIS" (Proxy @CRC64_REDIS) 0xe9c6d914c4b8d9ca 0x0000000000000000
  , mkCRCProps "CRC64_WE" (Proxy @CRC64_WE) 0x62ec59e3f1a4f00a 0xfcacbebd5931a992
  , mkCRCProps "CRC64_XZ" (Proxy @CRC64_XZ) 0x995dc9bbdf1939fa 0x49958c9abd7d353f
  , mkCRCProps "CRC82_DARC" (Proxy @CRC82_DARC) 0x09ea83f625023801fd612 0x000000000000000000000
  ]
