{-|
  Copyright   :  (C) 2024, Rowan Goemans <goemansrowan@gmail.com>
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  CRC validation
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cores.Crc where

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

import           Clash.Cores.Crc.Internal
import           Clash.Cores.Crc.Catalog

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
  => KnownCrc crc
  => crc
  -> BitVector (CrcWidth crc)
  -> SNat dataWidth
  -> TestTree
checkProp crc checkVal dataWidth = QC.testProperty testName $ checkVal QC.=== resultCheckVal
  where
    CrcParams {..} = crcParams crc
    testName = ("datawidth ~ " List.++ (show (snatToNum dataWidth :: Integer)))
    wordsToFeed = getCheckInput (SNat @checkWords) dataWidth _crcReflectInput
    resultCheckVal = digest $ List.foldl' feed (mkSoftwareCrc crc dataWidth) wordsToFeed

crcEngineEqSoftware
  :: forall crc
   . KnownCrc crc
  => crc
  -- ^ Which crc
  -> Index 7
  -- ^ nLanes
  -> Index (CrcWidth crc)
  -- ^ n * dataWidth ~ crcWidth
  -> [Maybe Integer]
  -> QC.Property
crcEngineEqSoftware crc nLanesI dataWidthCfg inpI
  = go (dataWidthCfgtoSomeNat @(CrcWidth crc) dataWidthCfg) (fromJust $ someNatVal (1 + fromIntegral nLanesI))
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
      prop nLanes@SNat dataWidth@SNat = hwCrc QC.=== swCrc
        where
          inps :: [Maybe (BitVector dataWidth)]
          inps = fmap (fmap fromIntegral) inpI

          hwParams = mkCrcHardwareParams crc dataWidth nLanes
          crcEngine' = exposeClockResetEnable crcEngineFromParams systemClockGen resetGen enableGen hwParams

          swCrc = digest $ List.foldl' feed (mkSoftwareCrc crc dataWidth) (catMaybes inps)
          hwInp = Nothing : (toVecN inps)
          hwOut = crcEngine' (fromList hwInp)
          hwCrc = List.last $ sampleN (1 + List.length hwInp) hwOut

crcValidatorEqSoftware
  :: forall crc
   . KnownCrc crc
  => crc
  -- ^ Which crc
  -> Index 7
  -- ^ nLanes
  -> Index (CrcWidth crc)
  -- ^ n * dataWidth ~ crcWidth
  -> [Maybe Integer]
  -> QC.Property
crcValidatorEqSoftware crc nLanesI dataWidthCfg inpI
  = go (dataWidthCfgtoSomeNat @(CrcWidth crc) dataWidthCfg) (fromJust $ someNatVal (1 + fromIntegral nLanesI))
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
          CrcParams{..} = crcParams crc

          inp :: [Maybe (BitVector dataWidth)]
          inp = fmap (fmap fromIntegral) inpI

          inpJusts = catMaybes inp
          swCrc = digest $ List.foldl' feed (mkSoftwareCrc crc dataWidth) inpJusts
          swCrcVec :: Vec (Div (CrcWidth crc) dataWidth) (BitVector dataWidth)
          swCrcVec = applyWhen _crcReflectInput reverse
                      $ bitCoerce
                      $ applyWhen (xor _crcReflectInput _crcReflectOutput) reverseBV
                      $ resize
                      $ swCrc
          inpsWithCrc = inpJusts List.++ (V.toList swCrcVec)

          hwParams = mkCrcHardwareParams crc dataWidth nLanes
          crcValidator' = exposeClockResetEnable crcValidatorFromParams systemClockGen resetGen enableGen hwParams

          hwInp = Nothing : (toVecN $ fmap Just inpsWithCrc)
          hwOut = crcValidator' (fromList hwInp)
          hwOk = List.last $ sampleN (1 + List.length hwInp) hwOut


mkCrcProps
  :: forall (crc :: Type)
   . KnownCrc crc
  => String
  -> crc
  -> BitVector (CrcWidth crc)
  -> BitVector (CrcWidth crc)
  -> TestTree
mkCrcProps crcName crc checkVal residueVal = props
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
          , QC.testProperty "crcEngine ~ SoftwareCrc" $ crcEngineEqSoftware crc
          , QC.testProperty "crcValidator ~ SoftwareCrc" $ crcValidatorEqSoftware crc
          ]

-- All of the check and residue values are taken from
-- https://reveng.sourceforge.io/crc-catalogue/all.htm
tests :: TestTree
tests = testGroup "Crc"
  [ mkCrcProps "Crc3_gsm" crc3_gsm 0x4 0x2
  -- , mkCrcProps "Crc3_rohc" (Proxy @Crc3_rohc) 0x6 0x0
  -- , mkCrcProps "Crc4_g_704" (Proxy @Crc4_g_704) 0x7 0x0
  -- , mkCrcProps "Crc4_interlaken" (Proxy @Crc4_interlaken) 0xb 0x2
  -- , mkCrcProps "Crc5_epc_c1g2" (Proxy @Crc5_epc_c1g2) 0x00 0x00
  -- , mkCrcProps "Crc5_g_704" (Proxy @Crc5_g_704) 0x07 0x00
  -- , mkCrcProps "Crc5_usb" (Proxy @Crc5_usb) 0x19 0x06
  -- , mkCrcProps "Crc6_cdma2000_a" (Proxy @Crc6_cdma2000_a) 0x0d 0x00
  -- , mkCrcProps "Crc6_cdma2000_b" (Proxy @Crc6_cdma2000_b) 0x3b 0x00
  -- , mkCrcProps "Crc6_darc" (Proxy @Crc6_darc) 0x26 0x00
  -- , mkCrcProps "Crc6_g_704" (Proxy @Crc6_g_704) 0x06 0x00
  -- , mkCrcProps "Crc6_gsm" (Proxy @Crc6_gsm) 0x13 0x3a
  -- , mkCrcProps "Crc7_mmc" (Proxy @Crc7_mmc) 0x75 0x00
  -- , mkCrcProps "Crc7_rohc" (Proxy @Crc7_rohc) 0x53 0x00
  -- , mkCrcProps "Crc7_umts" (Proxy @Crc7_umts) 0x61 0x00
  -- , mkCrcProps "Crc8_autosar" (Proxy @Crc8_autosar) 0xdf 0x42
  -- , mkCrcProps "Crc8_bluetooth" (Proxy @Crc8_bluetooth) 0x26 0x00
  -- , mkCrcProps "Crc8_cdma2000" (Proxy @Crc8_cdma2000) 0xda 0x00
  -- , mkCrcProps "Crc8_darc" (Proxy @Crc8_darc) 0x15 0x00
  -- , mkCrcProps "Crc8_dvb_s2" (Proxy @Crc8_dvb_s2) 0xbc 0x00
  -- , mkCrcProps "Crc8_gsm_a" (Proxy @Crc8_gsm_a) 0x37 0x00
  -- , mkCrcProps "Crc8_gsm_b" (Proxy @Crc8_gsm_b) 0x94 0x53
  -- , mkCrcProps "Crc8_hitag" (Proxy @Crc8_hitag) 0xb4 0x00
  -- , mkCrcProps "Crc8_i_432_1" (Proxy @Crc8_i_432_1) 0xa1 0xac
  -- , mkCrcProps "Crc8_i_code" (Proxy @Crc8_i_code) 0x7e 0x00
  -- , mkCrcProps "Crc8_lte" (Proxy @Crc8_lte) 0xea 0x00
  -- , mkCrcProps "Crc8_maxim_dow" (Proxy @Crc8_maxim_dow) 0xa1 0x00
  -- , mkCrcProps "Crc8_mifare_mad" (Proxy @Crc8_mifare_mad) 0x99 0x00
  -- , mkCrcProps "Crc8_nrsc_5" (Proxy @Crc8_nrsc_5) 0xf7 0x00
  -- , mkCrcProps "Crc8_opensafety" (Proxy @Crc8_opensafety) 0x3e 0x00
  -- , mkCrcProps "Crc8_rohc" (Proxy @Crc8_rohc) 0xd0 0x00
  -- , mkCrcProps "Crc8_sae_j1850" (Proxy @Crc8_sae_j1850) 0x4b 0xc4
  -- , mkCrcProps "Crc8_smbus" (Proxy @Crc8_smbus) 0xf4 0x00
  -- , mkCrcProps "Crc8_tech_3250" (Proxy @Crc8_tech_3250) 0x97 0x00
  -- , mkCrcProps "Crc8_wcdma" (Proxy @Crc8_wcdma) 0x25 0x00
  -- , mkCrcProps "Crc10_atm" (Proxy @Crc10_atm) 0x199 0x000
  -- , mkCrcProps "Crc10_cdma2000" (Proxy @Crc10_cdma2000) 0x233 0x000
  -- , mkCrcProps "Crc10_gsm" (Proxy @Crc10_gsm) 0x12a 0x0c6
  -- , mkCrcProps "Crc11_flexray" (Proxy @Crc11_flexray) 0x5a3 0x000
  -- , mkCrcProps "Crc11_umts" (Proxy @Crc11_umts) 0x061 0x000
  -- , mkCrcProps "Crc12_cdma2000" (Proxy @Crc12_cdma2000) 0xd4d 0x000
  -- , mkCrcProps "Crc12_dect" (Proxy @Crc12_dect) 0xf5b 0x000
  -- , mkCrcProps "Crc12_gsm" (Proxy @Crc12_gsm) 0xb34 0x178
  -- , mkCrcProps "Crc12_umts" (Proxy @Crc12_umts) 0xdaf 0x000
  -- , mkCrcProps "Crc13_bbc" (Proxy @Crc13_bbc) 0x04fa 0x0000
  -- , mkCrcProps "Crc14_darc" (Proxy @Crc14_darc) 0x082d 0x0000
  -- , mkCrcProps "Crc14_gsm" (Proxy @Crc14_gsm) 0x30ae 0x031e
  -- , mkCrcProps "Crc15_can" (Proxy @Crc15_can) 0x059e 0x0000
  -- , mkCrcProps "Crc15_mpt1327" (Proxy @Crc15_mpt1327) 0x2566 0x6815
  -- , mkCrcProps "Crc16_arc" (Proxy @Crc16_arc) 0xbb3d 0x0000
  -- , mkCrcProps "Crc16_cdma2000" (Proxy @Crc16_cdma2000) 0x4c06 0x0000
  -- , mkCrcProps "Crc16_cms" (Proxy @Crc16_cms) 0xaee7 0x0000
  -- , mkCrcProps "Crc16_dds_110" (Proxy @Crc16_dds_110) 0x9ecf 0x0000
  -- , mkCrcProps "Crc16_dect_r" (Proxy @Crc16_dect_r) 0x007e 0x0589
  -- , mkCrcProps "Crc16_dect_x" (Proxy @Crc16_dect_x) 0x007f 0x0000
  -- , mkCrcProps "Crc16_dnp" (Proxy @Crc16_dnp) 0xea82 0x66c5
  -- , mkCrcProps "Crc16_en_13757" (Proxy @Crc16_en_13757) 0xc2b7 0xa366
  -- , mkCrcProps "Crc16_genibus" (Proxy @Crc16_genibus) 0xd64e 0x1d0f
  -- , mkCrcProps "Crc16_gsm" (Proxy @Crc16_gsm) 0xce3c 0x1d0f
  -- , mkCrcProps "Crc16_ibm_3740" (Proxy @Crc16_ibm_3740) 0x29b1 0x0000
  -- , mkCrcProps "Crc16_ibm_sdlc" (Proxy @Crc16_ibm_sdlc) 0x906e 0xf0b8
  -- , mkCrcProps "Crc16_iso_iec_14443_3_a" (Proxy @Crc16_iso_iec_14443_3_a) 0xbf05 0x0000
  -- , mkCrcProps "Crc16_kermit" (Proxy @Crc16_kermit) 0x2189 0x0000
  -- , mkCrcProps "Crc16_lj1200" (Proxy @Crc16_lj1200) 0xbdf4 0x0000
  -- , mkCrcProps "Crc16_m17" (Proxy @Crc16_m17) 0x772b 0x0000
  -- , mkCrcProps "Crc16_maxim_dow" (Proxy @Crc16_maxim_dow) 0x44c2 0xb001
  -- , mkCrcProps "Crc16_mcrf4xx" (Proxy @Crc16_mcrf4xx) 0x6f91 0x0000
  -- , mkCrcProps "Crc16_modbus" (Proxy @Crc16_modbus) 0x4b37 0x0000
  -- , mkCrcProps "Crc16_nrsc_5" (Proxy @Crc16_nrsc_5) 0xa066 0x0000
  -- , mkCrcProps "Crc16_opensafety_a" (Proxy @Crc16_opensafety_a) 0x5d38 0x0000
  -- , mkCrcProps "Crc16_opensafety_b" (Proxy @Crc16_opensafety_b) 0x20fe 0x0000
  -- , mkCrcProps "Crc16_profibus" (Proxy @Crc16_profibus) 0xa819 0xe394
  -- , mkCrcProps "Crc16_riello" (Proxy @Crc16_riello) 0x63d0 0x0000
  -- , mkCrcProps "Crc16_spi_fujitsu" (Proxy @Crc16_spi_fujitsu) 0xe5cc 0x0000
  -- , mkCrcProps "Crc16_t10_dif" (Proxy @Crc16_t10_dif) 0xd0db 0x0000
  -- , mkCrcProps "Crc16_teledisk" (Proxy @Crc16_teledisk) 0x0fb3 0x0000
  -- , mkCrcProps "Crc16_tms37157" (Proxy @Crc16_tms37157) 0x26b1 0x0000
  -- , mkCrcProps "Crc16_umts" (Proxy @Crc16_umts) 0xfee8 0x0000
  -- , mkCrcProps "Crc16_usb" (Proxy @Crc16_usb) 0xb4c8 0xb001
  -- , mkCrcProps "Crc16_xmodem" (Proxy @Crc16_xmodem) 0x31c3 0x0000
  -- , mkCrcProps "Crc17_can_fd" (Proxy @Crc17_can_fd) 0x04f03 0x00000
  -- , mkCrcProps "Crc21_can_fd" (Proxy @Crc21_can_fd) 0x0ed841 0x000000
  -- , mkCrcProps "Crc24_ble" (Proxy @Crc24_ble) 0xc25a56 0x000000
  -- , mkCrcProps "Crc24_flexray_a" (Proxy @Crc24_flexray_a) 0x7979bd 0x000000
  -- , mkCrcProps "Crc24_flexray_b" (Proxy @Crc24_flexray_b) 0x1f23b8 0x000000
  -- , mkCrcProps "Crc24_interlaken" (Proxy @Crc24_interlaken) 0xb4f3e6 0x144e63
  -- , mkCrcProps "Crc24_lte_a" (Proxy @Crc24_lte_a) 0xcde703 0x000000
  -- , mkCrcProps "Crc24_lte_b" (Proxy @Crc24_lte_b) 0x23ef52 0x000000
  -- , mkCrcProps "Crc24_openpgp" (Proxy @Crc24_openpgp) 0x21cf02 0x000000
  -- , mkCrcProps "Crc24_os_9" (Proxy @Crc24_os_9) 0x200fa5 0x800fe3
  -- , mkCrcProps "Crc30_cdma" (Proxy @Crc30_cdma) 0x04c34abf 0x34efa55a
  -- , mkCrcProps "Crc31_philips" (Proxy @Crc31_philips) 0x0ce9e46c 0x4eaf26f1
  -- , mkCrcProps "Crc32_aixm" (Proxy @Crc32_aixm) 0x3010bf7f 0x00000000
  -- , mkCrcProps "Crc32_autosar" (Proxy @Crc32_autosar) 0x1697d06a 0x904cddbf
  -- , mkCrcProps "Crc32_base91_d" (Proxy @Crc32_base91_d) 0x87315576 0x45270551
  -- , mkCrcProps "Crc32_bzip2" (Proxy @Crc32_bzip2) 0xfc891918 0xc704dd7b
  -- , mkCrcProps "Crc32_cd_rom_edc" (Proxy @Crc32_cd_rom_edc) 0x6ec2edc4 0x00000000
  -- , mkCrcProps "Crc32_cksum" (Proxy @Crc32_cksum) 0x765e7680 0xc704dd7b
  -- , mkCrcProps "Crc32_iscsi" (Proxy @Crc32_iscsi) 0xe3069283 0xb798b438
  -- , mkCrcProps "Crc32_iso_hdlc" (Proxy @Crc32_iso_hdlc) 0xcbf43926 0xdebb20e3
  -- , mkCrcProps "Crc32_jamcrc" (Proxy @Crc32_jamcrc) 0x340bc6d9 0x00000000
  -- , mkCrcProps "Crc32_mef" (Proxy @Crc32_mef) 0xd2c22f51 0x00000000
  -- , mkCrcProps "Crc32_mpeg_2" (Proxy @Crc32_mpeg_2) 0x0376e6e7 0x00000000
  -- , mkCrcProps "Crc32_xfer" (Proxy @Crc32_xfer) 0xbd0be338 0x00000000
  -- , mkCrcProps "Crc40_gsm" (Proxy @Crc40_gsm) 0xd4164fc646 0xc4ff8071ff
  -- , mkCrcProps "Crc64_ecma_182" (Proxy @Crc64_ecma_182) 0x6c40df5f0b497347 0x0000000000000000
  -- , mkCrcProps "Crc64_go_iso" (Proxy @Crc64_go_iso) 0xb90956c775a41001 0x5300000000000000
  -- , mkCrcProps "Crc64_ms" (Proxy @Crc64_ms) 0x75d4b74f024eceea 0x0000000000000000
  -- , mkCrcProps "Crc64_redis" (Proxy @Crc64_redis) 0xe9c6d914c4b8d9ca 0x0000000000000000
  -- , mkCrcProps "Crc64_we" (Proxy @Crc64_we) 0x62ec59e3f1a4f00a 0xfcacbebd5931a992
  -- , mkCrcProps "Crc64_xz" (Proxy @Crc64_xz) 0x995dc9bbdf1939fa 0x49958c9abd7d353f
  -- , mkCrcProps "Crc82_darc" (Proxy @Crc82_darc) 0x09ea83f625023801fd612 0x000000000000000000000
  ]
