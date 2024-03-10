{-|
  Copyright   :  (C) 2024, Rowan Goemans <goemansrowan@gmail.com>
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  CRC validation
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cores.CRC where

import qualified Data.List as List
import           Data.Char (ord)
import           Data.Maybe (catMaybes, fromJust)
import           Data.Proxy (Proxy)
import qualified GHC.TypeLits as TyLit
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

dataWidthCfgtoSomeNat :: forall crcWidth n. KnownNat crcWidth => KnownNat n => Index n -> TyLit.SomeNat
dataWidthCfgtoSomeNat n = fromJust cleanDiv
  where
    cleanDiv = TyLit.someNatVal $ nearestCleanDivisor (natToNum @crcWidth) (1 + fromIntegral n)

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
  :: forall checkWords crcWidth dataWidth
   . (checkWords * dataWidth) ~ 72
  => KnownNat checkWords
  => CRCParams crcWidth dataWidth
  -> BitVector crcWidth
  -> TestTree
checkProp params@(CRCParams SNat dw@SNat _ _ _ _ _) checkVal = QC.testProperty testName okCheck
  where
    testName = ("datawidth ~ " List.++ (show (snatToNum dw :: Integer)))
    wordsToFeed = getCheckInput (SNat @checkWords) dw (_crcReflectInput params)
    resultCheckVal = digest $ List.foldl' feed (mkSoftwareCRC params) wordsToFeed
    okCheck = checkVal QC.=== resultCheckVal

crcEngineEqSoftware
  :: forall crcWidth
   . KnownNat crcWidth
  => CRCParams crcWidth 1
  -> Index 7
  -- ^ nLanes
  -> Index crcWidth
  -- ^ n * dataWidth ~ crcWidth
  -> [Maybe Integer]
  -> QC.Property
crcEngineEqSoftware params nLanesI dataWidthCfg inpI
  = go (dataWidthCfgtoSomeNat @crcWidth dataWidthCfg) (fromJust $ TyLit.someNatVal (1 + fromIntegral nLanesI))
    where
      go :: TyLit.SomeNat -> TyLit.SomeNat -> QC.Property
      go (TyLit.SomeNat dataWidthP) (TyLit.SomeNat nLanesP)
        = let nLanesSNat = proxyToSNat nLanesP
              dataWidthSNat = proxyToSNat dataWidthP
          in case (compareSNat d1 nLanesSNat, compareSNat d1 dataWidthSNat) of
            (SNatLE, SNatLE) -> prop nLanesSNat dataWidthSNat
            _                -> errorX "crcValidatorEqSoftware: Absurd3"

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
          params' = params { _crcDataWidth =  dataWidth }

          hwParams = mkParallelCRCParams params' nLanes
          crcEngine' = exposeClockResetEnable crcEngine systemClockGen resetGen enableGen hwParams

          swCRC = digest $ List.foldl' feed (mkSoftwareCRC params') (catMaybes inps)
          hwInp = Nothing : (toVecN inps)
          hwOut = crcEngine' (fromList hwInp)
          hwCRC = List.last $ sampleN (1 + List.length hwInp) hwOut

crcValidatorEqSoftware
  :: forall crcWidth
   . KnownNat crcWidth
  => CRCParams crcWidth 1
  -> Index 7
  -- ^ nLanes
  -> Index crcWidth
  -- ^ n * dataWidth ~ crcWidth
  -> [Maybe Integer]
  -> QC.Property
crcValidatorEqSoftware params@(CRCParams {..}) nLanesI dataWidthCfg inpI
  = go (dataWidthCfgtoSomeNat @crcWidth dataWidthCfg) (fromJust $ TyLit.someNatVal (1 + fromIntegral nLanesI))
    where
      go :: TyLit.SomeNat -> TyLit.SomeNat -> QC.Property
      go (TyLit.SomeNat dataWidthP) (TyLit.SomeNat nLanesP)
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
          inp :: [Maybe (BitVector dataWidth)]
          inp = fmap (fmap fromIntegral) inpI
          params' = params { _crcDataWidth =  dataWidth }

          inpJusts = catMaybes inp
          swCRC = digest $ List.foldl' feed (mkSoftwareCRC params') inpJusts
          swCRCVec :: Vec (Div crcWidth dataWidth) (BitVector dataWidth)
          swCRCVec = applyWhen _crcReflectInput reverse
                      $ bitCoerce
                      $ applyWhen (xor _crcReflectInput _crcReflectOutput) reverseBV
                      $ resize
                      $ swCRC
          inpsWithCrc = inpJusts List.++ (V.toList swCRCVec)

          hwParams = mkParallelCRCParams params' nLanes
          crcValidator' = exposeClockResetEnable crcValidator systemClockGen resetGen enableGen hwParams

          hwInp = Nothing : (toVecN $ fmap Just inpsWithCrc)
          hwOut = crcValidator' (fromList hwInp)
          hwOk = List.last $ sampleN (List.length hwInp) hwOut


mkCRCProps
  :: forall crcWidth
   . String
  -> (forall dataWidth. SNat dataWidth -> CRCParams crcWidth dataWidth)
  -> BitVector crcWidth
  -> BitVector crcWidth
  -> TestTree
mkCRCProps crcName mkParams checkVal residueVal = go $ mkParams d8
  where
    checkProp'
      :: forall checkWords n
       . (checkWords * n) ~ 72
      => KnownNat checkWords
      => SNat n
      -> TestTree
    checkProp' dw'@SNat = checkProp @checkWords (mkParams dw') checkVal
    go params@(CRCParams SNat SNat _ _ _ _ _)
      = testGroup crcName
          [ QC.testProperty "Residue" $ residueVal QC.=== (residue params)
          , testGroup "Software check"
              [ checkProp' d1, checkProp' d2, checkProp' d3, checkProp' d8
              , checkProp' d9, checkProp' d12, checkProp' d24, checkProp' d36
              , checkProp' d72
              ]
          , QC.testProperty "crcEngine ~ SoftwareCRC" $ crcEngineEqSoftware (mkParams d1)
          , QC.testProperty "crcValidator ~ SoftwareCRC" $ crcValidatorEqSoftware (mkParams d1)
          ]

-- All of the check and residue values are taken from
-- https://reveng.sourceforge.io/crc-catalogue/all.htm
tests :: TestTree
tests = testGroup "CRC"
  [ mkCRCProps "CRC3_GSM" cRC3_GSM 0x4 0x2
  , mkCRCProps "CRC3_ROHC" cRC3_ROHC 0x6 0x0
  , mkCRCProps "CRC4_G_704" cRC4_G_704 0x7 0x0
  , mkCRCProps "CRC4_INTERLAKEN" cRC4_INTERLAKEN 0xb 0x2
  , mkCRCProps "CRC5_EPC_C1G2" cRC5_EPC_C1G2 0x00 0x00
  , mkCRCProps "CRC5_G_704" cRC5_G_704 0x07 0x00
  , mkCRCProps "CRC5_USB" cRC5_USB 0x19 0x06
  , mkCRCProps "CRC6_CDMA2000_A" cRC6_CDMA2000_A 0x0d 0x00
  , mkCRCProps "CRC6_CDMA2000_B" cRC6_CDMA2000_B 0x3b 0x00
  , mkCRCProps "CRC6_DARC" cRC6_DARC 0x26 0x00
  , mkCRCProps "CRC6_G_704" cRC6_G_704 0x06 0x00
  , mkCRCProps "CRC6_GSM" cRC6_GSM 0x13 0x3a
  , mkCRCProps "CRC7_MMC" cRC7_MMC 0x75 0x00
  , mkCRCProps "CRC7_ROHC" cRC7_ROHC 0x53 0x00
  , mkCRCProps "CRC7_UMTS" cRC7_UMTS 0x61 0x00
  , mkCRCProps "CRC8_AUTOSAR" cRC8_AUTOSAR 0xdf 0x42
  , mkCRCProps "CRC8_BLUETOOTH" cRC8_BLUETOOTH 0x26 0x00
  , mkCRCProps "CRC8_CDMA2000" cRC8_CDMA2000 0xda 0x00
  , mkCRCProps "CRC8_DARC" cRC8_DARC 0x15 0x00
  , mkCRCProps "CRC8_DVB_S2" cRC8_DVB_S2 0xbc 0x00
  , mkCRCProps "CRC8_GSM_A" cRC8_GSM_A 0x37 0x00
  , mkCRCProps "CRC8_GSM_B" cRC8_GSM_B 0x94 0x53
  , mkCRCProps "CRC8_HITAG" cRC8_HITAG 0xb4 0x00
  , mkCRCProps "CRC8_I_432_1" cRC8_I_432_1 0xa1 0xac
  , mkCRCProps "CRC8_I_CODE" cRC8_I_CODE 0x7e 0x00
  , mkCRCProps "CRC8_LTE" cRC8_LTE 0xea 0x00
  , mkCRCProps "CRC8_MAXIM_DOW" cRC8_MAXIM_DOW 0xa1 0x00
  , mkCRCProps "CRC8_MIFARE_MAD" cRC8_MIFARE_MAD 0x99 0x00
  , mkCRCProps "CRC8_NRSC_5" cRC8_NRSC_5 0xf7 0x00
  , mkCRCProps "CRC8_OPENSAFETY" cRC8_OPENSAFETY 0x3e 0x00
  , mkCRCProps "CRC8_ROHC" cRC8_ROHC 0xd0 0x00
  , mkCRCProps "CRC8_SAE_J1850" cRC8_SAE_J1850 0x4b 0xc4
  , mkCRCProps "CRC8_SMBUS" cRC8_SMBUS 0xf4 0x00
  , mkCRCProps "CRC8_TECH_3250" cRC8_TECH_3250 0x97 0x00
  , mkCRCProps "CRC8_WCDMA" cRC8_WCDMA 0x25 0x00
  , mkCRCProps "CRC10_ATM" cRC10_ATM 0x199 0x000
  , mkCRCProps "CRC10_CDMA2000" cRC10_CDMA2000 0x233 0x000
  , mkCRCProps "CRC10_GSM" cRC10_GSM 0x12a 0x0c6
  , mkCRCProps "CRC11_FLEXRAY" cRC11_FLEXRAY 0x5a3 0x000
  , mkCRCProps "CRC11_UMTS" cRC11_UMTS 0x061 0x000
  , mkCRCProps "CRC12_CDMA2000" cRC12_CDMA2000 0xd4d 0x000
  , mkCRCProps "CRC12_DECT" cRC12_DECT 0xf5b 0x000
  , mkCRCProps "CRC12_GSM" cRC12_GSM 0xb34 0x178
  , mkCRCProps "CRC12_UMTS" cRC12_UMTS 0xdaf 0x000
  , mkCRCProps "CRC13_BBC" cRC13_BBC 0x04fa 0x0000
  , mkCRCProps "CRC14_DARC" cRC14_DARC 0x082d 0x0000
  , mkCRCProps "CRC14_GSM" cRC14_GSM 0x30ae 0x031e
  , mkCRCProps "CRC15_CAN" cRC15_CAN 0x059e 0x0000
  , mkCRCProps "CRC15_MPT1327" cRC15_MPT1327 0x2566 0x6815
  , mkCRCProps "CRC16_ARC" cRC16_ARC 0xbb3d 0x0000
  , mkCRCProps "CRC16_CDMA2000" cRC16_CDMA2000 0x4c06 0x0000
  , mkCRCProps "CRC16_CMS" cRC16_CMS 0xaee7 0x0000
  , mkCRCProps "CRC16_DDS_110" cRC16_DDS_110 0x9ecf 0x0000
  , mkCRCProps "CRC16_DECT_R" cRC16_DECT_R 0x007e 0x0589
  , mkCRCProps "CRC16_DECT_X" cRC16_DECT_X 0x007f 0x0000
  , mkCRCProps "CRC16_DNP" cRC16_DNP 0xea82 0x66c5
  , mkCRCProps "CRC16_EN_13757" cRC16_EN_13757 0xc2b7 0xa366
  , mkCRCProps "CRC16_GENIBUS" cRC16_GENIBUS 0xd64e 0x1d0f
  , mkCRCProps "CRC16_GSM" cRC16_GSM 0xce3c 0x1d0f
  , mkCRCProps "CRC16_IBM_3740" cRC16_IBM_3740 0x29b1 0x0000
  , mkCRCProps "CRC16_IBM_SDLC" cRC16_IBM_SDLC 0x906e 0xf0b8
  , mkCRCProps "CRC16_ISO_IEC_14443_3_A" cRC16_ISO_IEC_14443_3_A 0xbf05 0x0000
  , mkCRCProps "CRC16_KERMIT" cRC16_KERMIT 0x2189 0x0000
  , mkCRCProps "CRC16_LJ1200" cRC16_LJ1200 0xbdf4 0x0000
  , mkCRCProps "CRC16_M17" cRC16_M17 0x772b 0x0000
  , mkCRCProps "CRC16_MAXIM_DOW" cRC16_MAXIM_DOW 0x44c2 0xb001
  , mkCRCProps "CRC16_MCRF4XX" cRC16_MCRF4XX 0x6f91 0x0000
  , mkCRCProps "CRC16_MODBUS" cRC16_MODBUS 0x4b37 0x0000
  , mkCRCProps "CRC16_NRSC_5" cRC16_NRSC_5 0xa066 0x0000
  , mkCRCProps "CRC16_OPENSAFETY_A" cRC16_OPENSAFETY_A 0x5d38 0x0000
  , mkCRCProps "CRC16_OPENSAFETY_B" cRC16_OPENSAFETY_B 0x20fe 0x0000
  , mkCRCProps "CRC16_PROFIBUS" cRC16_PROFIBUS 0xa819 0xe394
  , mkCRCProps "CRC16_RIELLO" cRC16_RIELLO 0x63d0 0x0000
  , mkCRCProps "CRC16_SPI_FUJITSU" cRC16_SPI_FUJITSU 0xe5cc 0x0000
  , mkCRCProps "CRC16_T10_DIF" cRC16_T10_DIF 0xd0db 0x0000
  , mkCRCProps "CRC16_TELEDISK" cRC16_TELEDISK 0x0fb3 0x0000
  , mkCRCProps "CRC16_TMS37157" cRC16_TMS37157 0x26b1 0x0000
  , mkCRCProps "CRC16_UMTS" cRC16_UMTS 0xfee8 0x0000
  , mkCRCProps "CRC16_USB" cRC16_USB 0xb4c8 0xb001
  , mkCRCProps "CRC16_XMODEM" cRC16_XMODEM 0x31c3 0x0000
  , mkCRCProps "CRC17_CAN_FD" cRC17_CAN_FD 0x04f03 0x00000
  , mkCRCProps "CRC21_CAN_FD" cRC21_CAN_FD 0x0ed841 0x000000
  , mkCRCProps "CRC24_BLE" cRC24_BLE 0xc25a56 0x000000
  , mkCRCProps "CRC24_FLEXRAY_A" cRC24_FLEXRAY_A 0x7979bd 0x000000
  , mkCRCProps "CRC24_FLEXRAY_B" cRC24_FLEXRAY_B 0x1f23b8 0x000000
  , mkCRCProps "CRC24_INTERLAKEN" cRC24_INTERLAKEN 0xb4f3e6 0x144e63
  , mkCRCProps "CRC24_LTE_A" cRC24_LTE_A 0xcde703 0x000000
  , mkCRCProps "CRC24_LTE_B" cRC24_LTE_B 0x23ef52 0x000000
  , mkCRCProps "CRC24_OPENPGP" cRC24_OPENPGP 0x21cf02 0x000000
  , mkCRCProps "CRC24_OS_9" cRC24_OS_9 0x200fa5 0x800fe3
  , mkCRCProps "CRC30_CDMA" cRC30_CDMA 0x04c34abf 0x34efa55a
  , mkCRCProps "CRC31_PHILIPS" cRC31_PHILIPS 0x0ce9e46c 0x4eaf26f1
  , mkCRCProps "CRC32_AIXM" cRC32_AIXM 0x3010bf7f 0x00000000
  , mkCRCProps "CRC32_AUTOSAR" cRC32_AUTOSAR 0x1697d06a 0x904cddbf
  , mkCRCProps "CRC32_BASE91_D" cRC32_BASE91_D 0x87315576 0x45270551
  , mkCRCProps "CRC32_BZIP2" cRC32_BZIP2 0xfc891918 0xc704dd7b
  , mkCRCProps "CRC32_CD_ROM_EDC" cRC32_CD_ROM_EDC 0x6ec2edc4 0x00000000
  , mkCRCProps "CRC32_CKSUM" cRC32_CKSUM 0x765e7680 0xc704dd7b
  , mkCRCProps "CRC32_ISCSI" cRC32_ISCSI 0xe3069283 0xb798b438
  , mkCRCProps "CRC32_ISO_HDLC" cRC32_ISO_HDLC 0xcbf43926 0xdebb20e3
  , mkCRCProps "CRC32_JAMCRC" cRC32_JAMCRC 0x340bc6d9 0x00000000
  , mkCRCProps "CRC32_MEF" cRC32_MEF 0xd2c22f51 0x00000000
  , mkCRCProps "CRC32_MPEG_2" cRC32_MPEG_2 0x0376e6e7 0x00000000
  , mkCRCProps "CRC32_XFER" cRC32_XFER 0xbd0be338 0x00000000
  , mkCRCProps "CRC40_GSM" cRC40_GSM 0xd4164fc646 0xc4ff8071ff
  , mkCRCProps "CRC64_ECMA_182" cRC64_ECMA_182 0x6c40df5f0b497347 0x0000000000000000
  , mkCRCProps "CRC64_GO_ISO" cRC64_GO_ISO 0xb90956c775a41001 0x5300000000000000
  , mkCRCProps "CRC64_MS" cRC64_MS 0x75d4b74f024eceea 0x0000000000000000
  , mkCRCProps "CRC64_REDIS" cRC64_REDIS 0xe9c6d914c4b8d9ca 0x0000000000000000
  , mkCRCProps "CRC64_WE" cRC64_WE 0x62ec59e3f1a4f00a 0xfcacbebd5931a992
  , mkCRCProps "CRC64_XZ" cRC64_XZ 0x995dc9bbdf1939fa 0x49958c9abd7d353f
  , mkCRCProps "CRC82_DARC" cRC82_DARC 0x09ea83f625023801fd612 0x000000000000000000000
  ]
