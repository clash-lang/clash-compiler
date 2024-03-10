{-|
  Copyright   :  (C) 2024, Rowan Goemans <goemansrowan@gmail.com>
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

CRC parameter catalog

This module provides a variety of common CRCs
All entries are from https://reveng.sourceforge.io/crc-catalogue/all.htm

Every CRC expects a datawidth input which indicates what width in bits of data
will be fed into it.
-}

module Clash.Cores.CRC.Catalog where

import Clash.Prelude
import Clash.Cores.CRC.Internal

cRC3_GSM :: SNat dataWidth -> CRCParams 3 dataWidth
cRC3_GSM dataWidth
  = CRCParams
      { _crcWidth = SNat @3
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x3
      , _crcInitial = 0x0
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x7
      }

cRC3_ROHC :: SNat dataWidth -> CRCParams 3 dataWidth
cRC3_ROHC dataWidth
  = CRCParams
      { _crcWidth = SNat @3
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x3
      , _crcInitial = 0x7
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x0
      }

cRC4_G_704, cRC4_ITU :: SNat dataWidth -> CRCParams 4 dataWidth
cRC4_G_704 dataWidth
  = CRCParams
      { _crcWidth = SNat @4
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x3
      , _crcInitial = 0x0
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x0
      }
cRC4_ITU = cRC4_G_704

cRC4_INTERLAKEN :: SNat dataWidth -> CRCParams 4 dataWidth
cRC4_INTERLAKEN dataWidth
  = CRCParams
      { _crcWidth = SNat @4
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x3
      , _crcInitial = 0xf
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0xf
      }

cRC5_EPC_C1G2, cRC5_EPC :: SNat dataWidth -> CRCParams 5 dataWidth
cRC5_EPC_C1G2 dataWidth
  = CRCParams
      { _crcWidth = SNat @5
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x09
      , _crcInitial = 0x09
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00
      }
cRC5_EPC = cRC5_EPC_C1G2

cRC5_G_704, cRC5_ITU :: SNat dataWidth -> CRCParams 5 dataWidth
cRC5_G_704 dataWidth
  = CRCParams
      { _crcWidth = SNat @5
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x15
      , _crcInitial = 0x00
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x00
      }
cRC5_ITU = cRC5_G_704

cRC5_USB :: SNat dataWidth -> CRCParams 5 dataWidth
cRC5_USB dataWidth
  = CRCParams
      { _crcWidth = SNat @5
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x05
      , _crcInitial = 0x1f
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x1f
      }

cRC6_CDMA2000_A :: SNat dataWidth -> CRCParams 6 dataWidth
cRC6_CDMA2000_A dataWidth
  = CRCParams
      { _crcWidth = SNat @6
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x27
      , _crcInitial = 0x3f
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00
      }

cRC6_CDMA2000_B :: SNat dataWidth -> CRCParams 6 dataWidth
cRC6_CDMA2000_B dataWidth
  = CRCParams
      { _crcWidth = SNat @6
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x07
      , _crcInitial = 0x3f
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00
      }

cRC6_DARC :: SNat dataWidth -> CRCParams 6 dataWidth
cRC6_DARC dataWidth
  = CRCParams
      { _crcWidth = SNat @6
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x19
      , _crcInitial = 0x00
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x00
      }

cRC6_G_704, cRC6_ITU :: SNat dataWidth -> CRCParams 6 dataWidth
cRC6_G_704 dataWidth
  = CRCParams
      { _crcWidth = SNat @6
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x03
      , _crcInitial = 0x00
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x00
      }
cRC6_ITU = cRC6_G_704

cRC6_GSM :: SNat dataWidth -> CRCParams 6 dataWidth
cRC6_GSM dataWidth
  = CRCParams
      { _crcWidth = SNat @6
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x2f
      , _crcInitial = 0x00
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x3f
      }

cRC7_MMC :: SNat dataWidth -> CRCParams 7 dataWidth
cRC7_MMC dataWidth
  = CRCParams
      { _crcWidth = SNat @7
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x09
      , _crcInitial = 0x00
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00
      }

cRC7_ROHC :: SNat dataWidth -> CRCParams 7 dataWidth
cRC7_ROHC dataWidth
  = CRCParams
      { _crcWidth = SNat @7
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x4f
      , _crcInitial = 0x7f
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x00
      }

cRC7_UMTS :: SNat dataWidth -> CRCParams 7 dataWidth
cRC7_UMTS dataWidth
  = CRCParams
      { _crcWidth = SNat @7
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x45
      , _crcInitial = 0x00
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00
      }

cRC8_AUTOSAR :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_AUTOSAR dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x2f
      , _crcInitial = 0xff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0xff
      }

cRC8_BLUETOOTH :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_BLUETOOTH dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0xa7
      , _crcInitial = 0x00
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x00
      }

cRC8_CDMA2000 :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_CDMA2000 dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x9b
      , _crcInitial = 0xff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00
      }

cRC8_DARC :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_DARC dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x39
      , _crcInitial = 0x00
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x00
      }

cRC8_DVB_S2 :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_DVB_S2 dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0xd5
      , _crcInitial = 0x00
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00
      }

cRC8_GSM_A :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_GSM_A dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1d
      , _crcInitial = 0x00
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00
      }

cRC8_GSM_B :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_GSM_B dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x49
      , _crcInitial = 0x00
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0xff
      }

cRC8_HITAG :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_HITAG dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1d
      , _crcInitial = 0xff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00
      }

cRC8_I_432_1, cRC8_ITU :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_I_432_1 dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x07
      , _crcInitial = 0x00
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x55
      }
cRC8_ITU = cRC8_I_432_1

cRC8_I_CODE :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_I_CODE dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1d
      , _crcInitial = 0xfd
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00
      }

cRC8_LTE :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_LTE dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x9b
      , _crcInitial = 0x00
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00
      }

cRC8_MAXIM_DOW, cRC8_MAXIM :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_MAXIM_DOW dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x31
      , _crcInitial = 0x00
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x00
      }
cRC8_MAXIM = cRC8_MAXIM_DOW

cRC8_MIFARE_MAD :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_MIFARE_MAD dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1d
      , _crcInitial = 0xc7
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00
      }

cRC8_NRSC_5 :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_NRSC_5 dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x31
      , _crcInitial = 0xff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00
      }

cRC8_OPENSAFETY :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_OPENSAFETY dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x2f
      , _crcInitial = 0x00
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00
      }

cRC8_ROHC :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_ROHC dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x07
      , _crcInitial = 0xff
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x00
      }

cRC8_SAE_J1850 :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_SAE_J1850 dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1d
      , _crcInitial = 0xff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0xff
      }

cRC8_SMBUS :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_SMBUS dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x07
      , _crcInitial = 0x00
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00
      }

cRC8_TECH_3250, cRC8_AES, cRC8_ETU :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_TECH_3250 dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1d
      , _crcInitial = 0xff
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x00
      }
cRC8_AES = cRC8_TECH_3250
cRC8_ETU = cRC8_TECH_3250

cRC8_WCDMA :: SNat dataWidth -> CRCParams 8 dataWidth
cRC8_WCDMA dataWidth
  = CRCParams
      { _crcWidth = SNat @8
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x9b
      , _crcInitial = 0x00
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x00
      }

cRC10_ATM, cRC10_I_610 :: SNat dataWidth -> CRCParams 10 dataWidth
cRC10_ATM dataWidth
  = CRCParams
      { _crcWidth = SNat @10
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x233
      , _crcInitial = 0x000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x000
      }
cRC10_I_610 = cRC10_ATM

cRC10_CDMA2000 :: SNat dataWidth -> CRCParams 10 dataWidth
cRC10_CDMA2000 dataWidth
  = CRCParams
      { _crcWidth = SNat @10
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x3d9
      , _crcInitial = 0x3ff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x000
      }

cRC10_GSM :: SNat dataWidth -> CRCParams 10 dataWidth
cRC10_GSM dataWidth
  = CRCParams
      { _crcWidth = SNat @10
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x175
      , _crcInitial = 0x000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x3ff
      }

cRC11_FLEXRAY :: SNat dataWidth -> CRCParams 11 dataWidth
cRC11_FLEXRAY dataWidth
  = CRCParams
      { _crcWidth = SNat @11
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x385
      , _crcInitial = 0x01a
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x000
      }

cRC11_UMTS :: SNat dataWidth -> CRCParams 11 dataWidth
cRC11_UMTS dataWidth
  = CRCParams
      { _crcWidth = SNat @11
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x307
      , _crcInitial = 0x000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x000
      }

cRC12_CDMA2000 :: SNat dataWidth -> CRCParams 12 dataWidth
cRC12_CDMA2000 dataWidth
  = CRCParams
      { _crcWidth = SNat @12
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0xf13
      , _crcInitial = 0xfff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x000
      }

cRC12_DECT :: SNat dataWidth -> CRCParams 12 dataWidth
cRC12_DECT dataWidth
  = CRCParams
      { _crcWidth = SNat @12
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x80f
      , _crcInitial = 0x000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x000
      }

cRC12_GSM :: SNat dataWidth -> CRCParams 12 dataWidth
cRC12_GSM dataWidth
  = CRCParams
      { _crcWidth = SNat @12
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0xd31
      , _crcInitial = 0x000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0xfff
      }

cRC12_UMTS, cRC12_3GPP :: SNat dataWidth -> CRCParams 12 dataWidth
cRC12_UMTS dataWidth
  = CRCParams
      { _crcWidth = SNat @12
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x80f
      , _crcInitial = 0x000
      , _crcReflectInput = False
      , _crcReflectOutput = True
      , _crcXorOutput = 0x000
      }
cRC12_3GPP = cRC12_UMTS

cRC13_BBC :: SNat dataWidth -> CRCParams 13 dataWidth
cRC13_BBC dataWidth
  = CRCParams
      { _crcWidth = SNat @13
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1cf5
      , _crcInitial = 0x0000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000
      }

cRC14_DARC :: SNat dataWidth -> CRCParams 14 dataWidth
cRC14_DARC dataWidth
  = CRCParams
      { _crcWidth = SNat @14
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x0805
      , _crcInitial = 0x0000
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x0000
      }

cRC14_GSM :: SNat dataWidth -> CRCParams 14 dataWidth
cRC14_GSM dataWidth
  = CRCParams
      { _crcWidth = SNat @14
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x202d
      , _crcInitial = 0x0000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x3fff
      }

cRC15_CAN :: SNat dataWidth -> CRCParams 15 dataWidth
cRC15_CAN dataWidth
  = CRCParams
      { _crcWidth = SNat @15
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x4599
      , _crcInitial = 0x0000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000
      }

cRC15_MPT1327 :: SNat dataWidth -> CRCParams 15 dataWidth
cRC15_MPT1327 dataWidth
  = CRCParams
      { _crcWidth = SNat @15
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x6815
      , _crcInitial = 0x0000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0001
      }

cRC16_ARC, cRC16_IBM :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_ARC dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x8005
      , _crcInitial = 0x0000
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x0000
      }
cRC16_IBM = cRC16_ARC

cRC16_CDMA2000 :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_CDMA2000 dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0xc867
      , _crcInitial = 0xffff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000
      }

cRC16_CMS :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_CMS dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x8005
      , _crcInitial = 0xffff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000
      }

cRC16_DDS_110 :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_DDS_110 dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x8005
      , _crcInitial = 0x800d
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000
      }

cRC16_DECT_R :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_DECT_R dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x0589
      , _crcInitial = 0x0000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0001
      }

cRC16_DECT_X :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_DECT_X dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x0589
      , _crcInitial = 0x0000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000
      }

cRC16_DNP :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_DNP dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x3d65
      , _crcInitial = 0x0000
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0xffff
      }

cRC16_EN_13757 :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_EN_13757 dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x3d65
      , _crcInitial = 0x0000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0xffff
      }

cRC16_GENIBUS, cRC16_DARC, cRC16_EPC, cRC16_EPC_C1G2, cRC16_I_CODE :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_GENIBUS dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1021
      , _crcInitial = 0xffff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0xffff
      }
cRC16_DARC = cRC16_GENIBUS
cRC16_EPC = cRC16_GENIBUS
cRC16_EPC_C1G2 = cRC16_GENIBUS
cRC16_I_CODE = cRC16_GENIBUS

cRC16_GSM :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_GSM dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1021
      , _crcInitial = 0x0000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0xffff
      }

cRC16_IBM_3740, cRC16_AUTOSAR, cRC16_CCITT_FALSE :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_IBM_3740 dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1021
      , _crcInitial = 0xffff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000
      }
cRC16_AUTOSAR = cRC16_IBM_3740
cRC16_CCITT_FALSE = cRC16_IBM_3740

cRC16_IBM_SDLC, cRC16_ISO_HDLC, cRC16_ISO_IEC_14443_3_B, cRC16_X25 :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_IBM_SDLC dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1021
      , _crcInitial = 0xffff
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0xffff
      }
cRC16_ISO_HDLC = cRC16_IBM_SDLC
cRC16_ISO_IEC_14443_3_B = cRC16_IBM_SDLC
cRC16_X25 = cRC16_IBM_SDLC

cRC16_ISO_IEC_14443_3_A :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_ISO_IEC_14443_3_A dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1021
      , _crcInitial = 0xc6c6
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x0000
      }

cRC16_KERMIT, cRC16_BLUETOOTH, cRC16_CCITT, cRC16_CCITT_TRUE, cRC16_V_41_LSB :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_KERMIT dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1021
      , _crcInitial = 0x0000
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x0000
      }
cRC16_BLUETOOTH =cRC16_KERMIT
cRC16_CCITT = cRC16_KERMIT
cRC16_CCITT_TRUE = cRC16_KERMIT
cRC16_V_41_LSB = cRC16_KERMIT

cRC16_LJ1200 :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_LJ1200 dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x6f63
      , _crcInitial = 0x0000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000
      }

cRC16_M17 :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_M17 dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x5935
      , _crcInitial = 0xffff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000
      }

cRC16_MAXIM_DOW, cRC16_MAXIM :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_MAXIM_DOW dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x8005
      , _crcInitial = 0x0000
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0xffff
      }
cRC16_MAXIM = cRC16_MAXIM_DOW

cRC16_MCRF4XX :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_MCRF4XX dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1021
      , _crcInitial = 0xffff
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x0000
      }

cRC16_MODBUS :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_MODBUS dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x8005
      , _crcInitial = 0xffff
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x0000
      }

cRC16_NRSC_5 :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_NRSC_5 dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x080b
      , _crcInitial = 0xffff
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x0000
      }

cRC16_OPENSAFETY_A :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_OPENSAFETY_A dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x5935
      , _crcInitial = 0x0000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000
      }

cRC16_OPENSAFETY_B :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_OPENSAFETY_B dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x755b
      , _crcInitial = 0x0000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000
      }

cRC16_PROFIBUS, cRC16_IEC_61158_2 :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_PROFIBUS dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1dcf
      , _crcInitial = 0xffff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0xffff
      }
cRC16_IEC_61158_2 = cRC16_PROFIBUS

cRC16_RIELLO :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_RIELLO dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1021
      , _crcInitial = 0xb2aa
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x0000
      }

cRC16_SPI_FUJITSU, cRC16_AUG_CCITT :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_SPI_FUJITSU dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1021
      , _crcInitial = 0x1d0f
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000
      }
cRC16_AUG_CCITT = cRC16_SPI_FUJITSU

cRC16_T10_DIF :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_T10_DIF dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x8bb7
      , _crcInitial = 0x0000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000
      }

cRC16_TELEDISK :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_TELEDISK dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0xa097
      , _crcInitial = 0x0000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000
      }

cRC16_TMS37157 :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_TMS37157 dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1021
      , _crcInitial = 0x89ec
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x0000
      }

cRC16_UMTS, cRC16_VERIFONE, cRC16_BUYPASS :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_UMTS dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x8005
      , _crcInitial = 0x0000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000
      }
cRC16_VERIFONE = cRC16_UMTS
cRC16_BUYPASS = cRC16_UMTS

cRC16_USB :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_USB dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x8005
      , _crcInitial = 0xffff
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0xffff
      }

cRC16_XMODEM, cRC16_ACORN, cRC16_LTE, cRC16_V_41_MSB, cRC16_ZMODEM :: SNat dataWidth -> CRCParams 16 dataWidth
cRC16_XMODEM dataWidth
  = CRCParams
      { _crcWidth = SNat @16
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1021
      , _crcInitial = 0x0000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000
      }
cRC16_ACORN = cRC16_XMODEM
cRC16_LTE = cRC16_XMODEM
cRC16_V_41_MSB = cRC16_XMODEM
cRC16_ZMODEM = cRC16_XMODEM

cRC17_CAN_FD :: SNat dataWidth -> CRCParams 17 dataWidth
cRC17_CAN_FD dataWidth
  = CRCParams
      { _crcWidth = SNat @17
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1685b
      , _crcInitial = 0x00000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00000
      }

cRC21_CAN_FD :: SNat dataWidth -> CRCParams 21 dataWidth
cRC21_CAN_FD dataWidth
  = CRCParams
      { _crcWidth = SNat @21
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x102899
      , _crcInitial = 0x000000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x000000
      }

cRC24_BLE :: SNat dataWidth -> CRCParams 24 dataWidth
cRC24_BLE dataWidth
  = CRCParams
      { _crcWidth = SNat @24
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x00065b
      , _crcInitial = 0x555555
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x000000
      }

cRC24_FLEXRAY_A :: SNat dataWidth -> CRCParams 24 dataWidth
cRC24_FLEXRAY_A dataWidth
  = CRCParams
      { _crcWidth = SNat @24
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x5d6dcb
      , _crcInitial = 0xfedcba
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x000000
      }

cRC24_FLEXRAY_B :: SNat dataWidth -> CRCParams 24 dataWidth
cRC24_FLEXRAY_B dataWidth
  = CRCParams
      { _crcWidth = SNat @24
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x5d6dcb
      , _crcInitial = 0xabcdef
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x000000
      }

cRC24_INTERLAKEN :: SNat dataWidth -> CRCParams 24 dataWidth
cRC24_INTERLAKEN dataWidth
  = CRCParams
      { _crcWidth = SNat @24
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x328b63
      , _crcInitial = 0xffffff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0xffffff
      }

cRC24_LTE_A :: SNat dataWidth -> CRCParams 24 dataWidth
cRC24_LTE_A dataWidth
  = CRCParams
      { _crcWidth = SNat @24
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x864cfb
      , _crcInitial = 0x000000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x000000
      }

cRC24_LTE_B :: SNat dataWidth -> CRCParams 24 dataWidth
cRC24_LTE_B dataWidth
  = CRCParams
      { _crcWidth = SNat @24
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x800063
      , _crcInitial = 0x000000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x000000
      }

cRC24_OPENPGP :: SNat dataWidth -> CRCParams 24 dataWidth
cRC24_OPENPGP dataWidth
  = CRCParams
      { _crcWidth = SNat @24
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x864cfb
      , _crcInitial = 0xb704ce
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x000000
      }

cRC24_OS_9 :: SNat dataWidth -> CRCParams 24 dataWidth
cRC24_OS_9 dataWidth
  = CRCParams
      { _crcWidth = SNat @24
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x800063
      , _crcInitial = 0xffffff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0xffffff
      }

cRC30_CDMA :: SNat dataWidth -> CRCParams 30 dataWidth
cRC30_CDMA dataWidth
  = CRCParams
      { _crcWidth = SNat @30
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x2030b9c7
      , _crcInitial = 0x3fffffff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x3fffffff
      }

cRC31_PHILIPS :: SNat dataWidth -> CRCParams 31 dataWidth
cRC31_PHILIPS dataWidth
  = CRCParams
      { _crcWidth = SNat @31
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x04c11db7
      , _crcInitial = 0x7fffffff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x7fffffff
      }

cRC32_AIXM :: SNat dataWidth -> CRCParams 32 dataWidth
cRC32_AIXM dataWidth
  = CRCParams
      { _crcWidth = SNat @32
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x814141ab
      , _crcInitial = 0x00000000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00000000
      }

cRC32_AUTOSAR :: SNat dataWidth -> CRCParams 32 dataWidth
cRC32_AUTOSAR dataWidth
  = CRCParams
      { _crcWidth = SNat @32
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0xf4acfb13
      , _crcInitial = 0xffffffff
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0xffffffff
      }

cRC32_BASE91_D :: SNat dataWidth -> CRCParams 32 dataWidth
cRC32_BASE91_D dataWidth
  = CRCParams
      { _crcWidth = SNat @32
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0xa833982b
      , _crcInitial = 0xffffffff
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0xffffffff
      }

cRC32_BZIP2, cRC32_AAL5, cRC32_DECT_B :: SNat dataWidth -> CRCParams 32 dataWidth
cRC32_BZIP2 dataWidth
  = CRCParams
      { _crcWidth = SNat @32
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x04c11db7
      , _crcInitial = 0xffffffff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0xffffffff
      }
cRC32_AAL5 = cRC32_BZIP2
cRC32_DECT_B = cRC32_BZIP2

cRC32_CD_ROM_EDC :: SNat dataWidth -> CRCParams 32 dataWidth
cRC32_CD_ROM_EDC dataWidth
  = CRCParams
      { _crcWidth = SNat @32
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x8001801b
      , _crcInitial = 0x00000000
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x00000000
      }

cRC32_CKSUM, cRC32_POSIX :: SNat dataWidth -> CRCParams 32 dataWidth
cRC32_CKSUM dataWidth
  = CRCParams
      { _crcWidth = SNat @32
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x04c11db7
      , _crcInitial = 0x00000000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0xffffffff
      }
cRC32_POSIX = cRC32_CKSUM

cRC32_ISCSI, cRC32_BASE91_C, cRC32_CASTAGNOLI, cRC32_INTERLAKEN :: SNat dataWidth -> CRCParams 32 dataWidth
cRC32_ISCSI dataWidth
  = CRCParams
      { _crcWidth = SNat @32
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x1edc6f41
      , _crcInitial = 0xffffffff
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0xffffffff
      }
cRC32_BASE91_C = cRC32_ISCSI
cRC32_CASTAGNOLI = cRC32_ISCSI
cRC32_INTERLAKEN = cRC32_ISCSI

cRC32_ISO_HDLC, cRC32_ADCCP, cRC32_V_42, cRC32_XZ, cRC32_PKZIP, cRC32_ETHERNET :: SNat dataWidth -> CRCParams 32 dataWidth
cRC32_ISO_HDLC dataWidth
  = CRCParams
      { _crcWidth = SNat @32
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x04c11db7
      , _crcInitial = 0xffffffff
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0xffffffff
      }
cRC32_ADCCP = cRC32_ISO_HDLC
cRC32_V_42 = cRC32_ISO_HDLC
cRC32_XZ = cRC32_ISO_HDLC
cRC32_PKZIP = cRC32_ISO_HDLC
cRC32_ETHERNET = cRC32_ISO_HDLC

cRC32_JAMCRC :: SNat dataWidth -> CRCParams 32 dataWidth
cRC32_JAMCRC dataWidth
  = CRCParams
      { _crcWidth = SNat @32
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x04c11db7
      , _crcInitial = 0xffffffff
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x00000000
      }

cRC32_MEF :: SNat dataWidth -> CRCParams 32 dataWidth
cRC32_MEF dataWidth
  = CRCParams
      { _crcWidth = SNat @32
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x741b8cd7
      , _crcInitial = 0xffffffff
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x00000000
      }

cRC32_MPEG_2 :: SNat dataWidth -> CRCParams 32 dataWidth
cRC32_MPEG_2 dataWidth
  = CRCParams
      { _crcWidth = SNat @32
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x04c11db7
      , _crcInitial = 0xffffffff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00000000
      }

cRC32_XFER :: SNat dataWidth -> CRCParams 32 dataWidth
cRC32_XFER dataWidth
  = CRCParams
      { _crcWidth = SNat @32
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x000000af
      , _crcInitial = 0x00000000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x00000000
      }

cRC40_GSM :: SNat dataWidth -> CRCParams 40 dataWidth
cRC40_GSM dataWidth
  = CRCParams
      { _crcWidth = SNat @40
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x0004820009
      , _crcInitial = 0x0000000000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0xffffffffff
      }

cRC64_ECMA_182 :: SNat dataWidth -> CRCParams 64 dataWidth
cRC64_ECMA_182 dataWidth
  = CRCParams
      { _crcWidth = SNat @64
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x42f0e1eba9ea3693
      , _crcInitial = 0x0000000000000000
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0x0000000000000000
      }

cRC64_GO_ISO :: SNat dataWidth -> CRCParams 64 dataWidth
cRC64_GO_ISO dataWidth
  = CRCParams
      { _crcWidth = SNat @64
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x000000000000001b
      , _crcInitial = 0xffffffffffffffff
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0xffffffffffffffff
      }

cRC64_MS :: SNat dataWidth -> CRCParams 64 dataWidth
cRC64_MS dataWidth
  = CRCParams
      { _crcWidth = SNat @64
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x259c84cba6426349
      , _crcInitial = 0xffffffffffffffff
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x0000000000000000
      }

cRC64_REDIS :: SNat dataWidth -> CRCParams 64 dataWidth
cRC64_REDIS dataWidth
  = CRCParams
      { _crcWidth = SNat @64
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0xad93d23594c935a9
      , _crcInitial = 0x0000000000000000
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x0000000000000000
      }

cRC64_WE :: SNat dataWidth -> CRCParams 64 dataWidth
cRC64_WE dataWidth
  = CRCParams
      { _crcWidth = SNat @64
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x42f0e1eba9ea3693
      , _crcInitial = 0xffffffffffffffff
      , _crcReflectInput = False
      , _crcReflectOutput = False
      , _crcXorOutput = 0xffffffffffffffff
      }

cRC64_XZ, cRC64_ECMA :: SNat dataWidth -> CRCParams 64 dataWidth
cRC64_XZ dataWidth
  = CRCParams
      { _crcWidth = SNat @64
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x42f0e1eba9ea3693
      , _crcInitial = 0xffffffffffffffff
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0xffffffffffffffff
      }
cRC64_ECMA = cRC64_XZ

cRC82_DARC :: SNat dataWidth -> CRCParams 82 dataWidth
cRC82_DARC dataWidth
  = CRCParams
      { _crcWidth = SNat @82
      , _crcDataWidth = dataWidth
      , _crcPolynomial = 0x0308c0111011401440411
      , _crcInitial = 0x000000000000000000000
      , _crcReflectInput = True
      , _crcReflectOutput = True
      , _crcXorOutput = 0x000000000000000000000
      }
