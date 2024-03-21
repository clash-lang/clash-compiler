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
{-# LANGUAGE MultiParamTypeClasses #-}

module Clash.Cores.CRC.Catalog where

import Clash.Prelude
import Clash.Cores.CRC.Internal

data CRC3_GSM
instance KnownCRC CRC3_GSM where
  type CRCWidth CRC3_GSM = 3
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x3
    , _crcInitial = 0x0
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x7
    }

data CRC3_ROHC
instance KnownCRC CRC3_ROHC where
  type CRCWidth CRC3_ROHC = 3
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x3
    , _crcInitial = 0x7
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0
    }

data CRC4_G_704
instance KnownCRC CRC4_G_704 where
  type CRCWidth CRC4_G_704 = 4
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x3
    , _crcInitial = 0x0
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0
    }
type CRC4_ITU = CRC4_G_704

data CRC4_INTERLAKEN
instance KnownCRC CRC4_INTERLAKEN where
  type CRCWidth CRC4_INTERLAKEN = 4
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x3
    , _crcInitial = 0xf
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xf
    }

data CRC5_EPC_C1G2
instance KnownCRC CRC5_EPC_C1G2 where
  type CRCWidth CRC5_EPC_C1G2 = 5
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x09
    , _crcInitial = 0x09
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }
type CRC5_EPC = CRC5_EPC_C1G2

data CRC5_G_704
instance KnownCRC CRC5_G_704 where
  type CRCWidth CRC5_G_704 = 5
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x15
    , _crcInitial = 0x00
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }
type CRC5_ITU = CRC5_G_704

data CRC5_USB
instance KnownCRC CRC5_USB where
  type CRCWidth CRC5_USB = 5
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x05
    , _crcInitial = 0x1f
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x1f
    }

data CRC6_CDMA2000_A
instance KnownCRC CRC6_CDMA2000_A where
  type CRCWidth CRC6_CDMA2000_A = 6
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x27
    , _crcInitial = 0x3f
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data CRC6_CDMA2000_B
instance KnownCRC CRC6_CDMA2000_B where
  type CRCWidth CRC6_CDMA2000_B = 6
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x07
    , _crcInitial = 0x3f
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data CRC6_DARC
instance KnownCRC CRC6_DARC where
  type CRCWidth CRC6_DARC = 6
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x19
    , _crcInitial = 0x00
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }

data CRC6_G_704
instance KnownCRC CRC6_G_704 where
  type CRCWidth CRC6_G_704 = 6
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x03
    , _crcInitial = 0x00
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }
type CRC6_ITU = CRC6_G_704

data CRC6_GSM
instance KnownCRC CRC6_GSM where
  type CRCWidth CRC6_GSM = 6
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x2f
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x3f
    }

data CRC7_MMC
instance KnownCRC CRC7_MMC where
  type CRCWidth CRC7_MMC = 7
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x09
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data CRC7_ROHC
instance KnownCRC CRC7_ROHC where
  type CRCWidth CRC7_ROHC = 7
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x4f
    , _crcInitial = 0x7f
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }

data CRC7_UMTS
instance KnownCRC CRC7_UMTS where
  type CRCWidth CRC7_UMTS = 7
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x45
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data CRC8_AUTOSAR
instance KnownCRC CRC8_AUTOSAR where
  type CRCWidth CRC8_AUTOSAR = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x2f
    , _crcInitial = 0xff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xff
    }

data CRC8_BLUETOOTH
instance KnownCRC CRC8_BLUETOOTH where
  type CRCWidth CRC8_BLUETOOTH = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0xa7
    , _crcInitial = 0x00
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }

data CRC8_CDMA2000
instance KnownCRC CRC8_CDMA2000 where
  type CRCWidth CRC8_CDMA2000 = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x9b
    , _crcInitial = 0xff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data CRC8_DARC
instance KnownCRC CRC8_DARC where
  type CRCWidth CRC8_DARC = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x39
    , _crcInitial = 0x00
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }

data CRC8_DVB_S2
instance KnownCRC CRC8_DVB_S2 where
  type CRCWidth CRC8_DVB_S2 = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0xd5
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data CRC8_GSM_A
instance KnownCRC CRC8_GSM_A where
  type CRCWidth CRC8_GSM_A = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1d
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data CRC8_GSM_B
instance KnownCRC CRC8_GSM_B where
  type CRCWidth CRC8_GSM_B = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x49
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xff
    }

data CRC8_HITAG
instance KnownCRC CRC8_HITAG where
  type CRCWidth CRC8_HITAG = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1d
    , _crcInitial = 0xff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data CRC8_I_432_1
instance KnownCRC CRC8_I_432_1 where
  type CRCWidth CRC8_I_432_1 = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x07
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x55
    }
type CRC8_ITU = CRC8_I_432_1

data CRC8_I_CODE
instance KnownCRC CRC8_I_CODE where
  type CRCWidth CRC8_I_CODE = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1d
    , _crcInitial = 0xfd
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data CRC8_LTE
instance KnownCRC CRC8_LTE where
  type CRCWidth CRC8_LTE = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x9b
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data CRC8_MAXIM_DOW
instance KnownCRC CRC8_MAXIM_DOW where
  type CRCWidth CRC8_MAXIM_DOW = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x31
    , _crcInitial = 0x00
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }
type CRC8_MAXIM = CRC8_MAXIM_DOW

data CRC8_MIFARE_MAD
instance KnownCRC CRC8_MIFARE_MAD where
  type CRCWidth CRC8_MIFARE_MAD = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1d
    , _crcInitial = 0xc7
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data CRC8_NRSC_5
instance KnownCRC CRC8_NRSC_5 where
  type CRCWidth CRC8_NRSC_5 = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x31
    , _crcInitial = 0xff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data CRC8_OPENSAFETY
instance KnownCRC CRC8_OPENSAFETY where
  type CRCWidth CRC8_OPENSAFETY = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x2f
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data CRC8_ROHC
instance KnownCRC CRC8_ROHC where
  type CRCWidth CRC8_ROHC = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x07
    , _crcInitial = 0xff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }

data CRC8_SAE_J1850
instance KnownCRC CRC8_SAE_J1850 where
  type CRCWidth CRC8_SAE_J1850 = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1d
    , _crcInitial = 0xff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xff
    }

data CRC8_SMBUS
instance KnownCRC CRC8_SMBUS where
  type CRCWidth CRC8_SMBUS = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x07
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data CRC8_TECH_3250
instance KnownCRC CRC8_TECH_3250 where
  type CRCWidth CRC8_TECH_3250 = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1d
    , _crcInitial = 0xff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }
type CRC8_AES = CRC8_TECH_3250
type CRC8_ETU = CRC8_TECH_3250

data CRC8_WCDMA
instance KnownCRC CRC8_WCDMA where
  type CRCWidth CRC8_WCDMA = 8
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x9b
    , _crcInitial = 0x00
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }

data CRC10_ATM
instance KnownCRC CRC10_ATM where
  type CRCWidth CRC10_ATM = 10
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x233
    , _crcInitial = 0x000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000
    }
type CRC10_I_610 = CRC10_ATM

data CRC10_CDMA2000
instance KnownCRC CRC10_CDMA2000 where
  type CRCWidth CRC10_CDMA2000 = 10
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x3d9
    , _crcInitial = 0x3ff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000
    }

data CRC10_GSM
instance KnownCRC CRC10_GSM where
  type CRCWidth CRC10_GSM = 10
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x175
    , _crcInitial = 0x000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x3ff
    }

data CRC11_FLEXRAY
instance KnownCRC CRC11_FLEXRAY where
  type CRCWidth CRC11_FLEXRAY = 11
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x385
    , _crcInitial = 0x01a
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000
    }

data CRC11_UMTS
instance KnownCRC CRC11_UMTS where
  type CRCWidth CRC11_UMTS = 11
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x307
    , _crcInitial = 0x000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000
    }

data CRC12_CDMA2000
instance KnownCRC CRC12_CDMA2000 where
  type CRCWidth CRC12_CDMA2000 = 12
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0xf13
    , _crcInitial = 0xfff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000
    }

data CRC12_DECT
instance KnownCRC CRC12_DECT where
  type CRCWidth CRC12_DECT = 12
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x80f
    , _crcInitial = 0x000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000
    }

data CRC12_GSM
instance KnownCRC CRC12_GSM where
  type CRCWidth CRC12_GSM = 12
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0xd31
    , _crcInitial = 0x000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xfff
    }

data CRC12_UMTS
instance KnownCRC CRC12_UMTS where
  type CRCWidth CRC12_UMTS = 12
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x80f
    , _crcInitial = 0x000
    , _crcReflectInput = False
    , _crcReflectOutput = True
    , _crcXorOutput = 0x000
    }
type CRC12_3GPP = CRC12_UMTS

data CRC13_BBC
instance KnownCRC CRC13_BBC where
  type CRCWidth CRC13_BBC = 13
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1cf5
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data CRC14_DARC
instance KnownCRC CRC14_DARC where
  type CRCWidth CRC14_DARC = 14
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x0805
    , _crcInitial = 0x0000
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }

data CRC14_GSM
instance KnownCRC CRC14_GSM where
  type CRCWidth CRC14_GSM = 14
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x202d
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x3fff
    }

data CRC15_CAN
instance KnownCRC CRC15_CAN where
  type CRCWidth CRC15_CAN = 15
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x4599
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data CRC15_MPT1327
instance KnownCRC CRC15_MPT1327 where
  type CRCWidth CRC15_MPT1327 = 15
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x6815
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0001
    }

data CRC16_ARC
instance KnownCRC CRC16_ARC where
  type CRCWidth CRC16_ARC = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x8005
    , _crcInitial = 0x0000
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }
type CRC16_IBM = CRC16_ARC

data CRC16_CDMA2000
instance KnownCRC CRC16_CDMA2000 where
  type CRCWidth CRC16_CDMA2000 = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0xc867
    , _crcInitial = 0xffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data CRC16_CMS
instance KnownCRC CRC16_CMS where
  type CRCWidth CRC16_CMS = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x8005
    , _crcInitial = 0xffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data CRC16_DDS_110
instance KnownCRC CRC16_DDS_110 where
  type CRCWidth CRC16_DDS_110 = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x8005
    , _crcInitial = 0x800d
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data CRC16_DECT_R
instance KnownCRC CRC16_DECT_R where
  type CRCWidth CRC16_DECT_R = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x0589
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0001
    }

data CRC16_DECT_X
instance KnownCRC CRC16_DECT_X where
  type CRCWidth CRC16_DECT_X = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x0589
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data CRC16_DNP
instance KnownCRC CRC16_DNP where
  type CRCWidth CRC16_DNP = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x3d65
    , _crcInitial = 0x0000
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffff
    }

data CRC16_EN_13757
instance KnownCRC CRC16_EN_13757 where
  type CRCWidth CRC16_EN_13757 = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x3d65
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffff
    }

data CRC16_GENIBUS
instance KnownCRC CRC16_GENIBUS where
  type CRCWidth CRC16_GENIBUS = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1021
    , _crcInitial = 0xffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffff
    }
type CRC16_DARC = CRC16_GENIBUS
type CRC16_EPC = CRC16_GENIBUS
type CRC16_EPC_C1G2 = CRC16_GENIBUS
type CRC16_I_CODE = CRC16_GENIBUS

data CRC16_GSM
instance KnownCRC CRC16_GSM where
  type CRCWidth CRC16_GSM = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1021
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffff
    }

data CRC16_IBM_3740
instance KnownCRC CRC16_IBM_3740 where
  type CRCWidth CRC16_IBM_3740 = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1021
    , _crcInitial = 0xffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }
type CRC16_AUTOSAR = CRC16_IBM_3740
type CRC16_CCITT_FALSE = CRC16_IBM_3740

data CRC16_IBM_SDLC
instance KnownCRC CRC16_IBM_SDLC where
  type CRCWidth CRC16_IBM_SDLC = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1021
    , _crcInitial = 0xffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffff
    }
type CRC16_ISO_HDLC = CRC16_IBM_SDLC
type CRC16_ISO_IEC_14443_3_B = CRC16_IBM_SDLC
type CRC16_X25 = CRC16_IBM_SDLC

data CRC16_ISO_IEC_14443_3_A
instance KnownCRC CRC16_ISO_IEC_14443_3_A where
  type CRCWidth CRC16_ISO_IEC_14443_3_A = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1021
    , _crcInitial = 0xc6c6
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }

data CRC16_KERMIT
instance KnownCRC CRC16_KERMIT where
  type CRCWidth CRC16_KERMIT = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1021
    , _crcInitial = 0x0000
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }
type CRC16_BLUETOOTH = CRC16_KERMIT
type CRC16_CCITT = CRC16_KERMIT
type CRC16_CCITT_TRUE = CRC16_KERMIT
type CRC16_V_41_LSB = CRC16_KERMIT

data CRC16_LJ1200
instance KnownCRC CRC16_LJ1200 where
  type CRCWidth CRC16_LJ1200 = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x6f63
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data CRC16_M17
instance KnownCRC CRC16_M17 where
  type CRCWidth CRC16_M17 = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x5935
    , _crcInitial = 0xffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data CRC16_MAXIM_DOW
instance KnownCRC CRC16_MAXIM_DOW where
  type CRCWidth CRC16_MAXIM_DOW = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x8005
    , _crcInitial = 0x0000
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffff
    }
type CRC16_MAXIM = CRC16_MAXIM_DOW

data CRC16_MCRF4XX
instance KnownCRC CRC16_MCRF4XX where
  type CRCWidth CRC16_MCRF4XX = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1021
    , _crcInitial = 0xffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }

data CRC16_MODBUS
instance KnownCRC CRC16_MODBUS where
  type CRCWidth CRC16_MODBUS = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x8005
    , _crcInitial = 0xffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }

data CRC16_NRSC_5
instance KnownCRC CRC16_NRSC_5 where
  type CRCWidth CRC16_NRSC_5 = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x080b
    , _crcInitial = 0xffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }

data CRC16_OPENSAFETY_A
instance KnownCRC CRC16_OPENSAFETY_A where
  type CRCWidth CRC16_OPENSAFETY_A = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x5935
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data CRC16_OPENSAFETY_B
instance KnownCRC CRC16_OPENSAFETY_B where
  type CRCWidth CRC16_OPENSAFETY_B = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x755b
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data CRC16_PROFIBUS
instance KnownCRC CRC16_PROFIBUS where
  type CRCWidth CRC16_PROFIBUS = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1dcf
    , _crcInitial = 0xffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffff
    }
type CRC16_IEC_61158_2 = CRC16_PROFIBUS

data CRC16_RIELLO
instance KnownCRC CRC16_RIELLO where
  type CRCWidth CRC16_RIELLO = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1021
    , _crcInitial = 0xb2aa
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }

data CRC16_SPI_FUJITSU
instance KnownCRC CRC16_SPI_FUJITSU where
  type CRCWidth CRC16_SPI_FUJITSU = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1021
    , _crcInitial = 0x1d0f
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }
type CRC16_AUG_CCITT = CRC16_SPI_FUJITSU

data CRC16_T10_DIF
instance KnownCRC CRC16_T10_DIF where
  type CRCWidth CRC16_T10_DIF = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x8bb7
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data CRC16_TELEDISK
instance KnownCRC CRC16_TELEDISK where
  type CRCWidth CRC16_TELEDISK = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0xa097
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data CRC16_TMS37157
instance KnownCRC CRC16_TMS37157 where
  type CRCWidth CRC16_TMS37157 = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1021
    , _crcInitial = 0x89ec
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }

data CRC16_UMTS
instance KnownCRC CRC16_UMTS where
  type CRCWidth CRC16_UMTS = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x8005
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }
type CRC16_VERIFONE = CRC16_UMTS
type CRC16_BUYPASS = CRC16_UMTS

data CRC16_USB
instance KnownCRC CRC16_USB where
  type CRCWidth CRC16_USB = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x8005
    , _crcInitial = 0xffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffff
    }

data CRC16_XMODEM
instance KnownCRC CRC16_XMODEM where
  type CRCWidth CRC16_XMODEM = 16
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1021
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }
type CRC16_ACORN = CRC16_XMODEM
type CRC16_LTE = CRC16_XMODEM
type CRC16_V_41_MSB = CRC16_XMODEM
type CRC16_ZMODEM = CRC16_XMODEM

data CRC17_CAN_FD
instance KnownCRC CRC17_CAN_FD where
  type CRCWidth CRC17_CAN_FD = 17
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1685b
    , _crcInitial = 0x00000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00000
    }

data CRC21_CAN_FD
instance KnownCRC CRC21_CAN_FD where
  type CRCWidth CRC21_CAN_FD = 21
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x102899
    , _crcInitial = 0x000000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000000
    }

data CRC24_BLE
instance KnownCRC CRC24_BLE where
  type CRCWidth CRC24_BLE = 24
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x00065b
    , _crcInitial = 0x555555
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x000000
    }

data CRC24_FLEXRAY_A
instance KnownCRC CRC24_FLEXRAY_A where
  type CRCWidth CRC24_FLEXRAY_A = 24
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x5d6dcb
    , _crcInitial = 0xfedcba
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000000
    }

data CRC24_FLEXRAY_B
instance KnownCRC CRC24_FLEXRAY_B where
  type CRCWidth CRC24_FLEXRAY_B = 24
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x5d6dcb
    , _crcInitial = 0xabcdef
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000000
    }

data CRC24_INTERLAKEN
instance KnownCRC CRC24_INTERLAKEN where
  type CRCWidth CRC24_INTERLAKEN = 24
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x328b63
    , _crcInitial = 0xffffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffffff
    }

data CRC24_LTE_A
instance KnownCRC CRC24_LTE_A where
  type CRCWidth CRC24_LTE_A = 24
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x864cfb
    , _crcInitial = 0x000000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000000
    }

data CRC24_LTE_B
instance KnownCRC CRC24_LTE_B where
  type CRCWidth CRC24_LTE_B = 24
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x800063
    , _crcInitial = 0x000000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000000
    }

data CRC24_OPENPGP
instance KnownCRC CRC24_OPENPGP where
  type CRCWidth CRC24_OPENPGP = 24
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x864cfb
    , _crcInitial = 0xb704ce
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000000
    }

data CRC24_OS_9
instance KnownCRC CRC24_OS_9 where
  type CRCWidth CRC24_OS_9 = 24
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x800063
    , _crcInitial = 0xffffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffffff
    }

data CRC30_CDMA
instance KnownCRC CRC30_CDMA where
  type CRCWidth CRC30_CDMA = 30
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x2030b9c7
    , _crcInitial = 0x3fffffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x3fffffff
    }

data CRC31_PHILIPS
instance KnownCRC CRC31_PHILIPS where
  type CRCWidth CRC31_PHILIPS = 31
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x04c11db7
    , _crcInitial = 0x7fffffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x7fffffff
    }

data CRC32_AIXM
instance KnownCRC CRC32_AIXM where
  type CRCWidth CRC32_AIXM = 32
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x814141ab
    , _crcInitial = 0x00000000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00000000
    }

data CRC32_AUTOSAR
instance KnownCRC CRC32_AUTOSAR where
  type CRCWidth CRC32_AUTOSAR = 32
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0xf4acfb13
    , _crcInitial = 0xffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffffffff
    }

data CRC32_BASE91_D
instance KnownCRC CRC32_BASE91_D where
  type CRCWidth CRC32_BASE91_D = 32
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0xa833982b
    , _crcInitial = 0xffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffffffff
    }

data CRC32_BZIP2
instance KnownCRC CRC32_BZIP2 where
  type CRCWidth CRC32_BZIP2 = 32
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x04c11db7
    , _crcInitial = 0xffffffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffffffff
    }
type CRC32_AAL5 = CRC32_BZIP2
type CRC32_DECT_B = CRC32_BZIP2

data CRC32_CD_ROM_EDC
instance KnownCRC CRC32_CD_ROM_EDC where
  type CRCWidth CRC32_CD_ROM_EDC = 32
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x8001801b
    , _crcInitial = 0x00000000
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00000000
    }

data CRC32_CKSUM
instance KnownCRC CRC32_CKSUM where
  type CRCWidth CRC32_CKSUM = 32
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x04c11db7
    , _crcInitial = 0x00000000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffffffff
    }
type CRC32_POSIX = CRC32_CKSUM

data CRC32_ISCSI
instance KnownCRC CRC32_ISCSI where
  type CRCWidth CRC32_ISCSI = 32
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x1edc6f41
    , _crcInitial = 0xffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffffffff
    }
type CRC32_BASE91_C = CRC32_ISCSI
type CRC32_CASTAGNOLI = CRC32_ISCSI
type CRC32_INTERLAKEN = CRC32_ISCSI

data CRC32_ISO_HDLC
instance KnownCRC CRC32_ISO_HDLC where
  type CRCWidth CRC32_ISO_HDLC = 32
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x04c11db7
    , _crcInitial = 0xffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffffffff
    }
type CRC32_ADCCP = CRC32_ISO_HDLC
type CRC32_V_42 = CRC32_ISO_HDLC
type CRC32_XZ = CRC32_ISO_HDLC
type CRC32_PKZIP = CRC32_ISO_HDLC
type CRC32_ETHERNET = CRC32_ISO_HDLC

data CRC32_JAMCRC
instance KnownCRC CRC32_JAMCRC where
  type CRCWidth CRC32_JAMCRC = 32
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x04c11db7
    , _crcInitial = 0xffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00000000
    }

data CRC32_MEF
instance KnownCRC CRC32_MEF where
  type CRCWidth CRC32_MEF = 32
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x741b8cd7
    , _crcInitial = 0xffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00000000
    }

data CRC32_MPEG_2
instance KnownCRC CRC32_MPEG_2 where
  type CRCWidth CRC32_MPEG_2 = 32
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x04c11db7
    , _crcInitial = 0xffffffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00000000
    }

data CRC32_XFER
instance KnownCRC CRC32_XFER where
  type CRCWidth CRC32_XFER = 32
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x000000af
    , _crcInitial = 0x00000000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00000000
    }

data CRC40_GSM
instance KnownCRC CRC40_GSM where
  type CRCWidth CRC40_GSM = 40
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x0004820009
    , _crcInitial = 0x0000000000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffffffffff
    }

data CRC64_ECMA_182
instance KnownCRC CRC64_ECMA_182 where
  type CRCWidth CRC64_ECMA_182 = 64
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x42f0e1eba9ea3693
    , _crcInitial = 0x0000000000000000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000000000000000
    }

data CRC64_GO_ISO
instance KnownCRC CRC64_GO_ISO where
  type CRCWidth CRC64_GO_ISO = 64
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x000000000000001b
    , _crcInitial = 0xffffffffffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffffffffffffffff
    }

data CRC64_MS
instance KnownCRC CRC64_MS where
  type CRCWidth CRC64_MS = 64
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x259c84cba6426349
    , _crcInitial = 0xffffffffffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000000000000000
    }

data CRC64_REDIS
instance KnownCRC CRC64_REDIS where
  type CRCWidth CRC64_REDIS = 64
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0xad93d23594c935a9
    , _crcInitial = 0x0000000000000000
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000000000000000
    }

data CRC64_WE
instance KnownCRC CRC64_WE where
  type CRCWidth CRC64_WE = 64
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x42f0e1eba9ea3693
    , _crcInitial = 0xffffffffffffffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffffffffffffffff
    }

data CRC64_XZ
instance KnownCRC CRC64_XZ where
  type CRCWidth CRC64_XZ = 64
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x42f0e1eba9ea3693
    , _crcInitial = 0xffffffffffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffffffffffffffff
    }
type CRC64_ECMA = CRC64_XZ

data CRC82_DARC
instance KnownCRC CRC82_DARC where
  type CRCWidth CRC82_DARC = 82
  crcParams _ dataWidth = CRCParams
    { _crcWidth = SNat
    , _crcDataWidth = dataWidth
    , _crcPolynomial = 0x0308c0111011401440411
    , _crcInitial = 0x000000000000000000000
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x000000000000000000000
    }
