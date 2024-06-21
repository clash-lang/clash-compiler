{-|
  Copyright   :  (C) 2024, Rowan Goemans <goemansrowan@gmail.com>
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

CRC parameter catalog

This module provides a variety of common CRCs
All entries are from https://reveng.sourceforge.io/crc-catalogue/all.htm

-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Clash.Cores.Crc.Catalog where

import Clash.Prelude
import Clash.Class.HasDomain (TryDomain, TryDomainResult(NotFound))
import Clash.Cores.Crc.Internal

data Crc3_gsm = Crc3_gsm deriving Show
type instance TryDomain t Crc3_gsm = 'NotFound
instance KnownCrc Crc3_gsm where
  type CrcWidth Crc3_gsm = 3
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x3
    , _crcInitial = 0x0
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x7
    }

data Crc3_rohc = Crc3_rohc deriving Show
type instance TryDomain t Crc3_rohc = 'NotFound
instance KnownCrc Crc3_rohc where
  type CrcWidth Crc3_rohc = 3
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x3
    , _crcInitial = 0x7
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0
    }

data Crc4_g_704 = Crc4_g_704 deriving Show
type instance TryDomain t Crc4_g_704 = 'NotFound
instance KnownCrc Crc4_g_704 where
  type CrcWidth Crc4_g_704 = 4
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x3
    , _crcInitial = 0x0
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0
    }

data Crc4_itu = Crc4_itu deriving Show
type instance TryDomain t Crc4_itu = 'NotFound
instance KnownCrc Crc4_itu where
  type CrcWidth Crc4_itu = CrcWidth Crc4_g_704
  crcParams _ = crcParams Crc4_g_704

data Crc4_interlaken = Crc4_interlaken deriving Show
type instance TryDomain t Crc4_interlaken = 'NotFound
instance KnownCrc Crc4_interlaken where
  type CrcWidth Crc4_interlaken = 4
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x3
    , _crcInitial = 0xf
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xf
    }

data Crc5_epc_c1g2 = Crc5_epc_c1g2 deriving Show
type instance TryDomain t Crc5_epc_c1g2 = 'NotFound
instance KnownCrc Crc5_epc_c1g2 where
  type CrcWidth Crc5_epc_c1g2 = 5
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x09
    , _crcInitial = 0x09
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data Crc5_epc = Crc5_epc deriving Show
type instance TryDomain t Crc5_epc = 'NotFound
instance KnownCrc Crc5_epc where
  type CrcWidth Crc5_epc = CrcWidth Crc5_epc_c1g2
  crcParams _ = crcParams Crc5_epc_c1g2

data Crc5_g_704 = Crc5_g_704 deriving Show
type instance TryDomain t Crc5_g_704 = 'NotFound
instance KnownCrc Crc5_g_704 where
  type CrcWidth Crc5_g_704 = 5
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x15
    , _crcInitial = 0x00
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }

data Crc5_itu = Crc5_itu deriving Show
type instance TryDomain t Crc5_itu = 'NotFound
instance KnownCrc Crc5_itu where
  type CrcWidth Crc5_itu = CrcWidth Crc5_g_704
  crcParams _ = crcParams Crc5_g_704

data Crc5_usb = Crc5_usb deriving Show
type instance TryDomain t Crc5_usb = 'NotFound
instance KnownCrc Crc5_usb where
  type CrcWidth Crc5_usb = 5
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x05
    , _crcInitial = 0x1f
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x1f
    }

data Crc6_cdma2000_a = Crc6_cdma2000_a deriving Show
type instance TryDomain t Crc6_cdma2000_a = 'NotFound
instance KnownCrc Crc6_cdma2000_a where
  type CrcWidth Crc6_cdma2000_a = 6
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x27
    , _crcInitial = 0x3f
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data Crc6_cdma2000_b = Crc6_cdma2000_b deriving Show
type instance TryDomain t Crc6_cdma2000_b = 'NotFound
instance KnownCrc Crc6_cdma2000_b where
  type CrcWidth Crc6_cdma2000_b = 6
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x07
    , _crcInitial = 0x3f
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data Crc6_darc = Crc6_darc deriving Show
type instance TryDomain t Crc6_darc = 'NotFound
instance KnownCrc Crc6_darc where
  type CrcWidth Crc6_darc = 6
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x19
    , _crcInitial = 0x00
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }

data Crc6_g_704 = Crc6_g_704 deriving Show
type instance TryDomain t Crc6_g_704 = 'NotFound
instance KnownCrc Crc6_g_704 where
  type CrcWidth Crc6_g_704 = 6
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x03
    , _crcInitial = 0x00
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }

data Crc6_itu = Crc6_itu deriving Show
type instance TryDomain t Crc6_itu = 'NotFound
instance KnownCrc Crc6_itu where
  type CrcWidth Crc6_itu = CrcWidth Crc6_g_704
  crcParams _ = crcParams Crc6_g_704

data Crc6_gsm = Crc6_gsm deriving Show
type instance TryDomain t Crc6_gsm = 'NotFound
instance KnownCrc Crc6_gsm where
  type CrcWidth Crc6_gsm = 6
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x2f
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x3f
    }

data Crc7_mmc = Crc7_mmc deriving Show
type instance TryDomain t Crc7_mmc = 'NotFound
instance KnownCrc Crc7_mmc where
  type CrcWidth Crc7_mmc = 7
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x09
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data Crc7_rohc = Crc7_rohc deriving Show
type instance TryDomain t Crc7_rohc = 'NotFound
instance KnownCrc Crc7_rohc where
  type CrcWidth Crc7_rohc = 7
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x4f
    , _crcInitial = 0x7f
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }

data Crc7_umts = Crc7_umts deriving Show
type instance TryDomain t Crc7_umts = 'NotFound
instance KnownCrc Crc7_umts where
  type CrcWidth Crc7_umts = 7
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x45
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data Crc8_autosar = Crc8_autosar deriving Show
type instance TryDomain t Crc8_autosar = 'NotFound
instance KnownCrc Crc8_autosar where
  type CrcWidth Crc8_autosar = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x2f
    , _crcInitial = 0xff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xff
    }

data Crc8_bluetooth = Crc8_bluetooth deriving Show
type instance TryDomain t Crc8_bluetooth = 'NotFound
instance KnownCrc Crc8_bluetooth where
  type CrcWidth Crc8_bluetooth = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0xa7
    , _crcInitial = 0x00
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }

data Crc8_cdma2000 = Crc8_cdma2000 deriving Show
type instance TryDomain t Crc8_cdma2000 = 'NotFound
instance KnownCrc Crc8_cdma2000 where
  type CrcWidth Crc8_cdma2000 = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x9b
    , _crcInitial = 0xff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data Crc8_darc = Crc8_darc deriving Show
type instance TryDomain t Crc8_darc = 'NotFound
instance KnownCrc Crc8_darc where
  type CrcWidth Crc8_darc = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x39
    , _crcInitial = 0x00
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }

data Crc8_dvb_s2 = Crc8_dvb_s2 deriving Show
type instance TryDomain t Crc8_dvb_s2 = 'NotFound
instance KnownCrc Crc8_dvb_s2 where
  type CrcWidth Crc8_dvb_s2 = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0xd5
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data Crc8_gsm_a = Crc8_gsm_a deriving Show
type instance TryDomain t Crc8_gsm_a = 'NotFound
instance KnownCrc Crc8_gsm_a where
  type CrcWidth Crc8_gsm_a = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1d
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data Crc8_gsm_b = Crc8_gsm_b deriving Show
type instance TryDomain t Crc8_gsm_b = 'NotFound
instance KnownCrc Crc8_gsm_b where
  type CrcWidth Crc8_gsm_b = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x49
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xff
    }

data Crc8_hitag = Crc8_hitag deriving Show
type instance TryDomain t Crc8_hitag = 'NotFound
instance KnownCrc Crc8_hitag where
  type CrcWidth Crc8_hitag = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1d
    , _crcInitial = 0xff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data Crc8_i_432_1 = Crc8_i_432_1 deriving Show
type instance TryDomain t Crc8_i_432_1 = 'NotFound
instance KnownCrc Crc8_i_432_1 where
  type CrcWidth Crc8_i_432_1 = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x07
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x55
    }

data Crc8_itu = Crc8_itu deriving Show
type instance TryDomain t Crc8_itu = 'NotFound
instance KnownCrc Crc8_itu where
  type CrcWidth Crc8_itu = CrcWidth Crc8_i_432_1
  crcParams _ = crcParams Crc8_i_432_1

data Crc8_i_code = Crc8_i_code deriving Show
type instance TryDomain t Crc8_i_code = 'NotFound
instance KnownCrc Crc8_i_code where
  type CrcWidth Crc8_i_code = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1d
    , _crcInitial = 0xfd
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data Crc8_lte = Crc8_lte deriving Show
type instance TryDomain t Crc8_lte = 'NotFound
instance KnownCrc Crc8_lte where
  type CrcWidth Crc8_lte = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x9b
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data Crc8_maxim_dow = Crc8_maxim_dow deriving Show
type instance TryDomain t Crc8_maxim_dow = 'NotFound
instance KnownCrc Crc8_maxim_dow where
  type CrcWidth Crc8_maxim_dow = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x31
    , _crcInitial = 0x00
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }

data Crc8_maxim = Crc8_maxim deriving Show
type instance TryDomain t Crc8_maxim = 'NotFound
instance KnownCrc Crc8_maxim where
  type CrcWidth Crc8_maxim = CrcWidth Crc8_maxim_dow
  crcParams _ = crcParams Crc8_maxim_dow

data Crc8_mifare_mad = Crc8_mifare_mad deriving Show
type instance TryDomain t Crc8_mifare_mad = 'NotFound
instance KnownCrc Crc8_mifare_mad where
  type CrcWidth Crc8_mifare_mad = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1d
    , _crcInitial = 0xc7
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data Crc8_nrsc_5 = Crc8_nrsc_5 deriving Show
type instance TryDomain t Crc8_nrsc_5 = 'NotFound
instance KnownCrc Crc8_nrsc_5 where
  type CrcWidth Crc8_nrsc_5 = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x31
    , _crcInitial = 0xff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data Crc8_opensafety = Crc8_opensafety deriving Show
type instance TryDomain t Crc8_opensafety = 'NotFound
instance KnownCrc Crc8_opensafety where
  type CrcWidth Crc8_opensafety = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x2f
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data Crc8_rohc = Crc8_rohc deriving Show
type instance TryDomain t Crc8_rohc = 'NotFound
instance KnownCrc Crc8_rohc where
  type CrcWidth Crc8_rohc = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x07
    , _crcInitial = 0xff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }

data Crc8_sae_j1850 = Crc8_sae_j1850 deriving Show
type instance TryDomain t Crc8_sae_j1850 = 'NotFound
instance KnownCrc Crc8_sae_j1850 where
  type CrcWidth Crc8_sae_j1850 = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1d
    , _crcInitial = 0xff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xff
    }

data Crc8_smbus = Crc8_smbus deriving Show
type instance TryDomain t Crc8_smbus = 'NotFound
instance KnownCrc Crc8_smbus where
  type CrcWidth Crc8_smbus = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x07
    , _crcInitial = 0x00
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00
    }

data Crc8_tech_3250 = Crc8_tech_3250 deriving Show
type instance TryDomain t Crc8_tech_3250 = 'NotFound
instance KnownCrc Crc8_tech_3250 where
  type CrcWidth Crc8_tech_3250 = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1d
    , _crcInitial = 0xff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }

data Crc8_aes = Crc8_aes deriving Show
type instance TryDomain t Crc8_aes = 'NotFound
instance KnownCrc Crc8_aes where
  type CrcWidth Crc8_aes = CrcWidth Crc8_tech_3250
  crcParams _ = crcParams Crc8_tech_3250

data Crc8_etu = Crc8_etu deriving Show
type instance TryDomain t Crc8_etu = 'NotFound
instance KnownCrc Crc8_etu where
  type CrcWidth Crc8_etu = CrcWidth Crc8_tech_3250
  crcParams _ = crcParams Crc8_tech_3250

data Crc8_wcdma = Crc8_wcdma deriving Show
type instance TryDomain t Crc8_wcdma = 'NotFound
instance KnownCrc Crc8_wcdma where
  type CrcWidth Crc8_wcdma = 8
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x9b
    , _crcInitial = 0x00
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00
    }

data Crc10_atm = Crc10_atm deriving Show
type instance TryDomain t Crc10_atm = 'NotFound
instance KnownCrc Crc10_atm where
  type CrcWidth Crc10_atm = 10
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x233
    , _crcInitial = 0x000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000
    }

data Crc10_i_610 = Crc10_i_610 deriving Show
type instance TryDomain t Crc10_i_610 = 'NotFound
instance KnownCrc Crc10_i_610 where
  type CrcWidth Crc10_i_610 = CrcWidth Crc10_atm
  crcParams _ = crcParams Crc10_atm

data Crc10_cdma2000 = Crc10_cdma2000 deriving Show
type instance TryDomain t Crc10_cdma2000 = 'NotFound
instance KnownCrc Crc10_cdma2000 where
  type CrcWidth Crc10_cdma2000 = 10
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x3d9
    , _crcInitial = 0x3ff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000
    }

data Crc10_gsm = Crc10_gsm deriving Show
type instance TryDomain t Crc10_gsm = 'NotFound
instance KnownCrc Crc10_gsm where
  type CrcWidth Crc10_gsm = 10
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x175
    , _crcInitial = 0x000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x3ff
    }

data Crc11_flexray = Crc11_flexray deriving Show
type instance TryDomain t Crc11_flexray = 'NotFound
instance KnownCrc Crc11_flexray where
  type CrcWidth Crc11_flexray = 11
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x385
    , _crcInitial = 0x01a
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000
    }

data Crc11_umts = Crc11_umts deriving Show
type instance TryDomain t Crc11_umts = 'NotFound
instance KnownCrc Crc11_umts where
  type CrcWidth Crc11_umts = 11
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x307
    , _crcInitial = 0x000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000
    }

data Crc12_cdma2000 = Crc12_cdma2000 deriving Show
type instance TryDomain t Crc12_cdma2000 = 'NotFound
instance KnownCrc Crc12_cdma2000 where
  type CrcWidth Crc12_cdma2000 = 12
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0xf13
    , _crcInitial = 0xfff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000
    }

data Crc12_dect = Crc12_dect deriving Show
type instance TryDomain t Crc12_dect = 'NotFound
instance KnownCrc Crc12_dect where
  type CrcWidth Crc12_dect = 12
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x80f
    , _crcInitial = 0x000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000
    }

data Crc12_gsm = Crc12_gsm deriving Show
type instance TryDomain t Crc12_gsm = 'NotFound
instance KnownCrc Crc12_gsm where
  type CrcWidth Crc12_gsm = 12
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0xd31
    , _crcInitial = 0x000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xfff
    }

data Crc12_umts = Crc12_umts deriving Show
type instance TryDomain t Crc12_umts = 'NotFound
instance KnownCrc Crc12_umts where
  type CrcWidth Crc12_umts = 12
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x80f
    , _crcInitial = 0x000
    , _crcReflectInput = False
    , _crcReflectOutput = True
    , _crcXorOutput = 0x000
    }

data Crc12_3gpp = Crc12_3gpp deriving Show
type instance TryDomain t Crc12_3gpp = 'NotFound
instance KnownCrc Crc12_3gpp where
  type CrcWidth Crc12_3gpp = CrcWidth Crc12_umts
  crcParams _ = crcParams Crc12_umts

data Crc13_bbc = Crc13_bbc deriving Show
type instance TryDomain t Crc13_bbc = 'NotFound
instance KnownCrc Crc13_bbc where
  type CrcWidth Crc13_bbc = 13
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1cf5
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data Crc14_darc = Crc14_darc deriving Show
type instance TryDomain t Crc14_darc = 'NotFound
instance KnownCrc Crc14_darc where
  type CrcWidth Crc14_darc = 14
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x0805
    , _crcInitial = 0x0000
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }

data Crc14_gsm = Crc14_gsm deriving Show
type instance TryDomain t Crc14_gsm = 'NotFound
instance KnownCrc Crc14_gsm where
  type CrcWidth Crc14_gsm = 14
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x202d
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x3fff
    }

data Crc15_can = Crc15_can deriving Show
type instance TryDomain t Crc15_can = 'NotFound
instance KnownCrc Crc15_can where
  type CrcWidth Crc15_can = 15
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x4599
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data Crc15_mpt1327 = Crc15_mpt1327 deriving Show
type instance TryDomain t Crc15_mpt1327 = 'NotFound
instance KnownCrc Crc15_mpt1327 where
  type CrcWidth Crc15_mpt1327 = 15
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x6815
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0001
    }

data Crc16_arc = Crc16_arc deriving Show
type instance TryDomain t Crc16_arc = 'NotFound
instance KnownCrc Crc16_arc where
  type CrcWidth Crc16_arc = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x8005
    , _crcInitial = 0x0000
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }

data Crc16_ibm = Crc16_ibm deriving Show
type instance TryDomain t Crc16_ibm = 'NotFound
instance KnownCrc Crc16_ibm where
  type CrcWidth Crc16_ibm = CrcWidth Crc16_arc
  crcParams _ = crcParams Crc16_arc

data Crc16_cdma2000 = Crc16_cdma2000 deriving Show
type instance TryDomain t Crc16_cdma2000 = 'NotFound
instance KnownCrc Crc16_cdma2000 where
  type CrcWidth Crc16_cdma2000 = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0xc867
    , _crcInitial = 0xffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data Crc16_cms = Crc16_cms deriving Show
type instance TryDomain t Crc16_cms = 'NotFound
instance KnownCrc Crc16_cms where
  type CrcWidth Crc16_cms = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x8005
    , _crcInitial = 0xffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data Crc16_dds_110 = Crc16_dds_110 deriving Show
type instance TryDomain t Crc16_dds_110 = 'NotFound
instance KnownCrc Crc16_dds_110 where
  type CrcWidth Crc16_dds_110 = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x8005
    , _crcInitial = 0x800d
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data Crc16_dect_r = Crc16_dect_r deriving Show
type instance TryDomain t Crc16_dect_r = 'NotFound
instance KnownCrc Crc16_dect_r where
  type CrcWidth Crc16_dect_r = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x0589
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0001
    }

data Crc16_dect_x = Crc16_dect_x deriving Show
type instance TryDomain t Crc16_dect_x = 'NotFound
instance KnownCrc Crc16_dect_x where
  type CrcWidth Crc16_dect_x = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x0589
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data Crc16_dnp = Crc16_dnp deriving Show
type instance TryDomain t Crc16_dnp = 'NotFound
instance KnownCrc Crc16_dnp where
  type CrcWidth Crc16_dnp = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x3d65
    , _crcInitial = 0x0000
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffff
    }

data Crc16_en_13757 = Crc16_en_13757 deriving Show
type instance TryDomain t Crc16_en_13757 = 'NotFound
instance KnownCrc Crc16_en_13757 where
  type CrcWidth Crc16_en_13757 = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x3d65
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffff
    }

data Crc16_genibus = Crc16_genibus deriving Show
type instance TryDomain t Crc16_genibus = 'NotFound
instance KnownCrc Crc16_genibus where
  type CrcWidth Crc16_genibus = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1021
    , _crcInitial = 0xffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffff
    }

data Crc16_darc = Crc16_darc deriving Show
type instance TryDomain t Crc16_darc = 'NotFound
instance KnownCrc Crc16_darc where
  type CrcWidth Crc16_darc = CrcWidth Crc16_genibus
  crcParams _ = crcParams Crc16_genibus

data Crc16_epc = Crc16_epc deriving Show
type instance TryDomain t Crc16_epc = 'NotFound
instance KnownCrc Crc16_epc where
  type CrcWidth Crc16_epc = CrcWidth Crc16_genibus
  crcParams _ = crcParams Crc16_genibus

data Crc16_epc_c1g2 = Crc16_epc_c1g2 deriving Show
type instance TryDomain t Crc16_epc_c1g2 = 'NotFound
instance KnownCrc Crc16_epc_c1g2 where
  type CrcWidth Crc16_epc_c1g2 = CrcWidth Crc16_genibus
  crcParams _ = crcParams Crc16_genibus

data Crc16_i_code = Crc16_i_code deriving Show
type instance TryDomain t Crc16_i_code = 'NotFound
instance KnownCrc Crc16_i_code where
  type CrcWidth Crc16_i_code = CrcWidth Crc16_genibus
  crcParams _ = crcParams Crc16_genibus

data Crc16_gsm = Crc16_gsm deriving Show
type instance TryDomain t Crc16_gsm = 'NotFound
instance KnownCrc Crc16_gsm where
  type CrcWidth Crc16_gsm = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1021
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffff
    }

data Crc16_ibm_3740 = Crc16_ibm_3740 deriving Show
type instance TryDomain t Crc16_ibm_3740 = 'NotFound
instance KnownCrc Crc16_ibm_3740 where
  type CrcWidth Crc16_ibm_3740 = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1021
    , _crcInitial = 0xffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data Crc16_autosar = Crc16_autosar deriving Show
type instance TryDomain t Crc16_autosar = 'NotFound
instance KnownCrc Crc16_autosar where
  type CrcWidth Crc16_autosar = CrcWidth Crc16_ibm_3740
  crcParams _ = crcParams Crc16_ibm_3740

data Crc16_ccitt_false = Crc16_ccitt_false deriving Show
type instance TryDomain t Crc16_ccitt_false = 'NotFound
instance KnownCrc Crc16_ccitt_false where
  type CrcWidth Crc16_ccitt_false = CrcWidth Crc16_ibm_3740
  crcParams _ = crcParams Crc16_ibm_3740

data Crc16_ibm_sdlc = Crc16_ibm_sdlc deriving Show
type instance TryDomain t Crc16_ibm_sdlc = 'NotFound
instance KnownCrc Crc16_ibm_sdlc where
  type CrcWidth Crc16_ibm_sdlc = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1021
    , _crcInitial = 0xffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffff
    }

data Crc16_iso_hdlc = Crc16_iso_hdlc deriving Show
type instance TryDomain t Crc16_iso_hdlc = 'NotFound
instance KnownCrc Crc16_iso_hdlc where
  type CrcWidth Crc16_iso_hdlc = CrcWidth Crc16_ibm_sdlc
  crcParams _ = crcParams Crc16_ibm_sdlc

data Crc16_iso_iec_14443_3_b = Crc16_iso_iec_14443_3_b deriving Show
type instance TryDomain t Crc16_iso_iec_14443_3_b = 'NotFound
instance KnownCrc Crc16_iso_iec_14443_3_b where
  type CrcWidth Crc16_iso_iec_14443_3_b = CrcWidth Crc16_ibm_sdlc
  crcParams _ = crcParams Crc16_ibm_sdlc

data Crc16_x25 = Crc16_x25 deriving Show
type instance TryDomain t Crc16_x25 = 'NotFound
instance KnownCrc Crc16_x25 where
  type CrcWidth Crc16_x25 = CrcWidth Crc16_ibm_sdlc
  crcParams _ = crcParams Crc16_ibm_sdlc

data Crc16_iso_iec_14443_3_a = Crc16_iso_iec_14443_3_a deriving Show
type instance TryDomain t Crc16_iso_iec_14443_3_a = 'NotFound
instance KnownCrc Crc16_iso_iec_14443_3_a where
  type CrcWidth Crc16_iso_iec_14443_3_a = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1021
    , _crcInitial = 0xc6c6
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }

data Crc16_kermit = Crc16_kermit deriving Show
type instance TryDomain t Crc16_kermit = 'NotFound
instance KnownCrc Crc16_kermit where
  type CrcWidth Crc16_kermit = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1021
    , _crcInitial = 0x0000
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }

data Crc16_bluetooth = Crc16_bluetooth deriving Show
type instance TryDomain t Crc16_bluetooth = 'NotFound
instance KnownCrc Crc16_bluetooth where
  type CrcWidth Crc16_bluetooth = CrcWidth Crc16_kermit
  crcParams _ = crcParams Crc16_kermit

data Crc16_ccitt = Crc16_ccitt deriving Show
type instance TryDomain t Crc16_ccitt = 'NotFound
instance KnownCrc Crc16_ccitt where
  type CrcWidth Crc16_ccitt = CrcWidth Crc16_kermit
  crcParams _ = crcParams Crc16_kermit

data Crc16_ccitt_true = Crc16_ccitt_true deriving Show
type instance TryDomain t Crc16_ccitt_true = 'NotFound
instance KnownCrc Crc16_ccitt_true where
  type CrcWidth Crc16_ccitt_true = CrcWidth Crc16_kermit
  crcParams _ = crcParams Crc16_kermit

data Crc16_v_41_lsb = Crc16_v_41_lsb deriving Show
type instance TryDomain t Crc16_v_41_lsb = 'NotFound
instance KnownCrc Crc16_v_41_lsb where
  type CrcWidth Crc16_v_41_lsb = CrcWidth Crc16_kermit
  crcParams _ = crcParams Crc16_kermit

data Crc16_lj1200 = Crc16_lj1200 deriving Show
type instance TryDomain t Crc16_lj1200 = 'NotFound
instance KnownCrc Crc16_lj1200 where
  type CrcWidth Crc16_lj1200 = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x6f63
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data Crc16_m17 = Crc16_m17 deriving Show
type instance TryDomain t Crc16_m17 = 'NotFound
instance KnownCrc Crc16_m17 where
  type CrcWidth Crc16_m17 = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x5935
    , _crcInitial = 0xffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data Crc16_maxim_dow = Crc16_maxim_dow deriving Show
type instance TryDomain t Crc16_maxim_dow = 'NotFound
instance KnownCrc Crc16_maxim_dow where
  type CrcWidth Crc16_maxim_dow = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x8005
    , _crcInitial = 0x0000
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffff
    }

data Crc16_maxim = Crc16_maxim deriving Show
type instance TryDomain t Crc16_maxim = 'NotFound
instance KnownCrc Crc16_maxim where
  type CrcWidth Crc16_maxim = CrcWidth Crc16_maxim_dow
  crcParams _ = crcParams Crc16_maxim_dow

data Crc16_mcrf4xx = Crc16_mcrf4xx deriving Show
type instance TryDomain t Crc16_mcrf4xx = 'NotFound
instance KnownCrc Crc16_mcrf4xx where
  type CrcWidth Crc16_mcrf4xx = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1021
    , _crcInitial = 0xffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }

data Crc16_modbus = Crc16_modbus deriving Show
type instance TryDomain t Crc16_modbus = 'NotFound
instance KnownCrc Crc16_modbus where
  type CrcWidth Crc16_modbus = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x8005
    , _crcInitial = 0xffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }

data Crc16_nrsc_5 = Crc16_nrsc_5 deriving Show
type instance TryDomain t Crc16_nrsc_5 = 'NotFound
instance KnownCrc Crc16_nrsc_5 where
  type CrcWidth Crc16_nrsc_5 = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x080b
    , _crcInitial = 0xffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }

data Crc16_opensafety_a = Crc16_opensafety_a deriving Show
type instance TryDomain t Crc16_opensafety_a = 'NotFound
instance KnownCrc Crc16_opensafety_a where
  type CrcWidth Crc16_opensafety_a = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x5935
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data Crc16_opensafety_b = Crc16_opensafety_b deriving Show
type instance TryDomain t Crc16_opensafety_b = 'NotFound
instance KnownCrc Crc16_opensafety_b where
  type CrcWidth Crc16_opensafety_b = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x755b
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data Crc16_profibus = Crc16_profibus deriving Show
type instance TryDomain t Crc16_profibus = 'NotFound
instance KnownCrc Crc16_profibus where
  type CrcWidth Crc16_profibus = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1dcf
    , _crcInitial = 0xffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffff
    }

data Crc16_iec_61158_2 = Crc16_iec_61158_2 deriving Show
type instance TryDomain t Crc16_iec_61158_2 = 'NotFound
instance KnownCrc Crc16_iec_61158_2 where
  type CrcWidth Crc16_iec_61158_2 = CrcWidth Crc16_profibus
  crcParams _ = crcParams Crc16_profibus

data Crc16_riello = Crc16_riello deriving Show
type instance TryDomain t Crc16_riello = 'NotFound
instance KnownCrc Crc16_riello where
  type CrcWidth Crc16_riello = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1021
    , _crcInitial = 0xb2aa
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }

data Crc16_spi_fujitsu = Crc16_spi_fujitsu deriving Show
type instance TryDomain t Crc16_spi_fujitsu = 'NotFound
instance KnownCrc Crc16_spi_fujitsu where
  type CrcWidth Crc16_spi_fujitsu = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1021
    , _crcInitial = 0x1d0f
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data Crc16_aug_ccitt = Crc16_aug_ccitt deriving Show
type instance TryDomain t Crc16_aug_ccitt = 'NotFound
instance KnownCrc Crc16_aug_ccitt where
  type CrcWidth Crc16_aug_ccitt = CrcWidth Crc16_spi_fujitsu
  crcParams _ = crcParams Crc16_spi_fujitsu

data Crc16_t10_dif = Crc16_t10_dif deriving Show
type instance TryDomain t Crc16_t10_dif = 'NotFound
instance KnownCrc Crc16_t10_dif where
  type CrcWidth Crc16_t10_dif = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x8bb7
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data Crc16_teledisk = Crc16_teledisk deriving Show
type instance TryDomain t Crc16_teledisk = 'NotFound
instance KnownCrc Crc16_teledisk where
  type CrcWidth Crc16_teledisk = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0xa097
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data Crc16_tms37157 = Crc16_tms37157 deriving Show
type instance TryDomain t Crc16_tms37157 = 'NotFound
instance KnownCrc Crc16_tms37157 where
  type CrcWidth Crc16_tms37157 = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1021
    , _crcInitial = 0x89ec
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000
    }

data Crc16_umts = Crc16_umts deriving Show
type instance TryDomain t Crc16_umts = 'NotFound
instance KnownCrc Crc16_umts where
  type CrcWidth Crc16_umts = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x8005
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data Crc16_verifone = Crc16_verifone deriving Show
type instance TryDomain t Crc16_verifone = 'NotFound
instance KnownCrc Crc16_verifone where
  type CrcWidth Crc16_verifone = CrcWidth Crc16_umts
  crcParams _ = crcParams Crc16_umts

data Crc16_buypass = Crc16_buypass deriving Show
type instance TryDomain t Crc16_buypass = 'NotFound
instance KnownCrc Crc16_buypass where
  type CrcWidth Crc16_buypass = CrcWidth Crc16_umts
  crcParams _ = crcParams Crc16_umts

data Crc16_usb = Crc16_usb deriving Show
type instance TryDomain t Crc16_usb = 'NotFound
instance KnownCrc Crc16_usb where
  type CrcWidth Crc16_usb = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x8005
    , _crcInitial = 0xffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffff
    }

data Crc16_xmodem = Crc16_xmodem deriving Show
type instance TryDomain t Crc16_xmodem = 'NotFound
instance KnownCrc Crc16_xmodem where
  type CrcWidth Crc16_xmodem = 16
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1021
    , _crcInitial = 0x0000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000
    }

data Crc16_acorn = Crc16_acorn deriving Show
type instance TryDomain t Crc16_acorn = 'NotFound
instance KnownCrc Crc16_acorn where
  type CrcWidth Crc16_acorn = CrcWidth Crc16_xmodem
  crcParams _ = crcParams Crc16_xmodem

data Crc16_lte = Crc16_lte deriving Show
type instance TryDomain t Crc16_lte = 'NotFound
instance KnownCrc Crc16_lte where
  type CrcWidth Crc16_lte = CrcWidth Crc16_xmodem
  crcParams _ = crcParams Crc16_xmodem

data Crc16_v_41_msb = Crc16_v_41_msb deriving Show
type instance TryDomain t Crc16_v_41_msb = 'NotFound
instance KnownCrc Crc16_v_41_msb where
  type CrcWidth Crc16_v_41_msb = CrcWidth Crc16_xmodem
  crcParams _ = crcParams Crc16_xmodem

data Crc16_zmodem = Crc16_zmodem deriving Show
type instance TryDomain t Crc16_zmodem = 'NotFound
instance KnownCrc Crc16_zmodem where
  type CrcWidth Crc16_zmodem = CrcWidth Crc16_xmodem
  crcParams _ = crcParams Crc16_xmodem

data Crc17_can_fd = Crc17_can_fd deriving Show
type instance TryDomain t Crc17_can_fd = 'NotFound
instance KnownCrc Crc17_can_fd where
  type CrcWidth Crc17_can_fd = 17
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1685b
    , _crcInitial = 0x00000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00000
    }

data Crc21_can_fd = Crc21_can_fd deriving Show
type instance TryDomain t Crc21_can_fd = 'NotFound
instance KnownCrc Crc21_can_fd where
  type CrcWidth Crc21_can_fd = 21
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x102899
    , _crcInitial = 0x000000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000000
    }

data Crc24_ble = Crc24_ble deriving Show
type instance TryDomain t Crc24_ble = 'NotFound
instance KnownCrc Crc24_ble where
  type CrcWidth Crc24_ble = 24
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x00065b
    , _crcInitial = 0x555555
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x000000
    }

data Crc24_flexray_a = Crc24_flexray_a deriving Show
type instance TryDomain t Crc24_flexray_a = 'NotFound
instance KnownCrc Crc24_flexray_a where
  type CrcWidth Crc24_flexray_a = 24
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x5d6dcb
    , _crcInitial = 0xfedcba
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000000
    }

data Crc24_flexray_b = Crc24_flexray_b deriving Show
type instance TryDomain t Crc24_flexray_b = 'NotFound
instance KnownCrc Crc24_flexray_b where
  type CrcWidth Crc24_flexray_b = 24
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x5d6dcb
    , _crcInitial = 0xabcdef
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000000
    }

data Crc24_interlaken = Crc24_interlaken deriving Show
type instance TryDomain t Crc24_interlaken = 'NotFound
instance KnownCrc Crc24_interlaken where
  type CrcWidth Crc24_interlaken = 24
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x328b63
    , _crcInitial = 0xffffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffffff
    }

data Crc24_lte_a = Crc24_lte_a deriving Show
type instance TryDomain t Crc24_lte_a = 'NotFound
instance KnownCrc Crc24_lte_a where
  type CrcWidth Crc24_lte_a = 24
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x864cfb
    , _crcInitial = 0x000000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000000
    }

data Crc24_lte_b = Crc24_lte_b deriving Show
type instance TryDomain t Crc24_lte_b = 'NotFound
instance KnownCrc Crc24_lte_b where
  type CrcWidth Crc24_lte_b = 24
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x800063
    , _crcInitial = 0x000000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000000
    }

data Crc24_openpgp = Crc24_openpgp deriving Show
type instance TryDomain t Crc24_openpgp = 'NotFound
instance KnownCrc Crc24_openpgp where
  type CrcWidth Crc24_openpgp = 24
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x864cfb
    , _crcInitial = 0xb704ce
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x000000
    }

data Crc24_os_9 = Crc24_os_9 deriving Show
type instance TryDomain t Crc24_os_9 = 'NotFound
instance KnownCrc Crc24_os_9 where
  type CrcWidth Crc24_os_9 = 24
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x800063
    , _crcInitial = 0xffffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffffff
    }

data Crc30_cdma = Crc30_cdma deriving Show
type instance TryDomain t Crc30_cdma = 'NotFound
instance KnownCrc Crc30_cdma where
  type CrcWidth Crc30_cdma = 30
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x2030b9c7
    , _crcInitial = 0x3fffffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x3fffffff
    }

data Crc31_philips = Crc31_philips deriving Show
type instance TryDomain t Crc31_philips = 'NotFound
instance KnownCrc Crc31_philips where
  type CrcWidth Crc31_philips = 31
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x04c11db7
    , _crcInitial = 0x7fffffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x7fffffff
    }

data Crc32_aixm = Crc32_aixm deriving Show
type instance TryDomain t Crc32_aixm = 'NotFound
instance KnownCrc Crc32_aixm where
  type CrcWidth Crc32_aixm = 32
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x814141ab
    , _crcInitial = 0x00000000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00000000
    }

data Crc32_autosar = Crc32_autosar deriving Show
type instance TryDomain t Crc32_autosar = 'NotFound
instance KnownCrc Crc32_autosar where
  type CrcWidth Crc32_autosar = 32
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0xf4acfb13
    , _crcInitial = 0xffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffffffff
    }

data Crc32_base91_d = Crc32_base91_d deriving Show
type instance TryDomain t Crc32_base91_d = 'NotFound
instance KnownCrc Crc32_base91_d where
  type CrcWidth Crc32_base91_d = 32
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0xa833982b
    , _crcInitial = 0xffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffffffff
    }

data Crc32_bzip2 = Crc32_bzip2 deriving Show
type instance TryDomain t Crc32_bzip2 = 'NotFound
instance KnownCrc Crc32_bzip2 where
  type CrcWidth Crc32_bzip2 = 32
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x04c11db7
    , _crcInitial = 0xffffffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffffffff
    }

data Crc32_aal5 = Crc32_aal5 deriving Show
type instance TryDomain t Crc32_aal5 = 'NotFound
instance KnownCrc Crc32_aal5 where
  type CrcWidth Crc32_aal5 = CrcWidth Crc32_bzip2
  crcParams _ = crcParams Crc32_bzip2

data Crc32_dect_b = Crc32_dect_b deriving Show
type instance TryDomain t Crc32_dect_b = 'NotFound
instance KnownCrc Crc32_dect_b where
  type CrcWidth Crc32_dect_b = CrcWidth Crc32_bzip2
  crcParams _ = crcParams Crc32_bzip2

data Crc32_cd_rom_edc = Crc32_cd_rom_edc deriving Show
type instance TryDomain t Crc32_cd_rom_edc = 'NotFound
instance KnownCrc Crc32_cd_rom_edc where
  type CrcWidth Crc32_cd_rom_edc = 32
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x8001801b
    , _crcInitial = 0x00000000
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00000000
    }

data Crc32_cksum = Crc32_cksum deriving Show
type instance TryDomain t Crc32_cksum = 'NotFound
instance KnownCrc Crc32_cksum where
  type CrcWidth Crc32_cksum = 32
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x04c11db7
    , _crcInitial = 0x00000000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffffffff
    }

data Crc32_posix = Crc32_posix deriving Show
type instance TryDomain t Crc32_posix = 'NotFound
instance KnownCrc Crc32_posix where
  type CrcWidth Crc32_posix = CrcWidth Crc32_cksum
  crcParams _ = crcParams Crc32_cksum

data Crc32_iscsi = Crc32_iscsi deriving Show
type instance TryDomain t Crc32_iscsi = 'NotFound
instance KnownCrc Crc32_iscsi where
  type CrcWidth Crc32_iscsi = 32
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x1edc6f41
    , _crcInitial = 0xffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffffffff
    }

data Crc32_base91_c = Crc32_base91_c deriving Show
type instance TryDomain t Crc32_base91_c = 'NotFound
instance KnownCrc Crc32_base91_c where
  type CrcWidth Crc32_base91_c = CrcWidth Crc32_iscsi
  crcParams _ = crcParams Crc32_iscsi

data Crc32_castagnoli = Crc32_castagnoli deriving Show
type instance TryDomain t Crc32_castagnoli = 'NotFound
instance KnownCrc Crc32_castagnoli where
  type CrcWidth Crc32_castagnoli = CrcWidth Crc32_iscsi
  crcParams _ = crcParams Crc32_iscsi

data Crc32_interlaken = Crc32_interlaken deriving Show
type instance TryDomain t Crc32_interlaken = 'NotFound
instance KnownCrc Crc32_interlaken where
  type CrcWidth Crc32_interlaken = CrcWidth Crc32_iscsi
  crcParams _ = crcParams Crc32_iscsi

data Crc32_iso_hdlc = Crc32_iso_hdlc deriving Show
type instance TryDomain t Crc32_iso_hdlc = 'NotFound
instance KnownCrc Crc32_iso_hdlc where
  type CrcWidth Crc32_iso_hdlc = 32
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x04c11db7
    , _crcInitial = 0xffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffffffff
    }

data Crc32_adccp = Crc32_adccp deriving Show
type instance TryDomain t Crc32_adccp = 'NotFound
instance KnownCrc Crc32_adccp where
  type CrcWidth Crc32_adccp = CrcWidth Crc32_iso_hdlc
  crcParams _ = crcParams Crc32_iso_hdlc

data Crc32_v_42 = Crc32_v_42 deriving Show
type instance TryDomain t Crc32_v_42 = 'NotFound
instance KnownCrc Crc32_v_42 where
  type CrcWidth Crc32_v_42 = CrcWidth Crc32_iso_hdlc
  crcParams _ = crcParams Crc32_iso_hdlc

data Crc32_xz = Crc32_xz deriving Show
type instance TryDomain t Crc32_xz = 'NotFound
instance KnownCrc Crc32_xz where
  type CrcWidth Crc32_xz = CrcWidth Crc32_iso_hdlc
  crcParams _ = crcParams Crc32_iso_hdlc

data Crc32_pkzip = Crc32_pkzip deriving Show
type instance TryDomain t Crc32_pkzip = 'NotFound
instance KnownCrc Crc32_pkzip where
  type CrcWidth Crc32_pkzip = CrcWidth Crc32_iso_hdlc
  crcParams _ = crcParams Crc32_iso_hdlc

data Crc32_ethernet = Crc32_ethernet deriving Show
type instance TryDomain t Crc32_ethernet = 'NotFound
instance KnownCrc Crc32_ethernet where
  type CrcWidth Crc32_ethernet = CrcWidth Crc32_iso_hdlc
  crcParams _ = crcParams Crc32_iso_hdlc

data Crc32_jamcrc = Crc32_jamcrc deriving Show
type instance TryDomain t Crc32_jamcrc = 'NotFound
instance KnownCrc Crc32_jamcrc where
  type CrcWidth Crc32_jamcrc = 32
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x04c11db7
    , _crcInitial = 0xffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00000000
    }

data Crc32_mef = Crc32_mef deriving Show
type instance TryDomain t Crc32_mef = 'NotFound
instance KnownCrc Crc32_mef where
  type CrcWidth Crc32_mef = 32
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x741b8cd7
    , _crcInitial = 0xffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x00000000
    }

data Crc32_mpeg_2 = Crc32_mpeg_2 deriving Show
type instance TryDomain t Crc32_mpeg_2 = 'NotFound
instance KnownCrc Crc32_mpeg_2 where
  type CrcWidth Crc32_mpeg_2 = 32
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x04c11db7
    , _crcInitial = 0xffffffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00000000
    }

data Crc32_xfer = Crc32_xfer deriving Show
type instance TryDomain t Crc32_xfer = 'NotFound
instance KnownCrc Crc32_xfer where
  type CrcWidth Crc32_xfer = 32
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x000000af
    , _crcInitial = 0x00000000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x00000000
    }

data Crc40_gsm = Crc40_gsm deriving Show
type instance TryDomain t Crc40_gsm = 'NotFound
instance KnownCrc Crc40_gsm where
  type CrcWidth Crc40_gsm = 40
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x0004820009
    , _crcInitial = 0x0000000000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffffffffff
    }

data Crc64_ecma_182 = Crc64_ecma_182 deriving Show
type instance TryDomain t Crc64_ecma_182 = 'NotFound
instance KnownCrc Crc64_ecma_182 where
  type CrcWidth Crc64_ecma_182 = 64
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x42f0e1eba9ea3693
    , _crcInitial = 0x0000000000000000
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0x0000000000000000
    }

data Crc64_go_iso = Crc64_go_iso deriving Show
type instance TryDomain t Crc64_go_iso = 'NotFound
instance KnownCrc Crc64_go_iso where
  type CrcWidth Crc64_go_iso = 64
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x000000000000001b
    , _crcInitial = 0xffffffffffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffffffffffffffff
    }

data Crc64_ms = Crc64_ms deriving Show
type instance TryDomain t Crc64_ms = 'NotFound
instance KnownCrc Crc64_ms where
  type CrcWidth Crc64_ms = 64
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x259c84cba6426349
    , _crcInitial = 0xffffffffffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000000000000000
    }

data Crc64_redis = Crc64_redis deriving Show
type instance TryDomain t Crc64_redis = 'NotFound
instance KnownCrc Crc64_redis where
  type CrcWidth Crc64_redis = 64
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0xad93d23594c935a9
    , _crcInitial = 0x0000000000000000
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x0000000000000000
    }

data Crc64_we = Crc64_we deriving Show
type instance TryDomain t Crc64_we = 'NotFound
instance KnownCrc Crc64_we where
  type CrcWidth Crc64_we = 64
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x42f0e1eba9ea3693
    , _crcInitial = 0xffffffffffffffff
    , _crcReflectInput = False
    , _crcReflectOutput = False
    , _crcXorOutput = 0xffffffffffffffff
    }

data Crc64_xz = Crc64_xz deriving Show
type instance TryDomain t Crc64_xz = 'NotFound
instance KnownCrc Crc64_xz where
  type CrcWidth Crc64_xz = 64
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x42f0e1eba9ea3693
    , _crcInitial = 0xffffffffffffffff
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0xffffffffffffffff
    }

data Crc64_ecma = Crc64_ecma deriving Show
type instance TryDomain t Crc64_ecma = 'NotFound
instance KnownCrc Crc64_ecma where
  type CrcWidth Crc64_ecma = CrcWidth Crc64_xz
  crcParams _ = crcParams Crc64_xz

data Crc82_darc = Crc82_darc deriving Show
type instance TryDomain t Crc82_darc = 'NotFound
instance KnownCrc Crc82_darc where
  type CrcWidth Crc82_darc = 82
  crcParams _ = CrcParams
    { _crcWidth = SNat
    , _crcPolynomial = 0x0308c0111011401440411
    , _crcInitial = 0x000000000000000000000
    , _crcReflectInput = True
    , _crcReflectOutput = True
    , _crcXorOutput = 0x000000000000000000000
    }
