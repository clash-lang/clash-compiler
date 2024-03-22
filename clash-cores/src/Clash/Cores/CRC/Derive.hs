{-|
  Copyright   :  (C) 2024, Rowan Goemans <goemansrowan@gmail.com>
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

-}

module Clash.Cores.CRC.Derive (deriveHardwareCRC) where

import           Clash.Prelude
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import           Clash.Cores.CRC.Internal
import           Data.Proxy

import           Type.Reflection

typeRepToTHType :: SomeTypeRep -> TH.Type
typeRepToTHType (SomeTypeRep (Con tyCon)) = TH.ConT $ TH.Name nameBase flavor
  where
    nameBase = TH.mkOccName (tyConName tyCon)
    flavor
      = TH.NameG TH.TcClsName
          (TH.mkPkgName $ tyConPackage tyCon)
          (TH.mkModName $ tyConModule tyCon)
typeRepToTHType _ = error "typeRepToTHType: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

-- | Derive an instance for the 'HardwareCRC' class for the given arguments
--
-- For example, the following derives a 'HardwareCRC' instance for the 32-bit Ethernet CRC
-- where you can feed it 8, 16, 24 or 32 bits at a time:
--
-- @
-- {\-\# LANGUAGE MultiParamTypeClasses \#-\}
--
-- deriveHardwareCRC (Proxy \@CRC32_ETHERNET) d8 d4
-- @
--
-- For the derivation to work the @MultiParamTypeClasses@ pragma must be enabled in
-- the module that uses 'deriveHardwareCRC'.
--
-- See 'HardwareCRC', 'crcEngine' and 'crcValidator' for more information about what
-- the arguments mean.
deriveHardwareCRC
  :: Typeable crc
  => KnownCRC crc
  => 1 <= dataWidth
  => 1 <= nLanes
  => Proxy crc
  -- ^ For which CRC to derive the instance
  -> SNat dataWidth
  -- ^ The width in bits of the words it can handle every clock cycle
  -> SNat nLanes
  -- ^ The number of lanes
  -> TH.Q [TH.Dec]
deriveHardwareCRC crc dataWidth@SNat nLanes@SNat = do
  let crcTy = pure $ typeRepToTHType $ someTypeRep crc
      dataWidthTy = pure $ TH.LitT $ TH.NumTyLit $ snatToNum dataWidth
      nLanesTy = pure $ TH.LitT $ TH.NumTyLit $ snatToNum nLanes
      hwParams = lift $ mkCRCHardwareParams crc dataWidth nLanes

  [d| instance HardwareCRC $crcTy $dataWidthTy $nLanesTy where
        crcHardwareParams _ _ _ = $hwParams |]
