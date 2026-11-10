{-# LANGUAGE UndecidableInstances #-}
{- # OPTIONS_GHC -ddump-splices #-}
module T3111_Constrained_NFDataX where

{-
Tests that deriveAutoReg can work with types with constrained NFDataX instances
-}

import Clash.Prelude

data MsgConfig = MsgConfig
  { _msgChannelWidth :: Nat
  , _msgDataLen :: Nat
  }

type family MsgChannelWidth (cfg :: MsgConfig) :: Nat where
  MsgChannelWidth ('MsgConfig x _) = x

type family MsgDataLen (cfg :: MsgConfig) :: Nat where
  MsgDataLen ('MsgConfig _ x) = x

type KnownMsgConfig cfg = (KnownNat (MsgDataLen cfg), KnownNat (MsgChannelWidth cfg))

data Message (cfg :: MsgConfig) = Status
  { _msg_channel :: Unsigned (MsgChannelWidth cfg)
  , _msg_data :: Vec (MsgDataLen cfg) (Unsigned 8)
  , _msg_chksum  :: Unsigned 4
  }
  deriving (Bundle, Generic)
-- deriving instance (KnownMsgConfig cfg) => BitPack (Message cfg)
deriving instance (KnownMsgConfig cfg) => NFDataX (Message cfg)
deriveAutoReg ''Message

type MyConfig = Message ('MsgConfig 3 4)

topEntity :: SystemClockResetEnable => Signal System MyConfig -> Signal System MyConfig
topEntity = autoReg (Status 1 (repeat 2) 3)
