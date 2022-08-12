{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -O -fno-worker-wrapper -fexpose-all-unfoldings #-}

module T2289.Core where

import Clash.Prelude

import T2289.Counter

data SDRAMSpec = SDRAMSpec Nat

type family SDRAMtBDL (s :: SDRAMSpec) :: Nat where
  SDRAMtBDL ('SDRAMSpec v) = v

type KnownSDRAMSpec (s :: SDRAMSpec)
   = ( 1 <= SDRAMtBDL s
     , KnownNat (SDRAMtBDL s)
     )

data SDRAMModule (spec :: SDRAMSpec) where
  SDRAMModule :: KnownSDRAMSpec spec => SDRAMModule spec

data SDRAMState (s :: SDRAMSpec)
   = SDRAMInitRefresh
       { sDRAMWaitRefreshCounter :: Counter 1
       }
   | SDRAMInitLoadModeReg
       { sDRAMInitLoadModeRegCounter :: Counter (SDRAMtBDL s)
       }

deriving instance KnownSDRAMSpec s => Show (SDRAMState s)
deriving instance KnownSDRAMSpec s => Generic (SDRAMState s)
deriving instance KnownSDRAMSpec s => NFDataX (SDRAMState s)

sDRAMCoreT
  :: forall (s :: SDRAMSpec)
   . KnownSDRAMSpec s
  => SDRAMState s
  -> SDRAMState s
sDRAMCoreT SDRAMInitRefresh{..} | hasOverflown sDRAMWaitRefreshCounter
                                = SDRAMInitLoadModeReg mkCounter
                                | otherwise
                                = SDRAMInitRefresh (tick sDRAMWaitRefreshCounter)
sDRAMCoreT SDRAMInitLoadModeReg{..} = SDRAMInitLoadModeReg (tick sDRAMInitLoadModeRegCounter)
