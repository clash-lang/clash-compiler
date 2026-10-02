{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -ffull-laziness #-}

module T3461 where

import Clash.Prelude

newtype St = St {field :: Vec 100 Bit}
  deriving (BitPack, Generic, NFDataX)

topEntity :: HiddenClockResetEnable System => Signal System (Vec 2 Bit)
topEntity = register twoBits $ pure twoBits
 where
  twoBits = takeTwoBits St{field=repeat 0}
  takeTwoBits = take d2 . bv2v . pack
