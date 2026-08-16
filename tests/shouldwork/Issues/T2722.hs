{-# LANGUAGE OverloadedStrings #-}

-- | Test whether two instantiations of the same primitive rendered in their own
-- module can use the same local identifier. See
-- https://github.com/clash-lang/clash-compiler/issues/2722.
module T2722 where

import Clash.Explicit.Prelude

import Clash.Annotations.Primitive (Primitive(..))
import Clash.Backend (blockDecl)
import Data.Monoid (Ap(getAp))

import qualified Clash.Netlist.Types as N
import qualified Clash.Netlist.Id as Id

bbTF :: N.TemplateFunction
bbTF = N.TemplateFunction used valid $ \_bbCtx -> do
  x <- Id.make Id.Local "x"

  () <- case Id.toText x of
    "x" -> pure ()
    xName -> error $ "Unexpected name: " <> show xName <> ". Expected: x."

  getAp $ blockDecl x [N.NetDecl Nothing x N.Bit]
 where
  used    = [0,1]
  valid _ = True

{-# ANN bb (InlinePrimitive [minBound..] "[ { \"BlackBox\" : { \"name\" : \"T2722.bb\", \"kind\": \"Declaration\", \"workInfo\": \"Always\", \"format\": \"Haskell\", \"templateFunction\": \"T2722.bbTF\"}} ]") #-}
bb :: Signal System Bit
bb = pure low
{-# OPAQUE bb #-}

-- | 'bbWrapper' is marked as opaque, so Clash will generate a separate HDL module
-- for it. Note that it accepts an extra (unused) argument to prevent Clash's
-- specialization from caching it.
bbWrapper :: Bit -> Signal System Bit
bbWrapper !_ = bb
{-# OPAQUE bbWrapper #-}

topEntity :: Signal System Bit
topEntity = bbWrapper low + bbWrapper high
