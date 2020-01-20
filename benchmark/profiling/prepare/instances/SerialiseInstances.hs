{-# OPTIONS_GHC -Wno-orphans    #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module SerialiseInstances where

import Data.Binary

import Data.Hashable (Hashable)

import           Clash.Annotations.Primitive                  (PrimitiveGuard)
import qualified Clash.Annotations.BitRepresentation.Internal as CP

import qualified Clash.Primitives.Types                       as CL
import qualified Clash.Netlist.Types                          as CL (BlackBox)
import qualified Clash.Netlist.BlackBox.Types                 as CL

import qualified Data.HashMap.Strict                          as HM


-- | Like C.CompiledPrimitive but without the functions BlackBoxFunction so we can serialise it
type CompiledPrimitive' = CL.Primitive CL.BlackBoxTemplate CL.BlackBox () ()
type CompiledPrimMap' = CL.PrimMap (PrimitiveGuard CompiledPrimitive')

-- Remove BlackBoxFunctions
removeBBfunc :: CL.CompiledPrimitive -> CompiledPrimitive'
removeBBfunc = fmap $ const ()

-- Put errors in places where BlackBoxFunctions used to be
unremoveBBfunc :: CompiledPrimitive' -> CL.CompiledPrimitive
unremoveBBfunc = fmap $ const (error "BlackBoxFunction can't be preserved by serialisation")

-- BitRepresentation internal
instance Binary CP.Type'
instance Binary CP.DataRepr'
instance Binary CP.ConstrRepr'

instance (Binary k, Binary v, Eq k, Hashable k) => Binary (HM.HashMap k v) where
  put = put . HM.toList
  get = HM.fromList <$> get
