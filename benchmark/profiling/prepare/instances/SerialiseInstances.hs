{-# OPTIONS_GHC -Wno-orphans    #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module SerialiseInstances where

import Data.Binary

import Data.Hashable (Hashable)

import           Clash.Annotations.Primitive                  (PrimitiveGuard)
import qualified Clash.Annotations.BitRepresentation.Internal as CP

import qualified Clash.Primitives.Types                       as CL
import qualified Clash.Netlist.Types                          as CL (BlackBox, TopEntityT)
import qualified Clash.Netlist.BlackBox.Types                 as CL

import qualified Data.HashMap.Strict                          as HM

import           Clash.Annotations.BitRepresentation.Internal (CustomReprs)
import           Clash.Core.Binding (BindingMap)
import           Clash.Core.Term (Term)
import           Clash.Core.Var (Id)
import           Clash.Core.TyCon (TyConMap,TyConName)
import           Data.IntMap.Strict (IntMap)


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

-- data that's serialised to file between profile-normalization-prepare  and profile-normalization-run
type NormalizationInputs = (BindingMap Term,TyConMap,IntMap TyConName,CompiledPrimMap',CustomReprs,[Id],Id)

-- data that's serialised to file between profile-netlist-prepare  and profile-netlist-run
type NetlistInputs = (BindingMap Term,[CL.TopEntityT],CompiledPrimMap',TyConMap,CustomReprs,Id)
