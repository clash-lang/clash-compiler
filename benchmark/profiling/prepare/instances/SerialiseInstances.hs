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
import           Clash.Core.Scoped.Binary (decodeWithHeader, encodeWithHeader)
import           Clash.Core.Scoped.Syntax (GlobalId)
import           Clash.Core.TyCon (TyConMap,TyConName)
import           Clash.Driver.Types (DomainMap, GlobalBindings)
import qualified Data.ByteString.Lazy as BL
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
type NormalizationInputs =
  ( GlobalBindings
  , TyConMap
  , IntMap TyConName
  , CompiledPrimMap'
  , CustomReprs
  , [GlobalId]
  , GlobalId
  , DomainMap
  )

-- data that's serialised to file between profile-netlist-prepare  and profile-netlist-run
type NetlistInputs =
  ( GlobalBindings
  , [CL.TopEntityT]
  , CompiledPrimMap'
  , TyConMap
  , CustomReprs
  , GlobalId
  , DomainMap
  )

-- | Write inputs, or normalized bindings, to a file of scoped Clash Core: with
-- the header of "Clash.Core.Scoped.Binary"
writeInputs :: Binary a => FilePath -> a -> IO ()
writeInputs f = BL.writeFile f . encodeWithHeader

-- | Read a file written by 'writeInputs'
readInputs :: Binary a => FilePath -> IO a
readInputs f = decodeWithHeader f <$> BL.readFile f
