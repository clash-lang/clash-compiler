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
import           Clash.Core.Var (Id)
import           Clash.Core.TyCon (TyConMap,TyConName)
import           Clash.Driver.Types (BindingMap, DomainMap)
import           Clash.Rewrite.Capture (CaptureLog)
import           Clash.Rewrite.Capture.Encoding (decodeCaptureLog, hPutCaptureLog)
import           System.IO (Handle)
import           Data.Binary.Get (runGetOrFail)
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
  ( BindingMap
  , TyConMap
  , IntMap TyConName
  , CompiledPrimMap'
  , CustomReprs
  , [Id]
  , Id
  , DomainMap
  )

-- data that's serialised to file between profile-netlist-prepare  and profile-netlist-run
type NetlistInputs =
  ( BindingMap
  , [CL.TopEntityT]
  , CompiledPrimMap'
  , TyConMap
  , CustomReprs
  , Id
  , DomainMap
  )

-- | Write the captured sites at which transformations fired while
-- normalizing a design, see "Clash.Rewrite.Capture": the inputs of
-- normalization, without their bindings, followed by the captures, which
-- have the bindings at the end of normalization.
hPutCorpus :: Handle -> NormalizationInputs -> CaptureLog -> IO ()
hPutCorpus h inputs cl = BL.hPut h (encode inputs) >> hPutCaptureLog h cl

-- | Decode a corpus written by 'hPutCorpus'. Decodes the captures lazily, see
-- 'decodeCaptureLog'.
decodeCorpus :: BL.ByteString -> (NormalizationInputs, CaptureLog)
decodeCorpus bs = case runGetOrFail get bs of
  Left (_, _, err) -> error ("decodeCorpus: " <> err)
  Right (rest, _, inputs) -> (inputs, decodeCaptureLog rest)
