{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Reads the serialized designs of before the native 'Data.Binary.Binary'
  instances of scoped Clash Core ("Clash.Core.Scoped.Binary"): files without
  its header, whose bindings are terms of Clash Core, and whose global
  variables, data constructors and type constructors are encoded as those of
  Clash Core.

  The decoders here follow the encoding of the instances of then, which were
  derived ('GHC.Generics') for the terms of Clash Core: a constructor is
  encoded as its index in the declaration as a 'Data.Word.Word8', followed by
  its fields.

  TODO: remove, together with Clash Core (package B of the port).
-}

{-# LANGUAGE LambdaCase #-}

module Legacy
  ( -- * Reading
    readLegacyNormalizationInputs
  , readLegacyNetlistInputs
  , readLegacyBindingMap
  , readBindingMap
  , getLegacyTerm
  ) where

import Control.Monad (replicateM)
import Data.Binary (Binary (..), Get)
import Data.Binary.Get (getWord8, runGetOrFail)
import qualified Data.ByteString.Lazy as BL
import Data.IntMap.Strict (IntMap)

import Clash.Annotations.BitRepresentation.Internal (CustomReprs)
-- For the orphan instances of 'Binary' of top entity annotations
import Clash.Driver.Manifest ()
import Clash.Core.DataCon (DataCon)
import Clash.Core.Term (Bind (..), Pat (..), Term (..), TickInfo (..))
import Clash.Core.TyCon (AlgTyConRhs (..), TyCon, TyConMap, TyConName)
import Clash.Core.Var (Id)
import Clash.Data.UniqMap (UniqMap)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types (Binding (..), BindingMap, DomainMap)
import Clash.Netlist.Types (TopEntityT (..))
import Clash.Unique (Unique)
import qualified Clash.Util.Supply as Supply

import Clash.Core.Scoped.Binary (hasHeader)
import Clash.Core.Scoped.Convert
  (mkAlgTyConFromClash, mkDataConFromClash, mkFunTyConFromClash, mkGlobalIdFromClash,
   mkNewTyConRhsFromClash, mkPrimTyConFromClash, mkPromotedDataConFromClash)
import Clash.Rewrite.Scoped.Globals (toClashBindings)

import SerialiseInstances (CompiledPrimMap', readInputs)

-- * Decoders

-- | A list, encoded as its length and its elements
getListOf :: Get a -> Get [a]
getListOf getA = do
  n <- get :: Get Int
  replicateM n getA

-- | A 'UniqMap', encoded as an 'IntMap'
getUniqMapOf :: Get a -> Get (UniqMap a)
getUniqMapOf getA =
  UniqMap.fromList <$> getListOf ((,) <$> (fromIntegral <$> (get :: Get Int) :: Get Unique) <*> getA)

getDataCon :: Get DataCon
getDataCon =
  mkDataConFromClash
    <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

getAlgTyConRhs :: Get AlgTyConRhs
getAlgTyConRhs = getWord8 >>= \case
  0 -> DataTyCon <$> getListOf getDataCon
  1 -> mkNewTyConRhsFromClash <$> getDataCon <*> get
  t -> fail ("legacy AlgTyConRhs: unknown tag " <> show t)

getTyCon :: Get TyCon
getTyCon = getWord8 >>= \case
  0 -> mkAlgTyConFromClash <$> get <*> get <*> get <*> get <*> getAlgTyConRhs <*> get
  1 -> mkPromotedDataConFromClash <$> get <*> get <*> get <*> get <*> getDataCon
  2 -> mkFunTyConFromClash <$> get <*> get <*> get <*> get <*> get
  3 -> mkPrimTyConFromClash <$> get <*> get <*> get <*> get
  t -> fail ("legacy TyCon: unknown tag " <> show t)

getTyConMap :: Get TyConMap
getTyConMap = getUniqMapOf getTyCon

-- | A term of Clash Core
getLegacyTerm :: Get Term
getLegacyTerm = getTerm

getTerm :: Get Term
getTerm = getWord8 >>= \case
  0 -> Var <$> get
  1 -> Data <$> getDataCon
  2 -> Literal <$> get
  3 -> Prim <$> get
  4 -> Lam <$> get <*> getTerm
  5 -> TyLam <$> get <*> getTerm
  6 -> App <$> getTerm <*> getTerm
  7 -> TyApp <$> getTerm <*> get
  8 -> Let <$> getBind <*> getTerm
  9 -> Case <$> getTerm <*> get <*> getListOf ((,) <$> getPat <*> getTerm)
  10 -> Cast <$> getTerm <*> get <*> get
  11 -> Tick <$> getTickInfo <*> getTerm
  t -> fail ("legacy Term: unknown tag " <> show t)
 where
  getBind = getWord8 >>= \case
    0 -> NonRec <$> get <*> getTerm
    1 -> Rec <$> getListOf ((,) <$> get <*> getTerm)
    t -> fail ("legacy Bind: unknown tag " <> show t)

  getPat = getWord8 >>= \case
    0 -> DataPat <$> getDataCon <*> get <*> get
    1 -> LitPat <$> get
    2 -> pure DefaultPat
    t -> fail ("legacy Pat: unknown tag " <> show t)

  getTickInfo = getWord8 >>= \case
    0 -> SrcSpan <$> get
    1 -> NameMod <$> get <*> get
    2 -> pure DeDup
    3 -> pure NoDeDup
    4 -> Attributes <$> get <*> getTerm
    t -> fail ("legacy TickInfo: unknown tag " <> show t)

-- | The global variable of a binding was encoded as the 'Id' of Clash Core
getBinding :: Get (Binding Term)
getBinding =
  Binding
    <$> (mkGlobalIdFromClash <$> get)
    <*> get <*> get <*> get <*> getTerm <*> get

getBindingMap :: Get BindingMap
getBindingMap = getUniqMapOf getBinding

getTopEntityT :: Get TopEntityT
getTopEntityT = TopEntityT <$> (mkGlobalIdFromClash <$> get) <*> get <*> get

-- | The inputs of normalization, with the bindings of Clash Core
type LegacyNormalizationInputs =
  ( BindingMap, TyConMap, IntMap TyConName, CompiledPrimMap', CustomReprs, [Id], Id
  , DomainMap )

getNormalizationInputs :: Get LegacyNormalizationInputs
getNormalizationInputs =
  (,,,,,,,)
    <$> getBindingMap <*> getTyConMap <*> get <*> get <*> get <*> get <*> get <*> get

-- | The inputs of netlist generation, with the bindings of Clash Core
type LegacyNetlistInputs =
  ( BindingMap, [TopEntityT], CompiledPrimMap', TyConMap, CustomReprs, Id, DomainMap )

getNetlistInputs :: Get LegacyNetlistInputs
getNetlistInputs =
  (,,,,,,)
    <$> getBindingMap <*> getListOf getTopEntityT <*> get <*> getTyConMap <*> get
    <*> get <*> get

-- * Reading

-- | Decode all of a file with the given decoder
runLegacy :: FilePath -> Get a -> BL.ByteString -> a
runLegacy f g bs = case runGetOrFail g bs of
  Left (_, off, err) -> error (f <> ": " <> err <> ", at byte " <> show off)
  Right (rest, off, a)
    | BL.null rest -> a
    | otherwise -> error (f <> ": trailing input after byte " <> show off)

readLegacy :: FilePath -> Get a -> IO a
readLegacy f g = runLegacy f g <$> BL.readFile f

-- | The inputs of normalization (@*.bin@) of before the native encoding
readLegacyNormalizationInputs :: FilePath -> IO LegacyNormalizationInputs
readLegacyNormalizationInputs f = readLegacy f getNormalizationInputs

-- | The inputs of netlist generation (@*.net.bin@) of before the native
-- encoding
readLegacyNetlistInputs :: FilePath -> IO LegacyNetlistInputs
readLegacyNetlistInputs f = readLegacy f getNetlistInputs

-- | Normalized bindings (@*.norm.bin@) of before the native encoding
readLegacyBindingMap :: FilePath -> IO BindingMap
readLegacyBindingMap f = readLegacy f getBindingMap

-- | Normalized bindings (@*.norm.bin@) in either encoding, as bindings of
-- Clash Core
readBindingMap :: FilePath -> IO BindingMap
readBindingMap f = do
  bs <- BL.readFile f
  if hasHeader bs
    then do
      supply <- Supply.newSupply
      toClashBindings supply <$> readInputs f
    else pure (runLegacy f getBindingMap bs)
