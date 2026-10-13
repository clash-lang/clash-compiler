{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  'Binary' instances of closed types and terms of scoped Clash Core
  ("Clash.Core.Syntax"), and of the values they refer to: global
  variables, primitives, data constructors and type constructors. E.g.
  the manifest hashes the call graph of a top entity with them
  ('Clash.Driver.Manifest.readFreshManifest'), the rewrite history of
  @-fclash-debug-history@ is written with them, and so are the inputs of the
  profiling tools.

  = Encoding

  Types and terms are encoded in pre-order: a tag per constructor, followed by
  its fields from left to right.

  * A binder is encoded as its human readable name, with the raw name of the
    binder as its unique, followed by its kind or type. So two encodings of
    alpha-equivalent terms whose binders have the same raw names are equal:
    the unique of the human readable name, which is not used (see
    t'TyBinder'), doesn't leak into the encoding.

  * An occurrence of a local variable is encoded as its raw name. Its human
    readable name and its type (or kind) are those of its binder.

  * Global variables, primitives and data constructors are encoded in full at
    every occurrence.

  = Decoding

  Decoding goes through the builders of "Clash.Core.Builder", which
  refer to variables by a key: the unique of the human readable name of a
  binder, i.e. the raw name it was encoded with. So decoding adds nothing to
  the trusted core: a decoded term is well-scoped by construction. The
  builders give the binders fresh raw names in depth-first order, so
  decoding a term whose binders are numbered that way, such as a term of
  GHC2Core, gives back the same raw names. Decoding also builds a term lazily,
  as GHC2Core does: the scoped term is built when it is first used.

  An occurrence takes the human readable name and the type of its binder.

  = Files

  Files of scoped Clash Core start with a header ('putHeader'): a magic number
  and the version of this encoding ('formatVersion'). 'encodeWithHeader' and
  'decodeWithHeader' write and read it.
-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Core.Binary
  ( -- * Files
    formatVersion
  , putHeader
  , getHeader
  , hasHeader
  , encodeWithHeader
  , decodeWithHeader
    -- * Encoding
  , putType
  , putTerm
    -- * Decoding
  , getTypeB
  , getTermB
  ) where

import Control.Monad (replicateM, unless, when)
import Data.Binary (Binary (..), Get, Put)
import Data.Binary.Get (getByteString, getWord16be, getWord8, runGetOrFail)
import Data.Binary.Put (putByteString, putWord16be, putWord8, runPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word16)
import GHC.Stack (HasCallStack)

import Clash.Core.DataCon (DataCon (..))
import qualified Clash.Core.Name as C

import Clash.Core.TyCon (AlgTyConRhs (..), TyCon (..))
import GHC.SrcLoc.Extra ()

import Clash.Core.Builder
import Clash.Core.DataCon (mkDataCon)
import Clash.Core.NameMap (GlobalEnv, globalEnvFromList, globalEnvToList)
import Clash.Core.Sig (DataConSig (..), NewTyConSig (..), TyFamEqn (..))
import Clash.Core.Syntax

-- * Files

-- | The version of the encoding of this module, see 'putHeader'. Bump it
-- whenever the encoding changes.
formatVersion :: Word16
formatVersion = 1

-- | The magic number that files of scoped Clash Core start with
magic :: BS.ByteString
magic = BS8.pack "CLASHSCC"

-- | The header of a file of scoped Clash Core: a magic number, and the
-- version of the encoding
putHeader :: Put
putHeader = putByteString magic >> putWord16be formatVersion

-- | Read the header of a file of scoped Clash Core. Fails if it isn't one,
-- or if its version is not 'formatVersion'.
getHeader :: Get ()
getHeader = do
  m <- getByteString (BS.length magic)
  unless (m == magic) $
    fail "not a file of scoped Clash Core: no magic number"
  v <- getWord16be
  when (v /= formatVersion) $
    fail ("a file of scoped Clash Core of version " <> show v
          <> ", while this version of Clash reads version " <> show formatVersion)

-- | Whether a file starts with the magic number of 'putHeader'
hasHeader :: BL.ByteString -> Bool
hasHeader bs = BL.toStrict (BL.take (fromIntegral (BS.length magic)) bs) == magic

-- | Encode a value, preceded by the header ('putHeader')
encodeWithHeader :: Binary a => a -> BL.ByteString
encodeWithHeader a = runPut (putHeader >> put a)

-- | Decode a value encoded with 'encodeWithHeader'. Errors, mentioning the
-- given description of the input (e.g. a file name), if decoding fails or if
-- there is input left.
decodeWithHeader :: (HasCallStack, Binary a) => String -> BL.ByteString -> a
decodeWithHeader what bs = case runGetOrFail (getHeader >> get) bs of
  Left (_, off, err) -> error (what <> ": " <> err <> ", at byte " <> show off)
  Right (rest, off, a)
    | BL.null rest -> a
    | otherwise -> error (what <> ": trailing input after byte " <> show off)

-- * Binders

-- | The human readable name of a binder, with the raw name of the binder as
-- its unique, see the module header
binderName :: C.Name a -> Int -> C.Name a
binderName nm raw = nm {C.nameUniq = fromIntegral raw}

putTyBinder :: TyBinder n l -> Put
putTyBinder b = do
  put (binderName (tyBinderOcc b) (tyNameRaw (tyNameOf b)))
  putType (tyBinderKind b)

putIdBinder :: IdBinder n l -> Put
putIdBinder b = do
  put (binderName (idBinderOcc b) (tmNameRaw (tmNameOf b)))
  putType (idBinderType b)

-- | A binder for the builders: its key, its human readable name, and its kind
-- or type
getBinder :: Get (Key, C.Name a, TyB)
getBinder = do
  nm <- get
  ty <- getTypeB
  pure (fromIntegral (C.nameUniq nm), nm, ty)

-- | A telescope: the number of binders, then the binders
putTele :: forall b n l. (forall x y. b x y -> Put) -> Tele b n l -> Put
putTele putB tele = do
  put (teleLength tele)
  go tele
 where
  go :: Tele b x y -> Put
  go TNil = pure ()
  go (TCons b bs) = putB b >> go bs

getTele :: Get [(Key, C.Name a, TyB)]
getTele = do
  n <- get
  replicateM n getBinder

-- | A list: its length, then its elements
putListWith :: (a -> Put) -> [a] -> Put
putListWith putA xs = put (length xs) >> mapM_ putA xs

getListWith :: Get a -> Get [a]
getListWith getA = do
  n <- get
  replicateM n getA

-- * Types

-- | Encode a type, see the module header
putType :: Type n -> Put
putType = \case
  VarTy _ _ nm -> putWord8 0 >> put (tyNameRaw nm)
  ConstTy c -> putWord8 1 >> put c
  ForAllTy b t -> putWord8 2 >> putTyBinder b >> putType t
  AppTy t1 t2 -> putWord8 3 >> putType t1 >> putType t2
  LitTy l -> putWord8 4 >> put l
  AnnType attrs t -> putWord8 5 >> put attrs >> putType t

-- | Decode a type encoded with 'putType', see the module header
getTypeB :: Get TyB
getTypeB = getWord8 >>= \case
  0 -> varTyB <$> get
  1 -> constTyB <$> get
  2 -> do
    (key, nm, k) <- getBinder
    forAllTyB key nm k <$> getTypeB
  3 -> appTyB <$> getTypeB <*> getTypeB
  4 -> litTyB <$> get
  5 -> annTyB <$> get <*> getTypeB
  t -> fail ("Binary Type: unknown tag " <> show t)

-- | Closed types
instance Binary (Type 'VoidS) where
  put = putType
  get = runClosedType <$> getTypeB

-- * Terms

-- | Encode a term, see the module header
putTerm :: Term n -> Put
putTerm = \case
  Var _ _ nm -> putWord8 0 >> put (tmNameRaw nm)
  GlobalVar g -> putWord8 1 >> put g
  Data dc -> putWord8 2 >> put dc
  Literal l -> putWord8 3 >> put l
  Prim p -> putWord8 4 >> put p
  Lam b e -> putWord8 5 >> putIdBinder b >> putTerm e
  TyLam b e -> putWord8 6 >> putTyBinder b >> putTerm e
  App e1 e2 -> putWord8 7 >> putTerm e1 >> putTerm e2
  TyApp e ty -> putWord8 8 >> putTerm e >> putType ty
  Let (NonRec b x) e -> putWord8 9 >> putIdBinder b >> putTerm x >> putTerm e
  Let (Rec bs xs) e ->
    putWord8 10 >> putTele putIdBinder bs >> putListWith putTerm xs >> putTerm e
  Case e ty alts -> putWord8 11 >> putTerm e >> putType ty >> putListWith putAlt alts
  Cast e t1 t2 -> putWord8 12 >> putTerm e >> putType t1 >> putType t2
  Tick t e -> putWord8 13 >> putTick t >> putTerm e

putAlt :: Alt n -> Put
putAlt (Alt pat e) = case pat of
  DataPat dc tvs ids ->
    putWord8 0 >> put dc >> putTele putTyBinder tvs >> putTele putIdBinder ids >> putTerm e
  LitPat l -> putWord8 1 >> put l >> putTerm e
  DefaultPat -> putWord8 2 >> putTerm e

putTick :: TickInfo n -> Put
putTick = \case
  SrcSpan sp -> putWord8 0 >> put sp
  NameMod m ty -> putWord8 1 >> put m >> putType ty
  DeDup -> putWord8 2
  NoDeDup -> putWord8 3
  Attributes ty e -> putWord8 4 >> putType ty >> putTerm e

-- | Decode a term encoded with 'putTerm', see the module header
getTermB :: Get TmB
getTermB = getWord8 >>= \case
  0 -> varB <$> get
  1 -> globalB <$> get
  2 -> dataB <$> get
  3 -> literalB <$> get
  4 -> primB <$> get
  5 -> do
    (key, nm, ty) <- getBinder
    lamB key nm ty <$> getTermB
  6 -> do
    (key, nm, k) <- getBinder
    tyLamB key nm k <$> getTermB
  7 -> appB <$> getTermB <*> getTermB
  8 -> tyAppB <$> getTermB <*> getTypeB
  9 -> do
    (key, nm, ty) <- getBinder
    letB key nm ty <$> getTermB <*> getTermB
  10 -> do
    bs <- getTele
    xs <- getListWith getTermB
    when (length bs /= length xs) $
      fail "Binary Term: a recursive let-binding with as many binders as right-hand sides"
    letrecB [(key, nm, ty, x) | ((key, nm, ty), x) <- zip bs xs] <$> getTermB
  11 -> caseB <$> getTermB <*> getTypeB <*> getListWith getAltB
  12 -> castB <$> getTermB <*> getTypeB <*> getTypeB
  13 -> tickB <$> getTickB <*> getTermB
  t -> fail ("Binary Term: unknown tag " <> show t)

getAltB :: Get AltB
getAltB = getWord8 >>= \case
  0 -> dataAltB <$> get <*> getTele <*> getTele <*> getTermB
  1 -> litAltB <$> get <*> getTermB
  2 -> defaultAltB <$> getTermB
  t -> fail ("Binary Alt: unknown tag " <> show t)

getTickB :: Get TickB
getTickB = getWord8 >>= \case
  0 -> srcSpanTickB <$> get
  1 -> nameModTickB <$> get <*> getTypeB
  2 -> pure deDupTickB
  3 -> pure noDeDupTickB
  4 -> attributesTickB <$> getTypeB <*> getTermB
  t -> fail ("Binary TickInfo: unknown tag " <> show t)

-- | Closed terms
instance Binary (Term 'VoidS) where
  put = putTerm
  get = runClosedTerm <$> getTermB

-- * Globals

-- | Its name and its type
instance Binary GlobalId where
  put g = put (globalIdName g) >> putType (globalIdType g)
  get = mkGlobalId <$> (get :: Get (C.Name Term)) <*> get

instance Binary PrimUnfolding where
  put = \case
    NoUnfolding -> putWord8 0
    Unfolding g -> putWord8 1 >> put g
  get = getWord8 >>= \case
    0 -> pure NoUnfolding
    1 -> Unfolding <$> get
    t -> fail ("Binary PrimUnfolding: unknown tag " <> show t)

instance Binary PrimInfo where
  put p = do
    put (primName p)
    putType (primType p)
    put (primWorkInfo p)
    put (primMultiResult p)
    put (primUnfolding p)
  get = mkPrimInfo <$> get <*> get <*> get <*> get <*> get

-- | Its globals and their values, e.g. global bindings
-- ('Clash.Driver.Types.GlobalBindings')
instance Binary a => Binary (GlobalEnv a) where
  put = put . globalEnvToList
  get = globalEnvFromList <$> get

-- * Data constructors and type constructors

-- | Through the telescopes of its quantified type variables, see
-- "Clash.Core.Sig"
instance Binary DataConSig where
  put (DataConSig univ ext args) =
    putTele putTyBinder univ >> putTele putTyBinder ext >> putListWith putType args
  get = runDataConSig <$> getTele <*> getTele <*> getListWith getTypeB

instance Binary NewTyConSig where
  put (NewTyConSig tvs rhs) = putTele putTyBinder tvs >> putType rhs
  get = runNewTyConSig <$> getTele <*> getTypeB

instance Binary TyFamEqn where
  put (TyFamEqn tvs lhs rhs) = putTele putTyBinder tvs >> putListWith putType lhs >> putType rhs
  get = runTyFamEqn <$> getTele <*> getListWith getTypeB <*> getTypeB

-- | Its unique is that of its name, see 'dcUniq'
instance Binary DataCon where
  put dc = do
    put (dcName dc)
    put (dcTag dc)
    put (dcType dc)
    put (dcSig dc)
    put (dcArgStrict dc)
    put (dcFieldLabels dc)
  get = mkDataCon <$> get <*> get <*> get <*> get <*> get <*> get

instance Binary AlgTyConRhs where
  put = \case
    DataTyCon dcs -> putWord8 0 >> put dcs
    NewTyCon dc sig -> putWord8 1 >> put dc >> put sig
  get = getWord8 >>= \case
    0 -> DataTyCon <$> get
    1 -> NewTyCon <$> get <*> get
    t -> fail ("Binary AlgTyConRhs: unknown tag " <> show t)

-- | Its unique need not be that of its name
instance Binary TyCon where
  put = \case
    AlgTyCon u nm k ar rhs cls ->
      putWord8 0 >> put u >> put nm >> put k >> put ar >> put rhs >> put cls
    PromotedDataCon u nm k ar dc ->
      putWord8 1 >> put u >> put nm >> put k >> put ar >> put dc
    FunTyCon u nm k ar eqns ->
      putWord8 2 >> put u >> put nm >> put k >> put ar >> put eqns
    PrimTyCon u nm k ar ->
      putWord8 3 >> put u >> put nm >> put k >> put ar
  get = getWord8 >>= \case
    0 -> AlgTyCon <$> get <*> get <*> get <*> get <*> get <*> get
    1 -> PromotedDataCon <$> get <*> get <*> get <*> get <*> get
    2 -> FunTyCon <$> get <*> get <*> get <*> get <*> get
    3 -> PrimTyCon <$> get <*> get <*> get <*> get
    t -> fail ("Binary TyCon: unknown tag " <> show t)
