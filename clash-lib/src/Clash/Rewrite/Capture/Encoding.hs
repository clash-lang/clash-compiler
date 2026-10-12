{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Encoding the captures of "Clash.Rewrite.Capture" such that they keep their
  sharing.

  The 'Data.Binary.Binary' instances of Clash Core encode a value once for
  every reference to it: every occurrence of a variable gets a copy of its
  type, and terms that share subterms, such as the term a transformation was
  offered and its result, get a copy each. In memory, Clash Core shares those,
  and a capture shares most of its terms with the other captures of the same
  binding and with the final state. Encoded with 'Data.Binary.Binary', the
  captures of a design are a hundred times bigger than the design, and they
  take a hundred times more memory once decoded.

  So this encodes types, variables, data constructors and primitives once per
  object: the first time it meets an object, it encodes it, and numbers it;
  afterwards, it refers to it by that number. Terms are encoded once per
  object per capture (and once for the final state): numbering every term
  object of a session would keep millions of 'StableName's alive, and the
  garbage collector traverses all of them on every collection. Objects are
  identified by their 'StableName', i.e. by pointer: two equal values that
  are different objects are encoded twice, which is only a loss of space.
  Decoding builds every numbered object once, so the decoded captures share
  types and variables as much as the originals did, and a capture shares
  terms within itself.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Clash.Rewrite.Capture.Encoding
  ( hPutCaptureLog
  , decodeCaptureLog
  ) where

import           Control.Exception        (evaluate)
import           Control.Monad            (forM_, replicateM)
import           Control.Monad.State.Strict (StateT, get, put, runStateT)
import           Control.Monad.Trans.Class (lift)
import           Data.Binary              (Binary)
import qualified Data.Binary              as Binary
import           Data.Binary.Get          (Get, getInt64be, getWord8, runGetOrFail)
import           Data.Binary.Put          (execPut)
import           Data.ByteString.Builder  (Builder, hPutBuilder, int64BE, word8)
import qualified Data.ByteString.Lazy     as BL
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HashMap
import           Data.IORef
import           Data.IntMap.Strict       (IntMap)
import qualified Data.IntMap.Strict       as IntMap
import qualified Data.Map.Strict          as Map
import           Data.Word                (Word8)
import           GHC.Exts                 (Any)
import           System.IO                (Handle)
import           System.Mem.StableName    (StableName, makeStableName)
import           Unsafe.Coerce            (unsafeCoerce)

import           GHC.SrcLoc.Extra         ()

import           Clash.Core.DataCon
  (DataCon (..), dcArgTys, dcExtTyVars, dcLegacyType, dcUnivTyVars)
import           Clash.Core.Scoped.Convert (mkDataConFromClash, mkGlobalIdFromClash)
import           Clash.Core.Scoped.Syntax (globalIdLegacy, globalIdUniq)
import           Clash.Core.Term
import           Clash.Core.Type          (Type (..))
import           Clash.Core.Var           (Var (..))
import qualified Clash.Data.UniqMap       as UniqMap
import           Clash.Driver.Types       (Binding (..), BindingMap)
import           Clash.Rewrite.Capture

-- * Encoding

-- | Tables of the objects encoded so far, by kind
data Enc = Enc
  { encOut :: !(IORef Builder)
  , encTerms :: !(Table Term)
  , encTypes :: !(Table Type)
  , encVars :: !(Table (Var Any))
  , encDataCons :: !(Table DataCon)
  , encPrims :: !(Table PrimInfo)
  }

data Table a = Table !(IORef (HashMap (StableName a) Int)) !(IORef Int)

newTable :: IO (Table a)
newTable = Table <$> newIORef HashMap.empty <*> newIORef 0

emit :: Enc -> Builder -> IO ()
emit e b = modifyIORef' (encOut e) (<> b)

tag :: Enc -> Word8 -> IO ()
tag e = emit e . word8

int :: Enc -> Int -> IO ()
int e = emit e . int64BE . fromIntegral

bin :: Binary a => Enc -> a -> IO ()
bin e = emit e . execPut . Binary.put

list :: Enc -> (a -> IO ()) -> [a] -> IO ()
list e f xs = int e (length xs) >> mapM_ f xs

-- | Encode an object once: a reference to it if it was encoded before,
-- otherwise its encoding, after which it gets the next number
shared :: Enc -> (Enc -> Table a) -> (a -> IO ()) -> a -> IO ()
shared e table enc x0 = do
  x <- evaluate x0
  let Table ref next = table e
  sn <- makeStableName x
  HashMap.lookup sn <$> readIORef ref >>= \case
    Just i -> tag e 0 >> int e i
    Nothing -> do
      tag e 1
      enc x
      i <- readIORef next
      writeIORef next (i + 1)
      modifyIORef' ref (HashMap.insert sn i)

term :: Enc -> Term -> IO ()
term e = shared e encTerms $ \case
  Var v -> tag e 0 >> var e v
  Data dc -> tag e 1 >> dataCon e dc
  Literal l -> tag e 2 >> bin e l
  Prim p -> tag e 3 >> prim e p
  Lam v x -> tag e 4 >> var e v >> term e x
  TyLam v x -> tag e 5 >> var e v >> term e x
  App f a -> tag e 6 >> term e f >> term e a
  TyApp f t -> tag e 7 >> term e f >> ty e t
  Let b x -> tag e 8 >> bind e b >> term e x
  Case s t alts -> tag e 9 >> term e s >> ty e t >> list e (alt e) alts
  Cast x t1 t2 -> tag e 10 >> term e x >> ty e t1 >> ty e t2
  Tick t x -> tag e 11 >> tick e t >> term e x

ty :: Enc -> Type -> IO ()
ty e = shared e encTypes $ \case
  VarTy v -> tag e 0 >> var e v
  ConstTy c -> tag e 1 >> bin e c
  ForAllTy v t -> tag e 2 >> var e v >> ty e t
  AppTy t1 t2 -> tag e 3 >> ty e t1 >> ty e t2
  LitTy l -> tag e 4 >> bin e l
  AnnType as t -> tag e 5 >> bin e as >> ty e t

var :: Enc -> Var a -> IO ()
var e v0 = shared e encVars enc (unsafeCoerce v0)
 where
  enc :: Var Any -> IO ()
  enc = \case
    TyVar nm u k -> tag e 0 >> bin e nm >> bin e u >> ty e k
    Id nm u t sc -> tag e 1 >> bin e nm >> bin e u >> ty e t >> bin e sc

dataCon :: Enc -> DataCon -> IO ()
dataCon e = shared e encDataCons $ \dc -> do
  bin e (dcName dc) >> bin e (dcUniq dc) >> bin e (dcTag dc) >> ty e (dcLegacyType dc)
  list e (var e) (dcUnivTyVars dc) >> list e (var e) (dcExtTyVars dc)
  list e (ty e) (dcArgTys dc)
  bin e (dcArgStrict dc) >> bin e (dcFieldLabels dc)

prim :: Enc -> PrimInfo -> IO ()
prim e = shared e encPrims $ \(PrimInfo nm t wi multi unf) -> do
  bin e nm >> ty e t >> bin e wi >> bin e multi
  case unf of
    NoUnfolding -> tag e 0
    Unfolding v -> tag e 1 >> var e v

bind :: Enc -> Bind Term -> IO ()
bind e = \case
  NonRec v x -> tag e 0 >> var e v >> term e x
  Rec bs -> tag e 1 >> list e (\(v, x) -> var e v >> term e x) bs

alt :: Enc -> Alt -> IO ()
alt e (p, x) = pat e p >> term e x

pat :: Enc -> Pat -> IO ()
pat e = \case
  DataPat dc tvs ids -> tag e 0 >> dataCon e dc >> list e (var e) tvs >> list e (var e) ids
  LitPat l -> tag e 1 >> bin e l
  DefaultPat -> tag e 2

tick :: Enc -> TickInfo -> IO ()
tick e = \case
  SrcSpan s -> tag e 0 >> bin e s
  NameMod m t -> tag e 1 >> bin e m >> ty e t
  DeDup -> tag e 2
  NoDeDup -> tag e 3
  Attributes t x -> tag e 4 >> ty e t >> term e x

frame :: Enc -> CoreContext -> IO ()
frame e = \case
  AppFun -> tag e 0
  AppArg a -> tag e 1 >> bin e a
  TyAppC -> tag e 2
  LetBinding v vs -> tag e 3 >> var e v >> list e (var e) vs
  LetBody bs -> tag e 4 >> list e (\(v, x) -> var e v >> term e x) bs
  LamBody v -> tag e 5 >> var e v
  TyLamBody v -> tag e 6 >> var e v
  CaseAlt p -> tag e 7 >> pat e p
  CaseScrut -> tag e 8
  CastBody -> tag e 9
  TickC t -> tag e 10 >> tick e t

binding :: Enc -> Binding Term -> IO ()
binding e (Binding v loc spec isPrim x r) =
  var e (globalIdLegacy v) >> bin e loc >> bin e spec >> bin e isPrim >> term e x >> bin e r

bindingMap :: Enc -> BindingMap -> IO ()
bindingMap e bm = list e (binding e) (UniqMap.elems bm)

capture :: Enc -> Capture -> IO ()
capture e c = do
  bin e (capName c) >> bin e (capFiring c) >> bin e (capTransformCounter c)
  var e (fst (capCurFun c)) >> bin e (snd (capCurFun c))
  bin e (capInScope c)
  list e (frame e) (capContext c)
  term e (capInput c) >> term e (capOutput c)
  bin e (capSupply c) >> bin e (capNameCounter c)
  let (heap, next) = capGlobalHeap c
  list e (\(k, x) -> int e k >> term e x) (IntMap.toList heap) >> int e next
  int e (capBindingsEpoch c)

finalState :: Enc -> FinalState -> IO ()
finalState e fs = do
  bindingMap e (fsBindings fs)
  bin e (fsWorkFreeBinders fs)
  bindingMap e (fsNormalized fs)
  list e (\((f, i, a), g) -> do
            var e f >> int e i
            either (\x -> tag e 0 >> term e x) (\t -> tag e 1 >> ty e t) a
            var e g)
    (Map.toAscList (fsSpecialisationCache fs))
  bin e (fsSpecialisationHistory fs)
  bin e (fsInlineHistory fs)
  bin e (fsPrimitiveArgs fs)
  bin e (fsRecursiveComponents fs)
  list e (\(k, x) -> term e k >> term e x) (fsWorkFreeAppCache fs)
  list e (var e) (fsTopEntities fs)
  bin e (fsOpts fs)
  bin e (fsUniqueBound fs)

-- | Write a log, encoded such that it keeps its sharing (see the module
-- header): its final state, how often transformations fired, and then its
-- captures one at a time, such that 'decodeCaptureLog' can decode them one
-- at a time.
hPutCaptureLog :: Handle -> CaptureLog -> IO ()
hPutCaptureLog h (CaptureLog fs firings epochs caps) = do
  e <- Enc <$> newIORef mempty <*> newTable <*> newTable <*> newTable <*> newTable <*> newTable
  let -- Write what was encoded, and forget the terms encoded so far (see the
      -- module header)
      flush = do
        b <- readIORef (encOut e)
        writeIORef (encOut e) mempty
        hPutBuilder h b
        let Table ref next = encTerms e
        writeIORef ref HashMap.empty
        writeIORef next 0
  finalState e fs >> bin e firings >> bin e epochs >> int e (length caps)
  flush
  forM_ caps $ \c -> capture e c >> flush

-- * Decoding

-- | The objects decoded so far, by kind and number
data Dec = Dec
  { decTerms :: !(DTable Term)
  , decTypes :: !(DTable Type)
  , decVars :: !(DTable (Var Any))
  , decDataCons :: !(DTable DataCon)
  , decPrims :: !(DTable PrimInfo)
  }

-- | The number of objects, and the objects by number
data DTable a = DTable !Int !(IntMap a)

emptyDTable :: DTable a
emptyDTable = DTable 0 IntMap.empty

type D = StateT Dec Get

dTag :: D Word8
dTag = lift getWord8

dInt :: D Int
dInt = fromIntegral <$> lift getInt64be

dBin :: Binary a => D a
dBin = lift Binary.get

dList :: D a -> D [a]
dList d = dInt >>= \n -> replicateM n d

dShared :: (Dec -> DTable a) -> (Dec -> DTable a -> Dec) -> D a -> D a
dShared look set d = dTag >>= \case
  0 -> do
    i <- dInt
    DTable _ tbl <- look <$> get
    case IntMap.lookup i tbl of
      Just x -> pure x
      Nothing -> error ("decodeCaptureLog: unknown reference " <> show i)
  _ -> do
    !x <- d
    s <- get
    let DTable n tbl = look s
    put (set s (DTable (n + 1) (IntMap.insert n x tbl)))
    pure x

dTerm :: D Term
dTerm = dShared decTerms (\s t -> s {decTerms = t}) $ dTag >>= \case
  0 -> Var <$> dVar
  1 -> Data <$> dDataCon
  2 -> Literal <$> dBin
  3 -> Prim <$> dPrim
  4 -> Lam <$> dVar <*> dTerm
  5 -> TyLam <$> dVar <*> dTerm
  6 -> App <$> dTerm <*> dTerm
  7 -> TyApp <$> dTerm <*> dType
  8 -> Let <$> dBind <*> dTerm
  9 -> Case <$> dTerm <*> dType <*> dList ((,) <$> dPat <*> dTerm)
  10 -> Cast <$> dTerm <*> dType <*> dType
  11 -> Tick <$> dTick <*> dTerm
  t -> error ("decodeCaptureLog: term tag " <> show t)

dType :: D Type
dType = dShared decTypes (\s t -> s {decTypes = t}) $ dTag >>= \case
  0 -> VarTy <$> dVar
  1 -> ConstTy <$> dBin
  2 -> ForAllTy <$> dVar <*> dType
  3 -> AppTy <$> dType <*> dType
  4 -> LitTy <$> dBin
  5 -> AnnType <$> dBin <*> dType
  t -> error ("decodeCaptureLog: type tag " <> show t)

dVar :: D (Var a)
dVar = unsafeCoerce <$> dShared decVars (\s t -> s {decVars = t}) (dTag >>= \case
  0 -> TyVar <$> dBin <*> dBin <*> dType
  1 -> Id <$> dBin <*> dBin <*> dType <*> dBin
  t -> error ("decodeCaptureLog: variable tag " <> show t) :: D (Var Any))

dDataCon :: D DataCon
dDataCon = dShared decDataCons (\s t -> s {decDataCons = t}) $
  mkDataConFromClash <$> dBin <*> dBin <*> dBin <*> dType
         <*> dList dVar <*> dList dVar <*> dList dType
         <*> dBin <*> dBin

dPrim :: D PrimInfo
dPrim = dShared decPrims (\s t -> s {decPrims = t}) $
  PrimInfo <$> dBin <*> dType <*> dBin <*> dBin <*> (dTag >>= \case
    0 -> pure NoUnfolding
    _ -> Unfolding <$> dVar)

dBind :: D (Bind Term)
dBind = dTag >>= \case
  0 -> NonRec <$> dVar <*> dTerm
  _ -> Rec <$> dList ((,) <$> dVar <*> dTerm)

dPat :: D Pat
dPat = dTag >>= \case
  0 -> DataPat <$> dDataCon <*> dList dVar <*> dList dVar
  1 -> LitPat <$> dBin
  _ -> pure DefaultPat

dTick :: D TickInfo
dTick = dTag >>= \case
  0 -> SrcSpan <$> dBin
  1 -> NameMod <$> dBin <*> dType
  2 -> pure DeDup
  3 -> pure NoDeDup
  _ -> Attributes <$> dType <*> dTerm

dFrame :: D CoreContext
dFrame = dTag >>= \case
  0 -> pure AppFun
  1 -> AppArg <$> dBin
  2 -> pure TyAppC
  3 -> LetBinding <$> dVar <*> dList dVar
  4 -> LetBody <$> dList ((,) <$> dVar <*> dTerm)
  5 -> LamBody <$> dVar
  6 -> TyLamBody <$> dVar
  7 -> CaseAlt <$> dPat
  8 -> pure CaseScrut
  9 -> pure CastBody
  _ -> TickC <$> dTick

dBinding :: D (Binding Term)
dBinding = Binding <$> (mkGlobalIdFromClash <$> dVar) <*> dBin <*> dBin <*> dBin <*> dTerm <*> dBin

dBindingMap :: D BindingMap
dBindingMap = UniqMap.fromList . map (\b -> (globalIdUniq (bindingId b), b)) <$> dList dBinding

dCapture :: D Capture
dCapture = do
  nm <- dBin
  firing <- dBin
  counter <- dBin
  cf <- (,) <$> dVar <*> dBin
  is <- dBin
  frames <- dList dFrame
  input <- dTerm
  output <- dTerm
  sup <- dBin
  names <- dBin
  heap <- IntMap.fromList <$> dList ((,) <$> dInt <*> dTerm)
  next <- dInt
  epoch <- dInt
  pure (Capture nm firing counter cf is frames input output sup names (heap, next) epoch)

dFinalState :: D FinalState
dFinalState = do
  bs <- dBindingMap
  wf <- dBin
  normalized <- dBindingMap
  specCache <- Map.fromDistinctAscList <$> dList (do
    f <- dVar
    i <- dInt
    a <- dTag >>= \case
      0 -> Left <$> dTerm
      _ -> Right <$> dType
    g <- dVar
    pure ((f, i, a), g))
  FinalState bs wf normalized specCache
    <$> dBin <*> dBin <*> dBin <*> dBin
    <*> dList ((,) <$> dTerm <*> dTerm)
    <*> dList dVar <*> dBin <*> dBin

-- | Decode a log written by 'hPutCaptureLog', from the start of a byte string.
-- Decodes the captures lazily, as the list is consumed. Captures don't share
-- terms with each other, so a capture can be garbage collected once it has
-- been consumed, except for its types and variables.
decodeCaptureLog :: BL.ByteString -> CaptureLog
decodeCaptureLog bs0 = case runGetOrFail (runStateT header dec0) bs0 of
  Left (_, _, err) -> error ("decodeCaptureLog: " <> err)
  Right (rest, _, ((fs, firings, epochs, n), dec1)) ->
    CaptureLog fs firings epochs (go n (forgetTerms dec1) rest)
 where
  dec0 = Dec emptyDTable emptyDTable emptyDTable emptyDTable emptyDTable
  header = (,,,) <$> dFinalState <*> dBin <*> dBin <*> dInt

  go :: Int -> Dec -> BL.ByteString -> [Capture]
  go 0 _ _ = []
  go k dec bs = case runGetOrFail (runStateT dCapture dec) bs of
    Left (_, _, err) -> error ("decodeCaptureLog: capture: " <> err)
    Right (rest, _, (c, dec')) -> c : go (k - 1) (forgetTerms dec') rest

  forgetTerms dec = dec {decTerms = emptyDTable}
