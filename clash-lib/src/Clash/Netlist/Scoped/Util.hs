{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utilities of Netlist generation on scoped Clash Core: opening a normalized
  component into a single scope ('splitNormalized'), and giving its variables
  their HDL identifiers ('mkUniqueNormalized').

  The other utilities are adapters that run their counterparts in
  "Clash.Netlist.Util" on Clash Core, see
  'Clash.Netlist.Scoped.Types.toLegacy'. TODO(Phase 5, Util): port.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Netlist.Scoped.Util
  ( -- * Components
    SplitNormalized (..)
  , splitNormalized
  , UniqueNormalized (..)
  , mkUniqueNormalized
  , renameBinder
  , uniqueIdentifier
  , idToPort
  , idToInPort
  , idToOutPort
    -- * Adapters
  , withTicks
  , unsafeCoreTypeToHWTypeM
  , unsafeCoreTypeToHWTypeM'
  ) where

import           Control.Applicative         ((<|>))
import           Control.Exception           (throw)
import qualified Control.Lens                as Lens
import           Control.Monad               (when, zipWithM)
import           Control.Monad.Extra         (concatMapM)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (catMaybes, isNothing)
import           Data.Text                   (Text)
import           GHC.Stack                   (HasCallStack)

import           Clash.Annotations.TopEntity (TopEntity)
import qualified Clash.Core.HasType          as C (inferCoreTypeOf)
import qualified Clash.Core.Name             as C
import           Clash.Core.Pretty           (showPpr)
import qualified Clash.Core.Term             as C
import qualified Clash.Core.Type             as C (Type)
import           Clash.Core.Term             (MultiPrimInfo (..))
import           Clash.Core.TermInfo         (multiPrimInfo')
import qualified Clash.Core.Var              as C
import           Clash.Util                  (ClashException (..), curLoc)
import           Clash.Util.Supply           (Supply, splitSupply)

import           Clash.Core.Scoped.Convert   (fromClashTypeClosed, toClashTerm, toClashType)
import           Clash.Core.Scoped.FreeVars  (tmNameOccursIn)
import           Clash.Core.Scoped.Subst
  (openBind, openIdBinder, teleIdVars, withFreshIdBinder)
import           Clash.Core.Scoped.Syntax
import           Clash.Core.Scoped.Term      (collectArgsTicks, collectTicks, mkTicks, stripTicks)
import           Clash.Core.Scoped.TypeEnv   (TypeEnv, typeEnvTyConMap)

import           Clash.Netlist.BlackBox.Types (BlackBoxMeta (..))
import qualified Clash.Netlist.Id            as Id
import           Clash.Netlist.Scoped.BlackBox
  (extractPrimWarnOrFail, mkBlackBoxContext)
import           Clash.Netlist.Scoped.Types
import           Clash.Netlist.Types
  (BlackBox, Declaration, DeclarationType (..), ExpandedPortName, ExpandedTopEntity (..),
   FilteredHWType (..), HWType, Identifier, NetlistMonad, backend, curCompNm, tcCache)
import           Clash.Netlist.Util
  (containsBiSignalIn, evalBlackBox, expandTopEntityOrErrM, isBiSignalIn, isVoid,
   mkTopInput, mkTopOutput, preserveVarEnv, primWarnContext)
import qualified Clash.Netlist.Util          as Legacy
import           Clash.Primitives.Types      (CompiledPrimitive, Primitive (..))

-- * Components

-- | A let-binding of a component
data LetBinding (n :: S) = LetBinding (LocalId n) (Term n)

instance Sinkable LetBinding where
  sinkabilityProof r (LetBinding i e) =
    LetBinding (sinkabilityProof r i) (sinkabilityProof r e)

-- | A normalized component, opened into a single scope @l@: its arguments, its
-- let-bindings, and its result, a variable
data SplitNormalized where
  SplitNormalized
    :: Distinct l
    => Scope l
    -> [LocalId l]
    -- ^ Arguments
    -> [(LocalId l, Term l)]
    -- ^ Let-bindings
    -> LocalId l
    -- ^ Result
    -> SplitNormalized

-- | Open a normalized term into a single scope: its lambdas, and the
-- let-expressions in their body, see 'SplitNormalized'. The ticks around the
-- let-expressions go on every right-hand side. Gives the reason if the term
-- is not in normal form.
--
-- Clash Core's 'Clash.Netlist.Util.splitNormalized' had to deshadow nested
-- let-expressions where a binder captured a variable ('squashLets'), but the
-- binders of scoped Core are distinct.
splitNormalized :: TypeEnv -> Term 'VoidS -> Either String SplitNormalized
splitNormalized tenv expr = lams emptyScope [] expr
 where
  lams :: Distinct n => Scope n -> [LocalId n] -> Term n -> Either String SplitNormalized
  lams sc argsRev = \case
    Lam b e -> openIdBinder sc b e $ \sc' b' e' ->
      lams sc' (binderLocalId b' : sink1 argsRev) e'
    TyLam {} -> Left ($(curLoc) ++ "Not in normal form: tyArgs")
    e -> case collectTicks e of
      (e1@Let {}, ticks) -> lets sc (reverse argsRev) ticks [] e1
      _ -> Left ($(curLoc) ++ "Not in normal form: no Letrec:\n\n" ++ showPpr exprC ++
                 "\n\nWhich has type:\n\n" ++ showPpr (C.inferCoreTypeOf (typeEnvTyConMap tenv) exprC))

  lets
    :: Distinct n => Scope n -> [LocalId n] -> [TickInfo n] -> [LetBinding n] -> Term n
    -> Either String SplitNormalized
  lets sc args ticks accRev = \case
    Let bind body -> openBind sc bind body $ \sc' bind' body' ->
      let new = case bind' of
            NonRec b x -> [LetBinding (binderLocalId b) (sink x)]
            Rec bs xs -> zipWith LetBinding (teleLocalIds bs) xs
      in lets sc' (sink1 args) (sink1 ticks) (reverse new ++ sink1 accRev) body'
    e -> case stripTicks e of
      Var o ty n ->
        Right (SplitNormalized sc args
                 [ (i, mkTicks x ticks) | LetBinding i x <- reverse accRev ]
                 (LocalId o ty n))
      t -> Left ($(curLoc) ++ "Not in normal form: res not simple var: " ++ showPpr (toClashTerm t))

  teleLocalIds :: Distinct l => Tele IdBinder n l -> [LocalId l]
  teleLocalIds bs = [ LocalId o ty n | Var o ty n <- teleIdVars bs ]

  exprC = toClashTerm expr

-- | A component whose variables have their HDL identifiers, see
-- 'mkUniqueNormalized'
data UniqueNormalized where
  UniqueNormalized
    :: Distinct l
    => CompCtx l
    -- ^ The HDL identifiers of the variables
    -> [Bool]
    -- ^ Which arguments are void
    -> [(Identifier, HWType)]
    -- ^ Input ports
    -> [Declaration]
    -- ^ Declarations of the input ports
    -> [(Identifier, HWType)]
    -- ^ Output ports
    -> [Declaration]
    -- ^ Declarations of the output ports
    -> [(LocalId l, Term l)]
    -- ^ Let-bindings
    -> Maybe (LocalId l)
    -- ^ The variable assigned to the output port, if any
    -> UniqueNormalized

-- | Give the variables of a component unique HDL identifiers, and generate its
-- ports. Like Clash Core's 'Clash.Netlist.Util.mkUniqueNormalized': it
-- generates the same identifiers, in the same order, but puts them in the
-- context rather than in the occurrence names of the variables.
mkUniqueNormalized
  :: HasCallStack
  => TypeEnv
  -> Supply
  -- ^ Uniques for the Clash Core counterparts of the variables
  -> Maybe (Maybe TopEntity)
  -- ^ Top entity annotation where:
  --
  --     * Nothing: term is not a top entity
  --     * Just Nothing: term is a top entity, but has no explicit annotation
  --     * Just (Just ..): term is a top entity, and has an explicit annotation
  -> Maybe C.Type
  -- ^ The type of the result, if not that of the result variable
  -> SplitNormalized
  -> NetlistMonad UniqueNormalized
mkUniqueNormalized tenv supply0 topMM resTyM (SplitNormalized sc args binds res0) = do
  let res = case resTyM of
        Just ty -> res0 {localIdType = fromClashTypeClosed sc ty}
        Nothing -> res0
      (supply1, supply2) = splitSupply supply0
  -- Generate port names and add them to set of seen identifiers
  argHwtys <- mapM (unsafeCoreTypeToHWTypeM $(curLoc) . toClashType . localIdType) args
  resHwty <- unsafeCoreTypeToHWTypeM $(curLoc) (toClashType (localIdType res))
  etopM <-
    mapM
      (expandTopEntityOrErrM
         (zip (map (Just . legacyOccId) args) argHwtys)
         (Just (legacyOccId res), resHwty))
      topMM

  -- Make arguments unique
  (wereVoids, iports, iwrappers, argNames) <- mkUniqueArguments etopM args

  -- Make result unique. This might yield 'Nothing' in which case the result
  -- was a single BiSignalOut. This is superfluous in the HDL, as the argument
  -- will already contain a bidirectional signal complementing the BiSignalOut.
  resM <- mkUniqueResult etopM res
  case resM of
    Just (oports, owrappers, res1Name) -> do
      -- Collect new names, see 'renameBinder' for more information. The
      -- blackboxes see the original names.
      let ctx0 = compCtxFromList sc tenv supply1
                   [ (v, Id.unsafeMake (C.nameOcc (localIdOcc v)), toClashType (localIdType v))
                   | v <- args ++ map fst binds ]
      renames <- concatMapM (renameBinder ctx0 binds) binds
      let
        isRes = (== localIdName res)
        resRenameM0 = case filter (isRes . fst) renames of
          (_, occ) : _ -> Just occ
          [] -> Nothing
        renames0 = Map.fromList [ (tmNameRaw nm, occ) | (nm, occ) <- renames, not (isRes nm) ]
        -- Is the result variable read by any of the other binders? In that case
        -- we need to add a redirection as most synthesis tools don't allow reads
        -- from output ports. Note that if the result is renamed anyway, we don't
        -- have to do anything here.
        resultRead = any (tmNameOccursIn (localIdName res) . snd) binds
        resBinder = case filter (isRes . localIdName . fst) binds of
          (b, _) : _ -> b
          [] -> error (concat [ "internal error: unable to find: "
                              , show (localIdOcc res), " in: "
                              , show (map (localIdOcc . fst) binds) ])
        -- The renamed result binder: renamed by a blackbox, it has the type of
        -- the binder, renamed because it is read, that of the result.
        resRenameM1 =
          (\occ -> resBinder {localIdOcc = occ}) <$> resRenameM0
            <|> (if resultRead
                   then Just res {localIdOcc = C.appendToName (localIdOcc res) "_rec"}
                   else Nothing)
        renamed b = maybe b (\occ -> b {localIdOcc = occ})
                      (Map.lookup (tmNameRaw (localIdName b)) renames0)
        (bndrsL, bndrsR) = case break (isRes . localIdName) (map fst binds) of
          (ls, _ : rs) -> (ls, rs)
          _ -> error (concat [ "internal error: unable to find: "
                             , show (localIdOcc res), " in: "
                             , show (map (localIdOcc . fst) binds) ])

      -- The result binder: if it was renamed, it is an internal signal with
      -- an indirection to the output port. The indirection binder must not
      -- inherit any 'AnnType' synthesis attributes attached to the top
      -- entity's return type; those belong on the output port only. See #3224.
      resN <- case resRenameM1 of
        Nothing -> pure (res, res1Name)
        Just newName0 -> do
          let newName0' = newName0 {localIdType = stripAnnTypes (localIdType newName0)}
          i <- uniqueIdentifier (localIdOcc newName0')
          pure (newName0' {localIdName = localIdName res}, i)

      namesL <- mapM (\b -> (b,) <$> uniqueIdentifier (localIdOcc (renamed b))) bndrsL
      namesR <- mapM (\b -> (b,) <$> uniqueIdentifier (localIdOcc (renamed b))) bndrsR

      let
        binds1 = [ (if isRes (localIdName b) then fst resN else b, e) | (b, e) <- binds ]
        names = argNames ++ namesL ++ [resN] ++ namesR
        result
          :: Distinct x => CompCtx x -> [(LocalId x, Term x)] -> LocalId x
          -> NetlistMonad UniqueNormalized
        result ctx bs r =
          pure (UniqueNormalized ctx wereVoids iports iwrappers oports owrappers bs (Just r))
      case resRenameM1 of
        Nothing ->
          result (mkCtx sc supply2 names) binds1 (fst resN)
        Just _ ->
          -- Assign the internal signal to the output port
          withFreshIdBinder sc (localIdOcc res) (localIdType res) $ \sc' b ->
            let res1 = binderLocalId b
                extraBind = (res1, sink (localIdVar (fst resN)))
            in result (mkCtx sc' supply2 (sink1Names names ++ [(res1, res1Name)]))
                 (sink1Binds binds1 ++ [extraBind]) res1

    Nothing -> do
      names <- mapM (\(b, _) -> (b,) <$> uniqueIdentifier (localIdOcc b)) binds
      pure (UniqueNormalized (mkCtx sc supply2 (argNames ++ names))
              wereVoids iports iwrappers [] [] binds Nothing)
 where
  -- A Clash Core variable with the human readable name of a variable, as a
  -- name hint
  legacyOccId v = C.mkLocalId (toClashType (localIdType v)) (localIdOcc v)

  mkCtx :: Scope x -> Supply -> [(LocalId x, Identifier)] -> CompCtx x
  mkCtx sc' sup names = compCtxFromList sc' tenv sup
    [ (v, i, toClashType (localIdType v)) | (v, i) <- names ]

  sink1Names :: DExt x y => [(LocalId x, Identifier)] -> [(LocalId y, Identifier)]
  sink1Names = map (\(v, i) -> (sink v, i))

  sink1Binds :: DExt x y => [(LocalId x, Term x)] -> [(LocalId y, Term y)]
  sink1Binds bs = [ (i, e) | LetBinding i e <- sink1 [ LetBinding i e | (i, e) <- bs ] ]

  stripAnnTypes :: Type x -> Type x
  stripAnnTypes = \case
    AnnType _ ty -> stripAnnTypes ty
    ty -> ty

-- | Give the arguments of a component unique HDL identifiers, or those of
-- the ports of the top entity
mkUniqueArguments
  :: Maybe (ExpandedTopEntity Identifier)
  -- ^ Top entity annotation where:
  --
  --     * Nothing: term is not a top entity
  --     * Just ..: term is a top entity
  -> [LocalId l]
  -> NetlistMonad
       ( [Bool]                    -- Were voids
       , [(Identifier,HWType)]     -- Arguments and their types
       , [Declaration]             -- Extra declarations
       , [(LocalId l, Identifier)] -- The HDL identifiers of the arguments
       )
mkUniqueArguments Nothing args = do
  names <- mapM (\a -> (a,) <$> uniqueIdentifier (localIdOcc a)) args
  ports <- mapM (uncurry idToInPort) names
  return (map isNothing ports, catMaybes ports, [], names)

mkUniqueArguments (Just (ExpandedTopEntity{..})) args = do
  (ports, decls, names) <- (unzip3 . catMaybes) <$> zipWithM go et_inputs args
  return ( map isNothing et_inputs
         , concat ports
         , concat decls
         , names ++ [ (a, Id.unsafeMake (C.nameOcc (localIdOcc a)))
                    | (Nothing, a) <- zip et_inputs args ] )
 where
  go :: Maybe (ExpandedPortName Identifier) -> LocalId l
     -> NetlistMonad (Maybe ([(Identifier, HWType)], [Declaration], (LocalId l, Identifier)))
  go Nothing _var =
    pure Nothing
  go (Just port) var = do
    (ports, decls, _, portI) <- mkTopInput port
    return (Just (ports, decls, (var, Id.unsafeMake (Id.toText portI))))

-- | Give the result of a component a unique HDL identifier, or that of the
-- output port of the top entity. Gives 'Nothing' if it has no output port.
mkUniqueResult
  :: Maybe (ExpandedTopEntity Identifier)
  -- ^ Top entity annotation where:
  --
  --     * Nothing: term is not a top entity
  --     * Just ..: term is a top entity
  -> LocalId l
  -> NetlistMonad (Maybe ([(Identifier,HWType)], [Declaration], Identifier))
mkUniqueResult Nothing res = do
  i <- uniqueIdentifier (localIdOcc res)
  portM <- idToOutPort i (localIdType res)
  case portM of
    Just port -> return (Just ([port], [], i))
    _         -> return Nothing

mkUniqueResult (Just (ExpandedTopEntity{et_output=Nothing})) _res =
  pure Nothing
mkUniqueResult (Just (ExpandedTopEntity{et_output=Just iPort})) res = do
  (_, sp) <- Lens.use curCompNm
  (FilteredHWType hwty _) <- unsafeCoreTypeToHWTypeM $(curLoc) (toClashType (localIdType res))
  when (containsBiSignalIn hwty)
    (throw (ClashException sp ($(curLoc) ++ "BiSignalIn cannot be part of a function's result. Use 'readFromBiSignal'.") Nothing))
  (ports, decls, portI) <- mkTopOutput iPort
  return (Just (ports, decls, Id.unsafeMake (Id.toText portI)))

-- | A unique HDL identifier based on the human readable name of a variable.
-- Like Clash Core's 'Clash.Netlist.Util.mkUnique', which put it in the
-- occurrence name of the variable, from where
-- 'Clash.Netlist.Id.unsafeFromCoreId' made it an identifier again.
uniqueIdentifier :: C.Name C.Term -> NetlistMonad Identifier
uniqueIdentifier occ = Id.unsafeMake . Id.toText <$> Id.make (C.nameOcc occ)

-- | Same as 'idToPort', but throws an error if the port is a composite type
-- with a BiSignalIn
idToInPort :: LocalId n -> Identifier -> NetlistMonad (Maybe (Identifier, HWType))
idToInPort var i = do
  (_, sp) <- Lens.use curCompNm
  portM <- idToPort i (localIdType var)
  case portM of
    Just (_,hty) -> do
      when (containsBiSignalIn hty && not (isBiSignalIn hty))
        (throw (ClashException sp ($(curLoc) ++ "BiSignalIn currently cannot be part of a composite type when it's a function's argument") Nothing))
      return portM
    _ -> return Nothing

-- | Same as 'idToPort', but throws an error if the port is of type BiSignalIn
idToOutPort :: Identifier -> Type n -> NetlistMonad (Maybe (Identifier,HWType))
idToOutPort i ty = do
  (_, srcspan) <- Lens.use curCompNm
  portM <- idToPort i ty
  case portM of
    Just (_,hty) -> do
      when (containsBiSignalIn hty)
        (throw (ClashException srcspan ($(curLoc) ++ "BiSignalIn cannot be part of a function's result. Use 'readFromBiSignal'.") Nothing))
      return portM
    _ -> return Nothing

-- | The port of a variable, given its HDL identifier and type. 'Nothing' if
-- the variable is void.
idToPort :: Identifier -> Type n -> NetlistMonad (Maybe (Identifier, HWType))
idToPort i ty = do
  hwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) (toClashType ty)
  if isVoid hwTy
    then return Nothing
    else return (Just (i, hwTy))

-- | The new human readable name of the binder of a let-binding, if its
-- right-hand side is a blackbox that requests a specific name for its result.
-- It might give multiple names in case of a multi result primitive.
renameBinder
  :: forall n
   . (HasCallStack, Distinct n)
  => CompCtx n
  -> [(LocalId n, Term n)]
  -- ^ All let-bindings, for warnings
  -> (LocalId n, Term n)
  -> NetlistMonad [(TmName n, C.Name C.Term)]
renameBinder ctx binds (i, collectArgsTicks -> (k, args, ticks)) =
  withTicks ctx ticks $ \_ -> do
    case k of
      Prim p ->
        case C.primMultiResult p of
          C.SingleResult -> extractPrimWarnOrFail context (C.primName p) >>= goSingle p
          C.MultiResult -> extractPrimWarnOrFail context (C.primName p) >>= goMulti p
      _ -> pure []
 where
  context =
    primWarnContext
      [ (toLegacyLocalId ctx b, toLegacy ctx e) | (b, e) <- binds ]
      (showPpr (C.varName (toLegacyLocalId ctx i)))
      (toLegacyArgs ctx args)

  -- Routine for multi result primitives. For more info:
  -- 'Clash.Normalize.Transformations.setupMultiResultPrim'.
  goMulti :: C.PrimInfo -> CompiledPrimitive -> NetlistMonad [(TmName n, C.Name C.Term)]
  goMulti pInfo (BlackBoxHaskell{function=(_, function)}) = do
    tcm <- Lens.view tcCache
    let MultiPrimInfo{mpi_resultTypes} = multiPrimInfo' tcm pInfo
        (args1, resArgs) = splitAt (length args - length mpi_resultTypes) args
        resIds = [ LocalId o ty n | Left (Var o ty n) <- resArgs ]
    funRes <- preserveVarEnv $ case function of
      LegacyBBF f -> f False (C.primName pInfo) (toLegacyArgs ctx args1) mpi_resultTypes
      ScopedBBF f ->
        f ctx False (C.primName pInfo) args1
          (map (fromClashTypeClosed (compScope ctx)) mpi_resultTypes)
    let BlackBoxMeta{bbResultNames} = either error fst funRes
    go (C.primName pInfo) resIds args1 bbResultNames
  goMulti _ _ = pure []

  -- Routine for single result primitives (the default kind of primitive)
  goSingle :: C.PrimInfo -> CompiledPrimitive -> NetlistMonad [(TmName n, C.Name C.Term)]
  goSingle pInfo (BlackBoxHaskell{function=(_, function)}) = do
    funRes <- preserveVarEnv $
      callBlackBoxFunction ctx function False (C.primName pInfo) args [localIdType i]
    case either error fst funRes of
      BlackBoxMeta{bbResultNames=[bbResultName]} ->
        go (C.primName pInfo) [i] args [bbResultName]
      _ -> pure []
  goSingle pInfo (BlackBox{resultNames=[resultName]}) = do
    go (C.primName pInfo) [i] args [resultName]
  goSingle _ _ = pure []

  go
    :: Text -> [LocalId n] -> [Either (Term n) (Type n)] -> [BlackBox]
    -> NetlistMonad [(TmName n, C.Name C.Term)]
  go nm is0 bbArgs bbResultTemplates = do
    (bbCtx, _) <- preserveVarEnv (mkBlackBoxContext ctx nm Concurrent is0 bbArgs)
    be <- Lens.use backend
    let
      newNames = map (evalBlackBox be bbCtx) bbResultTemplates
      modName v newRetName = (localIdName v, (localIdOcc v) {C.nameOcc = newRetName})

    -- TODO: _sameName check disabled due to
    --       https://github.com/clash-lang/clash-compiler/issues/1566
    -- Don't rename if we didn't change any names, it will cause superfluous
    -- redirections in 'mkUniqueNormalized'.
    pure (zipWith modName is0 newNames)

-- * Adapters

-- | Run an action in the context of the ticks of a term: see
-- 'Clash.Netlist.Util.withTicks'
withTicks
  :: CompCtx n
  -> [TickInfo n]
  -> ([Declaration] -> NetlistMonad a)
  -- ^ The source ticks are turned into 'TickDecl's and are passed as an
  -- argument to the action
  -> NetlistMonad a
withTicks ctx ticks = Legacy.withTicks (map (toLegacyTick ctx) ticks)

-- | Converts a type to a HWType: see 'Clash.Netlist.Util.unsafeCoreTypeToHWTypeM'
unsafeCoreTypeToHWTypeM :: String -> C.Type -> NetlistMonad FilteredHWType
unsafeCoreTypeToHWTypeM = Legacy.unsafeCoreTypeToHWTypeM

-- | Converts a type to a HWType: see 'Clash.Netlist.Util.unsafeCoreTypeToHWTypeM''
unsafeCoreTypeToHWTypeM' :: String -> C.Type -> NetlistMonad HWType
unsafeCoreTypeToHWTypeM' = Legacy.unsafeCoreTypeToHWTypeM'
