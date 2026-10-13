{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utilities of Netlist generation on scoped Clash Core: opening a normalized
  component into a single scope ('splitNormalized'), and giving its variables
  their HDL identifiers ('mkUniqueNormalized').

  The translation of the types of scoped Core to HWTypes
  ('unsafeCoreTypeToHWTypeM', ...) takes the scope of the type, to look
  through newtypes and type families.
-}

{-# LANGUAGE PolyKinds #-}
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
  , primWarnContext
  , localIdText
  , globalIdentifier
  , idToPort
  , idToInPort
  , idToOutPort
    -- * Types
  , unsafeCoreTypeToHWTypeM
  , unsafeCoreTypeToHWTypeM'
  , coreTypeToHWTypeM
  , coreTypeToHWTypeM'
    -- * Ticks
  , withTicks
  ) where

import           Control.Applicative         ((<|>))
import           Control.Exception           (throw)
import qualified Control.Lens                as Lens
import           Control.Monad               (when, zipWithM)
import           Control.Monad.Extra         (concatMapM)
import           Control.Monad.Reader        (local)
import           Control.Monad.Trans.Except  (runExcept)
import qualified Data.IntMap.Strict          as IntMap
import qualified Data.List                   as List
import           Data.Proxy                  (Proxy (..))
import qualified Data.Text                   as Text
import           GHC.TypeLits                (someNatVal)
import           GHC.TypeNats                (SomeNat (..))
import           GHC.Utils.Outputable        (ppr, showSDocUnsafe)
import qualified Data.Map.Strict             as Map
import           Control.Monad.State.Strict  (runState)
import           Data.Maybe                  (catMaybes, isNothing)
import           Data.Text                   (Text)
import           GHC.Stack                   (HasCallStack)

import Clash.Core.Basic (IsMultiPrim (..), LitTy (..))
import           Clash.Annotations.TopEntity (TopEntity)
import qualified Clash.Core.Name             as C
import           Clash.Core.Pretty           (PrettyPrec, showPpr, showPprDiag)
import           Clash.Core.Scoped.Pretty    ()
import           Clash.Core.Scoped.TermInfo  (MultiPrimInfo (..), multiPrimInfo')
import           Clash.Util                  (ClashException (..), curLoc)

import           Clash.Core.Scoped.Alpha     (aeqTm, aeqTy)
import           Clash.Core.Scoped.Evaluator (nfData)
import           Clash.Core.Scoped.Evaluator.Primitives (scopedPrimReductions)
import           Clash.Core.Scoped.Evaluator.Types
  (Env (..), Machine (..), PrimReductions (..), initMachine)
import           Clash.Core.Scoped.NameMap   (emptyNameMap)
import           Clash.Core.Scoped.TermLiteral (termToDataError)
import           Clash.Core.Scoped.Type      (coreView, tyLitShow)
import           Clash.Core.Scoped.FreeVars  (tmNameOccursIn)
import           Clash.Core.Scoped.Subst
  (closeTerm, openBind, openIdBinder, teleIdVars, withFreshIdBinder)
import           Clash.Core.Scoped.Syntax
import           Clash.Core.Scoped.HasType   (inferCoreTypeOf)
import           Clash.Core.Scoped.Term      (collectArgsTicks, collectTicks, mkTicks, stripTicks)
import           Clash.Core.Scoped.TypeEnv   (TypeEnv)

import           Clash.Netlist.BlackBox.Types (BlackBoxMeta (..))
import qualified Clash.Netlist.Id            as Id
import {-# SOURCE #-} Clash.Netlist.Scoped.BlackBox (extractPrimWarnOrFail, mkBlackBoxContext)
import           Clash.Netlist.Scoped.Types
import           Clash.Netlist.Types
  (BlackBox, CommentOrDirective (..), Declaration (..), DeclarationType (..), ExpandedPortName,
   ExpandedTopEntity (..),
   FilteredHWType (..), HWType, Identifier, NetlistEnv (..), NetlistMonad, backend, bindings,
   curCompNm, customReprs, htyCache, tcCache, typeEnv, typeTranslator)
import           Clash.Netlist.Util
  (containsBiSignalIn, coreTypeToHWType, evalBlackBox, expandTopEntityOrErrM, isBiSignalIn,
   applyNameMod, isVoid, mkTopInput, mkTopOutput, preserveVarEnv, stripFiltered,
   unsafeCoreTypeToHWType)
import qualified Clash.Sized.Vector          as V
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
      _ -> Left ($(curLoc) ++ "Not in normal form: no Letrec:\n\n" ++ showPpr expr ++
                 "\n\nWhich has type:\n\n" ++ showPpr (inferCoreTypeOf tenv emptyScope expr))

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
      t -> Left ($(curLoc) ++ "Not in normal form: res not simple var: " ++ showPpr t)

  teleLocalIds :: Distinct l => Tele IdBinder n l -> [LocalId l]
  teleLocalIds bs = [ LocalId o ty n | Var o ty n <- teleIdVars bs ]

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
  -> Maybe (Maybe TopEntity)
  -- ^ Top entity annotation where:
  --
  --     * Nothing: term is not a top entity
  --     * Just Nothing: term is a top entity, but has no explicit annotation
  --     * Just (Just ..): term is a top entity, and has an explicit annotation
  -> Maybe (Type 'VoidS)
  -- ^ The type of the result, if not that of the result variable
  -> SplitNormalized
  -> NetlistMonad UniqueNormalized
mkUniqueNormalized tenv topMM resTyM (SplitNormalized sc args binds res0) = do
  let res = case resTyM of
        Just ty -> res0 {localIdType = sinkClosed ty}
        Nothing -> res0
  -- Generate port names and add them to set of seen identifiers
  argHwtys <- mapM (unsafeCoreTypeToHWTypeM $(curLoc) sc . localIdType) args
  resHwty <- unsafeCoreTypeToHWTypeM $(curLoc) sc (localIdType res)
  etopM <-
    mapM
      (expandTopEntityOrErrM
         (zip (map (Just . C.nameOcc . localIdOcc) args) argHwtys)
         (Just (C.nameOcc (localIdOcc res)), resHwty))
      topMM

  -- Make arguments unique
  (wereVoids, iports, iwrappers, argNames) <- mkUniqueArguments sc etopM args

  -- Make result unique. This might yield 'Nothing' in which case the result
  -- was a single BiSignalOut. This is superfluous in the HDL, as the argument
  -- will already contain a bidirectional signal complementing the BiSignalOut.
  resM <- mkUniqueResult sc etopM res
  case resM of
    Just (oports, owrappers, res1Name) -> do
      -- Collect new names, see 'renameBinder' for more information. The
      -- blackboxes see the original names.
      let ctx0 = compCtxFromList sc tenv
                   [ (v, Id.unsafeMake (C.nameOcc (localIdOcc v))) | v <- args ++ map fst binds ]
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
          result (mkCtx sc names) binds1 (fst resN)
        Just _ ->
          -- Assign the internal signal to the output port
          withFreshIdBinder sc (localIdOcc res) (localIdType res) $ \sc' b ->
            let res1 = binderLocalId b
                extraBind = (res1, sink (localIdVar (fst resN)))
            in result (mkCtx sc' (sink1Names names ++ [(res1, res1Name)]))
                 (sink1Binds binds1 ++ [extraBind]) res1

    Nothing -> do
      names <- mapM (\(b, _) -> (b,) <$> uniqueIdentifier (localIdOcc b)) binds
      pure (UniqueNormalized (mkCtx sc (argNames ++ names))
              wereVoids iports iwrappers [] [] binds Nothing)
 where
  mkCtx :: Scope x -> [(LocalId x, Identifier)] -> CompCtx x
  mkCtx sc' = compCtxFromList sc' tenv

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
  :: Distinct l
  => Scope l
  -> Maybe (ExpandedTopEntity Identifier)
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
mkUniqueArguments sc Nothing args = do
  names <- mapM (\a -> (a,) <$> uniqueIdentifier (localIdOcc a)) args
  ports <- mapM (uncurry (idToInPort sc)) names
  return (map isNothing ports, catMaybes ports, [], names)

mkUniqueArguments _sc (Just (ExpandedTopEntity{..})) args = do
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
  :: Distinct l
  => Scope l
  -> Maybe (ExpandedTopEntity Identifier)
  -- ^ Top entity annotation where:
  --
  --     * Nothing: term is not a top entity
  --     * Just ..: term is a top entity
  -> LocalId l
  -> NetlistMonad (Maybe ([(Identifier,HWType)], [Declaration], Identifier))
mkUniqueResult sc Nothing res = do
  i <- uniqueIdentifier (localIdOcc res)
  portM <- idToOutPort sc i (localIdType res)
  case portM of
    Just port -> return (Just ([port], [], i))
    _         -> return Nothing

mkUniqueResult _sc (Just (ExpandedTopEntity{et_output=Nothing})) _res =
  pure Nothing
mkUniqueResult sc (Just (ExpandedTopEntity{et_output=Just iPort})) res = do
  (_, sp) <- Lens.use curCompNm
  (FilteredHWType hwty _) <- unsafeCoreTypeToHWTypeM $(curLoc) sc (localIdType res)
  when (containsBiSignalIn hwty)
    (throw (ClashException sp ($(curLoc) ++ "BiSignalIn cannot be part of a function's result. Use 'readFromBiSignal'.") Nothing))
  (ports, decls, portI) <- mkTopOutput iPort
  return (Just (ports, decls, Id.unsafeMake (Id.toText portI)))

-- | A unique HDL identifier based on the human readable name of a variable.
-- Like Clash Core's 'Clash.Netlist.Util.mkUnique', which put it in the
-- occurrence name of the variable, from where
-- 'Clash.Netlist.Id.unsafeFromCoreId' made it an identifier again.
uniqueIdentifier :: C.Name Term -> NetlistMonad Identifier
uniqueIdentifier occ = Id.unsafeMake . Id.toText <$> Id.make (C.nameOcc occ)

-- | Same as 'idToPort', but throws an error if the port is a composite type
-- with a BiSignalIn
idToInPort
  :: Distinct n => Scope n -> LocalId n -> Identifier -> NetlistMonad (Maybe (Identifier, HWType))
idToInPort sc var i = do
  (_, sp) <- Lens.use curCompNm
  portM <- idToPort sc i (localIdType var)
  case portM of
    Just (_,hty) -> do
      when (containsBiSignalIn hty && not (isBiSignalIn hty))
        (throw (ClashException sp ($(curLoc) ++ "BiSignalIn currently cannot be part of a composite type when it's a function's argument") Nothing))
      return portM
    _ -> return Nothing

-- | Same as 'idToPort', but throws an error if the port is of type BiSignalIn
idToOutPort
  :: Distinct n => Scope n -> Identifier -> Type n -> NetlistMonad (Maybe (Identifier,HWType))
idToOutPort sc i ty = do
  (_, srcspan) <- Lens.use curCompNm
  portM <- idToPort sc i ty
  case portM of
    Just (_,hty) -> do
      when (containsBiSignalIn hty)
        (throw (ClashException srcspan ($(curLoc) ++ "BiSignalIn cannot be part of a function's result. Use 'readFromBiSignal'.") Nothing))
      return portM
    _ -> return Nothing

-- | The port of a variable, given its HDL identifier, and its type in the
-- given scope. 'Nothing' if the variable is void.
idToPort :: Distinct n => Scope n -> Identifier -> Type n -> NetlistMonad (Maybe (Identifier, HWType))
idToPort sc i ty = do
  hwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) sc ty
  if isVoid hwTy
    then return Nothing
    else return (Just (i, hwTy))

-- | The context that the warnings of a guarded primitive mention (see
-- 'Clash.Netlist.BlackBox.extractPrimWarnOrFail'): the variable its result is
-- assigned to, and the arguments it is applied to. Variables among the
-- arguments are resolved into the given sibling let-bindings, a few levels
-- deep.
primWarnContext
  :: forall n
   . [(LocalId n, Term n)]
  -- ^ Sibling bindings, used to resolve variable references. Pass @[]@ if they
  -- are not available.
  -> String
  -- ^ Name of the binder the primitive's result is assigned to
  -> [Either (Term n) (Type n)]
  -- ^ Arguments the primitive is applied to
  -> String
primWarnContext binds dst args =
  "result assigned to: " <> dst <> ", applied to: "
    <> unwords (map (either (showArg maxResolveDepth) showDiag) args)
 where
  showArg :: Int -> Term n -> String
  showArg depth (stripTicks -> v@(Var _ _ nm))
    | depth > 0
    , Just rhs <- List.lookup nm [ (localIdName b, e) | (b, e) <- binds ]
    , (h, rhsArgs, _) <- collectArgsTicks rhs
    = "(" <> showPpr v <> " = "
        <> unwords (showDiag h : map (either (showArg (depth - 1)) showDiag) rhsArgs)
        <> ")"
  showArg _ tm = showDiag tm

  showDiag :: PrettyPrec p => p -> String
  showDiag = showPprDiag maxArgLen

  -- How deep to resolve variable references into sibling bindings
  maxResolveDepth = 5

  -- Longest rendering of a single argument
  maxArgLen = 160

-- | A local variable in messages: its HDL identifier, or its human readable
-- name if the context doesn't give it one
localIdText :: CompCtx n -> LocalId n -> String
localIdText ctx i =
  maybe (showPpr (localIdOcc i)) (Text.unpack . Id.toText)
    (localIdentifierMaybe ctx (localIdName i))

-- | The HDL identifier of a global variable used as an expression: its
-- occurrence name
globalIdentifier :: GlobalId -> Identifier
globalIdentifier = Id.unsafeMake . C.nameOcc . globalIdName

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
  -> NetlistMonad [(TmName n, C.Name Term)]
renameBinder ctx binds (i, collectArgsTicks -> (k, args, ticks)) =
  withTicks ctx ticks $ \_ -> do
    case k of
      Prim p ->
        case primMultiResult p of
          SingleResult -> extractPrimWarnOrFail context (primName p) >>= goSingle p
          MultiResult -> extractPrimWarnOrFail context (primName p) >>= goMulti p
      _ -> pure []
 where
  context = primWarnContext binds (localIdText ctx i) args

  -- Routine for multi result primitives. For more info:
  -- 'Clash.Normalize.Scoped.Transformations.MultiPrim.setupMultiResultPrim'.
  goMulti :: PrimInfo -> CompiledPrimitive -> NetlistMonad [(TmName n, C.Name Term)]
  goMulti pInfo (BlackBoxHaskell{function=(_, function)}) = do
    tcm <- Lens.view tcCache
    let MultiPrimInfo{mpi_resultCount} = multiPrimInfo' tcm pInfo
        (args1, resArgs) = splitAt (length args - mpi_resultCount) args
        resIds = [ LocalId o ty n | Left (Var o ty n) <- resArgs ]
    -- The result types of 'MultiPrimInfo' are those of the type of the
    -- primitive, whose type variables are not in scope: those of the result
    -- variables are instantiated
    funRes <- preserveVarEnv $
      callBlackBoxFunction ctx function False (primName pInfo) args1 (map localIdType resIds)
    let BlackBoxMeta{bbResultNames} = either error fst funRes
    go (primName pInfo) resIds args1 bbResultNames
  goMulti _ _ = pure []

  -- Routine for single result primitives (the default kind of primitive)
  goSingle :: PrimInfo -> CompiledPrimitive -> NetlistMonad [(TmName n, C.Name Term)]
  goSingle pInfo (BlackBoxHaskell{function=(_, function)}) = do
    funRes <- preserveVarEnv $
      callBlackBoxFunction ctx function False (primName pInfo) args [localIdType i]
    case either error fst funRes of
      BlackBoxMeta{bbResultNames=[bbResultName]} ->
        go (primName pInfo) [i] args [bbResultName]
      _ -> pure []
  goSingle pInfo (BlackBox{resultNames=[resultName]}) = do
    go (primName pInfo) [i] args [resultName]
  goSingle _ _ = pure []

  go
    :: Text -> [LocalId n] -> [Either (Term n) (Type n)] -> [BlackBox]
    -> NetlistMonad [(TmName n, C.Name Term)]
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

-- * Types

-- | Converts a type, in the given scope, to a HWType; errors on failure. See
-- 'Clash.Netlist.Util.unsafeCoreTypeToHWType'.
unsafeCoreTypeToHWTypeM
  :: Distinct n
  => String
  -> Scope n
  -> Type n
  -> NetlistMonad FilteredHWType
unsafeCoreTypeToHWTypeM loc sc ty = do
  (_,cmpNm) <- Lens.use curCompNm
  tt <- Lens.use typeTranslator
  reprs <- Lens.view customReprs
  te <- Lens.view typeEnv
  htm0 <- Lens.use htyCache
  let (hty, htm1) = runState (unsafeCoreTypeToHWType cmpNm loc tt reprs te sc ty) htm0
  htyCache Lens..= htm1
  return hty

-- | Same as 'unsafeCoreTypeToHWTypeM', but discards void filter information
unsafeCoreTypeToHWTypeM' :: Distinct n => String -> Scope n -> Type n -> NetlistMonad HWType
unsafeCoreTypeToHWTypeM' loc sc ty = stripFiltered <$> unsafeCoreTypeToHWTypeM loc sc ty

-- | Converts a type, in the given scope, to a HWType; 'Nothing' on failure.
-- See 'Clash.Netlist.Util.coreTypeToHWType'.
coreTypeToHWTypeM :: Distinct n => Scope n -> Type n -> NetlistMonad (Maybe FilteredHWType)
coreTypeToHWTypeM sc ty = do
  tt <- Lens.use typeTranslator
  reprs <- Lens.view customReprs
  te <- Lens.view typeEnv
  htm0 <- Lens.use htyCache
  let (hty, htm1) = runState (coreTypeToHWType tt reprs te sc ty) htm0
  htyCache Lens..= htm1
  return (either (const Nothing) Just hty)

-- | Same as 'coreTypeToHWTypeM', but discards void filter information
coreTypeToHWTypeM' :: Distinct n => Scope n -> Type n -> NetlistMonad (Maybe HWType)
coreTypeToHWTypeM' sc ty = fmap stripFiltered <$> coreTypeToHWTypeM sc ty

-- * Ticks

-- | Run an action in the context of the given source ticks and name modifier
-- ticks. See 'Clash.Netlist.Util.withTicks'.
--
-- The attributes of an 'Attributes' tick are evaluated with the evaluator of
-- scoped Clash Core ('nfData').
withTicks
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> [TickInfo n]
  -> ([Declaration] -> NetlistMonad a)
  -- ^ The source ticks are turned into 'TickDecl's and are passed as an
  -- argument to the action. Name modifier ticks will change the local
  -- environment for the action.
  -> NetlistMonad a
withTicks ctx ticks0 k = go [] (reverse (List.nubBy eqTick ticks0))
 where
  sc = compScope ctx
  te = compTypeEnv ctx

  go decls [] = k (reverse decls)

  go decls (DeDup:ticks) = go decls ticks

  go decls (NoDeDup:ticks) = go decls ticks

  go decls (SrcSpan sp:ticks) =
    go (TickDecl (Comment (Text.pack (showSDocUnsafe (ppr sp)))):decls) ticks

  go decls (Attributes ty attrs0:ticks) =
    -- The term `attrs0` represents a value of type `Vec n (Attr String)`,
    -- which 'termToDataError' decodes. It only decodes literals, so the term
    -- is evaluated as far as it is data ('nfData') first: it might not be
    -- beta-reduced, e.g. `(/\b.Nil 0 b (0~0)) Bool`.
    case coreView te sc ty of
      LitTy (NumTy n)
        | Just (SomeNat (Proxy :: Proxy n)) <- someNatVal n
        -> case closeTerm attrs0 of
            Just attrsC -> do
              globals <- Lens.use bindings
              let
                m0 = initMachine emptyScope emptyNameMap globals (IntMap.empty, 0) attrsC
                -- See 'Clash.Rewrite.Scoped.Whnf.evaluatorEnv'
                env0 = Env te (PrimReductions scopedPrimReductions Nothing)
                attrsE = nfData env0 m0 (termToDataError . mTerm)
              case attrsE of
                Left msg -> error msg
                Right attrs1 ->
                  let attrs2 = fmap (fmap Text.pack) attrs1
                  in local (\env -> env {_localAttrs = V.toList @n attrs2}) (go decls ticks)
            Nothing -> error errFV
      _ -> error errNoNum
   where
    errHeader = "ERROR: cannot add attribues: " <> showPpr attrs0
    errNoNum  = unlines [errHeader
                        ,"REASON: Internal length is not a literal" <> showPpr ty]
    errFV     = unlines [errHeader
                        ,"REASON: attributes mentions variables" <> showPpr attrs0
                        ]

  go decls (NameMod m nm0:ticks) =
    case runExcept (tyLitShow te sc nm0) of
      Right nm1 -> local (applyNameMod m (Text.pack nm1)) (go decls ticks)
      _ -> go decls ticks

  -- See the 'Eq' instance of 'C.TickInfo'
  eqTick :: TickInfo n -> TickInfo n -> Bool
  eqTick t1 t2 = case (t1, t2) of
    (SrcSpan sp1, SrcSpan sp2) -> sp1 == sp2
    (NameMod m1 ty1, NameMod m2 ty2) -> m1 == m2 && aeqTy ty1 ty2
    (DeDup, DeDup) -> True
    (NoDeDup, NoDeDup) -> True
    (Attributes ty1 e1, Attributes ty2 e2) -> aeqTy ty1 ty2 && aeqTm e1 e2
    _ -> False
