{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd
                    2017-2018, Google Inc.
                    2021-2024, QBayLogic B.V.
                    2022     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utilities for converting Core Type/Term to Netlist datatypes
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
#if !MIN_VERSION_ghc(8,8,0)
{-# LANGUAGE MonadFailDesugaring #-}
#endif
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Netlist.Util where

import           Data.Coerce             (coerce)
import           Control.Exception       (throw)
import           Control.Lens            ((.=), (%=))
import qualified Control.Lens            as Lens
import           Control.Monad           (when, zipWithM)
import           Control.Monad.Extra     (concatMapM)
import           Control.Monad.Reader    (ask, local)
import qualified Control.Monad.State as State
import           Control.Monad.State.Strict
  (State, evalState, get, modify, runState)
import           Control.Monad.Trans.Except
  (ExceptT (..), runExcept, runExceptT, throwE)
import           Data.Bifunctor          (second)
import           Data.Char               (ord)
import           Data.Either             (partitionEithers)
import           Data.Foldable           (Foldable(toList))
import           Data.Functor            (($>))
import           Data.Hashable           (Hashable)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
#if MIN_VERSION_ghc(9,10,0)
import qualified GHC.Data.Word64Set      as IntSet
#else
import qualified Data.IntSet             as IntSet
#endif
import           Data.Primitive.ByteArray (ByteArray (..))
import           Control.Applicative     (Alternative((<|>)))
import           Data.List               (unzip4, partition)
import qualified Data.List               as List
import qualified Data.Map                as Map
import           Data.Map                (Map)
import           Data.Maybe
  (catMaybes, fromMaybe, isNothing, mapMaybe, isJust, listToMaybe, maybeToList)
import           Text.Printf             (printf)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Extra         (showt)
import           Data.Text.Lazy          (toStrict)
import           Data.Text.Prettyprint.Doc.Extra

#if MIN_VERSION_base(4,15,0)
import           GHC.Num.Integer                  (Integer (..))
#else
import           GHC.Integer.GMP.Internals        (Integer (..), BigNat (..))
#endif

import           GHC.Stack               (HasCallStack)

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Utils.Monad         (zipWith3M)
import           GHC.Utils.Outputable    (ppr, showSDocUnsafe)
#else
import           MonadUtils              (zipWith3M)
import           Outputable              (ppr, showSDocUnsafe)
#endif

import           Clash.Annotations.TopEntity
  (TopEntity(..), PortName(..), defSyn)
import           Clash.Annotations.BitRepresentation.ClashLib
  (coreToType')
import           Clash.Annotations.BitRepresentation.Internal
  (CustomReprs, ConstrRepr'(..), DataRepr'(..), getDataRepr,
   uncheckedGetConstrRepr)
import           Clash.Annotations.SynthesisAttributes (Attr)
import           Clash.Annotations.Primitive (HDL(VHDL))
import           Clash.Backend           (HasUsageMap (..), HWKind(..), hdlHWTypeKind, hdlKind)
import           Clash.Core.DataCon      (DataCon (..))
import           Clash.Core.EqSolver     (typeEq)
import           Clash.Core.FreeVars     (typeFreeVars, typeFreeVars')
import           Clash.Core.HasFreeVars  (elemFreeVars)
import           Clash.Core.HasType
import qualified Clash.Core.Literal      as C
import           Clash.Core.Name
  (Name (..), appendToName, nameOcc)
import           Clash.Core.Pretty       (showPpr)
import           Clash.Core.Subst
  (Subst (..), extendIdSubst, extendIdSubstList, extendInScopeId,
   extendInScopeIdList, mkSubst, substTm)
import           Clash.Core.Term
  (primMultiResult, MultiPrimInfo(..), Alt, LetBinding, Pat (..), Term (..), TickInfo (..), NameMod (..),
   IsMultiPrim (..), collectArgsTicks, collectTicks, collectBndrs, PrimInfo(primName), mkTicks, stripTicks)
import           Clash.Core.TermInfo
import           Clash.Core.TyCon
  (TyCon (FunTyCon), TyConName, TyConMap, tyConDataCons)
import           Clash.Core.Type
  (Type (..), TyVar, TypeView (..), coreView1, normalizeType, splitTyConAppM, tyView)
import           Clash.Core.Util
  (substArgTys, tyLitShow)
import           Clash.Core.Var
  (Id, Var (..), mkLocalId, modifyVarName)
import           Clash.Core.VarEnv
  (InScopeSet, extendInScopeSetList, uniqAway, lookupVarEnv)
import qualified Clash.Data.UniqMap as UniqMap
import {-# SOURCE #-} Clash.Netlist.BlackBox
import {-# SOURCE #-} Clash.Netlist.BlackBox.Util
import           Clash.Netlist.BlackBox.Types
  (bbResultNames, BlackBoxMeta(BlackBoxMeta))
import qualified Clash.Netlist.Id as Id
import           Clash.Netlist.Types     as HW
import           Clash.Primitives.Types
import           Clash.Util
import qualified Clash.Util.Interpolate  as I

hmFindWithDefault :: (Eq k, Hashable k) => v -> k -> HashMap k v -> v
#if MIN_VERSION_unordered_containers(0,2,11)
hmFindWithDefault = HashMap.findWithDefault
#else
hmFindWithDefault = HashMap.lookupDefault
#endif

-- | Generate a simple port_name expression. See:
--
--   https://www.hdlworks.com/hdl_corner/vhdl_ref/VHDLContents/PortMap.htm
--
-- This function will simply make the left part of a single port map, e.g. "Rst"
-- in:
--
--  Rst => Reset
--
-- If you need more complex constructions, e.g.
--
--  Q(3 downto 1)
--
-- you can build an Expr manually.
instPort :: Text -> Expr
instPort pn = Identifier (Id.unsafeMake pn) Nothing

-- | Throw away information indicating which constructor fields were filtered
-- due to being void.
stripFiltered :: FilteredHWType -> HWType
stripFiltered (FilteredHWType hwty _filtered) = hwty

-- | Strip as many "Void" layers as possible. Might still return a Void if the
-- void doesn't contain a hwtype.
stripVoid :: HWType -> HWType
stripVoid (Void (Just e)) = stripVoid e
stripVoid e = e

flattenFiltered :: FilteredHWType -> [[Bool]]
flattenFiltered (FilteredHWType _hwty filtered) = map (map fst) filtered

isVoidMaybe :: Bool -> Maybe HWType -> Bool
isVoidMaybe dflt Nothing = dflt
isVoidMaybe _dflt (Just t) = isVoid t

-- | Determines if type is a zero-width construct ("void")
isVoid :: HWType -> Bool
isVoid Void {} = True
isVoid _       = False

-- | Same as @isVoid@, but on @FilteredHWType@ instead of @HWType@
isFilteredVoid :: FilteredHWType -> Bool
isFilteredVoid = isVoid . stripFiltered

squashLets :: Term -> Term
squashLets (Letrec xs (Letrec ys e)) =
  squashLets (Letrec (xs <> ys) e)
squashLets e = e

-- | Split a normalized term into: a list of arguments, a list of let-bindings,
-- and a variable reference that is the body of the let-binding. Returns a
-- String containing the error if the term was not in a normalized form.
splitNormalized
  :: TyConMap
  -> Term
  -> (Either String ([Id],[LetBinding],Id))
splitNormalized tcm expr = case collectBndrs expr of
  (args, collectTicks -> (squashLets -> Letrec xes e, ticks))
    | (tmArgs,[]) <- partitionEithers args -> case stripTicks e of
        Var v -> Right (tmArgs, fmap (second (`mkTicks` ticks)) xes,v)
        t     -> Left ($(curLoc) ++ "Not in normal form: res not simple var: " ++ showPpr t)
    | otherwise -> Left ($(curLoc) ++ "Not in normal form: tyArgs")
  _ ->
    Left ($(curLoc) ++ "Not in normal form: no Letrec:\n\n" ++ showPpr expr ++
          "\n\nWhich has type:\n\n" ++ showPpr ty)
 where
  ty = inferCoreTypeOf tcm expr

-- | Converts a Core type to a HWType given a function that translates certain
-- builtin types. Errors if the Core type is not translatable.
unsafeCoreTypeToHWType
  :: SrcSpan
  -- ^ Approximate location in original source file
  -> String
  -> (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -> CustomReprs
  -> TyConMap
  -> Type
  -> State HWMap FilteredHWType
unsafeCoreTypeToHWType sp loc builtInTranslation reprs m ty =
  either (\msg -> throw (ClashException sp (loc ++ msg) Nothing)) id <$>
    coreTypeToHWType builtInTranslation reprs m ty

-- | Same as @unsafeCoreTypeToHWTypeM@, but discards void filter information
unsafeCoreTypeToHWTypeM'
  :: String
  -> Type
  -> NetlistMonad HWType
unsafeCoreTypeToHWTypeM' loc ty =
  stripFiltered <$> unsafeCoreTypeToHWTypeM loc ty

-- | Converts a Core type to a HWType within the NetlistMonad; errors on failure
unsafeCoreTypeToHWTypeM
  :: String
  -> Type
  -> NetlistMonad FilteredHWType
unsafeCoreTypeToHWTypeM loc ty = do
  (_,cmpNm) <- Lens.use curCompNm
  tt        <- Lens.use typeTranslator
  reprs     <- Lens.view customReprs
  tcm       <- Lens.view tcCache
  htm0      <- Lens.use htyCache
  let (hty,htm1) = runState (unsafeCoreTypeToHWType cmpNm loc tt reprs tcm ty) htm0
  htyCache Lens..= htm1
  return hty

-- | Same as @coreTypeToHWTypeM@, but discards void filter information
coreTypeToHWTypeM'
  :: Type
  -- ^ Type to convert to HWType
  -> NetlistMonad (Maybe HWType)
coreTypeToHWTypeM' ty =
  fmap stripFiltered <$> coreTypeToHWTypeM ty


-- | Converts a Core type to a HWType within the NetlistMonad; 'Nothing' on failure
coreTypeToHWTypeM
  :: Type
  -- ^ Type to convert to HWType
  -> NetlistMonad (Maybe FilteredHWType)
coreTypeToHWTypeM ty = do
  tt    <- Lens.use typeTranslator
  reprs <- Lens.view customReprs
  tcm   <- Lens.view tcCache
  htm0  <- Lens.use htyCache
  let (hty,htm1) = runState (coreTypeToHWType tt reprs tcm ty) htm0
  htyCache Lens..= htm1
  return (either (const Nothing) Just hty)

-- | Constructs error message for unexpected projections out of a type annotated
-- with a custom bit representation.
unexpectedProjectionErrorMsg
  :: DataRepr'
  -> Int
  -- ^ Constructor index
  -> Int
  -- ^ Field index
  -> String
unexpectedProjectionErrorMsg dataRepr cI fI =
     "Unexpected projection of zero-width type: " ++ show (drType dataRepr)
  ++ ". Tried to make a projection of field " ++ show fI ++ " of "
  ++ constrNm ++ ". Did you try to project a field marked as zero-width"
  ++ " by a custom bit representation annotation?"
 where
   constrNm = show (crName (drConstrs dataRepr !! cI))

-- | Helper function of 'maybeConvertToCustomRepr'
convertToCustomRepr
  :: HasCallStack
  => CustomReprs
  -> DataRepr'
  -> HWType
  -> HWType
convertToCustomRepr reprs dRepr@(DataRepr' name' size constrs) hwTy =
  if length constrs == nConstrs then
    if size <= 0 then
      Void (Just cs)
    else
      cs
  else
    error (unwords
      [ "Type", show name', "has", show nConstrs, "constructor(s), "
      , "but the custom bit representation only specified", show (length constrs)
      , "constructors."
      ])
 where
  cs = insertVoids $ case hwTy of
    Sum name conIds ->
      CustomSum name dRepr size (map packSum conIds)
    SP name conIdsAndFieldTys ->
      CustomSP name dRepr size (map packSP conIdsAndFieldTys)
    Product name maybeFieldNames fieldTys
      | [ConstrRepr' _cName _pos _mask _val fieldAnns] <- constrs ->
      CustomProduct name dRepr size maybeFieldNames (zip fieldAnns fieldTys)
    _ ->
      error
        ( "Found a custom bit representation annotation " ++ show dRepr ++ ", "
       ++ "but it was applied to an unsupported HWType: " ++ show hwTy ++ ".")

  nConstrs :: Int
  nConstrs = case hwTy of
    (Sum _name conIds) -> length conIds
    (SP _name conIdsAndFieldTys) -> length conIdsAndFieldTys
    (Product {}) -> 1
    _ -> error ("Unexpected HWType: " ++ show hwTy)

  packSP (name, tys) = (uncheckedGetConstrRepr name reprs, name, tys)
  packSum name = (uncheckedGetConstrRepr name reprs, name)

  -- Replace some "hwTy" with "Void (Just hwTy)" if the custom bit
  -- representation indicated that field is represented by zero bits. We can't
  -- simply remove them, as we'll later have to deal with an "overapplied"
  -- constructor. If we remove the arguments altogether, we wouldn't know which
  -- - on their own potentially non-void! - arguments to ignore.
  insertVoids :: HWType -> HWType
  insertVoids (CustomSP i d s constrs0) =
    CustomSP i d s (map go0 constrs0)
   where
    go0 (con@(ConstrRepr' _ _ _ _ fieldAnns), i0, hwTys) =
      (con, i0, zipWith go1 fieldAnns hwTys)
    go1 0 hwTy0 = Void (Just hwTy0)
    go1 _ hwTy0 = hwTy0
  insertVoids (CustomProduct i d s f fieldAnns) =
    CustomProduct i d s f (map go fieldAnns)
   where
    go (0, hwTy0) = (0, Void (Just hwTy0))
    go (n, hwTy0) = (n, hwTy0)
  insertVoids hwTy0 = hwTy0

-- | Given a map containing custom bit representation, a type, and the same
-- type represented as HWType, convert the HWType to a CustomSP/CustomSum if
-- it has a custom bit representation.
maybeConvertToCustomRepr
  :: CustomReprs
  -- ^ Map containing all custom representations index on its type
  -> Type
  -- ^ Custom reprs are index on type, so we need the clash core type to look
  -- it up.
  -> FilteredHWType
  -- ^ Type of previous argument represented as a HWType
  -> FilteredHWType
maybeConvertToCustomRepr reprs (coreToType' -> Right tyName) (FilteredHWType hwTy filtered)
  | Just dRepr <- getDataRepr tyName reprs =
    FilteredHWType
      (convertToCustomRepr reprs dRepr hwTy)
      [ [ (fieldAnn == 0, hwty) | ((_, hwty), fieldAnn) <- zip fields (crFieldAnns constr) ]
                                | (fields, constr) <- zip filtered (drConstrs dRepr)]
maybeConvertToCustomRepr _reprs _ty hwTy = hwTy

-- | Same as @coreTypeToHWType@, but discards void filter information
coreTypeToHWType'
  :: (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -> CustomReprs
  -> TyConMap
  -> Type
  -- ^ Type to convert to HWType
  -> State HWMap (Either String HWType)
coreTypeToHWType' builtInTranslation reprs m ty =
  fmap stripFiltered <$> coreTypeToHWType builtInTranslation reprs m ty


-- | Converts a Core type to a HWType given a function that translates certain
-- builtin types. Returns a string containing the error message when the Core
-- type is not translatable.
coreTypeToHWType
  :: (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -> CustomReprs
  -> TyConMap
  -> Type
  -- ^ Type to convert to HWType
  -> State HWMap (Either String FilteredHWType)
coreTypeToHWType builtInTranslation reprs m ty = do
  htyM <- Map.lookup ty <$> get
  case htyM of
    Just hty -> return hty
    _ -> do
      hty0M <- builtInTranslation reprs m ty
      hty1  <- go hty0M ty
      modify (Map.insert ty hty1)
      return hty1
 where
  -- Try builtin translation; for now this is hardcoded to be the one in ghcTypeToHWType
  go :: Maybe (Either String FilteredHWType)
     -> Type
     -> State (Map Type (Either String FilteredHWType))
              (Either String FilteredHWType)
  go (Just hwtyE) _ = pure $ maybeConvertToCustomRepr reprs ty <$> hwtyE
  -- Strip transparant types:
  go _ (coreView1 m -> Just ty') =
    coreTypeToHWType builtInTranslation reprs m ty'
  -- Try to create hwtype based on AST:
  go _ (tyView -> TyConApp tc args) = runExceptT $ do
    hwty <- mkADT builtInTranslation reprs m (showPpr ty) tc args
    return (maybeConvertToCustomRepr reprs ty hwty)
  -- All methods failed:
  go _ _ = return $ Left $ "Can't translate non-tycon type: " ++ showPpr ty

-- | Generates original indices in list before filtering, given a list of
-- removed indices.
--
-- >>> originalIndices [False, False, True, False]
-- [0,1,3]
originalIndices
  :: [Bool]
  -- ^ Were voids. Length must be less than or equal to n.
  -> [Int]
  -- ^ Original indices
originalIndices wereVoids =
  [i | (i, void) <- zip [0..] wereVoids, not void]

-- | Converts an algebraic Core type (split into a TyCon and its argument) to a HWType.
mkADT
  :: (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -- ^ Hardcoded Type -> HWType translator
  -> CustomReprs
  -> TyConMap
  -- ^ TyCon cache
  -> String
  -- ^ String representation of the Core type for error messages
  -> TyConName
  -- ^ The TyCon
  -> [Type]
  -- ^ Its applied arguments
  -> ExceptT String (State HWMap) FilteredHWType
  -- ^ An error string or a tuple with the type and possibly a list of
  -- removed arguments.
mkADT _ _ m tyString tc _
  | isRecursiveTy m tc
  = throwE $ $(curLoc) ++ "Can't translate recursive type: " ++ tyString

mkADT builtInTranslation reprs m tyString tc args = case tyConDataCons (UniqMap.find tc m) of
  []  -> return (FilteredHWType (Void Nothing) [])
  dcs -> do
    let tcName           = nameOcc tc
        substArgTyss     = map (`substArgTys` args) dcs
    argHTyss0           <- mapM (mapM (ExceptT . coreTypeToHWType builtInTranslation reprs m)) substArgTyss
    let argHTyss1        = map (\tys -> zip (map isFilteredVoid tys) tys) argHTyss0
    let areVoids         = map (map fst) argHTyss1
    let filteredArgHTyss = map (map snd . filter (not . fst)) argHTyss1

    -- Every alternative is annotated with some examples. Be sure to read them.
    case (dcs, filteredArgHTyss) of
      _ | any (hasUnconstrainedExistential m) dcs ->
        throwE $ $(curLoc) ++
                 "Can't translate data types with unconstrained existentials: " ++
                 tyString
      -- Type has one constructor and that constructor has a single field,
      -- modulo empty fields if keepVoid is False. Examples of such fields
      -- are:
      --
      -- >>> data ABC = ABC Int
      -- >>> data DEF = DEF Int ()
      --
      -- Notice that @DEF@'s constructor has an "empty" second argument. The
      -- second field of FilteredHWType would then look like:
      --
      -- >>> [[False, True]]
      (_:[],[[elemTy]]) ->
        return (FilteredHWType (stripFiltered elemTy) argHTyss1)

      -- Type has one constructor, but multiple fields modulo empty fields
      -- (see previous case for more thorough explanation). Examples:
      --
      -- >>> data GHI = GHI Int Int
      -- >>> data JKL = JKL Int () Int
      --
      -- In the second case the second field of FilteredHWType would be
      -- [[False, True, False]]
      ([dcFieldLabels -> labels0],[elemTys@(_:_)]) -> do
        labelsM <-
          if null labels0 then
            return Nothing
          else
            -- Filter out labels belonging to arguments filtered due to being
            -- void. See argHTyss1.
            let areNotVoids = case areVoids of
                                areVoid:_ -> map not areVoid
                                _ -> error "internal error: insufficient areVoids"
                labels1     = filter fst (zip areNotVoids labels0)
                labels2     = map snd labels1
             in return (Just labels2)
        let hwty = Product tcName labelsM (map stripFiltered elemTys)
        return (FilteredHWType hwty argHTyss1)

      -- Either none of the constructors have fields, or they have been filtered
      -- due to them being empty. Examples:
      --
      -- >>> data MNO = M    | N | O
      -- >>> data PQR = P () | Q | R ()
      -- >>> data STU = STU
      -- >>> data VWX
      (_, concat -> [])
        -- If none of the dataconstructors have fields, and there are 1 or less
        -- of them, this type only has one inhabitant. It can therefore be
        -- represented by zero bits, and is therefore empty:
        | length dcs <= 1 -> case argHTyss0 of
            [argHTys0] ->
              -- We need this to preserve constraint-tuples of `KnownDomains`
              let argHTys1 = map (stripVoid . stripFiltered) argHTys0
              in  return (FilteredHWType
                            (Void (Just (Product tcName Nothing argHTys1)))
                            argHTyss1)
            _ -> return (FilteredHWType (Void Nothing) argHTyss1)
        -- None of the dataconstructors have fields. This type is therefore a
        -- simple Sum type.
        | otherwise ->
          return (FilteredHWType (Sum tcName $ map (nameOcc . dcName) dcs) argHTyss1)

      -- A sum of product, due to multiple constructors, where at least one
      -- of the constructor has one or more fields modulo empty fields. Example:
      --
      -- >>> data YZA = Y Int | Z () | A
      (_,elemHTys) ->
        return $ FilteredHWType (SP tcName $ zipWith
          (\dc tys ->  ( nameOcc (dcName dc), tys))
          dcs (map stripFiltered <$> elemHTys)) argHTyss1

-- | Determine whether a data constructor has unconstrained existential type
-- variables, i.e. those that cannot be inferred by the (potential) constraints
-- between the existential type variables and universal type variables.
--
-- So here we have an example of a constrained existential:
--
-- data Vec :: Nat -> Type -> Type
--  where
--   Nil  :: Vec 0 a
--   Cons :: forall m . (n ~ m + 1) => a -> Vec m a -> Vec n a
--
-- where we can generate a type for `m` when we know `n` (by doing `n-1`).
--
-- And here is an example of an unconstrained existential:
--
-- data SomeSNat where
--  where
--   SomeSNat :: forall m . SNat m -> SomeSNat
--
-- where there is no way to generate a type for `m` from any context.
--
-- So why do we care? Because terms need to be completely monomorphic in order
-- to be translated to circuits. And having a topEntity lambda-bound variable
-- with an unconstrained existential type prevents us from achieving a fully
-- monomorphic term.
hasUnconstrainedExistential
  :: TyConMap
  -> DataCon
  -> Bool
hasUnconstrainedExistential tcm dc =
  let eTVs        = dcExtTyVars dc
      uTVs        = dcUnivTyVars dc
      constraints = mapMaybe (typeEq tcm) (dcArgTys dc)

      -- Is the existential `eTV` constrained by the constraint `(ty1,ty2)`
      isConstrainedBy eTV (ty1,ty2) =
        let -- Free FVs in the LHS and RHS of the constraint that are not the
            -- in the set of universal type variables of the constructor.
            ty1FEVs = Lens.toListOf (typeFreeVars' ((`notElem` uTVs) . coerce)
                                                   IntSet.empty)
                                    ty1
            ty2FEVs = Lens.toListOf (typeFreeVars' ((`notElem` uTVs) . coerce)
                                                   IntSet.empty)
                                    ty2

            -- Determine whether `eTV` can be generated from one side of a
            -- constraint, under the assumption that the other side of the
            -- constraint mentions no existential type variables.
            isGenerative ::
              -- Side (LHS or RHS) of a constraint
              Type ->
              -- Its free type variables (that are no in the set of universal
              -- type variables)
              [TyVar] ->
              Bool
            isGenerative t efvs = case tyView t of
              TyConApp tcNm _
                | Just (FunTyCon {}) <- UniqMap.lookup tcNm tcm
                -- For type families we can only "calculate" the `eTV` if it is
                -- the only free variable. e.g. we can work out from `n + 1 ~ 4`
                -- that `n ~ 3`, but can't do anything for `n + m ~ 4`.
                -> [eTV] == efvs
                | otherwise
                -- Normal type constructors are fully generative, e.g. given:
                -- DomainConfiguration a b ~ DomainConfiguration "System" 10000
                --
                -- we can infer both `a ~ "System"` and `b ~ 10000`
                -> eTV `elem` efvs
              FunTy {}
                -- Functions are also fully generative
                -> eTV `elem` efvs
              OtherType other -> case other of
                VarTy v -> v == eTV
                LitTy _ -> False
                -- Anything else, like some higher-kinded quantified type we
                -- just give up for now. TODO: implement this
                _ -> False

            onlyTy1 = isGenerative ty1 ty1FEVs && null ty2FEVs
            onlyTy2 = isGenerative ty2 ty2FEVs && null ty1FEVs
        in  onlyTy1 || onlyTy2

      -- The existential type variables that are not constrained by any of the
      -- constraints.
      unconstrainedETVs =
        filter (\v -> not (any (isConstrainedBy v) constraints)) eTVs

  in  not (null unconstrainedETVs)


-- | Simple check if a TyCon is recursively defined.
--
-- Note [Look through type families in recursivity check]
--
-- Consider:
--
-- @
-- data SList :: [Type] -> Type where
--   SNil  :: SList []
--   CSons :: a -> Sing (as :: [k]) -> SList (a:as)
--
-- type family Sing [a] = SList [a]
-- @
--
-- Without looking through type families, we would think that /SList/ is not
-- recursive. This lead to issue #1921
isRecursiveTy :: TyConMap -> TyConName -> Bool
isRecursiveTy m tc = case tyConDataCons (UniqMap.find tc m) of
    []  -> False
    dcs -> let argTyss   = map dcArgTys dcs
               argTycons = (map fst . catMaybes)
                         $ (concatMap . map)
                               -- Note [Look through type families in recursivity check]
                               (splitTyConAppM . normalizeType m)
                               argTyss
           in tc `elem` argTycons

-- | Determines if a Core type is translatable to a HWType given a function that
-- translates certain builtin types.
representableType
  :: (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -> CustomReprs
  -> Bool
  -- ^ String considered representable
  -> TyConMap
  -> Type
  -> Bool
representableType builtInTranslation reprs stringRepresentable m =
    either (const False) isRepresentable .
    flip evalState mempty .
    coreTypeToHWType' builtInTranslation reprs m
  where
    isRepresentable hty = case hty of
      String            -> stringRepresentable
      Vector _ elTy     -> isRepresentable elTy
      RTree  _ elTy     -> isRepresentable elTy
      Product _ _ elTys -> all isRepresentable elTys
      SP _ elTyss       -> all (all isRepresentable . snd) elTyss
      BiDirectional _ t -> isRepresentable t
      Annotated _ ty    -> isRepresentable ty
      _                 -> True

-- | Determines the bitsize of a type. For types that don't get turned
-- into real values in hardware (string, integer) the size is 0.
typeSize :: HWType
         -> Int
typeSize (Void {}) = 0
typeSize FileType = 32 -- (ref. page 287 of IEEE 1364-2005)
typeSize String = 0
typeSize Integer = 0
typeSize (KnownDomain {}) = 0
typeSize Bool = 1
typeSize Bit = 1
typeSize (Clock _) = 1
typeSize (ClockN _) = 1
typeSize (Reset _) = 1
typeSize (Enable _) = 1
typeSize (BitVector i) = i
typeSize (Index 0) = 0
typeSize (Index 1) = 1
typeSize (Index u) = fromMaybe 0 (clogBase 2 u)
typeSize (Signed i) = i
typeSize (Unsigned i) = i
typeSize (Vector n el) = n * typeSize el
typeSize (MemBlob n m) = n * m
typeSize (RTree d el) = (2^d) * typeSize el
typeSize t@(SP _ cons) = conSize t +
  maximum (map (sum . map typeSize . snd) cons)
typeSize (Sum _ dcs) = fromMaybe 0 . clogBase 2 . toInteger $ length dcs
typeSize (Product _ _ tys) = sum $ map typeSize tys
typeSize (BiDirectional In h) = typeSize h
typeSize (BiDirectional Out _) = 0
typeSize (CustomSP _ _ size _) = fromIntegral size
typeSize (CustomSum _ _ size _) = fromIntegral size
typeSize (CustomProduct _ _ size _ _) = fromIntegral size
typeSize (Annotated _ ty) = typeSize ty

-- | Determines the bitsize of the constructor of a type
conSize :: HWType
        -> Int
conSize (SP _ cons) = fromMaybe 0 . clogBase 2 . toInteger $ length cons
conSize t           = typeSize t

-- | Gives the HWType corresponding to a term. Returns an error if the term has
-- a Core type that is not translatable to a HWType.
termHWType :: String
           -> Term
           -> NetlistMonad HWType
termHWType loc e = do
  m <- Lens.view tcCache
  let ty = inferCoreTypeOf m e
  stripFiltered <$> unsafeCoreTypeToHWTypeM loc ty

-- | Gives the HWType corresponding to a term. Returns 'Nothing' if the term has
-- a Core type that is not translatable to a HWType.
termHWTypeM
  :: Term
  -- ^ Term to convert to HWType
  -> NetlistMonad (Maybe FilteredHWType)
termHWTypeM e = do
  m  <- Lens.view tcCache
  let ty = inferCoreTypeOf m e
  coreTypeToHWTypeM ty

isBiSignalIn :: HWType -> Bool
isBiSignalIn (BiDirectional In _) = True
isBiSignalIn (Annotated _ ty)     = isBiSignalIn ty
isBiSignalIn _                    = False

isBiSignalOut :: HWType -> Bool
isBiSignalOut (BiDirectional Out _) = True
isBiSignalOut (Annotated _ ty)      = isBiSignalOut ty
isBiSignalOut _                     = False

containsBiSignalIn
  :: HWType
  -> Bool
containsBiSignalIn (BiDirectional In _) = True
containsBiSignalIn (Product _ _ tys) = any containsBiSignalIn tys
containsBiSignalIn (SP _ tyss)       = any (any containsBiSignalIn . snd) tyss
containsBiSignalIn (Vector _ ty)     = containsBiSignalIn ty
containsBiSignalIn (RTree _ ty)      = containsBiSignalIn ty
containsBiSignalIn (Annotated _ ty)  = containsBiSignalIn ty
containsBiSignalIn _                 = False

-- | Uniquely rename all the variables and their references in a normalized
-- term
mkUniqueNormalized
  :: HasCallStack
  => InScopeSet
  -> Maybe (Maybe TopEntity)
  -- ^ Top entity annotation where:
  --
  --     * Nothing: term is not a top entity
  --     * Just Nothing: term is a top entity, but has no explicit annotation
  --     * Just (Just ..): term is a top entity, and has an explicit annotation
  -> ( [Id]
     , [LetBinding]
     , Id
     )
  -> NetlistMonad
      ([Bool]
      ,[(Identifier,HWType)]
      ,[Declaration]
      ,[(Identifier,HWType)]
      ,[Declaration]
      ,[LetBinding]
      ,Maybe Id)
mkUniqueNormalized is0 topMM (args, binds, res) = do
  -- Generate port names and add them to set of seen identifiers
  argHwtys <- mapM (unsafeCoreTypeToHWTypeM $(curLoc) . coreTypeOf) args
  resHwty <- unsafeCoreTypeToHWTypeM $(curLoc) (coreTypeOf res)
  etopM <-
    mapM
      (expandTopEntityOrErrM (zip (map Just args) argHwtys) (Just res, resHwty))
      topMM

  -- Make arguments unique
  let (bndrs, exprs) = unzip binds
  let is1 = is0 `extendInScopeSetList` (args ++ bndrs)
  (wereVoids, iports, iwrappers, substArgs) <-
    mkUniqueArguments (mkSubst is1) etopM args

  -- Make result unique. This might yield 'Nothing' in which case the result
  -- was a single BiSignalOut. This is superfluous in the HDL, as the argument
  -- will already contain a bidirectional signal complementing the BiSignalOut.
  resM <- mkUniqueResult substArgs etopM res
  case resM of
    Just (oports, owrappers, res1, subst0) -> do
      -- Collect new names, see 'renameBinder' for more information
      (listToMaybe -> resRenameM0, HashMap.fromList -> renames0) <-
        partition ((== res) . fst) <$> concatMapM renameBinder binds

      let
        -- Is the result variable read by any of the other binders? In that case
        -- we need to add a redirection as most synthesis tools don't allow reads
        -- from output ports. Note that if the result is renamed anyway, we don't
        -- have to do anything here.
        resultRead = any (elemFreeVars res) exprs
        recResult = modifyVarName (`appendToName` "_rec") res
        resRenameM1 = resRenameM0 <|> orNothing resultRead (res, recResult)

      (resN, extraBind, subst1) <-
        case resRenameM1 of
          Nothing ->
            -- Result binder was not renamed, so we can assign result expression
            -- directly to new name given by 'res1'
            pure (res1, Nothing, subst0)
          Just (_, newName0) -> do
            -- Result binder was renamed. We cannot rename 'res1', so we need
            -- to create an indirection.
            ([newName1], s) <- mkUnique subst0 [newName0]
            pure (newName1, Just (res1, Var newName1), s)

      let
        -- Result binder is already unique, so don't rename that
        renames1 = [(b, hmFindWithDefault b b renames0) | b <- bndrs]
        (renamesL0, renamesR0) = case break ((==res) . fst) renames1 of
          (ls,_:rs) -> (ls,rs)
          _ -> error (concat [ "internal error: unable to find: "
                             , show res , " in: "
                             , show renames1 ])

      (renamesL1, subst2) <- mkUnique subst1 (map snd renamesL0)
      (renamesR1, subst3) <- mkUnique subst2 (map snd renamesR0)

      let
        exprs1 = map (substTm "mkUniqueNormalized1" subst3) exprs
        binds0 = zip (renamesL1 <> [resN] <> renamesR1) exprs1
        binds1 = binds0 <> maybeToList extraBind

      -- Return the uniquely named arguments, let-binders, and result
      return (wereVoids, iports, iwrappers, oports, owrappers, binds1, Just res1)

    Nothing -> do
      (bndrs1, substArgs1) <- mkUnique substArgs bndrs
      let binds1 = zip bndrs1 (map (substTm "mkUniqueNormalized2" substArgs1) exprs)
      return (wereVoids, iports, iwrappers, [], [], binds1, Nothing)

-- | Produce a 'Just' when predicate is True, else Nothing
orNothing :: Bool -> a -> Maybe a
orNothing True a = Just a
orNothing False _ = Nothing

-- | Set the name of the binder if the given term is a blackbox requesting
-- a specific name for the result binder. It might return multiple names in
-- case of a multi result primitive.
--
renameBinder :: (Id, Term) -> NetlistMonad [(Id, Id)]
renameBinder (i, collectArgsTicks -> (k, args, ticks)) = withTicks ticks $ \_ -> do
  case k of
    Prim p ->
      case primMultiResult p of
        SingleResult -> extractPrimWarnOrFail (primName p) >>= goSingle p
        MultiResult -> extractPrimWarnOrFail (primName p) >>= goMulti p
    _ -> pure []
 where
  -- Routine for multi result primitives. For more info:
  -- 'Clash.Normalize.Transformations.setupMultiResultPrim'.
  goMulti :: PrimInfo -> CompiledPrimitive -> NetlistMonad [(Id, Id)]
  goMulti pInfo (BlackBoxHaskell{function=(_, function)}) = do
    tcm <- Lens.view tcCache
    let mpInfo@MultiPrimInfo{mpi_resultTypes} = multiPrimInfo' tcm pInfo
    let (args1, resIds) = splitMultiPrimArgs mpInfo args
    funRes <- preserveVarEnv (function False (primName pInfo) args1 mpi_resultTypes)
    let BlackBoxMeta{bbResultNames} = either error fst funRes
    go (primName pInfo) resIds args1 bbResultNames
  goMulti _ _ = pure []

  -- Routine for single result primitives (the default kind of primitive)
  goSingle :: PrimInfo -> CompiledPrimitive -> NetlistMonad [(Id, Id)]
  goSingle pInfo (BlackBoxHaskell{function=(_, function)}) = do
    funRes <- preserveVarEnv (function False (primName pInfo) args [coreTypeOf i])
    case either error fst funRes of
      BlackBoxMeta{bbResultNames=[bbResultName]} ->
        go (primName pInfo) [i] args [bbResultName]
      _ -> pure []
  goSingle pInfo (BlackBox{resultNames=[resultName]}) = do
    go (primName pInfo) [i] args [resultName]
  goSingle _ _ = pure []

  go :: Text -> [Id] -> [Either Term Type] -> [BlackBox] -> NetlistMonad [(Id, Id)]
  go nm is0 bbArgs bbResultTemplates = do
    (bbCtx, _) <- preserveVarEnv (mkBlackBoxContext nm is0 bbArgs)
    be <- Lens.use backend
    let
      _sameName i0 i1 = nameOcc (varName i0) == nameOcc (varName i1)
      newNames = map (evalBlackBox be bbCtx) bbResultTemplates
      modName newRetName = modifyVarName (\n -> n {nameOcc = newRetName})
      is1 = zipWith modName newNames is0

    -- TODO: _sameName check disabled due to
    --       https://github.com/clash-lang/clash-compiler/issues/1566
    -- Don't rename if we didn't change any names, it will cause superfluous
    -- redirections in 'mkUniqueNormalized'.
    -- pure (if and (zipWith sameName is0 is1) then [] else zip is0 is1)
    pure (zip is0 is1)

-- | Render a blackbox given its context. Renders _just_ the blackbox, not any
-- corresponding includes, libraries, and so forth.
evalBlackBox :: HasCallStack => SomeBackend -> BlackBoxContext -> BlackBox -> Text
evalBlackBox (SomeBackend s) bbCtx bb
  | BBFunction _bbName _bbHash (TemplateFunction _usedArgs _verifFunc func) <- bb =
    let layout = LayoutOptions (AvailablePerLine 120 0.4) in
    toStrict (renderLazy (layoutPretty layout (State.evalState (func bbCtx) s)))
  | BBTemplate bbt <- bb =
    toStrict ((State.evalState (renderTemplate bbCtx bbt) s) 0)

mkUniqueArguments
  :: Subst
  -> Maybe (ExpandedTopEntity Identifier)
  -- ^ Top entity annotation where:
  --
  --     * Nothing: term is not a top entity
  --     * Just ..: term is a top entity
  -> [Id]
  -> NetlistMonad
       ( [Bool]                 -- Were voids
       , [(Identifier,HWType)]  -- Arguments and their types
       , [Declaration]          -- Extra declarations
       , Subst                  -- Substitution with new vars in scope
       )
mkUniqueArguments subst0 Nothing args = do
  (args', subst1) <- mkUnique subst0 args
  ports <- mapM idToInPort args'
  return (map isNothing ports, catMaybes ports, [], subst1)

mkUniqueArguments subst0 (Just (ExpandedTopEntity{..})) args = do
  (ports, decls, subst1) <- (unzip3 . catMaybes) <$> zipWithM go et_inputs args
  return ( map isNothing et_inputs
         , concat ports
         , concat decls
         , extendInScopeIdList (extendIdSubstList subst0 (map snd subst1))
                               (map fst subst1))
  where
    go Nothing _var =
      pure Nothing
    go (Just port) var = do
      (ports, decls, _, portI) <- mkTopInput port
      let portName = Id.toText portI
          pId  = mkLocalId (coreTypeOf var) (setRepName portName (varName var))
      return (Just (ports, decls, (pId, (var, Var pId))))


mkUniqueResult
  :: Subst
  -> Maybe (ExpandedTopEntity Identifier)
  -- ^ Top entity annotation where:
  --
  --     * Nothing: term is not a top entity
  --     * Just ..: term is a top entity
  -> Id
  -> NetlistMonad (Maybe ([(Identifier,HWType)],[Declaration],Id,Subst))
mkUniqueResult subst0 Nothing res = do
  ([res'],subst1) <- mkUnique subst0 [res]
  portM <- idToOutPort res'
  case portM of
    Just port -> return (Just ([port],[],res',subst1))
    _         -> return Nothing

mkUniqueResult _subst0 (Just (ExpandedTopEntity{et_output=Nothing})) _res =
  pure Nothing
mkUniqueResult subst0 (Just (ExpandedTopEntity{et_output=Just iPort})) res = do
  (_, sp) <- Lens.use curCompNm
  (FilteredHWType hwty _) <- unsafeCoreTypeToHWTypeM $(curLoc) (coreTypeOf res)
  when (containsBiSignalIn hwty)
    (throw (ClashException sp ($(curLoc) ++ "BiSignalIn cannot be part of a function's result. Use 'readFromBiSignal'.") Nothing))
  (ports, decls, portI) <- mkTopOutput iPort
  let pO = setRepName (Id.toText portI) (varName res)
      pOId = mkLocalId (coreTypeOf res) pO
      subst1 = extendInScopeId (extendIdSubst subst0 res (Var pOId)) pOId
  return (Just (ports, decls, pOId, subst1))

-- | Same as idToPort, but
--    * Throws an error if the port is a composite type with a BiSignalIn
idToInPort :: Id -> NetlistMonad (Maybe (Identifier, HWType))
idToInPort var = do
  (_, sp) <- Lens.use curCompNm
  portM <- idToPort var
  case portM of
    Just (_,hty) -> do
      when (containsBiSignalIn hty && not (isBiSignalIn hty))
        (throw (ClashException sp ($(curLoc) ++ "BiSignalIn currently cannot be part of a composite type when it's a function's argument") Nothing))
      return portM
    _ -> return Nothing

-- | Same as idToPort, but:
--    * Throws an error if port is of type BiSignalIn
idToOutPort :: Id -> NetlistMonad (Maybe (Identifier,HWType))
idToOutPort var = do
  (_, srcspan) <- Lens.use curCompNm
  portM <- idToPort var
  case portM of
    Just (_,hty) -> do
      when (containsBiSignalIn hty)
        (throw (ClashException srcspan ($(curLoc) ++ "BiSignalIn cannot be part of a function's result. Use 'readFromBiSignal'.") Nothing))
      return portM
    _ -> return Nothing

idToPort :: Id -> NetlistMonad (Maybe (Identifier, HWType))
idToPort var = do
  hwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) (coreTypeOf var)
  if isVoid hwTy
    then return Nothing
    else return (Just (Id.unsafeFromCoreId var, hwTy))

setRepName :: Text -> Name a -> Name a
setRepName s (Name sort' _ i loc) = Name sort' s i loc

-- | Make a set of IDs unique; also returns a substitution from old ID to new
-- updated unique ID.
mkUnique
  :: Subst
  -- ^ Existing substitution
  -> [Id]
  -- ^ IDs to make unique
  -> NetlistMonad ([Id],Subst)
  -- ^ (Unique IDs, update substitution)
mkUnique = go []
  where
    go :: [Id] -> Subst -> [Id] -> NetlistMonad ([Id],Subst)
    go processed subst []     = return (reverse processed,subst)
    go processed subst@(Subst isN _ _ _) (i:is) = do
      iN <- Id.toText <$> Id.fromCoreId i
      let i' = uniqAway isN (modifyVarName (setRepName iN) i)
          subst' = extendInScopeId (extendIdSubst subst i (Var i')) i'
      go (i':processed)
         subst'
         is

-- | Preserve the complete state before running an action, and restore it
-- afterwards.
preserveState
  :: NetlistMonad a
  -> NetlistMonad a
preserveState action = do
  state <- State.get
  val <- action
  State.put state
  pure val

-- | Preserve the Netlist '_curCompNm','_seenIds','_usageMap' when executing
-- a monadic action
preserveVarEnv
  :: NetlistMonad a
  -> NetlistMonad a
preserveVarEnv action = do
  -- store state
  vComp <- Lens.use curCompNm
  vSeen <- Lens.use seenIds
  vUses <- Lens.use usageMap
  -- perform action
  val <- action
  -- restore state
  curCompNm .= vComp
  seenIds   .= vSeen
  usageMap  .= vUses
  return val

dcToLiteral :: HWType -> Int -> Literal
dcToLiteral Bool 1 = BoolLit False
dcToLiteral Bool 2 = BoolLit True
dcToLiteral _ i    = NumLit (toInteger i-1)

-- * TopEntity Annotations

extendPorts :: [PortName] -> [Maybe PortName]
extendPorts ps = map Just ps ++ repeat Nothing

-- | Prefix given string before portnames /except/ when this string is empty.
prefixParent :: String -> PortName -> PortName
prefixParent ""     p                   = p
prefixParent parent (PortName p)        = PortName (parent <> "_" <> p)
prefixParent parent (PortProduct "" ps) = PortProduct parent ps
prefixParent parent (PortProduct p ps)  = PortProduct (parent <> "_" <> p) ps

-- | Make a new signal which is assigned with an initial value. This should be
-- used in place of NetDecl directly, as it also updates the usage map to
-- include the new identifier and usage.
--
mkInit
  :: HasCallStack
  => DeclarationType
  -- ^ Are we in a concurrent or sequential context?
  -> Usage
  -- ^ How is the initial value assigned if the assignment is separate
  -> Identifier
  -- ^ The identifier of the declared net
  -> HWType
  -- ^ The typr of the declared net
  -> Expr
  -- ^ The value assigned to the net
  -> NetlistMonad [Declaration]
  -- ^ The declarations needed to declare and assign the net
mkInit _ _ i ty e
  -- When the initial value is a constant, we can set the value at the same
  -- point the declaration happens. Initial assignments of this form do not
  -- count as a usage as they are always allowed.
  | isConstExpr e
  = pure [NetDecl' Nothing i ty (Just e)]

mkInit Concurrent Cont i ty e = do
  usageMap %= Map.insert (Id.toText i) Cont
  pure [NetDecl' Nothing i ty Nothing, Assignment i Cont e]

mkInit Concurrent proc i ty e = do
  usageMap %= Map.insert (Id.toText i) proc
  pure
    [ NetDecl' Nothing i ty Nothing
    , Seq [Initial [SeqDecl (Assignment i proc e)]]
    ]

mkInit Sequential Cont _ _ _ =
  error "mkInit: Cannot continuously assign in a sequential block"

mkInit Sequential proc i ty e = do
  usageMap %= Map.insert (Id.toText i) proc
  pure
    [ NetDecl' Nothing i ty Nothing
    , Seq [SeqDecl (Assignment i proc e)]
    ]

-- | Determine if for the specified HDL, the type of assignment wanted can be
-- performed on a signal which has been assigned another way. This identifies
-- when a new intermediary signal needs to be created, e.g.
--
--   * when attempting to use blocking and non-blocking procedural assignment
--     on the same signal in VHDL
--
--   * when attempting to use continuous and procedural assignment on the same
--     signal in (System)Verilog
--
canUse :: HDL -> Usage -> Usage -> Bool
canUse VHDL (Proc Blocking) = \case
  Proc Blocking -> True
  _ -> False

canUse VHDL _ = \case
  Proc Blocking -> False
  _ -> True

canUse _ Cont = \case
  Cont -> True
  _ -> False

canUse _ _ = \case
  Cont -> False
  _ -> True

declareUse :: Usage -> Identifier -> NetlistMonad ()
declareUse u i = usageMap %= Map.insertWith (<>) (Id.toText i) u

-- | Like 'declareUse', but will throw an exception if we run into a name
-- collision.
declareUseOnce :: HasUsageMap s => Usage -> Identifier -> State.State s ()
declareUseOnce u i = usageMap %= Map.alter go (Id.toText i)
 where
  go Nothing = Just u
  go Just{}  = error ("Internal error: unexpected re-declaration of usage for" ++ show i)

-- | Declare uses which occur as a result of a component being instantiated,
-- for example the following design (verilog)
--
-- > module f ( input p; output reg r ) ... endmodule
-- >
-- > module top ( ... )
-- >   ...
-- >   f f_inst ( .p(p), .r(foo));
-- >   ...
-- > endmodule
--
-- would declare a usage of foo, since it is assigned by @f_inst@.
--
declareInstUses
  :: [(Expr, PortDirection, HWType, Expr)]
  -- ^ The port mappings (named)
  -> NetlistMonad ()
declareInstUses =
  mapM_ declare
 where
  -- Modifiers don't matter for these identifiers
  declare (Identifier _ _, Out, _, Identifier n _) =
    -- See NOTE [output ports are continuously assigned]
    declareUse Cont n

  declare (_, In, _, _) =
    pure ()

  declare portMapping =
    error ("declareInstUses: Unexpected port mapping: " <> show portMapping)

{-
NOTE [output ports are continuously assigned]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a module is instantiated in Verilog, the output of the module is always
continuously assigned. This means if I have the module

  module f ( ..., output reg result)

and an instantiation

  f f_inst ( ..., .result(foo) );

then the assignment of result to foo is a continuous assignment. This may seem
strange, but consider

  * instantiations can only occur in a concurrent context, and concurrent
    contexts only admit continuous assignment

  * the usage being tracked is not for the result port, but for foo. Whenever
    the value of the port changes, the value of foo will change (i.e. there is
    a wire between them, it cannot "hold" state).

This means when we declare uses that occur as a result of an instantiation in
netlist, the uses are always continuous assignment, and not the usage of the
output port as given in the `outputs` field of the `Component` type.
-}

assignmentWith
  :: HasCallStack
  => (Identifier -> Declaration)
  -> Usage
  -> Identifier
  -> NetlistMonad Declaration
assignmentWith assign new i = do
  u <- Lens.use usageMap
  SomeBackend b <- Lens.use backend

  case lookupUsage i u of
    Just old | not $ canUse (hdlKind b) new old ->
      error $ mconcat
        [ "assignmentWith: Cannot assign as "
        , show new
        , " after "
        , show old
        , " for "
        , show i
        ]

    _ ->
      declareUse new i $> assign i

-- | Attempt to continuously assign an expression to the given identifier. If
-- the assignment is not allowed for the backend being used, a new signal is
-- created which allows the assignment. The identifier which holds the result
-- of the assignment is returned alongside the new declarations.
--
-- This function assumes the identifier being assigned is already declared. If
-- the identifier is not in the usage map then an error is thrown.
--
contAssign
  :: HasCallStack
  => Identifier
  -> Expr
  -> NetlistMonad Declaration
contAssign dst expr =
  assignmentWith (\i -> Assignment i Cont expr) Cont dst

procAssign
  :: HasCallStack
  => Blocking
  -> Identifier
  -> Expr
  -> NetlistMonad Declaration
procAssign block dst expr =
  assignmentWith (\i -> Assignment i (Proc block) expr) (Proc block) dst

condAssign
  :: Identifier
  -> HWType
  -> Expr
  -> HWType
  -> [(Maybe Literal, Expr)]
  -> NetlistMonad Declaration
condAssign dst dstTy scrut scrutTy alts = do
  -- Currently, all CondAssignment get rendered as `always @*` blocks in
  -- (System)Verilog. This means when we target these HDL, this is _really_ a
  -- blocking procedural assignment.
  SomeBackend b <- Lens.use backend
  let use = case hdlKind b of { VHDL -> Cont ; _ -> Proc Blocking }
  assignmentWith (\i -> CondAssignment i dstTy scrut scrutTy alts) use dst

-- | See 'toPrimitiveType' / 'fromPrimitiveType'
convPrimitiveType :: HWType -> a -> NetlistMonad a -> NetlistMonad a
convPrimitiveType hwty a action = do
  b <- Lens.use backend
  let kind = case b of {SomeBackend s -> State.evalState (hdlHWTypeKind hwty) s}
  case kind of
    UserType -> action
    SynonymType -> pure a
    PrimitiveType -> pure a

-- | Top entities only expose primitive types or types that don't need explicit
-- conversion to a primitive type (i.e., no types from the '_types' module). This
-- function converts from a custom type to a primitive type if needed.
--
-- See 'HWKind' for more info on primitive type kinds.
toPrimitiveType
  :: Identifier
  -> HWType
  -> NetlistMonad ([Declaration], Identifier, Expr, HWType)
toPrimitiveType id0 hwty0 = convPrimitiveType hwty0 dflt $ do
  id1 <- Id.next id0
  ds  <- mkInit Concurrent Cont id1 hwty1 expr
  pure (ds, id1, expr, hwty1)
 where
  dflt = ([], id0, Identifier id0 Nothing, hwty0)
  hwty1 = BitVector (typeSize hwty0)
  expr = ToBv Nothing hwty0 (Identifier id0 Nothing)

-- | Top entities only expose primitive types or types that don't need explicit
-- conversion to a primitive type (i.e., no types from the '_types' module). This
-- function converts from a primitive type to a custom type if needed.
--
-- See 'HWKind' for more info on primitive type kinds.
fromPrimitiveType
  :: Identifier
  -> HWType
  -> NetlistMonad ([Declaration], Identifier, Expr, HWType)
fromPrimitiveType id0 hwty0 = convPrimitiveType hwty0 dflt $ do
  id1 <- Id.next id0
  ds <- mkInit Concurrent Cont id1 hwty0 expr
  pure (ds, id1, expr, hwty1)
 where
  dflt = ([], id0, Identifier id0 Nothing, hwty0)
  hwty1 = BitVector (typeSize hwty0)
  expr = FromBv Nothing hwty0 (Identifier id0 Nothing)

-- | Create port names for the declaration of a top entity. For /instantiation/
-- see 'mkTopInstInput'.
mkTopInput
  :: ExpandedPortName Identifier
  -- ^ Port name description
  -> NetlistMonad ([(Identifier, HWType)], [Declaration], Expr, Identifier)
  -- ^ (port names, signal decls for intermediate signals, argument expr, argument id)
mkTopInput (ExpandedPortName hwty0 i0) = do
  (decls, i1, expr, hwty1) <- fromPrimitiveType i0 hwty0
  return ([(i0, hwty1)], decls, expr, i1)

mkTopInput epp@(ExpandedPortProduct p hwty ps) = do
  pN <- Id.makeBasic p
  case hwty of
    Vector sz eHwty -> do
      (ports, _, exprs, _) <- unzip4 <$> mapM mkTopInput ps
      let vecExpr  = mkVectorChain sz eHwty exprs
      decls <- mkInit Concurrent Cont pN hwty vecExpr
      return (concat ports, decls, vecExpr, pN)

    RTree d eHwty -> do
      (ports, _, exprs, _) <- unzip4 <$> mapM mkTopInput ps
      let trExpr   = mkRTreeChain d eHwty exprs
      decls <- mkInit Concurrent Cont pN hwty trExpr
      return (concat ports, decls, trExpr, pN)

    Product _ _ _ -> do
      (ports, _, exprs, _) <- unzip4 <$> mapM mkTopInput ps
      case exprs of
        [expr] -> do
          decls <- mkInit Concurrent Cont pN hwty expr
          return (concat ports, decls, expr, pN)
        _ -> do
          let dcExpr = DataCon hwty (DC (hwty, 0)) exprs
          decls <- mkInit Concurrent Cont pN hwty dcExpr
          return (concat ports, decls, dcExpr, pN)

    SP _ ((concat . map snd) -> [elTy]) -> do
      (ports, _, exprs, _) <- unzip4 <$> mapM mkTopInput ps
      case exprs of
        [conExpr, elExpr] -> do
          let dcExpr   = DataCon hwty (DC (BitVector (typeSize hwty), 0))
                          [conExpr, ToBv Nothing elTy elExpr]
          decls <- mkInit Concurrent Cont pN hwty dcExpr
          return (concat ports, decls, dcExpr, pN)
        _ -> error $ $(curLoc) ++ "Internal error"

    _ ->
      -- 'expandTopEntity' should have made sure this isn't possible
      error $ $(curLoc) ++ "Internal error: " ++ show epp

portProductError :: String -> HWType -> ExpandedPortName Identifier -> a
portProductError loc hwty portProduct = error $ loc ++ [I.i|
  #{loc}PortProduct used, but did not see Vector, RTree, or Product. Saw the
  following instead:

    #{hwty}

  PortProduct used:

    #{portProduct}

  Note that the PortProduct as shown above might is only indicative, and might
  not correspond exactly to the one given in the Clash design. |]

-- | Create a Vector chain for a list of 'Identifier's
mkVectorChain :: Int
              -> HWType
              -> [Expr]
              -> Expr
mkVectorChain _ elTy []      = DataCon (Vector 0 elTy) VecAppend []
mkVectorChain _ elTy [e]     = DataCon (Vector 1 elTy) VecAppend
                                [e]
mkVectorChain sz elTy (e:es) = DataCon (Vector sz elTy) VecAppend
                                [ e
                                , mkVectorChain (sz-1) elTy es
                                ]

-- | Create a RTree chain for a list of 'Identifier's
mkRTreeChain :: Int
             -> HWType
             -> [Expr]
             -> Expr
mkRTreeChain _ elTy [e] = DataCon (RTree 0 elTy) RTreeAppend
                                  [e]
mkRTreeChain d elTy es =
  let (esL,esR) = splitAt (length es `div` 2) es
  in  DataCon (RTree d elTy) RTreeAppend
        [ mkRTreeChain (d-1) elTy esL
        , mkRTreeChain (d-1) elTy esR
        ]

genComponentName
  :: Bool
  -- ^ New inline strategy enabled
  -> Maybe Text
  -- ^ Component name prefix
  -> Id
  -- ^ Create component name based on this Core Id
  -> Text
genComponentName newInlineStrat prefixM nm =
  Text.intercalate "_" (prefix ++ [fn1])
 where
  nm0 = Text.splitOn "." (nameOcc (varName nm))
  fn0 = Id.stripDollarPrefixes (last nm0)
  fn1 = if Text.null fn0 then "Component" else fn0
  prefix = fromMaybe (if newInlineStrat then [] else init nm0) (pure <$> prefixM)

genTopName
  :: IdentifierSetMonad m
  => Maybe Text
  -- ^ Top entity name prefix
  -> TopEntity
  -- ^ Top entity annotation
  -> m Identifier
  -- ^ New identifier
genTopName prefixM ann =
  case prefixM of
    Just prefix | not (Text.null prefix) ->
      Id.addRaw (Text.concat [prefix, "_", Text.pack (t_name ann)])
    _ ->
      Id.addRaw (Text.pack (t_name ann))

-- | Strips one or more layers of attributes from a HWType; stops at first
-- non-Annotated. Accumulates all attributes of nested annotations.
stripAttributes
  :: HWType
  -> ([Attr Text], HWType)
-- Recursively strip type, accumulate attrs:
stripAttributes (Annotated attrs typ) =
  let (attrs', typ') = stripAttributes typ
  in (attrs ++ attrs', typ')
-- Not an annotated type, so just return it:
stripAttributes typ = ([], typ)

-- | Create output port names for the declaration of a top entity. For
-- /instantiation/ see 'mkTopInstOutput'.
mkTopOutput
  :: ExpandedPortName Identifier
  -> NetlistMonad ([(Identifier, HWType)], [Declaration], Identifier)
mkTopOutput (ExpandedPortName hwty0 i0) = do
  i1 <- Id.next i0
  (_, _, bvExpr, hwty1) <- toPrimitiveType i1 hwty0
  if hwty0 == hwty1 then
    -- No type conversion happened, so we can just request caller to assign to
    -- port name directly.
    return ([(i0, hwty0)], [], i0)
  else do
    -- Type conversion happened, so we must use intermediate variable.
    assn <- contAssign i0 bvExpr
    pure ( [(i0, hwty1)], [NetDecl' Nothing i1 hwty0 Nothing, assn], i1)

mkTopOutput epp@(ExpandedPortProduct p hwty ps) = do
  pN <- Id.makeBasic p
  let netdecl = NetDecl' Nothing pN hwty Nothing
  case hwty of
    Vector {} -> do
      (ports, decls, ids) <- unzip3 <$> mapM mkTopOutput ps
      assigns <- zipWithM (\i n -> assignId pN hwty 10 i n) ids [0..]
      return (concat ports, netdecl : assigns ++ concat decls, pN)

    RTree {} -> do
      (ports, decls, ids) <- unzip3 <$> mapM mkTopOutput ps
      assigns <- zipWithM (\i n -> assignId pN hwty 10 i n) ids [0..]
      return (concat ports, netdecl : assigns ++ concat decls, pN)

    Product {} -> do
      (ports, decls, ids) <- unzip3 <$> mapM mkTopOutput ps
      case ids of
        [i] -> do
          assn <- contAssign i (Identifier pN Nothing)
          pure (concat ports, netdecl : assn : concat decls, pN)

        _   -> do
          assigns <- zipWithM (\i n -> assignId pN hwty 0 i n) ids [0..]
          pure (concat ports, netdecl : assigns ++ concat decls, pN)

    SP _ ((concat . map snd) -> [elTy]) -> do
      (ports, decls, ids) <- unzip3 <$> mapM mkTopOutput ps
      case ids of
        [conId, elId] -> do
          let conIx   = Sliced ( BitVector (typeSize hwty)
                               , typeSize hwty - 1
                               , typeSize elTy )
              elIx    = Sliced ( BitVector (typeSize hwty)
                               , typeSize elTy - 1
                               , 0 )

          conAssgn <- contAssign conId (Identifier pN (Just conIx))
          elAssgn <- contAssign elId (FromBv Nothing elTy (Identifier pN (Just elIx)))

          pure (concat ports, netdecl:conAssgn:elAssgn:concat decls, pN)
        _ -> error $ $(curLoc) ++ "Internal error"
    -- 'expandTopEntity' should have made sure this isn't possible
    _ -> error $ $(curLoc) ++ "Internal error: " ++ show epp

 where
  assignId p_ hwty_ con i n =
    contAssign i (Identifier p_ (Just (Indexed (hwty_, con, n))))

mkTopCompDecl
  :: Maybe Text
  -- ^ Library entity is defined in
  -> [Attr Text]
  -- ^ Attributes to add to generate code
  -> Identifier
  -- ^ The component's (or entity's) name
  -> Identifier
  -- ^ Instance label
  -> [(Expr, HWType, Expr)]
  -- ^ List of parameters for this component (param name, param type, param value)
  -> [InstancePort]
  -- ^ Input port assignments
  -> [InstancePort]
  -- ^ Output port assignments
  -> Declaration
mkTopCompDecl lib attrs name instName params inputs outputs =
  InstDecl Entity lib attrs name instName params (IndexedPortMap ports)
 where
  ports = map (toPort In) inputs ++ map (toPort Out) outputs
  toExpr id_ = Identifier id_ Nothing
  toPort dir ip = (dir, (ip_type ip), toExpr (ip_id ip))

-- | Instantiate a TopEntity, and add the proper type-conversions where needed
mkTopUnWrapper
  :: Id
  -- ^ Name of the TopEntity component
  -> ExpandedTopEntity Identifier
  -- ^ A corresponding @TopEntity@ annotation
  -> (Identifier, HWType)
  -- ^ The name and type of the signal to which to assign the result
  -> [(Expr,HWType)]
  -- ^ The arguments with voids filtered.
  -> [Declaration]
  -- ^ Tick declarations
  -> NetlistMonad [Declaration]
mkTopUnWrapper topEntity annM dstId args tickDecls = do
  -- component name
  compNameM <- lookupVarEnv topEntity <$> Lens.use componentNames
  let
    topName = Id.toText topIdentifier
    topIdentifier = flip fromMaybe compNameM (error [I.i|
     Internal error in 'mkTopUnWrapper': tried to lookup (netlist) name
     of #{showPpr (varName topEntity)}, but couldn't find it in NetlistState's
     'componentNames'. This should have been put there by 'runNetlistMonad' /
     'genNames'. |])

  -- inputs
  (iports, wrappers, idsI) <- unzip3 <$> mapM mkTopInstInput (catMaybes (et_inputs annM))
  inpAssigns <- zipWithM (\i e -> contAssign i e) idsI (fst <$> args)

  -- output
  let
    iResult = inpAssigns : wrappers
    instLabel0 = Text.concat [topName, "_", Id.toText (fst dstId)]

  instLabel1 <- fromMaybe instLabel0 <$> Lens.view setName
  instLabel2 <- affixName instLabel1
  instLabel3 <- Id.makeBasic instLabel2
  topOutputM <- traverse mkTopInstOutput (et_output annM)

  let topDecl = mkTopCompDecl (Just topName) [] topIdentifier instLabel3 [] (concat iports)

  case topOutputM of
    Nothing ->
      pure (topDecl [] : concat iResult)
    Just (oports, unwrappers, id0) -> do
      outpAssign <- contAssign (fst dstId) (Identifier id0 Nothing)
      pure (concat iResult ++ tickDecls ++ (topDecl oports:unwrappers) ++ [outpAssign])

data InstancePort = InstancePort
  { ip_id :: Identifier
  -- ^ Identifier to assign. Top entities are instantiated using positional
  -- arguments, so this doesn't hold a port name.
  , ip_type :: HWType
  -- ^ Type assigned to port
  } deriving Show

-- | Generate input port(s) associated with a single argument for an
-- instantiation of a top entity. This function composes the input ports into
-- a single signal and returns its name.
mkTopInstInput
  :: ExpandedPortName Identifier
  -- ^ The @PortName@ of a _TopEntity_ annotation for this input.
  -> NetlistMonad ([InstancePort], [Declaration], Identifier)
  -- ^ (ports to assign, declarations for intermediate signals, argument signal)
mkTopInstInput (ExpandedPortName hwty0 pN) = do
  pN' <- Id.next pN
  (decls, pN'', _bvExpr, hwty1) <- toPrimitiveType pN' hwty0
  return ( [InstancePort pN'' hwty1]
          , NetDecl' Nothing pN' hwty0 Nothing : decls
          , pN' )

mkTopInstInput epp@(ExpandedPortProduct pNameHint hwty0 ps) = do
  pName <- Id.makeBasic pNameHint
  let pDecl = NetDecl' Nothing pName hwty0 Nothing

  let
    (attrs, hwty1) = stripAttributes hwty0
    indexPN constr n = Identifier pName (Just (Indexed (hwty0, constr, n)))

  case hwty1 of
    Vector {} -> do
      (ports, decls, ids) <- unzip3 <$> mapM mkTopInstInput ps
      let assigns = zipWith3 Assignment ids (repeat Cont) (map (indexPN 10) [0..])
      if null attrs then
        return (concat ports, pDecl:assigns ++ concat decls, pName)
      else
        throwAnnotatedSplitError $(curLoc) "Vector"

    RTree {} -> do
      (ports, decls, ids) <- unzip3 <$> mapM mkTopInstInput ps
      let assigns = zipWith3 Assignment ids (repeat Cont) (map (indexPN 10) [0..])
      if null attrs then
        return (concat ports, pDecl:assigns ++ concat decls, pName)
      else
        throwAnnotatedSplitError $(curLoc) "RTree"

    Product {} -> do
      (ports, decls, ids) <- unzip3 <$> mapM mkTopInstInput ps
      let assigns = zipWith3 Assignment ids (repeat Cont) (map (indexPN 0) [0..])
      if null attrs then
        return (concat ports, pDecl:assigns ++ concat decls, pName)
      else
        throwAnnotatedSplitError $(curLoc) "Product"

    SP _ ((concat . map snd) -> [elTy]) -> do
      (ports, decls, ids) <- unzip3 <$> mapM mkTopInstInput ps
      case ids of
        [conId,elId] -> do
          let
            conIx = Sliced
              ( BitVector (typeSize hwty1)
              , typeSize hwty1 - 1
              , typeSize elTy )

            elIx = Sliced
              ( BitVector (typeSize hwty1)
              , typeSize elTy - 1
              , 0 )

            assigns =
              [ Assignment conId Cont (Identifier pName (Just conIx))
              , Assignment elId  Cont (FromBv Nothing elTy (Identifier pName (Just elIx))) ]

          return (concat ports, pDecl:assigns ++ concat decls, pName)
        _ -> error "Internal error: Unexpected error for PortProduct"
    _ ->
      portProductError $(curLoc) hwty0 epp


-- | Consider the following type signature:
--
-- @
--   f :: Signal dom (Vec 6 A) \`Annotate\` Attr "keep"
--     -> Signal dom (Vec 6 B)
-- @
--
-- What does the annotation mean, considering that Clash will split these
-- vectors into multiple in- and output ports? Should we apply the annotation
-- to all individual ports? How would we handle pin mappings? For now, we simply
-- throw an error. This is a helper function to do so.
throwAnnotatedSplitError
  :: String
  -> String
  -> NetlistMonad a
throwAnnotatedSplitError loc typ = do
  (_,sp) <- Lens.use curCompNm
  throw $ ClashException sp (loc ++ printf msg typ typ) Nothing
 where
  msg = unwords $ [ "Attempted to split %s into a number of HDL ports. This"
                  , "is not allowed in combination with attribute annotations."
                  , "You can annotate %s's components by splitting it up"
                  , "manually." ]

-- | Generate output port(s) for an instantiation of a top entity. This function
-- combines all output ports into a signal identifier and returns its name.
mkTopInstOutput
  :: HasCallStack
  => ExpandedPortName Identifier
  -- ^ The @PortName@ of a _TopEntity_ annotation for this output
  -> NetlistMonad ([InstancePort], [Declaration], Identifier)
  -- ^ (ports to assign, declarations for intermediate signals, result signal)
mkTopInstOutput (ExpandedPortName hwty0 portName) = do
  assignName0 <- Id.next portName
  (decls, assignName1, _expr, hwty1) <- fromPrimitiveType assignName0 hwty0
  let net = NetDecl' Nothing assignName0 hwty1 Nothing
  return ([InstancePort assignName0 hwty1], net : decls, assignName1)

mkTopInstOutput epp@(ExpandedPortProduct productNameHint hwty ps) = do
  pName <- Id.makeBasic productNameHint
  let pDecl = NetDecl' Nothing pName hwty Nothing
  let (attrs, hwty') = stripAttributes hwty
  case hwty' of
    Vector sz hwty'' -> do
      (ports, decls, ids0) <- unzip3 <$> mapM mkTopInstOutput ps
      let ids1 = map (flip Identifier Nothing) ids0
          netassgn = Assignment pName Cont (mkVectorChain sz hwty'' ids1)
      if null attrs then
        return (concat ports, pDecl:netassgn:concat decls, pName)
      else
        throwAnnotatedSplitError $(curLoc) "Vector"

    RTree d hwty'' -> do
      (ports, decls, ids0) <- unzip3 <$> mapM mkTopInstOutput ps
      let ids1 = map (flip Identifier Nothing) ids0
          netassgn = Assignment pName Cont (mkRTreeChain d hwty'' ids1)
      if null attrs then
        return (concat ports, pDecl:netassgn:concat decls, pName)
      else
        throwAnnotatedSplitError $(curLoc) "RTree"

    Product {} -> do
      (ports, decls, ids0) <- unzip3 <$> mapM mkTopInstOutput ps
      let ids1 = map (flip Identifier Nothing) ids0
          netassgn = Assignment pName Cont (DataCon hwty (DC (hwty,0)) ids1)
      if null attrs then
        return (concat ports, pDecl:netassgn:concat decls, pName)
      else
        throwAnnotatedSplitError $(curLoc) "Product"

    SP _ ((concat . map snd) -> [elTy]) -> do
      (ports, decls, ids0) <- unzip3 <$> mapM mkTopInstOutput ps
      let ids1 = map (flip Identifier Nothing) ids0
          ids2 = case ids1 of
                  [conId, elId] -> [conId, ToBv Nothing elTy elId]
                  _ -> error "Unexpected error for PortProduct"
          netassgn = Assignment pName Cont (DataCon hwty (DC (BitVector (typeSize hwty),0)) ids2)
      return (concat ports, pDecl:netassgn:concat decls, pName)

    _ ->
      portProductError $(curLoc) hwty' epp

-- | Try to merge nested modifiers into a single modifier, needed by the VHDL
-- and SystemVerilog backend.
nestM :: Modifier -> Modifier -> Maybe Modifier
nestM (Nested a b) m2
  | Just m1  <- nestM a b  = maybe (Just (Nested m1 m2)) Just (nestM m1 m2)
  | Just m2' <- nestM b m2 = maybe (Just (Nested a m2')) Just (nestM a m2')

nestM (Indexed (Vector n t1,1,1)) (Indexed (Vector _ t2,1,0))
  | t1 == t2 = Just (Indexed (Vector n t1,10,1))

nestM (Indexed (Vector n t1,1,1)) (Indexed (Vector _ t2,10,k))
  | t1 == t2 = Just (Indexed (Vector n t1,10,k+1))

nestM (Indexed (RTree d1 t1,1,n)) (Indexed (RTree d2 t2,0,0))
  | t1 == t2
  , d1 >= 0
  , d2 >= 0
  = Just (Indexed (RTree d1 t1,10,n))

nestM (Indexed (RTree d1 t1,1,n)) (Indexed (RTree d2 t2,1,m))
  | t1 == t2
  , d1 >= 0
  , d2 >= 0
  = if | n == 1 && m == 1 -> let r = 2 ^ d1
                                 l = r - (2 ^ (d1-1) `div` 2)
                             in  Just (Indexed (RTree (-1) t1, l, r))
       | n == 1 && m == 0 -> let l = 2 ^ (d1-1)
                                 r = l + (l `div` 2)
                             in  Just (Indexed (RTree (-1) t1, l, r))
       | n == 0 && m == 1 -> let l = (2 ^ (d1-1)) `div` 2
                                 r = 2 ^ (d1-1)
                             in  Just (Indexed (RTree (-1) t1, l, r))
       | n == 0 && m == 0 -> let l = 0
                                 r = (2 ^ (d1-1)) `div` 2
                             in  Just (Indexed (RTree (-1) t1, l, r))
       | n > 1 || n < 0   -> error $ "nestM: n should be 0 or 1, not:" ++ show n
       | m > 1 || m < 0   -> error $ "nestM: m should be 0 or 1, not:" ++ show m
       | otherwise        -> error $ "nestM: unexpected (n, m): " ++ show (n, m)
nestM (Indexed (RTree (-1) t1,l,_)) (Indexed (RTree d t2,10,k))
  | t1 == t2
  , d  >= 0
  = Just (Indexed (RTree d t1,10,l+k))

nestM _ _ = Nothing


-- | Determines if any type variables (exts) are bound in any of the given
-- type or term variables (tms). It's currently only used to detect bound
-- existentials, hence the name.
bindsExistentials
  :: [TyVar]
  -> [Var a]
  -> Bool
bindsExistentials exts tms = any (`elem` freeVars) exts
 where
  freeVars = concatMap (Lens.toListOf typeFreeVars) (map coreTypeOf tms)

iteAlts :: HWType -> [Alt] -> Maybe (Term,Term)
iteAlts sHTy [(pat0,alt0),(pat1,alt1)] | validIteSTy sHTy = case pat0 of
  DataPat dc _ _ -> case dcTag dc of
    2 -> Just (alt0,alt1)
    _ -> Just (alt1,alt0)
  LitPat (C.IntegerLiteral l) -> case l of
    1 -> Just (alt0,alt1)
    _ -> Just (alt1,alt0)
  DefaultPat -> case pat1 of
    DataPat dc _ _ -> case dcTag dc of
      2 -> Just (alt1,alt0)
      _ -> Just (alt0,alt1)
    LitPat (C.IntegerLiteral l) -> case l of
      1 -> Just (alt1,alt0)
      _ -> Just (alt0,alt1)
    _ -> Nothing
  _ -> Nothing
 where
  validIteSTy Bool          = True
  validIteSTy Bit           = True
  validIteSTy (Sum _ [_,_]) = True
  validIteSTy (SP _ [_,_])  = True
  validIteSTy (Unsigned 1)  = True
  validIteSTy (Index 2)     = True
  validIteSTy _             = False

iteAlts _ _ = Nothing

-- | Run a NetlistMonad computation in the context of the given source ticks and
-- name modifier ticks
withTicks
  :: [TickInfo]
  -> ([Declaration] -> NetlistMonad a)
  -- ^ The source ticks are turned into 'TickDecl's and are passed as an argument
  -- to the NetlistMonad computation. Name modifier ticks will change the local
  -- environment for the NetlistMonad computation.
  -> NetlistMonad a
withTicks ticks0 k = do
  let ticks1 = List.nub ticks0
  go [] (reverse ticks1)
 where
  go decls [] = k (reverse decls)

  go decls (DeDup:ticks) = go decls ticks

  go decls (NoDeDup:ticks) = go decls ticks

  go decls (SrcSpan sp:ticks) =
    go (TickDecl (Comment (Text.pack (showSDocUnsafe (ppr sp)))):decls) ticks

  go decls (NameMod m nm0:ticks) = do
    tcm <- Lens.view tcCache
    case runExcept (tyLitShow tcm nm0) of
      Right nm1 -> local (modName m nm1) (go decls ticks)
      _ -> go decls ticks

  modName PrefixName (Text.pack -> s2) env@(NetlistEnv {_prefixName = s1})
    | Text.null s1 = env {_prefixName = s2}
    | otherwise    = env {_prefixName = s1 <> "_" <> s2}
  modName SuffixName (Text.pack -> s2) env@(NetlistEnv {_suffixName = s1})
    | Text.null s1 = env {_suffixName = s2}
    | otherwise    = env {_suffixName = s2 <> "_" <> s1}
  modName SuffixNameP (Text.pack -> s2) env@(NetlistEnv {_suffixName = s1})
    | Text.null s1 = env {_suffixName = s2}
    | otherwise    = env {_suffixName = s1 <> "_" <> s2}
  modName SetName (Text.pack -> s) env = env {_setName = Just s}

-- | Add the pre- and suffix names in the current environment to the given
-- identifier
affixName
  :: Text
  -> NetlistMonad Text
affixName nm0 = do
  NetlistEnv _ pre suf _ <- ask
  let nm1 = if Text.null pre then nm0 else pre <> "_" <> nm0
      nm2 = if Text.null suf then nm1 else nm1 <> "_" <> suf
  return nm2

-- | Errors 'expandTopEntity' might yield
data ExpandError
  -- | Synthesis attributes are not supported on PortProducts
  = AttrError [Attr Text]
  -- | Something was annotated as being a PortProduct, but wasn't one
  | PortProductError PortName HWType

-- | Same as 'expandTopEntity', but also adds identifiers to the identifier
-- set of the monad.
expandTopEntityOrErrM
  :: HasCallStack
  => [(Maybe Id, FilteredHWType)]
  -- ^ Arguments. Ids are used as name hints.
  -> (Maybe Id, FilteredHWType)
  -- ^ Result. Id is used as name hint.
  -> Maybe TopEntity
  -- ^ If /Nothing/, an expanded top entity will be generated as if /defSyn/
  -- was passed.
  -> NetlistMonad (ExpandedTopEntity Identifier)
  -- ^ Either some error (see "ExpandError") or and expanded top entity. All
  -- identifiers in the expanded top entity will be added to NetlistState's
  -- IdentifierSet.
expandTopEntityOrErrM ihwtys ohwty topM = do
  is <- identifierSetM id

  case expandTopEntity ihwtys ohwty topM of
    Left (AttrError attrs) ->
      (error [I.i|
        Cannot use attribute annotations on product types of top entities. Saw
        annotation:

          #{attrs}
      |])
    Left (PortProductError pn hwty) ->
      (error [I.i|
        Saw a PortProduct in a Synthesize annotation:

          #{pn}

        but the port type:

          #{hwty}

        is not a product!
      |])
    Right eTop -> do
      let ete = evalState (traverse (either Id.addRaw Id.makeBasic) eTop) (Id.clearSet is)
      Id.addMultiple (toList ete)
      pure ete

-- | Take a top entity and /expand/ its port names. I.e., make sure that every
-- port that should be generated in the HDL is part of the data structure. It
-- works on "FilteredHWType" in order to generate stable port names.
expandTopEntity
  :: HasCallStack
  => [(Maybe Id, FilteredHWType)]
  -- ^ Arguments. Ids are used as name hints.
  -> (Maybe Id, FilteredHWType)
  -- ^ Result. Id is used as name hint.
  -> Maybe TopEntity
  -- ^ Top entity to expand
  -> Either ExpandError (ExpandedTopEntity (Either Text Text))
  -- ^ Either some error (see "ExpandError") or and expanded top entity. The
  -- expanded top entity in turn contains an Either too. /Left/ means that
  -- the name was supplied by the user and should be inserted at verbatim,
  -- /Right/ is a name generated by Clash.
expandTopEntity ihwtys (oId, ohwty) topEntityM
  | Synthesize {..} <- fromMaybe (defSyn (error $(curLoc))) topEntityM = do
  -- TODO 1: Check sizes against number of PortProduct fields
  -- TODO 2: Warn about duplicate fields
  let
    argHints = map (maybe "arg" (Id.toText . Id.unsafeFromCoreId) . fst) ihwtys
    resHint = maybe "result" (Id.toText . Id.unsafeFromCoreId) oId

  inputs <- zipWith3M goInput argHints (map snd ihwtys) (extendPorts t_inputs)

  output <-
    -- BiSignalOut signals are filtered as their counterpart - BiSignalIn - will
    -- be printed as an inout port in HDL.
    if isVoid (stripFiltered ohwty) || isBiSignalOut (stripFiltered ohwty) then
      pure Nothing
    else
      Just <$> goPort resHint ohwty t_output

  pure (ExpandedTopEntity {
      et_inputs = inputs
    , et_output = output
    })
 where
  goInput
    :: Text
    -> FilteredHWType
    -> Maybe PortName
    -> Either ExpandError (Maybe (ExpandedPortName (Either Text Text)))
  goInput hint fHwty@(FilteredHWType hwty _) pM
    | isVoid hwty = Right Nothing
    | otherwise = Just <$> go hint fHwty pM

  -- Vector and RTree are hardcoded as product types, even when instantiated as
  -- /Vec 1 a/ or /RTree 0 a/ respectively.
  isProduct :: FilteredHWType -> Bool
  isProduct (FilteredHWType (CustomProduct {}) _) =
    -- CustomProducts are not yet support in mkTopInput/mkTopOutput so we can't treat
    -- them as product types.
    -- FIXME: Support CustomProduct in top entity annotations
    False
  isProduct (FilteredHWType (Vector {}) _) = True
  isProduct (FilteredHWType (RTree {}) _) = True
  isProduct (FilteredHWType _ [(_:_:_)]) = True
  isProduct _ = False

  go
    :: Text
    -> FilteredHWType
    -> Maybe PortName
    -> Either ExpandError (ExpandedPortName (Either Text Text))
  go hint hwty Nothing = goNoPort hint hwty
  go hint hwty (Just p) = goPort hint hwty p

  goPort
    :: Text
    -> FilteredHWType
    -> PortName
    -> Either ExpandError (ExpandedPortName (Either Text Text))
  goPort hint fHwty@(FilteredHWType hwty _) (PortName "") =
    -- TODO: The following logic makes using /no/ top entity annotation and
    -- TODO: using 'defSyn' behave differently. This is probably not what we
    -- TODO: want.
    if isJust topEntityM then
      -- If top entity annotation was explicitly given, render a single port
      pure (ExpandedPortName hwty (Right hint))
    else
      -- Treat empty 'PortName's as an non-annotated port if no explicit
      -- synthesize annotation was given.
      goNoPort hint fHwty
  goPort _hint (FilteredHWType hwty _) (PortName pn) =
    pure (ExpandedPortName hwty (Left (Text.pack pn)))
  goPort hint0 fHwty@(FilteredHWType hwty0 fields0) pp@(PortProduct p ps0)
    -- Attrs not allowed on product types
    | isProduct fHwty
    , (_:_) <- attrs
    = Left (AttrError attrs)

    -- Product types (products, vec, rtree) of which all but one field are
    -- zero-width.
    | isProduct fHwty
    , [fields1] <- fields0
    , ((_:_), [_]) <- partition fst fields1
    = case [go h t p_ | (h, (False, t), p_) <- zip3 hints fields1 ps1] of
        port:_ -> port
        _ -> error "internal error: insuffient ports"

    -- Product types (products, vec, rtree)
    | isProduct fHwty
    , [fields1] <- fields0
    = ExpandedPortProduct hint1 hwty1 <$>
        sequence [go h t p_ | (h, (False, t), p_) <- zip3 hints fields1 ps1]

    -- Things like "Maybe a" are allowed to be split up using a port
    -- annotation (but won't be split up if it's missing, should it?)
    -- FIXME: We probably shouldn't filter void constructs here, it's
    -- FIXME: inconsistent with how we deal with port annotations elsewhere
    | [(False, eHwty)] <- filter (not . fst) (concat fields0)
    , length fields0 > 1
    , length ps0 == 2
    , conHwty <- FilteredHWType (BitVector (conSize hwty0)) []
    = ExpandedPortProduct hint1 hwty1 <$>
        sequence [go h t p_ | (h, t, p_) <- zip3 hints [conHwty, eHwty] ps1]

    -- Port annotated as PortProduct, but wasn't one
    | otherwise
    = Left (PortProductError pp hwty1)

   where
    hint1 = if null p then hint0 else Text.pack p
    ps1 = extendPorts (map (prefixParent p) ps0)
    hints = map (\i -> hint1 <> "_" <> showt i) [(0::Int)..]
    (attrs, hwty1) = stripAttributes hwty0

  goNoPort
    :: Text
    -> FilteredHWType
    -> Either ExpandError (ExpandedPortName (Either Text Text))
  goNoPort hint fHwty@(FilteredHWType hwty0 fields0)
    -- Attrs not allowed on product types
    | isProduct fHwty
    , (_:_) <- attrs
    = Left (AttrError attrs)

    -- Product types (products, vec, rtree) of which all but one field are
    -- zero-width.
    | isProduct fHwty
    , [fields1] <- fields0
    , ((_:_), [_]) <- partition fst fields1
    = case [goNoPort h t | (h, (False, t)) <- zip hints fields1] of
        port:_ -> port
        _ -> error "internal error: insuffient ports"

    -- Product types (products, vec, rtree)
    | isProduct fHwty
    , [fields1] <- fields0
    = ExpandedPortProduct hint hwty1 <$>
        sequence [goNoPort h t | (h, (False, t)) <- zip hints fields1]

    -- All other types (sum of product, sum, "native")
    | otherwise
    = pure (ExpandedPortName hwty0 (Right hint))
   where
    (attrs, hwty1) = stripAttributes hwty0
    hints = map (\i -> hint <> "_" <> showt i) [(0::Int)..]

expandTopEntity _ _ topEntityM =
  error ("expandTopEntity expects a Synthesize annotation, but got: " <> show topEntityM)

-- | Convert a Core 'C.Literal' to a Netlist v'HW.Literal'
mkLiteral :: Int -- ^ int width
          -> C.Literal -> HW.Expr
mkLiteral iw lit = case lit of
  C.IntegerLiteral i -> HW.Literal (Just (Signed iw,iw)) $ NumLit i
  C.IntLiteral i     -> HW.Literal (Just (Signed iw,iw)) $ NumLit i
  C.WordLiteral w    -> HW.Literal (Just (Unsigned iw,iw)) $ NumLit w
  C.Int64Literal i   -> HW.Literal (Just (Signed 64,64)) $ NumLit i
  C.Word64Literal w  -> HW.Literal (Just (Unsigned 64,64)) $ NumLit w
#if MIN_VERSION_ghc(8,8,0)
  C.Int8Literal i    -> HW.Literal (Just (Signed 8,8)) $ NumLit i
  C.Int16Literal i   -> HW.Literal (Just (Signed 16,16)) $ NumLit i
  C.Int32Literal i   -> HW.Literal (Just (Signed 32,32)) $ NumLit i
  C.Word8Literal w   -> HW.Literal (Just (Unsigned 8,8)) $ NumLit w
  C.Word16Literal w  -> HW.Literal (Just (Unsigned 16,16)) $ NumLit w
  C.Word32Literal w  -> HW.Literal (Just (Unsigned 32,32)) $ NumLit w
#endif
  C.CharLiteral c    -> HW.Literal (Just (Unsigned 21,21)) . NumLit . toInteger $ ord c
  C.FloatLiteral w   -> HW.Literal (Just (BitVector 32,32)) (NumLit $ toInteger w)
  C.DoubleLiteral w  -> HW.Literal (Just (BitVector 64,64)) (NumLit $ toInteger w)
  C.NaturalLiteral n -> HW.Literal (Just (Unsigned iw,iw)) $ NumLit n
#if MIN_VERSION_base(4,15,0)
  C.ByteArrayLiteral (ByteArray ba) -> HW.Literal Nothing (NumLit (IP ba))
#else
  C.ByteArrayLiteral (ByteArray ba) -> HW.Literal Nothing (NumLit (Jp# (BN# ba)))
#endif
  C.StringLiteral s  -> HW.Literal Nothing $ StringLit s
