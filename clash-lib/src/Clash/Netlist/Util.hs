{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd
                    2017-2018, Google Inc.
                    2021-2026, QBayLogic B.V.
                    2022     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utilities for converting Core Type/Term to Netlist datatypes

  Utilities of Netlist generation on scoped Clash Core: opening a normalized
  component into a single scope ('splitNormalized'), and giving its variables
  their HDL identifiers ('mkUniqueNormalized').

  The translation of the types of scoped Core to HWTypes
  ('unsafeCoreTypeToHWTypeM', ...) takes the scope of the type, to look
  through newtypes and type families.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Netlist.Util where

import           Control.Exception       (throw)
import           Control.Lens            ((.=), (%=))
import qualified Control.Lens            as Lens
import           Control.Monad (zipWithM)
import           Control.Monad.Reader (ask)
import qualified Control.Monad.State as State
import           Control.Monad.State.Strict (State, evalState, get, modify)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import           Data.Char               (ord)
import           Data.Foldable           (Foldable(toList))
import           Data.Functor            (($>))
import           Data.Primitive.ByteArray (ByteArray (..))
import           Data.List               (unzip4, partition)
import qualified Data.Map                as Map
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe, isJust)
import           Text.Printf             (printf)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Extra         (showt)
import           Data.Text.Lazy          (toStrict)
import           Data.Text.Prettyprint.Doc.Extra

import           GHC.Num.Integer                  (Integer (..))

import           GHC.Stack               (HasCallStack)

import           GHC.Utils.Monad         (zipWith3M)

import Clash.Core.Basic (ConstTy (..), IsMultiPrim (..), LitTy (..), NameMod (..))
import           Clash.Annotations.TopEntity
  (TopEntity(..), PortName(..), defSyn)
import           Clash.Annotations.BitRepresentation.Internal
  (CustomReprs, ConstrRepr'(..), DataRepr'(..), Type'(..), getDataRepr,
   uncheckedGetConstrRepr)
import           Clash.Annotations.SynthesisAttributes (Attr)
import           Clash.Annotations.Primitive (HDL(VHDL))
import           Clash.Backend           (HasUsageMap (..), HWKind(..), hdlHWTypeKind, hdlKind)
import           Clash.Core.DataCon (DataCon (..))
import qualified Clash.Core.Literal      as C
import           Clash.Core.Name (Name (..), nameOcc)
import           Clash.Core.Pretty (PrettyPrec, showPpr, showPprDiag)
import           Clash.Core.TyCon (TyCon (FunTyCon), TyConName, tyConDataCons)
import           Clash.Core.NameMap (emptyNameMap, lookupGlobalEnv)
import           Clash.Core.Alpha (AlphaType (..), aeqTm, aeqTy)
import           Clash.Core.DataCon (dataConInstExtTyVars, openDataConSig)
import qualified Clash.Core.EqSolver as S (typeEq)
import qualified Data.IntSet as IntSet
import           Clash.Core.Subst
  (closeTerm, closeType, openBind, openIdBinder, openTyBinder, teleIdVars, withFreshIdBinder)
import qualified Clash.Core.Syntax as S
import qualified Clash.Core.Type as S
  (TypeView (..), coreView1, normalizeType, splitTyConAppM, tyView)
import qualified Clash.Core.Util as S (splitShouldSplit)
import           Clash.Core.TypeEnv (TypeEnv, findTyCon, lookupTyCon)
import {-# SOURCE #-} Clash.Netlist.BlackBox.Util
import qualified Clash.Netlist.Id as Id
import           Clash.Netlist.Types     as HW
import           Clash.Util
import qualified Clash.Util.Interpolate  as I

import           Control.Applicative         ((<|>))
import           Control.Monad               (when)
import           Control.Monad.Extra         (concatMapM)
import           Control.Monad.Reader        (local)
import           Control.Monad.Trans.Except  (runExcept)
import qualified Data.IntMap.Strict          as IntMap
import qualified Data.List                   as List
import           Data.Proxy                  (Proxy (..))
import           GHC.TypeLits                (someNatVal)
import           GHC.TypeNats                (SomeNat (..))
import           GHC.Utils.Outputable        (ppr, showSDocUnsafe)
import qualified Data.Map.Strict             as MapStrict
import           Control.Monad.State.Strict  (runState)
import           Data.Maybe                  (isNothing)

import qualified Clash.Core.Name             as C
import           Clash.Core.TermInfo  (MultiPrimInfo (..), multiPrimInfo')

import           Clash.Core.Evaluator (nfData)
import           Clash.Core.Evaluator.Primitives (primReductionMap)
import           Clash.Core.Evaluator.Types
  (Env (..), Machine (..), PrimReductions (..), initMachine)
import           Clash.Core.TermLiteral (termToDataError)
import           Clash.Core.Type      (coreView, tyLitShow)
import           Clash.Core.FreeVars  (tmNameOccursIn)
import           Clash.Core.Syntax
import           Clash.Core.HasType   (inferCoreTypeOf)
import           Clash.Core.Term      (collectArgsTicks, collectTicks, mkTicks, stripTicks)

import           Clash.Netlist.BlackBox.Types (BlackBoxMeta (..))
import {-# SOURCE #-} Clash.Netlist.Scoped.BlackBox (extractPrimWarnOrFail, mkBlackBoxContext)
import qualified Clash.Sized.Vector          as V
import           Clash.Primitives.Types      (CompiledPrimitive, Primitive (..))

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

-- | Converts a type to a HWType given a function that translates certain
-- builtin types. Errors if the type is not translatable.
unsafeCoreTypeToHWType
  :: Distinct n
  => SrcSpan
  -- ^ Approximate location in original source file
  -> String
  -> HWTypeTranslator
  -> CustomReprs
  -> TypeEnv
  -> S.Scope n
  -> S.Type n
  -> State HWMap FilteredHWType
unsafeCoreTypeToHWType sp loc builtInTranslation reprs te sc ty =
  either (\msg -> throw (ClashException sp (loc ++ msg) Nothing)) id <$>
    coreTypeToHWType builtInTranslation reprs te sc ty

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
  -> S.Type n
  -- ^ Custom reprs are index on type, so we need the clash core type to look
  -- it up.
  -> FilteredHWType
  -- ^ Type of previous argument represented as a HWType
  -> FilteredHWType
maybeConvertToCustomRepr reprs (typeToType' -> Right tyName) (FilteredHWType hwTy filtered)
  | Just dRepr <- getDataRepr tyName reprs =
    FilteredHWType
      (convertToCustomRepr reprs dRepr hwTy)
      [ [ (fieldAnn == 0, hwty) | ((_, hwty), fieldAnn) <- zip fields (crFieldAnns constr) ]
                                | (fields, constr) <- zip filtered (drConstrs dRepr)]
maybeConvertToCustomRepr _reprs _ty hwTy = hwTy

-- | Convert a type to the type of a custom bit representation
-- ('Clash.Annotations.BitRepresentation.Internal.Type''): applications of
-- type constructors and type-level literals
typeToType' :: S.Type n -> Either String Type'
typeToType' = \case
  S.AppTy t1 t2 -> AppTy' <$> typeToType' t1 <*> typeToType' t2
  S.ConstTy (TyCon name) -> return (ConstTy' (nameOcc name))
  S.LitTy (NumTy n) -> return (LitTy' n)
  S.LitTy (SymTy lit) -> return (SymLitTy' (Text.pack lit))
  e -> Left ($(curLoc) ++ "Unexpected type: " ++ show e)

-- | Same as @coreTypeToHWType@, but discards void filter information
coreTypeToHWType'
  :: Distinct n
  => HWTypeTranslator
  -> CustomReprs
  -> TypeEnv
  -> S.Scope n
  -> S.Type n
  -- ^ Type to convert to HWType
  -> State HWMap (Either String HWType)
coreTypeToHWType' builtInTranslation reprs te sc ty =
  fmap stripFiltered <$> coreTypeToHWType builtInTranslation reprs te sc ty

-- | Converts a type to a HWType given a function that translates certain
-- builtin types. Returns a string containing the error message when the type
-- is not translatable.
--
-- Caches the translation of closed types, see 'HWMap'.
coreTypeToHWType
  :: Distinct n
  => HWTypeTranslator
  -> CustomReprs
  -> TypeEnv
  -> S.Scope n
  -> S.Type n
  -- ^ Type to convert to HWType
  -> State HWMap (Either String FilteredHWType)
coreTypeToHWType builtInTranslation reprs te sc ty = case closeType ty of
  Just tyC -> do
    htyM <- Map.lookup (AlphaType tyC) <$> get
    case htyM of
      Just hty -> return hty
      _ -> do
        hty <- translate S.emptyScope tyC
        modify (Map.insert (AlphaType tyC) hty)
        return hty
  Nothing -> translate sc ty
 where
  translate
    :: Distinct m => S.Scope m -> S.Type m -> State HWMap (Either String FilteredHWType)
  translate sc1 ty1 = do
    -- Try builtin translation; for now this is hardcoded to be the one in ghcTypeToHWType
    hty0M <- runHWTypeTranslator builtInTranslation reprs te sc1 ty1
    case hty0M of
      Just hwtyE -> pure (maybeConvertToCustomRepr reprs ty1 <$> hwtyE)
      Nothing -> case S.coreView1 te sc1 ty1 of
        -- Strip transparant types:
        Just ty2 -> coreTypeToHWType builtInTranslation reprs te sc1 ty2
        Nothing -> case S.tyView ty1 of
          -- Try to create hwtype based on AST:
          S.TyConApp tc args -> runExceptT $ do
            hwty <- mkADT builtInTranslation reprs te sc1 (showPpr ty1) tc args
            return (maybeConvertToCustomRepr reprs ty1 hwty)
          -- All methods failed:
          _ -> return $ Left $ "Can't translate non-tycon type: " ++ showPpr ty1

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
  :: Distinct n
  => HWTypeTranslator
  -- ^ Hardcoded Type -> HWType translator
  -> CustomReprs
  -> TypeEnv
  -> S.Scope n
  -> String
  -- ^ String representation of the Core type for error messages
  -> TyConName
  -- ^ The TyCon
  -> [S.Type n]
  -- ^ Its applied arguments
  -> ExceptT String (State HWMap) FilteredHWType
  -- ^ An error string or a tuple with the type and possibly a list of
  -- removed arguments.
mkADT _ _ te _ tyString tc _
  | isRecursiveTy te tc
  = throwE $ $(curLoc) ++ "Can't translate recursive type: " ++ tyString

mkADT builtInTranslation reprs te sc tyString tc args = case tyConDataCons (findTyCon te tc) of
  []  -> return (FilteredHWType (Void Nothing) [])
  dcs -> do
    let tcName           = nameOcc tc
        -- The types of the fields, in the scope of the existentially
        -- quantified type variables of the data constructor
        fieldHWTys dc    = fromMaybe (error ($(curLoc) ++ "Wrong number of type arguments for: " ++ tyString)) $
          dataConInstExtTyVars te sc dc args $ \sc1 _exts argTys ->
            mapM (ExceptT . coreTypeToHWType builtInTranslation reprs te sc1) argTys
    argHTyss0           <- mapM fieldHWTys dcs
    let argHTyss1        = map (\tys -> zip (map isFilteredVoid tys) tys) argHTyss0
    let areVoids         = map (map fst) argHTyss1
    let filteredArgHTyss = map (map snd . filter (not . fst)) argHTyss1

    -- Every alternative is annotated with some examples. Be sure to read them.
    case (dcs, filteredArgHTyss) of
      _ | any (hasUnconstrainedExistential te) dcs ->
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
  :: TypeEnv
  -> DataCon
  -> Bool
hasUnconstrainedExistential te dc =
  openDataConSig dc $ \sc univ ext argTys ->
  let uTVs        = IntSet.fromList (teleTyRaws univ)
      eTVs        = teleTyRaws ext
      constraints = mapMaybe (S.typeEq te sc) argTys

      -- Is the existential `eTV` constrained by the constraint `(ty1,ty2)`
      isConstrainedBy eTV (ty1,ty2) =
        let -- Free FVs in the LHS and RHS of the constraint that are not the
            -- in the set of universal type variables of the constructor.
            ty1FEVs = filter (`IntSet.notMember` uTVs) (freeTyVarOccs ty1)
            ty2FEVs = filter (`IntSet.notMember` uTVs) (freeTyVarOccs ty2)

            -- Determine whether `eTV` can be generated from one side of a
            -- constraint, under the assumption that the other side of the
            -- constraint mentions no existential type variables.
            isGenerative ::
              -- Side (LHS or RHS) of a constraint
              S.Type x ->
              -- The raw names of its free type variables (that are no in the
              -- set of universal type variables)
              [Int] ->
              Bool
            isGenerative t efvs = case S.tyView t of
              S.TyConApp tcNm _
                | Just (FunTyCon {}) <- lookupTyCon te tcNm
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
              S.FunTy {}
                -- Functions are also fully generative
                -> eTV `elem` efvs
              S.OtherType other -> case other of
                S.VarTy _ _ v -> S.tyNameRaw v == eTV
                S.LitTy _ -> False
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
 where
  -- The raw names of the type variables a telescope binds
  teleTyRaws :: S.Tele S.TyBinder x y -> [Int]
  teleTyRaws = \case
    S.TNil -> []
    S.TCons b bs -> S.tyNameRaw (S.tyNameOf b) : teleTyRaws bs

  -- The raw names of the free type variables of a type, in order, as often as
  -- they occur, and those of the kinds of the free type variables
  freeTyVarOccs :: S.Type x -> [Int]
  freeTyVarOccs = go IntSet.empty
   where
    go :: IntSet.IntSet -> S.Type y -> [Int]
    go bound = \case
      S.VarTy _ k v ->
        let r = S.tyNameRaw v
            isBound = r `IntSet.member` bound
        in [r | not isBound] <> go (if isBound then bound else IntSet.empty) k
      S.ForAllTy b t ->
        go bound (S.tyBinderKind b) <> go (IntSet.insert (S.tyNameRaw (S.tyNameOf b)) bound) t
      S.AppTy l r -> go bound l <> go bound r
      S.AnnType _ t -> go bound t
      _ -> []

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
isRecursiveTy :: TypeEnv -> TyConName -> Bool
isRecursiveTy te tc = case tyConDataCons (findTyCon te tc) of
    []  -> False
    dcs -> let argTycons = concatMap fieldTyCons dcs
           in tc `elem` argTycons
 where
  fieldTyCons dc = openDataConSig dc $ \sc _ _ argTys ->
    -- Note [Look through type families in recursivity check]
    map fst (mapMaybe (S.splitTyConAppM . S.normalizeType te sc) argTys)

-- | Determines if a type is translatable to a HWType given a function that
-- translates certain builtin types.
representableType
  :: Distinct n
  => HWTypeTranslator
  -> CustomReprs
  -> Bool
  -- ^ String considered representable
  -> TypeEnv
  -> S.Scope n
  -> S.Type n
  -> Bool
representableType builtInTranslation reprs stringRepresentable te sc =
    flip evalState mempty .
    representableTypeState builtInTranslation reprs stringRepresentable te sc

-- | Like 'representableType', but memoizes the type to HWType translation in
-- the given state.
representableTypeState
  :: Distinct n
  => HWTypeTranslator
  -> CustomReprs
  -> Bool
  -- ^ String considered representable
  -> TypeEnv
  -> S.Scope n
  -> S.Type n
  -> State HWMap Bool
representableTypeState builtInTranslation reprs stringRepresentable te sc =
    fmap (either (const False) isRepresentable) .
    coreTypeToHWType' builtInTranslation reprs te sc
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

-- | Produce a 'Just' when predicate is True, else Nothing
orNothing :: Bool -> a -> Maybe a
orNothing True a = Just a
orNothing False _ = Nothing

-- | Render a blackbox given its context. Renders _just_ the blackbox, not any
-- corresponding includes, libraries, and so forth.
evalBlackBox :: HasCallStack => SomeBackend -> BlackBoxContext -> BlackBox -> Text
evalBlackBox (SomeBackend s) bbCtx bb
  | BBFunction _bbName _bbHash (TemplateFunction _usedArgs _verifFunc func) <- bb =
    let layout = LayoutOptions (AvailablePerLine 120 0.4) in
    toStrict (renderLazy (layoutPretty layout (State.evalState (func bbCtx) s)))
  | BBTemplate bbt <- bb =
    toStrict ((State.evalState (renderTemplate bbCtx bbt) s) 0)

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
  -> GlobalId
  -- ^ Create component name based on this global
  -> Text
genComponentName newInlineStrat prefixM g =
  Text.intercalate "_" (prefix ++ [fn1])
 where
  nm0 = Text.splitOn "." (nameOcc (S.globalIdName g))
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
genTopName _prefixM tb@(TestBench {}) =
  error $ "Internal error: genTopName: unexpected TestBench: " <> show tb
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
  :: GlobalId
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
  compNameM <- lookupGlobalEnv topEntity <$> Lens.use componentNames
  let
    topName = Id.toText topIdentifier
    topIdentifier = flip fromMaybe compNameM (error [I.i|
     Internal error in 'mkTopUnWrapper': tried to lookup (netlist) name
     of #{showPpr (S.globalIdName topEntity)}, but couldn't find it in NetlistState's
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
      assigns <- zipWithM contAssign ids (map (indexPN 10) [0..])
      if null attrs then
        return (concat ports, pDecl:assigns ++ concat decls, pName)
      else
        throwAnnotatedSplitError $(curLoc) "Vector"

    RTree {} -> do
      (ports, decls, ids) <- unzip3 <$> mapM mkTopInstInput ps
      assigns <- zipWithM contAssign ids (map (indexPN 10) [0..])
      if null attrs then
        return (concat ports, pDecl:assigns ++ concat decls, pName)
      else
        throwAnnotatedSplitError $(curLoc) "RTree"

    Product {} -> do
      (ports, decls, ids) <- unzip3 <$> mapM mkTopInstInput ps
      assigns <- zipWithM contAssign ids (map (indexPN 0) [0..])
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

          assigns <- sequence
              [ contAssign conId (Identifier pName (Just conIx))
              , contAssign elId  (FromBv Nothing elTy (Identifier pName (Just elIx))) ]

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
      netassgn <- contAssign pName (mkVectorChain sz hwty'' ids1)
      if null attrs then
        return (concat ports, pDecl:netassgn:concat decls, pName)
      else
        throwAnnotatedSplitError $(curLoc) "Vector"

    RTree d hwty'' -> do
      (ports, decls, ids0) <- unzip3 <$> mapM mkTopInstOutput ps
      let ids1 = map (flip Identifier Nothing) ids0
      netassgn <- contAssign pName (mkRTreeChain d hwty'' ids1)
      if null attrs then
        return (concat ports, pDecl:netassgn:concat decls, pName)
      else
        throwAnnotatedSplitError $(curLoc) "RTree"

    Product {} -> do
      (ports, decls, ids0) <- unzip3 <$> mapM mkTopInstOutput ps
      let ids1 = map (flip Identifier Nothing) ids0
      netassgn <- contAssign pName (DataCon hwty (DC (hwty,0)) ids1)
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
      netassgn <- contAssign pName (DataCon hwty (DC (BitVector (typeSize hwty),0)) ids2)
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

-- | Apply a name modifier to the names of the environment, see
-- 'Clash.Netlist.Util.withTicks'
applyNameMod :: NameMod -> Text -> NetlistEnv -> NetlistEnv
applyNameMod PrefixName s2 env@(NetlistEnv {_prefixName = s1})
  | Text.null s1 = env {_prefixName = s2}
  | otherwise    = env {_prefixName = s1 <> "_" <> s2}
applyNameMod SuffixName s2 env@(NetlistEnv {_suffixName = s1})
  | Text.null s1 = env {_suffixName = s2}
  | otherwise    = env {_suffixName = s2 <> "_" <> s1}
applyNameMod SuffixNameP s2 env@(NetlistEnv {_suffixName = s1})
  | Text.null s1 = env {_suffixName = s2}
  | otherwise    = env {_suffixName = s1 <> "_" <> s2}
applyNameMod SetName s env = env {_setName = Just s}

-- | Add the pre- and suffix names in the current environment to the given
-- identifier
affixName
  :: Text
  -> NetlistMonad Text
affixName nm0 = do
  NetlistEnv{_prefixName=pre, _suffixName=suf} <- ask
  let nm1 = if Text.null pre then nm0 else pre <> "_" <> nm0
      nm2 = if Text.null suf then nm1 else nm1 <> "_" <> suf
  return nm2

-- | Errors 'expandTopEntity' might yield
data ExpandError
  -- | Synthesis attributes are not supported on PortProducts
  = AttrError [Attr Text]
  -- | Something was annotated as being a PortProduct, but wasn't one
  | PortProductError PortName HWType

-- | Render an 'ExpandError' as a human readable error message.
expandErrorMessage :: ExpandError -> String
expandErrorMessage (AttrError attrs) = [I.i|
  Cannot use attribute annotations on product types of top entities. Saw
  annotation:

    #{attrs}
|]
expandErrorMessage (PortProductError pn hwty) = [I.i|
  Saw a PortProduct in a Synthesize annotation:

    #{pn}

  but the port type:

    #{hwty}

  is not a product!
|]

-- | Like 'expandTopEntityOrErrM', but pure: expands the top entity and throws a
-- formatted error (via 'expandErrorMessage') if the annotation is invalid.
expandTopEntityOrErr
  :: HasCallStack
  => [(Maybe Text, FilteredHWType)]
  -- ^ Arguments, with name hints
  -> (Maybe Text, FilteredHWType)
  -- ^ Result, with name hint
  -> Maybe TopEntity
  -- ^ If /Nothing/, an expanded top entity will be generated as if /defSyn/
  -- was passed.
  -> ExpandedTopEntity (Either Text Text)
expandTopEntityOrErr ihwtys ohwty topM =
  case expandTopEntity ihwtys ohwty topM of
    Left err -> error (expandErrorMessage err)
    Right eTop -> eTop

-- | Check the 'Synthesize' port annotation of a top entity against the types
-- of its arguments and result, throwing an error if the annotation is invalid.
--
-- The annotation is expected to already be split (see
-- 'Clash.Driver.splitTopEntityT'); the argument types are split to match before
-- checking.
checkTopEntityPorts
  :: HasCallStack
  => HWTypeTranslator
  -- ^ Hardcoded 'Type' -> 'HWType' translator
  -> CustomReprs
  -> TypeEnv
  -> GlobalId
  -- ^ Top entity binder
  -> Maybe TopEntity
  -- ^ Top entity annotation, if any. Expected to be already split, see above.
  -> ()
checkTopEntityPorts typeTrans reprs te topId topM =
  let
    hwtysE =
      flip evalState mempty $ fmap sequence $
        portTypes S.emptyScope (S.globalIdType topId) (S.globalIdType topId)
  in
    case hwtysE of
      -- A port type is polymorphic or otherwise untranslatable. Skip the check
      -- and let normalization produce its (clearer) diagnostic (e.g. "can only
      -- normalize monomorphic functions" or "non-representable return type").
      Left _ -> ()
      Right hwtys ->
        let (argHwtys, resHwty) = (init hwtys, last hwtys)
        -- Force the expansion, which throws on an invalid annotation.
        in expandTopEntityOrErr (map (Nothing,) argHwtys) (Nothing, resHwty) topM
             `seq` ()
 where
  -- The HWTypes of the arguments and of the result of a function type, split
  -- like 'S.splitCoreFunForallTy' does. Only value arguments carry ports;
  -- type/dictionary arguments are skipped. 'S.splitShouldSplit' mirrors
  -- 'separateArguments', so the resulting types align with the (already
  -- split) ports in the annotation.
  portTypes
    :: Distinct n
    => S.Scope n
    -> S.Type n
    -- ^ The type, as it was before looking through it with 'S.coreView1'
    -> S.Type n
    -> State HWMap [Either String FilteredHWType]
  portTypes sc origTy ty
    | Just ty1 <- S.coreView1 te sc ty = portTypes sc origTy ty1
    | S.ForAllTy b res <- ty =
        openTyBinder sc b res (\sc1 _ res1 -> portTypes sc1 res1 res1)
    | S.FunTy arg res <- S.tyView ty =
        (++) <$> mapM (coreTypeToHWType typeTrans reprs te sc) (S.splitShouldSplit te sc [arg])
             <*> portTypes sc res res
    | otherwise = (:[]) <$> coreTypeToHWType typeTrans reprs te sc origTy

-- | Same as 'expandTopEntity', but also adds identifiers to the identifier
-- set of the monad.
expandTopEntityOrErrM
  :: HasCallStack
  => [(Maybe Text, FilteredHWType)]
  -- ^ Arguments, with name hints
  -> (Maybe Text, FilteredHWType)
  -- ^ Result, with name hint
  -> Maybe TopEntity
  -- ^ If /Nothing/, an expanded top entity will be generated as if /defSyn/
  -- was passed.
  -> NetlistMonad (ExpandedTopEntity Identifier)
  -- ^ Either some error (see "ExpandError") or and expanded top entity. All
  -- identifiers in the expanded top entity will be added to NetlistState's
  -- IdentifierSet.
expandTopEntityOrErrM ihwtys ohwty topM = do
  is <- identifierSetM id
  let eTop = expandTopEntityOrErr ihwtys ohwty topM
  let ete = evalState (traverse (either Id.addRaw Id.makeBasic) eTop) (Id.clearSet is)
  Id.addMultiple (toList ete)
  pure ete

-- | Take a top entity and /expand/ its port names. I.e., make sure that every
-- port that should be generated in the HDL is part of the data structure. It
-- works on "FilteredHWType" in order to generate stable port names.
expandTopEntity
  :: HasCallStack
  => [(Maybe Text, FilteredHWType)]
  -- ^ Arguments, with name hints
  -> (Maybe Text, FilteredHWType)
  -- ^ Result, with name hint
  -> Maybe TopEntity
  -- ^ Top entity to expand
  -> Either ExpandError (ExpandedTopEntity (Either Text Text))
  -- ^ Either some error (see "ExpandError") or and expanded top entity. The
  -- expanded top entity in turn contains an Either too. /Left/ means that
  -- the name was supplied by the user and should be inserted at verbatim,
  -- /Right/ is a name generated by Clash.
expandTopEntity ihwtys (oHint, ohwty) topEntityM
  | Synthesize {..} <- fromMaybe (defSyn (error $(curLoc))) topEntityM = do
  -- TODO 1: Check sizes against number of PortProduct fields
  -- TODO 2: Warn about duplicate fields
  let
    argHints = map (fromMaybe "arg" . fst) ihwtys
    resHint = fromMaybe "result" oHint

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
  C.Int8Literal i    -> HW.Literal (Just (Signed 8,8)) $ NumLit i
  C.Int16Literal i   -> HW.Literal (Just (Signed 16,16)) $ NumLit i
  C.Int32Literal i   -> HW.Literal (Just (Signed 32,32)) $ NumLit i
  C.Word8Literal w   -> HW.Literal (Just (Unsigned 8,8)) $ NumLit w
  C.Word16Literal w  -> HW.Literal (Just (Unsigned 16,16)) $ NumLit w
  C.Word32Literal w  -> HW.Literal (Just (Unsigned 32,32)) $ NumLit w
  C.CharLiteral c    -> HW.Literal (Just (Unsigned 21,21)) . NumLit . toInteger $ ord c
  C.FloatLiteral w   -> HW.Literal (Just (BitVector 32,32)) (NumLit $ toInteger w)
  C.DoubleLiteral w  -> HW.Literal (Just (BitVector 64,64)) (NumLit $ toInteger w)
  C.NaturalLiteral n -> HW.Literal (Just (Unsigned iw,iw)) $ NumLit n
  C.ByteArrayLiteral (ByteArray ba) -> HW.Literal Nothing (NumLit (IP ba))
  C.StringLiteral s  -> HW.Literal Nothing $ StringLit s

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
-- Nested let-expressions need no deshadowing: the binders of scoped Core are
-- distinct.
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
-- ports. The identifiers go in the context (t'CompCtx').
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
        renames0 = MapStrict.fromList [ (tmNameRaw nm, occ) | (nm, occ) <- renames, not (isRes nm) ]
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
                      (MapStrict.lookup (tmNameRaw (localIdName b)) renames0)
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

-- | A unique HDL identifier based on the human readable name of a variable
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
-- 'Clash.Netlist.Scoped.BlackBox.extractPrimWarnOrFail'): the variable its
-- result is assigned to, and the arguments it is applied to. Variables among
-- the arguments are resolved into the given sibling let-bindings, a few
-- levels deep.
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
-- ticks.
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
                -- See 'Clash.Rewrite.Whnf.evaluatorEnv'
                env0 = Env te (PrimReductions primReductionMap Nothing)
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

  -- See 'Clash.Core.Term.eqTickInfo'
  eqTick :: TickInfo n -> TickInfo n -> Bool
  eqTick t1 t2 = case (t1, t2) of
    (SrcSpan sp1, SrcSpan sp2) -> sp1 == sp2
    (NameMod m1 ty1, NameMod m2 ty2) -> m1 == m2 && aeqTy ty1 ty2
    (DeDup, DeDup) -> True
    (NoDeDup, NoDeDup) -> True
    (Attributes ty1 e1, Attributes ty2 e2) -> aeqTy ty1 ty2 && aeqTm e1 e2
    _ -> False
