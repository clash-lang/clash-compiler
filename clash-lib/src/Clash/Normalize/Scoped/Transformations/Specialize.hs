{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Specialization for scoped Clash Core: the ported counterparts of
  "Clash.Normalize.Transformations.Specialize".

  'specialize' creates a closed global function, which abstracts over the
  arguments before the one it specializes on, and over the free variables of
  that argument. Its binders are created in the empty scope:

  * The binders of the arguments get the types of the parameters of the
    function, instantiated with the new type binders before them (see
    'Clash.Normalize.Transformations.Specialize.instArgBndrs').

  * The free variables of the argument are bound with
    'Clash.Core.Scoped.Subst.closeOver', type variables first, every type
    variable after the type variables in its kind.

  Only global variables ('GlobalVar') are looked up in the binding map, the
  top entities, and the specialisation history, so a local variable never
  gets specialized using the binding of a global with the same unique.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Normalize.Scoped.Transformations.Specialize
  ( specialize
  , specialize'
  , specArgBndrsAndVars
    -- * Helpers of 'specialize'
  , specCacheKey
  , preferNoInline
  , specializeName
  ) where

import           Control.Exception           (throw)
import           Control.Lens                ((%=))
import qualified Control.Lens                as Lens
import           Data.Bifunctor              (bimap)
import           Data.Coerce                 (coerce)
import qualified Data.Map.Strict             as Map
import qualified Data.Text                   as Text
import qualified Data.Text.Extra             as Text
import           GHC.BasicTypes.Extra        (isNoInline)
import           GHC.Num.Integer             (Integer (IS))
import           GHC.Num.Natural             (Natural (NS))
import           GHC.Stack                   (HasCallStack)
import           GHC.Types.Basic             (InlineSpec (..))

import           Clash.Core.Literal          (Literal (..))
import qualified Clash.Core.Name             as C
import           Clash.Core.Pretty           (showPpr)
import qualified Clash.Core.Term             as C
import qualified Clash.Core.Type             as C
import qualified Clash.Core.Var              as C
import qualified Clash.Data.UniqMap          as UniqMap
import           Clash.Debug                 (traceIf, traceM)
import           Clash.Driver.Types
  (Binding (..), TransformationInfo (..), hasTransformationInfo)
import           Clash.Normalize.Types
  (NormalizeSession, specialisationCache, specialisationHistory)
import           Clash.Rewrite.Types
  (curFun, debugOpts, extra, scopedTypeEnv, specializationLimit)
import           Clash.Unique                (Unique)
import           Clash.Util                  (ClashException (..))

import           Clash.Core.Scoped.Convert   (toClashTerm, toClashType)
import           Clash.Core.Scoped.FreeVars  (freeTyVarsOfType)
import           Clash.Core.Scoped.HasType   (globalIdType, piResultTy)
import           Clash.Core.Scoped.NameMap   (nullNameSet)
import           Clash.Core.Scoped.Pretty    ()
import           Clash.Core.Scoped.Subst
import           Clash.Core.Scoped.Syntax
import           Clash.Core.Scoped.Term
  (AbsBinder (..), collectArgsTicks, mkAbstraction, mkApps, mkLams, mkTicks, mkTyLams)
import           Clash.Core.Scoped.TermInfo  (isPolyFun)
import           Clash.Core.Scoped.Type      (TypeView (..), coreView1, normalizeType, tyView)
import           Clash.Core.Scoped.TypeEnv   (TypeEnv)
import           Clash.Rewrite.Scoped.Globals
  (findAlphaEqBinding, isTopEntity, lookupBinding, mkFunction)
import           Clash.Rewrite.Scoped.Types  (TransformContext, tfScope)
import           Clash.Rewrite.Scoped.Util
  (abstractOver, changed, dependencySortTyVars, normalizeTermTypes)

-- | Specialize an application on its argument, see
-- 'Clash.Normalize.Transformations.Specialize.specialize'
specialize
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> NormalizeSession (Term n)
specialize ctx e = case e of
  TyApp e1 ty -> specialize' ctx e (collectArgsTicks e1) (Right ty)
  App e1 e2 -> specialize' ctx e (collectArgsTicks e1) (Left e2)
  _ -> return e

-- | Specialize an application on its argument, see
-- 'Clash.Normalize.Transformations.Specialize.specialize''
specialize'
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -- ^ Transformation context
  -> Term n
  -- ^ Original term
  -> (Term n, [Either (Term n) (Type n)], [TickInfo n])
  -- ^ Function part of the term, split into root and applied arguments
  -> Either (Term n) (Type n)
  -- ^ Argument to specialize on
  -> NormalizeSession (Term n)
specialize' ctx e (fHd, args, ticks) specArgIn = case fHd of
  -- Only global functions have a binding to specialize
  Var {} -> return e
  GlobalVar f -> specializeGlobal ctx e f args ticks specArgIn
  -- Specializing non Var's is used by nonRepANF
  _ | Left specArg <- specArgIn -> do
        let (specTvs, specIds) = specArgBndrsAndVars (Left specArg)
            -- Create specialized function
            newBody = abstractOver "specialize" specTvs specIds specArg
        -- See if there's an existing binder that's alpha-equivalent to the
        -- specialized function
        newf <- findAlphaEqBinding newBody >>= \case
          -- Create a new function if an alpha-equivalent binder doesn't exist
          Nothing -> do
            (cf, sp) <- Lens.use curFun
            mkFunction (C.appendToName (C.varName cf) "_specF") sp NoUserInlinePrag newBody
          Just g -> pure g
        -- Create specialized argument
        let newArg = Left (mkApps (GlobalVar newf) (specVarArgs specTvs specIds))
        -- Use specialized argument
        changed (mkApps (mkTicks fHd ticks) (args ++ [newArg]))
    | otherwise -> return e

-- | 'specialize'' for an application of a global function
specializeGlobal
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> GlobalId
  -> [Either (Term n) (Type n)]
  -> [TickInfo n]
  -> Either (Term n) (Type n)
  -> NormalizeSession (Term n)
specializeGlobal ctx e f args ticks specArgIn = do
  opts <- Lens.view debugOpts
  te <- Lens.view scopedTypeEnv
  topEnt <- isTopEntity f
  if topEnt then
    -- Don't specialize TopEntities
    case specArgIn of
      Left _ -> do
        traceM ("Not specializing TopEntity: " ++ showPpr (C.varName fV))
        return e
      Right tyArg
        -- TopEntities aren't allowed to be semantically polymorphic. But using
        -- type equality constraints they may be syntactically polymorphic.
        -- > topEntity :: forall dom . (dom ~ "System") => Signal dom Bool -> Signal dom Bool
        -- The TyLam's in the body will have been removed by
        -- 'Clash.Normalize.Util.substWithTyEq'. So we drop the TyApp
        -- ("specializing" on it) and change the type of the variable to match.
        -- A global has a closed type, so this needs a closed type argument.
        | let newTy = piResultTy te sc (globalIdType te sc f) tyArg
        , nullNameSet (freeTyVarsOfType newTy) ->
            traceIf (hasTransformationInfo AppliedTerm opts)
              ("Dropping type application on TopEntity: " ++ showPpr (C.varName fV)
                 ++ "\ntype:\n" ++ showPpr tyArg) $
              changed (mkApps (mkTicks (GlobalVar (mkGlobalId fV {C.varType = toClashType newTy})) ticks) args)
        | otherwise -> return e
  else do
    let specArg = bimap (normalizeTermTypes te sc) (normalizeType te sc) specArgIn
        -- The free variables of 'specArg' to abstract over, see
        -- 'specArgBndrsAndVars'
        (specTvs, specIds) = specArgBndrsAndVars specArg
        specIdBndrs = map (normalizeTermTypes te sc) specIds
        specVars = specVarArgs specTvs specIds
        argLen = length args
        -- See Note [ticks and specialization] and Note [bignum literals and
        -- specialization] in "Clash.Normalize.Transformations.Specialize"
        specAbs :: Either C.Term C.Type
        specAbs = case specArg of
          Left tm -> Left (specCacheKey (toClashTerm (abstractOver "specialize" specTvs specIdBndrs tm)))
          Right ty -> Right (toClashType ty)
    -- Determine if 'f' has already been specialized on (a type-normalized)
    -- 'specArg'
    specM <- Map.lookup (fV, argLen, specAbs) <$> Lens.use (extra . specialisationCache)
    case specM of
      -- Use previously specialized function
      Just f' ->
        traceIf (hasTransformationInfo AppliedTerm opts)
          ("Using previous specialization of " ++ showPpr (C.varName fV) ++ " on " ++
            either showPpr showPpr specAbs ++ ": " ++ showPpr (C.varName f')) $
          changed (mkApps (mkTicks (GlobalVar (mkGlobalId f')) ticks) (args ++ specVars))
      -- Create new specialized function
      Nothing -> lookupBinding f >>= \case
        -- Determine if we can specialize f
        Nothing -> return e
        Just (Binding _ sp inl _ bodyTm _) -> do
          -- Determine if we see a sequence of specializations on a growing
          -- argument
          specHistM <- UniqMap.lookup fV <$> Lens.use (extra . specialisationHistory)
          specLim <- Lens.view specializationLimit
          if maybe False (> specLim) specHistM
            then throw (ClashException
                        sp
                        (unlines [ "Hit specialization limit " ++ show specLim ++ " on function `" ++ showPpr (C.varName fV) ++ "'.\n"
                                 , "The function `" ++ showPpr fV ++ "' is most likely recursive, and looks like it is being indefinitely specialized on a growing argument.\n"
                                 , "Body of `" ++ showPpr fV ++ "':\n" ++ showPpr bodyTm ++ "\n"
                                 , "Argument (in position: " ++ show argLen ++ ") that triggered termination:\n" ++ either showPpr showPpr specArg
                                 , "Run with '-fclash-spec-limit=N' to increase the specialization limit to N."
                                 ])
                        Nothing)
            else do
              -- Determine name the resulting specialized function, and the
              -- form of the specialized-on argument, see
              -- 'Clash.Normalize.Transformations.Specialize.specialize''
              (newName, inl', specArg') <- case specArg of
                Left a@(collectArgsTicks -> (gHd, gArgs, _))
                  | Just gNm <- varName gHd
                  , isPolyFun te sc a -> do
                      gTmM <- case gHd of
                        GlobalVar g -> lookupBinding g
                        _ -> pure Nothing
                      -- Don't inline the bodies of functions with a
                      -- Synthesize annotation, see issue #3024
                      gTop <- case gHd of
                        GlobalVar g -> isTopEntity g
                        _ -> pure False
                      let gBody = if gTop then Nothing else fmap bindingTerm gTmM
                      return
                        ( specializeName (inl, C.varName fV) (bindingSpec <$> gTmM, gNm)
                        , preferNoInline inl (maybe NoUserInlinePrag bindingSpec gTmM)
                        , maybe specArg (\b -> Left (mkApps (sinkClosed b) gArgs)) gBody
                        )
                _ -> return (C.varName fV, inl, specArg)
              -- Create the specialized function
              let names = collectBndrsMinusApps bodyTm ++
                    [ C.mkUnsafeInternalName ("pTS" `Text.append` Text.pack (show n)) n
                    | n <- [(0::Unique)..] ]
              case specBody te (globalIdType te emptyScope f) names args specTvs specIdBndrs bodyTm specArg' of
                -- The type of 'f' doesn't have the parameters for the
                -- arguments
                Nothing -> return e
                Just newBody -> do
                  newf <- mkFunction newName sp inl' newBody
                  -- Remember specialization
                  (extra . specialisationHistory) %= UniqMap.insertWith (+) fV 1
                  (extra . specialisationCache) %= Map.insert (fV, argLen, specAbs) (globalIdVar newf)
                  -- Use specialized function
                  changed (mkApps (mkTicks (GlobalVar newf) ticks) (args ++ specVars))
 where
  sc = tfScope ctx
  fV = globalIdVar f

  varName :: Term x -> Maybe (C.Name C.Term)
  varName = \case
    Var occ _ _ -> Just occ
    GlobalVar g -> Just (C.varName (globalIdVar g))
    _ -> Nothing

{-
Note [ticks and specialization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As Clash now distinguishes between ticks in expressions when comparing for
alpha equality, this has a knock-on effect when accessing the specialization
cache. Consider these applications which differ only by ticks:

    f[GlobalId] (\x -> ... x[LocalId])
    f[GlobalId] <tick>(\x -> ... x[LocalId])
    f[GlobalId] (\x -> ... <tick>x[LocalId])

If one of these had been specialized, the other two would hit that term in the
specialization cache, saving Clash from having to re-do work which is in effect
the same. To preserve this behavior, we strip all ticks from the keys for the
specialization cache, see 'specCacheKey'.

TODO While this preserves the old behavior, the old behavior is likely not
quite what we want. Using a value from the specialization cache may change the
ticks present, which can affect naming / debugging information in generated HDL.
We may also not want to look at ticks, as then the specialization cache will
miss on virtually every lookup which could add to normalization time.
-}

{-
Note [bignum literals and specialization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC represents a small 'Natural' or 'Integer' constant as a constructor applied
to a literal, e.g. @NS 8000##@. The evaluator reads such a constant back as a
plain literal, @8000@, so a constant that went through 'reduceConst' looks
different from one that didn't, even though they're the same value. Both forms
reach 'specialize', for example when a 'KnownDomain' dictionary built by GHC is
passed to a function at one call site, while at another call site the same
dictionary is the result of normalizing an application of a work-free function
(see 'Clash.Normalize.Util.normalizeWorkFreeApp'). To prevent creating two
identical specializations, we use the literal form in the keys of the
specialization cache.
-}

-- | Prepare a term for use as a key in the specialization cache: strip all
-- ticks (see Note [ticks and specialization]) and replace small bignum
-- constants by literals (see Note [bignum literals and specialization]).
specCacheKey :: C.Term -> C.Term
specCacheKey = go
 where
  go (C.Lam i x) = C.Lam i (go x)
  go (C.TyLam i x) = C.TyLam i (go x)
  go (C.App f x) = case (go f, go x) of
    (C.Prim p, C.Literal (WordLiteral w))
      | C.primName p == Text.showt 'NS -> C.Literal (NaturalLiteral w)
    (C.Prim p, C.Literal (IntLiteral i))
      | C.primName p == Text.showt 'IS -> C.Literal (IntegerLiteral i)
    (f', x') -> C.App f' x'
  go (C.TyApp f a) = C.TyApp (go f) a
  go (C.Let bs x) = C.Let (goBinds bs) (go x)
  go (C.Case x ty alts) = C.Case (go x) ty (fmap go <$> alts)
  go (C.Cast x a b) = C.Cast (go x) a b
  go (C.Tick _ x) = go x
  go x = x

  goBinds (C.NonRec i x) = C.NonRec i (go x)
  goBinds (C.Rec ixs) = C.Rec (fmap go <$> ixs)

-- | Given two 'InlineSpec's, return the \"strongest\" one. I.e., the one that's
-- closest to @NoInline@ (or @Opaque@ for newer GHCs).
preferNoInline :: InlineSpec -> InlineSpec -> InlineSpec
preferNoInline is0 is1
  | enumInlineSpec is0 >= enumInlineSpec is1 = is0
  | otherwise                                = is1
 where
  enumInlineSpec :: InlineSpec -> Int
  enumInlineSpec = \case
    NoUserInlinePrag {} -> -1
    Inline {} -> 0
    Inlinable {} -> 1
    NoInline {} -> 2
    Opaque {} -> 3

-- | The name of the function that specializes function @f@ on function @g@,
-- given their inline specifications (if @g@ has a binding) and names. It
-- depends on their NOINLINE / OPAQUE annotations, see
-- 'Clash.Normalize.Transformations.Specialize.specialize''.
specializeName
  :: (InlineSpec, C.Name C.Term) -> (Maybe InlineSpec, C.Name C.Term) -> C.Name C.Term
specializeName (spec0, n0) (spec1, n1)
  | (ann0 && ann1) || (not ann0 && not ann1)
  = n0{C.nameOcc = C.nameOcc n0 <> "_" <> stripMods (C.nameOcc n1)}
  | ann0 = n0
  | otherwise = n1
 where
  ann0 = isNoInline spec0
  ann1 = fmap isNoInline spec1 == Just True

  stripMods :: Text.Text -> Text.Text
  stripMods = Text.takeWhileEnd (/= '.')

-- | The body of the specialized function: the body of the function applied
-- to its arguments, and to the argument specialized on, abstracted over the
-- arguments and the free variables of the argument specialized on. 'Nothing'
-- if the type of the function doesn't have parameters for the arguments.
specBody
  :: forall n
   . HasCallStack
  => TypeEnv
  -> Type 'VoidS
  -- ^ The type of the function
  -> [C.Name C.Term]
  -- ^ Names for the binders of the arguments
  -> [Either (Term n) (Type n)]
  -- ^ The arguments
  -> [Type n]
  -- ^ The free type variables of the argument specialized on
  -> [Term n]
  -- ^ The free term variables of the argument specialized on
  -> Term 'VoidS
  -- ^ The body of the function
  -> Either (Term n) (Type n)
  -- ^ The argument specialized on
  -> Maybe (Term 'VoidS)
specBody te fTy names args specTvs specIds bodyTm specArg =
  case closeOver specTvs specIds build of
    Just r -> r
    Nothing -> error "specialize: can't abstract over the free variables of the argument"
 where
  build
    :: (DExt 'VoidS i, DExt i l)
    => Scope l -> Tele TyBinder 'VoidS i -> Tele IdBinder i l -> Closure n l
    -> Maybe (Term 'VoidS)
  build _ tvTele idTele cl = case either (fmap Left . closureTerm cl) (fmap Right . closureType cl) specArg of
    Nothing -> error "specialize: the argument mentions other variables"
    Just specArgL ->
      -- Binders for the arguments, followed by the free variables of the
      -- argument specialized on, moved after them
      withArgBinders te emptyScope fTy names args $ \sc1 argTele argVars ->
        substTele substTyBinder (closedSubst sc1) tvTele $ \s1 tvTele1 ->
          substTele substIdBinder s1 idTele $ \s2 idTele1 ->
            let sc3 = substScope s2
                argVars1 = map (bimap (sinkTo sc3) (sinkTo sc3)) (map (bimap (sinkTo (substScope s1)) (sinkTo (substScope s1))) argVars)
                specArg1 = bimap (substTm s2) (substTy s2) specArgL
                body = mkApps (sinkClosed bodyTm) (argVars1 ++ [specArg1])
            in  mkAbstraction (mkTyLams (mkLams body idTele1) tvTele1) argTele

  sinkTo :: (Sinkable e, DExt x y) => Scope y -> e x -> e y
  sinkTo _ = sink

-- | Binders for the arguments of a function of the given type, with the given
-- names, and their variables. A type binder gets the kind of the type
-- parameter it replaces, a term binder the type of its parameter,
-- instantiated with the type binders before it. 'Nothing' if the type doesn't
-- have parameters for all arguments. See
-- 'Clash.Normalize.Transformations.Specialize.instArgBndrs'.
withArgBinders
  :: (HasCallStack, Distinct x)
  => TypeEnv
  -> Scope x
  -> Type x
  -- ^ The type of the function, applied to the arguments before
  -> [C.Name C.Term]
  -> [Either a b]
  -- ^ The arguments: only whether they are terms or types matters
  -> (forall l. DExt x l => Scope l -> Tele AbsBinder x l -> [Either (Term l) (Type l)] -> r)
  -> Maybe r
withArgBinders te sc ty names args k = case (args, names) of
  ([], _) -> Just (k sc TNil [])
  (arg : rest, nm : nms) -> case (ty, arg) of
    (ForAllTy b res, Right _) ->
      withFreshTyBinder sc (coerce nm) (tyBinderKind b) $ \sc1 b1 ->
        let res1 = substTy (extendTySubst (extendSubstScope sc1 (idSubst sc)) b (tyBinderVar b1)) res
        in  withArgBinders te sc1 res1 nms rest $ \sc2 tele vars ->
              k sc2 (TCons (AbsTy b1) tele) (Right (sink (tyBinderVar b1)) : vars)
    (tyView -> FunTy argTy res, Left _) ->
      withFreshIdBinder sc nm argTy $ \sc1 b1 ->
        withArgBinders te sc1 (sink res) nms rest $ \sc2 tele vars ->
          k sc2 (TCons (AbsId b1) tele) (Left (sink (idBinderVar b1)) : vars)
    _ | Just ty1 <- coreView1 te sc ty -> withArgBinders te sc ty1 names args k
      | otherwise -> Nothing
  (_ : _, []) -> error "withArgBinders: not enough names"

-- | The names of the binders of the lambdas of a term, see
-- @collectBndrsMinusApps@ in
-- 'Clash.Normalize.Transformations.Specialize.specialize''
collectBndrsMinusApps :: Term n -> [C.Name C.Term]
collectBndrsMinusApps = reverse . go []
 where
  go :: [C.Name C.Term] -> Term x -> [C.Name C.Term]
  go bs = \case
    Lam b e' -> go (idBinderOcc b : bs) e'
    TyLam b e' -> go (coerce (tyBinderOcc b) : bs) e'
    App e' _ -> case go [] e' of
      [] -> bs
      bs' -> init bs' ++ bs
    TyApp e' _ -> case go [] e' of
      [] -> bs
      bs' -> init bs' ++ bs
    _ -> bs

-- | The free variables of the argument to specialize on, to abstract over:
-- the type variables in the order of their first occurrence, but every type
-- variable after the type variables in its kind; and the term variables in
-- the order of their first occurrence. See Note [Collect free-variables in an
-- insertion-ordered set] in "Clash.Normalize.Transformations.Specialize".
--
-- Unlike 'Clash.Normalize.Transformations.Specialize.specArgBndrsAndVars',
-- this gives the occurrences of the variables, of which
-- 'Clash.Core.Scoped.Subst.closeOver' makes the binders. The free type
-- variables of a type are in the order of their first occurrence too, rather
-- than in the order of their uniques.
specArgBndrsAndVars
  :: HasCallStack
  => Either (Term n) (Type n)
  -> ([Type n], [Term n])
specArgBndrsAndVars = \case
  Left tm -> case freeVarOccs tm of
    (tvs, ids) -> (dependencySortTyVars tvs, ids)
  Right ty -> (dependencySortTyVars (freeTyVarOccsOfType ty), [])

-- | The free variables of the argument specialized on, as arguments of the
-- specialized function
specVarArgs :: [Type n] -> [Term n] -> [Either (Term n) (Type n)]
specVarArgs tvs ids = map Right tvs ++ map Left ids
