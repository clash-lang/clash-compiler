{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations for specialization on scoped Clash Core, and application
  propagation ('appProp').

  'specialize' creates a closed global function, which abstracts over the
  arguments before the one it specializes on, and over the free variables of
  that argument. Its binders are created in the empty scope:

  * The binders of the arguments get the types of the parameters of the
    function, instantiated with the new type binders before them (see
    'withArgBinders'), as the types of the arguments can mention type
    variables bound at the call site.

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
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Normalize.Scoped.Transformations.Specialize
  ( appProp
  , appPropWorker
  , constantSpec
  , constantSpecWorker
  , specialize
  , specializeIn
  , nonRepSpec
  , nonRepSpecWorker
  , typeSpec
  , typeSpecWorker
  , zeroWidthSpec
  , zeroWidthSpecWorker
  , specArgBndrsAndVars
    -- * Helpers of 'specialize'
  , preferNoInline
  , specializeName
  ) where

import           Control.Exception           (throw)
import           Control.Lens                ((%=))
import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
import qualified Control.Monad.Writer        as Writer
import           Data.Bifunctor              (bimap)
import           Data.Coerce                 (coerce)
import           Data.Either                 (rights)
import qualified Data.Map.Strict             as Map
import qualified Data.Monoid                 as Monoid
import qualified Data.Text                   as Text
import qualified Data.Text.Extra             as Text
import           GHC.BasicTypes.Extra        (isNoInline)
import           GHC.Num.Integer             (Integer (IS))
import           GHC.Num.Natural             (Natural (NS))
import           GHC.Stack                   (HasCallStack)
import           GHC.Types.Basic             (InlineSpec (..))

import Clash.Core.Basic (IsMultiPrim (..), LitTy (..), WorkInfo (..))
import qualified Clash.Sized.Internal.BitVector as BV (BitVector, fromInteger#)
import qualified Clash.Sized.Internal.Index  as I (Index, fromInteger#)
import qualified Clash.Sized.Internal.Signed as S (Signed, fromInteger#)
import qualified Clash.Sized.Internal.Unsigned as U (Unsigned, fromInteger#)

import           Clash.Core.DataCon          (dcExtTyVarCount)
import           Clash.Core.Literal          (Literal (..))
import qualified Clash.Core.Name             as C
import           Clash.Core.Pretty           (showPpr)
import qualified Clash.Core.TyCon            as C
import           Clash.Core.Scoped.TysPrim   (integerPrimTyConName, naturalPrimTyConName, typeNatKindTyConName)
import           Clash.Debug                 (traceIf, traceM)
import           Clash.Driver.Types
  (Binding (..), TransformationInfo (..), hasTransformationInfo)
import           Clash.Normalize.Types
  (NormalizeSession, specialisationCache, specialisationHistory)
import           Clash.Rewrite.StrategyDSL
  (Transformation, onApp, onAppNode, onLam, onTyApp, onTyAppNode, toScopedTransformation)
import           Clash.Rewrite.Types
  (censor, curFun, debugOpts, extra, typeEnv, specializationLimit)
import           Clash.Unique                (Unique)
import           Clash.Util                  (ClashException (..))

import           Clash.Core.Scoped.Builder   (TyB, forAllTyB, funTyB, runClosedType, tyConAppB, varTyB)
import           Clash.Core.Scoped.DataCon   (dataConInstArgTys)
import           Clash.Core.Scoped.FreeVars  (freeTyVars, freeTyVarsOfType)
import           Clash.Core.Scoped.HasType   (inferCoreTypeOf, piResultTy)
import           Clash.Core.Scoped.Alpha     (AlphaTerm (..), AlphaType (..))
import           Clash.Core.Scoped.NameMap
  (insertWithGlobalEnv, lookupGlobalEnv, nullNameSet, singletonNameMap)
import           Clash.Core.Scoped.Pretty    ()
import           Clash.Core.Scoped.Subst
import           Clash.Core.Scoped.Syntax
import           Clash.Core.Scoped.Term
  ( AbsBinder (..), collectArgs, collectArgsTicks, mkAbstraction, mkApps, mkLams, mkTicks
  , mkTyLams )
import           Clash.Core.Scoped.TermInfo  (isLocalVar, isPolyFun, isVar)
import           Clash.Core.Scoped.Type
  (TypeView (..), applyFunTy, coreView1, normalizeType, splitTyConAppM, tyView)
import           Clash.Core.Scoped.TypeEnv   (TypeEnv, lookupTyCon)
import           Clash.Core.Scoped.Util      (mkLets)
import           Clash.Normalize.Scoped.Util (ConstantSpecInfo (..), constantSpecInfo)
import           Clash.Rewrite.Scoped.Combinators (topdownR)
import           Clash.Rewrite.Scoped.Globals
  (findAlphaEqBinding, isTopEntity, isWorkFreeTerm, lookupBinding, mkFunction)
import           Clash.Rewrite.Scoped.Types
  (ScopedTransform (..), TransformContext, mkDerivedName, tfScope)
import           Clash.Rewrite.Scoped.Util
  ( abstractOver, changed, dependencySortTyVars, isUntranslatableType, mkTmBinderFor
  , normalizeTermTypes, setChanged )
import           Clash.Rewrite.Scoped.Whnf   (whnfRW)

-- | Propagate arguments of application inwards; except for 'Lam' where the
-- argument becomes let-bound (or is substituted, if it is a variable or
-- work-free). 'appProp' tries to propagate as many arguments as possible, down
-- as many levels as possible; and should be called in a top-down traversal.
-- Arguments propagated into a case-expression are let-bound first, unless
-- they are variables or work-free, so that they aren't duplicated in every
-- alternative.
--
-- The idea is that this reduces the number of traversals, which hopefully
-- leads to shorter compile times.
--
-- The arguments are only moved under a binder (of a lambda, a let-expression
-- or a pattern) after it is opened, which renames it if it clashes with the
-- scope of the arguments, so the binder can't capture a free variable of an
-- argument. That holds for the binders of the function part, and for those of
-- arguments that end up as the function part after substitution alike.
appProp :: Transformation
appProp = toScopedTransformation "applicationPropagation"
  (onAppNode 'appPropWorker <> onTyAppNode 'appPropWorker)

-- | The application-spine handler of 'appProp'. Matches the constructors it
-- is registered for itself, so it can also be used as a plain uninstrumented
-- rewrite, as 'nonRepSpec' does.
appPropWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> NormalizeSession (Term n)
appPropWorker ctx node = case node of
  App {} -> propagate
  TyApp {} -> propagate
  _ -> return node
 where
  propagate = do
    let (fun, args, ticks) = collectArgsTicks node
    (eN, hasChanged) <- Writer.listen (go (tfScope ctx) fun args ticks)
    if Monoid.getAny hasChanged
      then return eN
      else return node

  go
    :: Distinct m
    => Scope m
    -> Term m
    -> [Either (Term m) (Type m)]
    -> [TickInfo m]
    -> NormalizeSession (Term m)
  go sc (collectArgsTicks -> (fun, args0@(_:_), ticks0)) args1 ticks1 =
    go sc fun (args0 ++ args1) (ticks0 ++ ticks1)

  go sc (Lam b e) (Left arg : args) ticks = do
    setChanged
    isVarOrWorkFree arg >>= \case
      True ->
        (`mkTicks` ticks) <$> go sc (substTm (extendTmSubst (idSubst sc) b arg) e) args []
      False ->
        openIdBinder sc b e $ \sc1 b1 e1 ->
          Let (NonRec b1 arg) <$> go sc1 e1 (sinkArgs args) (map sink ticks)

  go sc (Let bind e) args@(_:_) ticks = do
    setChanged
    openBind sc bind e $ \sc1 bind1 e1 ->
      Let bind1 <$> go sc1 e1 (sinkArgs args) (map sink ticks)

  go sc (TyLam b e) (Right t : args) ticks = do
    setChanged
    (`mkTicks` ticks) <$> go sc (substTm (extendTySubst (idSubst sc) b t) e) args []

  go sc (Case scrut ty0 alts) args0@(_:_) ticks = do
    setChanged
    te <- Lens.view typeEnv
    letBindCaseArgs te sc ty0 args0 $ \sc1 wrapLets ty1 args1 -> do
      alts1 <- mapM (goAlt sc1 args1) (map sink alts)
      pure (wrapLets (mkTicks (Case (sink scrut) ty1 alts1) (map sink ticks)))

  go sc (Tick sp e) args ticks = do
    setChanged
    go sc e args (sp : ticks)

  go _ fun args ticks = return (mkApps (mkTicks fun ticks) args)

  goAlt
    :: Distinct m
    => Scope m
    -> [Either (Term m) (Type m)]
    -> Alt m
    -> NormalizeSession (Alt m)
  goAlt sc args alt =
    openAlt sc alt $ \sc1 p e -> Alt p <$> go sc1 e (sinkArgs args) []

  -- Let-bind the arguments that aren't variables or work free, so that they
  -- aren't duplicated in every alternative. Also gives the type of the
  -- case-expression applied to the arguments. The arguments are bound in
  -- order, with non-recursive let-bindings.
  letBindCaseArgs
    :: Distinct m
    => TypeEnv
    -> Scope m
    -> Type m
    -> [Either (Term m) (Type m)]
    -> (forall l. DExt m l => Scope l -> (Term l -> Term m) -> Type l -> [Either (Term l) (Type l)] -> NormalizeSession r)
    -> NormalizeSession r
  letBindCaseArgs te sc ty args0 k = case args0 of
    [] -> k sc id ty []
    Right t : args ->
      letBindCaseArgs te sc (piResultTy te sc ty t) args $ \sc1 wrapLets ty1 args1 ->
        k sc1 wrapLets ty1 (Right (sink t) : args1)
    Left arg : args -> do
      let ty1 = applyFunTy te sc ty (inferCoreTypeOf te sc arg)
      isVarOrWorkFree arg >>= \case
        True ->
          letBindCaseArgs te sc ty1 args $ \sc1 wrapLets ty2 args1 ->
            k sc1 wrapLets ty2 (Left (sink arg) : args1)
        False ->
          mkTmBinderFor te sc (mkDerivedName ctx "app_arg") arg $ \sc1 b ->
            letBindCaseArgs te sc1 (sink ty1) (sinkArgs args) $ \sc2 wrapLets ty2 args1 ->
              k sc2 (Let (NonRec b arg) . wrapLets) ty2 (Left (sink (idBinderVar b)) : args1)

  isVarOrWorkFree :: Term m -> NormalizeSession Bool
  isVarOrWorkFree arg
    | isVar arg = pure True
    | otherwise = isWorkFreeTerm arg
{-# SCC appPropWorker #-}

-- | Move the arguments of an application into an extension of their scope
sinkArgs :: DExt n l => [Either (Term n) (Type n)] -> [Either (Term l) (Type l)]
sinkArgs = map (bimap sink sink)

-- | Specialize functions on arguments which are constant, except when they
-- are clock, reset generators.
constantSpec :: Transformation
constantSpec = toScopedTransformation "constantSpec" (onApp 'constantSpecWorker)

-- | The 'App' handler of 'constantSpec'
constantSpecWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Term n
  -> NormalizeSession (Term n)
constantSpecWorker ctx e e1 e2
  | (hd, args) <- collectArgs e1
  , isVar hd
  , null (rights args)
  , nullNameSet (freeTyVars e2)
  = constantSpecInfo ctx e2 >>= \case
      -- e2 has no constant parts
      ConstantSpecInfo _ _ _ False -> return e
      -- Whole of e2 is constant, we reduce it here eagerly before
      -- specialization because we have observed long running compile times
      -- when having recursively defined constants, see:
      -- https://github.com/clash-lang/clash-compiler/issues/3129
      ConstantSpecInfo _ TNil _ True -> do
        e2Red <- case collectArgs e2 of
          (Prim p0, _) -> whnfRW False ctx e2 $ \_ctx1 e2Red -> case collectArgs e2Red of
            (Prim p1, _) | primName p0 == primName p1 -> return (sink e2)
            _ -> changed e2Red
          _ -> return e2
        specialize ctx (App e1 e2Red)
      -- Parts of e2 are constant: specialize under let-bindings of the
      -- other parts
      ConstantSpecInfo sc1 binds newTerm True -> do
        (body, isSpec) <- Writer.listen (specializeIn sc1 (App (sink e1) newTerm))
        if Monoid.getAny isSpec
          then changed (mkLets binds body)
          else return e
  | otherwise = return e
{-# SCC constantSpecWorker #-}

-- | Specialize an application on its argument
specialize
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> NormalizeSession (Term n)
specialize ctx = specializeIn (tfScope ctx)

-- | 'specialize' in a scope: specialization only needs the scope of the
-- context, e.g. to specialize under new let-bindings ('constantSpec')
specializeIn
  :: (HasCallStack, Distinct n)
  => Scope n
  -> Term n
  -> NormalizeSession (Term n)
specializeIn sc e = case e of
  TyApp e1 ty -> specializeArgIn sc e (collectArgsTicks e1) (Right ty)
  App e1 e2 -> specializeArgIn sc e (collectArgsTicks e1) (Left e2)
  _ -> return e

-- | Specialize an application on its argument, given the function part of
-- the application split into its root, applied arguments and ticks, in a
-- scope, see 'specializeIn'
specializeArgIn
  :: (HasCallStack, Distinct n)
  => Scope n
  -> Term n
  -> (Term n, [Either (Term n) (Type n)], [TickInfo n])
  -> Either (Term n) (Type n)
  -> NormalizeSession (Term n)
specializeArgIn sc e (fHd, args, ticks) specArgIn = case fHd of
  -- Only global functions have a binding to specialize
  Var {} -> return e
  GlobalVar f -> specializeGlobal sc e f args ticks specArgIn
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
            mkFunction (C.appendToName (globalIdName cf) "_specF") sp NoUserInlinePrag newBody
          Just g -> pure g
        -- Create specialized argument
        let newArg = Left (mkApps (GlobalVar newf) (specVarArgs specTvs specIds))
        -- Use specialized argument
        changed (mkApps (mkTicks fHd ticks) (args ++ [newArg]))
    | otherwise -> return e

-- | 'specializeArgIn' for an application of a global function
specializeGlobal
  :: (HasCallStack, Distinct n)
  => Scope n
  -> Term n
  -> GlobalId
  -> [Either (Term n) (Type n)]
  -> [TickInfo n]
  -> Either (Term n) (Type n)
  -> NormalizeSession (Term n)
specializeGlobal sc e f args ticks specArgIn = do
  opts <- Lens.view debugOpts
  te <- Lens.view typeEnv
  topEnt <- isTopEntity f
  if topEnt then
    -- Don't specialize TopEntities
    case specArgIn of
      Left _ -> do
        traceM ("Not specializing TopEntity: " ++ showPpr (globalIdName f))
        return e
      Right tyArg
        -- TopEntities aren't allowed to be semantically polymorphic. But using
        -- type equality constraints they may be syntactically polymorphic.
        -- > topEntity :: forall dom . (dom ~ "System") => Signal dom Bool -> Signal dom Bool
        -- The TyLam's in the body will have been removed by
        -- 'Clash.Normalize.Scoped.Util.substWithTyEq'. So we drop the TyApp
        -- ("specializing" on it) and change the type of the variable to match.
        -- A global has a closed type, so this needs a closed type argument.
        | Just newTy <- closeType (piResultTy te sc (sinkClosed (globalIdType f)) tyArg) ->
            traceIf (hasTransformationInfo AppliedTerm opts)
              ("Dropping type application on TopEntity: " ++ showPpr (globalIdName f)
                 ++ "\ntype:\n" ++ showPpr tyArg) $
              changed (mkApps (mkTicks (GlobalVar (mkGlobalId (globalIdName f) newTy)) ticks) args)
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
        -- specialization]
        -- The key of the specialisation cache: closed, as 'abstractOver'
        -- abstracts the term over its free variables. A type with free type
        -- variables isn't cached.
        specAbs :: Maybe (Either AlphaTerm AlphaType)
        specAbs = case specArg of
          Left tm ->
            Left . AlphaTerm <$>
              closeTerm (scopedSpecCacheKey (abstractOver "specialize" specTvs specIdBndrs tm))
          Right ty -> Right . AlphaType <$> closeType ty
        showSpecAbs = either (\(AlphaTerm t) -> showPpr t) (\(AlphaType t) -> showPpr t)
    -- Determine if 'f' has already been specialized on (a type-normalized)
    -- 'specArg'
    specM <- case specAbs of
      Just k -> Map.lookup (f, argLen, k) <$> Lens.use (extra . specialisationCache)
      Nothing -> pure Nothing
    case specM of
      -- Use previously specialized function
      Just f' ->
        traceIf (hasTransformationInfo AppliedTerm opts)
          ("Using previous specialization of " ++ showPpr (globalIdName f) ++ " on " ++
            maybe "" showSpecAbs specAbs ++ ": " ++ showPpr (globalIdName f')) $
          changed (mkApps (mkTicks (GlobalVar f') ticks) (args ++ specVars))
      -- Create new specialized function
      Nothing -> lookupBinding f >>= \case
        -- Determine if we can specialize f
        Nothing -> return e
        Just (Binding _ sp inl _ bodyTm _) -> do
          -- Determine if we see a sequence of specializations on a growing
          -- argument
          specHistM <- lookupGlobalEnv f <$> Lens.use (extra . specialisationHistory)
          specLim <- Lens.view specializationLimit
          if maybe False (> specLim) specHistM
            then throw (ClashException
                        sp
                        (unlines [ "Hit specialization limit " ++ show specLim ++ " on function `" ++ showPpr (globalIdName f) ++ "'.\n"
                                 , "The function `" ++ showPpr f ++ "' is most likely recursive, and looks like it is being indefinitely specialized on a growing argument.\n"
                                 , "Body of `" ++ showPpr f ++ "':\n" ++ showPpr bodyTm ++ "\n"
                                 , "Argument (in position: " ++ show argLen ++ ") that triggered termination:\n" ++ either showPpr showPpr specArg
                                 , "Run with '-fclash-spec-limit=N' to increase the specialization limit to N."
                                 ])
                        Nothing)
            else do
              -- Determine name the resulting specialized function, and the
              -- form of the specialized-on argument.
              --
              -- In case we are specializing on an argument that is a global
              -- function then we use that function's name as part of the name
              -- of the specialized higher-order function, see
              -- 'specializeName'. Additionally, we will return the body of the
              -- global function, instead of a variable reference to the
              -- global function.
              --
              -- This will turn things like @mealy g@ into a new binding where
              -- both the body of @mealy@ and @g@ are inlined, meaning the
              -- state-transition-function and the memory element will be in a
              -- single function.
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
                        ( specializeName (inl, globalIdName f) (bindingSpec <$> gTmM, gNm)
                        , preferNoInline inl (maybe NoUserInlinePrag bindingSpec gTmM)
                        , maybe specArg (\b -> Left (mkApps (sinkClosed b) gArgs)) gBody
                        )
                _ -> return (globalIdName f, inl, specArg)
              -- Create the specialized function
              let names = collectBndrsMinusApps bodyTm ++
                    [ C.mkUnsafeInternalName ("pTS" `Text.append` Text.pack (show n)) n
                    | n <- [(0::Unique)..] ]
              case specBody te (sinkClosed (globalIdType f)) names args specTvs specIdBndrs bodyTm specArg' of
                -- The type of 'f' doesn't have the parameters for the
                -- arguments
                Nothing -> return e
                Just newBody -> do
                  newf <- mkFunction newName sp inl' newBody
                  -- Remember specialization
                  (extra . specialisationHistory) %= insertWithGlobalEnv (+) f 1
                  Monad.forM_ specAbs $ \k ->
                    (extra . specialisationCache) %= Map.insert (f, argLen, k) newf
                  -- Use specialized function
                  changed (mkApps (mkTicks (GlobalVar newf) ticks) (args ++ specVars))
 where
  varName :: Term x -> Maybe (C.Name Term)
  varName = \case
    Var occ _ _ -> Just occ
    GlobalVar g -> Just (globalIdName g)
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
specialization cache, see 'scopedSpecCacheKey'.

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
(see 'Clash.Normalize.Scoped.Util.normalizeWorkFreeApp'). To prevent creating
two identical specializations, we use the literal form in the keys of the
specialization cache.
-}

-- | Prepare a term for use as a key in the specialization cache: strip all
-- ticks (see Note [ticks and specialization]) and replace small bignum
-- constants by literals (see Note [bignum literals and specialization]).
scopedSpecCacheKey :: Term n -> Term n
scopedSpecCacheKey = go
 where
  go :: Term x -> Term x
  go = \case
    Lam b x -> Lam b (go x)
    TyLam b x -> TyLam b (go x)
    App f x -> case (go f, go x) of
      (Prim p, Literal (WordLiteral w))
        | primName p == Text.showt 'NS -> Literal (NaturalLiteral w)
      (Prim p, Literal (IntLiteral i))
        | primName p == Text.showt 'IS -> Literal (IntegerLiteral i)
      (f', x') -> App f' x'
    TyApp f a -> TyApp (go f) a
    Let (NonRec b x) e -> Let (NonRec b (go x)) (go e)
    Let (Rec bs xs) e -> Let (Rec bs (map go xs)) (go e)
    Case x ty alts -> Case (go x) ty [Alt p (go e) | Alt p e <- alts]
    Cast x a b -> Cast (go x) a b
    Tick _ x -> go x
    x -> x

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
-- depends on their NOINLINE / OPAQUE annotations:
--
-- >  OPAQUE    | Name for f g
-- > -----------|---------------
-- >  f and g   | f_g
-- >  f         | f
-- >  g         | g
-- >  !f and !g | f_g
specializeName
  :: (InlineSpec, C.Name Term) -> (Maybe InlineSpec, C.Name Term) -> C.Name Term
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

-- | Specialize functions on their non-representable argument
nonRepSpec :: Transformation
nonRepSpec = toScopedTransformation "nonRepSpec" (onApp 'nonRepSpecWorker)

-- | The 'App' handler of 'nonRepSpec'
nonRepSpecWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Term n
  -> NormalizeSession (Term n)
nonRepSpecWorker ctx e e1 e2
  | (hd, args) <- collectArgs e1
  , isVar hd
  , null (rights args)
  , nullNameSet (freeTyVars e2)
  = do te <- Lens.view typeEnv
       nonRepE2 <- isUntranslatableType False (tfScope ctx) (inferCoreTypeOf te (tfScope ctx) e2)
       if nonRepE2 && not (isLocalVar e2)
         then do
           e2' <- inlineInternalSpecialisationArgument e2
           specialize ctx (App e1 e2')
         else return e
  | otherwise = return e
 where
  -- If the argument on which we're specializing is an internal function, one
  -- created by the compiler, then inline that function before we specialize.
  --
  -- We need to do this because otherwise the specialization history won't
  -- recognize the new specialization argument as something the function has
  -- already been specialized on
  inlineInternalSpecialisationArgument app = case collectArgsTicks app of
    (GlobalVar f, fArgs, ticks) -> lookupBinding f >>= \case
      Just b
        | C.nameSort (globalIdName (bindingId b)) == C.Internal
        -> censor (const mempty)
             (runScopedTransform (topdownR (ScopedTransform appPropWorker)) ctx
               (mkApps (mkTicks (sinkClosed (bindingTerm b)) ticks) fArgs))
      _ -> return app
    _ -> return app
{-# SCC nonRepSpecWorker #-}

-- | Specialize functions on their type
typeSpec :: Transformation
typeSpec = toScopedTransformation "typeSpec" (onTyApp 'typeSpecWorker)

-- | The 'TyApp' handler of 'typeSpec'
typeSpecWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Type n
  -> NormalizeSession (Term n)
typeSpecWorker ctx e e1 ty
  | (hd, args) <- collectArgs e1
  , isVar hd
  , nullNameSet (freeTyVarsOfType ty)
  , null (rights args)
  = specialize ctx e
  | otherwise = return e
{-# SCC typeSpecWorker #-}

-- | Specialize functions on arguments which are zero-width. These arguments
-- can have only one possible value, and specializing on this value may create
-- additional opportunities for transformations to fire.
--
-- As we can't remove zero-width arguments (as transformations cannot change
-- the type of a term), we instead substitute all occurrences of a lambda-bound
-- variable with a zero-width type with the only value of that type.
zeroWidthSpec :: Transformation
zeroWidthSpec = toScopedTransformation "zeroWidthSpec" (onLam 'zeroWidthSpecWorker)

-- | The 'Lam' handler of 'zeroWidthSpec'
zeroWidthSpecWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> IdBinder n l
  -> Term l
  -> NormalizeSession (Term n)
zeroWidthSpecWorker ctx e b body = do
  te <- Lens.view typeEnv
  let sc = tfScope ctx
  case zeroWidthTypeElem te sc (normalizeType te sc (idBinderType b)) of
    Just tm ->
      openIdBinder sc b body $ \sc1 b1 body1 ->
        let s = tmVarsSubst sc1 (singletonNameMap (tmNameOf b1) (sink tm))
        in  changed (Lam b1 (substTm s body1))
    Nothing ->
      return e
{-# SCC zeroWidthSpecWorker #-}

-- | The only element of a type, if it is zero-width
zeroWidthTypeElem :: Distinct n => TypeEnv -> Scope n -> Type n -> Maybe (Term n)
zeroWidthTypeElem te sc = zeroWidthTypeElemIn te sc []

-- | 'zeroWidthTypeElem' for a type that is the type of a field of types with
-- the given type constructors. We do not look into the fields of a type with
-- a type constructor we are already looking into: recursive types, such as
-- 'Clash.Signal.Internal.Signal', would make us loop.
zeroWidthTypeElemIn
  :: Distinct n => TypeEnv -> Scope n -> [C.TyConName] -> Type n -> Maybe (Term n)
zeroWidthTypeElemIn te sc enclosing ty = do
  (tcNm, args) <- splitTyConAppM ty

  if | C.nameOcc tcNm == Text.showt ''BV.BitVector
     , [LitTy (NumTy 0)] <- args
     -> return (zeroWidthNum (bitVectorFromIntegerPrim tcNm) args
                  [NaturalLiteral 0, NaturalLiteral 0, IntegerLiteral 0])

     | C.nameOcc tcNm == Text.showt ''I.Index
     , [LitTy (NumTy 1)] <- args
     -> return (zeroWidthNum (sizedNumFromIntegerPrim tcNm (Text.showt 'I.fromInteger#)) args
                  [NaturalLiteral 1, IntegerLiteral 0])

     | C.nameOcc tcNm == Text.showt ''S.Signed
     , [LitTy (NumTy 0)] <- args
     -> return (zeroWidthNum (sizedNumFromIntegerPrim tcNm (Text.showt 'S.fromInteger#)) args
                  [NaturalLiteral 0, IntegerLiteral 0])

     | C.nameOcc tcNm == Text.showt ''U.Unsigned
     , [LitTy (NumTy 0)] <- args
     -> return (zeroWidthNum (sizedNumFromIntegerPrim tcNm (Text.showt 'U.fromInteger#)) args
                  [NaturalLiteral 0, IntegerLiteral 0])

     -- Any other zero-width type should only have a single data constructor
     -- where all fields are also zero-width. We don't handle constructors with
     -- existential type variables, as we don't know which types to apply them
     -- to.
     | otherwise
     -> do
       Monad.guard (tcNm `notElem` enclosing)
       tc <- lookupTyCon te tcNm

       case C.tyConDataCons tc of
         [dc] | dcExtTyVarCount dc == 0 -> do
           argTys <- dataConInstArgTys te sc dc args
           zwArgs <- traverse (zeroWidthTypeElemIn te sc (tcNm : enclosing)) argTys
           return (mkApps (Data dc) (fmap Right args <> fmap Left zwArgs))

         _ ->
           Nothing
 where
  zeroWidthNum pr tyArgs lits =
    mkApps (Prim pr) (fmap Right tyArgs <> fmap (Left . Literal) lits)

-- | @BitVector.fromInteger# :: forall n. Natural -> Natural -> Integer ->
-- BitVector n@, for the given type constructor of 'BV.BitVector'
bitVectorFromIntegerPrim :: C.TyConName -> PrimInfo
bitVectorFromIntegerPrim tcNm =
  zeroWidthPrim (Text.showt 'BV.fromInteger#) tcNm
    [tyConAppB naturalPrimTyConName [], tyConAppB naturalPrimTyConName [], tyConAppB integerPrimTyConName []]

-- | The primitive with the given name, of type @forall n. Natural -> Integer
-- -> T n@, for the given type constructor @T@: @fromInteger#@ of 'I.Index',
-- 'S.Signed' or 'U.Unsigned'
sizedNumFromIntegerPrim :: C.TyConName -> Text.Text -> PrimInfo
sizedNumFromIntegerPrim tcNm nm =
  zeroWidthPrim nm tcNm [tyConAppB naturalPrimTyConName [], tyConAppB integerPrimTyConName []]

-- | A primitive of 'zeroWidthSpec': its name, and its type @forall n. args ->
-- T n@, given @T@ and the argument types
zeroWidthPrim :: Text.Text -> C.TyConName -> [TyB] -> PrimInfo
zeroWidthPrim nm tcNm args =
  mkPrimInfo nm (runClosedType prTy) WorkNever SingleResult NoUnfolding
 where
  prTy =
    forAllTyB 0 (C.mkUnsafeSystemName "n" 0) (tyConAppB typeNatKindTyConName [])
      (funTyB args (tyConAppB tcNm [varTyB 0]))

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
  -> [C.Name Term]
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
-- have parameters for all arguments.
--
-- The binders get the types of the parameters of the function, rather than
-- those of the arguments, as the types of the arguments can mention type
-- variables bound at the call site.
withArgBinders
  :: (HasCallStack, Distinct x)
  => TypeEnv
  -> Scope x
  -> Type x
  -- ^ The type of the function, applied to the arguments before
  -> [C.Name Term]
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

-- | The names of the binders of the leading lambdas of a term, minus the
-- binders of the lambdas that the term applies to an argument. The binders of
-- the arguments of a specialized function get these names.
collectBndrsMinusApps :: Term n -> [C.Name Term]
collectBndrsMinusApps = reverse . go []
 where
  go :: [C.Name Term] -> Term x -> [C.Name Term]
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

{- Note [Collect free-variables in an insertion-ordered set]
In order for the specialization cache to work, 'specArgBndrsAndVars' should
yield (alpha equivalent) results for the same specialization. While collecting
free variables in a given term or type it should therefore keep a stable
ordering based on the order in which it finds free vars. To see why,
consider the following two pseudo-code calls to 'specialize':

    specialize {f ('a', x[123], y[456])}
    specialize {f ('b', x[456], y[123])}

Collecting the binders in a set ordered on their uniques would yield the
following sets:

    {x[123], y[456]}
    {y[123], x[456]}

..and therefore breaking specializing caching. So we collect them in the order
of their first occurrence instead, yielding:

    {x[123], y[456]}
    {x[456], y[123]}
-}

-- | The free variables of the argument to specialize on, to abstract over:
-- the type variables in the order of their first occurrence, but every type
-- variable after the type variables in its kind; and the term variables in
-- the order of their first occurrence. See Note [Collect free-variables in an
-- insertion-ordered set].
--
-- This gives the occurrences of the variables, of which
-- 'Clash.Core.Scoped.Subst.closeOver' makes the binders. The free type
-- variables of a type are in the order of their first occurrence too.
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
