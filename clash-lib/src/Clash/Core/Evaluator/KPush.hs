{-|
  Copyright   :  (C) 2006, The University of Glasgow
                     2020, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  KPush as described in:

  Stephanie Weirich, Justin Hsu, Richard A. Eisenberg,
  "System FC with Explicit Kind Equality",
  ICFP '13, Boston, Massachusetts, USA.

  and mostly copied from:

  https://github.com/ghc/ghc/blob/0c5ed5c7eb30bc5462b67ff097c3388597265a4b/compiler/GHC/Core/SimpleOpt.hs#L1472-L1532
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Core.Evaluator.KPush
  ( kpush
  )
where

import qualified Control.Lens as Lens
import qualified Data.Either as Either
import qualified Data.List.Extra as List

import Clash.Core.DataCon (DataCon (..))
import Clash.Core.FreeVars (typeFreeVars)
import Clash.Core.Name (nameOcc)
import Clash.Core.Pretty (showPpr)
import Clash.Core.Subst
  (TvSubst (..), composeTvSubst, extendTvInScope, extendTvInScopeSet,
   getTvInScope, mkEmptyTvSubst, substTyTvSubst, substTyVar)
import Clash.Core.Term (PrimInfo (..), Term (..))
import Clash.Core.TyCon (TyCon (tyConArity), TyConMap)
import Clash.Core.Type
  (Type (..), TypeView (..), mkFunTy, mkTyConApp, splitFunForallTy, tyView)
import Clash.Core.Var (Var (..), TyVar, setVarType)
import Clash.Core.VarEnv
  (VarEnv, emptyInScopeSet, extendVarEnv, mkInScopeSet, mapVarEnv, mkVarEnv,
   lookupVarEnv, uniqAway, unitVarSet)
import Clash.Unique (lookupUniqMap')

type Coercion = (Type,Type)

data LiftingContext = LC TvSubst LiftCoEnv

-- | Maps *type variables* to *coercions*.
-- That's the whole point of this function!
type LiftCoEnv = VarEnv Coercion

kpush :: TyConMap -> DataCon -> [Either Term Type] -> Coercion -> Maybe [Either Term Type]
kpush tcm dc args (fromTy,toTy)
  | fromTy == toTy -- Refl
  = Just args

  | TyConApp toTc toTcArgTys <- tyView toTy
  , TyConApp dTc _ <- tyView (snd (splitFunForallTy (dcType dc)))
  , dTc == toTc
  -- These two tests can fail; we might see
  --      (C x y) `cast` (g :: T a ~ S [a]),
  -- where S is a type function.
  -- There's nothing wrong with that, but kpush should not be called in those
  -- situations.
  = let nonUnivArgs      = List.dropList (dcUnivTyVars dc) args
        (exArgs,valArgs) = List.splitAtList (dcExtTyVars dc) nonUnivArgs

        tc = tcm `lookupUniqMap'` toTc

        omegas = decomposeCo (tyConArity tc) (fromTy,toTy)

        (phiSubst,toExArgs) =
          liftCoSubstWithEx
            (dcUnivTyVars dc)
            omegas
            (dcExtTyVars dc)
            (Either.fromRight (error "Not a Type") <$> exArgs)

        newValArgs =
          zipWith castArg
                  (dcArgTys dc)
                  (Either.fromLeft (error "Not a Term") <$> valArgs)

        castArg argTy arg = mkCast arg (phiSubst argTy)
        mkCast arg (ty1,ty2)
          | ty1 == ty2
          = arg

        mkCast (Cast arg ty1 _) (_,ty2)
          = Cast arg ty1 ty2

        mkCast (Tick t arg) co
          = Tick t (mkCast arg co)

        mkCast (TyApp p@(Prim (PrimInfo {primName = "_CO_"})) pTy) (lCo,rCo)
          | TyConApp (nameOcc -> "GHC.Prim.~#") _ <- tyView pTy
          -- (co :: s1 ~# t1) |> (s1 ~# t1) ~ (s2 ~# t2)  ::  (s2 ~# t2)
          = case coList of
              (_g2:_g1:_) -> TyApp p rCo
              _ -> error ("mkCoCast" <> unlines [showPpr pTy, showPpr lCo, showPpr rCo])
          where
            TyConApp lCoTcNm _ = tyView lCo
            tcCo = tcm `lookupUniqMap'` lCoTcNm
            coList = decomposeCo (tyConArity tcCo) (lCo,rCo)

        mkCast arg (ty1,ty2)
          -- TODO: error out when `termType tcm arg /= ty1`
          = Cast arg ty1 ty2

    in  Just (map Right toTcArgTys ++ map Right toExArgs ++ map Left newValArgs)
  | otherwise
  = Nothing

-- | @liftCoSubst role lc ty@ produces a coercion (at role @role@)
-- that coerces between @lc_left(ty)@ and @lc_right(ty)@, where
-- @lc_left@ is a substitution mapping type variables to the left-hand
-- types of the mapped coercions in @lc@, and similar for @lc_right@.
--
-- like liftCoSubstWith, but allows for existentially-bound types as well
liftCoSubstWithEx ::
  -- Universally quantified tyvars
  [TyVar] ->
  -- Coercions
  [Coercion] ->
  -- Existentially quantified tycovars
  [TyVar] ->
  -- types and coercions bound to the ex vars
  [Type] ->
  -- (Lifting function, converted ex args)
  (Type -> Coercion, [Type])
liftCoSubstWithEx univs omegas exs rhos =
  let theta = mkLiftingContext (List.zipEqual univs omegas)
      psi   = extendLiftingContextEx theta (List.zipEqual exs rhos)
  in  (tyCoSubst psi, substTyTvSubst (lcSubstRight psi) <$> (VarTy <$> exs))

-- | This breaks a 'Coercion' with type @T A B C ~ T D E F@ into
-- a list of 'Coercion's of kinds @A ~ D@, @B ~ E@ and @E ~ F@. Hence:
--
-- > decomposeCo 3 c [r1, r2, r3] = [nth r1 0 c, nth r2 1 c, nth r3 2 c]
decomposeCo ::
  -- Arity
  Int ->
  -- Coercion
  Coercion ->
  -- decomposed coercions
  [Coercion]
decomposeCo n (tyView -> TyConApp _ args1,tyView -> TyConApp _ args2) =
  go n args1 args2
  where
  go 0 _ _ = []
  go m (t1:rest1) (t2:rest2) = (t1,t2):go (m-1) rest1 rest2
  go _ _ _ = error "unequal length"

decomposeCo _ (ty1,ty2) =
  error (unlines ["Expected TyConApp:"
                  ,showPpr ty1
                  ," ~ "
                  ,showPpr ty2])

mkLiftingContext :: [(TyVar,Coercion)] -> LiftingContext
mkLiftingContext pairs =
  LC (mkEmptyTvSubst (mkInScopeSet (Lens.foldMapOf (tupOf typeFreeVars)
                                                  unitVarSet
                                                  (map snd pairs))))
     (mkVarEnv pairs)
  where
  tupOf fld = \f -> traverse (\(a,b) -> (,) <$> fld f a <*> fld f b)

lcSubstRight :: LiftingContext -> TvSubst
lcSubstRight (LC subst lcEnv) =
  composeTvSubst (TvSubst emptyInScopeSet tEnv) subst
  where
  tEnv = mapVarEnv snd lcEnv

-- | The \"lifting\" operation which substitutes coercions for type
--   variables in a type to produce a coercion.
tyCoSubst :: LiftingContext -> Type -> Coercion
tyCoSubst !lc = go
  where
  go tyC = case tyView tyC of
    OtherType oTy -> case oTy of
      AnnType _ tyA -> go tyA
      VarTy tv -> case liftCoSubstTyVar lc tv of
        Nothing -> error "tyCoSubst bad roles"
        Just co -> co
      LitTy {} -> (oTy,oTy) -- Refl
      AppTy lTy rTy ->
        let (lTyL,lTyR) = go lTy
            (rTyL,rTyR) = go rTy
        in  (AppTy lTyL rTyL,AppTy lTyR rTyR)
      ForAllTy tv tyBody ->
        let (lcN, tvN, h) = liftCoSubstVarBndr lc tv
            bodyCo = tyCoSubst lcN tyBody
        in  mkForallCo tvN h bodyCo
      ConstTy {} -> error "impossible"
    FunTy lTy rTy ->
      let (lTyL,lTyR) = go lTy
          (rTyL,rTyR) = go rTy
      in  (mkFunTy lTyL rTyL,mkFunTy lTyR rTyR)
    TyConApp tcNm tyArgs ->
      let (lArgs,rArgs) = unzip (map go tyArgs)
      in  (mkTyConApp tcNm lArgs, mkTyConApp tcNm rArgs)

liftCoSubstTyVar :: LiftingContext -> TyVar -> Maybe Coercion
liftCoSubstTyVar (LC subst env) v
  | Just coArg <- lookupVarEnv v env
  = -- TODO:
    -- downgradeRoleMaybe role (coercionRole coArg) coArg
    Just coArg

  | otherwise
  = let coTy = substTyVar subst v
    in  Just (coTy,coTy) -- Refl

liftCoSubstVarBndr ::
  LiftingContext ->
  TyVar ->
  (LiftingContext, TyVar, Coercion)
liftCoSubstVarBndr lc tv =
  let (lcN, tvN, h, _) = liftCoSubstVarBndrUsing callback lc tv
  in  (lcN, tvN, h)
  where
  callback lcN tyN = (tyCoSubst lcN tyN, ())


{- |
Note [liftCoSubstVarBndr]

callback:
  We want 'liftCoSubstVarBndrUsing' to be general enough to be reused in
  FamInstEnv, therefore the input arg 'fun' returns a pair with polymorphic type
  in snd.
  However in 'liftCoSubstVarBndr', we don't need the snd, so we use unit and
  ignore the fourth component of the return value.
liftCoSubstTyVarBndrUsing:
  Given
    forall tv:k. t
  We want to get
    forall (tv:k1) (kind_co :: k1 ~ k2) body_co
  We lift the kind k to get the kind_co
    kind_co = ty_co_subst k :: k1 ~ k2
  Now in the LiftingContext, we add the new mapping
    tv |-> (tv :: k1) ~ ((tv |> kind_co) :: k2)
liftCoSubstCoVarBndrUsing:
  Given
    forall cv:(s1 ~ s2). t
  We want to get
    forall (cv:s1'~s2') (kind_co :: (s1'~s2') ~ (t1 ~ t2)) body_co
  We lift s1 and s2 respectively to get
    eta1 :: s1' ~ t1
    eta2 :: s2' ~ t2
  And
    kind_co = TyConAppCo Nominal (~#) eta1 eta2
  Now in the liftingContext, we add the new mapping
    cv |-> (cv :: s1' ~ s2') ~ ((sym eta1;cv;eta2) :: t1 ~ t2)
-}
liftCoSubstVarBndrUsing ::
  (LiftingContext -> Type -> (Coercion, a)) ->
  -- ^ The callback, must produce a nominal coercion
  LiftingContext ->
  TyVar ->
  (LiftingContext, TyVar, Coercion, a)
liftCoSubstVarBndrUsing fun lc@(LC subst env) oldVar =
  ( LC (subst `extendTvInScope` newVar) newEnv
  , newVar, eta, stuff )
 where
  oldKind = varType oldVar
  (eta,stuff) = fun lc oldKind
  ki = fst eta
  newVar = uniqAway (getTvInScope subst) (setVarType oldVar ki)
  lifted = mkGReflRightCo (VarTy newVar) eta
  newEnv = extendVarEnv oldVar lifted env

{- | Make a Coercion from a tycovar, a kind coercion, and a body coercion.
The kind of the tycovar should be the left-hand kind of the kind coercion.
See Note [Unused coercion variable in ForAllCo]

Note [Unused coercion variable in ForAllCo]
See Note [Unused coercion variable in ForAllTy] in GHC.Core.TyCo.Rep for the
motivation for checking coercion variable in types.
To lift the design choice to (ForAllCo cv kind_co body_co), we have two options:
(1) In mkForAllCo, we check whether cv is a coercion variable
    and whether it is not used in body_co. If so we construct a FunCo.
(2) We don't do this check in mkForAllCo.
    In coercionKind, we use mkTyCoForAllTy to perform the check and construct
    a FunTy when necessary.
We chose (2) for two reasons:
* for a coercion, all that matters is its kind, So ForAllCo or FunCo does not
  make a difference.
* even if cv occurs in body_co, it is possible that cv does not occur in the kind
  of body_co. Therefore the check in coercionKind is inevitable.
The last wrinkle is that there are restrictions around the use of the cv in the
coercion, as described in Section 5.8.5.2 of Richard's thesis. The idea is that
we cannot prove that the type system is consistent with unrestricted use of this
cv; the consistency proof uses an untyped rewrite relation that works over types
with all coercions and casts removed. So, we can allow the cv to appear only in
positions that are erased. As an approximation of this (and keeping close to the
published theory), we currently allow the cv only within the type in a Refl node
and under a GRefl node (including in the Coercion stored in a GRefl). It's
possible other places are OK, too, but this is a safe approximation.
Sadly, with heterogeneous equality, this restriction might be able to be violated;
Richard's thesis is unable to prove that it isn't. Specifically, the liftCoSubst
function might create an invalid coercion. Because a violation of the
restriction might lead to a program that "goes wrong", it is checked all the time,
even in a production compiler and without -dcore-list. We *have* proved that the
problem does not occur with homogeneous equality, so this check can be dropped
once ~# is made to be homogeneous.
-}
mkForallCo :: TyVar -> Coercion -> Coercion -> Coercion
mkForallCo tvN (kL,kR) (bodyL,bodyR)
  | kL == kR
  = (ForAllTy tvN bodyL, ForAllTy tvN bodyR)
  | otherwise
  = error (unlines ["Kind coercions not supported",showPpr kL,showPpr kR])

-- | Extend a lifting context with existential-variable bindings.
-- See Note [extendLiftingContextEx]
extendLiftingContextEx ::
  -- Original lifting context
  LiftingContext ->
  -- ex. var / value pairs
  [(TyVar,Type)] ->
  LiftingContext
extendLiftingContextEx lc [] = lc
extendLiftingContextEx lc@(LC subst env) ((v,exTy):rest)
  = let lcN = LC (subst `extendTvInScopeSet`
                    (Lens.foldMapOf typeFreeVars unitVarSet exTy))
                 (extendVarEnv
                    v
                    (mkGReflRightCo exTy
                                    (tyCoSubst lc (varType v)))
                    env)
    in  extendLiftingContextEx lcN rest

-- | Given @ty :: k1@, @co :: k1 ~ k2@,
-- produces @co' :: ty ~r (ty |> co)@
mkGReflRightCo :: Type -> Coercion -> Coercion
mkGReflRightCo t (l,r)
  | l == r
  = (t,t) -- Refl
  | otherwise
  = error (unlines ["Kind coercions not supported:",showPpr l, showPpr r])
