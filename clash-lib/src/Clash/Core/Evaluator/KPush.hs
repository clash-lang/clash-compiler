{-|
  Copyright   :  (C) 2006, The University of Glasgow
                     2020,2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  KPush as described in:

  Stephanie Weirich, Justin Hsu, Richard A. Eisenberg,
  "System FC with Explicit Kind Equality",
  ICFP '13, Boston, Massachusetts, USA.

  and mostly derived from GHC's @pushCoDataCon@:

  https://github.com/ghc/ghc/blob/0c5ed5c7eb30bc5462b67ff097c3388597265a4b/compiler/GHC/Core/SimpleOpt.hs#L1472-L1532

  KPush moves a cast on a data-constructor application into the arguments of
  that data constructor:

  > (K exTys args) ▷ (T tys ~ T tys')  ⇒  K exTys' args'

  so that a case-decomposition can match on the constructor directly.
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
import Clash.Core.Util (castEqType)
import Clash.Core.Var (Var (..), TyVar)
import Clash.Core.VarEnv
  (VarEnv, emptyInScopeSet, extendVarEnv, mkInScopeSet, mapVarEnv, mkVarEnv,
   lookupVarEnv, uniqAway, unitVarSet)
import qualified Clash.Data.UniqMap as UniqMap

-- | A coercion between two types. Clash's core language does not have
-- evidence-carrying coercions; a cast is fully described by its source and
-- target type.
type Coercion = (Type, Type)

data LiftingContext = LC TvSubst LiftCoEnv

-- | Maps *type variables* to *coercions*.
-- That's the whole point of this function!
type LiftCoEnv = VarEnv Coercion

-- | Push a coercion on a data-constructor application into the arguments of
-- the data constructor. Returns 'Nothing' when the coercion cannot be
-- decomposed, e.g. when its target is a (stuck) type-family application:
--
-- > (K x y) ▷ (T a ~ S [a])
--
-- There is nothing wrong with such a term, but @kpush@ cannot simplify it.
kpush :: TyConMap -> DataCon -> [Either Term Type] -> Coercion -> Maybe [Either Term Type]
kpush tcm dc args (fromTy,toTy)
  | castEqType tcm fromTy toTy -- Refl
  = Just args

  | TyConApp toTc toTcArgTys <- tyView toTy
  , TyConApp fromTc _ <- tyView fromTy
  , fromTc == toTc
  , TyConApp dTc _ <- tyView (snd (splitFunForallTy (dcType dc)))
  , dTc == toTc
  = let nonUnivArgs      = List.dropList (dcUnivTyVars dc) args
        (exArgs,valArgs) = List.splitAtList (dcExtTyVars dc) nonUnivArgs

        tc = UniqMap.find toTc tcm

        omegas = decomposeCo (tyConArity tc) (fromTy,toTy)

        (phiSubst,toExArgs) =
          liftCoSubstWithEx
            (dcUnivTyVars dc)
            omegas
            (dcExtTyVars dc)
            (Either.fromRight (error "kpush: Not a Type") <$> exArgs)

        newValArgs =
          zipWith castArg
                  (dcArgTys dc)
                  (Either.fromLeft (error "kpush: Not a Term") <$> valArgs)

        castArg argTy arg = mkCast arg (phiSubst argTy)

        mkCast arg (ty1,ty2)
          | castEqType tcm ty1 ty2
          = arg

        mkCast (Cast arg ty1 _) (_,ty2)
          = Cast arg ty1 ty2

        mkCast (Tick t arg) co
          = Tick t (mkCast arg co)

        mkCast (TyApp p@(Prim (PrimInfo {primName = "_CO_"})) pTy) (lCo,rCo)
          -- (co :: s1 ~# t1) |> (s1 ~# t1) ~ (s2 ~# t2)  ::  (s2 ~# t2)
          | TyConApp lCoTcNm@(nameOcc -> "GHC.Prim.~#") _ <- tyView lCo
          , let tcCo = UniqMap.find lCoTcNm tcm
          , let coList = decomposeCo (tyConArity tcCo) (lCo,rCo)
          = case coList of
              (_g2:_g1:_) -> TyApp p rCo
              _ -> error ("kpush.mkCoCast" <> unlines [showPpr pTy, showPpr lCo, showPpr rCo])

        mkCast arg (ty1,ty2)
          -- TODO: error out when `termType arg /= ty1`
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
  -- | Universally quantified tyvars
  [TyVar] ->
  -- | Coercions
  [Coercion] ->
  -- | Existentially quantified tycovars
  [TyVar] ->
  -- | Types and coercions bound to the ex vars
  [Type] ->
  -- | (Lifting function, converted ex args)
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
  -- | Arity
  Int ->
  -- | Coercion
  Coercion ->
  -- | Decomposed coercions
  [Coercion]
decomposeCo n (tyView -> TyConApp _ args1,tyView -> TyConApp _ args2) =
  go n args1 args2
  where
  go 0 _ _ = []
  go m (t1:rest1) (t2:rest2) = (t1,t2):go (m-1) rest1 rest2
  go _ _ _ = error "decomposeCo: unequal length"

decomposeCo _ (ty1,ty2) =
  error (unlines ["decomposeCo: Expected TyConApp:"
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
      ConstTy {} -> error "tyCoSubst: impossible"
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
  = Just coArg

  | otherwise
  = let coTy = substTyVar subst v
    in  Just (coTy,coTy) -- Refl

{- Note [liftCoSubstVarBndr]

callback:
  We want 'liftCoSubstVarBndrUsing' to be general enough to be reused in
  FamInstEnv, therefore the input arg 'fun' returns a pair with polymorphic
  type in snd.
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
-}
liftCoSubstVarBndr ::
  LiftingContext ->
  TyVar ->
  (LiftingContext, TyVar, Coercion)
liftCoSubstVarBndr lc tv =
  let (lcN, tvN, h, _) = liftCoSubstVarBndrUsing callback lc tv
  in  (lcN, tvN, h)
  where
  callback lcN tyN = (tyCoSubst lcN tyN, ())

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
  newVar = uniqAway (getTvInScope subst) (oldVar { varType = ki })
  lifted = mkGReflRightCo (VarTy newVar) eta
  newEnv = extendVarEnv oldVar lifted env

-- | Make a Coercion from a tycovar, a kind coercion, and a body coercion.
-- The kind of the tycovar should be the left-hand kind of the kind coercion.
mkForallCo :: TyVar -> Coercion -> Coercion -> Coercion
mkForallCo tvN (kL,kR) (bodyL,bodyR)
  | kL == kR
  = (ForAllTy tvN bodyL, ForAllTy tvN bodyR)
  | otherwise
  = error (unlines ["mkForallCo: Kind coercions not supported",showPpr kL,showPpr kR])

-- | Extend a lifting context with existential-variable bindings.
extendLiftingContextEx ::
  -- | Original lifting context
  LiftingContext ->
  -- | Ex. var / value pairs
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
  = error (unlines ["mkGReflRightCo: Kind coercions not supported:",showPpr l, showPpr r])
