{-|
  Copyright   :  (C) 2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Call-by-need evaluator based on the evaluator described in:

  Maximilian Bolingbroke, Simon Peyton Jones, "Supercompilation by evaluation",
  Haskell '10, Baltimore, Maryland, USA.

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.GHC.Evaluator where

import           Prelude                                 hiding (lookup)

import           Control.Concurrent.Supply               (Supply, freshId)
import           Data.Either                             (lefts,rights)
import           Data.List                               (foldl',mapAccumL)
import qualified Data.Primitive.ByteArray                as BA
import qualified Data.Text as Text
import           GHC.Integer.GMP.Internals
  (Integer (..), BigNat (..))

import           Clash.Core.DataCon
import           Clash.Core.Evaluator.Types
import           Clash.Core.FreeVars
import           Clash.Core.Literal
import           Clash.Core.Name
import           Clash.Core.Pretty
import           Clash.Core.Subst
import           Clash.Core.Term
import           Clash.Core.TermInfo
import           Clash.Core.TyCon
import           Clash.Core.Type
import           Clash.Core.Util
import           Clash.Core.Var
import           Clash.Core.VarEnv
import           Clash.Debug
import           Clash.Unique

import           Clash.GHC.Evaluator.Primitive

evaluator :: Evaluator
evaluator = Evaluator
  { step = ghcStep
  , unwind = ghcUnwind
  , primStep = ghcPrimStep
  , primUnwind = ghcPrimUnwind
  }

{- [Note: forcing special primitives]
Clash uses the `whnf` function in two places (for now):

  1. The case-of-known-constructor transformation
  2. The reduceConstant transformation

The first transformation is needed to reach the required normal form. The
second transformation is more of cleanup transformation, so non-essential.

Normally, `whnf` would force the evaluation of all primitives, which is needed
in the `case-of-known-constructor` transformation. However, there are some
primitives which we want to leave unevaluated in the `reduceConstant`
transformation. Such primitives are:

  - Primitives such as `Clash.Sized.Vector.transpose`, `Clash.Sized.Vector.map`,
    etc. that do not reduce to an expression in normal form. Where the
    `reduceConstant` transformation is supposed to be normal-form preserving.
  - Primitives such as `GHC.Int.I8#`, `GHC.Word.W32#`, etc. which seem like
    wrappers around a 64-bit literal, but actually perform truncation to the
    desired bit-size.

This is why the Primitive Evaluator gets a flag telling whether it should
evaluate these special primitives.
-}

stepVar :: Id -> Step
stepVar i m _
  | Just e <- heapLookup LocalId i m
  = go LocalId e

  | Just e <- heapLookup GlobalId i m
  , isGlobalId i
  = go GlobalId e

  | otherwise
  = Nothing
 where
  go s e =
    let term = deShadowTerm (mScopeNames m) (tickExpr e)
     in Just . setTerm term . stackPush (Update s i) $ heapDelete s i m

  -- Removing the heap-bound value on a force ensures we do not get stuck on
  -- expressions such as: "let x = x in x"
  tickExpr = Tick (NameMod PrefixName (LitTy . SymTy $ toStr i))
  unQualName = snd . Text.breakOnEnd "."
  toStr = Text.unpack . unQualName . flip Text.snoc '_' . nameOcc . varName

stepData :: DataCon -> Step
stepData dc = ghcUnwind (DC dc [])

stepLiteral :: Literal -> Step
stepLiteral l = ghcUnwind (Lit l)

stepPrim :: PrimInfo -> Step
stepPrim pInfo m tcm
  | primName pInfo == "GHC.Prim.realWorld#" =
      ghcUnwind (PrimVal pInfo [] []) m tcm

  | otherwise =
      case fst $ splitFunForallTy (primType pInfo) of
        []  -> ghcPrimStep tcm (forcePrims m) pInfo [] [] m
        tys -> newBinder tys (Prim pInfo) m tcm

stepLam :: Id -> Term -> Step
stepLam x e = ghcUnwind (Lambda x e)

stepTyLam :: TyVar -> Term -> Step
stepTyLam x e = ghcUnwind (TyLambda x e)

stepApp :: Term -> Term -> Step
stepApp x y m tcm =
  case term of
    Cast {} -> error "stepApp QQ"

    Data dc ->
      let tys = fst $ splitFunForallTy (dcType dc)
       in case compare (length args) (length tys) of
            EQ -> ghcUnwind (DC dc args) m tcm
            LT -> newBinder tys' (App x y) m tcm
            GT -> error "Overapplied DC"

    Prim p ->
      let tys = fst $ splitFunForallTy (primType p)
       in case compare (length args) (length tys) of
            EQ -> case lefts args of
              -- We make boolean conjunction and disjunction extra lazy by
              -- deferring the evaluation of the arguments during the evaluation
              -- of the primop rule.
              --
              -- This allows us to implement:
              --
              -- x && True  --> x
              -- x && False --> False
              -- x || True  --> True
              -- x || False --> x
              --
              -- even when that 'x' is _|_. This makes the evaluation
              -- rule lazier than the actual Haskel implementations which
              -- are strict in the first argument and lazy in the second.
              [a0, a1] | primName p `elem` ["GHC.Classes.&&","GHC.Classes.||"] ->
                    let (m0,i) = newLetBinding tcm m  a0
                        (m1,j) = newLetBinding tcm m0 a1
                    in  ghcPrimStep tcm (forcePrims m) p [] [Suspend (Var i), Suspend (Var j)] m1

              (e':es) ->
                Just . setTerm e' $ stackPush (PrimApply p (rights args) [] es) m

              _ -> error "internal error"

            LT -> newBinder tys' (App x y) m tcm

            GT -> let (m0, n) = newLetBinding tcm m y
                   in Just . setTerm x $ stackPush (Apply n) m0

    _ -> let (m0, n) = newLetBinding tcm m y
          in Just . setTerm x $ stackPush (Apply n) m0
 where
  (term, args, _) = collectArgsTicks (App x y)
  tys' = fst . splitFunForallTy . termType tcm $ App x y

stepTyApp :: Term -> Type -> Step
stepTyApp x ty m tcm =
  case term of
    Cast {} -> error "stepTyApp QQ"

    Data dc ->
      let tys = fst $ splitFunForallTy (dcType dc)
       in case compare (length args) (length tys) of
            EQ -> ghcUnwind (DC dc args) m tcm
            LT -> newBinder tys' (TyApp x ty) m tcm
            GT -> error "Overapplied DC"

    Prim p ->
      let tys = fst $ splitFunForallTy (primType p)
       in case compare (length args) (length tys) of
            EQ -> case lefts args of
                    [] | primName p `elem` [ "Clash.Transformations.removedArg"
                                           , "Clash.Transformations.undefined" ] ->
                            ghcUnwind (PrimVal p (rights args) []) m tcm

                       | otherwise ->
                            ghcPrimStep tcm (forcePrims m) p (rights args) [] m

                    (e':es) ->
                      Just . setTerm e' $ stackPush (PrimApply p (rights args) [] es) m

            LT -> newBinder tys' (TyApp x ty) m tcm
            GT -> Just . setTerm x $ stackPush (Instantiate ty) m

    _ -> Just . setTerm x $ stackPush (Instantiate ty) m
 where
  (term, args, _) = collectArgsTicks (TyApp x ty)
  tys' = fst . splitFunForallTy . termType tcm $ TyApp x ty

stepLetRec :: [LetBinding] -> Term -> Step
stepLetRec bs x m _ = Just (allocate bs x m)

stepCase :: Term -> Type -> [Alt] -> Step
stepCase (Cast {}) _ty _alts _m _ = error "stepCase QQ"
stepCase scrut ty alts m _ =
  Just . setTerm scrut $ stackPush (Scrutinise ty alts) m

-- TODO Support stepwise evaluation of casts.
--
stepCast :: Term -> Type -> Type -> Step
stepCast x ty1 ty2 m _ = Just . setTerm x $ stackPush (Castish ty1 ty2) m

stepTick :: TickInfo -> Term -> Step
stepTick tick x m _ =
  Just . setTerm x $ stackPush (Tickish tick) m

-- | Small-step operational semantics.
--
ghcStep :: Step
ghcStep m = case mTerm m of
  Var i -> stepVar i m
  Data dc -> stepData dc m
  Literal l -> stepLiteral l m
  Prim p -> stepPrim p m
  Lam v x -> stepLam v x m
  TyLam v x -> stepTyLam v x m
  App x y -> stepApp x y m
  TyApp x ty -> stepTyApp x ty m
  Letrec bs x -> stepLetRec bs x m
  Case s ty as -> stepCase s ty as m
  Cast x a b -> stepCast x a b m
  Tick t x -> stepTick t x m

-- | Take a list of types or type variables and create a lambda / type lambda
-- for each one around the given term.
--
newBinder :: [Either TyVar Type] -> Term -> Step
newBinder tys x m tcm =
  let (s', iss', x') = mkAbstr (mSupply m, mScopeNames m, x) tys
      m' = m { mSupply = s', mScopeNames = iss', mTerm = x' }
   in ghcStep m' tcm
 where
  mkAbstr = foldr go
    where
      go (Left tv) (s', iss', e') =
        (s', iss', TyLam tv (TyApp e' (VarTy tv)))

      go (Right ty) (s', iss', e') =
        let ((s'', _), n) = mkUniqSystemId (s', iss') ("x", ty)
        in  (s'', iss' ,Lam n (App e' (Var n)))

newLetBinding
  :: TyConMap
  -> Machine
  -> Term
  -> (Machine, Id)
newLetBinding tcm m e
  | Var v <- e
  , heapContains LocalId v m
  = (m, v)

  | otherwise
  = let m' = heapInsert LocalId id_ e m
     in (m' { mSupply = ids', mScopeNames = is1 }, id_)
 where
  ty = termType tcm e
  ((ids', is1), id_) = mkUniqSystemId (mSupply m, mScopeNames m) ("x", ty)

-- | Unwind the stack by 1
ghcUnwind :: Unwind
ghcUnwind v m tcm = do
  (m', kf) <- stackPop m
  go kf m'
 where
  go (Update s x)             = return . update s x v
  go (Apply x)                = return . apply tcm v x
  go (Instantiate ty)         = return . instantiate tcm v ty
  go (PrimApply p tys vs tms) = ghcPrimUnwind tcm p tys vs v tms
  go (Scrutinise altTy as)    = return . scrutinise v altTy as
  go (Tickish _)              = return . setTerm (valToTerm v)
  -- A cast of a value is a value
  go (Castish ty1 ty2)        = \mN -> ghcUnwind (CastValue v ty1 ty2) mN tcm

-- | Update the Heap with the evaluated term
update :: IdScope -> Id -> Value -> Machine -> Machine
update s x (valToTerm -> term) =
  setTerm term . heapInsert s x term

-- | Apply a value to a function
apply :: TyConMap -> Value -> Id -> Machine -> Machine
apply _tcm (Lambda x' e) x m =
  setTerm (substTm "Evaluator.apply" subst e) m
 where
  subst  = extendIdSubst subst0 x' (Var x)
  subst0 = mkSubst $ extendInScopeSet (mScopeNames m) x
apply tcm pVal@(PrimVal (PrimInfo{primType}) tys []) x m
  | isUndefinedPrimVal pVal
  = setTerm (undefinedTm (piResultTys tcm primType (tys++[varType x]))) m

apply _ _ _ m = error $ "Evaluator.apply: Not a lambda: " ++ show m

-- | Instantiate a type-abstraction
instantiate :: TyConMap -> Value -> Type -> Machine -> Machine
instantiate _tcm (TyLambda x e) ty m =
  setTerm (substTm "Evaluator.instantiate1" subst e) m
 where
  subst  = extendTvSubst subst0 x ty
  subst0 = mkSubst iss0
  iss0   = mkInScopeSet (localFVsOfTerms [e] `unionUniqSet` tyFVsOfTypes [ty])
instantiate tcm pVal@(PrimVal (PrimInfo{primType}) tys []) ty m
  | isUndefinedPrimVal pVal
  = setTerm (undefinedTm (piResultTys tcm primType (tys ++ [ty]))) m

instantiate _ p _ _ = error $ "Evaluator.instantiate: Not a tylambda: " ++ show p

-- | Evaluate a case-expression
scrutinise :: Value -> Type -> [Alt] -> Machine -> Machine
scrutinise v _altTy [] m = setTerm (valToTerm v) m
-- [Note: empty case expressions]
--
-- Clash does not have empty case-expressions; instead, empty case-expressions
-- are used to indicate that the `whnf` function was called the context of a
-- case-expression, which means certain special primitives must be forced.
-- See also [Note: forcing special primitives]
scrutinise (Lit l) _altTy alts m = case alts of
  (DefaultPat, altE):alts1 -> setTerm (go altE alts1) m
  _ -> let term = go (error $ "Evaluator.scrutinise: no match "
                    <> showPpr (Case (valToTerm (Lit l)) (ConstTy Arrow) alts)) alts
        in setTerm term m
 where
  go def [] = def
  go _ ((LitPat l1,altE):_) | l1 == l = altE
  go _ ((DataPat dc [] [x],altE):_)
    | IntegerLiteral l1 <- l
    , Just patE <- case dcTag dc of
       1 | l1 >= ((-2)^(63::Int)) &&  l1 < 2^(63::Int) ->
          Just (IntLiteral l1)
       2 | l1 >= (2^(63::Int)) ->
          let !(Jp# !(BN# ba0)) = l1
              ba1 = BA.ByteArray ba0
          in  Just (ByteArrayLiteral ba1)
       3 | l1 < ((-2)^(63::Int)) ->
          let !(Jn# !(BN# ba0)) = l1
              ba1 = BA.ByteArray ba0
          in  Just (ByteArrayLiteral ba1)
       _ -> Nothing
    = let inScope = localFVsOfTerms [altE]
          subst0  = mkSubst (mkInScopeSet inScope)
          subst1  = extendIdSubst subst0 x (Literal patE)
      in  substTm "Evaluator.scrutinise" subst1 altE
    | NaturalLiteral l1  <- l
    , Just patE <- case dcTag dc of
       1 | l1 >= 0 &&  l1 < 2^(64::Int) ->
          Just (WordLiteral l1)
       2 | l1 >= (2^(64::Int)) ->
          let !(Jp# !(BN# ba0)) = l1
              ba1 = BA.ByteArray ba0
          in  Just (ByteArrayLiteral ba1)
       _ -> Nothing
    = let inScope = localFVsOfTerms [altE]
          subst0  = mkSubst (mkInScopeSet inScope)
          subst1  = extendIdSubst subst0 x (Literal patE)
      in  substTm "Evaluator.scrutinise" subst1 altE
  go def (_:alts1) = go def alts1

scrutinise (DC dc xs) _altTy alts m
  | altE:_ <- [substInAlt altDc tvs pxs xs altE
              | (DataPat altDc tvs pxs,altE) <- alts, altDc == dc ] ++
              [altE | (DefaultPat,altE) <- alts ]
  = setTerm altE m

scrutinise v@(PrimVal p _ vs) altTy alts m
  | isUndefinedPrimVal v
  = setTerm (undefinedTm altTy) m

  | any (\case {(LitPat {},_) -> True; _ -> False}) alts
  = case alts of
      ((DefaultPat,altE):alts1) -> setTerm (go altE alts1) m
      _ -> let term = go (error $ "Evaluator.scrutinise: no match "
                        <> showPpr (Case (valToTerm v) (ConstTy Arrow) alts)) alts
            in setTerm term m
 where
  go def [] = def
  go _   ((LitPat l1,altE):_) | l1 == l = altE
  go def (_:alts1) = go def alts1

  l = case primName p of
        "Clash.Sized.Internal.BitVector.fromInteger##"
          | [Lit (WordLiteral 0), Lit l0] <- vs -> l0
        "Clash.Sized.Internal.BitVector.fromInteger#"
          | [_,Lit (NaturalLiteral 0),Lit l0] <- vs -> l0
        "Clash.Sized.Internal.Index.fromInteger#"
          | [_,Lit l0] <- vs -> l0
        "Clash.Sized.Internal.Signed.fromInteger#"
          | [_,Lit l0] <- vs -> l0
        "Clash.Sized.Internal.Unsigned.fromInteger#"
          | [_,Lit l0] <- vs -> l0
        _ -> error ("scrutinise: " ++ showPpr (Case (valToTerm v) (ConstTy Arrow) alts))

scrutinise v _altTy alts _ =
  error ("scrutinise: " ++ showPpr (Case (valToTerm v) (ConstTy Arrow) alts))

substInAlt :: DataCon -> [TyVar] -> [Id] -> [Either Term Type] -> Term -> Term
substInAlt dc tvs xs args e = substTm "Evaluator.substInAlt" subst e
 where
  tys        = rights args
  tms        = lefts args
  substTyMap = zip tvs (drop (length (dcUnivTyVars dc)) tys)
  substTmMap = zip xs tms
  inScope    = tyFVsOfTypes tys `unionVarSet` localFVsOfTerms (e:tms)
  subst      = extendTvSubstList (extendIdSubstList subst0 substTmMap) substTyMap
  subst0     = mkSubst (mkInScopeSet inScope)

-- | Allocate let-bindings on the heap
allocate :: [LetBinding] -> Term -> Machine -> Machine
allocate xes e m =
  m { mHeapLocal = extendVarEnvList (mHeapLocal m) xes'
    , mSupply = ids'
    , mScopeNames = isN
    , mTerm = e'
    }
 where
  xNms      = fmap fst xes
  is1       = extendInScopeSetList (mScopeNames m) xNms
  (ids', s) = mapAccumL (letSubst (mHeapLocal m)) (mSupply m) xNms
  (nms, s') = unzip s
  isN       = extendInScopeSetList is1 nms
  subst     = extendIdSubstList subst0 s'
  subst0    = mkSubst (foldl' extendInScopeSet is1 nms)
  xes'      = zip nms (fmap (substTm "Evaluator.allocate0" subst . snd) xes)
  e'        = substTm "Evaluator.allocate1" subst e

-- | Create a unique name and substitution for a let-binder
letSubst
  :: PureHeap
  -> Supply
  -> Id
  -> (Supply, (Id, (Id, Term)))
letSubst h acc id0 =
  let (acc',id1) = mkUniqueHeapId h acc id0
  in  (acc',(id1,(id0,Var id1)))
 where
  mkUniqueHeapId :: PureHeap -> Supply -> Id -> (Supply, Id)
  mkUniqueHeapId h' ids x =
    maybe (ids', x') (const $ mkUniqueHeapId h' ids' x) (lookupVarEnv x' h')
   where
    (i,ids') = freshId ids
    x'       = modifyVarName (`setUnique` i) x
