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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Evaluator where

import           Prelude                                 hiding (lookup)

import           Control.Concurrent.Supply               (Supply, freshId)
import           Data.Either                             (lefts,rights)
import           Data.List                               (foldl',mapAccumL)
import           Data.Maybe                              (fromMaybe)
import qualified Data.Primitive.ByteArray                as BA
import qualified Data.Text as Text
import           Debug.Trace
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
import           Clash.Core.TyCon
import           Clash.Core.Type
import           Clash.Core.Util
import           Clash.Core.Var
import           Clash.Core.VarEnv
import           Clash.Driver.Types                      (BindingMap, Binding(..))
import           Clash.Pretty
import           Clash.Unique
import           Clash.Util                              (curLoc)

whnf'
  :: PrimStep
  -> PrimUnwind
  -> BindingMap
  -> TyConMap
  -> PrimHeap
  -> Supply
  -> InScopeSet
  -> Bool
  -> Term
  -> (PrimHeap, PureHeap, Term)
whnf' eval fu bm tcm ph ids is isSubj e =
  toResult $ whnf tcm isSubj m
 where
  toResult x = (mHeapPrim x, mHeapLocal x, mTerm x)

  m  = Machine eval fu ph gh emptyVarEnv [] ids is e
  gh = mapVarEnv bindingTerm bm

-- | Evaluate to WHNF given an existing Heap and Stack
whnf
  :: TyConMap
  -> Bool
  -> Machine
  -> Machine
whnf tcm isSubj m
  | isSubj =
      -- See [Note: empty case expressions]
      let ty = termType tcm (mTerm m)
       in go (stackPush (Scrutinise ty []) m)
  | otherwise = go m
  where
    go s = case step s tcm of
      Just s' -> go s'
      Nothing -> fromMaybe (error . showDoc . ppr $ mTerm m) (unwindStack s)

-- | Completely unwind the stack to get back the complete term
unwindStack :: Machine -> Maybe Machine
unwindStack m
  | stackNull m = Just m
  | otherwise = do
      (m', kf) <- stackPop m

      case kf of
        PrimApply p tys vs tms ->
          let term = foldl' App
                       (foldl' App
                         (foldl' TyApp (Prim p) tys)
                         (fmap valToTerm vs))
                       (mTerm m' : tms)
           in unwindStack (setTerm term m')

        Instantiate ty ->
          let term = TyApp (getTerm m') ty
           in unwindStack (setTerm term m')

        Apply n ->
          case heapLookup LocalId n m' of
            Just e ->
              let term = App (getTerm m') e
               in unwindStack (setTerm term m')

            Nothing -> error $ unlines $
              [ "Clash.Core.Evaluator.unwindStack:"
              , "Stack:"
              ] <>
              [ "  " <> showDoc (clashPretty frame) | frame <- mStack m] <>
              [ ""
              , "Expression:"
              , showPpr (mTerm m)
              , ""
              , "Heap:"
              , showDoc (clashPretty $ mHeapLocal m)
              ]

        Scrutinise _ [] ->
          unwindStack m'

        Scrutinise ty alts ->
          let term = Case (getTerm m') ty alts
           in unwindStack (setTerm term m')

        Update LocalId x ->
          unwindStack (heapInsert LocalId x (mTerm m') m')

        Update GlobalId _ ->
          unwindStack m'

        Tickish sp ->
          let term = Tick sp (getTerm m')
           in unwindStack (setTerm term m')

-- | A single step in the partial evaluator. The result is the new heap and
-- stack, and the next expression to be reduced.
--
type Step = Machine -> TyConMap -> Maybe Machine

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
stepData dc m tcm = unwind tcm m (DC dc [])

stepLiteral :: Literal -> Step
stepLiteral l m tcm = unwind tcm m (Lit l)

stepPrim :: PrimInfo -> Step
stepPrim pInfo m tcm
  | primName pInfo == "GHC.Prim.realWorld#" =
      unwind tcm m (PrimVal pInfo [] [])

  | otherwise =
      case fst $ splitFunForallTy (primType pInfo) of
        []  -> mPrimStep m tcm (forcePrims m) pInfo [] [] m
        tys -> newBinder tys (Prim pInfo) m tcm

stepLam :: Id -> Term -> Step
stepLam x e m tcm = unwind tcm m (Lambda x e)

stepTyLam :: TyVar -> Term -> Step
stepTyLam x e m tcm = unwind tcm m (TyLambda x e)

stepApp :: Term -> Term -> Step
stepApp x y m tcm =
  case term of
    Data dc ->
      let tys = fst $ splitFunForallTy (dcType dc)
       in case compare (length args) (length tys) of
            EQ -> unwind tcm m (DC dc args)
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
                    in  mPrimStep m tcm (forcePrims m) p [] [Suspend (Var i), Suspend (Var j)] m1

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
    Data dc ->
      let tys = fst $ splitFunForallTy (dcType dc)
       in case compare (length args) (length tys) of
            EQ -> unwind tcm m (DC dc args)
            LT -> newBinder tys' (TyApp x ty) m tcm
            GT -> error "Overapplied DC"

    Prim p ->
      let tys = fst $ splitFunForallTy (primType p)
       in case compare (length args) (length tys) of
            EQ -> case lefts args of
                    [] | primName p == "Clash.Transformations.removedArg" ->
                            unwind tcm m (PrimVal p (rights args) [])

                       | otherwise ->
                            mPrimStep m tcm (forcePrims m) p (rights args) [] m

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
stepCase scrut ty alts m _ =
  Just . setTerm scrut $ stackPush (Scrutinise ty alts) m

-- TODO Support stepwise evaluation of casts.
--
stepCast :: Term -> Type -> Type -> Step
stepCast _ _ _ _ _ =
  flip trace Nothing $ unlines
    [ "WARNING: " <> $(curLoc) <> "Clash can't symbolically evaluate casts"
    , "Please file an issue at https://github.com/clash-lang/clash-compiler/issues"
    ]

stepTick :: TickInfo -> Term -> Step
stepTick tick x m _ =
  Just . setTerm x $ stackPush (Tickish tick) m

-- | Small-step operational semantics.
--
step :: Step
step m = case mTerm m of
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
   in step m' tcm
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
unwind
  :: TyConMap
  -> Machine
  -> Value
  -> Maybe Machine
unwind tcm m v = do
  (m', kf) <- stackPop m
  go kf m'
 where
  go (Update s x)             = return . update s x v
  go (Apply x)                = return . apply v x
  go (Instantiate ty)         = return . instantiate v ty
  go (PrimApply p tys vs tms) = mPrimUnwind m tcm p tys vs v tms
  go (Scrutinise _ as)        = return . scrutinise v as
  go (Tickish _)              = return . setTerm (valToTerm v)

-- | Update the Heap with the evaluated term
update :: IdScope -> Id -> Value -> Machine -> Machine
update s x (valToTerm -> term) =
  setTerm term . heapInsert s x term

-- | Apply a value to a function
apply :: Value -> Id -> Machine -> Machine
apply (Lambda x' e) x m =
  setTerm (substTm "Evaluator.apply" subst e) m
 where
  subst  = extendIdSubst subst0 x' (Var x)
  subst0 = mkSubst $ extendInScopeSet (mScopeNames m) x

apply _ _ _ = error "Evaluator.apply: Not a lambda"

-- | Instantiate a type-abstraction
instantiate :: Value -> Type -> Machine -> Machine
instantiate (TyLambda x e) ty =
  setTerm (substTm "Evaluator.instantiate" subst e)
 where
  subst  = extendTvSubst subst0 x ty
  subst0 = mkSubst iss0
  iss0   = mkInScopeSet (localFVsOfTerms [e] `unionUniqSet` tyFVsOfTypes [ty])

instantiate _ _ = error "Evaluator.instantiate: Not a tylambda"

-- | Evaluate a case-expression
scrutinise :: Value -> [Alt] -> Machine -> Machine
scrutinise v [] m = setTerm (valToTerm v) m
-- [Note: empty case expressions]
--
-- Clash does not have empty case-expressions; instead, empty case-expressions
-- are used to indicate that the `whnf` function was called the context of a
-- case-expression, which means certain special primitives must be forced.
-- See also [Note: forcing special primitives]
scrutinise (Lit l) alts m = case alts of
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

scrutinise (DC dc xs) alts m
  | altE:_ <- [substInAlt altDc tvs pxs xs altE
              | (DataPat altDc tvs pxs,altE) <- alts, altDc == dc ] ++
              [altE | (DefaultPat,altE) <- alts ]
  = setTerm altE m

scrutinise v@(PrimVal p _ vs) alts m
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
        "Clash.Sized.Internal.BitVector.fromInteger#"
          | [_,Lit (IntegerLiteral 0),Lit l0] <- vs -> l0
        "Clash.Sized.Internal.Index.fromInteger#"
          | [_,Lit l0] <- vs -> l0
        "Clash.Sized.Internal.Signed.fromInteger#"
          | [_,Lit l0] <- vs -> l0
        "Clash.Sized.Internal.Unsigned.fromInteger#"
          | [_,Lit l0] <- vs -> l0
        _ -> error ("scrutinise: " ++ showPpr (Case (valToTerm v) (ConstTy Arrow) alts))

scrutinise v alts _ = error ("scrutinise: " ++ showPpr (Case (valToTerm v) (ConstTy Arrow) alts))

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

