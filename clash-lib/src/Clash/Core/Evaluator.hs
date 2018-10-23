{-|
  Copyright   :  (C) 2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Call-by-need evaluator based on the evaluator described in:

  Maximilian Bolingbroke, Simon Peyton Jones, "Supercompilation by evaluation",
  Haskell '10, Baltimore, Maryland, USA.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Core.Evaluator where

import           Control.Arrow                           (second)
import           Control.Concurrent.Supply               (Supply, freshId)
import           Data.Coerce                             (coerce)
import           Data.Either                             (lefts,rights)
import           Data.List
  (foldl',mapAccumL,uncons)
import           Data.IntMap                             (IntMap)
import           Data.Text                               (Text)
import           Data.Text.Prettyprint.Doc
import           Debug.Trace                             (trace)
import           Clash.Core.DataCon
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
import           Clash.Driver.Types                      (BindingMap)
import           Prelude                                 hiding (lookup)
import           Clash.Unique
import           Clash.Util                              (curLoc)

-- | The heap
data Heap = Heap GlobalHeap PureHeap Supply InScopeSet

type PureHeap = VarEnv Term

-- | Global heap
type GlobalHeap = (IntMap Term, Int)

-- | The stack
type Stack = [StackFrame]

data StackFrame
  = Update Id
  | Apply  Id
  | Instantiate Type
  | PrimApply  Text Type [Type] [Value] [Term]
  | Scrutinise Type [Alt]
  deriving Show

instance Pretty StackFrame where
  pretty (Update i) = hsep ["Update", ppr i]
  pretty (Apply i) = hsep ["Apply", ppr i]
  pretty (Instantiate t) = hsep ["Instantiate", ppr t]
  pretty (PrimApply a b c d e) = do
    hsep ["PrimApply", pretty a, "::", ppr b,
          "; type args=", ppr c,
          "; val args=", ppr (map valToTerm d),
          "term args=", ppr e]
  pretty (Scrutinise a b) =
    hsep ["Scrutinise ", ppr a, ppr (Case (Literal (CharLiteral '_')) a b)]

-- Values
data Value
  = Lambda Id Term
  -- ^ Functions
  | TyLambda TyVar Term
  -- ^ Type abstractions
  | DC DataCon [Either Term Type]
  -- ^ Data constructors
  | Lit Literal
  -- ^ Literals
  | PrimVal  Text Type [Type] [Value]
  -- ^ Clash's number types are represented by their "fromInteger#" primitive
  -- function. So some primitives are values.
  deriving Show

-- | State of the evaluator
type State = (Heap, Stack, Term)

-- | Function that can evaluator primitives, i.e., perform delta-reduction
type PrimEvaluator =
  Bool -> -- Force special primitives? See [Note: forcing special primitives]
  BindingMap -> -- Global binders
  TyConMap -> -- Type constructors
  Heap ->
  Stack ->
  Text -> -- Name of the primitive
  Type -> -- Type of the primitive
  [Type] -> -- Type arguments of the primitive
  [Value] -> -- Value arguments of the primitive
  Maybe State -- Delta-reduction can get stuck, so Nothing is an option

-- | Evaluate to WHNF starting with an empty Heap and Stack
whnf'
  :: PrimEvaluator
  -> BindingMap
  -> TyConMap
  -> GlobalHeap
  -> Supply
  -> InScopeSet
  -> Bool
  -> Term
  -> (GlobalHeap, PureHeap, Term)
whnf' eval gbl tcm gh ids is isSubj e
  = case whnf eval gbl tcm isSubj (Heap gh emptyVarEnv ids is,[],e) of
      (Heap gh' ph' _ _,_,e') -> (gh',ph',e')

-- | Evaluate to WHNF given an existing Heap and Stack
whnf
  :: PrimEvaluator
  -> BindingMap
  -> TyConMap
  -> Bool
  -> State
  -> State
whnf eval gbl tcm isSubj (h,k,e) =
    if isSubj
       then go (h,Scrutinise ty []:k,e) -- See [Note: empty case expressions]
       else go (h,k,e)
  where
    ty = termType tcm e

    go s = case step eval gbl tcm s of
      Just s' -> go s'
      Nothing
        | Just e' <- unwindStack s
        -> e'
        | otherwise
        -> error $ showDoc $ ppr e

-- | Are we in a context where special primitives must be forced.
--
-- See [Note: forcing special primitives]
isScrut :: Stack -> Bool
isScrut (Scrutinise {}:_) = True
isScrut (PrimApply {} :_) = True
isScrut _ = False

-- | Completely unwind the stack to get back the complete term
unwindStack :: State -> Maybe State
unwindStack s@(_,[],_) = Just s
unwindStack (h@(Heap _ h' _ _),(kf:k'),e) = case kf of
  PrimApply nm ty tys vs tms ->
    unwindStack
      (h,k'
      ,foldl' App
              (foldl' App (foldl' TyApp (Prim nm ty) tys) (map valToTerm vs))
              (e:tms))
  Instantiate ty ->
    unwindStack (h,k',TyApp e ty)
  Apply id_ -> do
    case lookupVarEnv id_ h' of
      Just e' -> unwindStack (h,k',App e e')
      Nothing -> error $ unlines
                       $ [ "Clash.Core.Evaluator.unwindStack:"
                         , "Stack:"
                         ] ++
                         [ "  "++ showDoc (pretty frame) | frame <- kf:k'] ++
                         [ ""
                         , "Expression:"
                         , showDoc $ ppr e
                         , ""
                         , "Heap:"
                         , showDoc (pretty h')
                         ]
  Scrutinise _ [] ->
    unwindStack (h,k',e)
  Scrutinise ty alts ->
    unwindStack (h,k',Case e ty alts)
  Update _ ->
    unwindStack (h,k',e)

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

-- | Small-step operational semantics.
step
  :: PrimEvaluator
  -> BindingMap
  -> TyConMap
  -> State
  -> Maybe State
step eval gbl tcm (h, k, e) = case e of
  Var v        -> force gbl h k v
  (Lam x e')   -> unwind eval gbl tcm h k (Lambda x e')
  (TyLam x e') -> unwind eval gbl tcm h k (TyLambda x e')
  (Literal l)  -> unwind eval gbl tcm h k (Lit l)
  (App e1 e2)
    | (Data dc,args) <- collectArgs e
    , (tys,_) <- splitFunForallTy (dcType dc)
    -> case compare (length args) (length tys) of
         EQ -> unwind eval gbl tcm h k (DC dc args)
         LT -> let (tys',_) = splitFunForallTy (termType tcm e)
                   (h2,e')  = mkAbstr (h,e) tys'
               in  step eval gbl tcm (h2,k,e')
         GT -> error "Overapplied DC"
    | (Prim nm ty,args) <- collectArgs e
    , (tys,_) <- splitFunForallTy ty
    -> case compare (length args) (length tys) of
         EQ -> let (e':es) = lefts args
               in  Just (h,PrimApply nm ty (rights args) [] es:k,e')
         LT -> let (tys',_) = splitFunForallTy (termType tcm e)
                   (h2,e') = mkAbstr (h,e) tys'
               in  step eval gbl tcm (h2,k,e')
         GT -> let (h2,id_) = newLetBinding tcm h e2
               in  Just (h2,Apply id_:k,e1)
  (TyApp e1 ty)
    | (Data dc,args) <- collectArgs e
    , (tys,_) <- splitFunForallTy (dcType dc)
    -> case compare (length args) (length tys) of
         EQ -> unwind eval gbl tcm h k (DC dc args)
         LT -> let (tys',_) = splitFunForallTy (termType tcm e)
                   (h2,e') = mkAbstr (h,e) tys'
               in  step eval gbl tcm (h2,k,e')
         GT -> error "Overapplied DC"
    | (Prim nm ty',args) <- collectArgs e
    , (tys,_) <- splitFunForallTy ty'
    -> case compare (length args) (length tys) of
         EQ -> case lefts args of
              [] | nm `elem` ["Clash.Transformations.removedArg"]
                 -- The above primitives are actually values, and not operations.
                 -> unwind eval gbl tcm h k (PrimVal nm ty' (rights args) [])
                 | otherwise
                 -> eval (isScrut k) gbl tcm h k nm ty' (rights args) []
              (e':es) -> Just (h,PrimApply nm ty' (rights args) [] es:k,e')
         LT -> let (tys',_) = splitFunForallTy (termType tcm e)
                   (h2,e') = mkAbstr (h,e) tys'
               in  step eval gbl tcm (h2,k,e')
         GT -> Just (h,Instantiate ty:k,e1)
  (Data dc) -> unwind eval gbl tcm h k (DC dc [])
  (Prim nm ty')
    | nm `elem` ["GHC.Prim.realWorld#"]
    -> unwind eval gbl tcm h k (PrimVal nm ty' [] [])
    | otherwise
    -> eval (isScrut k) gbl tcm h k nm ty' [] []
  (App e1 e2)  -> let (h2,id_) = newLetBinding tcm h e2
                  in  Just (h2,Apply id_:k,e1)
  (TyApp e1 ty) -> Just (h,Instantiate ty:k,e1)
  (Case scrut ty alts) -> Just (h,Scrutinise ty alts:k,scrut)
  (Letrec bs e') -> Just (allocate h k bs e')
  Cast _ _ _ -> trace (unlines ["WARNING: " ++ $(curLoc) ++ "Clash currently can't symbolically evaluate casts"
                                    ,"If you have testcase that produces this message, please open an issue about it."]) Nothing

newLetBinding
  :: TyConMap
  -> Heap
  -> Term
  -> (Heap,Id)
newLetBinding tcm h@(Heap gh h' ids is0) e
  | Var v <- e
  , Just _ <- lookupVarEnv v h'
  = (h, v)
  | otherwise
  = (Heap gh (extendVarEnv id_ e h') ids' is1,id_)
  where
    ty = termType tcm e
    ((ids',is1),id_) = mkUniqSystemId (ids,is0) ("x",ty)

newLetBindings'
  :: TyConMap
  -> Heap
  -> [Either Term Type]
  -> (Heap,[Either Term Type])
newLetBindings' tcm =
    (second (map (either (Left . toVar) (Right . id))) .) . mapAccumL go
  where
    go h (Left tm)  = second Left (newLetBinding tcm h tm)
    go h (Right ty) = (h,Right ty)

mkAbstr
  :: (Heap,Term)
  -> [Either TyVar Type]
  -> (Heap,Term)
mkAbstr = foldr go
  where
    go (Left tv)  (h,e)          =
      (h,TyLam tv (TyApp e (VarTy tv)))
    go (Right ty) (Heap gh h ids is,e) =
      let ((ids',_),id_) = mkUniqSystemId (ids,is) ("x",ty)
      in  (Heap gh h ids' is,Lam id_ (App e (Var id_)))

-- | Force the evaluation of a variable.
force :: BindingMap -> Heap -> Stack -> Id -> Maybe State
force gbl (Heap gh h ids is) k x' = case lookupVarEnv x' h of
    Nothing -> case lookupUniqMap x' gbl of
      Nothing        -> Nothing
      Just (_,_,_,e) -> Just (Heap  gh h ids is,k,e)
    Just e -> Just (Heap gh (delVarEnv h x') ids is,Update x':k,e)
    -- Removing the heap-bound value on a force ensures we do not get stuck on
    -- expressions such as: "let x = x in x"

-- | Unwind the stack by 1
unwind
  :: PrimEvaluator
  -> BindingMap
  -> TyConMap
  -> Heap -> Stack -> Value -> Maybe State
unwind eval gbl tcm h k v = do
  (kf,k') <- uncons k
  case kf of
    Update x                     -> return (update h k' x v)
    Apply x                      -> return (apply  h k' v x)
    Instantiate ty               -> return (instantiate gbl h k' v ty)
    PrimApply nm ty tys vals tms -> primop eval gbl tcm h k' nm ty tys vals v tms
    Scrutinise _ alts            -> return (scrutinise h k' v alts)

-- | Update the Heap with the evaluated term
update :: Heap -> Stack -> Id -> Value -> State
update (Heap gh h ids is) k x v = (Heap gh (extendVarEnv x v' h) ids is,k,v')
  where
    v' = valToTerm v

valToTerm :: Value -> Term
valToTerm v = case v of
  Lambda x e           -> Lam x e
  TyLambda x e         -> TyLam x e
  DC dc pxs            -> foldl' (\e a -> either (App e) (TyApp e) a)
                                 (Data dc) pxs
  Lit l                -> Literal l
  PrimVal nm ty tys vs -> foldl' App (foldl' TyApp (Prim nm ty) tys)
                                 (map valToTerm vs)

toVar :: Id -> Term
toVar x = Var x

toType :: TyVar -> Type
toType x = VarTy x

-- | Apply a value to a function
apply :: Heap -> Stack -> Value -> Id -> State
apply h@(Heap _ _ _ is0) k (Lambda x' e) x = (h,k,substTm "Evaluator.apply" subst e)
 where
  subst  = extendIdSubst subst0 x' (Var x)
  subst0 = mkSubst (extendInScopeSet is0 x)
apply _ _ _ _ = error "not a lambda"

-- | Instantiate a type-abstraction
instantiate :: BindingMap -> Heap -> Stack -> Value -> Type -> State
instantiate gbl h k (TyLambda x e) ty = (h,k,substTm "Evaluator.instantiate" subst e)
 where
  subst  = extendTvSubst subst0 x ty
  subst0 = mkSubst is0
  is0 = mkInScopeSet (gblVars `unionUniqSet` tyFVsOfTypes [ty])
  gblVars :: VarSet
  gblVars = uniqMapToUniqSet $ fmap ((\(nm,_,_,_) -> coerce nm)) gbl
instantiate _ _ _ _ _ = error "not a ty lambda"

-- | Evaluation of primitive operations
primop
  :: PrimEvaluator
  -> BindingMap
  -> TyConMap
  -> Heap
  -> Stack
  -> Text
  -- ^ Name of the primitive
  -> Type
  -- ^ Type of the primitive
  -> [Type]
  -- ^ Applied types
  -> [Value]
  -- ^ Applied values
  -> Value
  -- ^ The current value
  -> [Term]
  -- ^ The remaining terms which must be evaluated to a value
  -> Maybe State
primop eval gbl tcm h k nm ty tys vs v []
  | nm `elem` ["Clash.Sized.Internal.BitVector.fromInteger#"
              ,"Clash.Sized.Internal.BitVector.fromInteger##"
              ,"Clash.Sized.Internal.Index.fromInteger#"
              ,"Clash.Sized.Internal.Signed.fromInteger#"
              ,"Clash.Sized.Internal.Unsigned.fromInteger#"
              ,"GHC.CString.unpackCString#"
              ,"Clash.Transformations.removedArg"
              ,"GHC.Prim.MutableByteArray#"
              ]
              -- The above primitives are actually values, and not operations.
  = unwind eval gbl tcm h k (PrimVal nm ty tys (vs ++ [v]))
  | otherwise = eval (isScrut k) gbl tcm h k nm ty tys (vs ++ [v])
primop _ _ _ h k nm ty tys vs v (e:es) =
  Just (h,PrimApply nm ty tys (vs ++ [v]) es:k,e)

-- | Evaluate a case-expression
scrutinise :: Heap -> Stack -> Value -> [Alt] -> State
scrutinise h k (Lit l) alts
  | altE:_ <-
    [altE | (LitPat altL,altE) <- alts, altL == l ] ++
    [altE | (DataPat altDc _ _,altE) <- alts, matchLit altDc l ] ++
    [altE | (DefaultPat,altE) <- alts ]
  = (h,k,altE)
scrutinise h k (DC dc xs) alts
  | altE:_ <- [substAlt altDc tvs pxs xs altE
              | (DataPat altDc tvs pxs,altE) <- alts, altDc == dc ] ++
              [altE | (DefaultPat,altE) <- alts ]
  = (h,k,altE)
scrutinise h k v [] = (h,k,valToTerm v)
-- [Note: empty case expressions]
--
-- Clash does not have empty case-expressions; instead, empty case-expressions
-- are used to indicate that the `whnf` function was called the context of a
-- case-expression, which means certain special primitives must be forced.
-- See also [Note: forcing special primitives]
scrutinise _ _ _ _  = error "scrutinise"

matchLit :: DataCon -> Literal -> Bool
matchLit dc (IntegerLiteral l)
  | dcTag dc == 1
  = l < 2^(63::Int)
matchLit dc (NaturalLiteral l)
  | dcTag dc == 1
  = l < 2^(64::Int)
matchLit _ _ = False

substAlt :: DataCon -> [TyVar] -> [Id] -> [Either Term Type] -> Term -> Term
substAlt dc tvs xs args e = substTm "Evaluator.substAlt" subst e
 where
  tys        = rights args
  tms        = lefts args
  substTyMap = zip tvs (drop (length (dcUnivTyVars dc)) tys)
  substTmMap = zip xs tms
  inScope    = tyFVsOfTypes tys `unionVarSet` fVsOfTerms (e:tms)
  subst      = extendTvSubstList (extendIdSubstList subst0 substTmMap) substTyMap
  subst0     = mkSubst (mkInScopeSet inScope)

-- | Allocate let-bindings on the heap
allocate :: Heap -> Stack -> [LetBinding] -> Term -> State
allocate (Heap gh h ids is0) k xes e =
  (Heap gh (h `extendVarEnvList` xes') ids' isN,k,e')
 where
  xNms     = map fst xes
  is1      = extendInScopeSetList is0 xNms
  (ids',s) = mapAccumL (letSubst h) ids xNms
  (nms,s') = unzip s
  isN      = extendInScopeSetList is1 nms
  subst    = extendIdSubstList subst0 s'
  subst0   = mkSubst (foldl' extendInScopeSet is1 nms)
  xes'     = zip nms (map (substTm "Evaluator.allocate0" subst . snd) xes)
  e'       = substTm "Evaluator.allocate1" subst e

-- | Create a unique name and substitution for a let-binder
letSubst
  :: PureHeap
  -> Supply
  -> Id
  -> ( Supply
     , (Id,(Id,Term)))
letSubst h acc id0 =
  let (acc',id1) = uniqueInHeap h acc id0
  in  (acc',(id1,(id0,Var id1)))

-- | Create a name that's unique in the heap
uniqueInHeap
  :: PureHeap
  -> Supply
  -> Id
  -> (Supply, Id)
uniqueInHeap h ids x = case lookupVarEnv x' h of
  Just _ -> uniqueInHeap h ids' x
  _ -> (ids',x')
 where
  (i,ids') = freshId ids
  x'       = modifyVarName (\nm -> nm {nameUniq = i}) x
