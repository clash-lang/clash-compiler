{-|
  Copyright   :  (C) 2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Call-by-need evaluator based on the evaluator described in:

  Maximilian Bolingbroke, Simon Peyton Jones, "Supercompilation by evaluation",
  Haskell '10, Baltimore, Maryland, USA.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module CLaSH.Core.Evaluator where

import           Control.Arrow                           ((***), second)
import           Control.Concurrent.Supply               (Supply, freshId)
import           Data.Either                             (lefts,rights)
import qualified Data.HashMap.Lazy                       as HM
import           Data.List
  (foldl',mapAccumL,uncons)
import           Data.Map
  (Map,delete,fromList,insert,lookup,union)
import           Data.Text                               (Text)
import           CLaSH.Core.DataCon
import           CLaSH.Core.Literal
import           CLaSH.Core.Name
import           CLaSH.Core.Pretty
import           CLaSH.Core.Subst
import           CLaSH.Core.Term
import           CLaSH.Core.TyCon
import           CLaSH.Core.Type
import           CLaSH.Core.Util
import           CLaSH.Core.Var
import           CLaSH.Driver.Types                      (BindingMap)
import           Prelude                                 hiding (lookup)
import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe

-- | The heap
data Heap     = Heap PureHeap Supply
  deriving (Show)

type PureHeap = Map TmOccName Term

-- | The stack
type Stack    = [StackFrame]

data StackFrame
  = Update Id
  | Apply  Id
  | Instantiate Type
  | PrimApply  Text Type [Type] [Value] [Term]
  | Scrutinise [Alt]
  deriving Show

-- Values
data Value
  = Lambda (Bind Id      Term)
  -- ^ Functions
  | TyLambda (Bind TyVar   Term)
  -- ^ Type abstractions
  | DC DataCon [Either Term Type]
  -- ^ Data constructors
  | Lit Literal
  -- ^ Literals
  | PrimVal  Text Type [Either Term Type]
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
  -> Supply
  -> Bool
  -> Term
  -> Term
whnf' eval gbl tcm ids isSubj e
  = case whnf eval gbl tcm isSubj (Heap (fromList []) ids,[],e) of
      (_,_,e') -> e'

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
       then go (h,Scrutinise []:k,e) -- See [Note: empty case expressions]
       else go (h,k,e)
  where
    go s = case step eval gbl tcm s of
      Just s' -> go s'
      Nothing
        | Just e' <- unwindStack s
        -> e'
        | otherwise
        -> error $ showDoc e

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
unwindStack (h@(Heap h' _),(kf:k'),e) = case kf of
  PrimApply nm ty tys vals tms ->
    unwindStack (h,k',valToTerm (PrimVal nm ty (map Right tys ++
                                                map (Left . valToTerm) vals ++
                                                map Left (tms ++ [e]))))
  Instantiate ty ->
    unwindStack (h,k',TyApp e ty)
  Apply id_ -> do
    e' <- lookup (nameOcc (varName id_)) h'
    unwindStack (h,k',App e e')
  Scrutinise [] ->
    unwindStack (h,k',e)
  _ -> error (show kf)

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

  - Primitives such as `CLaSH.Sized.Vector.transpose`, `CLaSH.Sized.Vector.map`,
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
  Var ty nm    -> force gbl h k (Id nm (embed ty))
  (Lam b)      -> unwind eval gbl tcm h k (Lambda b)
  (TyLam b)    -> unwind eval gbl tcm h k (TyLambda b)
  (Literal l)  -> unwind eval gbl tcm h k (Lit l)
  (App e1 e2)
    | (Data dc,args) <- collectArgs e
    , (tys,_) <- splitFunForallTy (dcType dc)
    -> case compare (length args) (length tys) of
         EQ -> unwind eval gbl tcm h k (DC dc args)
         LT -> let (h2,e') = mkAbstr (h,e) (drop (length args) tys)
               in  step eval gbl tcm (h2,k,e')
         GT -> error "Overapplied DC"
    | (Prim nm ty,args) <- collectArgs e
    , (tys,_) <- splitFunForallTy ty
    -> case compare (length args) (length tys) of
         EQ | nm `elem` ["CLaSH.Sized.Internal.BitVector.fromInteger#"
                        ,"CLaSH.Sized.Internal.Index.fromInteger#"
                        ,"CLaSH.Sized.Internal.Signed.fromInteger#"
                        ,"CLaSH.Sized.Internal.Unsigned.fromInteger#"
                        ,"GHC.CString.unpackCString#"
                        ]
               -- The above primitives are actually values, and not operations.
            -> unwind eval gbl tcm h k (PrimVal nm ty args)
            | otherwise
            -> let (e':es)    = lefts args
               in  Just (h,PrimApply nm ty (rights args) [] es:k,e')
         LT -> let (h2,e') = mkAbstr (h,e) (drop (length args) tys)
               in  step eval gbl tcm (h2,k,e')
         GT -> let (h2,id_) = newLetBinding tcm h e2
               in  Just (h2,Apply id_:k,e1)
  (TyApp e1 ty)
    | (Data dc,args) <- collectArgs e
    , (tys,_) <- splitFunForallTy (dcType dc)
    -> case compare (length args) (length tys) of
         EQ -> unwind eval gbl tcm h k (DC dc args)
         LT -> let (h2,e') = mkAbstr (h,e) (drop (length args) tys)
               in  step eval gbl tcm (h2,k,e')
         GT -> error "Overapplied DC"
    | (Prim nm ty',args) <- collectArgs e
    , (tys,_) <- splitFunForallTy ty'
    -> case compare (length args) (length tys) of
         EQ | nm `elem` ["CLaSH.Transformations.removedArg"
                        ]
              -- The above primitives are actually values, and not operations.
            -> unwind eval gbl tcm h k (PrimVal nm ty' args)
            | otherwise
            -> case lefts args of
              [] -> eval (isScrut k) gbl tcm h k nm ty' (rights args) []
              (e':es) -> Just (h,PrimApply nm ty' (rights args) [] es:k,e')
         LT -> let (h2,e') = mkAbstr (h,e) (drop (length args) tys)
               in  step eval gbl tcm (h2,k,e')
         GT -> Just (h,Instantiate ty:k,e1)
  (Data dc) -> unwind eval gbl tcm h k (DC dc [])
  (Prim nm ty') -> eval (isScrut k) gbl tcm h k nm ty' [] []
  (App e1 e2)  -> let (h2,id_) = newLetBinding tcm h e2
                  in  Just (h2,Apply id_:k,e1)
  (TyApp e1 ty) -> Just (h,Instantiate ty:k,e1)
  (Case scrut _ alts) -> Just (h,Scrutinise alts:k,scrut)
  (Letrec bs)   -> Just (allocate h k bs)

newLetBinding
  :: TyConMap
  -> Heap
  -> Term
  -> (Heap,Id)
newLetBinding _   h            (Var ty nm) = (h,Id nm (embed ty))
newLetBinding tcm (Heap h ids) e           =
    (Heap (insert (nameOcc nm) e h) ids',Id nm (embed ty))
  where
    (i,ids') = freshId ids
    nm       = makeSystemName "x" (toInteger i)
    ty       = runFreshM (termType tcm e)

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
      (h,TyLam (bind tv (TyApp e (VarTy (unembed (varKind tv)) (varName tv)))))
    go (Right ty) (Heap h ids,e) =
      let (i,ids') = freshId ids
          nm       = makeSystemName "x" (toInteger i)
          id_      = Id nm (embed ty)
      in  (Heap h ids',Lam (bind id_ (App e (Var ty nm))))

-- | Force the evaluation of a variable.
force :: BindingMap -> Heap -> Stack -> Id -> Maybe State
force gbl (Heap h ids) k x' = case lookup nm h of
    Nothing -> case HM.lookup nm gbl of
      Nothing          -> Nothing
      Just (_,_,_,_,e) -> Just (Heap h ids,k,e)
    Just e -> Just (Heap (delete nm h) ids,k,e)
    -- Removing the heap-bound value on a force ensures we do not get stuck on
    -- expressions such as: "let x = x in x"
  where
    nm = nameOcc (varName x')

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
    Instantiate ty               -> return (instantiate h k' v ty)
    PrimApply nm ty tys vals tms -> primop eval gbl tcm h k' nm ty tys vals v tms
    Scrutinise alts              -> return (scrutinise h k' v alts)

-- | Update the Heap with the evaluated term
update :: Heap -> Stack -> Id -> Value -> State
update (Heap h ids) k x v = (Heap (insert (nameOcc (varName x)) v' h) ids,k,v')
  where
    v' = valToTerm v

valToTerm :: Value -> Term
valToTerm v = case v of
  Lambda b           -> Lam b
  TyLambda b         -> TyLam b
  DC dc pxs          -> foldl' (\e a -> either (App e) (TyApp e) a) (Data dc) pxs
  Lit l              -> Literal l
  PrimVal nm ty args -> foldl' (\e a -> either (App e) (TyApp e) a) (Prim nm ty) args

toVar :: Id -> Term
toVar x = Var (unembed (varType x)) (varName x)

toType :: TyVar -> Type
toType x = VarTy (unembed (varKind x)) (varName x)

-- | Apply a value to a function
apply :: Heap -> Stack -> Value -> Id -> State
apply h k (Lambda b) x = (h,k,subst nm (toVar x) e)
  where
    (x',e) = unsafeUnbind b
    nm     = nameOcc (varName x')
apply _ _ _ _ = error "not a lambda"

-- | Instantiate a type-abstraction
instantiate :: Heap -> Stack -> Value -> Type -> State
instantiate h k (TyLambda b) ty = (h,k,subst nm ty e)
  where
    (x,e) = unsafeUnbind b
    nm    = nameOcc (varName x)
instantiate _ _ _ _ = error "not a ty lambda"

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
primop eval gbl tcm h k nm ty tys vs v [] =
  eval (isScrut k) gbl tcm h k nm ty tys (vs ++ [v])
primop _ _ _ h k nm ty tys vs v (e:es) =
  Just (h,PrimApply nm ty tys (vs ++ [v]) es:k,e)

-- | Evaluate a case-expression
scrutinise :: Heap -> Stack -> Value -> [Alt] -> State
scrutinise h k (Lit l) (map unsafeUnbind -> alts)
  | altE:_ <-
    [altE | (LitPat (unembed -> altL),altE) <- alts, altL == l ] ++
    [altE | (DataPat (unembed -> altDc) _,altE) <- alts, matchLit altDc l ] ++
    [altE | (DefaultPat,altE) <- alts ]
  = (h,k,altE)
scrutinise h k (DC dc xs) (map unsafeUnbind -> alts)
  | altE:_ <- [substAlt altDc pxs xs altE
              | (DataPat (unembed -> altDc) pxs,altE) <- alts, altDc == dc ] ++
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

substAlt :: DataCon -> Rebind [TyVar] [Id] -> [Either Term Type] -> Term -> Term
substAlt dc pxs args e =
  let (tvs,xs)   = unrebind pxs
      substTyMap = zip (map (nameOcc.varName) tvs)
                       (drop (length (dcUnivTyVars dc)) (rights args))
      substTmMap = zip (map (nameOcc.varName) xs) (lefts args)
  in  substTysinTm substTyMap (substTms substTmMap e)

-- | Allocate let-bindings on the heap
allocate :: Heap -> Stack -> (Bind (Rec [LetBinding]) Term) -> State
allocate (Heap h ids) k b =
  let (xesR,e) = unsafeUnbind b
      xes      = unrec xesR
      xes'     = map ((nameOcc . varName) *** unembed) xes
  in  (Heap (h `union` fromList xes') ids,k,e)
