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
import           Data.Either                             (lefts,rights)
import qualified Data.HashMap.Lazy                       as HM
import           Data.List
  (foldl',mapAccumL,uncons)
import           Data.IntMap                             (IntMap)
import           Data.Map
  (Map,delete,fromList,insert,lookup,union)
import qualified Data.Map                                as M
import           Data.Text                               (Text)
import           Data.Text.Prettyprint.Doc               (hsep)
import           Debug.Trace                             (trace)
import           Clash.Core.DataCon
import           Clash.Core.Literal
import           Clash.Core.Name
import           Clash.Core.Pretty
import           Clash.Core.Subst
import           Clash.Core.Term
import           Clash.Core.TyCon
import           Clash.Core.Type
import           Clash.Core.Util
import           Clash.Core.Var
import           Clash.Driver.Types                      (BindingMap)
import           Prelude                                 hiding (lookup)
import           Clash.Util                              (curLoc)
import           Unbound.Generics.LocallyNameless        as Unbound
import           Unbound.Generics.LocallyNameless.Unsafe

-- | The heap
data Heap     = Heap GlobalHeap PureHeap Supply
  deriving (Show)

type PureHeap = Map TmOccName Term

-- | Global heap
type GlobalHeap = (IntMap Term, Int)

-- | The stack
type Stack    = [StackFrame]

data StackFrame
  = Update Id
  | Apply  Id
  | Instantiate Type
  | PrimApply  Text Type [Type] [Value] [Term]
  | Scrutinise Type [Alt]
  deriving Show

instance Pretty StackFrame where
  pprPrec _ (Update i) = do
    i' <- ppr i
    pure (hsep ["Update", i'])
  pprPrec _ (Apply i) = do
    i' <- ppr i
    pure (hsep ["Apply", i'])
  pprPrec _ (Instantiate t) = do
    t' <- ppr t
    pure (hsep ["Instantiate", t'])
  pprPrec _ (PrimApply a b c d e) = do
      a' <- ppr a
      b' <- ppr b
      c' <- ppr c
      d' <- ppr (map valToTerm d)
      e' <- ppr e
      pure $ hsep ["PrimApply", a', "::", b',
                   "; type args=", c',
                   "; val args=", d',
                   "term args=", e']
  pprPrec _ (Scrutinise a b) = do
      a' <- ppr a
      b' <- ppr (Case (Literal (CharLiteral '_')) a b)
      pure $ hsep ["Scrutinise ", a', b']

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
  -> Bool
  -> Term
  -> (GlobalHeap, Term)
whnf' eval gbl tcm gh ids isSubj e
  = case whnf eval gbl tcm isSubj (Heap gh (fromList []) ids,[],e) of
      (Heap gh' _ _,_,e') -> (gh',e')

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
    ty = runFreshM $ termType tcm e

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
unwindStack (h@(Heap _ h' _),(kf:k'),e) = case kf of
  PrimApply nm ty tys vs tms ->
    unwindStack
      (h,k'
      ,foldl' App
              (foldl' App (foldl' TyApp (Prim nm ty) tys) (map valToTerm vs))
              (e:tms))
  Instantiate ty ->
    unwindStack (h,k',TyApp e ty)
  Apply id_ -> do
    case lookup (nameOcc (varName id_)) h' of
      Just e' -> unwindStack (h,k',App e e')
      Nothing -> error $ unlines
                       $ [ "Clash.Core.Evaluator.unwindStack:"
                         , "Stack:"
                         ] ++
                         [ "  "++showDoc frame | frame <- kf:k'] ++
                         [ ""
                         , "Expression:"
                         , showDoc e
                         , ""
                         , "Heap:"
                         ] ++
                         [ "  "++show name ++ "  ===  " ++ showDoc value
                         | (name,value) <- M.toList h'
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
         EQ -> let (e':es) = lefts args
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
         EQ -> case lefts args of
              [] | nm `elem` ["Clash.Transformations.removedArg"]
                 -- The above primitives are actually values, and not operations.
                 -> unwind eval gbl tcm h k (PrimVal nm ty' (rights args) [])
                 | otherwise
                 -> eval (isScrut k) gbl tcm h k nm ty' (rights args) []
              (e':es) -> Just (h,PrimApply nm ty' (rights args) [] es:k,e')
         LT -> let (h2,e') = mkAbstr (h,e) (drop (length args) tys)
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
  (Letrec bs)   -> Just (allocate h k bs)
  Cast _ _ _ -> trace (unlines ["WARNING: " ++ $(curLoc) ++ "Clash currently can't symbolically evaluate casts"
                                    ,"If you have testcase that produces this message, please open an issue about it."]) Nothing

newLetBinding
  :: TyConMap
  -> Heap
  -> Term
  -> (Heap,Id)
newLetBinding tcm h@(Heap gh h' ids) e
  | Var ty' nm' <- e
  , Just _ <- lookup (nameOcc nm') h'
  = (h, Id nm' (embed ty'))
  | otherwise
  = (Heap gh (insert (nameOcc nm) e h') ids',Id nm (embed ty))
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
    go (Right ty) (Heap gh h ids,e) =
      let (i,ids') = freshId ids
          nm       = makeSystemName "x" (toInteger i)
          id_      = Id nm (embed ty)
      in  (Heap gh h ids',Lam (bind id_ (App e (Var ty nm))))

-- | Force the evaluation of a variable.
force :: BindingMap -> Heap -> Stack -> Id -> Maybe State
force gbl (Heap gh h ids) k x' = case lookup nm h of
    Nothing -> case HM.lookup nm gbl of
      Nothing          -> Nothing
      Just (_,_,_,_,e) -> Just (Heap  gh h ids,k,e)
    Just e -> Just (Heap gh (delete nm h) ids,Update x':k,e)
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
    Scrutinise _ alts            -> return (scrutinise h k' v alts)

-- | Update the Heap with the evaluated term
update :: Heap -> Stack -> Id -> Value -> State
update (Heap gh h ids) k x v = (Heap gh (insert (nameOcc (varName x)) v' h) ids,k,v')
  where
    v' = valToTerm v

valToTerm :: Value -> Term
valToTerm v = case v of
  Lambda b             -> Lam b
  TyLambda b           -> TyLam b
  DC dc pxs            -> foldl' (\e a -> either (App e) (TyApp e) a)
                                 (Data dc) pxs
  Lit l                -> Literal l
  PrimVal nm ty tys vs -> foldl' App (foldl' TyApp (Prim nm ty) tys)
                                 (map valToTerm vs)

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
allocate (Heap gh h ids) k b =
  (Heap gh (h `union` fromList xes') ids',k,e')
 where
  (xesR,e) = unsafeUnbind b
  xes      = unrec xesR
  (ids',s) = mapAccumL (letSubst h) ids (map fst xes)
  (nms,s') = unzip s
  xes'     = zip nms (map (substTms s' . unembed . snd) xes)
  e'       = substTms s' e

-- | Create a unique name and substitution for a let-binder
letSubst
  :: PureHeap
  -> Supply
  -> Id
  -> ( Supply
     , (TmOccName,(TmOccName,Term)))
letSubst h acc id_ =
  let nm = nameOcc (varName id_)
      (acc',nm') = uniqueInHeap h acc nm
  in  (acc',(nameOcc nm',(nm,Var (unembed (varType id_)) nm')))

-- | Create a name that's unique in the heap
uniqueInHeap
  :: PureHeap
  -> Supply
  -> TmOccName
  -> (Supply, TmName)
uniqueInHeap h ids nm =
  let (i,ids') = freshId ids
      nm'      = makeSystemName (Unbound.name2String nm) (toInteger i)
  in  case nameOcc nm' `M.member` h of
        True -> uniqueInHeap h ids' nm
        _    -> (ids',nm')
