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

import           Control.Arrow                           (second)
import           Control.Concurrent.Supply               (Supply, freshId)
import           Control.Lens                            (view, _4)
import           Data.Bits                               (shiftL)
import           Data.Either                             (lefts,rights)
import           Data.List
  (foldl',mapAccumL,uncons)
import           Data.IntMap                             (IntMap)
import qualified Data.Primitive.ByteArray                as BA
import qualified Data.Vector.Primitive                   as PV
import           Data.Text                               (Text)
import qualified Data.Text as Text
import           Data.Text.Prettyprint.Doc
import           Debug.Trace
import           GHC.Integer.GMP.Internals
  (Integer (..), BigNat (..))
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
import           Clash.Pretty

-- | The heap
data Heap = Heap GlobalHeap GPureHeap PureHeap Supply InScopeSet

type PureHeap = VarEnv Term
newtype GPureHeap = GPureHeap { unGPureHeap :: PureHeap }

-- | Global heap
type GlobalHeap = (IntMap Term, Int)

-- | The stack
type Stack = [StackFrame]

data StackFrame
  = Update Id
  | GUpdate Id
  | Apply  Id
  | Instantiate Type
  | PrimApply  Text PrimInfo [Type] [Value] [Term]
  | Scrutinise Type [Alt]
  | Tickish TickInfo
  deriving Show

instance ClashPretty StackFrame where
  clashPretty (Update i) = hsep ["Update", fromPpr i]
  clashPretty (GUpdate i) = hsep ["GUpdate", fromPpr i]
  clashPretty (Apply i) = hsep ["Apply", fromPpr i]
  clashPretty (Instantiate t) = hsep ["Instantiate", fromPpr t]
  clashPretty (PrimApply a b c d e) = do
    hsep ["PrimApply", fromPretty a, "::", fromPpr (primType b),
          "; type args=", fromPpr c,
          "; val args=", fromPpr (map valToTerm d),
          "term args=", fromPpr e]
  clashPretty (Scrutinise a b) =
    hsep ["Scrutinise ", fromPpr a,
          fromPpr (Case (Literal (CharLiteral '_')) a b)]
  clashPretty (Tickish sp) =
    hsep ["Tick", fromPpr sp]

mkTickish
  :: Stack
  -> [TickInfo]
  -> Stack
mkTickish s sps = map Tickish sps ++ s

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
  | PrimVal  Text PrimInfo [Type] [Value]
  -- ^ Clash's number types are represented by their "fromInteger#" primitive
  -- function. So some primitives are values.
  | Suspend Term
  -- ^ Used by lazy primitives
  deriving Show

-- | State of the evaluator
type State = (Heap, Stack, Term)

-- | Function that can evaluator primitives, i.e., perform delta-reduction
type PrimEvaluator =
  Bool -> -- Force special primitives? See [Note: forcing special primitives]
  TyConMap -> -- Type constructors
  Heap ->
  Stack ->
  Text -> -- Name of the primitive
  PrimInfo -> -- Type of the primitive
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
whnf' eval gbl0 tcm gh ids is isSubj e
  = case whnf eval tcm isSubj (Heap gh gbl1 emptyVarEnv ids is,[],e) of
      (Heap gh' _ ph' _ _,_,e') -> (gh',ph',e')
 where
  gbl1 = GPureHeap (mapVarEnv (view _4) gbl0)

-- | Evaluate to WHNF given an existing Heap and Stack
whnf
  :: PrimEvaluator
  -> TyConMap
  -> Bool
  -> State
  -> State
whnf eval tcm isSubj (h,k,e) =
    if isSubj
       then go (h,Scrutinise ty []:k,e) -- See [Note: empty case expressions]
       else go (h,k,e)
  where
    ty = termType tcm e

    go s = case step eval tcm s of
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
isScrut (Tickish {}:k) = isScrut k
isScrut _ = False

-- | Completely unwind the stack to get back the complete term
unwindStack :: State -> Maybe State
unwindStack s@(_,[],_) = Just s
unwindStack (h@(Heap gh gbl h' ids is),(kf:k'),e) = case kf of
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
                         [ "  "++ showDoc (clashPretty frame) | frame <- kf:k'] ++
                         [ ""
                         , "Expression:"
                         , showPpr e
                         , ""
                         , "Heap:"
                         , showDoc (clashPretty h')
                         ]
  Scrutinise _ [] ->
    unwindStack (h,k',e)
  Scrutinise ty alts ->
    unwindStack (h,k',Case e ty alts)
  Update x ->
    unwindStack (Heap gh gbl (extendVarEnv x e h') ids is,k',e)
  GUpdate _ ->
    unwindStack (h,k',e)
  Tickish sp ->
    unwindStack (h,k',Tick sp e)

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
  -> TyConMap
  -> State
  -> Maybe State
step eval tcm (h, k, e) = case e of
  Var v        -> force h k v
  (Lam x e')   -> unwind eval tcm h k (Lambda x e')
  (TyLam x e') -> unwind eval tcm h k (TyLambda x e')
  (Literal l)  -> unwind eval tcm h k (Lit l)
  (App e1 e2)
    | (Data dc,args,_ticks) <- collectArgsTicks e
    , (tys,_) <- splitFunForallTy (dcType dc)
    -> case compare (length args) (length tys) of
         EQ -> unwind eval tcm h k (DC dc args)
         LT -> let (tys',_) = splitFunForallTy (termType tcm e)
                   (h2,e')  = mkAbstr (h,e) tys'
               in  step eval tcm (h2,k,e')
         GT -> error "Overapplied DC"
    | (Prim nm pInfo,args,_ticks) <- collectArgsTicks e
    , let ty = primType pInfo
    , (tys,_) <- splitFunForallTy ty
    -> case compare (length args) (length tys) of
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
                  [a0,a1 ] | nm `elem` ["GHC.Classes.&&","GHC.Classes.||"] ->
                    let (h0,i) = newLetBinding tcm h  a0
                        (h1,j) = newLetBinding tcm h0 a1
                    in  eval (isScrut k) tcm h1 k nm pInfo []
                             [Suspend (Var i),Suspend (Var j)]
                  (e':es) ->
                    Just (h,PrimApply nm pInfo (rights args) [] es:k,e')
                  _ -> error "internal error"
         LT -> let (tys',_) = splitFunForallTy (termType tcm e)
                   (h2,e') = mkAbstr (h,e) tys'
               in  step eval tcm (h2,k,e')
         GT -> let (h2,id_) = newLetBinding tcm h e2
               in  Just (h2,Apply id_:k,e1)
  (TyApp e1 ty)
    | (Data dc,args,_ticks) <- collectArgsTicks e
    , (tys,_) <- splitFunForallTy (dcType dc)
    -> case compare (length args) (length tys) of
         EQ -> unwind eval tcm h k (DC dc args)
         LT -> let (tys',_) = splitFunForallTy (termType tcm e)
                   (h2,e') = mkAbstr (h,e) tys'
               in  step eval tcm (h2,k,e')
         GT -> error "Overapplied DC"
    | (Prim nm pInfo,args,_ticks) <- collectArgsTicks e
    , let ty' = primType pInfo
    , (tys,_) <- splitFunForallTy ty'
    -> case compare (length args) (length tys) of
         EQ -> case lefts args of
              [] | nm `elem` ["Clash.Transformations.removedArg"]
                 -- The above primitives are actually values, and not operations.
                 -> unwind eval tcm h k (PrimVal nm pInfo (rights args) [])
                 | otherwise
                 -> eval (isScrut k) tcm h k nm pInfo (rights args) []
              (e':es) -> Just (h,PrimApply nm pInfo (rights args) [] es:k,e')
         LT -> let (tys',_) = splitFunForallTy (termType tcm e)
                   (h2,e') = mkAbstr (h,e) tys'
               in  step eval tcm (h2,k,e')
         GT -> Just (h,Instantiate ty:k,e1)
  (Data dc) -> unwind eval tcm h k (DC dc [])
  (Prim nm pInfo)
    | nm `elem` ["GHC.Prim.realWorld#"]
    -> unwind eval tcm h k (PrimVal nm pInfo [] [])
    | otherwise
    , let ty' = primType pInfo
    -> case fst (splitFunForallTy ty')  of
        []  -> eval (isScrut k) tcm h k nm pInfo [] []
        tys -> let (h2,e') = mkAbstr (h,e) tys
               in  step eval tcm (h2,k,e')
  (App e1 e2)  -> let (h2,id_) = newLetBinding tcm h e2
                  in  Just (h2,Apply id_:k,e1)
  (TyApp e1 ty) -> Just (h,Instantiate ty:k,e1)
  (Case scrut ty alts) -> Just (h,Scrutinise ty alts:k,scrut)
  (Letrec bs e') -> Just (allocate h k bs e')
  Tick sp e' -> Just (h,Tickish sp:k,e')
  Cast _ _ _ -> trace (unlines ["WARNING: " ++ $(curLoc) ++ "Clash currently can't symbolically evaluate casts"
                                    ,"If you have testcase that produces this message, please open an issue about it."]) Nothing

newLetBinding
  :: TyConMap
  -> Heap
  -> Term
  -> (Heap,Id)
newLetBinding tcm h@(Heap gh gbl h' ids is0) e
  | Var v <- e
  , Just _ <- lookupVarEnv v h'
  = (h, v)
  | otherwise
  = (Heap gh gbl (extendVarEnv id_ e h') ids' is1,id_)
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
    go (Right ty) (Heap gh gbl h ids is,e) =
      let ((ids',_),id_) = mkUniqSystemId (ids,is) ("x",ty)
      in  (Heap gh gbl h ids' is,Lam id_ (App e (Var id_)))

-- | Force the evaluation of a variable.
force :: Heap -> Stack -> Id -> Maybe State
force (Heap gh g@(GPureHeap gbl) h ids is) k x' = case lookupVarEnv x' h of
    Nothing -> case lookupVarEnv x' gbl of
      Just e | isGlobalId x'
        -> let e' = tickExpr e
            in Just ( Heap gh (GPureHeap (delVarEnv gbl x')) h ids is
                , GUpdate x':k
                , deShadowTerm is e'
                )
      _ -> Nothing
    Just e -> let e' = tickExpr e
               in Just (Heap gh g (delVarEnv h x') ids is,Update x':k,e')
    -- Removing the heap-bound value on a force ensures we do not get stuck on
    -- expressions such as: "let x = x in x"
 where
  tickExpr = Tick (NameMod PrefixName (LitTy . SymTy $ toStr x'))
  unQualName = snd . Text.breakOnEnd "."
  toStr = Text.unpack . unQualName . flip Text.snoc '_' . nameOcc . varName

-- | Unwind the stack by 1
unwind
  :: PrimEvaluator
  -> TyConMap
  -> Heap -> Stack -> Value -> Maybe State
unwind eval tcm h k v = do
  (kf,k') <- uncons k
  case kf of
    Update x                     -> return (update h k' x v)
    GUpdate x                    -> return (gupdate h k' x v)
    Apply x                      -> return (apply  h k' v x)
    Instantiate ty               -> return (instantiate h k' v ty)
    PrimApply nm ty tys vals tms -> primop eval tcm h k' nm ty tys vals v tms
    Scrutinise _ alts            -> return (scrutinise h k' v alts)
    -- Adding back the Tick constructor will make the evaluator loop
    Tickish _                    -> return (h,k',valToTerm v)

-- | Update the Heap with the evaluated term
update :: Heap -> Stack -> Id -> Value -> State
update (Heap gh gbl h ids is) k x v = (Heap gh gbl (extendVarEnv x v' h) ids is,k,v')
  where
    v' = valToTerm v

-- | Update the Globals with the evaluated term
gupdate :: Heap -> Stack -> Id -> Value -> State
gupdate (Heap gh (GPureHeap gbl) h ids is) k x v =
  (Heap gh (GPureHeap (extendVarEnv x v' gbl)) h ids is,k,v')
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
  Suspend e            -> e

toVar :: Id -> Term
toVar x = Var x

toType :: TyVar -> Type
toType x = VarTy x

-- | Apply a value to a function
apply :: Heap -> Stack -> Value -> Id -> State
apply h@(Heap _ _ _ _ is0) k (Lambda x' e) x = (h,k,substTm "Evaluator.apply" subst e)
 where
  subst  = extendIdSubst subst0 x' (Var x)
  subst0 = mkSubst (extendInScopeSet is0 x)
apply _ _ _ _ = error "not a lambda"

-- | Instantiate a type-abstraction
instantiate :: Heap -> Stack -> Value -> Type -> State
instantiate h k (TyLambda x e) ty = (h,k,substTm "Evaluator.instantiate" subst e)
 where
  subst  = extendTvSubst subst0 x ty
  subst0 = mkSubst is0
  is0    = mkInScopeSet (localFVsOfTerms [e] `unionUniqSet` tyFVsOfTypes [ty])
instantiate _ _ _ _ = error "not a ty lambda"

naturalLiteral :: Value -> Maybe Integer
naturalLiteral v =
  case v of
    Lit (NaturalLiteral i) -> Just i
    DC dc [Left (Literal (WordLiteral i))]
      | dcTag dc == 1
      -> Just i
    DC dc [Left (Literal (ByteArrayLiteral (PV.Vector _ _ (BA.ByteArray ba))))]
      | dcTag dc == 2
      -> Just (Jp# (BN# ba))
    _ -> Nothing

integerLiteral :: Value -> Maybe Integer
integerLiteral v =
  case v of
    Lit (IntegerLiteral i) -> Just i
    DC dc [Left (Literal (IntLiteral i))]
      | dcTag dc == 1
      -> Just i
    DC dc [Left (Literal (ByteArrayLiteral (PV.Vector _ _ (BA.ByteArray ba))))]
      | dcTag dc == 2
      -> Just (Jp# (BN# ba))
      | dcTag dc == 3
      -> Just (Jn# (BN# ba))
    _ -> Nothing

-- | Evaluation of primitive operations
primop
  :: PrimEvaluator
  -> TyConMap
  -> Heap
  -> Stack
  -> Text
  -- ^ Name of the primitive
  -> PrimInfo
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
primop eval tcm h k nm ty tys vs v []
  | nm `elem` ["Clash.Sized.Internal.Index.fromInteger#"
              ,"GHC.CString.unpackCString#"
              ,"Clash.Transformations.removedArg"
              ,"GHC.Prim.MutableByteArray#"
              ]
              -- The above primitives are actually values, and not operations.
  = unwind eval tcm h k (PrimVal nm ty tys (vs ++ [v]))
  | nm == "Clash.Sized.Internal.BitVector.fromInteger#"
  = case (vs,v) of
    ([naturalLiteral -> Just n,mask], integerLiteral -> Just i) ->
      unwind eval tcm h k (PrimVal nm ty tys [Lit (NaturalLiteral n)
                                             ,mask
                                             ,Lit (IntegerLiteral (wrapUnsigned n i))])
    _ -> error ($(curLoc) ++ "Internal error"  ++ show (vs,v))
  | nm == "Clash.Sized.Internal.BitVector.fromInteger##"
  = case (vs,v) of
    ([mask], integerLiteral -> Just i) ->
      unwind eval tcm h k (PrimVal nm ty tys [mask
                                             ,Lit (IntegerLiteral (wrapUnsigned 1 i))])
    _ -> error ($(curLoc) ++ "Internal error"  ++ show (vs,v))
  | nm == "Clash.Sized.Internal.Signed.fromInteger#"
  = case (vs,v) of
    ([naturalLiteral -> Just n],integerLiteral -> Just i) ->
      unwind eval tcm h k (PrimVal nm ty tys [Lit (NaturalLiteral n)
                                             ,Lit (IntegerLiteral (wrapSigned n i))])
    _ -> error ($(curLoc) ++ "Internal error"  ++ show (vs,v))
  | nm == "Clash.Sized.Internal.Unsigned.fromInteger#"
  = case (vs,v) of
    ([naturalLiteral -> Just n],integerLiteral -> Just i) ->
      unwind eval tcm h k (PrimVal nm ty tys [Lit (NaturalLiteral n)
                                             ,Lit (IntegerLiteral (wrapUnsigned n i))])
    _ -> error ($(curLoc) ++ "Internal error"  ++ show (vs,v))
  | otherwise = eval (isScrut k) tcm h k nm ty tys (vs ++ [v])
primop eval tcm h0 k nm ty tys vs v [e]
  | nm `elem` [ "Clash.Sized.Vector.lazyV"
              , "Clash.Sized.Vector.replicate"
              , "Clash.Sized.Vector.replace_int"
              , "GHC.Classes.&&"
              , "GHC.Classes.||"
              ]
  = let (h1,i) = newLetBinding tcm h0 e
    in  eval (isScrut k) tcm h1 k nm ty tys (vs ++ [v,Suspend (Var i)])
primop _ _ h k nm ty tys vs v (e:es) =
  Just (h,PrimApply nm ty tys (vs ++ [v]) es:k,e)

-- | Evaluate a case-expression
scrutinise :: Heap -> Stack -> Value -> [Alt] -> State
scrutinise h k v [] = (h,k,valToTerm v)
-- [Note: empty case expressions]
--
-- Clash does not have empty case-expressions; instead, empty case-expressions
-- are used to indicate that the `whnf` function was called the context of a
-- case-expression, which means certain special primitives must be forced.
-- See also [Note: forcing special primitives]
scrutinise h k (Lit l) alts = case alts of
  (DefaultPat,altE):alts1 -> (h,k,go altE alts1)
  _ -> (h,k,go (error ("scrutinise: no match " ++
          showPpr (Case (valToTerm (Lit l)) (ConstTy Arrow) alts))) alts)
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
              bv = PV.Vector 0 (BA.sizeofByteArray ba1) ba1
          in  Just (ByteArrayLiteral bv)
       3 | l1 < ((-2)^(63::Int)) ->
          let !(Jn# !(BN# ba0)) = l1
              ba1 = BA.ByteArray ba0
              bv = PV.Vector 0 (BA.sizeofByteArray ba1) ba1
          in  Just (ByteArrayLiteral bv)
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
              bv = PV.Vector 0 (BA.sizeofByteArray ba1) ba1
          in  Just (ByteArrayLiteral bv)
       _ -> Nothing
    = let inScope = localFVsOfTerms [altE]
          subst0  = mkSubst (mkInScopeSet inScope)
          subst1  = extendIdSubst subst0 x (Literal patE)
      in  substTm "Evaluator.scrutinise" subst1 altE
  go def (_:alts1) = go def alts1

scrutinise h k (DC dc xs) alts
  | altE:_ <- [substAlt altDc tvs pxs xs altE
              | (DataPat altDc tvs pxs,altE) <- alts, altDc == dc ] ++
              [altE | (DefaultPat,altE) <- alts ]
  = (h,k,altE)

scrutinise h k v@(PrimVal nm _ _ vs) alts
  | any (\case {(LitPat {},_) -> True; _ -> False}) alts
  = case alts of
      ((DefaultPat,altE):alts1) -> (h,k,go altE alts1)
      _ -> (h,k,go (error ("scrutinise: no match " ++
                showPpr (Case (valToTerm v) (ConstTy Arrow) alts))) alts)
 where
  go def [] = def
  go _   ((LitPat l1,altE):_) | l1 == l = altE
  go def (_:alts1) = go def alts1

  l = case nm of
        "Clash.Sized.Internal.BitVector.fromInteger#"
          | [_,Lit (IntegerLiteral 0),Lit l0] <- vs -> l0
        "Clash.Sized.Internal.Index.fromInteger#"
          | [_,Lit l0] <- vs -> l0
        "Clash.Sized.Internal.Signed.fromInteger#"
          | [_,Lit l0] <- vs -> l0
        "Clash.Sized.Internal.Unsigned.fromInteger#"
          | [_,Lit l0] <- vs -> l0
        _ -> error ("scrutinise: " ++ showPpr (Case (valToTerm v) (ConstTy Arrow) alts))

scrutinise _ _ v alts = error ("scrutinise: " ++ showPpr (Case (valToTerm v) (ConstTy Arrow) alts))

substAlt :: DataCon -> [TyVar] -> [Id] -> [Either Term Type] -> Term -> Term
substAlt dc tvs xs args e = substTm "Evaluator.substAlt" subst e
 where
  tys        = rights args
  tms        = lefts args
  substTyMap = zip tvs (drop (length (dcUnivTyVars dc)) tys)
  substTmMap = zip xs tms
  inScope    = tyFVsOfTypes tys `unionVarSet` localFVsOfTerms (e:tms)
  subst      = extendTvSubstList (extendIdSubstList subst0 substTmMap) substTyMap
  subst0     = mkSubst (mkInScopeSet inScope)

-- | Allocate let-bindings on the heap
allocate :: Heap -> Stack -> [LetBinding] -> Term -> State
allocate (Heap gh gbl h ids is0) k xes e =
  (Heap gh gbl (h `extendVarEnvList` xes') ids' isN,k,e')
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

wrapUnsigned :: Integer -> Integer -> Integer
wrapUnsigned n i = i `mod` sz
 where
  sz = 1 `shiftL` fromInteger n

wrapSigned :: Integer -> Integer -> Integer
wrapSigned n i = if mask == 0 then 0 else res
 where
  mask = 1 `shiftL` fromInteger (n - 1)
  res  = case divMod i mask of
           (s,i1) | even s    -> i1
                  | otherwise -> i1 - mask
