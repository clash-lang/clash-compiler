{-|
  Copyright   :  (C) 2017-2022, Google Inc.,
                     2021-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  The call-by-need whnf evaluator of scoped Clash Core: a port of the machine
  of "Clash.GHC.Evaluator", which is based on the evaluator described in:

  Maximilian Bolingbroke, Simon Peyton Jones, "Supercompilation by
  evaluation", Haskell '10, Baltimore, Maryland, USA.

  See "Clash.Core.Scoped.Evaluator.Types" for how the machine is scoped. The
  differences with the machine of Clash Core that scoping forces or allows:

  * __No deshadowing on a heap lookup.__ The machine of Clash Core deshadows
    every term it takes from a heap, because it then copies it into the term
    under evaluation. Here, binders are renamed when they are opened, if they
    clash with the scope, so a heap lookup is a lookup.

  * __Allocation extends the scope.__ Let-binders are opened
    ('Clash.Core.Scoped.Subst.openBind'), which renames them only if they
    clash with the scope of the machine, and the whole machine moves into the
    extended scope ('extendMachine'). A step that holds on to a value across
    an allocation, e.g. the function of an application whose argument it
    let-binds, has to 'sink' it. There is no unique supply nor in-scope set:
    new binders are fresh for the scope of the machine, which includes the
    variables of the caller.

  * __Local and global variables are different.__ A local variable is looked
    up in the local heap, a global one in the global heap.

  * __Nested evaluation is visible in the types.__ A primitive that evaluates
    an argument with a nested 'whnf' gets the resulting machine in an
    extension of its scope, and has to continue in /that/ machine: continuing
    in the original one with a term from the nested one is a type error. See
    @(&&)@ in "Clash.Core.Scoped.Evaluator.Primitives".

  Primitives are reduced by the 'PrimReductions' of the 'Env': those ported to
  scoped Core ("Clash.Core.Scoped.Evaluator.Primitives"), and the others with
  the evaluator of Clash Core ("Clash.Core.Scoped.Evaluator.Adapter").
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Scoped.Evaluator
  ( -- * Running the machine
    whnf
  , step
  , unwind
  , unwindStack
  , bindAllocated
  , allocatedBind
    -- * Heaps
  , newLetBinding
  , heapLookupLocal
    -- * Primitives
  , primStep
  , reduceIn
  , reduce
  , reduceWHNF
  , primResultType
  , isUndefinedPrimVal
  , isUndefinedXPrimVal
  , integerLiteral
  , naturalLiteral
  , nameModTick
  ) where

import qualified Data.Either as Either
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Primitive.ByteArray as BA
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Extra (showt)
import GHC.Exts (MutableByteArray#)
import GHC.Num.Integer (Integer (..))
import GHC.Stack (HasCallStack)

import qualified GHC.Classes
import qualified GHC.CString
import qualified GHC.Prim

import qualified Clash.Sized.Internal.BitVector
import qualified Clash.Sized.Internal.Index
import qualified Clash.Sized.Internal.Signed
import qualified Clash.Sized.Internal.Unsigned
import qualified Clash.Sized.Vector

import Clash.Core.DataCon (DataCon (..))
import Clash.Core.Literal (Literal (..))
import qualified Clash.Core.Name as C
import Clash.Core.Pretty (showPpr)
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C
import Clash.Core.Util (undefinedPrims, undefinedXPrims)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Debug (trace)
import Clash.Driver.Types (Binding (..))
import qualified Clash.Normalize.Primitives as NP
import Clash.Util (curLoc)
import Clash.XException (isX)

import Clash.Core.Scoped.Convert (fromClashTermClosedIn)
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.HasType
  (applyTypeToArgs, inferCoreTypeOf, piResultTys, primInfoType)
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Pretty ()
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (collectArgsTicks)
import Clash.Core.Scoped.Type (TypeView (..), tyView)

-- * Running the machine

-- | Evaluate the term of a machine to WHNF, and give the machine with the
-- result, its stack unwound. When the term is the subject of a
-- case-expression, special primitives are forced, see
-- [Note: forcing special primitives] in "Clash.GHC.Evaluator". See
-- 'Clash.Core.Evaluator.Types.whnf'.
whnf
  :: forall o n r
   . Distinct n
  => Env
  -> Bool
  -- ^ Whether the term is the subject of a case-expression
  -> Machine o n
  -> (forall l. DExt n l => Machine o l -> r)
  -> r
whnf env isSubj m0 k
  | isSubj =
      -- See [Note: empty case expressions] in "Clash.GHC.Evaluator"
      let ty = inferCoreTypeOf (envTypeEnv env) (mScope m0) (mTerm m0)
      in go (m0 {mStack = Scrutinise ty [] : mStack m0})
  | otherwise = go m0
 where
  go :: forall l. DExt n l => Machine o l -> r
  go m = case step env m of
    Just (Next (m' :: Machine o l')) -> withExtTrans @n @l @l' (go m')
    Nothing -> k (unwindStack m)

-- | Pop all frames, rebuilding the term. See
-- 'Clash.Core.Evaluator.Types.unwindStack'.
unwindStack :: HasCallStack => Machine o n -> Machine o n
unwindStack m = case mStack m of
  [] -> m
  f : rest ->
    let m' = m {mStack = rest}
        t = mTerm m
    in unwindStack $ case f of
      PrimApply p tys vs tms ->
        m' {mTerm = List.foldl' App
                      (List.foldl' App (List.foldl' TyApp (Prim p) tys) (map valToTerm vs))
                      (t : tms)}
      Instantiate ty -> m' {mTerm = TyApp t ty}
      Apply (HeapVar occ _ x) -> case heapLookupLocal x m' of
        Just e -> m' {mTerm = App t e}
        Nothing -> error ($(curLoc) ++ "Apply of a variable without a heap binding: "
                            ++ show occ)
      Scrutinise _ [] -> m'
      Scrutinise ty alts -> m' {mTerm = Case t ty alts}
      UpdateLocal x -> m' {mHeapLocal = insertNameMap x t (mHeapLocal m')}
      UpdateGlobal _ -> m'
      Tickish tk -> m' {mTerm = Tick tk t}

-- | The term of a machine, bound by the bindings the machine allocated: the
-- result lives in the scope of the caller. The binders of the @letrec@ are
-- exactly the names of the machine's scope that are not in the caller's, so
-- this can't leave a heap variable unbound, nor capture anything.
bindAllocated :: (HasCallStack, Distinct n) => Machine o n -> Term n -> Term o
bindAllocated m e = case mAlloc m of
  ANil -> e
  ASnoc {} -> Let (allocatedBind m) e

-- | The bindings the machine allocated, as a @letrec@, with their right-hand
-- sides in the local heap
allocatedBind :: (HasCallStack, Distinct n) => Machine o n -> Bind o n
allocatedBind m =
  let tele = allocTele (mAlloc m)
      rhs = \case
        Var occ _ x -> case heapLookupLocal x m of
          Just r -> r
          Nothing ->
            error ($(curLoc) ++ "allocated binder not in the heap: " ++ show occ)
        _ -> error ($(curLoc) ++ "not a variable")
  in Rec tele (map rhs (teleIdVars tele))

-- * Heaps

-- | The binding of a local variable in the local heap
heapLookupLocal :: TmName n -> Machine o n -> Maybe (Term n)
heapLookupLocal x m = lookupNameMap x (mHeapLocal m)
{-# INLINE heapLookupLocal #-}

-- | Let-bind a term on the local heap, unless it is a variable that is bound
-- there already. The continuation gets the machine in the extended scope.
-- See 'Clash.GHC.Evaluator.newLetBinding'.
newLetBinding
  :: forall o n r
   . Distinct n
  => Env
  -> Machine o n
  -> Term n
  -> (forall l. DExt n l => Machine o l -> HeapVar l -> r)
  -> r
newLetBinding env m e k
  | Var occ ty x <- e
  , memberNameMap x (mHeapLocal m)
  = k m (HeapVar occ ty x)
  | otherwise
  = withFreshIdBinder (mScope m) xName (inferCoreTypeOf (envTypeEnv env) (mScope m) e) go
 where
  go :: forall l. DExt n l => Scope l -> IdBinder n l -> r
  go sc b =
    let m1 = extendMachine sc b m
    in k m1 {mHeapLocal = insertNameMap (tmNameOf b) (sink e) (mHeapLocal m1)}
         (idBinderHeapVar b)

-- | The name of new heap binders, see 'Clash.Core.Util.mkUniqSystemId'
xName :: C.Name C.Term
xName = C.mkUnsafeSystemName "x" 0

-- | Allocate let-bindings on the heap. See 'Clash.GHC.Evaluator.allocate'.
allocate :: forall o n l. Distinct n => Machine o n -> Bind n l -> Term l -> Next o n
allocate m bs0 e0 = openBind (mScope m) bs0 e0 go
 where
  go :: forall l'. DExt n l' => Scope l' -> Bind n l' -> Term l' -> Next o n
  go sc bs e = case bs of
    NonRec b rhs ->
      let m1 = extendMachine sc b m
      in Next m1 { mHeapLocal = insertNameMap (tmNameOf b) (sink rhs) (mHeapLocal m1)
                 , mTerm = e }
    Rec bndrs rhss ->
      let m1 = extendMachineTele sc bndrs m
          heap = List.foldl' (\h (x, r) -> insertNameMap x r h) (mHeapLocal m1)
                   (zip (map varName (teleIdVars bndrs)) rhss)
      in Next m1 {mHeapLocal = heap, mTerm = e}

  varName :: Term x -> TmName x
  varName = \case
    Var _ _ x -> x
    _ -> error ($(curLoc) ++ "not a variable")

-- * Steps

-- | A step of the machine. 'Nothing' if the term can't be reduced, and the
-- stack can't be unwound. See 'Clash.GHC.Evaluator.ghcStep'.
step :: forall o n. Distinct n => Env -> Machine o n -> Maybe (Next o n)
step env m = case mTerm m of
  Var occ _ x -> stepVar occ x
  GlobalVar g -> stepGlobal g
  Data dc -> unwind env (DC dc []) m
  Literal l -> unwind env (Lit l) m
  Prim p -> stepPrim env p m
  Lam b e -> unwind env (Lambda b e) m
  TyLam b e -> unwind env (TyLambda b e) m
  App x y -> stepApp env x y m
  TyApp x ty -> stepTyApp env x ty m
  Let bs e -> Just (allocate m bs e)
  Case s ty alts -> Just (stay m {mStack = Scrutinise ty alts : mStack m, mTerm = s})
  Cast {} ->
    -- TODO Support stepwise evaluation of casts.
    flip trace Nothing $ unlines
      [ "WARNING: " <> $(curLoc) <> "Clash can't symbolically evaluate casts"
      , "Please file an issue at https://github.com/clash-lang/clash-compiler/issues"
      ]
  Tick t x -> Just (stay m {mStack = Tickish t : mStack m, mTerm = x})
 where
  -- Remove the binding while it is evaluated, so we don't get stuck on
  -- expressions such as "let x = x in x". No deshadowing: see the module
  -- header.
  stepVar occ x = case heapLookupLocal x m of
    Just e -> Just $ stay m
      { mHeapLocal = deleteNameMap x (mHeapLocal m)
      , mStack = UpdateLocal x : mStack m
      , mTerm = Tick (nameModTick occ) e
      }
    Nothing -> Nothing

  stepGlobal g
    | memberGlobalSet g (mHeapGlobalDeleted m) = Nothing
    | Just e <- lookupGlobalEnv g (mHeapGlobalOverlay m) = force e
    | Just b <- UniqMap.lookup (globalIdUniq g) (mHeapGlobalBase m) =
        -- Globals are closed: converting one into the machine's scope numbers
        -- its binders above the scope. Like the deshadowing the machine of
        -- Clash Core does here, this copies the term.
        force (fromClashTermClosedIn (mScope m) (bindingTerm b))
    | otherwise = Nothing
   where
    force e = Just $ stay m
      { mHeapGlobalOverlay = deleteGlobalEnv g (mHeapGlobalOverlay m)
      , mHeapGlobalDeleted = insertGlobalSet g (mHeapGlobalDeleted m)
      , mStack = UpdateGlobal g : mStack m
      , mTerm = Tick (nameModTick (C.varName (globalIdVar g))) e
      }

-- | The tick that names the result of evaluating a variable after it. See
-- 'Clash.GHC.Evaluator.stepVar'.
nameModTick :: C.Name a -> TickInfo n
nameModTick occ = NameMod C.PrefixName (LitTy (C.SymTy (toStr occ)))
 where
  unQualName = snd . Text.breakOnEnd "."
  toStr = Text.unpack . unQualName . flip Text.snoc '_' . C.nameOcc

-- | The number of arguments of a closed type: type and term arguments
closedArity :: C.Type -> Int
closedArity = length . fst . C.splitFunForallTy

stepPrim :: Distinct n => Env -> C.PrimInfo -> Machine o n -> Maybe (Next o n)
stepPrim env p m
  | C.primName p == showt 'GHC.Prim.realWorld# = unwind env (PrimVal p [] []) m
  | closedArity (C.primType p) == 0 = primStep env (forcePrims m) p [] [] m
  | otherwise = newBinder env m (Prim p)

stepApp
  :: forall o n. Distinct n
  => Env -> Term n -> Term n -> Machine o n -> Maybe (Next o n)
stepApp env x y m = case collectArgsTicks (App x y) of
  (Data dc, args, _) -> case compare (length args) (closedArity (dcType dc)) of
    EQ -> unwind env (DC dc args) m
    LT -> newBinder env m (App x y)
    GT -> error "Overapplied DC"
  (Prim p, args, _) -> case compare (length args) (closedArity (C.primType p)) of
    EQ -> case Either.lefts args of
      -- We make boolean conjunction and disjunction extra lazy by deferring
      -- the evaluation of the arguments during the evaluation of the primop
      -- rule. See 'Clash.GHC.Evaluator.stepApp'.
      [a0, a1] | C.primName p `elem` [andName, orName] ->
        newLetBinding env m a0 $ \m0 i ->
          newLetBinding env m0 (sink a1) $ \m1 j ->
            nextFrom <$>
              primStep env (forcePrims m) p (sink1 (Either.rights args))
                [Suspend (sink (heapVarTerm i)), Suspend (heapVarTerm j)] m1
      e' : es
        | C.primName p `elem` (undefinedXPrims ++ undefinedPrims)
        -- These primitives are (bottoming) values, whose arguments are never
        -- used anywhere in the rest of the compiler. So instead of pushing a
        -- PrimApply frame on the stack to evaluate those arguments, we just
        -- unwind the stack with the primitive value and leave its arguments
        -- in an unevaluated state (Suspend).
        -> unwind env (PrimVal p (Either.rights args) (map Suspend (e' : es))) m
        | otherwise
        -> Just (stay m { mStack = PrimApply p (Either.rights args) [] es : mStack m
                        , mTerm = e' })
      [] -> error ($(curLoc) ++ "internal error")
    LT -> newBinder env m (App x y)
    GT -> applyTo
  _ -> applyTo
 where
  applyTo = newLetBinding env m y $ \m1 hv ->
    Just (nextFrom (stay m1 {mTerm = sink x, mStack = Apply hv : mStack m1}))

stepTyApp
  :: Distinct n => Env -> Term n -> Type n -> Machine o n -> Maybe (Next o n)
stepTyApp env x ty m = case collectArgsTicks (TyApp x ty) of
  (Data dc, args, _) -> case compare (length args) (closedArity (dcType dc)) of
    EQ -> unwind env (DC dc args) m
    LT -> newBinder env m (TyApp x ty)
    GT -> error "Overapplied DC"
  (Prim p, args, _) -> case compare (length args) (closedArity (C.primType p)) of
    EQ -> case Either.lefts args of
      []
        | C.primName p `elem` map C.primName [NP.removedArg, NP.undefined, NP.undefinedX]
        -> unwind env (PrimVal p (Either.rights args) []) m
        | otherwise
        -> primStep env (forcePrims m) p (Either.rights args) [] m
      e' : es ->
        Just (stay m { mStack = PrimApply p (Either.rights args) [] es : mStack m
                     , mTerm = e' })
    LT -> newBinder env m (TyApp x ty)
    GT -> Just (stay m {mStack = Instantiate ty : mStack m, mTerm = x})
  _ -> Just (stay m {mStack = Instantiate ty : mStack m, mTerm = x})

-- | Eta-expand a partially applied data constructor or primitive, and step
-- the resulting abstraction. See 'Clash.GHC.Evaluator.newBinder'.
newBinder :: Distinct n => Env -> Machine o n -> Term n -> Maybe (Next o n)
newBinder env m e0 =
  step env m {mTerm = go (mScope m) e0 (inferCoreTypeOf (envTypeEnv env) (mScope m) e0)}
 where
  go :: Distinct x => Scope x -> Term x -> Type x -> Term x
  go sc e ty = case ty of
    ForAllTy b res ->
      substTyBinder (idSubst sc) b $ \s' b' ->
        TyLam b' (go (substScope s') (TyApp (sink e) (tyBinderVar b')) (substTy s' res))
    (tyView -> FunTy a r) ->
      withFreshIdBinder sc xName a $ \sc' b ->
        Lam b (go sc' (App (sink e) (idBinderVar b)) (sink r))
    _ -> e

andName, orName :: Text
andName = showt '(GHC.Classes.&&)
orName = showt '(GHC.Classes.||)

-- * Unwinding

-- | Unwind the stack by one frame. See 'Clash.GHC.Evaluator.ghcUnwind'.
unwind :: Distinct n => Env -> Value n -> Machine o n -> Maybe (Next o n)
unwind env v m = case mStack m of
  [] -> Nothing
  f : rest ->
    let m' = m {mStack = rest}
    in case f of
      UpdateLocal x ->
        let t = valToTerm v
        in Just (stay m' {mHeapLocal = insertNameMap x t (mHeapLocal m'), mTerm = t})
      UpdateGlobal g ->
        let t = valToTerm v
        in Just (stay m'
             { mHeapGlobalOverlay = insertGlobalEnv g t (mHeapGlobalOverlay m')
             , mHeapGlobalDeleted = deleteGlobalSet g (mHeapGlobalDeleted m')
             , mTerm = t })
      Apply hv -> Just (stay (apply env v hv m'))
      Instantiate ty -> Just (stay (instantiate env v ty m'))
      PrimApply p tys vs tms -> primUnwind env p tys vs v tms m'
      Scrutinise ty alts -> Just (stay m' {mTerm = scrutinise (mScope m') v ty alts})
      Tickish _ -> Just (stay m' {mTerm = valToTerm v})

-- | Apply a value to a heap variable. See 'Clash.GHC.Evaluator.apply'.
apply :: Distinct n => Env -> Value n -> HeapVar n -> Machine o n -> Machine o n
apply env v hv@(HeapVar _ hvTy _) m = case v of
  Lambda b e -> m {mTerm = substTm (extendTmSubst (idSubst sc) b (heapVarTerm hv)) e}
  PrimVal p tys vs
    | isUndefinedXPrimVal v -> m {mTerm = TyApp (Prim NP.undefinedX) (resTy p tys vs)}
    | isUndefinedPrimVal v -> m {mTerm = TyApp (Prim NP.undefined) (resTy p tys vs)}
  _ -> error ($(curLoc) ++ "Not a lambda: " ++ show v)
 where
  sc = mScope m
  te = envTypeEnv env
  resTy p tys vs =
    piResultTys te sc (primInfoType sc p)
      (tys ++ map (inferCoreTypeOf te sc . valToTerm) vs ++ [hvTy])

-- | Instantiate a type abstraction. See 'Clash.GHC.Evaluator.instantiate'.
instantiate :: Distinct n => Env -> Value n -> Type n -> Machine o n -> Machine o n
instantiate env v ty m = case v of
  TyLambda b e -> m {mTerm = substTm (extendTySubst (idSubst sc) b ty) e}
  -- The evaluator is set up in such a way that under normal conditions
  -- anything of type 'forall a . ty' must be a ty-lambda. However, sometimes
  -- we evaluate to an error /value/, see 'Clash.GHC.Evaluator.instantiate'.
  PrimVal p tys es
    | isUndefinedXPrimVal v -> m {mTerm = TyApp (Prim NP.undefinedX) (resTy p tys es)}
    | isUndefinedPrimVal v -> m {mTerm = TyApp (Prim NP.undefined) (resTy p tys es)}
  _ -> error ($(curLoc) ++ "Not a tylambda: " ++ show v)
 where
  sc = mScope m
  te = envTypeEnv env
  resTy p tys es =
    piResultTys te sc (primInfoType sc p)
      (tys ++ map (inferCoreTypeOf te sc . valToTerm) es ++ [ty])

-- | Select the alternative of a case-expression. See
-- 'Clash.GHC.Evaluator.scrutinise'.
scrutinise
  :: (HasCallStack, Distinct n) => Scope n -> Value n -> Type n -> [Alt n] -> Term n
-- An empty case-expression indicates that 'whnf' was called in the context of
-- a case-expression, see [Note: empty case expressions] in
-- "Clash.GHC.Evaluator".
scrutinise _ v _ [] = valToTerm v
scrutinise sc (Lit l) _ alts = case alts of
  Alt DefaultPat e : alts1 -> go e alts1
  _ -> go (error ($(curLoc) ++ "no match")) alts
 where
  go def [] = def
  go _ (Alt (LitPat l1) e : _) | l1 == l = e
  go def (Alt (DataPat dc TNil (TCons x TNil)) e : rest)
    | Just patE <- litConField (dcTag dc) l
    = substTm (extendTmSubst (idSubst sc) x (Literal patE)) e
    | otherwise = go def rest
  go def (_ : rest) = go def rest
scrutinise sc (DC dc xs) _ alts
  | altE : _ <- [ substInAlt sc dc tvs pxs xs altE
                | Alt (DataPat altDc tvs pxs) altE <- alts, altDc == dc ]
             ++ [ altE | Alt DefaultPat altE <- alts ]
  = altE
scrutinise _ v@(PrimVal p _ vs) altTy alts
  | isUndefinedXPrimVal v = TyApp (Prim NP.undefinedX) altTy
  | isUndefinedPrimVal v = TyApp (Prim NP.undefined) altTy
  | any isLitAlt alts = case alts of
      Alt DefaultPat e : alts1 -> go e alts1
      _ -> go (error ($(curLoc) ++ "no match")) alts
 where
  isLitAlt (Alt (LitPat _) _) = True
  isLitAlt _ = False
  go def [] = def
  go _ (Alt (LitPat l1) e : _) | l1 == l = e
  go def (_ : rest) = go def rest
  l = case C.primName p of
    "Clash.Sized.Internal.BitVector.fromInteger##"
      | [Lit (WordLiteral 0), Lit l0] <- vs -> l0
    "Clash.Sized.Internal.BitVector.fromInteger#"
      | [_, Lit (NaturalLiteral 0), Lit l0] <- vs -> l0
    "Clash.Sized.Internal.Index.fromInteger#"
      | [_, Lit l0] <- vs -> l0
    "Clash.Sized.Internal.Signed.fromInteger#"
      | [_, Lit l0] <- vs -> l0
    "Clash.Sized.Internal.Unsigned.fromInteger#"
      | [_, Lit l0] <- vs -> l0
    _ -> error ($(curLoc) ++ "scrutinise: " ++ show (C.primName p))
scrutinise _ v _ _ = error ($(curLoc) ++ "scrutinise: " ++ show v)

-- | The field of the constructor of 'Integer' or 'Natural' with the given
-- tag, for a literal. See 'Clash.GHC.Evaluator.scrutinise'.
litConField :: Int -> Literal -> Maybe Literal
litConField tag = \case
  IntegerLiteral l1 -> case tag of
    1 | l1 >= ((-2)^(63::Int)) && l1 < 2^(63::Int) -> Just (IntLiteral l1)
    2 | l1 >= 2^(63::Int), IP ba <- l1 -> Just (ByteArrayLiteral (BA.ByteArray ba))
    3 | l1 < ((-2)^(63::Int)), IN ba <- l1 -> Just (ByteArrayLiteral (BA.ByteArray ba))
    _ -> Nothing
  NaturalLiteral l1 -> case tag of
    1 | l1 >= 0 && l1 < 2^(64::Int) -> Just (WordLiteral l1)
    2 | l1 >= 2^(64::Int), IP ba <- l1 -> Just (ByteArrayLiteral (BA.ByteArray ba))
    _ -> Nothing
  _ -> Nothing

-- | Substitute the existential types and the fields of a constructor
-- application in an alternative
substInAlt
  :: (HasCallStack, Distinct n)
  => Scope n -> DataCon -> Tele TyBinder n i -> Tele IdBinder i l
  -> [Either (Term n) (Type n)] -> Term l -> Term n
substInAlt sc dc tvs xs args e =
  fromMaybe (error ($(curLoc) ++ "fields don't match the pattern")) $ do
    s1 <- extendTySubstTele (idSubst sc) tvs
            (drop (length (dcUnivTyVars dc)) (Either.rights args))
    s2 <- extendTmSubstTele s1 xs (Either.lefts args)
    pure (substTm s2 e)

-- * Primitives

-- | Reduce a fully applied primitive, see 'PrimReductions'
primStep
  :: Distinct n
  => Env -> Bool -> C.PrimInfo -> [Type n] -> [Value n] -> Machine o n
  -> Maybe (Next o n)
primStep env isSubj p tys vs m =
  case HashMap.lookup (C.primName p) (primReductions prims) of
    Just r -> runPrimReduction r ctx
    Nothing -> case primFallback prims of
      Just r -> runPrimReduction r ctx
      Nothing -> Nothing
 where
  prims = envPrims env
  ctx = PrimContext env isSubj p tys vs m

-- | Unwind a primitive argument frame. See
-- 'Clash.GHC.Evaluator.Primitive.ghcPrimUnwind'.
primUnwind
  :: Distinct n
  => Env -> C.PrimInfo -> [Type n] -> [Value n] -> Value n -> [Term n] -> Machine o n
  -> Maybe (Next o n)
primUnwind env p tys vs v [] m
  -- These primitives are actually values, and not operations
  | nm `elem` valuePrims = unwind env (PrimVal p tys (vs ++ [v])) m
  | nm == showt 'Clash.Sized.Internal.BitVector.fromInteger# = case (vs, v) of
      ([naturalLiteral -> Just n, mask], integerLiteral -> Just i) ->
        unwind env (PrimVal p tys
          [Lit (NaturalLiteral n), mask, Lit (IntegerLiteral (wrapUnsigned n i))]) m
      _ -> internalError
  | nm == showt 'Clash.Sized.Internal.BitVector.fromInteger## = case (vs, v) of
      ([mask], integerLiteral -> Just i) ->
        unwind env (PrimVal p tys [mask, Lit (IntegerLiteral (wrapUnsigned 1 i))]) m
      _ -> internalError
  | nm == showt 'Clash.Sized.Internal.Signed.fromInteger# = case (vs, v) of
      ([naturalLiteral -> Just n], integerLiteral -> Just i) ->
        unwind env (PrimVal p tys [Lit (NaturalLiteral n), Lit (IntegerLiteral (wrapSigned n i))]) m
      _ -> internalError
  | nm == showt 'Clash.Sized.Internal.Unsigned.fromInteger# = case (vs, v) of
      ([naturalLiteral -> Just n], integerLiteral -> Just i) ->
        unwind env (PrimVal p tys [Lit (NaturalLiteral n), Lit (IntegerLiteral (wrapUnsigned n i))]) m
      _ -> internalError
  | isUndefinedPrimVal v = undefinedResult NP.undefined env p tys (vs ++ [v]) [] m
  | isUndefinedXPrimVal v = undefinedResult NP.undefinedX env p tys (vs ++ [v]) [] m
  | otherwise = primStep env (forcePrims m) p tys (vs ++ [v]) m
 where
  nm = C.primName p
  internalError = error ($(curLoc) ++ "Internal error" ++ show (vs, v))
-- Note [Lazy primitives] in "Clash.GHC.Evaluator.Primitive": some primitives
-- are lazy in their last argument
primUnwind env p tys vs v [e] m
  | C.primName p `elem` lazyPrims
  = if isUndefinedPrimVal v
      then undefinedResult NP.undefined env p tys (vs ++ [v]) [e] m
      else newLetBinding env m e $ \m1 i ->
        nextFrom <$>
          primStep env (forcePrims m) p (sink1 tys)
            (sink1 (vs ++ [v]) ++ [Suspend (heapVarTerm i)]) m1
primUnwind env p tys vs (collectValueTicks -> (v, ts)) (e : es) m
  | isUndefinedPrimVal v = undefinedResult NP.undefined env p tys (vs ++ [v]) (e : es) m
  | otherwise =
      Just (stay m { mStack = PrimApply p tys (vs ++ [foldr TickValue v ts]) es : mStack m
                   , mTerm = e })

-- | Continue with an undefined value of the result type of an applied
-- primitive
undefinedResult
  :: Distinct n
  => C.PrimInfo -> Env -> C.PrimInfo -> [Type n] -> [Value n] -> [Term n]
  -> Machine o n -> Maybe (Next o n)
undefinedResult undef env p tys vs es m =
  let sc = mScope m
      args = map Right tys ++ map (Left . valToTerm) vs ++ map Left es
      ty = applyTypeToArgs (Prim p) (envTypeEnv env) sc (primInfoType sc p) args
  in Just (stay m {mTerm = TyApp (Prim undef) ty})

-- | Primitives that are values, not operations
valuePrims :: [Text]
valuePrims =
  [ showt 'Clash.Sized.Internal.Index.fromInteger#
  , showt 'GHC.CString.unpackCString#
  , C.primName NP.removedArg
  , showt ''MutableByteArray#
  , C.primName NP.undefined
  , C.primName NP.undefinedX
  ]

-- | Primitives that are lazy in their last argument, see Note [Lazy
-- primitives] in "Clash.GHC.Evaluator.Primitive"
lazyPrims :: [Text]
lazyPrims =
  [ showt 'Clash.Sized.Vector.lazyV
  , showt 'Clash.Sized.Vector.replicate
  , "Clash.Sized.Vector.replace_int"
  , andName
  , orName
  , showt 'Clash.Sized.Internal.BitVector.xToBV
  , "Clash.Sized.Vector.imap_go"
  ]

-- | Whether a value is an undefined primitive
isUndefinedPrimVal :: Value n -> Bool
isUndefinedPrimVal (PrimVal p _ _) = C.primName p `elem` undefinedPrims
isUndefinedPrimVal _ = False

-- | Whether a value is an undefined primitive that throws an 'XException'
isUndefinedXPrimVal :: Value n -> Bool
isUndefinedXPrimVal (PrimVal p _ _) = C.primName p `elem` undefinedXPrims
isUndefinedXPrimVal _ = False

-- | The result type of a fully applied primitive
primResultType :: Distinct n => Env -> Scope n -> C.PrimInfo -> [Type n] -> Type n
primResultType env sc p tys =
  dropFuns (piResultTys (envTypeEnv env) sc (primInfoType sc p) tys)
 where
  dropFuns ty = case tyView ty of
    FunTy _ r -> dropFuns r
    _ -> ty

-- ** Helpers for primitive reductions

-- | Continue with a term in the given machine, an extension of the one of the
-- primitive. If evaluating the term to WHNF throws an 'XException', continue
-- with an undefined value instead. See
-- @Clash.GHC.Evaluator.Primitive.Util.reduceWith@.
reduceIn
  :: forall o n l. (Distinct n, DExt n l)
  => PrimContext o n -> Machine o l -> Term l -> Maybe (Next o n)
reduceIn pc m e = case isX e of
  Left msg ->
    let sc = mScope m
        resTy = primResultType (pcEnv pc) sc (pcInfo pc) (sink1 (pcTys pc))
    in trace (warning msg) (Just (Next m {mTerm = TyApp (Prim NP.undefined) resTy}))
  Right e' -> Just (Next m {mTerm = e'})
 where
  warning msg = unlines
    [ "Warning: caught XException: \"" ++ msg ++ "\" while trying to evaluate: "
    , showPpr (List.foldl' App (Prim (pcInfo pc)) (map valToTerm (pcArgs pc)))
    ]

-- | Continue with a term in the machine of the primitive
reduce :: Distinct n => PrimContext o n -> Term n -> Maybe (Next o n)
reduce pc = reduceIn pc (pcMachine pc)

-- | Evaluate a term to WHNF with a nested machine, and continue with the
-- result, with the original stack. See
-- @Clash.GHC.Evaluator.Primitive.Util.reduceWHNF@.
reduceWHNF :: Distinct n => PrimContext o n -> Term n -> Maybe (Next o n)
reduceWHNF pc e =
  let m = pcMachine pc
  in whnf (pcEnv pc) (pcIsSubj pc) m {mStack = [], mTerm = e} $ \m1 ->
       Just (Next m1 {mStack = sink1 (mStack m)})

-- | The integer of a literal, or of a constructor of 'Integer'
integerLiteral :: Value n -> Maybe Integer
integerLiteral = \case
  Lit (IntegerLiteral i) -> Just i
  DC dc [Left (Literal (IntLiteral i))] | dcTag dc == 1 -> Just i
  DC dc [Left (Literal (ByteArrayLiteral (BA.ByteArray ba)))]
    | dcTag dc == 2 -> Just (IP ba)
    | dcTag dc == 3 -> Just (IN ba)
  _ -> Nothing

-- | The natural of a literal, or of a constructor of 'Natural'
naturalLiteral :: Value n -> Maybe Integer
naturalLiteral = \case
  Lit (NaturalLiteral i) -> Just i
  DC dc [Left (Literal (WordLiteral i))] | dcTag dc == 1 -> Just i
  DC dc [Left (Literal (ByteArrayLiteral (BA.ByteArray ba)))]
    | dcTag dc == 2 -> Just (IP ba)
  _ -> Nothing

-- | See @Clash.GHC.Evaluator.Primitive.Util.wrapUnsigned@
wrapUnsigned :: Integer -> Integer -> Integer
wrapUnsigned n i = i `mod` (2 ^ n)

-- | See @Clash.GHC.Evaluator.Primitive.Util.wrapSigned@
wrapSigned :: Integer -> Integer -> Integer
wrapSigned n i = if n == 0 then 0 else res
 where
  mask = 2 ^ (n - 1)
  res = case divMod i mask of
    (s, i1) | even s -> i1
            | otherwise -> i1 - mask
