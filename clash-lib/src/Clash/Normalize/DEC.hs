{-|
  Copyright  :  (C) 2015-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Helper functions for the 'disjointExpressionConsolidation' transformation

  The 'disjointExpressionConsolidation' transformation lifts applications of
  global binders out of alternatives of case-statements.

  e.g. It converts:

  > case x of
  >   A -> f 3 y
  >   B -> f x x
  >   C -> h x

  into:

  > let f_arg0 = case x of {A -> 3; B -> x}
  >     f_arg1 = case x of {A -> y; B -> x}
  >     f_out  = f f_arg0 f_arg1
  > in  case x of
  >       A -> f_out
  >       B -> f_out
  >       C -> h x
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize.DEC
  (collectGlobals
  ,isDisjoint
  ,mkDisjointGroup
  )
where

-- external
import           Control.Concurrent.Supply        (splitSupply)
import qualified Control.Lens                     as Lens
import           Data.Bits                        ((.&.),complement)
import           Data.Coerce                      (coerce)
import qualified Data.Either                      as Either
import qualified Data.Foldable                    as Foldable
import qualified Data.IntMap.Strict               as IM
import qualified Data.IntSet                      as IntSet
import qualified Data.List                        as List
import qualified Data.Map.Strict                  as Map
import qualified Data.Maybe                       as Maybe
import           Data.Monoid                      (All (..))

-- internal
import Clash.Core.DataCon    (DataCon, dcTag)
import Clash.Core.Evaluator  (whnf')
import Clash.Core.FreeVars
  (termFreeVars', typeFreeVars', localVarsDoNotOccurIn)
import Clash.Core.Literal    (Literal (..))
import Clash.Core.Term
  (LetBinding, Pat (..), PrimInfo (..), Term (..), TickInfo (..), collectArgs,
   collectArgsTicks)
import Clash.Core.TyCon      (tyConDataCons)
import Clash.Core.Type       (Type, isPolyFunTy, mkTyConApp, splitFunForallTy)
import Clash.Core.Util       (mkApps, mkTicks, patIds, termType)
import Clash.Core.Var        (isGlobalId)
import Clash.Core.VarEnv
  (InScopeSet, elemInScopeSet, notElemInScopeSet)
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Types
  (RewriteMonad, bindings, evaluator, globalHeap, tcCache, tupleTcCache, uniqSupply)
import Clash.Rewrite.Util    (mkInternalVar, mkSelectorCase,
                              isUntranslatableType, isConstant)
import Clash.Unique          (lookupUniqMap)
import Clash.Util

data CaseTree a
  = Leaf a
  | LB [LetBinding] (CaseTree a)
  | Branch Term [(Pat,CaseTree a)]
  deriving (Eq,Show,Functor,Foldable)

-- | Test if a 'CaseTree' collected from an expression indicates that
-- application of a global binder is disjoint: occur in separate branches of a
-- case-expression.
isDisjoint :: CaseTree ([Either Term Type])
           -> Bool
isDisjoint (Branch _ [_]) = False
isDisjoint ct = go ct
  where
    go (Leaf _)             = False
    go (LB _ ct')           = go ct'
    go (Branch _ [])        = False
    go (Branch _ [(_,x)])   = go x
    go b@(Branch _ (_:_:_)) = allEqual (map Either.rights (Foldable.toList b))

-- Remove empty branches from a 'CaseTree'
removeEmpty :: Eq a => CaseTree [a] -> CaseTree [a]
removeEmpty l@(Leaf _) = l
removeEmpty (LB lb ct) =
  case removeEmpty ct of
    Leaf [] -> Leaf []
    ct'     -> LB lb ct'
removeEmpty (Branch s bs) =
  case filter ((/= (Leaf [])) . snd) (map (second removeEmpty) bs) of
    []  -> Leaf []
    bs' -> Branch s bs'

-- | Test if all elements in a list are equal to each other.
allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

-- | Collect 'CaseTree's for (potentially) disjoint applications of globals out
-- of an expression. Also substitute truly disjoint applications of globals by a
-- reference to a lifted out application.
collectGlobals'
  :: InScopeSet
  -> [(Term,Term)]
  -- ^ Substitution of (applications of) a global binder by a reference to a
  -- lifted term.
  -> [Term]
  -- ^ List of already seen global binders
  -> Term
  -- ^ The expression
  -> Bool
  -- ^ Whether expression is constant
  -> RewriteMonad
      NormalizeState
      (Term, [(Term, ([Term], CaseTree [Either Term Type]))])
collectGlobals' inScope substitution seen (Case scrut ty alts) _eIsConstant = do
  rec (alts' ,collected)  <- collectGlobalsAlts inScope substitution seen scrut'
                                                alts
      (scrut',collected') <- collectGlobals inScope substitution
                                            (map fst collected ++ seen) scrut
  return (Case scrut' ty alts',collected ++ collected')

collectGlobals' inScope substitution seen e@(collectArgsTicks -> (fun, args@(_:_), ticks)) eIsconstant
  | not eIsconstant = do
    tcm <- Lens.view tcCache
    bndrs <- Lens.use bindings
    (primEval, primUnwind) <- Lens.view evaluator
    gh <- Lens.use globalHeap
    ids <- Lens.use uniqSupply
    let (ids1,ids2) = splitSupply ids
    uniqSupply Lens..= ids2
    let eval = (Lens.view Lens._3) . whnf' primEval primUnwind bndrs tcm gh ids1 inScope False
        eTy  = termType tcm e
    untran <- isUntranslatableType False eTy
    case untran of
      -- Don't lift out non-representable values, because they cannot be let-bound
      -- in our desired normal form.
      False -> do
        isInteresting <- interestingToLift inScope eval fun args ticks
        case isInteresting of
          Just fun' | fun' `notElem` seen -> do
            (args',collected) <- collectGlobalsArgs inScope substitution
                                                    (fun':seen) args
            let e' = Maybe.fromMaybe (mkApps fun' args') (List.lookup fun' substitution)
            -- This function is lifted out an environment with the currently 'seen'
            -- binders. When we later apply substitution, we need to start with this
            -- environment, otherwise we perform incorrect substitutions in the
            -- arguments.
            return (e',(fun',(seen,Leaf args')):collected)
          _ -> do (args',collected) <- collectGlobalsArgs inScope substitution
                                                          seen args
                  return (mkApps (mkTicks fun ticks) args',collected)
      _ -> return (e,[])

-- FIXME: This duplicates A LOT of let-bindings, where I just pray that after
-- the ANF, CSE, and DeadCodeRemoval pass all duplicates are removed.
--
-- I think we should be able to do better, but perhaps we cannot fix it here.
collectGlobals' inScope substitution seen (Letrec lbs body) _eIsConstant = do
  (body',collected)   <- collectGlobals    inScope substitution seen body
  (lbs',collected')   <- collectGlobalsLbs inScope substitution
                                           (map fst collected ++ seen)
                                           lbs
  return (Letrec lbs' body'
         ,map (second (second (LB lbs'))) (collected ++ collected')
         )

collectGlobals' inScope substitution seen (Tick t e) eIsConstant = do
  (e',collected) <- collectGlobals' inScope substitution seen e eIsConstant
  return (Tick t e',collected)

collectGlobals' _ _ _ e _ = return (e,[])

-- | Collect 'CaseTree's for (potentially) disjoint applications of globals out
-- of an expression. Also substitute truly disjoint applications of globals by a
-- reference to a lifted out application.
collectGlobals
  :: InScopeSet
  -> [(Term,Term)]
  -- ^ Substitution of (applications of) a global binder by a reference to
  -- a lifted term.
  -> [Term]
  -- ^ List of already seen global binders
  -> Term
  -- ^ The expression
  -> RewriteMonad
      NormalizeState
      (Term, [(Term, ([Term], CaseTree [Either Term Type]))])
collectGlobals inScope substitution seen e =
  collectGlobals' inScope substitution seen e (isConstant e)

-- | Collect 'CaseTree's for (potentially) disjoint applications of globals out
-- of a list of application arguments. Also substitute truly disjoint
-- applications of globals by a reference to a lifted out application.
collectGlobalsArgs ::
     InScopeSet
  -> [(Term,Term)] -- ^ Substitution of (applications of) a global
                   -- binder by a reference to a lifted term.
  -> [Term] -- ^ List of already seen global binders
  -> [Either Term Type] -- ^ The list of arguments
  -> RewriteMonad NormalizeState
                  ([Either Term Type]
                  ,[(Term,([Term],CaseTree [(Either Term Type)]))]
                  )
collectGlobalsArgs inScope substitution seen args = do
    (_,(args',collected)) <- second unzip <$> mapAccumLM go seen args
    return (args',concat collected)
  where
    go s (Left tm) = do
      (tm',collected) <- collectGlobals inScope substitution s tm
      return (map fst collected ++ s,(Left tm',collected))
    go s (Right ty) = return (s,(Right ty,[]))

-- | Collect 'CaseTree's for (potentially) disjoint applications of globals out
-- of a list of alternatives. Also substitute truly disjoint applications of
-- globals by a reference to a lifted out application.
collectGlobalsAlts ::
     InScopeSet
  -> [(Term,Term)] -- ^ Substitution of (applications of) a global
                   -- binder by a reference to a lifted term.
  -> [Term] -- ^ List of already seen global binders
  -> Term -- ^ The subject term
  -> [(Pat,Term)] -- ^ The list of alternatives
  -> RewriteMonad NormalizeState
                  ([(Pat,Term)]
                  ,[(Term,([Term],CaseTree [(Either Term Type)]))]
                  )
collectGlobalsAlts inScope substitution seen scrut alts = do
    (alts',collected) <- unzip <$> mapM go alts
    let collectedM  = map (Map.fromList . map (second (second (:[])))) collected
        collectedUN = Map.unionsWith (\(l1,r1) (l2,r2) -> (List.nub (l1 ++ l2),r1 ++ r2)) collectedM
        collected'  = map (second (second (Branch scrut))) (Map.toList collectedUN)
    return (alts',collected')
  where
    go (p,e) = do
      (e',collected) <- collectGlobals inScope substitution seen e
      return ((p,e'),map (second (second (p,))) collected)

-- | Collect 'CaseTree's for (potentially) disjoint applications of globals out
-- of a list of let-bindings. Also substitute truly disjoint applications of
-- globals by a reference to a lifted out application.
collectGlobalsLbs ::
     InScopeSet
  -> [(Term,Term)] -- ^ Substitution of (applications of) a global
                   -- binder by a reference to a lifted term.
  -> [Term] -- ^ List of already seen global binders
  -> [LetBinding] -- ^ The list let-bindings
  -> RewriteMonad NormalizeState
                  ([LetBinding]
                  ,[(Term,([Term],CaseTree [(Either Term Type)]))]
                  )
collectGlobalsLbs inScope substitution seen lbs = do
    (_,(lbs',collected)) <- second unzip <$> mapAccumLM go seen lbs
    return (lbs',concat collected)
  where
    go :: [Term] -> LetBinding
       -> RewriteMonad NormalizeState
                  ([Term]
                  ,(LetBinding
                   ,[(Term,([Term],CaseTree [(Either Term Type)]))]
                   )
                  )
    go s (id_, e) = do
      (e',collected) <- collectGlobals inScope substitution s e
      return (map fst collected ++ s,((id_,e'),collected))

-- | Given a case-tree corresponding to a disjoint interesting \"term-in-a-
-- function-position\", return a let-expression: where the let-binding holds
-- a case-expression selecting between the distinct arguments of the case-tree,
-- and the body is an application of the term applied to the shared arguments of
-- the case tree, and projections of let-binding corresponding to the distinct
-- argument positions.
mkDisjointGroup
  :: InScopeSet
  -- ^ Variables in scope at the very top of the case-tree, i.e., the original
  -- expression
  -> (Term,([Term],CaseTree [(Either Term Type)]))
  -- ^ Case-tree of arguments belonging to the applied term.
  -> RewriteMonad NormalizeState (Term,[Term])
mkDisjointGroup inScope (fun,(seen,cs)) = do
    let argss    = Foldable.toList cs
        argssT   = zip [0..] (List.transpose argss)
        (sharedT,distinctT) = List.partition (areShared inScope . snd) argssT
        shared   = map (second head) sharedT
        distinct = map (Either.lefts) (List.transpose (map snd distinctT))
        cs'      = fmap (zip [0..]) cs
        cs''     = removeEmpty
                 $ fmap (Either.lefts . map snd)
                        (if null shared
                           then cs'
                           else fmap (filter (`notElem` shared)) cs')
    tcm <- Lens.view tcCache
    (distinctCaseM,distinctProjections) <- case distinct of
      -- only shared arguments: do nothing.
      [] -> return (Nothing,[])
      -- Create selectors and projections
      (uc:_) -> do
        let argTys = map (termType tcm) uc
        disJointSelProj inScope argTys cs''
    let newArgs = mkDJArgs 0 shared distinctProjections
    case distinctCaseM of
      Just lb -> return (Letrec [lb] (mkApps fun newArgs), seen)
      Nothing -> return (mkApps fun newArgs, seen)

-- | Create a single selector for all the representable distinct arguments by
-- selecting between tuples. This selector is only ('Just') created when the
-- number of representable uncommmon arguments is larger than one, otherwise it
-- is not ('Nothing').
--
-- It also returns:
--
-- * For all the non-representable distinct arguments: a selector
-- * For all the representable distinct arguments: a projection out of the tuple
--   created by the larger selector. If this larger selector does not exist, a
--   single selector is created for the single representable distinct argument.
disJointSelProj
  :: InScopeSet
  -> [Type]
  -- ^ Types of the arguments
  -> CaseTree [Term]
  -- The case-tree of arguments
  -> RewriteMonad NormalizeState (Maybe LetBinding,[Term])
disJointSelProj _ _ (Leaf []) = return (Nothing,[])
disJointSelProj inScope argTys cs = do
    let maxIndex = length argTys - 1
        css = map (\i -> fmap ((:[]) . (!!i)) cs) [0..maxIndex]
    (untran,tran) <- partitionM (isUntranslatableType False . snd) (zip [0..] argTys)
    let untranCs   = map (css!!) (map fst untran)
        untranSels = zipWith (\(_,ty) cs' -> genCase ty Nothing []  cs')
                             untran untranCs
    (lbM,projs) <- case tran of
      []       -> return (Nothing,[])
      [(i,ty)] -> return (Nothing,[genCase ty Nothing [] (css!!i)])
      tys      -> do
        tcm    <- Lens.view tcCache
        tupTcm <- Lens.view tupleTcCache
        let m            = length tys
            Just tupTcNm = IM.lookup m tupTcm
            Just tupTc   = lookupUniqMap tupTcNm tcm
            [tupDc]      = tyConDataCons tupTc
            (tyIxs,tys') = unzip tys
            tupTy        = mkTyConApp tupTcNm tys'
            cs'          = fmap (\es -> map (es !!) tyIxs) cs
            djCase       = genCase tupTy (Just tupDc) tys' cs'
        scrutId <- mkInternalVar inScope "tupIn" tupTy
        projections <- mapM (mkSelectorCase ($(curLoc) ++ "disJointSelProj")
                                            inScope tcm (Var scrutId) (dcTag tupDc)) [0..m-1]
        return (Just (scrutId,djCase),projections)
    let selProjs = tranOrUnTran 0 (zip (map fst untran) untranSels) projs

    return (lbM,selProjs)
  where
    tranOrUnTran _ []       projs     = projs
    tranOrUnTran _ sels     []        = map snd sels
    tranOrUnTran n ((ut,s):uts) (p:projs)
      | n == ut   = s : tranOrUnTran (n+1) uts          (p:projs)
      | otherwise = p : tranOrUnTran (n+1) ((ut,s):uts) projs

-- | Arguments are shared between invocations if:
--
-- * They contain _no_ references to locally-bound variables
-- * Are all equal
areShared :: InScopeSet -> [Either Term Type] -> Bool
areShared _       []       = True
areShared inScope xs@(x:_) = noFV1 && allEqual xs
 where
  noFV1 = case x of
    Right ty -> getAll (Lens.foldMapOf (typeFreeVars' isLocallyBound IntSet.empty)
                                       (const (All False)) ty)
    Left tm  -> getAll (Lens.foldMapOf (termFreeVars' isLocallyBound)
                                       (const (All False)) tm)

  isLocallyBound v = v `notElemInScopeSet` inScope

-- | Create a list of arguments given a map of positions to common arguments,
-- and a list of arguments
mkDJArgs :: Int -- ^ Current position
         -> [(Int,Either Term Type)] -- ^ map from position to common argument
         -> [Term] -- ^ (projections for) distinct arguments
         -> [Either Term Type]
mkDJArgs _ cms []   = map snd cms
mkDJArgs _ [] uncms = map Left uncms
mkDJArgs n ((m,x):cms) (y:uncms)
  | n == m    = x       : mkDJArgs (n+1) cms (y:uncms)
  | otherwise = Left y  : mkDJArgs (n+1) ((m,x):cms) uncms

-- | Create a case-expression that selects between the distinct arguments given
-- a case-tree
genCase :: Type -- ^ Type of the alternatives
        -> Maybe DataCon -- ^ DataCon to pack multiple arguments
        -> [Type] -- ^ Types of the arguments
        -> CaseTree [Term] -- ^ CaseTree of arguments
        -> Term
genCase ty dcM argTys = go
  where
    go (Leaf tms) =
      case dcM of
        Just dc -> mkApps (Data dc) (map Right argTys ++ map Left tms)
        _ -> head tms

    go (LB lb ct) =
      Letrec lb (go ct)

    go (Branch scrut [(p,ct)]) =
      let ct' = go ct
          (ptvs,pids) = patIds p
      in  if (coerce ptvs ++ coerce pids) `localVarsDoNotOccurIn` ct'
             then ct'
             else Case scrut ty [(p,ct')]

    go (Branch scrut pats) =
      Case scrut ty (map (second go) pats)

-- | Determine if a term in a function position is interesting to lift out of
-- of a case-expression.
--
-- This holds for all global functions, and certain primitives. Currently those
-- primitives are:
--
-- * All non-power-of-two multiplications
-- * All division-like operations with a non-power-of-two divisor
interestingToLift
  :: InScopeSet
  -- ^ in scope
  -> (Term -> Term)
  -- ^ Evaluator
  -> Term
  -- ^ Term in function position
  -> [Either Term Type]
  -- ^ Arguments
  -> [TickInfo]
  -- ^ Tick annoations
  -> RewriteMonad extra (Maybe Term)
interestingToLift inScope _ e@(Var v) _ ticks =
  if NoDeDup `notElem` ticks && (isGlobalId v ||  v `elemInScopeSet` inScope)
     then pure (Just e)
     else pure Nothing
interestingToLift inScope eval e@(Prim pInfo) args ticks
  | NoDeDup `notElem` ticks = do
  let anyArgNotConstant = any (not . isConstant) lArgs
  case List.lookup (primName pInfo) interestingPrims of
    Just t | t || anyArgNotConstant -> pure (Just e)
    _ | DeDup `elem` ticks -> pure (Just e)
    _ -> do
      let isInteresting = (\(x, y, z) -> interestingToLift inScope eval x y z) . collectArgsTicks
      if isHOTy (primType pInfo) then do
        anyInteresting <- anyM (fmap Maybe.isJust . isInteresting) lArgs
        if anyInteresting then pure (Just e) else pure Nothing
      else
        pure Nothing

  where
    interestingPrims =
      [("Clash.Sized.Internal.BitVector.*#",tailNonPow2)
      ,("Clash.Sized.Internal.BitVector.times#",tailNonPow2)
      ,("Clash.Sized.Internal.BitVector.quot#",lastNotPow2)
      ,("Clash.Sized.Internal.BitVector.rem#",lastNotPow2)
      ,("Clash.Sized.Internal.Index.*#",tailNonPow2)
      ,("Clash.Sized.Internal.Index.quot#",lastNotPow2)
      ,("Clash.Sized.Internal.Index.rem#",lastNotPow2)
      ,("Clash.Sized.Internal.Signed.*#",tailNonPow2)
      ,("Clash.Sized.Internal.Signed.times#",tailNonPow2)
      ,("Clash.Sized.Internal.Signed.rem#",lastNotPow2)
      ,("Clash.Sized.Internal.Signed.quot#",lastNotPow2)
      ,("Clash.Sized.Internal.Signed.div#",lastNotPow2)
      ,("Clash.Sized.Internal.Signed.mod#",lastNotPow2)
      ,("Clash.Sized.Internal.Unsigned.*#",tailNonPow2)
      ,("Clash.Sized.Internal.Unsigned.times#",tailNonPow2)
      ,("Clash.Sized.Internal.Unsigned.quot#",lastNotPow2)
      ,("Clash.Sized.Internal.Unsigned.rem#",lastNotPow2)
      ,("GHC.Base.quotInt",lastNotPow2)
      ,("GHC.Base.remInt",lastNotPow2)
      ,("GHC.Base.divInt",lastNotPow2)
      ,("GHC.Base.modInt",lastNotPow2)
      ,("GHC.Classes.divInt#",lastNotPow2)
      ,("GHC.Classes.modInt#",lastNotPow2)
      ,("GHC.Integer.Type.timesInteger",allNonPow2)
      ,("GHC.Integer.Type.divInteger",lastNotPow2)
      ,("GHC.Integer.Type.modInteger",lastNotPow2)
      ,("GHC.Integer.Type.quotInteger",lastNotPow2)
      ,("GHC.Integer.Type.remInteger",lastNotPow2)
      ,("GHC.Prim.*#",allNonPow2)
      ,("GHC.Prim.quotInt#",lastNotPow2)
      ,("GHC.Prim.remInt#",lastNotPow2)
      ]

    lArgs       = Either.lefts args

    allNonPow2  = all (not . termIsPow2) lArgs
    tailNonPow2 = case lArgs of
                    [] -> True
                    _  -> all (not . termIsPow2) (tail lArgs)
    lastNotPow2 = case lArgs of
                    [] -> True
                    _  -> not (termIsPow2 (last lArgs))

    termIsPow2 e' = case eval e' of
      Literal (IntegerLiteral n) -> isPow2 n
      a -> case collectArgs a of
        (Prim p,[Right _,Left _,Left (Literal (IntegerLiteral n))])
          | isFromInteger (primName p) -> isPow2 n
        (Prim p,[Right _,Left _,Left _,Left (Literal (IntegerLiteral n))])
          | primName p == "Clash.Sized.Internal.BitVector.fromInteger#"  -> isPow2 n
        (Prim p,[Right _,       Left _,Left (Literal (IntegerLiteral n))])
          | primName p == "Clash.Sized.Internal.BitVector.fromInteger##" -> isPow2 n

        _ -> False

    isPow2 x = x /= 0 && (x .&. (complement x + 1)) == x

    isFromInteger x = x `elem` ["Clash.Sized.Internal.BitVector.fromInteger#"
                               ,"Clash.Sized.Integer.Index.fromInteger"
                               ,"Clash.Sized.Internal.Signed.fromInteger#"
                               ,"Clash.Sized.Internal.Unsigned.fromInteger#"
                               ]

    isHOTy t = case splitFunForallTy t of
      (args',_) -> any isPolyFunTy (Either.rights args')

interestingToLift _ _ _ _ _ = pure Nothing
