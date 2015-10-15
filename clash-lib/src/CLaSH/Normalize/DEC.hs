{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Helper functions for the 'disjointExpressionConsolidation' transformation
--
-- The 'disjointExpressionConsolidation' transformation lifts applications of
-- global binders out of alternatives of case-statements.
--
-- e.g. It converts:
--
-- @
-- case x of
--   A -> f 3 y
--   B -> f x x
--   C -> h x
-- @
--
-- into:
--
-- @
-- let f_arg0 = case x of {A -> 3; B -> x}
--     f_arg1 = case x of {A -> y; B -> x}
--     f_out  = f f_arg0 f_arg1
-- in  case x of
--       A -> f_out
--       B -> f_out
--       C -> h x
-- @
module CLaSH.Normalize.DEC
  (collectGlobals
  ,isDisjoint
  ,mkDisjointGroup
  )
where

-- external
import qualified Control.Lens                     as Lens
import           Data.Bits                        ((.&.),complement)
import qualified Data.Either                      as Either
import qualified Data.Foldable                    as Foldable
import qualified Data.HashMap.Strict              as HashMap
import qualified Data.IntMap.Strict               as IM
import qualified Data.List                        as List
import qualified Data.Maybe                       as Maybe
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import qualified Data.Set.Lens                    as Lens

import           Unbound.Generics.LocallyNameless (Bind, bind, embed, rec,
                                                   unbind, unembed, unrec)

-- internal
import CLaSH.Core.DataCon    (DataCon, dcTag)
import CLaSH.Core.FreeVars   (termFreeIds, typeFreeVars)
import CLaSH.Core.Literal    (Literal (..))
import CLaSH.Core.Term       (LetBinding, Pat (..), Term (..), TmName)
import CLaSH.Core.TyCon      (tyConDataCons)
import CLaSH.Core.Type       (Type, mkTyConApp, splitFunForallTy)
import CLaSH.Core.Util       (collectArgs, mkApps, termType)
import CLaSH.Normalize.Types (NormalizeState)
import CLaSH.Normalize.Util  (isConstant)
import CLaSH.Rewrite.Types   (RewriteMonad, evaluator, tcCache, tupleTcCache)
import CLaSH.Rewrite.Util    (mkInternalVar, mkSelectorCase,
                              isUntranslatableType)
import CLaSH.Util

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
isDisjoint (Leaf _)             = False
isDisjoint (LB _ ct)            = isDisjoint ct
isDisjoint (Branch _ [])        = False
isDisjoint (Branch _ [(_,x)])   = isDisjoint x
isDisjoint b@(Branch _ (_:_:_)) = allEqual (map Either.rights
                                                (Foldable.toList b))

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
collectGlobals ::
     Set TmName
  -> [(Term,Term)] -- ^ Substitution of (applications of) a global
                   -- binder by a reference to a lifted term.
  -> [Term] -- ^ List of already seen global binders
  -> Term -- ^ The expression
  -> RewriteMonad NormalizeState
                  (Term,[(Term,CaseTree [(Either Term Type)])])
collectGlobals inScope substitution seen (Case scrut ty alts) = do
  (scrut',collected)  <- collectGlobals     inScope substitution seen scrut
  (alts' ,collected') <- collectGlobalsAlts inScope substitution seen scrut' alts
  return (Case scrut' ty alts',collected ++ collected')

collectGlobals inScope substitution seen e@(collectArgs -> (fun, args@(_:_)))
  | not (isConstant e) = do
    tcm <- Lens.view tcCache
    eval <- Lens.view evaluator
    eTy <- termType tcm e
    case splitFunForallTy eTy of
      ([],_) -> case interestingToLift inScope (eval tcm False) fun args of
        Just fun' | fun' `notElem` seen -> do
          (args',collected) <- collectGlobalsArgs inScope substitution (fun':seen) args
          let e' = Maybe.fromMaybe e (List.lookup fun' substitution)
          return (e',(fun',Leaf args'):collected)
        _ -> do (args',collected) <- collectGlobalsArgs inScope substitution seen args
                return (mkApps fun args',collected)
      _ -> return (e,[])

-- FIXME: This duplicates A LOT of let-bindings, where I just pray that after
-- the ANF, CSE, and DeadCodeRemoval pass all duplicates are removed.
--
-- I think we should be able to do better, but perhaps we cannot fix it here.
collectGlobals inScope substitution seen (Letrec b) = do
  (unrec -> lbs,body) <- unbind b
  (body',collected)   <- collectGlobals    inScope substitution seen body
  (lbs',collected')   <- collectGlobalsLbs inScope substitution
                                           (map fst collected ++ seen)
                                           lbs
  return (Letrec (bind (rec lbs') body')
         ,map (second (LB lbs')) (collected ++ collected')
         )

collectGlobals _ _ _ e = return (e,[])

-- | Collect 'CaseTree's for (potentially) disjoint applications of globals out
-- of a list of application arguments. Also substitute truly disjoint
-- applications of globals by a reference to a lifted out application.
collectGlobalsArgs ::
     Set TmName
  -> [(Term,Term)] -- ^ Substitution of (applications of) a global
                   -- binder by a reference to a lifted term.
  -> [Term] -- ^ List of already seen global binders
  -> [Either Term Type] -- ^ The list of arguments
  -> RewriteMonad NormalizeState
                  ([Either Term Type]
                  ,[(Term,CaseTree [(Either Term Type)])]
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
     Set TmName
  -> [(Term,Term)] -- ^ Substitution of (applications of) a global
                   -- binder by a reference to a lifted term.
  -> [Term] -- ^ List of already seen global binders
  -> Term -- ^ The subject term
  -> [Bind Pat Term] -- ^ The list of alternatives
  -> RewriteMonad NormalizeState
                  ([Bind Pat Term]
                  ,[(Term,CaseTree [(Either Term Type)])]
                  )
collectGlobalsAlts inScope substitution seen scrut alts = do
    (alts',collected) <- unzip <$> mapM go alts
    let collected'  = List.groupBy ((==) `on` fst) (concat collected)
        collected'' = map (\xs -> (fst (head xs),Branch scrut (map snd xs)))
                          collected'
    return (alts',collected'')
  where
    go pe = do (p,e) <- unbind pe
               (e',collected) <- collectGlobals inScope substitution seen e
               return (bind p e',map (second (p,)) collected)

-- | Collect 'CaseTree's for (potentially) disjoint applications of globals out
-- of a list of let-bindings. Also substitute truly disjoint applications of
-- globals by a reference to a lifted out application.
collectGlobalsLbs ::
     Set TmName
  -> [(Term,Term)] -- ^ Substitution of (applications of) a global
                   -- binder by a reference to a lifted term.
  -> [Term] -- ^ List of already seen global binders
  -> [LetBinding] -- ^ The list let-bindings
  -> RewriteMonad NormalizeState
                  ([LetBinding]
                  ,[(Term,CaseTree [(Either Term Type)])]
                  )
collectGlobalsLbs inScope substitution seen lbs = do
    (_,(lbs',collected)) <- second unzip <$> mapAccumLM go seen lbs
    return (lbs',concat collected)
  where
    go :: [Term] -> LetBinding
       -> RewriteMonad NormalizeState
                  ([Term]
                  ,(LetBinding
                   ,[(Term,CaseTree [(Either Term Type)])]
                   )
                  )
    go s (id_,unembed -> e) = do
      (e',collected) <- collectGlobals inScope substitution s e
      return (map fst collected ++ s,((id_,embed e'),collected))

-- | Given a case-tree corresponding to a disjoint interesting \"term-in-a-
-- function-position\", return a let-expression: where the let-binding holds
-- a case-expression selecting between the uncommon arguments of the case-tree,
-- and the body is an application of the term applied to the common arguments of
-- the case tree, and projections of let-binding corresponding to the uncommon
-- argument positions.
mkDisjointGroup :: Set TmName -- ^ Current free variables.
                -> (Term,CaseTree [(Either Term Type)])
                   -- ^ Case-tree of arguments belonging to the applied term.
                -> RewriteMonad NormalizeState Term
mkDisjointGroup fvs (fun,cs) = do
    let argss    = Foldable.toList cs
        argssT   = zip [0..] (List.transpose argss)
        (commonT,uncommonT) = List.partition (isCommon fvs . snd) argssT
        common   = map (second head) commonT
        uncommon = map (Either.lefts) (List.transpose (map snd uncommonT))
        cs'      = fmap (zip [0..]) cs
        cs''     = removeEmpty
                 $ fmap (Either.lefts . map snd)
                        (if null common
                           then cs'
                           else fmap (filter (`notElem` common)) cs')
    tcm <- Lens.view tcCache
    (uncommonCaseM,uncommonProjections) <- case uncommon of
      -- only common arguments: do nothing.
      [] -> return (Nothing,[])
      -- Create selectors and projections
      (uc:_) -> do
        argTys <- mapM (termType tcm) uc
        disJointSelProj argTys cs''
    let newArgs = mkDJArgs 0 common uncommonProjections
    case uncommonCaseM of
      Just lb -> return (Letrec (bind (rec [lb]) (mkApps fun newArgs)))
      Nothing -> return (mkApps fun newArgs)

-- | Create a single selector for all the representable uncommon arguments by
-- selecting between tuples. This selector is only ('Just') created when the
-- number of representable uncommmon arguments is larger than one, otherwise it
-- is not ('Nothing').
--
-- It also returns:
--
-- * For all the non-representable uncommon arguments: a selector
-- * For all the representable uncommon arguments: a projection out of the tuple
--   created by the larger selector. If this larger selector does not exist, a
--   single selector is created for the single representable uncommon argument.
disJointSelProj :: [Type] -- ^ Types of the arguments
                -> CaseTree [Term] -- The case-tree of arguments
                -> RewriteMonad NormalizeState (Maybe LetBinding,[Term])
disJointSelProj _ (Leaf []) = return (Nothing,[])
disJointSelProj argTys cs = do
    let maxIndex = length argTys - 1
        css = map (\i -> fmap ((:[]) . (!!i)) cs) [0..maxIndex]
    (untran,tran) <- partitionM (isUntranslatableType . snd) (zip [0..] argTys)
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
            Just tupTc   = HashMap.lookup tupTcNm tcm
            [tupDc]      = tyConDataCons tupTc
            (tyIxs,tys') = unzip tys
            tupTy        = mkTyConApp tupTcNm tys'
            cs'          = fmap (\es -> map (es !!) tyIxs) cs
            djCase       = genCase tupTy (Just tupDc) tys' cs'
        (scrutId,scrutVar) <- mkInternalVar "tupIn" tupTy
        projections <- mapM (mkSelectorCase ($(curLoc) ++ "disJointSelProj")
                                            tcm scrutVar (dcTag tupDc)) [0..m-1]
        return (Just (scrutId,embed djCase),projections)
    let selProjs = tranOrUnTran 0 (zip (map fst untran) untranSels) projs

    return (lbM,selProjs)
  where
    tranOrUnTran _ []       projs     = projs
    tranOrUnTran _ sels     []        = map snd sels
    tranOrUnTran n ((ut,s):uts) (p:projs)
      | n == ut   = s : tranOrUnTran (n+1) uts          (p:projs)
      | otherwise = p : tranOrUnTran (n+1) ((ut,s):uts) projs


isCommon :: Set TmName -> [Either Term Type] -> Bool
isCommon _   []             = True
isCommon _   (Right ty:tys) = Set.null (Lens.setOf typeFreeVars ty) &&
                              allEqual (Right ty:tys)
isCommon fvs (Left tm:tms)  = Set.null (Lens.setOf termFreeIds tm Set.\\ fvs) &&
                              allEqual (Left tm:tms)

-- | Create a list of arguments given a map of positions to common arguments,
-- and a list of arguments
mkDJArgs :: Int -- ^ Current position
         -> [(Int,Either Term Type)] -- ^ map from position to common argument
         -> [Term] -- ^ (projections for) uncommon arguments
         -> [Either Term Type]
mkDJArgs _ cms []   = map snd cms
mkDJArgs _ [] uncms = map Left uncms
mkDJArgs n ((m,x):cms) (y:uncms)
  | n == m    = x       : mkDJArgs (n+1) cms (y:uncms)
  | otherwise = Left y  : mkDJArgs (n+1) ((m,x):cms) uncms

-- | Create a case-expression that selects between the uncommon arguments given
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
      Letrec (bind (rec lb) (go ct))

    go (Branch scrut pats) =
      Case scrut ty (map (\(p,ct) -> bind p (go ct)) pats)

-- | Determine if a term in a function position is interesting to lift out of
-- of a case-expression.
--
-- This holds for all global functions, and certain primitives. Currently those
-- primitives are:
--
-- * All non-constant multiplications
-- * All non-power-of-two division-like operations
interestingToLift :: Set TmName -- ^ in scope
                  -> (Term -> Term) -- ^ Evaluator
                  -> Term -- ^ Term in function position
                  -> [Either Term Type] -- ^ Arguments
                  -> Maybe Term
interestingToLift inScope _ e@(Var _ nm) _ =
  if nm `Set.member` inScope
     then Just e
     else Nothing
interestingToLift _ eval e@(Prim nm _) args =
    case List.lookup nm interestingPrims of
      Just t | t -> Just e
      _ -> Nothing
  where
    interestingPrims =
      [("CLaSH.Sized.Internal.BitVector.*#",tailNonConstant)
      ,("CLaSH.Sized.Internal.BitVector.times#",tailNonConstant)
      ,("CLaSH.Sized.Internal.BitVector.quot#",lastNotPow2)
      ,("CLaSH.Sized.Internal.BitVector.rem#",lastNotPow2)
      ,("CLaSH.Sized.Internal.Index.*#",tailNonConstant)
      ,("CLaSH.Sized.Internal.Index.quot#",lastNotPow2)
      ,("CLaSH.Sized.Internal.Index.rem#",lastNotPow2)
      ,("CLaSH.Sized.Internal.Signed.*#",tailNonConstant)
      ,("CLaSH.Sized.Internal.Signed.times#",tailNonConstant)
      ,("CLaSH.Sized.Internal.Signed.rem#",lastNotPow2)
      ,("CLaSH.Sized.Internal.Signed.quot#",lastNotPow2)
      ,("CLaSH.Sized.Internal.Signed.div#",lastNotPow2)
      ,("CLaSH.Sized.Internal.Signed.mod#",lastNotPow2)
      ,("CLaSH.Sized.Internal.Unsigned.*#",tailNonConstant)
      ,("CLaSH.Sized.Internal.Unsigned.times#",tailNonConstant)
      ,("CLaSH.Sized.Internal.Unsigned.quot#",lastNotPow2)
      ,("CLaSH.Sized.Internal.Unsigned.rem#",lastNotPow2)
      ,("GHC.Base.quotInt",lastNotPow2)
      ,("GHC.Base.remInt",lastNotPow2)
      ,("GHC.Base.divInt",lastNotPow2)
      ,("GHC.Base.modInt",lastNotPow2)
      ,("GHC.Classes.divInt#",lastNotPow2)
      ,("GHC.Classes.modInt#",lastNotPow2)
      ,("GHC.Integer.Type.timesInteger",allNonConstant)
      ,("GHC.Integer.Type.divInteger",lastNotPow2)
      ,("GHC.Integer.Type.modInteger",lastNotPow2)
      ,("GHC.Integer.Type.quotInteger",lastNotPow2)
      ,("GHC.Integer.Type.remInteger",lastNotPow2)
      ,("GHC.Prim.*#",allNonConstant)
      ,("GHC.Prim.quotInt#",lastNotPow2)
      ,("GHC.Prim.remInt#",lastNotPow2)
      ]

    lArgs           = Either.lefts args
    allNonConstant  = all (not . isConstant) lArgs
    tailNonConstant = all (not . isConstant) (tail lArgs)
    lastNotPow2 =
      case eval (last lArgs) of
        Literal (IntegerLiteral n) -> not (isPow2 n)
        a -> case collectArgs a of
              (Prim nm' _,[Right _,Left _,Left (Literal (IntegerLiteral n))])
                | isFromInteger nm' -> not (isPow2 n)
              _ -> False
    isPow2 x        = x /= 0 && (x .&. (complement x + 1)) == x
    isFromInteger x = x `elem` ["CLaSH.Sized.Internal.BitVector.fromInteger#"
                               ,"CLaSH.Sized.Integer.Index.fromInteger"
                               ,"CLaSH.Sized.Internal.Signed.fromInteger#"
                               ,"CLaSH.Sized.Internal.Unsigned.fromInteger#"
                               ]

interestingToLift _ _ _ _ = Nothing
