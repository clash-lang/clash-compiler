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
--- @
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
import           Unbound.Generics.LocallyNameless (Bind, bind, embed, rec,
                                                   unbind)

-- internal
import CLaSH.Core.DataCon    (DataCon, dcTag)
import CLaSH.Core.Literal    (Literal (..))
import CLaSH.Core.Term       (Pat (..), Term (..))
import CLaSH.Core.TyCon      (tyConDataCons)
import CLaSH.Core.Type       (Type, mkTyConApp, splitFunForallTy)
import CLaSH.Core.Util       (collectArgs, mkApps, termType)
import CLaSH.Normalize.Types (NormalizeState)
import CLaSH.Normalize.Util  (isConstant)
import CLaSH.Rewrite.Types   (RewriteMonad, evaluator, tcCache, tupleTcCache)
import CLaSH.Rewrite.Util    (mkInternalVar, mkSelectorCase)
import CLaSH.Util

data CaseTree a
  = Leaf a
  | Branch Term [(Pat,CaseTree a)]
  deriving (Eq,Show,Functor,Foldable)

-- | Test if a 'CaseTree' collected from an expression indicates that
-- application of a global binder is disjoint: occur in separate branches of a
-- case-expression.
isDisjoint :: CaseTree ([Either Term Type])
           -> Bool
isDisjoint (Leaf _)             = False
isDisjoint (Branch _ [])        = False
isDisjoint (Branch _ [(_,x)])   = isDisjoint x
isDisjoint b@(Branch _ (_:_:_)) = allEqual (map Either.rights
                                                (Foldable.toList b))

-- Remove empty branches from a 'CaseTree'
removeEmpty :: Eq a => CaseTree [a] -> CaseTree [a]
removeEmpty l@(Leaf _)    = l
removeEmpty (Branch s bs) = Branch s (filter ((/= (Leaf [])) . snd)
                                      (map (second removeEmpty) bs))

-- | Test if all elements in a list are equal to each other.
allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

-- | Collect 'CaseTree's for (potentially) disjoint applications of globals out
-- of an expression. Also substitute truly disjoint applications of globals by a
-- reference to a lifted out application.
collectGlobals ::
     [(Term,Term)] -- ^ Substitution of (applications of) a global
                   -- binder by a reference to a lifted term.
  -> [Term] -- ^ List of already seen global binders
  -> Term -- ^ The expression
  -> RewriteMonad NormalizeState
                  (Term,[(Term,CaseTree [(Either Term Type)])])
collectGlobals substitution seen (Case scrut ty alts) = do
  (scrut',collected)  <- collectGlobals     substitution seen scrut
  (alts' ,collected') <- collectGlobalsAlts substitution seen scrut' alts
  return (Case scrut' ty alts',collected ++ collected')

collectGlobals substitution seen e@(collectArgs -> (fun, args@(_:_)))
  | not (isConstant e) = do
    tcm <- Lens.view tcCache
    eval <- Lens.view evaluator
    eTy <- termType tcm e
    case splitFunForallTy eTy of
      ([],_) -> case interestingToLift (eval tcm False) fun args of
        Just fun' | fun' `notElem` seen -> do
          (args',collected) <- collectGlobalsArgs substitution (fun':seen) args
          let e' = Maybe.fromMaybe e (List.lookup fun' substitution)
          return (e',(fun',Leaf args'):collected)
        _ -> do (args',collected) <- collectGlobalsArgs substitution seen args
                return (mkApps fun args',collected)
      _ -> return (e,[])

collectGlobals _ _ e = return (e,[])

-- | Collect 'CaseTree's for (potentially) disjoint applications of globals out
-- of a list of application arguments. Also substitute truly disjoint
-- applications of globals by a reference to a lifted out application.
collectGlobalsArgs ::
     [(Term,Term)] -- ^ Substitution of (applications of) a global
                   -- binder by a reference to a lifted term.
  -> [Term] -- ^ List of already seen global binders
  -> [Either Term Type] -- ^ The list of arguments
  -> RewriteMonad NormalizeState
                  ([Either Term Type]
                  ,[(Term,CaseTree [(Either Term Type)])]
                  )
collectGlobalsArgs substitution seen args = do
    (_,(args',collected)) <- second unzip <$> mapAccumLM go seen args
    return (args',concat collected)
  where
    go s (Left tm) = do
      (tm',collected) <- collectGlobals substitution s tm
      return (map fst collected ++ s,(Left tm',collected))
    go s (Right ty) = return (s,(Right ty,[]))

-- | Collect 'CaseTree's for (potentially) disjoint applications of globals out
-- of a list of alternatives. Also substitute truly disjoint applications of
-- globals by a reference to a lifted out application.
collectGlobalsAlts ::
     [(Term,Term)] -- ^ Substitution of (applications of) a global
                   -- binder by a reference to a lifted term.
  -> [Term] -- ^ List of already seen global binders
  -> Term -- ^ The subject term
  -> [Bind Pat Term] -- ^ The list of alternatives
  -> RewriteMonad NormalizeState
                  ([Bind Pat Term]
                  ,[(Term,CaseTree [(Either Term Type)])]
                  )
collectGlobalsAlts substitution seen scrut alts = do
    (alts',collected) <- unzip <$> mapM go alts
    let collected'  = List.groupBy ((==) `on` fst) (concat collected)
        collected'' = map (\xs -> (fst (head xs),Branch scrut (map snd xs)))
                          collected'
    return (alts',collected'')
  where
    go pe = do (p,e) <- unbind pe
               (e',collected) <- collectGlobals substitution seen e
               return (bind p e',map (second (p,)) collected)

-- | Given a case-tree corresponding to a disjoint interesting \"term-in-a-
-- function-position\", return a let-expression: where the let-binding holds
-- a case-expression selecting between the uncommon arguments of the case-tree,
-- and the body is an application of the term applied to the common arguments of
-- the case tree, and projections of let-binding corresponding to the uncommon
-- argument positions.
mkDisjointGroup :: (Term,CaseTree [(Either Term Type)])
                -> RewriteMonad NormalizeState Term
mkDisjointGroup (fun,cs) = do
    let argss    = Foldable.toList cs
        argssT   = zip [0..] (List.transpose argss)
        (commonT,uncommonT) = List.partition (allEqual . snd) argssT
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
      []       -> return (Nothing,[])
      -- only a single uncommon argument: no projections needed
      ([uc]:_) -> do argTy <- termType tcm uc
                     let c = genCase argTy Nothing [] cs''
                     return (Nothing,[c])
      -- multiple uncommon arguments: case statement that selects between the
      -- tuples that hold uncommon arguments, and projections for every element
      -- of the tuple.
      (uc:_)   -> do tupTcm <- Lens.view tupleTcCache
                     let m              = length uc
                         (Just tupTcNm) = IM.lookup m tupTcm
                         (Just tupTc)   = HashMap.lookup tupTcNm tcm
                         [tupDc]        = tyConDataCons tupTc
                     argTys <- mapM (termType tcm) uc
                     let tupTy  = mkTyConApp tupTcNm argTys
                         djCase = genCase tupTy (Just tupDc) argTys cs''
                     (scrutId,scrutVar) <- mkInternalVar "tupIn" tupTy
                     selectors <- mapM (mkSelectorCase
                                          ($(curLoc) ++ "mkDisjointGroup")
                                          tcm scrutVar (dcTag tupDc))
                                       [0..(m-1)]
                     return (Just (scrutId,djCase),selectors)
    let newArgs = mkDJArgs 0 common uncommonProjections
        newFun  = case uncommonCaseM of
                    Just (lbId,lbExpr) ->
                      Letrec (bind (rec [(lbId,embed lbExpr)])
                                   (mkApps fun newArgs))
                    Nothing -> mkApps fun newArgs
    return newFun

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
genCase _ Nothing _  (Leaf tms) = head tms
genCase _ (Just dc) argTys  (Leaf tms) =
  mkApps (Data dc) (map Right argTys ++ map Left tms)
genCase ty dc argTys (Branch scrut pats) =
  Case scrut ty (map (\(p,ct) -> bind p (genCase ty dc argTys ct)) pats)

-- | Determine if a term in a function position is interesting to lift out of
-- of a case-expression.
--
-- This holds for all global functions, and certain primitives. Currently those
-- primitives are:
--
-- * All non-constant multiplications
-- * All non-power-of-two division-like operations
interestingToLift :: (Term -> Term) -- ^ Evaluator
                  -> Term -- ^ Term in function position
                  -> [Either Term Type] -- ^ Arguments
                  -> Maybe Term
interestingToLift _    e@(Var _ _)   _    = Just e
interestingToLift eval e@(Prim nm _) args =
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

interestingToLift _ _ _ = Nothing
