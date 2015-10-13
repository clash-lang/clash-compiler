{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}
module CLaSH.Normalize.DEC where

-- external
import qualified Control.Lens                     as Lens
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
import CLaSH.Core.Term       (Pat (..), Term (..), TmName)
import CLaSH.Core.TyCon      (tyConDataCons)
import CLaSH.Core.Type       (Type, mkTyConApp, splitFunForallTy)
import CLaSH.Core.Util       (collectArgs, mkApps, termType)
import CLaSH.Normalize.Types (NormalizeState)
import CLaSH.Normalize.Util  (isConstant)
import CLaSH.Rewrite.Types   (RewriteMonad, bindings, tcCache, tupleTcCache)
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
     [(TmName,Term)] -- ^ Substitution of (applications of) a global binder by a
                     -- reference to a lifted term.
  -> [TmName] -- ^ List of already seen global binders
  -> Term -- ^ The expression
  -> RewriteMonad NormalizeState (Term,[(TmName,CaseTree [(Either Term Type)])])
collectGlobals substitution seen (Case scrut ty alts) = do
  (scrut',collected)  <- collectGlobals     substitution seen scrut
  (alts' ,collected') <- collectGlobalsAlts substitution seen scrut' alts
  return (Case scrut' ty alts',collected ++ collected')

collectGlobals substitution seen e@(collectArgs -> (fun, args@(_:_)))
  | not (isConstant e) = do
    tcm <- Lens.view tcCache
    eTy <- termType tcm e
    case splitFunForallTy eTy of
      ([],_) -> case fun of
        (Var _ nm) | nm `notElem` seen -> do
          (args',collected) <- collectGlobalsArgs substitution (nm:seen) args
          let e' = Maybe.fromMaybe e (List.lookup nm substitution)
          return (e',(nm,Leaf args'):collected)
        _ -> do (args',collected) <- collectGlobalsArgs substitution seen args
                return (mkApps fun args',collected)
      _ -> return (e,[])

collectGlobals _ _ e = return (e,[])

-- | Collect 'CaseTree's for (potentially) disjoint applications of globals out
-- of a list of application arguments. Also substitute truly disjoint
-- applications of globals by a reference to a lifted out application.
collectGlobalsArgs ::
     [(TmName,Term)] -- ^ Substitution of (applications of) a global binder by a
                     -- reference to a lifted term.
  -> [TmName] -- ^ List of already seen global binders
  -> [Either Term Type] -- ^ The list of arguments
  -> RewriteMonad NormalizeState ([Either Term Type]
                                 ,[(TmName,CaseTree [(Either Term Type)])]
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
     [(TmName,Term)] -- ^ Substitution of (applications of) a global binder by a
                     -- reference to a lifted term.
  -> [TmName] -- ^ List of already seen global binders
  -> Term -- ^ The subject term
  -> [Bind Pat Term] -- ^ The list of alternatives
  -> RewriteMonad NormalizeState ([Bind Pat Term]
                                 ,[(TmName,CaseTree [(Either Term Type)])]
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

mkDisjointGroup :: (TmName,CaseTree [(Either Term Type)])
                -> RewriteMonad NormalizeState Term
mkDisjointGroup (nm,cs) = do
    Just (ty,_) <- HashMap.lookup nm <$> Lens.use bindings
    let argss    = Foldable.toList cs
        argssT   = zip [0..] (List.transpose argss)
        (commonT,uncommonT) = List.partition (allEqual . snd) argssT
        common   = map (second head) commonT
        uncommon = map (Either.lefts) (List.transpose (map snd uncommonT))
        cs'      = removeEmpty
                 $ fmap (Either.lefts)
                        (if null common
                           then cs
                           else fmap (filter (`notElem` (map snd common))) cs)
    tcm <- Lens.view tcCache
    (uncommonCaseM,uncommonProjections) <- case uncommon of
      -- only common arguments: do nothing.
      []       -> return (Nothing,[])
      -- only a single uncommon argument: no projections needed
      ([uc]:_) -> do argTy <- termType tcm uc
                     let c = genCase argTy Nothing [] cs'
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
                         djCase = genCase tupTy (Just tupDc) argTys cs'
                     (scrutId,scrutVar) <- mkInternalVar "tupIn" tupTy
                     selectors <- mapM (mkSelectorCase
                                          ($(curLoc) ++ "mkDisjointGroup")
                                          tcm scrutVar (dcTag tupDc))
                                       [0..(m-1)]
                     return (Just (scrutId,djCase),selectors)
    let newArgs = mkDJArgs 0 common uncommonProjections
        fun     = Var ty nm
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
