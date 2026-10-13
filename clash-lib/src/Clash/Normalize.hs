{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2017     , Google Inc.,
                     2021-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Turn CoreHW terms into normalized CoreHW Terms

  The normalizer runs on Clash Core ("Clash.Core.Syntax"): the
  global bindings are closed terms, which GHC2Core produces and Netlist
  generation consumes.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

--{-# OPTIONS_GHC -ddump-splices #-}

module Clash.Normalize
  ( runNormalization
  , normalize
  , normalize'
  , checkNonRecursive
  , cleanupGraph
  , CallTree (..)
  , mkCallTree
  , flattenNode
  , flattenCallTree
  , callTreeToList
  , etaReduceWrapper
  ) where

import           Control.Exception                (throw)
import qualified Control.Lens                     as Lens
import           Control.Monad                    ((>=>), when)
import qualified Control.Monad                    as Monad
import           Control.Monad.IO.Class           (liftIO)
import           Data.Default                     (def)
import           Data.Either                      (partitionEithers)
import qualified Data.IntMap                      as IntMap
import qualified Data.IORef                       as IORef
import           Data.List                        (intercalate, mapAccumL)
import qualified Data.Map                         as Map
import qualified Data.Maybe                       as Maybe

#if MIN_VERSION_prettyprinter(1,7,0)
import           Prettyprinter                    (vcat)
#else
import           Data.Text.Prettyprint.Doc        (vcat)
#endif

import           GHC.BasicTypes.Extra             (isNoInline)

import           Clash.Core.Pretty                (PrettyOptions(..), showPpr, showPpr', ppr)
import           Clash.Debug                      (traceIf)
import           Clash.Driver.Types
  (Binding(..), DebugOpts(..), ClashEnv(..), GlobalBindings)
import           Clash.Netlist.Types
  (HWTypeTranslator)
import           Clash.Normalize.Strategy         (constantPropagation, normalization)
import           Clash.Normalize.Strategy.Spec    (flattenSpec)
import           Clash.Normalize.Types
import           Clash.Normalize.Util
  (isCheapFunction, normalizeTopLvlBndr, rewriteExpr, tvSubstWithTyEq)
import           Clash.Rewrite.StrategyDSL.TH     (compileStrategy)
import           Clash.Rewrite.Types
  (RewriteEnv (..), RewriteState (..), debugOpts, extra, bindings,
   topEntities, newInlineStrategy, typeEnv)
import           Clash.Rewrite.Util
  (runRewriteSession, isUntranslatableType)
import           Clash.Util
import           Clash.Util.Interpolate           (i)
import           Clash.Util.Supply                (Supply)

import           Clash.Rewrite.History (recordRewriteStep)

import           Clash.Core.FreeVars       (termGlobalIds, teleNameBinders)
import           Clash.Core.NameMap
import           Clash.Core.Subst          (substGlobals, unsinkTerm)
import           Clash.Core.Syntax         hiding (Type)
import qualified Clash.Core.Syntax         as S
import           Clash.Core.Term
  (AbsBinder (..), collectArgsTicks, collectTicks, mkApps, mkTicks, openBndrs,
   stripTicks)
import           Clash.Core.TypeEnv        (mkTypeEnv)
import           Clash.Core.Type
  (SplitCoreFunForall (..), isPolyTy, piTeleTyArity, splitCoreFunForallTy)
import           Clash.Core.Alpha          (aeqTy)

-- | Run a NormalizeSession in a given environment
runNormalization
  :: ClashEnv
  -> Supply
  -- ^ UniqueSupply
  -> GlobalBindings
  -- ^ Global Binders
  -> HWTypeTranslator
  -- ^ Hardcoded Type -> HWType translator
  -> GlobalEnv Bool
  -- ^ Map telling whether a components is part of a recursive group
  -> [GlobalId]
  -- ^ topEntities
  -> NormalizeSession a
  -- ^ NormalizeSession to run
  -> IO a
runNormalization env supply globals typeTrans rcsMap topEnts m =
  runRewriteSession rwEnv rwState m
  where
    -- TODO The RewriteEnv should just take ClashOpts.
    rwEnv     = RewriteEnv
                  env
                  typeTrans
                  (globalSetFromList topEnts)
                  (mkTypeEnv (envTyConMap env))

    rwState   = RewriteState
                  { _transformCounter = 0
                  , _transformAppliedCounters = mempty
                  , _transformTriedCounters = mempty
                  , _uniqSupply = supply
                  , _curFun = (error $ $(curLoc) ++ "Report as bug: no curFun",noSrcSpan)
                  , _nameCounter = 0
                  , _globalHeap = (IntMap.empty, 0)
                  , _bindings = globals
                  , _workFreeBinders = emptyGlobalEnv
                  , _hwTypeCache = Map.empty
                  , _extra = normState
                  }

    normState = NormalizeState
                  { _primitiveArgs = Map.empty
                  , _normalizationStrategy = normalization
                  , _constantPropagationStrategy = constantPropagation
                  , _normalized = emptyGlobalEnv
                  , _specialisationCache = Map.empty
                  , _specialisationHistory = emptyGlobalEnv
                  , _inlineHistory = emptyGlobalEnv
                  , _recursiveComponents = rcsMap
                  , _workFreeAppCache = Map.empty
                  }

-- | Normalize the given global binders, and the ones they refer to
normalize
  :: [GlobalId]
  -> NormalizeSession (GlobalEnv (Binding (Term 'VoidS)))
normalize = go >=> unionWithCache
 where
  go []  = return emptyGlobalEnv
  go top = do
    (new,topNormalized) <- unzip <$> mapM normalize' top
    newNormalized <- normalize (concat new)
    return (unionGlobalEnv (globalEnvFromList topNormalized) newNormalized)

  unionWithCache
    :: GlobalEnv (Binding (Term 'VoidS))
    -> NormalizeSession (GlobalEnv (Binding (Term 'VoidS)))
  unionWithCache env = do
    cache <- Lens.use (extra.normalized)
    -- We need to include the cache in our final result, forgetting to do so
    -- leads to https://github.com/clash-lang/clash-compiler/issues/3109
    --
    -- On the other hand, just returning the cache as our final result could
    -- not be enough, because normalize' might return a non-normalized binder
    -- that is later picked up and cleaned up by flattenCallTree.
    return (unionGlobalEnv cache env)

-- | Normalize a global binder. Gives the globals it refers to that still need
-- normalizing, and the normalized binding.
normalize'
  :: GlobalId
  -> NormalizeSession ([GlobalId], (GlobalId, Binding (Term 'VoidS)))
normalize' nm = do
  exprM <- lookupGlobalEnv nm <$> Lens.use bindings
  let nmS = showPpr (globalIdName nm)
  case exprM of
    Just (Binding nmG sp inl pr tm r) -> do
      te <- Lens.view typeEnv
      topEnts <- Lens.view topEntities
      let isTop = nm `memberGlobalSet` topEnts
          ty0 = globalIdType nmG
          ty1 = if isTop then tvSubstWithTyEq emptyScope ty0 else ty0

      -- check for polymorphic types
      when (isPolyTy ty1) $
        let msg = $curLoc ++ [i|
              Clash can only normalize monomorphic functions, but this is polymorphic:
              #{showPpr' def{displayUniques=False\} nmG}
              |]
            msgExtra | aeqTy ty0 ty1 = Nothing
                     | otherwise = Just $ [i|
              Even after applying type equality constraints it remained polymorphic:
              #{showPpr' def{displayUniques=False\} (mkGlobalId (globalIdName nmG) ty1)}
                         |]
        in throw (ClashException sp msg msgExtra)

      -- check for unrepresentable result type
      (isFunction, resTyRep) <- case splitCoreFunForallTy te emptyScope ty1 of
        SplitCoreFunForall scR tele resTy -> do
          resTyRep <- not <$> isUntranslatableType False scR resTy
          -- Whether it quantifies over type variables
          pure (piTeleTyArity tele /= 0, resTyRep)
      if resTyRep
         then do
            tmNorm <- normalizeTopLvlBndr isTop nm (Binding nmG sp inl pr tm r)
            let usedBndrs = globalsInOrder (bindingTerm tmNorm)
            traceIf (bindingRecursive tmNorm)
                    (concat [ $(curLoc),"Expr belonging to bndr: ",nmS ," (:: "
                            , showPpr (globalIdType (bindingId tmNorm))
                            , ") remains recursive after normalization:\n"
                            , showPpr (bindingTerm tmNorm) ])
                    (return ())
            prevNorm <- Lens.use (extra.normalized)
            let toNormalize = filter (\g -> not (g `memberGlobalSet` topEnts))
                            $ filter (\g -> g /= nm && not (memberGlobalEnv g prevNorm)) usedBndrs
            return (toNormalize,(nm,tmNorm))
         else
           do
            -- Throw an error for unrepresentable topEntities and functions
            when (isTop || isFunction) $
              let msg = $(curLoc) ++ [i|
                    This bndr has a non-representable return type and can't be normalized:
                    #{showPpr' def{displayUniques=False\} nmG}
                    |]
              in throw (ClashException sp msg Nothing)

            -- But allow the compilation to proceed for nonrepresentable values.
            -- This can happen for example when GHC decides to create a toplevel binder
            -- for the ByteArray# inside of a Natural constant.
            -- (GHC-8.4 does this with tests/shouldwork/Numbers/Exp.hs)
            -- It will later be inlined by flattenCallTree.
            opts <- Lens.view debugOpts
            -- Which already-normalized binders reference this one? Without that,
            -- the trace below says nothing about where the offending binder came
            -- from.
            referers <-
              if dbg_invariants opts
                 then do
                   prevNorm <- Lens.use (extra.normalized)
                   pure [ showPpr (globalIdName (bindingId b))
                        | b <- globalEnvElems prevNorm
                        , nm `memberGlobalSet` termGlobalIds (bindingTerm b)
                        ]
                 else pure []
            traceIf (dbg_invariants opts)
                    (concat [$(curLoc), "Expr belonging to bndr: ", nmS, " (:: "
                            , showPpr (globalIdType nmG)
                            , ") has a non-representable return type."
                            , " Referenced by: "
                            , if null referers
                                 then "nothing normalized so far"
                                 else intercalate ", " referers
                            , ". Not normalizing:\n", showPpr tm] )
                    (return ([],(nm,(Binding nmG sp inl pr tm r))))


    Nothing -> error $ $(curLoc) ++ "Expr belonging to bndr: " ++ nmS ++ " not found"

-- | The globals a term refers to, in the order in which they first occur: the
-- right-hand sides of a let-expression before its body, and the term of a tick
-- before the term it is attached to. It determines the order in which
-- 'normalize' normalizes them, and with that, the uniques of the globals that
-- normalization creates.
globalsInOrder :: Term n -> [GlobalId]
globalsInOrder = dedup emptyGlobalSet . ($ []) . go
 where
  dedup _ [] = []
  dedup seen (g : gs)
    | g `memberGlobalSet` seen = dedup seen gs
    | otherwise = g : dedup (insertGlobalSet g seen) gs

  go :: Term x -> [GlobalId] -> [GlobalId]
  go = \case
    GlobalVar g -> (g :)
    Var {} -> id
    Data {} -> id
    Literal {} -> id
    Prim {} -> id
    Lam _ e -> go e
    TyLam _ e -> go e
    App e1 e2 -> go e1 . go e2
    TyApp e _ -> go e
    Let (NonRec _ x) e -> go x . go e
    Let (Rec _ xs) e -> foldr ((.) . go) id xs . go e
    Case e _ alts -> go e . foldr (\(Alt _ x) -> (go x .)) id alts
    Cast e _ _ -> go e
    Tick (Attributes _ a) e -> go a . go e
    Tick _ e -> go e

-- | Check whether the normalized bindings are non-recursive. Errors when one
-- of the components is recursive.
checkNonRecursive
  :: GlobalEnv (Binding (Term 'VoidS))
  -- ^ List of normalized binders
  -> GlobalEnv (Binding (Term 'VoidS))
checkNonRecursive norm = case filterGlobalEnv bindingRecursive norm of
  rcs | nullGlobalEnv rcs  -> norm
  rcs -> error $ $(curLoc) ++ "Callgraph after normalization contains following recursive components: "
                   ++ show (vcat [ ppr (bindingId b) <> ppr (bindingTerm b)
                                 | b <- globalEnvElems rcs
                                 ])

-- | Perform general \"clean up\" of the normalized (non-recursive) function
-- hierarchy. This includes:
--
--   * Inlining functions that simply \"wrap\" another function
cleanupGraph
  :: GlobalId
  -> GlobalEnv (Binding (Term 'VoidS))
  -> NormalizeSession (GlobalEnv (Binding (Term 'VoidS)))
cleanupGraph topEntity norm
  | Just ct <- mkCallTree [] norm topEntity
  = do cache <- liftIO (IORef.newIORef emptyGlobalEnv)
       ctFlat <- flattenCallTree cache ct
       return (globalEnvFromList $ snd $ callTreeToList [] ctFlat)
cleanupGraph _ norm = return norm

-- | A tree of identifiers and their bindings, with branches containing
-- additional bindings which are used. See "Clash.Driver.Types.Binding".
--
data CallTree
  = CLeaf   (GlobalId, Binding (Term 'VoidS))
  | CBranch (GlobalId, Binding (Term 'VoidS)) [CallTree]

mkCallTree
  :: [GlobalId]
  -- ^ Visited
  -> GlobalEnv (Binding (Term 'VoidS))
  -- ^ Global binders
  -> GlobalId
  -- ^ Root of the call graph
  -> Maybe CallTree
mkCallTree visited bindingMap root
  | Just rootTm <- lookupGlobalEnv root bindingMap
  = let used   = globalSetToList $ termGlobalIds $ bindingTerm rootTm
        other  = Maybe.mapMaybe (mkCallTree (root:visited) bindingMap) (filter (`notElem` visited) used)
    in  case used of
          [] -> Just (CLeaf   (root,rootTm))
          _  -> Just (CBranch (root,rootTm) other)
mkCallTree _ _ _ = Nothing

-- | The shape of a normalized function whose body binds a single variable,
-- see 'etaReduceWrapper'
data SingleBinding
  = NotSingle
  -- ^ Not in normal form, or binds more than one variable
  | Single (Maybe (Term 'VoidS))
  -- ^ Binds a single variable. If the function only applies another function
  -- to its own arguments, in order, the eta-reduced function.

-- | Whether a normalized function, which has the shape
-- @\\x1 .. xn -> let y = e in y@ (see
-- 'Clash.Netlist.Util.splitNormalized'), binds a single variable, and
-- if so, whether it is a wrapper @\\x1 .. xn -> let y = f a1 .. am x1 .. xn
-- in y@ that eta-reduces to @f a1 .. am@: when @f@ and the @ai@ mention none
-- of the @xi@, nor @y@. The ticks around the let-expression and around the
-- application are kept. It only eta-reduces a wrapper whose result is its
-- let-bound variable.
etaReduceWrapper :: Term 'VoidS -> SingleBinding
etaReduceWrapper e = openBndrs emptyScope e $ \_ bs body ->
  case (termBinders bs, collectTicks body) of
    (Just ids, (Let bind res, ticks))
      | not (isLet res) -> case bind of
          NonRec b x -> single ids bs ticks (isResult b res) (Just x)
          Rec (TCons b TNil) [x] ->
            single ids bs ticks (isResult b res) (unsinkTerm (teleNameBinders (TCons b TNil)) x)
          _ -> NotSingle
    _ -> NotSingle
 where
  single
    :: [Int] -> Tele AbsBinder 'VoidS l -> [TickInfo l] -> Bool -> Maybe (Term l)
    -> SingleBinding
  single ids bs ticks isRes rhsM = Single $ do
    rhs <- rhsM
    Monad.guard isRes
    let (fun, args, ticks') = collectArgsTicks (mkTicks rhs ticks)
        nArgs = length args
        nIds = length ids
    Monad.guard (nArgs >= nIds)
    let (remainder, idArgs) = splitAt (nArgs - nIds) args
    Monad.guard (map argVar idArgs == map Just ids)
    unsinkTerm (teleNameBinders bs) (mkApps (mkTicks fun ticks') remainder)

  -- The raw names of the variables of the term binders, if there are no type
  -- binders
  termBinders :: Tele AbsBinder x y -> Maybe [Int]
  termBinders = \case
    TNil -> Just []
    TCons (AbsId b) rest -> (binderRaw b :) <$> termBinders rest
    TCons (AbsTy _) _ -> Nothing

  binderRaw :: IdBinder x y -> Int
  binderRaw = tmNameRaw . tmNameOf

  argVar :: Either (Term x) (S.Type x) -> Maybe Int
  argVar = \case
    Left (Var _ _ n) -> Just (tmNameRaw n)
    _ -> Nothing

  isResult :: IdBinder x y -> Term y -> Bool
  isResult b res = case stripTicks res of
    Var _ _ n -> tmNameRaw n == binderRaw b
    _ -> False

  isLet :: Term x -> Bool
  isLet = \case
    Let {} -> True
    _ -> False

flattenNode
  :: CallTree
  -> NormalizeSession (Either CallTree ((GlobalId, Term 'VoidS), [CallTree]))
flattenNode c@(CLeaf (_,(Binding _ _ spec _ _ _))) | isNoInline spec = return (Left c)
flattenNode c@(CLeaf (nm,(Binding _ _ _ _ e _))) = do
  isTopEntity <- memberGlobalSet nm <$> Lens.view topEntities
  if isTopEntity then return (Left c) else
    case etaReduceWrapper e of
      Single (Just e') -> return (Right ((nm,e'),[]))
      _ -> return (Right ((nm,e),[]))
flattenNode b@(CBranch (_,(Binding _ _ spec _ _ _)) _) | isNoInline spec =
  return (Left b)
flattenNode b@(CBranch (nm,(Binding _ _ _ _ e _)) us) = do
  isTopEntity <- memberGlobalSet nm <$> Lens.view topEntities
  if isTopEntity then return (Left b) else
    case etaReduceWrapper e of
      Single (Just e') -> return (Right ((nm,e'),us))
      Single Nothing -> return (Right ((nm,e),us))
      NotSingle -> do
        newInlineStrat <- Lens.view newInlineStrategy
        if newInlineStrat || isCheapFunction e
           then return (Right ((nm,e),us))
           else return (Left b)

-- | Flatten a 'CallTree', memoizing results by binder within one cleanup
-- pass. Without the cache, every binder reachable from the root is flattened
-- as many times as it appears in the (un-deduplicated) call tree.
flattenCallTree
  :: IORef.IORef (GlobalEnv CallTree)
  -- ^ Memo cache, keyed by binder. Local to one 'cleanupGraph' call.
  -> CallTree
  -> NormalizeSession CallTree
flattenCallTree _ c@(CLeaf _) = return c
flattenCallTree cache (CBranch (nm,(Binding nm' sp inl pr tm r)) used) = do
  -- XXX: Careful! If you ever add concurrency, this will have to be changed to
  --      account for multiple workers.
  cached <- liftIO (lookupGlobalEnv nm <$> IORef.readIORef cache)
  case cached of
    Just ct -> pure ct
    Nothing -> do
      ct <- doFlatten
      liftIO (IORef.modifyIORef' cache (insertGlobalEnv nm ct))
      pure ct
 where
  doFlatten = do
   flattenedUsed   <- mapM (flattenCallTree cache) used
   (newUsed,il_ct) <- partitionEithers <$> mapM flattenNode flattenedUsed
   let (toInline,il_used) = unzip il_ct
   newExpr <- case toInline of
     [] -> return tm
     _  -> do
       let tm1 = substGlobals (globalEnvFromList toInline) tm

       -- NB: When -fclash-debug-history is on, record the inlining
       recordRewriteStep "INLINE" (showPpr (globalIdName nm')) tm tm1
       rewriteExpr ("flattenExpr",flatten) (showPpr (globalIdName nm'), tm1) (nm, sp)
   let allUsed = newUsed ++ concat il_used
   -- inline all components when the resulting expression after flattening
   -- is still considered "cheap". This happens often at the topEntity which
   -- wraps another functions and has some selectors and data-constructors.
   if not (isNoInline inl) && isCheapFunction newExpr
      then do
         let (toInline',allUsed') = unzip (map goCheap allUsed)
             tm1 = substGlobals (globalEnvFromList (Maybe.catMaybes toInline')) newExpr
         newExpr' <- rewriteExpr ("flattenCheap",flatten) (showPpr (globalIdName nm'), tm1) (nm, sp)
         return (CBranch (nm,(Binding nm' sp inl pr newExpr' r)) (concat allUsed'))
      else return (CBranch (nm,(Binding nm' sp inl pr newExpr r)) allUsed)

  -- The flattening strategy is described in Clash.Normalize.Strategy.Spec.
  flatten = $(compileStrategy flattenSpec)

  goCheap c@(CLeaf   (nm2,(Binding _ _ inl2 _ e _)))
    | isNoInline inl2  = (Nothing     ,[c])
    | otherwise        = (Just (nm2,e),[])
  goCheap c@(CBranch (nm2,(Binding _ _ inl2 _ e _)) us)
    | isNoInline inl2  = (Nothing, [c])
    | otherwise        = (Just (nm2,e),us)

callTreeToList
  :: [GlobalId] -> CallTree -> ([GlobalId], [(GlobalId, Binding (Term 'VoidS))])
callTreeToList visited (CLeaf (nm,bndr))
  | nm `elem` visited = (visited,[])
  | otherwise         = (nm:visited,[(nm,bndr)])
callTreeToList visited (CBranch (nm,bndr) used)
  | nm `elem` visited = (visited,[])
  | otherwise         = (visited',(nm,bndr):(concat others))
  where
    (visited',others) = mapAccumL callTreeToList (nm:visited) used
