{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Utilities for rewriting: e.g. inlining, specialisation, etc.
module CLaSH.Rewrite.Util where

import           Control.DeepSeq
import           Control.Lens                (Lens', (%=), (+=), (^.))
import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
import qualified Control.Monad.Reader        as Reader
import qualified Control.Monad.State         as State
import           Control.Monad.Trans.Class   (lift)
import qualified Control.Monad.Writer        as Writer
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Lazy           as HML
import qualified Data.HashMap.Strict         as HMS
import qualified Data.List                   as List
import qualified Data.Map                    as Map
import qualified Data.Monoid                 as Monoid
import qualified Data.Set                    as Set
import qualified Data.Set.Lens               as Lens
import           Unbound.Generics.LocallyNameless     (Fresh, bind,
                                              embed, makeName, name2String,
                                              rebind, rec, string2Name, unbind,
                                              unembed, unrec)
import qualified Unbound.Generics.LocallyNameless     as Unbound

import           CLaSH.Core.DataCon          (dataConInstArgTys)
import           CLaSH.Core.FreeVars         (termFreeIds, termFreeTyVars, typeFreeVars)
import           CLaSH.Core.Pretty           (showDoc)
import           CLaSH.Core.Subst            (substTm)
import           CLaSH.Core.Term             (LetBinding, Pat (..), Term (..),
                                              TmName)
import           CLaSH.Core.TyCon            (TyCon, TyConName, tyConDataCons)
import           CLaSH.Core.Type             (KindOrType, Type (..),
                                              TypeView (..), transparentTy,
                                              typeKind, coreView)
import           CLaSH.Core.Util             (Delta, Gamma, collectArgs,
                                              mkAbstraction, mkApps, mkId,
                                              mkLams, mkTmApps, mkTyApps,
                                              mkTyLams, mkTyVar, termType)
import           CLaSH.Core.Var              (Id, TyVar, Var (..))
import           CLaSH.Netlist.Util          (representableType)
import           CLaSH.Rewrite.Types
import           CLaSH.Util

-- | Lift an action working in the inner monad to the 'RewriteMonad'
liftR :: Monad m => m a -> RewriteMonad m a
liftR m = lift . lift . lift . lift $ m

-- | Lift an action working in the inner monad to the 'RewriteSession'
liftRS :: Monad m => m a -> RewriteSession m a
liftRS m = lift . lift . lift $ m

-- | Record if a transformation is succesfully applied
apply :: (Monad m, Functor m)
      => String -- ^ Name of the transformation
      -> Rewrite m -- ^ Transformation to be applied
      -> Rewrite m
apply name rewrite ctx expr = R $ do
  lvl <- Lens.view dbgLevel
  let before = showDoc expr
  (expr', anyChanged) <- traceIf (lvl >= DebugAll) ("Trying: " ++ name ++ " on:\n" ++ before) $ Writer.listen $ runR $ rewrite ctx expr
  let hasChanged = Monoid.getAny anyChanged
  Monad.when hasChanged $ transformCounter += 1
  let after  = showDoc expr'
  let expr'' = if hasChanged then expr' else expr

  Monad.when (lvl > DebugNone && hasChanged) $ do
    tcm                  <- Lens.use tcCache
    beforeTy             <- fmap transparentTy $ termType tcm expr
    let beforeFTV        = Lens.setOf termFreeTyVars expr
    beforeFV             <- Lens.setOf <$> localFreeIds <*> pure expr
    afterTy              <- fmap transparentTy $ termType tcm expr'
    let afterFTV         = Lens.setOf termFreeTyVars expr
    afterFV              <- Lens.setOf <$> localFreeIds <*> pure expr'
    let newFV = Set.size afterFTV > Set.size beforeFTV ||
                Set.size afterFV > Set.size beforeFV
    Monad.when newFV $
            error ( concat [ $(curLoc)
                           , "Error when applying rewrite ", name
                           , " to:\n" , before
                           , "\nResult:\n" ++ after ++ "\n"
                           , "Changes free variables from: ", show (beforeFTV,beforeFV)
                           , "\nto: ", show (afterFTV,afterFV)
                           ]
                  )
    traceIf ( beforeTy /= afterTy)
            ( concat [ $(curLoc)
                     , "Error when applying rewrite ", name
                     , " to:\n" , before
                     , "\nResult:\n" ++ after ++ "\n"
                     , "Changes type from:\n", showDoc beforeTy
                     , "\nto:\n", showDoc afterTy
                     ]
            ) (return ())

  Monad.when (lvl >= DebugApplied && not hasChanged && expr /= expr') $
    error $ $(curLoc) ++ "Expression changed without notice(" ++ name ++  "): before" ++ before ++ "\nafter:\n" ++ after

  traceIf (lvl >= DebugName && hasChanged) name $
    traceIf (lvl >= DebugApplied && hasChanged) ("Changes when applying rewrite to:\n" ++ before ++ "\nResult:\n" ++ after ++ "\n") $
      traceIf (lvl >= DebugAll && not hasChanged) ("No changes when applying rewrite " ++ name ++ " to:\n" ++ after ++ "\n") $
        return expr''

-- | Perform a transformation on a Term
runRewrite :: (Monad m, Functor m)
           => String -- ^ Name of the transformation
           -> Rewrite m -- ^ Transformation to perform
           -> Term  -- ^ Term to transform
           -> RewriteSession m Term
runRewrite name rewrite expr = do
  (expr',_) <- Writer.runWriterT . runR $ apply name rewrite [] expr
  return expr'

-- | Evaluate a RewriteSession to its inner monad
runRewriteSession :: (Functor m, Monad m)
                  => DebugLevel
                  -> RewriteState
                  -> RewriteSession m a
                  -> m a
runRewriteSession lvl st
  = Unbound.runFreshMT
  . fmap (\(a,s) -> traceIf True ("Applied " ++ show (s ^. transformCounter) ++ " transformations") a)
  . (`State.runStateT` st)
  . (`Reader.runReaderT` RE lvl)

-- | Notify that a transformation has changed the expression
setChanged :: Monad m => RewriteMonad m ()
setChanged = Writer.tell (Monoid.Any True)

-- | Identity function that additionally notifies that a transformation has
-- changed the expression
changed :: Monad m => a -> RewriteMonad m a
changed val = do
  Writer.tell (Monoid.Any True)
  return val

-- | Create a type and kind context out of a transformation context
contextEnv :: [CoreContext]
           -> (Gamma, Delta)
contextEnv = go HML.empty HML.empty
  where
    go gamma delta []                   = (gamma,delta)
    go gamma delta (LetBinding ids:ctx) = go gamma' delta ctx
      where
        gamma' = foldl addToGamma gamma ids

    go gamma delta (LetBody ids:ctx)    = go gamma' delta ctx
      where
        gamma' = foldl addToGamma gamma ids

    go gamma delta (LamBody lId:ctx)    = go gamma' delta ctx
      where
        gamma' = addToGamma gamma lId

    go gamma delta (TyLamBody tv:ctx)   = go gamma delta' ctx
      where
        delta' = addToDelta delta tv

    go gamma delta (CaseAlt ids:ctx)    = go gamma' delta ctx
      where
        gamma' = foldl addToGamma gamma ids

    go gamma delta (_:ctx) = go gamma delta ctx

    addToGamma gamma (Id idName ty) = HML.insert idName (unembed ty) gamma
    addToGamma _     _              = error $ $(curLoc) ++ "Adding TyVar to Gamma"

    addToDelta delta (TyVar tvName ki) = HML.insert tvName (unembed ki) delta
    addToDelta _     _                 = error $ $(curLoc) ++ "Adding Id to Delta"

-- | Create a complete type and kind context out of the global binders and the
-- transformation context
mkEnv :: (Functor m, Monad m)
      => [CoreContext]
      -> RewriteMonad m (Gamma, Delta)
mkEnv ctx = do
  let (gamma,delta) = contextEnv ctx
  tsMap             <- fmap (HML.map fst) $ Lens.use bindings
  let gamma'        = tsMap `HML.union` gamma
  return (gamma',delta)

-- | Make a new binder and variable reference for a term
mkTmBinderFor :: (Functor m, Fresh m, MonadUnique m)
              => HashMap TyConName TyCon -- ^ TyCon cache
              -> String -- ^ Name of the new binder
              -> Term -- ^ Term to bind
              -> m (Id, Term)
mkTmBinderFor tcm name e = do
  (Left r) <- mkBinderFor tcm name (Left e)
  return r

-- | Make a new binder and variable reference for either a term or a type
mkBinderFor :: (Functor m, Monad m, MonadUnique m, Fresh m)
            => HashMap TyConName TyCon -- ^ TyCon cache
            -> String -- ^ Name of the new binder
            -> Either Term Type -- ^ Type or Term to bind
            -> m (Either (Id,Term) (TyVar,Type))
mkBinderFor tcm name (Left term) =
  Left <$> (mkInternalVar name =<< termType tcm term)

mkBinderFor tcm name (Right ty) = do
  name'     <- fmap (makeName name . toInteger) getUniqueM
  let kind  = typeKind tcm ty
  return $ Right (TyVar name' (embed kind), VarTy kind name')

-- | Make a new, unique, identifier and corresponding variable reference
mkInternalVar :: (Functor m, Monad m, MonadUnique m)
              => String -- ^ Name of the identifier
              -> KindOrType
              -> m (Id,Term)
mkInternalVar name ty = do
  name' <- fmap (makeName name . toInteger) getUniqueM
  return (Id name' (embed ty),Var ty name')

-- | Inline the binders in a let-binding that have a certain property
inlineBinders :: Monad m
              => (LetBinding -> RewriteMonad m Bool) -- ^ Property test
              -> Rewrite m
inlineBinders condition _ expr@(Letrec b) = R $ do
  (xes,res)        <- unbind b
  (replace,others) <- partitionM condition (unrec xes)
  case replace of
    [] -> return expr
    _  -> do
      let (others',res') = substituteBinders replace others res
          newExpr = case others' of
                          [] -> res'
                          _  -> Letrec (bind (rec others') res')
      changed newExpr

inlineBinders _ _ e = return e

-- | Substitute the RHS of the first set of Let-binders for references to the
-- first set of Let-binders in: the second set of Let-binders and the additional
-- term
substituteBinders :: [LetBinding] -- ^ Let-binders to substitute
                  -> [LetBinding] -- ^ Let-binders where substitution takes place
                  -> Term -- ^ Expression where substitution takes place
                  -> ([LetBinding],Term)
substituteBinders [] others res = (others,res)
substituteBinders ((bndr,valE):rest) others res = substituteBinders rest' others' res'
  where
    val      = unembed valE
    bndrName = varName bndr
    selfRef  = bndrName `elem` Lens.toListOf termFreeIds val
    (res',rest',others') = if selfRef
      then (res,rest,(bndr,valE):others)
      else ( substTm (varName bndr) val res
           , map (second ( embed
                         . substTm bndrName val
                         . unembed)
                 ) rest
           , map (second ( embed
                         . substTm bndrName val
                         . unembed)
                 ) others
           )

-- | Calculate the /local/ free variable of an expression: the free variables
-- that are not bound in the global environment.
localFreeIds :: (Applicative f, Lens.Contravariant f, Monad m)
             => RewriteMonad m ((TmName -> f TmName) -> Term -> f Term)
localFreeIds = do
  globalBndrs <- Lens.use bindings
  return ((termFreeIds . Lens.filtered (not . (`HML.member` globalBndrs))))

-- | Lift the binders in a let-binding to a global function that have a certain
-- property
liftBinders :: (Functor m, Monad m)
            => (LetBinding -> RewriteMonad m Bool) -- ^ Property test
            -> Rewrite m
liftBinders condition ctx expr@(Letrec b) = R $ do
  (xes,res)        <- unbind b
  (replace,others) <- partitionM condition (unrec xes)
  case replace of
    [] -> return expr
    _  -> do
      (gamma,delta) <- mkEnv (LetBinding (map fst $ unrec xes) : ctx)
      replace' <- mapM (liftBinding gamma delta) replace
      let (others',res') = substituteBinders replace' others res
          newExpr = case others' of
                          [] -> res'
                          _  -> Letrec (bind (rec others') res')
      changed newExpr

liftBinders _ _ e = return e

-- | Create a global function for a Let-binding and return a Let-binding where
-- the RHS is a reference to the new global function applied to the free
-- variables of the original RHS
liftBinding :: (Functor m, Monad m)
            => Gamma
            -> Delta
            -> LetBinding
            -> RewriteMonad m LetBinding
liftBinding gamma delta (Id idName tyE,eE) = do
  let ty = unembed tyE
      e  = unembed eE
  -- Get all local FVs, excluding the 'idName' from the let-binding
  let localFTVs = List.nub $ Lens.toListOf termFreeTyVars e
  localFVs <- List.nub <$> (Lens.toListOf <$> localFreeIds <*> pure e)
  let localFTVkinds = map (\k -> HML.lookupDefault (error $ $(curLoc) ++ show k ++ " not found") k delta) localFTVs
      localFVs'     = filter (/= idName) localFVs
      localFVtys'   = map (\k -> HML.lookupDefault (error $ $(curLoc) ++ show k ++ " not found") k gamma) localFVs'
  -- Abstract expression over its local FVs
      boundFTVs = zipWith mkTyVar localFTVkinds localFTVs
      boundFVs  = zipWith mkId localFVtys' localFVs'
  -- Make a new global ID
  tcm       <- Lens.use tcCache
  newBodyTy <- termType tcm $ mkTyLams (mkLams e boundFVs) boundFTVs
  newBodyId <- fmap (makeName (name2String idName) . toInteger) getUniqueM
  -- Make a new expression, consisting of the the lifted function applied to
  -- its free variables
  let newExpr = mkTmApps
                  (mkTyApps (Var newBodyTy newBodyId)
                            (zipWith VarTy localFTVkinds localFTVs))
                  (zipWith Var localFVtys' localFVs')
  -- Substitute the recursive calls by the new expression
      e' = substTm idName newExpr e
  -- Create a new body that abstracts over the free variables
      newBody = mkTyLams (mkLams e' boundFVs) boundFTVs
  -- Add the created function to the list of global bindings
  bindings %= HMS.insert newBodyId (newBodyTy,newBody)
  -- Return the new binder
  return (Id idName (embed ty), embed newExpr)

liftBinding _ _ _ = error $ $(curLoc) ++ "liftBinding: invalid core, expr bound to tyvar"

-- | Make a global function for a name-term tuple
mkFunction :: (Functor m, Monad m)
           => TmName -- ^ Name of the function
           -> Term -- ^ Term bound to the function
           -> RewriteMonad m (TmName,Type) -- ^ Name with a proper unique and the type of the function
mkFunction bndr body = do
  tcm    <- Lens.use tcCache
  bodyTy <- termType tcm body
  bodyId <- cloneVar bndr
  addGlobalBind bodyId bodyTy body
  return (bodyId,bodyTy)

-- | Add a function to the set of global binders
addGlobalBind :: (Functor m, Monad m)
              => TmName
              -> Type
              -> Term
              -> RewriteMonad m ()
addGlobalBind vId ty body = (ty,body) `deepseq` bindings %= HMS.insert vId (ty,body)

-- | Create a new name out of the given name, but with another unique
cloneVar :: (Functor m, Monad m)
         => TmName
         -> RewriteMonad m TmName
cloneVar name = fmap (makeName (name2String name) . toInteger) getUniqueM


-- | Test whether a term is a variable reference to a local binder
isLocalVar :: (Functor m, Monad m)
           => Term
           -> RewriteMonad m Bool
isLocalVar (Var _ name)
  = fmap (not . HML.member name)
  $ Lens.use bindings
isLocalVar _ = return False

-- | Determine if a term cannot be represented in hardware
isUntranslatable :: (Functor m, Monad m)
                 => Term
                 -> RewriteMonad m Bool
isUntranslatable tm = do
  tcm <- Lens.use tcCache
  not <$> (representableType <$> Lens.use typeTranslator <*> pure tcm <*> termType tcm tm)

-- | Is the Context a Lambda/Term-abstraction context?
isLambdaBodyCtx :: CoreContext
                -> Bool
isLambdaBodyCtx (LamBody _) = True
isLambdaBodyCtx _           = False

-- | Make a binder that should not be referenced
mkWildValBinder :: (Functor m, Monad m, MonadUnique m)
                => Type
                -> m Id
mkWildValBinder = fmap fst . mkInternalVar "wild"

-- | Make a case-decomposition that extracts a field out of a (Sum-of-)Product type
mkSelectorCase :: (Functor m, Monad m, MonadUnique m, Fresh m)
               => String -- ^ Name of the caller of this function
               -> HashMap TyConName TyCon -- ^ TyCon cache
               -> [CoreContext] -- ^ Transformation Context in which this function is called
               -> Term -- ^ Subject of the case-composition
               -> Int -- n'th DataCon
               -> Int -- n'th field
               -> m Term
mkSelectorCase caller tcm _ scrut dcI fieldI = do
  scrutTy <- termType tcm scrut
  let cantCreate loc info = error $ loc ++ "Can't create selector " ++ show (caller,dcI,fieldI) ++ " for: (" ++ showDoc scrut ++ " :: " ++ showDoc scrutTy ++ ")\nAdditional info: " ++ info
  case coreView tcm scrutTy of
    TyConApp tc args ->
      case tyConDataCons (tcm HMS.! tc) of
        [] -> cantCreate $(curLoc) ("TyCon has no DataCons: " ++ show tc ++ " " ++ showDoc tc)
        dcs | dcI > length dcs -> cantCreate $(curLoc) "DC index exceeds max"
            | otherwise -> do
          let dc = indexNote ($(curLoc) ++ "No DC with tag: " ++ show (dcI-1)) dcs (dcI-1)
          let fieldTys = dataConInstArgTys dc args
          if fieldI >= length fieldTys
            then cantCreate $(curLoc) "Field index exceed max"
            else do
              wildBndrs <- mapM mkWildValBinder fieldTys
              let ty = indexNote ($(curLoc) ++ "No DC field#: " ++ show fieldI) fieldTys fieldI
              selBndr <- mkInternalVar "sel" ty
              let bndrs  = take fieldI wildBndrs ++ [fst selBndr] ++ drop (fieldI+1) wildBndrs
                  pat    = DataPat (embed dc) (rebind [] bndrs)
                  retVal = Case scrut ty [ bind pat (snd selBndr) ]
              return retVal
    _ -> cantCreate $(curLoc) ("Type of subject is not a datatype: " ++ showDoc scrutTy)

-- | Specialise an application on its argument
specialise :: (Functor m, State.MonadState s m)
           => Lens' s (Map.Map (TmName, Int, Either Term Type) (TmName,Type)) -- ^ Lens into previous specialisations
           -> Lens' s (HashMap TmName Int) -- ^ Lens into the specialisation history
           -> Lens' s Int -- ^ Lens into the specialisation limit
           -> Bool
           -> Rewrite m
specialise specMapLbl specHistLbl specLimitLbl doCheck ctx e = case e of
  (TyApp e1 ty) -> specialise' specMapLbl specHistLbl specLimitLbl False ctx e (collectArgs e1) (Right ty)
  (App e1 e2)   -> specialise' specMapLbl specHistLbl specLimitLbl doCheck ctx e (collectArgs e1) (Left  e2)
  _             -> return e

-- | Specialise an application on its argument
specialise' :: (Functor m, State.MonadState s m)
            => Lens' s (Map.Map (TmName, Int, Either Term Type) (TmName,Type)) -- ^ Lens into previous specialisations
            -> Lens' s (HashMap TmName Int) -- ^ Lens into specialisation history
            -> Lens' s Int -- ^ Lens into the specialisation limit
            -> Bool -- ^ Perform specialisation limit check
            -> [CoreContext] -- Transformation context
            -> Term -- ^ Original term
            -> (Term, [Either Term Type]) -- ^ Function part of the term, split into root and applied arguments
            -> Either Term Type -- ^ Argument to specialize on
            -> R m Term
specialise' specMapLbl specHistLbl specLimitLbl doCheck ctx e (Var _ f, args) specArg = R $ do
  lvl <- Lens.view dbgLevel
  -- Create binders and variable references for free variables in 'specArg'
  (specBndrs,specVars) <- specArgBndrsAndVars ctx specArg
  let argLen  = length args
      specAbs = either (Left . (`mkAbstraction` specBndrs)) (Right . id) specArg
  -- Determine if 'f' has already been specialized on 'specArg'
  specM <- liftR $ fmap (Map.lookup (f,argLen,specAbs))
                 $ Lens.use specMapLbl
  case specM of
    -- Use previously specialized function
    Just (fname,fty) ->
      traceIf (lvl >= DebugApplied) ("Using previous specialization of " ++ showDoc f ++ " on " ++ (either showDoc showDoc) specAbs ++ ": " ++ showDoc fname) $
        changed $ mkApps (Var fty fname) (args ++ specVars)
    -- Create new specialized function
    Nothing -> do
      -- Determine if we can specialize f
      bodyMaybe <- fmap (HML.lookup f) $ Lens.use bindings
      case bodyMaybe of
        Just (_,bodyTm) -> do
          -- Determine if we see a sequence of specialisations on a growing argument
          specHistM <- liftR $ fmap (HML.lookup f) (Lens.use specHistLbl)
          specLim   <- liftR $ Lens.use specLimitLbl
          if doCheck && maybe False (> specLim) specHistM
            then fail $ unlines [ "Hit specialisation limit on function `" ++ showDoc f ++ "'.\n"
                                , "The function `" ++ showDoc f ++ "' is most likely recursive, and looks like it is being indefinitely specialized on a growing argument.\n"
                                , "Body of `" ++ showDoc f ++ "':\n" ++ showDoc bodyTm ++ "\n"
                                , "Argument (in position: " ++ show argLen ++ ") that triggered termination:\n" ++ (either showDoc showDoc) specArg
                                ]
            else do
              -- Make new binders for existing arguments
              tcm                 <- Lens.use tcCache
              (boundArgs,argVars) <- fmap (unzip . map (either (Left *** Left) (Right *** Right))) $
                                     mapM (mkBinderFor tcm "pTS") args
              -- Create specialized functions
              let newBody = mkAbstraction (mkApps bodyTm (argVars ++ [specArg])) (boundArgs ++ specBndrs)
              newf <- mkFunction f newBody
              -- Remember specialization
              liftR $ specHistLbl %= HML.insertWith (+) f 1
              liftR $ specMapLbl %= Map.insert (f,argLen,specAbs) newf
              -- use specialized function
              let newExpr = mkApps ((uncurry . flip) Var newf) (args ++ specVars)
              newf `deepseq` changed newExpr
        Nothing -> return e

specialise' _ _ _ _ ctx _ (appE,args) (Left specArg) = R $ do
  -- Create binders and variable references for free variables in 'specArg'
  (specBndrs,specVars) <- specArgBndrsAndVars ctx (Left specArg)
  -- Create specialized function
  let newBody = mkAbstraction specArg specBndrs
  newf <- mkFunction (string2Name "specF") newBody
  -- Create specialized argument
  let newArg  = Left $ mkApps ((uncurry . flip) Var newf) specVars
  -- Use specialized argument
  let newExpr = mkApps appE (args ++ [newArg])
  changed newExpr

specialise' _ _ _ _ _ e _ _ = return e

-- | Create binders and variable references for free variables in 'specArg'
specArgBndrsAndVars :: (Functor m, Monad m)
                    => [CoreContext]
                    -> Either Term Type
                    -> RewriteMonad m ([Either Id TyVar],[Either Term Type])
specArgBndrsAndVars ctx specArg = do
  let specFTVs = List.nub $ either (Lens.toListOf termFreeTyVars) (Lens.toListOf typeFreeVars) specArg
  specFVs <- List.nub <$> either ((Lens.toListOf <$> localFreeIds <*>) . pure) (const (pure [])) specArg
  (gamma,delta) <- mkEnv ctx
  let (specTyBndrs,specTyVars) = unzip
                 $ map (\tv -> let ki = HML.lookupDefault (error $ $(curLoc) ++ show tv ++ " not found") tv delta
                               in  (Right $ TyVar tv (embed ki), Right $ VarTy ki tv)) specFTVs
      (specTmBndrs,specTmVars) = unzip
                 $ map (\tm -> let ty = HML.lookupDefault (error $ $(curLoc) ++ show tm ++ " not found") tm gamma
                               in  (Left $ Id tm (embed ty), Left $ Var ty tm)) specFVs
  return (specTyBndrs ++ specTmBndrs,specTyVars ++ specTmVars)
