{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016     , Myrtle Software Ltd,
                    2017     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utilities for rewriting: e.g. inlining, specialisation, etc.
-}

{-# LANGUAGE CPP                      #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE Rank2Types               #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE ViewPatterns             #-}

module Clash.Rewrite.Util where

import           Control.DeepSeq
import           Control.Exception           (throw)
import           Control.Lens
  (Lens', (%=), (+=), (^.), _2, _4, _5, _Left)
import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
import qualified Control.Monad.State.Strict  as State
import qualified Control.Monad.Writer        as Writer
import           Data.Bifunctor              (bimap)
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Lazy           as HML
import qualified Data.HashMap.Strict         as HMS
import qualified Data.HashSet                as HashSet
import qualified Data.List                   as List
import qualified Data.Map                    as Map
import           Data.Maybe                  (catMaybes,isJust,mapMaybe)
import qualified Data.Monoid                 as Monoid
import qualified Data.Set                    as Set
import qualified Data.Set.Lens               as Lens
import           Unbound.Generics.LocallyNameless
  (Fresh, bind, embed, rebind, rec, unbind, unembed, unrec)
import qualified Unbound.Generics.LocallyNameless as Unbound
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           BasicTypes                  (InlineSpec (..))
import           SrcLoc                      (SrcSpan)

import           Clash.Core.DataCon          (dataConInstArgTys)
import           Clash.Core.FreeVars         (termFreeIds, termFreeTyVars,
                                              typeFreeVars)
import           Clash.Core.Name
import           Clash.Core.Pretty           (showDoc)
import           Clash.Core.Subst            (substTm)
import           Clash.Core.Term
  (LetBinding, Pat (..), Term (..), TmName, TmOccName)
import           Clash.Core.TyCon
  (TyCon, TyConOccName, tyConDataCons)
import           Clash.Core.Type             (KindOrType, Type (..),
                                              TypeView (..), coreView,
                                              normalizeType,
                                              typeKind, tyView)
import           Clash.Core.Util
  (Delta, Gamma, collectArgs, isPolyFun, mkAbstraction, mkApps, mkId, mkLams,
   mkTmApps, mkTyApps, mkTyLams, mkTyVar, termType)
import           Clash.Core.Var              (Id, TyVar, Var (..))
import           Clash.Driver.Types
  (ClashException (..), DebugLevel (..))
import           Clash.Netlist.Util          (representableType)
import           Clash.Rewrite.Types
import           Clash.Util

-- | Lift an action working in the '_extra' state to the 'RewriteMonad'
zoomExtra :: State.State extra a
          -> RewriteMonad extra a
zoomExtra m = R (\_ s -> case State.runState m (s ^. extra) of
                           (a,s') -> (a,s {_extra = s'},mempty))

-- | Record if a transformation is succesfully applied
apply :: String -- ^ Name of the transformation
      -> Rewrite extra -- ^ Transformation to be applied
      -> Rewrite extra
apply name rewrite ctx expr = do
  lvl <- Lens.view dbgLevel
  let before = showDoc expr
  (expr', anyChanged) <- traceIf (lvl >= DebugAll) ("Trying: " ++ name ++ " on:\n" ++ before) $ Writer.listen $ rewrite ctx expr
  let hasChanged = Monoid.getAny anyChanged
  Monad.when hasChanged $ transformCounter += 1
  let after  = showDoc expr'
  let expr'' = if hasChanged then expr' else expr

  Monad.when (lvl > DebugNone && hasChanged) $ do
    tcm                  <- Lens.view tcCache
    beforeTy             <- termType tcm expr
    let beforeFTV        = Lens.setOf termFreeTyVars expr
    beforeFV             <- Lens.setOf <$> localFreeIds <*> pure expr
    afterTy              <- termType tcm expr'
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
    traceIf (lvl >= DebugAll && beforeTy /= afterTy)
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
runRewrite :: String -- ^ Name of the transformation
           -> Rewrite extra -- ^ Transformation to perform
           -> Term  -- ^ Term to transform
           -> RewriteMonad extra Term
runRewrite name rewrite expr = apply name rewrite [] expr

-- | Evaluate a RewriteSession to its inner monad
runRewriteSession :: RewriteEnv
                  -> RewriteState extra
                  -> RewriteMonad extra a
                  -> a
runRewriteSession r s m = traceIf True ("Applied " ++
                                        show (s' ^. transformCounter) ++
                                        " transformations")
                                  a
  where
    (a,s',_) = runR m r s

-- | Notify that a transformation has changed the expression
setChanged :: RewriteMonad extra ()
setChanged = Writer.tell (Monoid.Any True)

-- | Identity function that additionally notifies that a transformation has
-- changed the expression
changed :: a -> RewriteMonad extra a
changed val = do
  Writer.tell (Monoid.Any True)
  return val

-- | Create a type and kind context out of a transformation context
contextEnv :: [CoreContext]
           -> (Gamma, Delta)
contextEnv = go HML.empty HML.empty
  where
    go gamma delta []                   = (gamma,delta)
    go gamma delta (LetBinding _ ids:ctx) = go gamma' delta ctx
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

    go gamma delta (CaseAlt tvs ids:ctx) = go gamma' delta' ctx
      where
        gamma' = foldl addToGamma gamma ids
        delta' = foldl addToDelta delta tvs

    go gamma delta (_:ctx) = go gamma delta ctx

    addToGamma gamma (Id idName ty) = HML.insert (nameOcc idName) (unembed ty) gamma
    addToGamma _     _              = error $ $(curLoc) ++ "Adding TyVar to Gamma"

    addToDelta delta (TyVar tvName ki) = HML.insert (nameOcc tvName) (unembed ki) delta
    addToDelta _     _                 = error $ $(curLoc) ++ "Adding Id to Delta"

closestLetBinder :: [CoreContext] -> Maybe Id
closestLetBinder [] = Nothing
closestLetBinder (LetBinding id_ _:_) = Just id_
closestLetBinder (_:ctx)              = closestLetBinder ctx

mkDerivedName :: [CoreContext] -> String -> TmName
mkDerivedName ctx sf = case closestLetBinder ctx of
  Just id_ -> appendToName (varName id_) ('_':sf)
  _ -> string2InternalName sf

-- | Create a complete type and kind context out of the global binders and the
-- transformation context
mkEnv :: [CoreContext]
      -> RewriteMonad extra (Gamma, Delta)
mkEnv ctx = do
  let (gamma,delta) = contextEnv ctx
  tsMap             <- fmap (HML.map (^. _2)) $ Lens.use bindings
  let gamma'        = tsMap `HML.union` gamma
  return (gamma',delta)

-- | Make a new binder and variable reference for a term
mkTmBinderFor :: (Fresh m, MonadUnique m)
              => HashMap TyConOccName TyCon -- ^ TyCon cache
              -> Name a -- ^ Name of the new binder
              -> Term -- ^ Term to bind
              -> m (Id, Term)
mkTmBinderFor tcm name e = do
  (Left r) <- mkBinderFor tcm name (Left e)
  return r

-- | Make a new binder and variable reference for either a term or a type
mkBinderFor :: (Monad m, MonadUnique m, Fresh m)
            => HashMap TyConOccName TyCon -- ^ TyCon cache
            -> Name a -- ^ Name of the new binder
            -> Either Term Type -- ^ Type or Term to bind
            -> m (Either (Id,Term) (TyVar,Type))
mkBinderFor tcm name (Left term) =
  Left <$> (mkInternalVar (coerceName name) =<< termType tcm term)

mkBinderFor tcm name (Right ty) = do
  name' <- cloneVar (coerceName name)
  let kind  = typeKind tcm ty
  return $ Right (TyVar name' (embed kind), VarTy kind name')

-- | Make a new, unique, identifier and corresponding variable reference
mkInternalVar :: (Monad m, MonadUnique m)
              => TmName -- ^ Name of the identifier
              -> KindOrType
              -> m (Id,Term)
mkInternalVar name ty = do
  name' <- cloneVar name
  return (Id name' (embed ty),Var ty name')

-- | Inline the binders in a let-binding that have a certain property
inlineBinders :: (Term -> LetBinding -> RewriteMonad extra Bool) -- ^ Property test
              -> Rewrite extra
inlineBinders condition _ expr@(Letrec b) = do
  (xes,res)        <- unbind b
  let expr' = Letrec (bind xes res)
  (replace,others) <- partitionM (condition expr') (unrec xes)
  case replace of
    [] -> return expr
    _  -> do
      let (others',res') = substituteBinders replace others res
          newExpr = case others' of
                          [] -> res'
                          _  -> Letrec (bind (rec others') res')

      changed newExpr

inlineBinders _ _ e = return e

-- | Determine whether a binder is a join-point created for a complex case
-- expression.
--
-- A join-point is when a local function only occurs in tail-call positions,
-- and when it does, more than once.
isJoinPointIn :: Id   -- ^ 'Id' of the local binder
              -> Term -- ^ Expression in which the binder is bound
              -> Bool
isJoinPointIn id_ e = case tailCalls id_ e of
                      Just n | n > 1 -> True
                      _              -> False

-- | Count the number of (only) tail calls of a function in an expression.
-- 'Nothing' indicates that the function was used in a non-tail call position.
tailCalls :: Id   -- ^ Function to check
          -> Term -- ^ Expression to check it in
          -> Maybe Int
tailCalls id_ expr = case expr of
  Var _ nm | varName id_ == nm -> Just 1
           | otherwise       -> Just 0
  Lam b -> let (_,expr') = unsafeUnbind b
           in  tailCalls id_ expr'
  TyLam b -> let (_,expr') = unsafeUnbind b
             in  tailCalls id_ expr'
  App l r  -> case tailCalls id_ r of
                Just 0 -> tailCalls id_ l
                _      -> Nothing
  TyApp l _ -> tailCalls id_ l
  Letrec b ->
    let (bsR,expr')     = unsafeUnbind b
        (bsIds,bsExprs) = unzip (unrec bsR)
        bsTls           = map (tailCalls id_ . unembed) bsExprs
        bsIdsUsed       = mapMaybe (\(l,r) -> pure l <* r) (zip bsIds bsTls)
        bsIdsTls        = map (`tailCalls` expr') bsIdsUsed
        bsCount         = pure . sum $ catMaybes bsTls
    in  case (all isJust bsTls) of
          False -> Nothing
          True  -> case (all (==0) $ catMaybes bsTls) of
            False  -> case all isJust bsIdsTls of
              False -> Nothing
              True  -> (+) <$> bsCount <*> tailCalls id_ expr'
            True -> tailCalls id_ expr'
  Case scrut _ alts ->
    let scrutTl = tailCalls id_ scrut
        altsTl  = map (tailCalls id_ . snd . unsafeUnbind) alts
    in  case scrutTl of
          Just 0 | all (/= Nothing) altsTl -> Just (sum (catMaybes altsTl))
          _ -> Nothing
  _ -> Just 0

-- | Determines whether a function has the following shape:
--
-- > \(w :: Void) -> f a b c
--
-- i.e. is a wrapper around a (partially) applied function 'f', where the
-- introduced argument 'w' is not used by 'f'
isVoidWrapper :: Term -> Bool
isVoidWrapper (Lam b) = case unsafeUnbind b of
  (bndr,e@(collectArgs -> (Var _ _,_)))
    -> nameOcc (varName bndr) `notElem` Lens.toListOf termFreeIds e
  _ -> False
isVoidWrapper _ = False

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
    bndrName = nameOcc (varName bndr)
    selfRef  = bndrName `elem` Lens.toListOf termFreeIds val
    (res',rest',others') = if selfRef
      then (res,rest,(bndr,valE):others)
      else ( substTm bndrName val res
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
localFreeIds :: (Applicative f, Lens.Contravariant f)
             => RewriteMonad extra ((TmOccName -> f TmOccName) -> Term -> f Term)
localFreeIds = do
  globalBndrs <- Lens.use bindings
  return ((termFreeIds . Lens.filtered (not . (`HML.member` globalBndrs))))

inlineOrLiftBinders :: (LetBinding -> RewriteMonad extra Bool) -- ^ Property test
                    -> (Term -> LetBinding -> RewriteMonad extra Bool)
                       -- ^ Test whether to lift or inline
                       --
                       -- * True: inline
                       -- * False: lift
                    -> Rewrite extra
inlineOrLiftBinders condition inlineOrLift ctx expr@(Letrec b) = do
  (xesR,res) <- unbind b
  let xes = unrec xesR
  (replace,others) <- partitionM condition xes
  case replace of
    [] -> return expr
    _  -> do
      -- Because 'unbind b' refreshes binders in xes, we must recreate
      -- the let expression, and _not_ reuse 'expr'
      let expr' = Letrec (bind xesR res)
      (doInline,doLift) <- partitionM (inlineOrLift expr') replace
      -- We first substitute the binders that we can inline both the binders
      -- that we intend to lift, the other binders, and the body
      let (others',res')     = substituteBinders doInline (doLift ++ others) res
          (doLift',others'') = splitAt (length doLift) others'
      (gamma,delta) <- mkEnv (LetBinding undefined (map fst xes) : ctx)
      doLift'' <- mapM (liftBinding gamma delta) doLift'
      -- We then substitute the lifted binders in the other binders and the body
      let (others3,res'') = substituteBinders doLift'' others'' res'
          newExpr = case others3 of
                      [] -> res''
                      _  -> Letrec (bind (rec others3) res'')
      changed newExpr

inlineOrLiftBinders _ _ _ e = return e

-- | Create a global function for a Let-binding and return a Let-binding where
-- the RHS is a reference to the new global function applied to the free
-- variables of the original RHS
liftBinding :: Gamma
            -> Delta
            -> LetBinding
            -> RewriteMonad extra LetBinding
liftBinding gamma delta (Id idName tyE,eE) = do
  let e  = unembed eE
  -- Get all local FVs, excluding the 'idName' from the let-binding
  let localFTVs = map (\nm -> Name Internal nm noSrcSpan)
                . List.nub $ Lens.toListOf termFreeTyVars e
  localFVs <- map (\nm -> Name Internal nm noSrcSpan) . List.nub <$>
                  (Lens.toListOf <$> localFreeIds <*> pure e)
  let localFTVkinds = map (\k -> HML.lookupDefault (error $ $(curLoc) ++ show k ++ " not found") (nameOcc k) delta) localFTVs
      localFVs'     = filter ((/= (nameOcc idName)) . nameOcc) localFVs
      localFVtys'   = map (\k -> HML.lookupDefault (error $ $(curLoc) ++ show k ++ " not found") (nameOcc k) gamma) localFVs'
  -- Abstract expression over its local FVs
      boundFTVs = zipWith mkTyVar localFTVkinds localFTVs
      boundFVs  = zipWith mkId localFVtys' localFVs'
  -- Make a new global ID
  tcm       <- Lens.view tcCache
  newBodyTy <- termType tcm $ mkTyLams (mkLams e boundFVs) boundFTVs
  (cf,sp)   <- Lens.use curFun
  -- newBodyId <- fmap (makeName (name2String cf ++ "_" ++ name2String idName) . toInteger) getUniqueM
  newBodyId <- cloneVar (appendToName cf ("_" ++ name2String idName))

  -- Make a new expression, consisting of the the lifted function applied to
  -- its free variables
  let newExpr = mkTmApps
                  (mkTyApps (Var newBodyTy newBodyId)
                            (zipWith VarTy localFTVkinds localFTVs))
                  (zipWith Var localFVtys' localFVs')
  -- Substitute the recursive calls by the new expression
      e' = substTm (nameOcc idName) newExpr e
  -- Create a new body that abstracts over the free variables
      newBody = mkTyLams (mkLams e' boundFVs) boundFTVs

  -- Check if an alpha-equivalent global binder already exists
  aeqExisting <- (HMS.toList . HMS.filter ((== newBody) . (^. _5))) <$> Lens.use bindings
  case aeqExisting of
    -- If it doesn't, create a new binder
    [] -> do -- Add the created function to the list of global bindings
             bindings %= HMS.insert (nameOcc newBodyId)
                                    -- We mark this function as internal so that
                                    -- it can be inlined at the very end of
                                    -- the normalisation pipeline as part of the
                                    -- flattening pass. We don't inline
                                    -- right away because we are lifting this
                                    -- function at this moment for a reason!
                                    -- (termination, CSE and DEC oppertunities,
                                    -- ,etc.)
                                    (newBodyId {nameSort = Internal}
                                    ,newBodyTy,sp
#if MIN_VERSION_ghc(8,4,1)
                                    ,NoUserInline
#else
                                    ,EmptyInlineSpec
#endif
                                    ,newBody)
             -- Return the new binder
             return (Id idName tyE, embed newExpr)
    -- If it does, use the existing binder
    ((_,(k,aeqTy,_,_,_)):_) ->
      let newExpr' = mkTmApps
                      (mkTyApps (Var aeqTy k)
                                (zipWith VarTy localFTVkinds localFTVs))
                      (zipWith Var localFVtys' localFVs')
      in  return (Id idName tyE, embed newExpr')

liftBinding _ _ _ = error $ $(curLoc) ++ "liftBinding: invalid core, expr bound to tyvar"

-- | Make a global function for a name-term tuple
mkFunction
  :: TmName
  -- ^ Name of the function
  -> SrcSpan
  -> InlineSpec
  -> Term
  -- ^ Term bound to the function
  -> RewriteMonad extra (TmName,Type)
  -- ^ Name with a proper unique and the type of the function
mkFunction bndr sp inl body = do
  tcm    <- Lens.view tcCache
  bodyTy <- termType tcm body
  bodyId <- cloneVar bndr
  addGlobalBind bodyId bodyTy sp inl body
  return (bodyId,bodyTy)

-- | Add a function to the set of global binders
addGlobalBind
  :: TmName
  -> Type
  -> SrcSpan
  -> InlineSpec
  -> Term
  -> RewriteMonad extra ()
addGlobalBind vId ty sp inl body =
  (ty,body) `deepseq` bindings %= HMS.insert (nameOcc vId) (vId,ty,sp,inl,body)

-- | Create a new name out of the given name, but with another unique
cloneVar
  :: (Monad m, MonadUnique m)
  => Name a
  -> m (Name a)
cloneVar (Name sort nm loc) = do
  i <- toInteger <$> getUniqueM
  return (Name sort (Unbound.makeName (Unbound.name2String nm) i) loc)

-- | Test whether a term is a variable reference to a local binder
isLocalVar :: Term
           -> RewriteMonad extra Bool
isLocalVar (Var _ name)
  = fmap (not . HML.member (nameOcc name))
  $ Lens.use bindings
isLocalVar _ = return False

{-# INLINE isUntranslatable #-}
-- | Determine if a term cannot be represented in hardware
isUntranslatable
  :: Bool
  -- ^ String representable
  -> Term
  -> RewriteMonad extra Bool
isUntranslatable stringRepresentable tm = do
  tcm <- Lens.view tcCache
  not <$> (representableType <$> Lens.view typeTranslator
                             <*> Lens.view customReprs
                             <*> pure stringRepresentable
                             <*> pure tcm
                             <*> termType tcm tm)

{-# INLINE isUntranslatableType #-}
-- | Determine if a type cannot be represented in hardware
isUntranslatableType
  :: Bool
  -- ^ String representable
  -> Type
  -> RewriteMonad extra Bool
isUntranslatableType stringRepresentable ty =
  not <$> (representableType <$> Lens.view typeTranslator
                             <*> Lens.view customReprs
                             <*> pure stringRepresentable
                             <*> Lens.view tcCache
                             <*> pure ty)

-- | Is the Context a Lambda/Term-abstraction context?
isLambdaBodyCtx :: CoreContext
                -> Bool
isLambdaBodyCtx (LamBody _) = True
isLambdaBodyCtx _           = False

-- | Make a binder that should not be referenced
mkWildValBinder :: (Monad m, MonadUnique m)
                => Type
                -> m Id
mkWildValBinder = fmap fst . mkInternalVar (string2InternalName "wild")

-- | Make a case-decomposition that extracts a field out of a (Sum-of-)Product type
mkSelectorCase :: (Functor m, Monad m, MonadUnique m, Fresh m)
               => String -- ^ Name of the caller of this function
               -> HashMap TyConOccName TyCon -- ^ TyCon cache
               -> Term -- ^ Subject of the case-composition
               -> Int -- n'th DataCon
               -> Int -- n'th field
               -> m Term
mkSelectorCase caller tcm scrut dcI fieldI = do
    scrutTy <- termType tcm scrut
    go scrutTy
  where
    go (coreView tcm -> Just ty')   = go ty'
    go scrutTy@(tyView -> TyConApp tc args) =
      case tyConDataCons (tcm HMS.! nameOcc tc) of
        [] -> cantCreate $(curLoc) ("TyCon has no DataCons: " ++ show tc ++ " " ++ showDoc tc) scrutTy
        dcs | dcI > length dcs -> cantCreate $(curLoc) "DC index exceeds max" scrutTy
            | otherwise -> do
          let dc = indexNote ($(curLoc) ++ "No DC with tag: " ++ show (dcI-1)) dcs (dcI-1)
          let (Just fieldTys) = dataConInstArgTys dc args
          if fieldI >= length fieldTys
            then cantCreate $(curLoc) "Field index exceed max" scrutTy
            else do
              wildBndrs <- mapM mkWildValBinder fieldTys
              let ty = indexNote ($(curLoc) ++ "No DC field#: " ++ show fieldI) fieldTys fieldI
              selBndr <- mkInternalVar (string2InternalName "sel") ty
              let bndrs  = take fieldI wildBndrs ++ [fst selBndr] ++ drop (fieldI+1) wildBndrs
                  pat    = DataPat (embed dc) (rebind [] bndrs)
                  retVal = Case scrut ty [ bind pat (snd selBndr) ]
              return retVal
    go scrutTy = cantCreate $(curLoc) ("Type of subject is not a datatype: " ++ showDoc scrutTy) scrutTy

    cantCreate loc info scrutTy = error $ loc ++ "Can't create selector " ++ show (caller,dcI,fieldI) ++ " for: (" ++ showDoc scrut ++ " :: " ++ showDoc scrutTy ++ ")\nAdditional info: " ++ info

-- | Specialise an application on its argument
specialise :: Lens' extra (Map.Map (TmOccName, Int, Either Term Type) (TmName,Type)) -- ^ Lens into previous specialisations
           -> Lens' extra (HashMap TmOccName Int) -- ^ Lens into the specialisation history
           -> Lens' extra Int -- ^ Lens into the specialisation limit
           -> Rewrite extra
specialise specMapLbl specHistLbl specLimitLbl ctx e = case e of
  (TyApp e1 ty) -> specialise' specMapLbl specHistLbl specLimitLbl ctx e (collectArgs e1) (Right ty)
  (App e1 e2)   -> specialise' specMapLbl specHistLbl specLimitLbl ctx e (collectArgs e1) (Left  e2)
  _             -> return e

-- | Specialise an application on its argument
specialise' :: Lens' extra (Map.Map (TmOccName, Int, Either Term Type) (TmName,Type)) -- ^ Lens into previous specialisations
            -> Lens' extra (HashMap TmOccName Int) -- ^ Lens into specialisation history
            -> Lens' extra Int -- ^ Lens into the specialisation limit
            -> [CoreContext] -- Transformation context
            -> Term -- ^ Original term
            -> (Term, [Either Term Type]) -- ^ Function part of the term, split into root and applied arguments
            -> Either Term Type -- ^ Argument to specialize on
            -> RewriteMonad extra Term
specialise' specMapLbl specHistLbl specLimitLbl ctx e (Var _ f, args) specArgIn = do
  lvl <- Lens.view dbgLevel

  -- Don't specialise TopEntities
  topEnts <- Lens.view topEntities
  if nameOcc f `HashSet.member` topEnts
  then traceIf (lvl >= DebugNone) ("Not specialising TopEntity: " ++ showDoc f) (return e)
  else do -- NondecreasingIndentation

  tcm <- Lens.view tcCache

  let specArg = bimap (normalizeTermTypes tcm) (normalizeType tcm) specArgIn
  -- Create binders and variable references for free variables in 'specArg'
  -- (specBndrsIn,specVars) :: ([Either Id TyVar], [Either Term Type])
  (specBndrsIn,specVars) <- specArgBndrsAndVars ctx specArg
  let argLen  = length args
      specBndrs :: [Either Id TyVar]
      specBndrs = map (Lens.over _Left (normalizeId tcm)) specBndrsIn
      specAbs :: Either Term Type
      specAbs = either (Left . (`mkAbstraction` specBndrs)) (Right . id) specArg
  -- Determine if 'f' has already been specialized on (a type-normalized) 'specArg'
  specM <- Map.lookup (nameOcc f,argLen,specAbs) <$> Lens.use (extra.specMapLbl)
  case specM of
    -- Use previously specialized function
    Just (fname,fty) ->
      traceIf (lvl >= DebugApplied) ("Using previous specialization of " ++ showDoc (nameOcc f) ++ " on " ++ (either showDoc showDoc) specAbs ++ ": " ++ showDoc fname) $
        changed $ mkApps (Var fty fname) (args ++ specVars)
    -- Create new specialized function
    Nothing -> do
      -- Determine if we can specialize f
      bodyMaybe <- fmap (HML.lookup (nameOcc f)) $ Lens.use bindings
      case bodyMaybe of
        Just (_,_,sp,inl,bodyTm) -> do
          -- Determine if we see a sequence of specialisations on a growing argument
          specHistM <- HML.lookup (nameOcc f) <$> Lens.use (extra.specHistLbl)
          specLim   <- Lens.use (extra . specLimitLbl)
          if maybe False (> specLim) specHistM
            then throw (ClashException
                        sp
                        (unlines [ "Hit specialisation limit " ++ show specLim ++ " on function `" ++ showDoc (nameOcc f) ++ "'.\n"
                                 , "The function `" ++ showDoc f ++ "' is most likely recursive, and looks like it is being indefinitely specialized on a growing argument.\n"
                                 , "Body of `" ++ showDoc f ++ "':\n" ++ showDoc bodyTm ++ "\n"
                                 , "Argument (in position: " ++ show argLen ++ ") that triggered termination:\n" ++ (either showDoc showDoc) specArg
                                 , "Run with '-fclash-spec-limit=N' to increase the specialisation limit to N."
                                 ])
                        Nothing)
            else do
              -- Make new binders for existing arguments
              (boundArgs,argVars) <- fmap (unzip . map (either (Left *** Left) (Right *** Right))) $
                                     Monad.zipWithM
                                       (mkBinderFor tcm)
                                       (unsafeCollectBndrs bodyTm ++ repeat (string2InternalName "pTS"))
                                       args
              -- Determine name the resulting specialised function, and the
              -- form of the specialised-on argument
              (fName,inl',specArg') <- case specArg of
                Left a@(collectArgs -> (Var _ g,gArgs)) -> do
                  polyFun <- isPolyFun tcm a
                  if polyFun
                    then do
                      -- In case we are specialising on an argument that is a
                      -- global function then we use that function's name as the
                      -- name of the specialised higher-order function.
                      -- Additionally, we will return the body of the global
                      -- function, instead of a variable reference to the
                      -- global function.
                      --
                      -- This will turn things like @mealy g k@ into a new
                      -- binding @g'@ where both the body of @mealy@ and @g@
                      -- are inlined, meaning the state-transition-function
                      -- and the memory element will be in a single function.
                      gTmM <- fmap (HML.lookup (nameOcc g)) $ Lens.use bindings
                      return (g,maybe inl (^. _4) gTmM, maybe specArg (Left . (`mkApps` gArgs) . (^. _5)) gTmM)
                    else return (f,inl,specArg)
                _ -> return (f,inl,specArg)
              -- Create specialized functions
              let newBody = mkAbstraction (mkApps bodyTm (argVars ++ [specArg'])) (boundArgs ++ specBndrs)
              newf <- mkFunction fName sp inl' newBody
              -- Remember specialization
              (extra.specHistLbl) %= HML.insertWith (+) (nameOcc f) 1
              (extra.specMapLbl)  %= Map.insert (nameOcc f,argLen,specAbs) newf
              -- use specialized function
              let newExpr = mkApps ((uncurry . flip) Var newf) (args ++ specVars)
              newf `deepseq` changed newExpr
        Nothing -> return e
  where
    unsafeCollectBndrs :: Term -> [Name a]
    unsafeCollectBndrs =
        map (either (coerceName . varName) (coerceName . varName)) . reverse . go []
      where
        go bs (Lam b)    = let (v,e')  = unsafeUnbind b in go (Left v:bs)   e'
        go bs (TyLam b)  = let (tv,e') = unsafeUnbind b in go (Right tv:bs) e'
        go bs (App e' _) = case go [] e' of
          []  -> bs
          bs' -> init bs' ++ bs
        go bs (TyApp e' _) = case go [] e' of
          []  -> bs
          bs' -> init bs' ++ bs
        go bs _ = bs

specialise' _ _ _ ctx _ (appE,args) (Left specArg) = do
  -- Create binders and variable references for free variables in 'specArg'
  (specBndrs,specVars) <- specArgBndrsAndVars ctx (Left specArg)
  -- Create specialized function
  let newBody = mkAbstraction specArg specBndrs
  -- See if there's an existing binder that's alpha-equivalent to the
  -- specialised function
  existing <- HML.filter ((== newBody) . (^. _5)) <$> Lens.use bindings
  -- Create a new function if an alpha-equivalent binder doesn't exist
  newf <- case HML.toList existing of
    [] -> do (cf,sp) <- Lens.use curFun
             mkFunction (appendToName cf "_specF")
                        sp
#if MIN_VERSION_ghc(8,4,1)
                        NoUserInline
#else
                        EmptyInlineSpec
#endif
                        newBody
    ((_,(k,kTy,_,_,_)):_) -> return (k,kTy)
  -- Create specialized argument
  let newArg  = Left $ mkApps ((uncurry . flip) Var newf) specVars
  -- Use specialized argument
  let newExpr = mkApps appE (args ++ [newArg])
  changed newExpr

specialise' _ _ _ _ e _ _ = return e

normalizeTermTypes :: HashMap TyConOccName TyCon -> Term -> Term
normalizeTermTypes tcm e = case e of
  Cast e' ty1 ty2 -> Cast (normalizeTermTypes tcm e') (normalizeType tcm ty1) (normalizeType tcm ty2)
  Var ty nm -> Var (normalizeType tcm ty) nm
  -- TODO other terms?
  _ -> e

normalizeId :: HashMap TyConOccName TyCon -> Id -> Id
normalizeId tcm (Id nm (Unbound.Embed ty)) = Id nm (Unbound.Embed $ normalizeType tcm ty)
normalizeId _   tyvar = tyvar


-- | Create binders and variable references for free variables in 'specArg'
specArgBndrsAndVars :: [CoreContext]
                    -> Either Term Type
                    -> RewriteMonad extra ([Either Id TyVar],[Either Term Type])
specArgBndrsAndVars ctx specArg = do
  let specFTVs = List.nub $ either (Lens.toListOf termFreeTyVars) (Lens.toListOf typeFreeVars) specArg
  specFVs <- List.nub <$> either ((Lens.toListOf <$> localFreeIds <*>) . pure) (const (pure [])) specArg
  (gamma,delta) <- mkEnv ctx
  let (specTyBndrs,specTyVars) = unzip
                 $ map (\tv -> let ki = HML.lookupDefault (error $ $(curLoc) ++ show tv ++ " not found") tv delta
                                   tv' = Name Internal tv noSrcSpan
                               in  (Right $ TyVar tv' (embed ki), Right $ VarTy ki tv')) specFTVs
      (specTmBndrs,specTmVars) = unzip
                 $ map (\tm -> let ty = HML.lookupDefault (error $ $(curLoc) ++ show tm ++ " not found") tm gamma
                                   tm' = Name Internal tm noSrcSpan
                               in  (Left $ Id tm' (embed ty), Left $ Var ty tm')) specFVs
  return (specTyBndrs ++ specTmBndrs,specTyVars ++ specTmVars)
