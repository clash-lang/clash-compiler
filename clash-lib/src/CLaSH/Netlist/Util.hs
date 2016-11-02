{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utilities for converting Core Type/Term to Netlist datatypes
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module CLaSH.Netlist.Util where

import           Control.Error           (hush)
import           Control.Lens            ((.=),(%=))
import qualified Control.Lens            as Lens
import qualified Control.Monad           as Monad
import           Control.Monad.Trans.Except (runExcept)
import           Data.Either             (partitionEithers)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Maybe              (catMaybes,fromMaybe)
import           Data.Text.Lazy          (append,pack,unpack)
import qualified Data.Text.Lazy          as Text
import           Unbound.Generics.LocallyNameless (Embed, Fresh, bind, embed, makeName,
                                          name2Integer, name2String, unbind,
                                          unembed, unrec)

import           CLaSH.Core.DataCon      (DataCon (..))
import           CLaSH.Core.FreeVars     (termFreeIds, typeFreeVars)
import           CLaSH.Core.Pretty       (showDoc)
import           CLaSH.Core.Subst        (substTys)
import           CLaSH.Core.Term         (LetBinding, Term (..), TmName)
import           CLaSH.Core.TyCon        (TyCon (..), TyConName, tyConDataCons)
import           CLaSH.Core.Type         (Type (..), TypeView (..), LitTy (..),
                                          coreView, splitTyConAppM, tyView)
import           CLaSH.Core.Util         (collectBndrs, termType, tyNatSize)
import           CLaSH.Core.Var          (Id, Var (..), modifyVarName)
import           CLaSH.Netlist.Types     as HW
import           CLaSH.Util

mkBasicId :: Identifier -> NetlistMonad Identifier
mkBasicId n = do
  f  <- Lens.use mkBasicIdFn
  let n' = f n
  if Text.null n'
     then return (pack "x")
     else return n'

-- | Split a normalized term into: a list of arguments, a list of let-bindings,
-- and a variable reference that is the body of the let-binding. Returns a
-- String containing the error is the term was not in a normalized form.
splitNormalized :: Fresh m
                => HashMap TyConName TyCon
                -> Term
                -> m (Either String ([Id],[LetBinding],Id))
splitNormalized tcm expr = do
  (args,letExpr) <- fmap (first partitionEithers) $ collectBndrs expr
  case letExpr of
    Letrec b
      | (tmArgs,[]) <- args -> do
          (xes,e) <- unbind b
          case e of
            Var t v -> return $! Right (tmArgs,unrec xes,Id v (embed t))
            _ -> return $! Left ($(curLoc) ++ "Not in normal form: res not simple var")
      | otherwise -> return $! Left ($(curLoc) ++ "Not in normal form: tyArgs")
    _ -> do
      ty <- termType tcm expr
      return $! Left ($(curLoc) ++ "Not in normal from: no Letrec:\n\n" ++ showDoc expr ++ "\n\nWhich has type:\n\n"  ++ showDoc ty)

-- | Converts a Core type to a HWType given a function that translates certain
-- builtin types. Errors if the Core type is not translatable.
unsafeCoreTypeToHWType :: String
                       -> (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType))
                       -> HashMap TyConName TyCon
                       -> Type
                       -> HWType
unsafeCoreTypeToHWType loc builtInTranslation m = either (error . (loc ++)) id . coreTypeToHWType builtInTranslation m

-- | Converts a Core type to a HWType within the NetlistMonad; errors on failure
unsafeCoreTypeToHWTypeM :: String
                        -> Type
                        -> NetlistMonad HWType
unsafeCoreTypeToHWTypeM loc ty = unsafeCoreTypeToHWType loc <$> Lens.use typeTranslator <*> Lens.use tcCache <*> pure ty

-- | Converts a Core type to a HWType within the NetlistMonad; 'Nothing' on failure
coreTypeToHWTypeM :: Type
                  -> NetlistMonad (Maybe HWType)
coreTypeToHWTypeM ty = hush <$> (coreTypeToHWType <$> Lens.use typeTranslator <*> Lens.use tcCache <*> pure ty)

-- | Returns the name and period of the clock corresponding to a type
synchronizedClk :: HashMap TyConName TyCon -- ^ TyCon cache
                -> Type
                -> Maybe (Identifier,Integer)
synchronizedClk tcm ty
  | not . null . Lens.toListOf typeFreeVars $ ty = Nothing
  | Just (tyCon,args) <- splitTyConAppM ty
  = case name2String tyCon of
      "CLaSH.Sized.Vector.Vec"        -> synchronizedClk tcm (args!!1)
      "CLaSH.Signal.Internal.SClock" -> case splitTyConAppM (head args) of
        Just (_,[LitTy (SymTy s),litTy])
          | Right i <- runExcept (tyNatSize tcm litTy) -> Just (pack s,i)
        _ -> error $ $(curLoc) ++ "Clock period not a simple literal: " ++ showDoc ty
      "CLaSH.Signal.Internal.Signal'" -> case splitTyConAppM (head args) of
        Just (_,[LitTy (SymTy s),litTy])
          | Right i <- runExcept (tyNatSize tcm litTy) -> Just (pack s,i)
        _ -> error $ $(curLoc) ++ "Clock period not a simple literal: " ++ showDoc ty
      _                               -> case tyConDataCons (tcm HashMap.! tyCon) of
                                           [dc] -> let argTys   = dcArgTys dc
                                                       argTVs   = dcUnivTyVars dc
                                                       argSubts = zip argTVs args
                                                       args'    = map (substTys argSubts) argTys
                                                   in case args' of
                                                      (arg:_) -> synchronizedClk tcm arg
                                                      _ -> Nothing
                                           _    -> Nothing
  | otherwise
  = Nothing

-- | Converts a Core type to a HWType given a function that translates certain
-- builtin types. Returns a string containing the error message when the Core
-- type is not translatable.
coreTypeToHWType :: (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType))
                 -> HashMap TyConName TyCon
                 -> Type
                 -> Either String HWType
coreTypeToHWType builtInTranslation m (builtInTranslation m -> Just hty) = hty
coreTypeToHWType builtInTranslation m (coreView m -> Just ty) = coreTypeToHWType builtInTranslation m ty
coreTypeToHWType builtInTranslation m ty@(tyView -> TyConApp tc args) = mkADT builtInTranslation m (showDoc ty) tc args
coreTypeToHWType _ _ ty = Left $ "Can't translate non-tycon type: " ++ showDoc ty

-- | Converts an algebraic Core type (split into a TyCon and its argument) to a HWType.
mkADT :: (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType)) -- ^ Hardcoded Type -> HWType translator
      -> HashMap TyConName TyCon -- ^ TyCon cache
      -> String -- ^ String representation of the Core type for error messages
      -> TyConName -- ^ The TyCon
      -> [Type] -- ^ Its applied arguments
      -> Either String HWType
mkADT _ m tyString tc _
  | isRecursiveTy m tc
  = Left $ $(curLoc) ++ "Can't translate recursive type: " ++ tyString

mkADT builtInTranslation m tyString tc args = case tyConDataCons (m HashMap.! tc) of
  []  -> Left $ $(curLoc) ++ "Can't translate empty type: " ++ tyString
  dcs -> do
    let tcName       = pack $ name2String tc
        argTyss      = map dcArgTys dcs
        argTVss      = map dcUnivTyVars dcs
        argSubts     = map (`zip` args) argTVss
        substArgTyss = zipWith (\s tys -> map (substTys s) tys) argSubts argTyss
    argHTyss         <- mapM (mapM (coreTypeToHWType builtInTranslation m)) substArgTyss
    case (dcs,argHTyss) of
      (_:[],[[elemTy]])      -> return elemTy
      (_:[],[elemTys@(_:_)]) -> return $ Product tcName elemTys
      (_   ,concat -> [])    -> return $ Sum tcName $ map (pack . name2String . dcName) dcs
      (_   ,elemHTys)        -> return $ SP tcName
                                      $ zipWith (\dc tys ->
                                                  ( pack . name2String $ dcName dc
                                                  , tys
                                                  )
                                                ) dcs elemHTys

-- | Simple check if a TyCon is recursively defined.
isRecursiveTy :: HashMap TyConName TyCon -> TyConName -> Bool
isRecursiveTy m tc = case tyConDataCons (m HashMap.! tc) of
    []  -> False
    dcs -> let argTyss      = map dcArgTys dcs
               argTycons    = (map fst . catMaybes) $ (concatMap . map) splitTyConAppM argTyss
           in tc `elem` argTycons

-- | Determines if a Core type is translatable to a HWType given a function that
-- translates certain builtin types.
representableType :: (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType))
                  -> HashMap TyConName TyCon
                  -> Type
                  -> Bool
representableType builtInTranslation m = either (const False) ((> 0) . typeSize) . coreTypeToHWType builtInTranslation m

-- | Determines the bitsize of a type
typeSize :: HWType
         -> Int
typeSize Void = 0
typeSize String = 1
typeSize Bool = 1
typeSize (Clock _ _) = 1
typeSize (Reset _ _) = 1
typeSize (BitVector i) = i
typeSize (Index 0) = 0
typeSize (Index 1) = 1
typeSize (Index u) = fromMaybe 0 (clogBase 2 u)
typeSize (Signed i) = i
typeSize (Unsigned i) = i
typeSize (Vector n el) = n * typeSize el
typeSize (RTree d el) = (2^d) * typeSize el
typeSize t@(SP _ cons) = conSize t +
  maximum (map (sum . map typeSize . snd) cons)
typeSize (Sum _ dcs) = max 1 (fromMaybe 0 . clogBase 2 . toInteger $ length dcs)
typeSize (Product _ tys) = sum $ map typeSize tys

-- | Determines the bitsize of the constructor of a type
conSize :: HWType
        -> Int
conSize (SP _ cons) = fromMaybe 0 . clogBase 2 . toInteger $ length cons
conSize t           = typeSize t

-- | Gives the length of length-indexed types
typeLength :: HWType
           -> Int
typeLength (Vector n _) = n
typeLength _            = 0

-- | Gives the HWType corresponding to a term. Returns an error if the term has
-- a Core type that is not translatable to a HWType.
termHWType :: String
           -> Term
           -> NetlistMonad HWType
termHWType loc e = do
  m  <- Lens.use tcCache
  ty <- termType m e
  unsafeCoreTypeToHWTypeM loc ty

-- | Gives the HWType corresponding to a term. Returns 'Nothing' if the term has
-- a Core type that is not translatable to a HWType.
termHWTypeM :: Term
            -> NetlistMonad (Maybe HWType)
termHWTypeM e = do
  m  <- Lens.use tcCache
  ty <- termType m e
  coreTypeToHWTypeM ty

-- | Uniquely rename all the variables and their references in a normalized
-- term
mkUniqueNormalized :: ([Id],[LetBinding],Id)
                   -> NetlistMonad ([Id],[LetBinding],TmName)
mkUniqueNormalized (args,binds,res) = do
  -- Make arguments unique
  (args',subst)   <- mkUnique []    args
  -- Make result unique
  ([res1],subst') <- mkUnique subst [res]
  let bndrs = map fst binds
      exprs = map (unembed . snd) binds
      usesOutput = concatMap (filter (== varName res) . Lens.toListOf termFreeIds) exprs
  -- If the let-binder carrying the result is used in a feedback loop
  -- rename the let-binder to "<X>_rec", and assign the "<X>_rec" to
  -- "<X>". We do this because output ports in most HDLs cannot be read.
  (res2,subst'',extraBndr) <- case usesOutput of
    [] -> return (varName res1,(res,res1):subst',[] :: [(Id, Embed Term)])
    _  -> do
      ([res3],_) <- mkUnique [] [modifyVarName (`appendToName` "_rec") res1]
      return (varName res3,(res,res3):subst'
             ,[(res1,embed $ Var (unembed $ varType res) (varName res3))])
  -- Replace occurences of "<X>" by "<X>_rec"
  let resN    = varName res
      bndrs'  = map (\i -> if varName i == resN then modifyVarName (const res2) i else i) bndrs
      (bndrsL,r:bndrsR) = break ((== res2).varName) bndrs'
  -- Make let-binders unique
  (bndrsL',substL) <- mkUnique subst'' bndrsL
  (bndrsR',substR) <- mkUnique substL  bndrsR
  -- Replace old IDs by update unique IDs in the RHSs of the let-binders
  exprs' <- fmap (map embed) $ Monad.foldM subsBndrs exprs substR
  -- Return the uniquely named arguments, let-binders, and result
  return (args',zip (bndrsL' ++ r:bndrsR') exprs' ++ extraBndr,varName res1)
  where
    subsBndrs :: [Term] -> (Id,Id) -> NetlistMonad [Term]
    subsBndrs es (f,r) = mapM (subsBndr f r) es

    subsBndr :: Id -> Id -> Term -> NetlistMonad Term
    subsBndr f r e = case e of
      Var t v | v == varName f -> return . Var t $ varName r
      App e1 e2                -> App <$> subsBndr f r e1
                                      <*> subsBndr f r e2
      Case scrut ty alts       -> Case <$> subsBndr f r scrut
                                       <*> pure ty
                                       <*> mapM ( return
                                                . uncurry bind
                                                <=< secondM (subsBndr f r)
                                                <=< unbind
                                                ) alts
      _ -> return e

-- | Make a set of IDs unique; also returns a substitution from old ID to new
-- updated unique ID.
mkUnique :: [(Id,Id)] -- ^ Existing substitution
         -> [Id]      -- ^ IDs to make unique
         -> NetlistMonad ([Id],[(Id,Id)])
         -- ^ (Unique IDs, update substitution)
mkUnique = go []
  where
    go :: [Id] -> [(Id,Id)] -> [Id] -> NetlistMonad ([Id],[(Id,Id)])
    go processed subst []     = return (reverse processed,subst)
    go processed subst (i:is) = do
      iN <- mkUniqueIdentifier . pack . name2String $ varName i
      let iN_unpacked = unpack iN
          i'          = modifyVarName (repName iN_unpacked) i
      go (i':processed) ((i,i'):subst) is

    repName s n = makeName s (name2Integer n)

mkUniqueIdentifier :: Identifier
                   -> NetlistMonad Identifier
mkUniqueIdentifier nm = do
  seen  <- Lens.use seenIds
  seenC <- Lens.use seenComps
  i     <- mkBasicId nm
  let s = seenC ++ seen
  if i `elem` s
     then go 0 s i
     else do
      seenIds %= (i:)
      return i
  where
    go :: Integer -> [Identifier] -> Identifier -> NetlistMonad Identifier
    go n s i = do
      i' <- mkBasicId (i `append` pack ('_':show n))
      if i' `elem` s
         then go (n+1) s i
         else do
          seenIds %= (i':)
          return i'

-- | Append a string to a name
appendToName :: TmName
             -> String
             -> TmName
appendToName n s = makeName (name2String n ++ s) (name2Integer n)

-- | Preserve the Netlist '_varEnv' and '_varCount' when executing a monadic action
preserveVarEnv :: NetlistMonad a
               -> NetlistMonad a
preserveVarEnv action = do
  -- store state
  vCnt  <- Lens.use varCount
  vEnv  <- Lens.use varEnv
  vComp <- Lens.use curCompNm
  vSeen <- Lens.use seenIds
  -- perform action
  val <- action
  -- restore state
  varCount  .= vCnt
  varEnv    .= vEnv
  curCompNm .= vComp
  seenIds   .= vSeen
  return val

dcToLiteral :: HWType -> Int -> Literal
dcToLiteral Bool 1 = BoolLit False
dcToLiteral Bool 2 = BoolLit True
dcToLiteral _ i    = NumLit (toInteger i-1)
