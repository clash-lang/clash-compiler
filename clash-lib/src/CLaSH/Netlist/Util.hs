{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

{-# OPTIONS_GHC -fcontext-stack=21 #-}

-- | Utilities for converting Core Type/Term to Netlist datatypes
module CLaSH.Netlist.Util where

import           Control.Error           (hush)
import           Control.Lens            ((.=),(<<%=))
import qualified Control.Lens            as Lens
import qualified Control.Monad           as Monad
import           Data.Either             (partitionEithers)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Maybe              (catMaybes,fromMaybe)
import           Data.Text.Lazy          (pack)
import           Unbound.LocallyNameless (Embed, Fresh, bind, embed, makeName,
                                          name2Integer, name2String, unbind,
                                          unembed, unrec)

import           CLaSH.Core.DataCon      (DataCon (..))
import           CLaSH.Core.FreeVars     (termFreeIds, typeFreeVars)
import           CLaSH.Core.Pretty       (showDoc)
import           CLaSH.Core.Subst        (substTys)
import           CLaSH.Core.Term         (LetBinding, Term (..), TmName)
import           CLaSH.Core.TyCon        (TyCon (..), TyConName, tyConDataCons)
import           CLaSH.Core.Type         (Type (..), TypeView (..), LitTy (..),
                                          splitTyConAppM, tyView)
import           CLaSH.Core.Util         (collectBndrs, termType)
import           CLaSH.Core.Var          (Id, Var (..), modifyVarName)
import           CLaSH.Netlist.Id
import           CLaSH.Netlist.Types     as HW
import           CLaSH.Util

-- | Split a normalized term into: a list of arguments, a list of let-bindings,
-- and a variable reference that is the body of the let-binding. Returns a
-- String containing the error is the term was not in a normalized form.
splitNormalized :: (Fresh m, Functor m)
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
      return $! Left ($(curLoc) ++ "Not in normal from: no Letrec:\n" ++ showDoc expr ++ "\nWhich has type:\n"  ++ showDoc ty)

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
                -> Maybe (Identifier,Int)
synchronizedClk tcm ty
  | not . null . typeFreeVars $ ty = Nothing
  | Just (tyCon,args) <- splitTyConAppM ty
  = case name2String tyCon of
      "CLaSH.Sized.Vector.Vec"        -> synchronizedClk tcm (args!!1)
      "CLaSH.Signal.Internal.SClock" -> case splitTyConAppM (head args) of
                                          Just (_,[LitTy (SymTy s),LitTy (NumTy i)]) -> Just (pack (s ++ show i),i)
                                          _ -> error $ $(curLoc) ++ "Clock period not a simple literal: " ++ showDoc ty
      "CLaSH.Signal.Internal.CSignal" -> case splitTyConAppM (head args) of
                                           Just (_,[LitTy (SymTy s),LitTy (NumTy i)]) -> Just (pack (s ++ show i),i)
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
coreTypeToHWType builtInTranslation m ty =
  fromMaybe
    (case tyView ty of
       TyConApp tc args -> mkADT builtInTranslation m (showDoc ty) tc args
       _                -> Left $ "Can't translate non-tycon type: " ++ showDoc ty)
    (builtInTranslation m ty)

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
representableType builtInTranslation m = either (const False) (const True) . coreTypeToHWType builtInTranslation m

-- | Determines the bitsize of a type
typeSize :: HWType
         -> Int
typeSize Void = 1
typeSize Bool = 1
typeSize (Clock _) = 1
typeSize (Reset _) = 1
typeSize Integer = 32
typeSize (BitVector i) = i
typeSize (Index u) = clog2 (max 2 u)
typeSize (Signed i) = i
typeSize (Unsigned i) = i
typeSize (Vector n el) = n * typeSize el
typeSize t@(SP _ cons) = conSize t +
  maximum (map (sum . map typeSize . snd) cons)
typeSize (Sum _ dcs) = max 1 (clog2 $ length dcs)
typeSize (Product _ tys) = sum $ map typeSize tys

-- | Determines the bitsize of the constructor of a type
conSize :: HWType
        -> Int
conSize (SP _ cons) = clog2 $ length cons
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

-- | Turns a Core variable reference to a Netlist expression. Errors if the term
-- is not a variable.
varToExpr :: Term
          -> Expr
varToExpr (Var _ var) = Identifier (mkBasicId . pack $ name2String var) Nothing
varToExpr _           = error "not a var"

-- | Uniquely rename all the variables and their references in a normalized
-- term
mkUniqueNormalized :: ([Id],[LetBinding],Id)
                   -> NetlistMonad ([Id],[LetBinding],TmName)
mkUniqueNormalized (args,binds,res) = do
  let args' = zipWith (\n s -> modifyVarName (`appendToName` s) n)
                args ["_i" ++ show i | i <- [(1::Integer)..]]
  let res1  = appendToName (varName res) "_o"
  let bndrs = map fst binds
  let exprs = map (unembed . snd) binds
  let usesOutput = concatMap (filter (== varName res) . termFreeIds) exprs
  let (res2,extraBndr) = case usesOutput of
                            [] -> (res1,[] :: [(Id, Embed Term)])
                            _  -> let res3 = appendToName (varName res) "_o_sig"
                                  in (res3,[(Id res1 (varType res),embed $ Var (unembed $ varType res) res3)])
  bndrs' <- mapM (mkUnique (varName res,res2)) bndrs
  let repl = zip args args' ++ zip bndrs bndrs'
  exprs' <- fmap (map embed) $ Monad.foldM subsBndrs exprs repl
  return (args',zip bndrs' exprs' ++ extraBndr,res1)

  where
    mkUnique :: (TmName,TmName) -> Id -> NetlistMonad Id
    mkUnique (find,repl) v = if find == varName v
      then return $ modifyVarName (const repl) v
      else do
        varCnt <- varCount <<%= (+1)
        let v' = modifyVarName (`appendToName` ('_' : show varCnt)) v
        return v'

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

-- | Append a string to a name
appendToName :: TmName
             -> String
             -> TmName
appendToName n s = makeName (name2String n ++ s) (name2Integer n)

-- | Preserve the Netlist '_varEnv' and '_varCount' when executing a monadic action
preserveVarEnv :: NetlistMonad a
               -> NetlistMonad a
preserveVarEnv action = do
  vCnt <- Lens.use varCount
  vEnv <- Lens.use varEnv
  val  <- action
  varCount .= vCnt
  varEnv   .= vEnv
  return val

dcToLiteral :: HWType -> Int -> Expr
dcToLiteral Bool 1 = HW.Literal Nothing (BoolLit False)
dcToLiteral Bool 2 = HW.Literal Nothing (BoolLit True)
dcToLiteral t i    = HW.Literal (Just  (t,conSize t)) (NumLit (toInteger i-1))
