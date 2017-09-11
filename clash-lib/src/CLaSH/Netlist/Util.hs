{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                         2017, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utilities for converting Core Type/Term to Netlist datatypes
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module CLaSH.Netlist.Util where

import           Control.Error           (hush)
import           Control.Lens            ((.=),(%=))
import qualified Control.Lens            as Lens
import           Control.Monad           (zipWithM)
import           Control.Monad.Trans.Except (runExcept)
import           Data.Either             (partitionEithers)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.List               (unzip4)
import           Data.Maybe              (catMaybes,fromMaybe,listToMaybe)
import           Data.Text.Lazy          (append,pack,unpack)
import           Unbound.Generics.LocallyNameless
  (Embed, Fresh, embed, unbind, unembed, unrec)
import qualified Unbound.Generics.LocallyNameless as Unbound

import           CLaSH.Annotations.TopEntity (PortName (..), TopEntity (..))
import           CLaSH.Driver.Types      (Manifest (..))
import           CLaSH.Core.DataCon      (DataCon (..))
import           CLaSH.Core.FreeVars     (termFreeIds, typeFreeVars)
import           CLaSH.Core.Name         (Name (..), appendToName, name2String)
import           CLaSH.Core.Pretty       (showDoc)
import           CLaSH.Core.Subst        (substTms, substTys)
import           CLaSH.Core.Term         (LetBinding, Term (..), TmName, TmOccName)
import           CLaSH.Core.TyCon
  (TyCon (..), TyConName, TyConOccName, tyConDataCons)
import           CLaSH.Core.Type         (Type (..), TypeView (..), LitTy (..),
                                          coreView, splitTyConAppM, tyView)
import           CLaSH.Core.Util         (collectBndrs, termType, tyNatSize)
import           CLaSH.Core.Var          (Id, Var (..), modifyVarName)
import           CLaSH.Netlist.Id        (IdType (..))
import           CLaSH.Netlist.Types     as HW
import           CLaSH.Util

mkIdentifier :: IdType -> Identifier -> NetlistMonad Identifier
mkIdentifier typ nm = Lens.use mkIdentifierFn <*> pure typ <*> pure nm

extendIdentifier
  :: IdType
  -> Identifier
  -> Identifier
  -> NetlistMonad Identifier
extendIdentifier typ nm ext =
  Lens.use extendIdentifierFn <*> pure typ <*> pure nm <*> pure ext

-- | Split a normalized term into: a list of arguments, a list of let-bindings,
-- and a variable reference that is the body of the let-binding. Returns a
-- String containing the error is the term was not in a normalized form.
splitNormalized :: Fresh m
                => HashMap TyConOccName TyCon
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
                       -> (HashMap TyConOccName TyCon -> Type -> Maybe (Either String HWType))
                       -> HashMap TyConOccName TyCon
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
synchronizedClk :: HashMap TyConOccName TyCon -- ^ TyCon cache
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
      "CLaSH.Signal.Internal.Signal" -> case splitTyConAppM (head args) of
        Just (_,[LitTy (SymTy s),litTy])
          | Right i <- runExcept (tyNatSize tcm litTy) -> Just (pack s,i)
        _ -> error $ $(curLoc) ++ "Clock period not a simple literal: " ++ showDoc ty
      _                               -> case tyConDataCons (tcm HashMap.! nameOcc tyCon) of
                                           [dc] -> let argTys   = dcArgTys dc
                                                       argTVs   = map nameOcc (dcUnivTyVars dc)
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
coreTypeToHWType :: (HashMap TyConOccName TyCon -> Type -> Maybe (Either String HWType))
                 -> HashMap TyConOccName TyCon
                 -> Type
                 -> Either String HWType
coreTypeToHWType builtInTranslation m (builtInTranslation m -> Just hty) = hty
coreTypeToHWType builtInTranslation m (coreView m -> Just ty) = coreTypeToHWType builtInTranslation m ty
coreTypeToHWType builtInTranslation m ty@(tyView -> TyConApp tc args) = mkADT builtInTranslation m (showDoc ty) tc args
coreTypeToHWType _ _ ty = Left $ "Can't translate non-tycon type: " ++ showDoc ty

-- | Converts an algebraic Core type (split into a TyCon and its argument) to a HWType.
mkADT :: (HashMap TyConOccName TyCon -> Type -> Maybe (Either String HWType)) -- ^ Hardcoded Type -> HWType translator
      -> HashMap TyConOccName TyCon -- ^ TyCon cache
      -> String -- ^ String representation of the Core type for error messages
      -> TyConName -- ^ The TyCon
      -> [Type] -- ^ Its applied arguments
      -> Either String HWType
mkADT _ m tyString tc _
  | isRecursiveTy m tc
  = Left $ $(curLoc) ++ "Can't translate recursive type: " ++ tyString

mkADT builtInTranslation m tyString tc args = case tyConDataCons (m HashMap.! nameOcc tc) of
  []  -> Left $ $(curLoc) ++ "Can't translate empty type: " ++ tyString
  dcs -> do
    let tcName       = pack $ name2String tc
        argTyss      = map dcArgTys dcs
        argTVss      = map dcUnivTyVars dcs
        argSubts     = map ((`zip` args) . map nameOcc) argTVss
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
isRecursiveTy :: HashMap TyConOccName TyCon -> TyConName -> Bool
isRecursiveTy m tc = case tyConDataCons (m HashMap.! nameOcc tc) of
    []  -> False
    dcs -> let argTyss      = map dcArgTys dcs
               argTycons    = (map fst . catMaybes) $ (concatMap . map) splitTyConAppM argTyss
           in tc `elem` argTycons

-- | Determines if a Core type is translatable to a HWType given a function that
-- translates certain builtin types.
representableType :: (HashMap TyConOccName TyCon -> Type -> Maybe (Either String HWType))
                  -> Bool -- ^ Allow zero-bit things
                  -> HashMap TyConOccName TyCon
                  -> Type
                  -> Bool
representableType builtInTranslation allowZero m = either (const False) isRepresentable . coreTypeToHWType builtInTranslation m
  where
    isRepresentable hty = case hty of
      String          -> True
      Bool            -> True
      BitVector n     -> (n > 0) || allowZero
      Index n         -> (n > 0) || allowZero
      Signed n        -> (n > 0) || allowZero
      Unsigned  n     -> (n > 0) || allowZero
      Vector n elTy
        | n > 0 || allowZero -> isRepresentable elTy
      RTree _n elTy   -> isRepresentable elTy
      Sum {}          -> True
      Product _ elTys -> all isRepresentable elTys
      SP _ elTyss     -> all (all isRepresentable . snd) elTyss
      Clock {}        -> True
      Reset {}        -> True
      _               -> False

-- | Determines the bitsize of a type
typeSize :: HWType
         -> Int
typeSize Void = 0
typeSize String = 1
typeSize Bool = 1
typeSize (Clock {}) = 1
typeSize (Reset {}) = 1
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
mkUniqueNormalized
  :: Maybe (Maybe TopEntity)
  -> ([Id],[LetBinding],Id)
  -> NetlistMonad
      ([(Identifier,HWType)]
      ,[Declaration]
      ,[(Identifier,HWType)]
      ,[Declaration]
      ,[LetBinding]
      ,TmName)
mkUniqueNormalized topMM (args,binds,res) = do
  -- Make arguments unique
  (iports,iwrappers,substArgs) <- mkUniqueArguments topMM args
  -- Make result unique
  (oports,owrappers,res1,substRes) <- mkUniqueResult topMM res
  let subst' = substRes:substArgs
      bndrs = map fst binds
      exprs = map (unembed . snd) binds
      usesOutput = concatMap (filter ( == (nameOcc . varName) res)
                                     . Lens.toListOf termFreeIds
                                     ) exprs
  -- If the let-binder carrying the result is used in a feedback loop
  -- rename the let-binder to "<X>_rec", and assign the "<X>_rec" to
  -- "<X>". We do this because output ports in most HDLs cannot be read.
  (res2,subst'',extraBndr) <- case usesOutput of
    [] -> return (varName res1
                 ,(nameOcc $ varName res, Var (unembed $ varType res1) (varName res1)):subst'
                 ,[] :: [(Id, Embed Term)])
    _  -> do
      ([res3],_) <- mkUnique [] [modifyVarName (`appendToName` "_rec") res]
      return (varName res3,(nameOcc $ varName res,Var (unembed $ varType res3) (varName res3)):subst'
             ,[(res1,embed $ Var (unembed $ varType res) (varName res3))])
  -- Replace occurences of "<X>" by "<X>_rec"
  let resN    = varName res
      bndrs'  = map (\i -> if varName i == resN then modifyVarName (const res2) i else i) bndrs
      (bndrsL,r:bndrsR) = break ((== res2).varName) bndrs'
  -- Make let-binders unique
  (bndrsL',substL) <- mkUnique subst'' bndrsL
  (bndrsR',substR) <- mkUnique substL  bndrsR
  -- Replace old IDs by updated unique IDs in the RHSs of the let-binders
  let exprs' = map (embed . substTms substR) exprs
  -- Return the uniquely named arguments, let-binders, and result
  return (iports,iwrappers,oports,owrappers,zip (bndrsL' ++ r:bndrsR') exprs' ++ extraBndr,varName res1)

mkUniqueArguments
  :: Maybe (Maybe TopEntity)
  -> [Id]
  -> NetlistMonad
       ([(Identifier,HWType)]
       ,[Declaration]
       ,[(TmOccName,Term)]
       )
mkUniqueArguments Nothing args = do
  (args',subst) <- mkUnique [] args
  ports <- mapM idToPort args'
  return (ports,[],subst)

mkUniqueArguments (Just teM) args = do
  let iPortSupply = maybe (repeat Nothing) (extendPorts . t_inputs) teM
  (ports,decls,subst) <- unzip3 <$> zipWithM go iPortSupply args
  let ports' = concat ports
  return (ports', concat decls, subst)
  where
    go pM var = do
      tcm       <- Lens.use tcCache
      typeTrans <- Lens.use typeTranslator
      let i    = varName var
          i'   = pack (name2String i)
          ty   = unembed (varType var)
          hwty = unsafeCoreTypeToHWType $(curLoc) typeTrans tcm ty
      (ports,decls,_,pN) <- mkInput pM (i',hwty)
      return (ports,decls,(nameOcc i, Var ty (repName (unpack pN) i)))

mkUniqueResult
  :: Maybe (Maybe TopEntity)
  -> Id
  -> NetlistMonad ([(Identifier,HWType)],[Declaration],Id,(TmOccName,Term))
mkUniqueResult Nothing res = do
  ([res'],[subst]) <- mkUnique [] [res]
  port <- idToPort res'
  return ([port],[],res',subst)

mkUniqueResult (Just teM) res = do
  tcm       <- Lens.use tcCache
  typeTrans <- Lens.use typeTranslator
  let o    = varName res
      o'   = pack (name2String o)
      ty   = unembed (varType res)
      hwty = unsafeCoreTypeToHWType $(curLoc) typeTrans tcm ty
      oPortSupply = fmap t_output teM
  (ports,decls,pN) <- mkOutput oPortSupply (o',hwty)
  let pO = repName (unpack pN) o
  return (ports,decls,Id pO (embed ty),(nameOcc o,Var ty pO))

idToPort :: Id -> NetlistMonad (Identifier,HWType)
idToPort var = do
      tcm <- Lens.use tcCache
      typeTrans <- Lens.use typeTranslator
      let i  = varName var
          ty = unembed (varType var)
      return
        ( pack $ name2String i
        , unsafeCoreTypeToHWType $(curLoc) typeTrans tcm ty
        )

repName :: String -> Name a -> Name a
repName s (Name sort _ loc) = Name sort (Unbound.string2Name s) loc

-- | Make a set of IDs unique; also returns a substitution from old ID to new
-- updated unique ID.
mkUnique
  :: [(TmOccName,Term)]
  -- ^ Existing substitution
  -> [Id]
  -- ^ IDs to make unique
  -> NetlistMonad ([Id],[(TmOccName,Term)])
  -- ^ (Unique IDs, update substitution)
mkUnique = go []
  where
    go :: [Id] -> [(TmOccName,Term)] -> [Id] -> NetlistMonad ([Id],[(TmOccName,Term)])
    go processed subst []     = return (reverse processed,subst)
    go processed subst (i:is) = do
      iN <- mkUniqueIdentifier Extended . pack . name2String $ varName i
      let iN_unpacked = unpack iN
          i'          = modifyVarName (repName iN_unpacked) i
      go (i':processed)
         ((nameOcc . varName $ i,Var (unembed $ varType i') (varName i')):subst)
         is

mkUniqueIdentifier
  :: IdType
  -> Identifier
  -> NetlistMonad Identifier
mkUniqueIdentifier typ nm = do
  seen  <- Lens.use seenIds
  seenC <- Lens.use seenComps
  i     <- mkIdentifier typ nm
  let s = seenC ++ seen
  if i `elem` s
     then go 0 s i
     else do
      seenIds %= (i:)
      return i
  where
    go :: Integer -> [Identifier] -> Identifier -> NetlistMonad Identifier
    go n s i = do
      i' <- extendIdentifier typ i (pack ('_':show n))
      if i' `elem` s
         then go (n+1) s i
         else do
          seenIds %= (i':)
          return i'

-- | Preserve the Netlist '_varEnv' and '_varCount' when executing a monadic action
preserveVarEnv :: NetlistMonad a
               -> NetlistMonad a
preserveVarEnv action = do
  -- store state
  vCnt  <- Lens.use varCount
  vComp <- Lens.use curCompNm
  vSeen <- Lens.use seenIds
  -- perform action
  val <- action
  -- restore state
  varCount  .= vCnt
  curCompNm .= vComp
  seenIds   .= vSeen
  return val

dcToLiteral :: HWType -> Int -> Literal
dcToLiteral Bool 1 = BoolLit False
dcToLiteral Bool 2 = BoolLit True
dcToLiteral _ i    = NumLit (toInteger i-1)

-- * TopEntity Annotations

extendPorts :: [PortName] -> [Maybe PortName]
extendPorts ps = map Just ps ++ repeat Nothing

appendNumber
  :: (Identifier,HWType)
  -> Int
  -> (Identifier,HWType)
appendNumber (nm,hwty) i =
  (nm `append` "_" `append` pack (show i),hwty)

portName
  :: String
  -> Identifier
  -> Identifier
portName [] i = i
portName x  _ = pack x

appendIdentifier
  :: (Identifier,HWType)
  -> Int
  -> NetlistMonad (Identifier,HWType)
appendIdentifier (nm,hwty) i =
  (,hwty) <$> extendIdentifier Extended nm (pack ('_':show i))

uniquePortName
  :: String
  -> Identifier
  -> NetlistMonad Identifier
uniquePortName [] i = mkUniqueIdentifier Extended i
uniquePortName x  _ = do
  let x' = pack x
  seenIds %= (x':)
  return x'

mkInput
  :: Maybe PortName
  -> (Identifier,HWType)
  -> NetlistMonad ([(Identifier,HWType)],[Declaration],Expr,Identifier)
mkInput pM = case pM of
  Nothing -> go
  Just p  -> go' p
  where
    go (i,hwty) = do
      i' <- mkUniqueIdentifier Extended i
      case hwty of
        Vector sz hwty' -> do
          arguments <- mapM (appendIdentifier (i',hwty')) [0..sz-1]
          (ports,_,exprs,_) <- unzip4 <$> mapM (mkInput Nothing) arguments
          let netdecl  = NetDecl Nothing i' hwty
              vecExpr  = mkVectorChain sz hwty' exprs
              netassgn = Assignment i' vecExpr
          return (concat ports,[netdecl,netassgn],vecExpr,i')

        RTree d hwty' -> do
          arguments <- mapM (appendIdentifier (i',hwty')) [0..2^d-1]
          (ports,_,exprs,_) <- unzip4 <$> mapM (mkInput Nothing) arguments
          let netdecl  = NetDecl Nothing i' hwty
              trExpr   = mkRTreeChain d hwty' exprs
              netassgn = Assignment i' trExpr
          return (concat ports,[netdecl,netassgn],trExpr,i')

        Product _ hwtys -> do
          arguments <- zipWithM appendIdentifier (map (i',) hwtys) [0..]
          (ports,_,exprs,_) <- unzip4 <$> mapM (mkInput Nothing) arguments
          let netdecl  = NetDecl Nothing i' hwty
              dcExpr   = DataCon hwty (DC (hwty,0)) exprs
              netassgn = Assignment i' dcExpr
          return (concat ports,[netdecl,netassgn],dcExpr,i')

        _ -> return ([(i',hwty)],[],Identifier i' Nothing,i')


    go' (PortName p) (i,hwty) = do
      pN <- uniquePortName p i
      return ([(pN,hwty)],[],Identifier pN Nothing,pN)

    go' (PortField p ps) (i,hwty) = do
      pN <- uniquePortName p i
      case hwty of
        Vector sz hwty' -> do
          arguments <- mapM (appendIdentifier (pN,hwty')) [0..sz-1]
          (ports,_,exprs,_) <- unzip4 <$> zipWithM mkInput (extendPorts ps) arguments
          let netdecl  = NetDecl Nothing pN hwty
              vecExpr  = mkVectorChain sz hwty' exprs
              netassgn = Assignment pN vecExpr
          return (concat ports,[netdecl,netassgn],vecExpr,pN)

        RTree d hwty' -> do
          arguments <- mapM (appendIdentifier (pN,hwty')) [0..2^d-1]
          (ports,_,exprs,_) <- unzip4 <$> zipWithM mkInput (extendPorts ps) arguments
          let netdecl  = NetDecl Nothing pN hwty
              trExpr  = mkRTreeChain d hwty' exprs
              netassgn = Assignment pN trExpr
          return (concat ports,[netdecl,netassgn],trExpr,pN)

        Product _ hwtys -> do
          arguments <- zipWithM appendIdentifier (map (pN,) hwtys) [0..]
          (ports,_,exprs,_) <- unzip4 <$> zipWithM mkInput (extendPorts ps) arguments
          let netdecl  = NetDecl Nothing pN hwty
              dcExpr   = DataCon hwty (DC (hwty,0)) exprs
              netassgn = Assignment pN dcExpr
          return (concat ports,[netdecl,netassgn],dcExpr,pN)

        _ -> return ([(pN,hwty)],[],Identifier pN Nothing,pN)

-- | Create a Vector chain for a list of 'Identifier's
mkVectorChain :: Int
              -> HWType
              -> [Expr]
              -> Expr
mkVectorChain _ elTy []      = DataCon (Vector 0 elTy) VecAppend []
mkVectorChain _ elTy [e]     = DataCon (Vector 1 elTy) VecAppend
                                [e]
mkVectorChain sz elTy (e:es) = DataCon (Vector sz elTy) VecAppend
                                [ e
                                , mkVectorChain (sz-1) elTy es
                                ]

-- | Create a RTree chain for a list of 'Identifier's
mkRTreeChain :: Int
             -> HWType
             -> [Expr]
             -> Expr
mkRTreeChain _ elTy [e] = DataCon (RTree 0 elTy) RTreeAppend
                                  [e]
mkRTreeChain d elTy es =
  let (esL,esR) = splitAt (length es `div` 2) es
  in  DataCon (RTree d elTy) RTreeAppend
        [ mkRTreeChain (d-1) elTy esL
        , mkRTreeChain (d-1) elTy esR
        ]

-- | Generate output port mappings
mkOutput
  :: Maybe PortName
  -> (Identifier,HWType)
  -> NetlistMonad ([(Identifier,HWType)],[Declaration],Identifier)
mkOutput pM = case pM of
  Nothing -> go
  Just p  -> go' p
  where
    go (o,hwty) = do
      o' <- mkUniqueIdentifier Extended o
      case hwty of
        Vector sz hwty' -> do
          results <- mapM (appendIdentifier (o',hwty')) [0..sz-1]
          (ports,decls,ids) <- unzip3 <$> mapM (mkOutput Nothing) results
          let netdecl = NetDecl Nothing o' hwty
              assigns = zipWith (assignId o' hwty 10) ids [0..]
          return (concat ports,netdecl:assigns ++ concat decls,o')

        RTree d hwty' -> do
          results <- mapM (appendIdentifier (o',hwty')) [0..2^d-1]
          (ports,decls,ids) <- unzip3 <$> mapM (mkOutput Nothing) results
          let netdecl = NetDecl Nothing o' hwty
              assigns = zipWith (assignId o' hwty 10) ids [0..]
          return (concat ports,netdecl:assigns ++ concat decls,o')

        Product _ hwtys -> do
          results <- zipWithM appendIdentifier (map (o,) hwtys) [0..]
          (ports,decls,ids) <- unzip3 <$> mapM (mkOutput Nothing) results
          let netdecl = NetDecl Nothing o' hwty
              assigns = zipWith (assignId o' hwty 0) ids [0..]
          return (concat ports,netdecl:assigns ++ concat decls,o')

        _ -> return ([(o',hwty)],[],o')

    go' (PortName p) (o,hwty) = do
      pN <- uniquePortName p o
      return ([(pN,hwty)],[],pN)

    go' (PortField p ps) (o,hwty) = do
      pN <- uniquePortName p o
      case hwty of
        Vector sz hwty' -> do
          results <- mapM (appendIdentifier (pN,hwty')) [0..sz-1]
          (ports,decls,ids) <- unzip3 <$> zipWithM mkOutput (extendPorts ps) results
          let netdecl = NetDecl Nothing pN hwty
              assigns = zipWith (assignId pN hwty 10) ids [0..]
          return (concat ports,netdecl:assigns ++ concat decls,pN)

        RTree d hwty' -> do
          results <- mapM (appendIdentifier (pN,hwty')) [0..2^d-1]
          (ports,decls,ids) <- unzip3 <$> zipWithM mkOutput (extendPorts ps) results
          let netdecl = NetDecl Nothing pN hwty
              assigns = zipWith (assignId pN hwty 10) ids [0..]
          return (concat ports,netdecl:assigns ++ concat decls,pN)

        Product _ hwtys -> do
          results <- zipWithM appendIdentifier (map (pN,) hwtys) [0..]
          (ports,decls,ids) <- unzip3 <$> zipWithM mkOutput (extendPorts ps) results
          let netdecl = NetDecl Nothing pN hwty
              assigns = zipWith (assignId pN hwty 0) ids [0..]
          return (concat ports,netdecl:assigns ++ concat decls,pN)

        _ -> return ([(pN,hwty)],[],pN)

    assignId p hwty con i n =
      Assignment i (Identifier p (Just (Indexed (hwty,con,n))))

-- | Instantiate a TopEntity, and add the proper type-conversions where needed
mkTopUnWrapper
  :: TmName
  -- ^ Name of the TopEntity component
  -> Maybe TopEntity
  -- ^ (maybe) a corresponding @TopEntity@ annotation
  -> Manifest
  -- ^ a corresponding @Manifest@
  -> (Identifier,HWType)
  -- ^ The name and type of the signal to which to assign the result
  -> [(Expr,HWType)]
  -- ^ The arguments
  -> NetlistMonad [Declaration]
mkTopUnWrapper topEntity annM man dstId args = do
  let inTys    = portInTypes man
      outTys   = portOutTypes man
      inNames  = portInNames man
      outNames = portOutNames man

  -- component name
  let modName = takeWhile (/= '.') (name2String topEntity) ++
                maybe "" (("_" ++) . t_name) annM
  topName <- extendIdentifier Basic (pack modName) "_topEntity"
  let topName' = maybe topName (pack . t_name) annM
      topM     = fmap (const topName') annM

  -- inputs
  let iPortSupply = maybe (repeat Nothing)
                        (extendPorts . t_inputs)
                        annM
  arguments <- zipWithM appendIdentifier (map (first (const "input")) args) [0..]
  (_,arguments1) <- mapAccumLM (\acc (p,i) -> mkTopInput topM acc p i)
                      (zip inNames inTys)
                      (zip iPortSupply arguments)
  let (iports,wrappers,idsI) = unzip3 arguments1
      inpAssigns             = zipWith (argBV topM) idsI (map fst args)

  -- output
  let oPortSupply = maybe (repeat Nothing)
                        (extendPorts . (:[]) . t_output)
                        annM

      result = ("result",snd dstId)
  (_,(oports,unwrappers,idsO)) <- mkTopOutput topM (zip outNames outTys)
                                    (head oPortSupply) result
  let outpAssign = Assignment (fst dstId) (resBV topM idsO)

  instLabel <- extendIdentifier Basic topName' ("_" `append` fst dstId)
  let topCompDecl =
        InstDecl
          topName'
          instLabel
          (map (\(p,i,t) -> (Identifier p Nothing,In, t,Identifier i Nothing)) (concat iports) ++
           map (\(p,o,t) -> (Identifier p Nothing,Out,t,Identifier o Nothing)) oports)


  return (inpAssigns ++ concat wrappers ++ (topCompDecl:unwrappers) ++ [outpAssign])

-- | Convert between BitVector for an argument
argBV
  :: Maybe Identifier
  -- ^ (maybe) Name of the _TopEntity_
  -> Either Identifier (Identifier, HWType)
  -- ^ Either:
  --   * A /normal/ argument
  --   * An argument with a @PortName@
  -> Expr
  -> Declaration
argBV _    (Left i)      e = Assignment i e
argBV topM (Right (i,t)) e = Assignment i
                           . doConv t (fmap Just topM)            False
                           $ doConv t (fmap (const Nothing) topM) True  e

-- | Convert between BitVector for the result
resBV
  :: Maybe Identifier
  -- ^ (mabye) Name of the _TopEntity_
  -> Either Identifier (Identifier, HWType)
  -- ^ Either:
  --   * A /normal/ result
  --   * A result with a @PortName@
  -> Expr
resBV _    (Left i)      = Identifier i Nothing
resBV topM (Right (i,t)) = doConv t (fmap (const Nothing) topM) False
                         . doConv t (fmap Just topM)            True
                         $ Identifier i Nothing


-- | Add to/from-BitVector conversion logic
doConv
  :: HWType
  -- ^ We only need it for certain types
  -> Maybe (Maybe Identifier)
  -- ^
  --   * Nothing:         No _given_ TopEntity, no need for conversion, this
  --                      happens when we have a _TestBench_, but no
  --                      _TopEntity_ annotation.
  --   * Just Nothing:    Converting to/from a BitVector for one of the
  --                      internally defined types.
  --   * Just (Just top): Converting to/from a BitVector for one of the
  --                      types defined by @top@.
  -> Bool
  -- ^
  --   * True:  convert to a BitVector
  --   * False: convert from a BitVector
  -> Expr
  -- ^ The expression on top of which we have to add conversion logic
  -> Expr
doConv _    Nothing     _ e = e
doConv hwty (Just topM) b e = case hwty of
  Vector  {} -> ConvBV topM hwty b e
  RTree   {} -> ConvBV topM hwty b e
  Product {} -> ConvBV topM hwty b e
  _          -> e

-- | Generate input port mappings for the TopEntity
mkTopInput
  :: Maybe Identifier
  -- ^ (maybe) Name of the _TopEntity_
  -> [(Identifier,Identifier)]
  -- ^ /Rendered/ input port names and types
  -> Maybe PortName
  -- ^ (maybe) The @PortName@ of a _TopEntity_ annotation for this input
  -> (Identifier,HWType)
  -> NetlistMonad ([(Identifier,Identifier)]
                  ,([(Identifier,Identifier,HWType)]
                    ,[Declaration]
                    ,Either Identifier (Identifier,HWType)))
mkTopInput topM inps pM = case pM of
  Nothing -> go inps
  Just p  -> go' p inps
  where
    -- No @PortName@
    go inps'@((iN,_):rest) (i,hwty) = do
      i' <- mkUniqueIdentifier Basic i
      let iDecl = NetDecl Nothing i' hwty
      case hwty of
        Vector sz hwty' -> do
          arguments <- mapM (appendIdentifier (i',hwty')) [0..sz-1]
          (inps'',arguments1) <- mapAccumLM go inps' arguments
          let (ports,decls,ids) = unzip3 arguments1
              assigns = zipWith (argBV topM) ids
                          [ Identifier i' (Just (Indexed (hwty,10,n)))
                          | n <- [0..]]
          return (inps'',(concat ports,iDecl:assigns++concat decls,Left i'))

        RTree d hwty' -> do
          arguments <- mapM (appendIdentifier (i',hwty')) [0..2^d-1]
          (inps'',arguments1) <- mapAccumLM go inps' arguments
          let (ports,decls,ids) = unzip3 arguments1
              assigns = zipWith (argBV topM) ids
                          [ Identifier i' (Just (Indexed (hwty,10,n)))
                          | n <- [0..]]
          return (inps'',(concat ports,iDecl:assigns++concat decls,Left i'))

        Product _ hwtys -> do
          arguments <- zipWithM appendIdentifier (map (i,) hwtys) [0..]
          (inps'',arguments1) <- mapAccumLM go inps' arguments
          let (ports,decls,ids) = unzip3 arguments1
              assigns = zipWith (argBV topM) ids
                          [ Identifier i' (Just (Indexed (hwty,0,n)))
                          | n <- [0..]]
          return (inps'',(concat ports,iDecl:assigns++concat decls,Left i'))

        _ -> return (rest,([(iN,i',hwty)],[iDecl],Left i'))

    go [] _ = error "This shouldn't happen"

    -- With a @PortName@
    go' (PortName _) ((iN,iTy):inps') (_,hwty) = do
      iN' <- mkUniqueIdentifier Extended iN
      return (inps',([(iN,iN',hwty)]
                    ,[NetDecl' Nothing Wire iN' (Left iTy)]
                    ,Right (iN',hwty)))

    go' (PortName _) [] _ = error "This shouldnt happen"

    go' (PortField p ps) inps' (i,hwty) = do
      let pN = portName p i
      pN' <- mkUniqueIdentifier Extended pN
      let pDecl = NetDecl Nothing pN' hwty
      case hwty of
        Vector sz hwty' -> do
          arguments <- mapM (appendIdentifier (pN',hwty')) [0..sz-1]
          (inps'',arguments1) <-
            mapAccumLM (\acc (p',o') -> mkTopInput topM acc p' o') inps'
                       (zip (extendPorts ps) arguments)
          let (ports,decls,ids) = unzip3 arguments1
              assigns = zipWith (argBV topM) ids
                          [ Identifier pN' (Just (Indexed (hwty,10,n)))
                          | n <- [0..]]
          return (inps'',(concat ports,pDecl:assigns ++ concat decls,Left pN'))

        RTree d hwty' -> do
          arguments <- mapM (appendIdentifier (pN',hwty')) [0..2^d-1]
          (inps'',arguments1) <-
            mapAccumLM (\acc (p',o') -> mkTopInput topM acc p' o') inps'
                       (zip (extendPorts ps) arguments)
          let (ports,decls,ids) = unzip3 arguments1
              assigns = zipWith (argBV topM) ids
                          [ Identifier pN' (Just (Indexed (hwty,10,n)))
                          | n <- [0..]]
          return (inps'',(concat ports,pDecl:assigns ++ concat decls,Left pN'))

        Product _ hwtys -> do
          arguments <- zipWithM appendIdentifier (map (pN',) hwtys) [0..]
          (inps'',arguments1) <-
            mapAccumLM (\acc (p',o') -> mkTopInput topM acc p' o') inps'
                       (zip (extendPorts ps) arguments)
          let (ports,decls,ids) = unzip3 arguments1
              assigns = zipWith (argBV topM) ids
                          [ Identifier pN' (Just (Indexed (hwty,0,n)))
                          | n <- [0..]]
          return (inps'',(concat ports,pDecl:assigns ++ concat decls,Left pN'))

        _ -> return (tail inps',([(pN,pN',hwty)],[pDecl],Left pN'))

-- | Generate output port mappings for the TopEntity
mkTopOutput
  :: Maybe Identifier
  -- ^ (maybe) Name of the _TopEntity_
  -> [(Identifier,Identifier)]
  -- ^ /Rendered/ output port names and types
  -> Maybe PortName
  -- ^ (maybe) The @PortName@ of a _TopEntity_ annotation for this output
  -> (Identifier,HWType)
  -> NetlistMonad ([(Identifier,Identifier)]
                  ,([(Identifier,Identifier,HWType)]
                   ,[Declaration]
                   ,Either Identifier (Identifier,HWType))
                  )
mkTopOutput topM outps pM = case pM of
  Nothing -> go outps
  Just p  -> go' p outps
  where
    -- No @PortName@
    go outps'@((oN,_):rest) (o,hwty) = do
      o' <- mkUniqueIdentifier Extended o
      let oDecl = NetDecl Nothing o' hwty
      case hwty of
        Vector sz hwty' -> do
          results <- mapM (appendIdentifier (o',hwty')) [0..sz-1]
          (outps'',results1) <- mapAccumLM go outps' results
          let (ports,decls,ids) = unzip3 results1
              ids' = map (resBV topM) ids
              netassgn = Assignment o' (mkVectorChain sz hwty' ids')
          return (outps'',(concat ports,oDecl:netassgn:concat decls,Left o'))

        RTree d hwty' -> do
          results <- mapM (appendIdentifier (o',hwty')) [0..2^d-1]
          (outps'',results1) <- mapAccumLM go outps' results
          let (ports,decls,ids) = unzip3 results1
              ids' = map (resBV topM) ids
              netassgn = Assignment o' (mkRTreeChain d hwty' ids')
          return (outps'',(concat ports,oDecl:netassgn:concat decls,Left o'))

        Product _ hwtys -> do
          results <- zipWithM appendIdentifier (map (o',) hwtys) [0..]
          (outps'',results1) <- mapAccumLM go outps' results
          let (ports,decls,ids) = unzip3 results1
              ids' = map (resBV topM) ids
              netassgn = Assignment o' (DataCon hwty (DC (hwty,0)) ids')
          return (outps'',(concat ports,oDecl:netassgn:concat decls,Left o'))

        _ -> return (rest,([(oN,o',hwty)],[oDecl],Left o'))

    go [] _ = error "This shouldn't happen"

    -- With a @PortName@
    go' (PortName _) ((oN,oTy):outps') (_,hwty) = do
      oN' <- mkUniqueIdentifier Extended oN
      return (outps',([(oN,oN',hwty)]
                     ,[NetDecl' Nothing Wire oN' (Left oTy)]
                     ,Right (oN',hwty)))

    go' (PortName _) [] _ = error "This shouldnt happen"

    go' (PortField p ps) outps' (o,hwty) = do
      let pN = portName p o
      pN' <- mkUniqueIdentifier Extended pN
      let pDecl = NetDecl Nothing pN' hwty
      case hwty of
        Vector sz hwty' -> do
          results <- mapM (appendIdentifier (pN',hwty')) [0..sz-1]
          (outps'',results1) <-
            mapAccumLM (\acc (p',o') -> mkTopOutput topM acc p' o') outps'
                       (zip (extendPorts ps) results)
          let (ports,decls,ids) = unzip3 results1
              ids' = map (resBV topM) ids
              netassgn = Assignment pN' (mkVectorChain sz hwty' ids')
          return (outps'',(concat ports,pDecl:netassgn:concat decls,Left pN'))

        RTree d hwty' -> do
          results <- mapM (appendIdentifier (pN',hwty')) [0..2^d-1]
          (outps'',results1) <-
            mapAccumLM (\acc (p',o') -> mkTopOutput topM acc p' o') outps'
                       (zip (extendPorts ps) results)
          let (ports,decls,ids) = unzip3 results1
              ids' = map (resBV topM) ids
              netassgn = Assignment pN' (mkRTreeChain d hwty' ids')
          return (outps'',(concat ports,pDecl:netassgn:concat decls,Left pN'))

        Product _ hwtys -> do
          results <- zipWithM appendIdentifier (map (pN',) hwtys) [0..]
          (outps'',results1) <-
            mapAccumLM (\acc (p',o') -> mkTopOutput topM acc p' o') outps'
                       (zip (extendPorts ps) results)
          let (ports,decls,ids) = unzip3 results1
              ids' = map (resBV topM) ids
              netassgn = Assignment pN' (DataCon hwty (DC (hwty,0)) ids')
          return (outps'',(concat ports,pDecl:netassgn:concat decls,Left pN'))

        _ -> return (tail outps',([(pN,pN',hwty)],[pDecl],Left pN'))

concatPortDecls3
  :: [([(Identifier,Identifier,HWType)]
      ,[Declaration]
      ,Either Identifier (Identifier,HWType))]
  -> ([(Identifier,Identifier,HWType)]
     ,[Declaration]
     ,[Either Identifier (Identifier,HWType)])
concatPortDecls3 portDecls = case unzip3 portDecls of
  (ps,decls,ids) -> (concat ps, concat decls, ids)
