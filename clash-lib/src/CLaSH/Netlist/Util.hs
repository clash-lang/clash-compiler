{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                         2017, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utilities for converting Core Type/Term to Netlist datatypes
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module CLaSH.Netlist.Util where

import           Control.Error           (hush)
import           Control.Lens            ((.=),(%=))
import qualified Control.Lens            as Lens
import           Control.Monad.Trans.Except (runExcept)
import           Data.Either             (partitionEithers)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Maybe              (catMaybes,fromMaybe)
import           Data.Text.Lazy          (Text,append,pack,unpack)
import qualified Data.Text.Lazy          as Text
import           Unbound.Generics.LocallyNameless
  (Embed, Fresh, embed, unbind, unembed, unrec)
import qualified Unbound.Generics.LocallyNameless as Unbound

import           CLaSH.Annotations.TopEntity (PortName (..), TopEntity (..))
import           CLaSH.Driver.TopWrapper
  (appendNumber, extendPorts, mkRTreeChain, mkVectorChain, portName)
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
mkUniqueNormalized :: ([Id],[LetBinding],Id)
                   -> NetlistMonad ([Id],[LetBinding],TmName)
mkUniqueNormalized (args,binds,res) = do
  -- Make arguments unique
  (args',subst)   <- mkUnique []    args
  -- Make result unique
  ([res1],subst') <- mkUnique subst [res]
  let bndrs = map fst binds
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
      ([res3],_) <- mkUnique [] [modifyVarName (`appendToName` "_rec") res1]
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
  return (args',zip (bndrsL' ++ r:bndrsR') exprs' ++ extraBndr,varName res1)

-- | Make a set of IDs unique; also returns a substitution from old ID to new
-- updated unique ID.
mkUnique :: [(TmOccName,Term)] -- ^ Existing substitution
         -> [Id]      -- ^ IDs to make unique
         -> NetlistMonad ([Id],[(TmOccName,Term)])
         -- ^ (Unique IDs, update substitution)
mkUnique = go []
  where
    go :: [Id] -> [(TmOccName,Term)] -> [Id] -> NetlistMonad ([Id],[(TmOccName,Term)])
    go processed subst []     = return (reverse processed,subst)
    go processed subst (i:is) = do
      iN <- mkUniqueIdentifier . pack . name2String $ varName i
      let iN_unpacked = unpack iN
          i'          = modifyVarName (repName iN_unpacked) i
      go (i':processed)
         ((nameOcc . varName $ i,Var (unembed $ varType i') (varName i')):subst)
         is

    repName s (Name sort _ loc) = Name sort (Unbound.string2Name s) loc

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

-- | Instantiate a TopEntity, and add the proper type-conversions where needed
mkTopUnWrapper
  :: TmName
  -- ^ Name of the TopEntity component
  -> Maybe (TopEntity,Manifest)
  -- ^ (maybe) a corresponding @TopEntity@ annotation and @Manifest@
  -> (Identifier,HWType)
  -- ^ The name and type of the signal to which to assign the result
  -> [(Expr,HWType)]
  -- ^ The arguments
  -> NetlistMonad [Declaration]
mkTopUnWrapper topEntity annManM dstId args = do
  let annM   = fst <$> annManM
      inTys  = maybe [] (portInTypes . snd) annManM
      outTys = maybe [] (portOutTypes . snd) annManM

  -- component name
  let modName = takeWhile (/= '.') (name2String topEntity) ++
                maybe "" (("_" ++) . t_name) annM
  topName <- mkBasicId (pack modName `append` "_topEntity")
  let topName' = maybe topName (pack . t_name) annM
      topM     = fmap (const topName') annM

  -- inputs
  let iPortSupply = maybe (repeat Nothing)
                        (extendPorts . t_inputs)
                        annM

      inputs1 = map (first (const "input")) args
  (_,inputs2) <- mapAccumLM (\acc (p,i) -> mkTopInput topM acc p i) inTys
                  (zip iPortSupply (zipWith appendNumber inputs1 [0..]))
  let (inputs3,wrappers,idsI) = concatPortDecls inputs2
      inpAssigns              = zipWith (argBV topM) idsI (map fst args)

  -- output
  let oPortSupply = maybe (repeat Nothing)
                        (extendPorts . t_outputs)
                        annM

      output = ("output_0",snd dstId)
  (_,(outputs1,unwrappers,idsO)) <- mkTopOutput topM outTys (head oPortSupply) output
  let outpAssign = Assignment (fst dstId) (resBV topM idsO)

  let topCompDecl =
        InstDecl
          topName'
          (topName' `append` "_" `append` fst dstId)
          (map (\(p,i,t) -> (Identifier p Nothing,In, t,Identifier i Nothing)) inputs3 ++
           map (\(p,o,t) -> (Identifier p Nothing,Out,t,Identifier o Nothing)) outputs1)


  return (inpAssigns ++ wrappers ++ (topCompDecl:unwrappers) ++ [outpAssign])

-- | Convert between BitVector for an argument
argBV
  :: Maybe Identifier
  -- ^ (maybe) Name of the _TopEntity_ annotation
  -> Either Identifier (Identifier, HWType)
  -- ^ Either:
  --   * A /normal/ argument
  --   * An argument with a @PortName@
  -> Expr
  -> Declaration
argBV _    (Left i)      e = Assignment i e
argBV topM (Right (i,t)) e = Assignment i
                           . doConv t (fmap Just            topM) False
                           $ doConv t (fmap (const Nothing) topM) True  e

-- | Convert between BitVector for the result
resBV
  :: Maybe Identifier
  -- ^ (maybe) Name of the _TopEntity_ annotation
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
  -- ^ (maybe) Name of the _TopEntity_ annotation
  -> [Text]
  -- ^ /Rendered/ input port types
  -> Maybe PortName
  -- ^ (maybe) The @PortName@ of a _TopEntity_ annotation for this input
  -> (Identifier,HWType)
  -> NetlistMonad ([Text]
                  ,([(Identifier,Identifier,HWType)]
                    ,[Declaration]
                    ,Either Identifier (Identifier,HWType)))
mkTopInput topM itys pM = case pM of
  Nothing -> go itys
  Just p  -> go' p itys
  where
    -- No @PortName@
    go itys' (i,hwty) = do
      i' <- mkUniqueIdentifier i
      let iDecl = NetDecl i' hwty
      case hwty of
        Vector sz hwty' -> do
          let inputs1 = map (appendNumber (i,hwty')) [0..sz-1]
          (itys'',inputs2) <- mapAccumLM go itys' inputs1
          let (ports,decls,ids) = concatPortDecls inputs2
              assigns = zipWith (argBV topM) ids
                          [ Identifier i' (Just (Indexed (hwty,10,n)))
                          | n <- [0..]]
          return (itys'',(ports,iDecl:assigns ++ decls,Left i'))

        RTree d hwty' -> do
          let inputs1 = map (appendNumber (i,hwty')) [0..2^d-1]
          (itys'',inputs2) <- mapAccumLM go itys' inputs1
          let (ports,decls,ids) = concatPortDecls inputs2
              assigns = zipWith (argBV topM) ids
                          [ Identifier i' (Just (Indexed (hwty,10,n)))
                          | n <- [0..]]
          return (itys'',(ports,iDecl:assigns ++ decls,Left i'))

        Product _ hwtys -> do
          let inputs1 = zipWith appendNumber (map (i,) hwtys) [0..]
          (itys'',inputs2) <- mapAccumLM go itys' inputs1
          let (ports,decls,ids) = concatPortDecls inputs2
              assigns = zipWith (argBV topM) ids
                          [ Identifier i' (Just (Indexed (hwty,10,n)))
                          | n <- [0..]]
          return (itys'',(ports,iDecl:assigns ++ decls,Left i'))

        _ -> return (tail itys',([(i,i',hwty)],[iDecl],Left i'))

    -- With a @PortName@
    go' (PortName p) (ity:itys') (i,hwty) = do
      let pN = portName p i
      pN' <- mkUniqueIdentifier pN
      return (itys',([(pN,pN',hwty)],[NetDecl' pN' (Left ity)],Right (pN',hwty)))

    go' (PortName _) [] _ = error "This shouldnt happen"

    go' (PortField p ps) itys' (i,hwty) = do
      let pN = portName p i
      pN' <- mkUniqueIdentifier pN
      let pDecl = NetDecl pN' hwty
      case hwty of
        Vector sz hwty' -> do
          let inputs1 = map (appendNumber (pN,hwty')) [0..sz-1]
          (itys'',inputs2) <-
            mapAccumLM (\acc (p',o') -> mkTopInput topM acc p' o') itys'
                       (zip (extendPorts ps) inputs1)
          let (ports,decls,ids) = concatPortDecls inputs2
              assigns = zipWith (argBV topM) ids
                          [ Identifier pN' (Just (Indexed (hwty,10,n)))
                          | n <- [0..]]
          return (itys'',(ports,pDecl:assigns ++ decls,Left pN'))

        RTree d hwty' -> do
          let inputs1 = map (appendNumber (pN,hwty')) [0..2^d-1]
          (itys'',inputs2) <-
            mapAccumLM (\acc (p',o') -> mkTopInput topM acc p' o') itys'
                       (zip (extendPorts ps) inputs1)
          let (ports,decls,ids) = concatPortDecls inputs2
              assigns = zipWith (argBV topM) ids
                          [ Identifier pN' (Just (Indexed (hwty,10,n)))
                          | n <- [0..]]
          return (itys'',(ports,pDecl:assigns ++ decls,Left pN'))

        Product _ hwtys -> do
          let inputs1 = zipWith appendNumber (map (pN,) hwtys) [0..]
          (itys'',inputs2) <-
            mapAccumLM (\acc (p',o') -> mkTopInput topM acc p' o') itys'
                       (zip (extendPorts ps) inputs1)
          let (ports,decls,ids) = concatPortDecls inputs2
              assigns = zipWith (argBV topM) ids
                          [ Identifier pN' (Just (Indexed (hwty,0,n)))
                          | n <- [0..]]
          return (itys'',(ports,pDecl:assigns ++ decls,Left pN'))

        _ -> return (tail itys',([(pN,pN',hwty)],[pDecl],Left pN'))

-- | Generate output port mappings for the TopEntity
mkTopOutput
  :: Maybe Identifier
  -- ^ (maybe) Name of the _TopEntity_ annotation
  -> [Text]
  -- ^ /Rendered/ output port types
  -> Maybe PortName
  -- ^ (maybe) The @PortName@ of a _TopEntity_ annotation for this output
  -> (Identifier,HWType)
  -> NetlistMonad ([Text]
                  ,([(Identifier,Identifier,HWType)]
                   ,[Declaration]
                   ,Either Identifier (Identifier,HWType))
                  )
mkTopOutput topM otys pM = case pM of
  Nothing -> go otys
  Just p  -> go' p otys
  where
    -- No @PortName@
    go otys' (o,hwty) = do
      o' <- mkUniqueIdentifier o
      let oDecl = NetDecl o' hwty
      case hwty of
        Vector sz hwty' -> do
          let outputs1 = map (appendNumber (o,hwty')) [0..sz-1]
          (otys'',outputs2) <- mapAccumLM go otys' outputs1
          let (ports,decls,ids) = concatPortDecls outputs2
              ids' = map (resBV topM) ids
              netassgn = Assignment o' (mkVectorChain sz hwty' ids')
          return (otys'',(ports,oDecl:netassgn:decls,Left o'))

        RTree d hwty' -> do
          let outputs1 = map (appendNumber (o,hwty')) [0..2^d-1]
          (otys'',outputs2) <- mapAccumLM go otys' outputs1
          let (ports,decls,ids) = concatPortDecls outputs2
              ids' = map (resBV topM) ids
              netassgn = Assignment o' (mkRTreeChain d hwty' ids')
          return (otys'',(ports,oDecl:netassgn:decls,Left o'))

        Product _ hwtys -> do
          let outputs1 = zipWith appendNumber (map (o,) hwtys) [0..]
          (otys'',outputs2) <- mapAccumLM go otys' outputs1
          let (ports,decls,ids) = concatPortDecls outputs2
              ids' = map (resBV topM) ids
              netassgn = Assignment o' (DataCon hwty (DC (hwty,0)) ids')
          return (otys'',(ports,oDecl:netassgn:decls,Left o'))

        _ -> return (tail otys',([(o,o',hwty)],[oDecl],Left o'))

    -- With a @PortName@
    go' (PortName p) (oty:otys') (o,hwty) = do
      let pN = portName p o
      pN' <- mkUniqueIdentifier pN
      return (otys',([(pN,pN',hwty)],[NetDecl' pN' (Left oty)],Right (pN',hwty)))

    go' (PortName _) [] _ = error "This shouldnt happen"

    go' (PortField p ps) otys' (o,hwty) = do
      let pN = portName p o
      pN' <- mkUniqueIdentifier pN
      let pDecl = NetDecl pN' hwty
      case hwty of
        Vector sz hwty' -> do
          let outputs1 = map (appendNumber (pN,hwty')) [0..sz-1]
          (otys'',outputs2) <-
            mapAccumLM (\acc (p',o') -> mkTopOutput topM acc p' o') otys'
                       (zip (extendPorts ps) outputs1)
          let (ports,decls,ids) = concatPortDecls outputs2
              ids' = map (resBV topM) ids
              netassgn = Assignment pN' (mkVectorChain sz hwty' ids')
          return (otys'',(ports,pDecl:netassgn:decls,Left pN'))

        RTree d hwty' -> do
          let outputs1 = map (appendNumber (pN,hwty')) [0..2^d-1]
          (otys'',outputs2) <-
            mapAccumLM (\acc (p',o') -> mkTopOutput topM acc p' o') otys'
                       (zip (extendPorts ps) outputs1)
          let (ports,decls,ids) = concatPortDecls outputs2
              ids' = map (resBV topM) ids
              netassgn = Assignment pN' (mkRTreeChain d hwty' ids')
          return (otys'',(ports,pDecl:netassgn:decls,Left pN'))

        Product _ hwtys -> do
          let outputs1 = zipWith appendNumber (map (pN,) hwtys) [0..]
          (otys'',outputs2) <-
            mapAccumLM (\acc (p',o') -> mkTopOutput topM acc p' o') otys'
                       (zip (extendPorts ps) outputs1)
          let (ports,decls,ids) = concatPortDecls outputs2
              ids' = map (resBV topM) ids
              netassgn = Assignment pN' (DataCon hwty (DC (hwty,0)) ids')
          return (otys'',(ports,pDecl:netassgn:decls,Left pN'))

        _ -> return (tail otys',([(pN,pN',hwty)],[pDecl],Left pN'))

concatPortDecls
  :: [([(Identifier,Identifier,HWType)]
      ,[Declaration]
      ,Either Identifier (Identifier,HWType))]
  -> ([(Identifier,Identifier,HWType)]
     ,[Declaration]
     ,[Either Identifier (Identifier,HWType)])
concatPortDecls portDecls = case unzip3 portDecls of
  (ps,decls,ids) -> (concat ps, concat decls, ids)
