{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd
                    2017-2018, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utilities for converting Core Type/Term to Netlist datatypes
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Netlist.Util where

import           Control.Error           (hush)
import           Control.Exception       (throw)
import           Control.Lens            ((.=),(%=))
import qualified Control.Lens            as Lens
import           Control.Monad           (unless, when, zipWithM, join)
import           Control.Monad.Trans.Except (runExcept)
import           Data.Either             (partitionEithers)
import           Data.String             (fromString)
import           Data.List               (intersperse, unzip4, sort, intercalate)
import qualified Data.List               as List
import           Data.Maybe              (catMaybes,fromMaybe)
import           Text.Printf             (printf)
import           Data.Semigroup          ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Prettyprint.Doc (Doc)

import           SrcLoc                  (SrcSpan)

import           Clash.Annotations.BitRepresentation.ClashLib
  (coreToType')
import           Clash.Annotations.BitRepresentation.Internal
  (CustomReprs, ConstrRepr'(..), DataRepr'(..), getDataRepr, getConstrRepr)
import           Clash.Annotations.TopEntity (PortName (..), TopEntity (..))
import           Clash.Driver.Types      (Manifest (..))
import           Clash.Core.DataCon      (DataCon (..))
import           Clash.Core.FreeVars     (termFreeIds, typeFreeVars)
import           Clash.Core.Name
  (Name (..), appendToName, nameOcc)
import           Clash.Core.Pretty       (showPpr)
import           Clash.Core.Subst
  (Subst (..), extendIdSubst, extendIdSubstList, extendInScopeId,
   extendInScopeIdList, extendTvSubstList, mkSubst, substTm, substTy, substTyWith)
import           Clash.Core.Term         (LetBinding, Term (..))
import           Clash.Core.TyCon
  (TyConName, TyConMap, tyConDataCons)
import           Clash.Core.Type         (Type (..), TypeView (..), LitTy (..),
                                          coreView, splitTyConAppM, tyView)
import           Clash.Core.Util         (collectBndrs, termType, tyNatSize)
import           Clash.Core.Var          (Id, Var (..), mkId, modifyVarName, Attr')
import           Clash.Core.VarEnv
  (emptyVarSet, extendInScopeSetList, mkInScopeSet, unionVarSet, uniqAway,
   unitVarSet, mkVarSet)
import           Clash.Netlist.Id        (IdType (..), stripDollarPrefixes)
import           Clash.Netlist.Types     as HW
import           Clash.Signal.Internal   (ClockKind (..))
import           Clash.Unique
import           Clash.Util

isVoid :: HWType -> Bool
isVoid Void {} = True
isVoid _       = False

isBiSignalOut :: HWType -> Bool
isBiSignalOut (Void (Just (BiDirectional Out _))) = True
isBiSignalOut (Vector n ty) | n /= 0              = isBiSignalOut ty
isBiSignalOut (RTree _ ty)                        = isBiSignalOut ty
isBiSignalOut _                                   = False

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
splitNormalized
  :: TyConMap
  -> Term
  -> (Either String ([Id],[LetBinding],Id))
splitNormalized tcm expr = case collectBndrs expr of
  (args,Letrec xes e)
    | (tmArgs,[]) <- partitionEithers args -> case e of
        Var v -> Right (tmArgs,xes,v)
        _     -> Left ($(curLoc) ++ "Not in normal form: res not simple var")
    | otherwise -> Left ($(curLoc) ++ "Not in normal form: tyArgs")
  _ ->
    Left ($(curLoc) ++ "Not in normal form: no Letrec:\n\n" ++ showPpr expr ++
          "\n\nWhich has type:\n\n" ++ showPpr ty)
 where
  ty = termType tcm expr

-- | Converts a Core type to a HWType given a function that translates certain
-- builtin types. Errors if the Core type is not translatable.
unsafeCoreTypeToHWType
  :: SrcSpan
  -> String
  -> (CustomReprs -> TyConMap -> Bool -> Type -> Maybe (Either String HWType))
  -> CustomReprs
  -> TyConMap
  -> Bool
  -> Type
  -> HWType
unsafeCoreTypeToHWType sp loc builtInTranslation reprs m keepVoid =
  either (\msg -> throw (ClashException sp (loc ++ msg) Nothing)) id .
  coreTypeToHWType builtInTranslation reprs m keepVoid

-- | Converts a Core type to a HWType within the NetlistMonad; errors on failure
unsafeCoreTypeToHWTypeM
  :: String
  -> Type
  -> NetlistMonad HWType
unsafeCoreTypeToHWTypeM loc ty =
  unsafeCoreTypeToHWType
    <$> (snd <$> Lens.use curCompNm)
    <*> pure loc
    <*> Lens.use typeTranslator
    <*> Lens.use customReprs
    <*> Lens.use tcCache
    <*> pure False
    <*> pure ty

-- | Converts a Core type to a HWType within the NetlistMonad; 'Nothing' on failure
coreTypeToHWTypeM
  :: Type
  -> NetlistMonad (Maybe HWType)
coreTypeToHWTypeM ty = hush <$> (coreTypeToHWType <$> Lens.use typeTranslator
                                                  <*> Lens.use customReprs
                                                  <*> Lens.use tcCache
                                                  <*> pure False
                                                  <*> pure ty)

-- | Returns the name and period of the clock corresponding to a type
synchronizedClk
  :: TyConMap
  -- ^ TyCon cache
  -> Type
  -> Maybe (Identifier,Integer)
synchronizedClk tcm ty
  | not . null . Lens.toListOf typeFreeVars $ ty = Nothing
  | Just (tcNm,args) <- splitTyConAppM ty
  = case nameOcc tcNm of
      "Clash.Sized.Vector.Vec"       -> synchronizedClk tcm (args!!1)
      "Clash.Signal.Internal.SClock" -> case splitTyConAppM (head args) of
        Just (_,[LitTy (SymTy s),litTy])
          | Right i <- runExcept (tyNatSize tcm litTy) -> Just (Text.pack s,i)
        _ -> error $ $(curLoc) ++ "Clock period not a simple literal: " ++ showPpr ty
      "Clash.Signal.Internal.Signal" -> case splitTyConAppM (head args) of
        Just (_,[LitTy (SymTy s),litTy])
          | Right i <- runExcept (tyNatSize tcm litTy) -> Just (Text.pack s,i)
        _ -> error $ $(curLoc) ++ "Clock period not a simple literal: " ++ showPpr ty
      _ -> case tyConDataCons (tcm `lookupUniqMap'` tcNm) of
             [dc] -> let argTys   = dcArgTys dc
                         argTVs   = dcUnivTyVars dc
                         -- argSubts = zip argTVs args
                         args'    = map (substTyWith argTVs args) argTys
                     in case args' of
                        (arg:_) -> synchronizedClk tcm arg
                        _ -> Nothing
             _    -> Nothing
  | otherwise
  = Nothing

packSP
  :: CustomReprs
  -> (Text, c)
  -> (ConstrRepr', Text, c)
packSP reprs (name, tys) =
  case getConstrRepr name reprs of
    Just repr -> (repr, name, tys)
    Nothing   -> error $ $(curLoc) ++ unwords
      [ "Could not find custom representation for", Text.unpack name ]

packSum
  :: CustomReprs
  -> Text
  -> (ConstrRepr', Text)
packSum reprs name =
  case getConstrRepr name reprs of
    Just repr -> (repr, name)
    Nothing   -> error $ $(curLoc) ++ unwords
      [ "Could not find custom representation for", Text.unpack name ]

fixCustomRepr
  :: CustomReprs
  -> Type
  -> HWType
  -> HWType
fixCustomRepr reprs (coreToType' -> Right tyName) sum_@(Sum name subtys) =
  case getDataRepr tyName reprs of
    Just dRepr@(DataRepr' name' size constrs) ->
      if length constrs == length subtys then
        CustomSum
          name
          dRepr
          (fromIntegral size)
          [packSum reprs ty | ty <- subtys]
      else
        error $ $(curLoc) ++ (Text.unpack $ Text.unwords
          [ "Type "
          , Text.pack $ show name'
          , "has"
          , Text.pack $ show $ length subtys
          , "constructors: \n\n"
          , Text.intercalate "\n" $ sort [Text.append " * " id_ | id_ <- subtys]
          , "\n\nBut the custom bit representation only specified"
          , Text.pack $ show $ length constrs
          , "constructors:\n\n"
          , Text.intercalate "\n" $ sort [Text.append " * " id_ | (ConstrRepr' id_ _ _ _ _) <- constrs]
          ])
    Nothing ->
      -- No custom representation found
      sum_

fixCustomRepr reprs (coreToType' -> Right tyName) sp@(SP name subtys) =
  case getDataRepr tyName reprs of
    Just dRepr@(DataRepr' name' size constrs) ->
      if length constrs == length subtys then
        CustomSP
          name
          dRepr
          (fromIntegral size)
          [packSP reprs ty | ty <- subtys]
      else
        error $ $(curLoc) ++ (Text.unpack $ Text.unwords
          [ "Type "
          , Text.pack $ show $ name'
          , "has"
          , Text.pack $ show $ length subtys
          , "constructors: \n\n"
          , Text.intercalate "\n" $ sort [Text.append " * " id_ | (id_, _) <- subtys]
          , "\n\nBut the custom bit representation only specified"
          , Text.pack $ show $ length constrs, "constructors:\n\n"
          , Text.intercalate "\n" $ sort [Text.append " * " id_ | (ConstrRepr' id_ _ _ _ _) <- constrs]
          ])
    Nothing ->
      -- No custom representation found
      sp

fixCustomRepr _ _ typ = typ

-- | Converts a Core type to a HWType given a function that translates certain
-- builtin types. Returns a string containing the error message when the Core
-- type is not translatable.
coreTypeToHWType
  :: (CustomReprs -> TyConMap -> Bool -> Type -> Maybe (Either String HWType))
  -> CustomReprs
  -> TyConMap
  -> Bool
  -> Type
  -> Either String HWType
coreTypeToHWType builtInTranslation reprs m keepVoid ty = go' ty
  where
    -- Try builtin translation; for now this is hardcoded to be the one in ghcTypeToHWType
    go' :: Type -> Either String HWType
    go' (builtInTranslation reprs m keepVoid -> Just hty) =
      fixCustomRepr reprs ty <$> hty
    -- Strip transparant types:
    go' (coreView m -> Just ty') =
      coreTypeToHWType builtInTranslation reprs m keepVoid ty'
    -- Try to create hwtype based on AST:
    go' (tyView -> TyConApp tc args) = do
      hwty <- mkADT builtInTranslation reprs m (showPpr ty) keepVoid tc args
      return $ fixCustomRepr reprs ty hwty
    -- All methods failed:
    go' _ = Left $ "Can't translate non-tycon type: " ++ showPpr ty


-- | Converts an algebraic Core type (split into a TyCon and its argument) to a HWType.
mkADT
  :: (CustomReprs -> TyConMap -> Bool -> Type -> Maybe (Either String HWType))
  -- ^ Hardcoded Type -> HWType translator
  -> CustomReprs
  -> TyConMap
  -- ^ TyCon cache
  -> String
  -- ^ String representation of the Core type for error messages
  -> Bool
  -- ^ Keep Void
  -> TyConName
  -- ^ The TyCon
  -> [Type]
  -- ^ Its applied arguments
  -> Either String HWType
mkADT _ _ m tyString _ tc _
  | isRecursiveTy m tc
  = Left $ $(curLoc) ++ "Can't translate recursive type: " ++ tyString

mkADT builtInTranslation reprs m _tyString keepVoid tc args = case tyConDataCons (m `lookupUniqMap'` tc) of
  []  -> return (Void Nothing)
  dcs -> do
    let tcName       = nameOcc tc
        substArgTyss = map substArgTys dcs
    argHTyss         <- mapM (mapM (coreTypeToHWType builtInTranslation reprs m keepVoid)) substArgTyss
    let argHTyss1    = if keepVoid
                          then argHTyss
                          else map (filter (not . isVoid)) argHTyss
    case (dcs,argHTyss1) of
      (_:[],[[elemTy]]) ->
        return elemTy

      (_:[],[elemTys@(_:_)]) ->
        return $ Product tcName elemTys

      (_   ,concat -> [])
        | length dcs < 2 ->
          return (Void Nothing)
        | otherwise ->
          return $ Sum tcName $ map (nameOcc . dcName) dcs

      (_   ,elemHTys) ->
        return $ SP tcName $ zipWith
          (\dc tys ->  ( nameOcc (dcName dc), tys))
          dcs elemHTys
 where
  argsFVs = List.foldl' unionVarSet emptyVarSet
                (map (Lens.foldMapOf typeFreeVars unitVarSet) args)

  substArgTys dc =
    let univTVs = dcUnivTyVars dc
        extTVs  = dcExtTyVars dc
        is      = mkInScopeSet (argsFVs `unionVarSet` mkVarSet extTVs)
        -- See Note [The substitution invariant]
        subst   = extendTvSubstList (mkSubst is) (univTVs `zipEqual` args)
    in  map (substTy subst) (dcArgTys dc)

-- | Simple check if a TyCon is recursively defined.
isRecursiveTy :: TyConMap -> TyConName -> Bool
isRecursiveTy m tc = case tyConDataCons (m `lookupUniqMap'` tc) of
    []  -> False
    dcs -> let argTyss      = map dcArgTys dcs
               argTycons    = (map fst . catMaybes) $ (concatMap . map) splitTyConAppM argTyss
           in tc `elem` argTycons

-- | Determines if a Core type is translatable to a HWType given a function that
-- translates certain builtin types.
representableType
  :: (CustomReprs -> TyConMap -> Bool -> Type -> Maybe (Either String HWType))
  -> CustomReprs
  -> Bool
  -- ^ String considered representable
  -> TyConMap
  -> Type
  -> Bool
representableType builtInTranslation reprs stringRepresentable m =
    either (const False) isRepresentable . coreTypeToHWType builtInTranslation reprs m False
  where
    isRepresentable hty = case hty of
      String          -> stringRepresentable
      Vector _ elTy   -> isRepresentable elTy
      RTree  _ elTy   -> isRepresentable elTy
      Product _ elTys -> all isRepresentable elTys
      SP _ elTyss     -> all (all isRepresentable . snd) elTyss
      BiDirectional _ t -> isRepresentable t
      Annotated _ ty  -> isRepresentable ty
      _               -> True

-- | Determines the bitsize of a type
typeSize :: HWType
         -> Int
typeSize (Void {}) = 0
typeSize String = 1
typeSize Bool = 1
typeSize Bit = 1
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
typeSize (Sum _ dcs) = fromMaybe 0 . clogBase 2 . toInteger $ length dcs
typeSize (Product _ tys) = sum $ map typeSize tys
typeSize (BiDirectional In h) = typeSize h
typeSize (BiDirectional Out _) = 0
typeSize (CustomSP _ _ size _) = fromIntegral size
typeSize (CustomSum _ _ size _) = fromIntegral size
typeSize (Annotated _ ty) = typeSize ty

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
  m <- Lens.use tcCache
  let ty = termType m e
  unsafeCoreTypeToHWTypeM loc ty

-- | Gives the HWType corresponding to a term. Returns 'Nothing' if the term has
-- a Core type that is not translatable to a HWType.
termHWTypeM :: Term
            -> NetlistMonad (Maybe HWType)
termHWTypeM e = do
  m  <- Lens.use tcCache
  let ty = termType m e
  coreTypeToHWTypeM ty

isBiSignalIn :: HWType -> Bool
isBiSignalIn (BiDirectional In _) = True
isBiSignalIn _                    = False

containsBiSignalIn
  :: HWType
  -> Bool
containsBiSignalIn (BiDirectional In _) = True
containsBiSignalIn (Product _ tys) = any containsBiSignalIn tys
containsBiSignalIn (SP _ tyss)     = any (any containsBiSignalIn . snd) tyss
containsBiSignalIn (Vector _ ty)   = containsBiSignalIn ty
containsBiSignalIn (RTree _ ty)    = containsBiSignalIn ty
containsBiSignalIn _               = False

-- | Helper function of @collectPortNames@, which operates on a @PortName@
-- instead of a TopEntity.
collectPortNames'
  :: [String]
  -> PortName
  -> [Identifier]
collectPortNames' prefixes (PortName nm) =
  let prefixes' = reverse (nm : prefixes) in
  [fromString (intercalate "_" prefixes')]
collectPortNames' prefixes (PortProduct "" nms) =
  concatMap (collectPortNames' prefixes) nms
collectPortNames' prefixes (PortProduct prefix nms) =
  concatMap (collectPortNames' (prefix : prefixes)) nms

-- | Recursively get all port names from top entity annotations. The result is
-- a list of user defined port names, which should not be used by routines
-- generating unique function names. Only completely qualified names are
-- returned, as it does not (and cannot) account for any implicitly named ports
-- under a PortProduct.
collectPortNames
  :: TopEntity
  -> [Identifier]
collectPortNames TestBench {} = []
collectPortNames Synthesize { t_inputs, t_output } =
  concatMap (collectPortNames' []) t_inputs ++ (collectPortNames' []) t_output

-- | Uniquely rename all the variables and their references in a normalized
-- term
mkUniqueNormalized
  :: Maybe (Maybe TopEntity)
  -> ( [Id]
     , [LetBinding]
     , Id
     )
  -> NetlistMonad
      ([(Identifier,HWType)]
      ,[Declaration]
      ,[(Identifier,HWType)]
      ,[Declaration]
      ,[LetBinding]
      ,Maybe Id)
mkUniqueNormalized topMM (args,binds,res) = do
  -- Add user define port names to list of seen ids to prevent name collisions.
  let
    portNames =
      case join topMM of
        Nothing  -> []
        Just top -> collectPortNames top

  seenIds %= (portNames++)

  let (bndrs,exprs) = unzip binds

  -- Make arguments unique
  is0 <- (`extendInScopeSetList` (args ++ bndrs)) <$> Lens.use globalInScope
  (iports,iwrappers,substArgs) <- mkUniqueArguments (mkSubst is0) topMM args

  -- Make result unique. This might yield 'Nothing' in which case the result
  -- was a single BiSignalOut. This is superfluous in the HDL, as the argument
  -- will already contain a bidirectional signal complementing the BiSignalOut.
  resM <- mkUniqueResult substArgs topMM res
  case resM of
    Just (oports,owrappers,res1,substRes) -> do
      let usesOutput = concatMap (filter ( == res)
                                         . Lens.toListOf termFreeIds
                                         ) exprs
      -- If the let-binder carrying the result is used in a feedback loop
      -- rename the let-binder to "<X>_rec", and assign the "<X>_rec" to
      -- "<X>". We do this because output ports in most HDLs cannot be read.
      (res2,subst'',extraBndr) <- case usesOutput of
        [] -> return (varName res1
                     ,substRes
                     ,[] :: [(Id, Term)])
        _  -> do
          ([res3],substRes') <- mkUnique substRes [modifyVarName (`appendToName` "_rec") res]
          return (varName res3,substRes'
                 ,[(res1, Var res3)])
      -- Replace occurences of "<X>" by "<X>_rec"
      let resN    = varName res
          bndrs'  = map (\i -> if varName i == resN then modifyVarName (const res2) i else i) bndrs
          (bndrsL,r:bndrsR) = break ((== res2).varName) bndrs'
      -- Make let-binders unique
      (bndrsL',substL) <- mkUnique subst'' bndrsL
      (bndrsR',substR) <- mkUnique substL  bndrsR
      -- Replace old IDs by updated unique IDs in the RHSs of the let-binders
      let exprs' = map (substTm ("mkUniqueNormalized1" :: Doc ()) substR) exprs
      -- Return the uniquely named arguments, let-binders, and result
      return (iports,iwrappers,oports,owrappers,zip (bndrsL' ++ r:bndrsR') exprs' ++ extraBndr,Just res1)
    Nothing -> do
      (bndrs', substArgs') <- mkUnique substArgs bndrs
      return (iports,iwrappers,[],[],zip bndrs' (map (substTm ("mkUniqueNormalized2" :: Doc ()) substArgs') exprs),Nothing)

mkUniqueArguments
  :: Subst
  -> Maybe (Maybe TopEntity)
  -> [Id]
  -> NetlistMonad
       ([(Identifier,HWType)]
       ,[Declaration]
       ,Subst
       )
mkUniqueArguments subst0 Nothing args = do
  (args',subst1) <- mkUnique subst0 args
  ports <- mapM idToInPort args'
  return (catMaybes ports,[],subst1)

mkUniqueArguments subst0 (Just teM) args = do
  let iPortSupply = maybe (repeat Nothing) (extendPorts . t_inputs) teM
  (ports,decls,subst) <- unzip3 . catMaybes <$> zipWithM go iPortSupply args
  let ports' = concat ports
  return ( ports', concat decls
         , extendInScopeIdList (extendIdSubstList subst0 (map snd subst))
                               (map fst subst))
  where
    go pM var = do
      tcm       <- Lens.use tcCache
      typeTrans <- Lens.use typeTranslator
      reprs     <- Lens.use customReprs
      (_,sp)    <- Lens.use curCompNm
      let i    = varName var
          i'   = nameOcc i
          ty   = varType var
          hwty = unsafeCoreTypeToHWType sp $(curLoc) typeTrans reprs tcm True ty
      (ports,decls,_,pN) <- mkInput pM (i',hwty)
      let pId  = mkId ty (repName pN i)
      if isVoid hwty
         then return Nothing
         else return (Just (ports,decls,(pId,(var,Var pId))))


mkUniqueResult
  :: Subst
  -> Maybe (Maybe TopEntity)
  -> Id
  -> NetlistMonad (Maybe ([(Identifier,HWType)],[Declaration],Id,Subst))
mkUniqueResult subst0 Nothing res = do
  ([res'],subst1) <- mkUnique subst0 [res]
  portM <- idToOutPort res'
  case portM of
    Just port -> return (Just ([port],[],res',subst1))
    _         -> return Nothing

mkUniqueResult subst0 (Just teM) res = do
  tcm       <- Lens.use tcCache
  typeTrans <- Lens.use typeTranslator
  reprs     <- Lens.use customReprs
  (_,sp)    <- Lens.use curCompNm
  let o    = varName res
      o'   = nameOcc o
      ty   = varType res
      hwty = unsafeCoreTypeToHWType sp $(curLoc) typeTrans reprs tcm True ty
      oPortSupply = fmap t_output teM
  when (containsBiSignalIn hwty)
    (throw (ClashException sp ($(curLoc) ++ "BiSignalIn cannot be part of a function's result. Use 'readFromBiSignal'.") Nothing))
  output <- mkOutput oPortSupply (o',hwty)
  case output of
    Just (ports, decls, pN) -> do
      let pO = repName pN o
          pOId = mkId ty pO
          subst1 = extendInScopeId (extendIdSubst subst0 res (Var pOId)) pOId
      return (Just (ports,decls,pOId,subst1))
    _ -> return Nothing

-- | Same as idToPort, but
--    * Throws an error if the port is a composite type with a BiSignalIn
idToInPort :: Id -> NetlistMonad (Maybe (Identifier,HWType))
idToInPort var = do
  (_, sp) <- Lens.use curCompNm
  portM <- idToPort var
  case portM of
    Just (_,hty) -> do
      when (containsBiSignalIn hty && not (isBiSignalIn hty))
        (throw (ClashException sp ($(curLoc) ++ "BiSignalIn currently cannot be part of a composite type when it's a function's argument") Nothing))
      return portM
    _ -> return Nothing

-- | Same as idToPort, but:
--    * Throws an error if port is of type BiSignalIn
idToOutPort :: Id -> NetlistMonad (Maybe (Identifier,HWType))
idToOutPort var = do
  (_, srcspan) <- Lens.use curCompNm
  portM <- idToPort var
  case portM of
    Just (_,hty) -> do
      when (containsBiSignalIn hty)
        (throw (ClashException srcspan ($(curLoc) ++ "BiSignalIn cannot be part of a function's result. Use 'readFromBiSignal'.") Nothing))
      return portM
    _ -> return Nothing

idToPort :: Id -> NetlistMonad (Maybe (Identifier,HWType))
idToPort var = do
  tcm <- Lens.use tcCache
  typeTrans <- Lens.use typeTranslator
  (_,sp) <- Lens.use curCompNm
  reprs <- Lens.use customReprs
  let i  = varName var
      ty = varType var
      hwTy = unsafeCoreTypeToHWType sp $(curLoc) typeTrans reprs tcm False ty
  if isVoid hwTy
    then return Nothing
    else return . Just $
      ( nameOcc i
      , unsafeCoreTypeToHWType sp $(curLoc) typeTrans reprs tcm False ty
      )

id2type :: Id -> Type
id2type = varType

id2identifier :: Id -> Identifier
id2identifier = nameOcc . varName

repName :: Text -> Name a -> Name a
repName s (Name sort' _ i loc) = Name sort' s i loc

-- | Make a set of IDs unique; also returns a substitution from old ID to new
-- updated unique ID.
mkUnique
  :: Subst
  -- ^ Existing substitution
  -> [Id]
  -- ^ IDs to make unique
  -> NetlistMonad ([Id],Subst)
  -- ^ (Unique IDs, update substitution)
mkUnique = go []
  where
    go :: [Id] -> Subst -> [Id] -> NetlistMonad ([Id],Subst)
    go processed subst []     = return (reverse processed,subst)
    go processed subst@(Subst isN _ _) (i:is) = do
      iN <- mkUniqueIdentifier Extended (id2identifier i)
      let i' = uniqAway isN (modifyVarName (repName iN) i)
          subst' = extendInScopeId (extendIdSubst subst i (Var i')) i'
      go (i':processed)
         subst'
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
      i' <- extendIdentifier typ i (Text.pack ('_':show n))
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

portName
  :: String
  -> Identifier
  -> Identifier
portName [] i = i
portName x  _ = Text.pack x

-- | Prefix given string before portnames /except/ when this string is empty.
prefixParent :: String -> PortName -> PortName
prefixParent ""     p                   = p
prefixParent parent (PortName p)        = PortName (parent <> "_" <> p)
prefixParent parent (PortProduct "" ps) = PortProduct parent ps
prefixParent parent (PortProduct p ps)  = PortProduct (parent <> "_" <> p) ps


appendIdentifier
  :: (Identifier,HWType)
  -> Int
  -> NetlistMonad (Identifier,HWType)
appendIdentifier (nm,hwty) i =
  (,hwty) <$> extendIdentifier Extended nm (Text.pack ('_':show i))

uniquePortName
  :: String
  -> Identifier
  -> NetlistMonad Identifier
uniquePortName [] i = mkUniqueIdentifier Extended i
uniquePortName x  _ = do
  let x' = Text.pack x
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
    -- No PortName given, infer names
    go (i,hwty) = do
      i' <- mkUniqueIdentifier Extended i
      let (attrs, hwty') = stripAttributes hwty
      case hwty' of
        Vector sz hwty'' -> do
          arguments <- mapM (appendIdentifier (i',hwty'')) [0..sz-1]
          (ports,_,exprs,_) <- unzip4 <$> mapM (mkInput Nothing) arguments
          let hwty2    = filterVoid hwty''
              netdecl  = NetDecl Nothing i' (Vector sz hwty2)
              vecExpr  = mkVectorChain sz hwty2 exprs
              netassgn = Assignment i' vecExpr
          if null attrs then
            return (concat ports,[netdecl,netassgn],vecExpr,i')
          else
            throwAnnotatedSplitError $(curLoc) "Vector"

        RTree d hwty'' -> do
          arguments <- mapM (appendIdentifier (i',hwty'')) [0..2^d-1]
          (ports,_,exprs,_) <- unzip4 <$> mapM (mkInput Nothing) arguments
          let hwty2    = filterVoid hwty''
              netdecl  = NetDecl Nothing i' (RTree d hwty2)
              trExpr   = mkRTreeChain d hwty2 exprs
              netassgn = Assignment i' trExpr
          if null attrs then
            return (concat ports,[netdecl,netassgn],trExpr,i')
          else
            throwAnnotatedSplitError $(curLoc) "RTree"

        Product _ hwtys -> do
          arguments <- zipWithM appendIdentifier (map (i',) hwtys) [0..]
          let argumentsBundled   = zip hwtys arguments
              argumentsFiltered  = filter (not . isVoid . fst) argumentsBundled
              argumentsFiltered' = map snd argumentsFiltered
          (ports,_,exprs,_) <- unzip4 <$> mapM (mkInput Nothing) argumentsFiltered'
          case exprs of
            [expr] ->
              let hwty''   = filterVoid hwty
                  netdecl  = NetDecl Nothing i' hwty''
                  dcExpr   = expr
                  netassgn = Assignment i' expr
              in  return (concat ports,[netdecl,netassgn],dcExpr,i')
            _ ->
              let hwty''   = filterVoid hwty
                  netdecl  = NetDecl Nothing i' hwty''
                  dcExpr   = DataCon hwty'' (DC (hwty'',0)) exprs
                  netassgn = Assignment i' dcExpr
              in  if null attrs then
                    return (concat ports,[netdecl,netassgn],dcExpr,i')
                  else
                    throwAnnotatedSplitError $(curLoc) "Product"

        Clock nm rt Gated -> do
          let hwtys = [Clock nm rt Source,Bool]
          arguments <- zipWithM appendIdentifier (map (i',) hwtys) [0..]
          (ports,_,exprs,_) <- unzip4 <$> mapM (mkInput Nothing) arguments
          let netdecl  = NetDecl Nothing i' hwty
              dcExpr   = DataCon hwty (DC (hwty,0)) exprs
              netassgn = Assignment i' dcExpr
          return (concat ports,[netdecl,netassgn],dcExpr,i')

        _ -> return ([(i',hwty)],[],Identifier i' Nothing,i')


    -- PortName specified by user
    go' (PortName p) (i,hwty) = do
      pN <- uniquePortName p i
      return ([(pN,hwty)],[],Identifier pN Nothing,pN)

    go' (PortProduct p ps) (i,hwty) = do
      pN <- uniquePortName p i
      let (attrs, hwty') = stripAttributes hwty
      case hwty' of
        Vector sz hwty'' -> do
          arguments <- mapM (appendIdentifier (pN,hwty'')) [0..sz-1]
          (ports,_,exprs,_) <- unzip4 <$> zipWithM mkInput (extendPorts $ map (prefixParent p) ps) arguments
          let hwty2    = filterVoid hwty''
              netdecl  = NetDecl Nothing pN (Vector sz hwty2)
              vecExpr  = mkVectorChain sz hwty2 exprs
              netassgn = Assignment pN vecExpr
          if null attrs then
            return (concat ports,[netdecl,netassgn],vecExpr,pN)
          else
            throwAnnotatedSplitError $(curLoc) "Vector"

        RTree d hwty'' -> do
          arguments <- mapM (appendIdentifier (pN,hwty'')) [0..2^d-1]
          (ports,_,exprs,_) <- unzip4 <$> zipWithM mkInput (extendPorts $ map (prefixParent p) ps) arguments
          let hwty2    = filterVoid hwty''
              netdecl  = NetDecl Nothing pN (RTree d hwty2)
              trExpr   = mkRTreeChain d hwty2 exprs
              netassgn = Assignment pN trExpr
          if null attrs then
            return (concat ports,[netdecl,netassgn],trExpr,pN)
          else
            throwAnnotatedSplitError $(curLoc) "RTree"

        Product _ hwtys -> do
          arguments <- zipWithM appendIdentifier (map (pN,) hwtys) [0..]
          let argumentsBundled   = zip hwtys (zip (extendPorts $ map (prefixParent p) ps) arguments)
              argumentsFiltered  = filter (not . isVoid . fst) argumentsBundled
              argumentsFiltered' = unzip (map snd argumentsFiltered)
          (ports,_,exprs,_) <- unzip4 <$> uncurry (zipWithM mkInput) argumentsFiltered'
          case exprs of
            [expr] ->
                 let hwty''   = filterVoid hwty'
                     netdecl  = NetDecl Nothing pN hwty''
                     dcExpr   = expr
                     netassgn = Assignment pN expr
                 in  return (concat ports,[netdecl,netassgn],dcExpr,pN)
            _ -> let hwty''   = filterVoid hwty'
                     netdecl  = NetDecl Nothing pN hwty''
                     dcExpr   = DataCon hwty'' (DC (hwty'',0)) exprs
                     netassgn = Assignment pN dcExpr
                 in  if null attrs then
                       return (concat ports,[netdecl,netassgn],dcExpr,pN)
                     else
                       throwAnnotatedSplitError $(curLoc) "Product"

        Clock nm rt Gated -> do
          let hwtys = [Clock nm rt Source, Bool]
          arguments <- zipWithM appendIdentifier (map (pN,) hwtys) [0..]
          (ports,_,exprs,_) <- unzip4 <$> zipWithM mkInput (extendPorts $ map (prefixParent p) ps) arguments
          let netdecl  = NetDecl Nothing pN hwty
              dcExpr   = DataCon hwty (DC (hwty,0)) exprs
              netassgn = Assignment pN dcExpr
          return (concat ports,[netdecl,netassgn],dcExpr,pN)

        _ ->  return ([(pN,hwty)],[],Identifier pN Nothing,pN)

filterVoid
  :: HWType
  -> HWType
filterVoid t = case t of
  Product nm hwtys
    | null hwtys'        -> Void Nothing
    | length hwtys' == 1 -> head hwtys'
    | otherwise          -> Product nm hwtys'
    where
      hwtys' = filter (not . isVoid) (map filterVoid hwtys)
  _ -> t

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

genComponentName
  :: [Identifier]
  -> (IdType -> Identifier -> Identifier)
  -> (Maybe Identifier,Maybe Identifier)
  -> Id
  -> Identifier
genComponentName seen mkIdFn prefixM nm =
  let nm' = Text.splitOn (Text.pack ".") (nameOcc (varName nm))
      fn  = mkIdFn Basic (stripDollarPrefixes (last nm'))
      fn' = if Text.null fn then Text.pack "Component" else fn
      prefix = maybe id (:) (snd prefixM) (init nm')
      nm2 = Text.concat (intersperse (Text.pack "_") (prefix ++ [fn']))
      nm3 = mkIdFn Basic nm2
  in  if nm3 `elem` seen then go 0 nm3 else nm3
  where
    go :: Integer -> Identifier -> Identifier
    go n i =
      let i' = mkIdFn Basic (i `Text.append` Text.pack ('_':show n))
      in  if i' `elem` seen
             then go (n+1) i
             else i'

genTopComponentName
  :: (IdType -> Identifier -> Identifier)
  -> (Maybe Identifier,Maybe Identifier)
  -> Maybe TopEntity
  -> Id
  -> Identifier
genTopComponentName _mkIdFn prefixM (Just ann) _nm =
  case prefixM of
    (Just p,_) -> p `Text.append` Text.pack ('_':t_name ann)
    _          -> Text.pack (t_name ann)
genTopComponentName mkIdFn prefixM Nothing nm =
  genComponentName [] mkIdFn prefixM nm


-- | Strips one or more layers of attributes from a HWType; stops at first
-- non-Annotated. Accumilates all attributes of nested annotations.
stripAttributes
  :: HWType
  -> ([Attr'], HWType)
-- Recursively strip type, accumulate attrs:
stripAttributes (Annotated attrs typ) =
  let (attrs', typ') = stripAttributes typ
  in (attrs ++ attrs', typ')
-- Not an annotated type, so just return it:
stripAttributes typ = ([], typ)

-- | Generate output port mappings
mkOutput
  :: Maybe PortName
  -> (Identifier,HWType)
  -> NetlistMonad (Maybe ([(Identifier,HWType)],[Declaration],Identifier))
mkOutput _pM (_o, (BiDirectional Out _)) = return Nothing
mkOutput _pM (_o, (Void _))  = return Nothing
mkOutput pM  (o,  hwty)      = Just <$> mkOutput' pM (o, hwty)

-- | Generate output port mappings. Will yield Nothing if the only output is
-- Void.
mkOutput'
  :: Maybe PortName
  -> (Identifier,HWType)
  -> NetlistMonad ([(Identifier,HWType)],[Declaration],Identifier)
mkOutput' pM = case pM of
  Nothing -> go
  Just p  -> go' p
  where
    go (o,hwty) = do
      o' <- mkUniqueIdentifier Extended o
      let (attrs, hwty') = stripAttributes hwty
      case hwty' of
        Vector sz hwty'' -> do
          unless (null attrs)
            (throwAnnotatedSplitError $(curLoc) "Vector")
          results <- mapM (appendIdentifier (o',hwty'')) [0..sz-1]
          (ports,decls,ids) <- unzip3 <$> mapM (mkOutput' Nothing) results
          let hwty2   = Vector sz (filterVoid hwty'')
              netdecl = NetDecl Nothing o' hwty2
              assigns = zipWith (assignId o' hwty2 10) ids [0..]
          return (concat ports,netdecl:assigns ++ concat decls,o')

        RTree d hwty'' -> do
          unless (null attrs)
            (throwAnnotatedSplitError $(curLoc) "RTree")
          results <- mapM (appendIdentifier (o',hwty'')) [0..2^d-1]
          (ports,decls,ids) <- unzip3 <$> mapM (mkOutput' Nothing) results
          let hwty2   = RTree d (filterVoid hwty'')
              netdecl = NetDecl Nothing o' hwty2
              assigns = zipWith (assignId o' hwty2 10) ids [0..]
          return (concat ports,netdecl:assigns ++ concat decls,o')

        Product _ hwtys -> do
          results <- zipWithM appendIdentifier (map (o,) hwtys) [0..]
          let resultsBundled   = zip hwtys results
              resultsFiltered  = filter (not . isVoid . fst) resultsBundled
              resultsFiltered' = map snd resultsFiltered
          (ports,decls,ids) <- unzip3 <$> mapM (mkOutput' Nothing) resultsFiltered'
          case ids of
            [i] ->
              let hwty''  = filterVoid hwty
                  netdecl = NetDecl Nothing o' hwty''
                  assign  = Assignment i (Identifier o' Nothing)
              in  return (concat ports,netdecl:assign:concat decls,o')
            _   ->
              let hwty''  = filterVoid hwty
                  netdecl = NetDecl Nothing o' hwty''
                  assigns = zipWith (assignId o' hwty'' 0) ids [0..]
              in  if null attrs then
                     return (concat ports,netdecl:assigns ++ concat decls,o')
                  else
                    throwAnnotatedSplitError $(curLoc) "Product"

        _ -> return ([(o',hwty)],[],o')

    go' (PortName p) (o,hwty) = do
      pN <- uniquePortName p o
      return ([(pN,hwty)],[],pN)

    go' (PortProduct p ps) (o,hwty) = do
      pN <- uniquePortName p o
      let (attrs, hwty') = stripAttributes hwty
      case hwty' of
        Vector sz hwty'' -> do
          unless (null attrs)
            (throwAnnotatedSplitError $(curLoc) "Vector")
          results <- mapM (appendIdentifier (pN,hwty'')) [0..sz-1]
          (ports,decls,ids) <- unzip3 <$> zipWithM mkOutput' (extendPorts $ map (prefixParent p) ps) results
          let hwty2   = Vector sz (filterVoid hwty'')
              netdecl = NetDecl Nothing pN hwty2
              assigns = zipWith (assignId pN hwty2 10) ids [0..]
          return (concat ports,netdecl:assigns ++ concat decls,pN)

        RTree d hwty'' -> do
          unless (null attrs)
            (throwAnnotatedSplitError $(curLoc) "RTree")
          results <- mapM (appendIdentifier (pN,hwty'')) [0..2^d-1]
          (ports,decls,ids) <- unzip3 <$> zipWithM mkOutput' (extendPorts $ map (prefixParent p) ps) results
          let hwty2   = RTree d (filterVoid hwty'')
              netdecl = NetDecl Nothing pN hwty2
              assigns = zipWith (assignId pN hwty2 10) ids [0..]
          return (concat ports,netdecl:assigns ++ concat decls,pN)

        Product _ hwtys -> do
          results <- zipWithM appendIdentifier (map (pN,) hwtys) [0..]
          let resultsBundled   = zip hwtys (zip (extendPorts $ map (prefixParent p) ps) results)
              resultsFiltered  = filter (not . isVoid . fst) resultsBundled
              resultsFiltered' = unzip (map snd resultsFiltered)
          (ports,decls,ids) <- unzip3 <$> uncurry (zipWithM mkOutput') resultsFiltered'
          case ids of
            [i] -> let hwty''  = filterVoid hwty'
                       netdecl = NetDecl Nothing pN hwty''
                       assign  = Assignment i (Identifier pN Nothing)
                   in  return (concat ports,netdecl:assign:concat decls,pN)
            _   -> let hwty''  = filterVoid hwty'
                       netdecl = NetDecl Nothing pN hwty''
                       assigns = zipWith (assignId pN hwty' 0) ids [0..]
                   in  if null attrs then
                         return (concat ports,netdecl:assigns ++ concat decls,pN)
                       else
                         throwAnnotatedSplitError $(curLoc) "Product"

        _ -> return ([(pN,hwty)],[],pN)

    assignId p hwty con i n =
      Assignment i (Identifier p (Just (Indexed (hwty,con,n))))

-- | Instantiate a TopEntity, and add the proper type-conversions where needed
mkTopUnWrapper
  :: Id
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
  mkIdFn <- Lens.use mkIdentifierFn
  prefixM <- Lens.use componentPrefix
  let topName = genTopComponentName mkIdFn prefixM annM topEntity
      topM    = fmap (const topName) annM

  -- inputs
  let iPortSupply = maybe (repeat Nothing)
                        (extendPorts . t_inputs)
                        annM
  arguments <- zipWithM appendIdentifier (map (\a -> ("input",snd a)) args) [0..]
  (_,arguments1) <- mapAccumLM (\acc (p,i) -> mkTopInput topM acc p i)
                      (zip inNames inTys)
                      (zip iPortSupply arguments)
  let (iports,wrappers,idsI) = unzip3 arguments1
      inpAssigns             = zipWith (argBV topM) idsI (map fst args)

  -- output
  let oPortSupply = maybe
                      (repeat Nothing)
                      (extendPorts . (:[]) . t_output)
                      annM

  let iResult = inpAssigns ++ concat wrappers
      result = ("result",snd dstId)

  topOutputM <- mkTopOutput
                  topM
                  (zip outNames outTys)
                  (head oPortSupply)
                  result

  (iResult ++) <$> case topOutputM of
    Nothing -> return []
    Just (_, (oports, unwrappers, idsO)) -> do
        instLabel <- extendIdentifier Basic topName ("_" `Text.append` fst dstId)
        let outpAssign = Assignment (fst dstId) (resBV topM idsO)
        let topCompDecl = InstDecl
                            Entity
                            (Just topName)
                            topName
                            instLabel
                            ( map (\(p,i,t) -> (Identifier p Nothing,In, t,Identifier i Nothing)) (concat iports) ++
                              map (\(p,o,t) -> (Identifier p Nothing,Out,t,Identifier o Nothing)) oports)

        return $ (topCompDecl:unwrappers) ++ [outpAssign]

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
  Clock _ _ Gated -> ConvBV topM hwty b e
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
      let (attrs, hwty') = stripAttributes hwty
      case hwty' of
        Vector sz hwty'' -> do
          arguments <- mapM (appendIdentifier (i',hwty'')) [0..sz-1]
          (inps'',arguments1) <- mapAccumLM go inps' arguments
          let (ports,decls,ids) = unzip3 arguments1
              assigns = zipWith (argBV topM) ids
                          [ Identifier i' (Just (Indexed (hwty,10,n)))
                          | n <- [0..]]
          if null attrs then
            return (inps'',(concat ports,iDecl:assigns++concat decls,Left i'))
          else
            throwAnnotatedSplitError $(curLoc) "Vector"

        RTree d hwty'' -> do
          arguments <- mapM (appendIdentifier (i',hwty'')) [0..2^d-1]
          (inps'',arguments1) <- mapAccumLM go inps' arguments
          let (ports,decls,ids) = unzip3 arguments1
              assigns = zipWith (argBV topM) ids
                          [ Identifier i' (Just (Indexed (hwty,10,n)))
                          | n <- [0..]]
          if null attrs then
            return (inps'',(concat ports,iDecl:assigns++concat decls,Left i'))
          else
            throwAnnotatedSplitError $(curLoc) "RTree"

        Product _ hwtys -> do
          arguments <- zipWithM appendIdentifier (map (i,) hwtys) [0..]
          (inps'',arguments1) <- mapAccumLM go inps' arguments
          let (ports,decls,ids) = unzip3 arguments1
              assigns = zipWith (argBV topM) ids
                          [ Identifier i' (Just (Indexed (hwty,0,n)))
                          | n <- [0..]]
          if null attrs then
            return (inps'',(concat ports,iDecl:assigns++concat decls,Left i'))
          else
            throwAnnotatedSplitError $(curLoc) "Product"

        Clock nm rt Gated -> do
          let hwtys = [Clock nm rt Source,Bool]
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

    go' (PortProduct p ps) inps' (i,hwty) = do
      let pN = portName p i
      pN' <- mkUniqueIdentifier Extended pN
      let pDecl = NetDecl Nothing pN' hwty
      let (attrs, hwty') = stripAttributes hwty
      case hwty' of
        Vector sz hwty'' -> do
          arguments <- mapM (appendIdentifier (pN',hwty'')) [0..sz-1]
          (inps'',arguments1) <-
            mapAccumLM (\acc (p',o') -> mkTopInput topM acc p' o') inps'
                       (zip (extendPorts ps) arguments)
          let (ports,decls,ids) = unzip3 arguments1
              assigns = zipWith (argBV topM) ids
                          [ Identifier pN' (Just (Indexed (hwty,10,n)))
                          | n <- [0..]]
          if null attrs then
            return (inps'',(concat ports,pDecl:assigns ++ concat decls,Left pN'))
          else
            throwAnnotatedSplitError $(curLoc) "Vector"

        RTree d hwty'' -> do
          arguments <- mapM (appendIdentifier (pN',hwty'')) [0..2^d-1]
          (inps'',arguments1) <-
            mapAccumLM (\acc (p',o') -> mkTopInput topM acc p' o') inps'
                       (zip (extendPorts ps) arguments)
          let (ports,decls,ids) = unzip3 arguments1
              assigns = zipWith (argBV topM) ids
                          [ Identifier pN' (Just (Indexed (hwty,10,n)))
                          | n <- [0..]]
          if null attrs then
            return (inps'',(concat ports,pDecl:assigns ++ concat decls,Left pN'))
          else
            throwAnnotatedSplitError $(curLoc) "RTree"

        Product _ hwtys -> do
          arguments <- zipWithM appendIdentifier (map (pN',) hwtys) [0..]
          (inps'',arguments1) <-
            mapAccumLM (\acc (p',o') -> mkTopInput topM acc p' o') inps'
                       (zip (extendPorts ps) arguments)
          let (ports,decls,ids) = unzip3 arguments1
              assigns = zipWith (argBV topM) ids
                          [ Identifier pN' (Just (Indexed (hwty,0,n)))
                          | n <- [0..]]
          if null attrs then
            return (inps'',(concat ports,pDecl:assigns ++ concat decls,Left pN'))
          else
            throwAnnotatedSplitError $(curLoc) "Product"

        Clock nm rt Gated -> do
          let hwtys = [Clock nm rt Source,Bool]
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


-- | Consider the following type signature:
--
-- @
--   f :: Signal dom (Vec 6 A) `Annotate` ConstAttr "keep"
--     -> Signal dom (Vec 6 B)
-- @
--
-- What does the annotation mean, considering that Clash will split these
-- vectors into multiple in- and output ports? Should we apply the annotation
-- to all individual ports? How would we handle pin mappings? For now, we simply
-- throw an error. This is a helper function to do so.
throwAnnotatedSplitError
  :: String
  -> String
  -> NetlistMonad a
throwAnnotatedSplitError loc typ = do
  (_,sp) <- Lens.use curCompNm
  throw $ ClashException sp (loc ++ printf msg typ typ) Nothing
 where
  msg = unwords $ [ "Attempted to split %s into a number of HDL ports. This"
                  , "is not allowed in combination with attribute annotations."
                  , "You can annotate %s's components by splitting it up"
                  , "manually." ]

-- | Generate output port mappings for the TopEntity. Yields /Nothing/ if
-- the output is Void
mkTopOutput
  :: Maybe Identifier
  -- ^ (maybe) Name of the _TopEntity_
  -> [(Identifier,Identifier)]
  -- ^ /Rendered/ output port names and types
  -> Maybe PortName
  -- ^ (maybe) The @PortName@ of a _TopEntity_ annotation for this output
  -> (Identifier,HWType)
  -> NetlistMonad ( Maybe ( [(Identifier, Identifier)]
                          , ( [(Identifier, Identifier, HWType)]
                            , [Declaration]
                            , Either Identifier (Identifier,HWType)
                            )
                          )
                  )
mkTopOutput _topM _outps _pM (_id, BiDirectional Out _) = return Nothing
mkTopOutput _topM _outps _pM (_id, Void _) = return Nothing
mkTopOutput topM outps pM (o, hwty) =
    Just <$> mkTopOutput' topM outps pM (o, hwty)

-- | Generate output port mappings for the TopEntity
mkTopOutput'
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
mkTopOutput' topM outps pM = case pM of
  Nothing -> go outps
  Just p  -> go' p outps
  where
    -- No @PortName@
    go outps'@((oN,_):rest) (o,hwty) = do
      o' <- mkUniqueIdentifier Extended o
      let oDecl = NetDecl Nothing o' hwty
      let (attrs, hwty') = stripAttributes hwty
      case hwty' of
        Vector sz hwty'' -> do
          results <- mapM (appendIdentifier (o',hwty'')) [0..sz-1]
          (outps'',results1) <- mapAccumLM go outps' results
          let (ports,decls,ids) = unzip3 results1
              ids' = map (resBV topM) ids
              netassgn = Assignment o' (mkVectorChain sz hwty'' ids')
          if null attrs then
            return (outps'',(concat ports,oDecl:netassgn:concat decls,Left o'))
          else
            throwAnnotatedSplitError $(curLoc) "Vector"

        RTree d hwty'' -> do
          results <- mapM (appendIdentifier (o',hwty'')) [0..2^d-1]
          (outps'',results1) <- mapAccumLM go outps' results
          let (ports,decls,ids) = unzip3 results1
              ids' = map (resBV topM) ids
              netassgn = Assignment o' (mkRTreeChain d hwty'' ids')
          if null attrs then
            return (outps'',(concat ports,oDecl:netassgn:concat decls,Left o'))
          else
            throwAnnotatedSplitError $(curLoc) "RTree"

        Product _ hwtys -> do
          results <- zipWithM appendIdentifier (map (o',) hwtys) [0..]
          (outps'',results1) <- mapAccumLM go outps' results
          let (ports,decls,ids) = unzip3 results1
              ids' = map (resBV topM) ids
              netassgn = Assignment o' (DataCon hwty (DC (hwty,0)) ids')
          if null attrs then
            return (outps'', (concat ports,oDecl:netassgn:concat decls,Left o'))
          else
            throwAnnotatedSplitError $(curLoc) "Product"

        _ -> return (rest,([(oN,o',hwty)],[oDecl],Left o'))

    go [] _ = error "This shouldn't happen"

    -- With a @PortName@
    go' (PortName _) ((oN,oTy):outps') (_,hwty) = do
      oN' <- mkUniqueIdentifier Extended oN
      return (outps',([(oN,oN',hwty)]
                     ,[NetDecl' Nothing Wire oN' (Left oTy)]
                     ,Right (oN',hwty)))

    go' (PortName _) [] _ = error "This shouldnt happen"

    go' (PortProduct p ps) outps' (o,hwty) = do
      let pN = portName p o
      pN' <- mkUniqueIdentifier Extended pN
      let pDecl = NetDecl Nothing pN' hwty
      let (attrs, hwty') = stripAttributes hwty
      case hwty' of
        Vector sz hwty'' -> do
          results <- mapM (appendIdentifier (pN',hwty'')) [0..sz-1]
          (outps'',results1) <-
            mapAccumLM (\acc (p',o') -> mkTopOutput' topM acc p' o') outps'
                       (zip (extendPorts ps) results)
          let (ports,decls,ids) = unzip3 results1
              ids' = map (resBV topM) ids
              netassgn = Assignment pN' (mkVectorChain sz hwty'' ids')
          if null attrs then
            return (outps'',(concat ports,pDecl:netassgn:concat decls,Left pN'))
          else
            throwAnnotatedSplitError $(curLoc) "Vector"

        RTree d hwty'' -> do
          results <- mapM (appendIdentifier (pN',hwty'')) [0..2^d-1]
          (outps'',results1) <-
            mapAccumLM (\acc (p',o') -> mkTopOutput' topM acc p' o') outps'
                       (zip (extendPorts ps) results)
          let (ports,decls,ids) = unzip3 results1
              ids' = map (resBV topM) ids
              netassgn = Assignment pN' (mkRTreeChain d hwty'' ids')
          if null attrs then
            return (outps'',(concat ports,pDecl:netassgn:concat decls,Left pN'))
          else
            throwAnnotatedSplitError $(curLoc) "RTree"

        Product _ hwtys -> do
          results <- zipWithM appendIdentifier (map (pN',) hwtys) [0..]
          (outps'',results1) <-
            mapAccumLM (\acc (p',o') -> mkTopOutput' topM acc p' o') outps'
                       (zip (extendPorts ps) results)
          let (ports,decls,ids) = unzip3 results1
              ids' = map (resBV topM) ids
              netassgn = Assignment pN' (DataCon hwty (DC (hwty,0)) ids')
          if null attrs then
            return (outps'',(concat ports,pDecl:netassgn:concat decls,Left pN'))
          else
            throwAnnotatedSplitError $(curLoc) "Product"

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

-- | Try to merge nested modifiers into a single modifier, needed by the VHDL
-- and SystemVerilog backend.
nestM :: Modifier -> Modifier -> Maybe Modifier
nestM (Nested a b) m2
  | Just m1  <- nestM a b  = maybe (Just (Nested m1 m2)) Just (nestM m1 m2)
  | Just m2' <- nestM b m2 = maybe (Just (Nested a m2')) Just (nestM a m2')

nestM (Indexed (Vector n t1,1,1)) (Indexed (Vector _ t2,1,0))
  | t1 == t2 = Just (Indexed (Vector n t1,10,1))

nestM (Indexed (Vector n t1,1,1)) (Indexed (Vector _ t2,10,k))
  | t1 == t2 = Just (Indexed (Vector n t1,10,k+1))

nestM (Indexed (RTree d1 t1,1,n)) (Indexed (RTree d2 t2,0,0))
  | t1 == t2
  , d1 >= 0
  , d2 >= 0
  = Just (Indexed (RTree d1 t1,10,n))

nestM (Indexed (RTree d1 t1,1,n)) (Indexed (RTree d2 t2,1,m))
  | t1 == t2
  , d1 >= 0
  , d2 >= 0
  = if | n == 1 && m == 1 -> let r = 2 ^ d1
                                 l = r - (2 ^ (d1-1) `div` 2)
                             in  Just (Indexed (RTree (-1) t1, l, r))
       | n == 1 && m == 0 -> let l = 2 ^ (d1-1)
                                 r = l + (l `div` 2)
                             in  Just (Indexed (RTree (-1) t1, l, r))
       | n == 0 && m == 1 -> let l = (2 ^ (d1-1)) `div` 2
                                 r = 2 ^ (d1-1)
                             in  Just (Indexed (RTree (-1) t1, l, r))
       | n == 0 && m == 0 -> let l = 0
                                 r = (2 ^ (d1-1)) `div` 2
                             in  Just (Indexed (RTree (-1) t1, l, r))
nestM (Indexed (RTree (-1) t1,l,_)) (Indexed (RTree d t2,10,k))
  | t1 == t2
  , d  >= 0
  = Just (Indexed (RTree d t1,10,l+k))

nestM _ _ = Nothing
