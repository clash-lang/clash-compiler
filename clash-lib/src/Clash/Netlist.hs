{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2018, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Create Netlists out of normalized CoreHW Terms
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Netlist where

import           Control.Exception                (throw)
import           Control.Lens                     ((.=))
import qualified Control.Lens                     as Lens
import           Control.Monad                    (join)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Reader             (runReaderT)
import           Control.Monad.State.Strict       (State, runStateT)
import           Data.Binary.IEEE754              (floatToWord, doubleToWord)
import           Data.Char                        (ord)
import           Data.Either                      (partitionEithers)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMapS
import qualified Data.HashMap.Lazy                as HashMap
import           Data.List                        (elemIndex, partition, sortOn)
import           Data.Maybe
  (catMaybes, listToMaybe, mapMaybe, fromMaybe)
import qualified Data.Set                         as Set
import           Data.Primitive.ByteArray         (ByteArray (..))
import qualified Data.Text                        as StrictText
import qualified Data.Vector.Primitive            as PV
import           GHC.Integer.GMP.Internals        (Integer (..), BigNat (..))
import           System.FilePath                  ((</>), (<.>))
import           Text.Read                        (readMaybe)

import           Outputable                       (ppr, showSDocUnsafe)
import           SrcLoc                           (isGoodSrcSpan)

import           Clash.Annotations.Primitive      (extractPrim)
import           Clash.Annotations.BitRepresentation.ClashLib
  (coreToType')
import           Clash.Annotations.BitRepresentation.Internal
  (CustomReprs, DataRepr'(..), ConstrRepr'(..), getDataRepr, getConstrRepr)
import           Clash.Annotations.TopEntity      (TopEntity (..))
import           Clash.Core.DataCon               (DataCon (..))
import           Clash.Core.Literal               (Literal (..))
import           Clash.Core.Name                  (Name(..))
import           Clash.Core.Pretty                (showPpr)
import           Clash.Core.Term
  (Alt, Pat (..), Term (..), TickInfo (..), PrimInfo(primName), collectArgs, collectArgsTicks, collectTicks)
import qualified Clash.Core.Term                  as Core
import           Clash.Core.Type
  (Type (..), coreView1, splitFunTys, splitCoreFunForallTy)
import           Clash.Core.TyCon                 (TyConMap)
import           Clash.Core.Util
  (mkApps, mkTicks, splitShouldSplit, stripTicks, termType)
import           Clash.Core.Var                   (Id, Var (..))
import           Clash.Core.VarEnv
  (VarEnv, eltsVarEnv, emptyInScopeSet, emptyVarEnv, extendVarEnv, lookupVarEnv,
   lookupVarEnv', mkVarEnv)
import           Clash.Driver.Types               (BindingMap, Binding(..), ClashOpts (..))
import           Clash.Netlist.BlackBox
import           Clash.Netlist.Id
import           Clash.Netlist.Types              as HW
import           Clash.Netlist.Util
import           Clash.Primitives.Types           as P
import           Clash.Util

-- | Generate a hierarchical netlist out of a set of global binders with
-- @topEntity@ at the top.
genNetlist
  :: Bool
  -- ^ Whether this we're compiling a testbench (suppresses certain warnings)
  -> ClashOpts
  -- ^ Options Clash was called with
  -> CustomReprs
  -- ^ Custom bit representations for certain types
  -> BindingMap
  -- ^ Global binders
  -> [TopEntityT]
  -- ^ All the TopEntities
  -> CompiledPrimMap
  -- ^ Primitive definitions
  -> TyConMap
  -- ^ TyCon cache
  -> (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -- ^ Hardcoded Type -> HWType translator
  -> Int
  -- ^ Int/Word/Integer bit-width
  -> (IdType -> Identifier -> Identifier)
  -- ^ valid identifiers
  -> (IdType -> Identifier -> Identifier -> Identifier)
  -- ^ extend valid identifiers
  -> Bool
  -- ^ Whether the backend supports ifThenElse expressions
  -> SomeBackend
  -- ^ The current HDL backend
  -> HashMap Identifier Word
  -- ^ Seen components
  -> FilePath
  -- ^ HDL dir
  -> (Maybe Identifier,Maybe Identifier)
  -- ^ Component name prefix
  -> Id
  -- ^ Name of the @topEntity@
  -> IO ([([Bool],SrcSpan,HashMap Identifier Word,Component)],HashMap Identifier Word)
genNetlist isTb opts reprs globals tops primMap tcm typeTrans iw mkId extId ite be seen env prefixM topEntity = do
  (_,s) <- runNetlistMonad isTb opts reprs globals topEntityMap
             primMap tcm typeTrans iw mkId extId ite be seen env prefixM $
             genComponent topEntity
  return ( eltsVarEnv $ _components s
         , _seenComps s
         )
  where
    topEntityMap :: VarEnv TopEntityT
    topEntityMap = mkVarEnv (zip (map topId tops) tops)

-- | Run a NetlistMonad action in a given environment
runNetlistMonad
  :: Bool
  -- ^ Whether this we're compiling a testbench (suppresses certain warnings)
  -> ClashOpts
  -- ^ Options Clash was called with
  -> CustomReprs
  -- ^ Custom bit representations for certain types
  -> BindingMap
  -- ^ Global binders
  -> VarEnv TopEntityT
  -- ^ TopEntity annotations
  -> CompiledPrimMap
  -- ^ Primitive Definitions
  -> TyConMap
  -- ^ TyCon cache
  -> (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -- ^ Hardcode Type -> HWType translator
  -> Int
  -- ^ Int/Word/Integer bit-width
  -> (IdType -> Identifier -> Identifier)
  -- ^ valid identifiers
  -> (IdType -> Identifier -> Identifier -> Identifier)
  -- ^ extend valid identifiers
  -> Bool
  -- ^ Whether the backend supports ifThenElse expressions
  -> SomeBackend
  -- ^ The current HDL backend
  -> HashMap Identifier Word
  -- ^ Seen components
  -> FilePath
  -- ^ HDL dir
  -> (Maybe Identifier,Maybe Identifier)
  -- ^ Component name prefix
  -> NetlistMonad a
  -- ^ Action to run
  -> IO (a, NetlistState)
runNetlistMonad isTb opts reprs s tops p tcm typeTrans iw mkId extId ite be seenIds_ env prefixM
  = flip runReaderT (NetlistEnv "" "" Nothing)
  . flip runStateT s'
  . runNetlist
  where
    s' =
      NetlistState
        s 0 emptyVarEnv p typeTrans tcm (StrictText.empty,noSrcSpan) iw mkId
        extId HashMapS.empty seenIds' Set.empty names tops env 0 prefixM reprs opts isTb ite be
        HashMapS.empty

    (seenIds',names) = genNames (opt_newInlineStrat opts) mkId prefixM seenIds_
                                emptyVarEnv s

genNames :: Bool
         -> (IdType -> Identifier -> Identifier)
         -> (Maybe Identifier,Maybe Identifier)
         -> HashMap Identifier Word
         -> VarEnv Identifier
         -> BindingMap
         -> (HashMap Identifier Word, VarEnv Identifier)
genNames newInlineStrat mkId prefixM s0 m0 = foldr go (s0,m0)
  where
    go b (s,m) =
      let nm' = genComponentName newInlineStrat s mkId prefixM (bindingId b)
          s'  = HashMapS.insert nm' 0 s
          m'  = extendVarEnv (bindingId b) nm' m
      in (s', m')

-- | Generate a component for a given function (caching)
genComponent
  :: HasCallStack
  => Id
  -- ^ Name of the function
  -> NetlistMonad ([Bool],SrcSpan,HashMap Identifier Word,Component)
genComponent compName = do
  compExprM <- lookupVarEnv compName <$> Lens.use bindings
  case compExprM of
    Nothing -> do
      (_,sp) <- Lens.use curCompNm
      throw (ClashException sp ($(curLoc) ++ "No normalized expression found for: " ++ show compName) Nothing)
    Just b -> do
      makeCachedU compName components $ genComponentT compName (bindingTerm b)

-- | Generate a component for a given function
genComponentT
  :: HasCallStack
  => Id
  -- ^ Name of the function
  -> Term
  -- ^ Corresponding term
  -> NetlistMonad ([Bool],SrcSpan,HashMap Identifier Word,Component)
genComponentT compName componentExpr = do
  varCount .= 0
  componentName1 <- (`lookupVarEnv'` compName) <$> Lens.use componentNames
  topEntMM <- fmap topAnnotation . lookupVarEnv compName <$> Lens.use topEntityAnns
  prefixM <- Lens.use componentPrefix
  let componentName2 = case (prefixM,join topEntMM) of
                         ((Just p,_),Just ann) -> p `StrictText.append` StrictText.pack ('_':t_name ann)
                         (_,Just ann) -> StrictText.pack (t_name ann)
                         _ -> componentName1
  sp <- (bindingLoc . (`lookupVarEnv'` compName)) <$> Lens.use bindings
  curCompNm .= (componentName2,sp)

  tcm <- Lens.use tcCache

  -- HACK: Determine resulttype of this function by looking at its definition
  -- in topEntityAnns, instead of looking at its last binder (which obscure
  -- any attributes [see: Clash.Annotations.SynthesisAttributes]).
  topEntityTypeM <- lookupVarEnv compName <$> Lens.use topEntityAnns
  let topEntityTypeM' = snd . splitCoreFunForallTy tcm . varType . topId <$> topEntityTypeM

  seenIds .= HashMapS.empty
  (wereVoids,compInps,argWrappers,compOutps,resUnwrappers,binders,resultM) <-
    case splitNormalized tcm componentExpr of
      Right (args, binds, res) -> do
        let varType'   = fromMaybe (varType res) topEntityTypeM'
        mkUniqueNormalized emptyInScopeSet topEntMM ((args, binds, res{varType=varType'}))
      Left err ->
        throw (ClashException sp err Nothing)

  netDecls <- fmap catMaybes . mapM mkNetDecl $ filter (maybe (const True) (/=) resultM . fst) binders
  decls    <- concat <$> mapM (uncurry mkDeclarations) binders

  case resultM of
    Just result -> do
      Just (NetDecl' _ rw _ _ rIM) <- mkNetDecl . head $ filter ((==result) . fst) binders

      let (compOutps',resUnwrappers') = case compOutps of
            [oport] -> ([(rw,oport,rIM)],resUnwrappers)
            _       -> let NetDecl n res resTy = head resUnwrappers
                       in  (map (Wire,,Nothing) compOutps
                           ,NetDecl' n rw res (Right resTy) Nothing:tail resUnwrappers
                           )
          component      = Component componentName2 compInps compOutps'
                             (netDecls ++ argWrappers ++ decls ++ resUnwrappers')
      ids <- Lens.use seenIds
      return (wereVoids, sp, ids, component)
    -- No result declaration means that the result is empty, this only happens
    -- when the TopEntity has an empty result. We just create an empty component
    -- in this case.
    Nothing -> do
      let component = Component componentName2 compInps [] (netDecls ++ argWrappers ++ decls)
      ids <- Lens.use seenIds
      return (wereVoids, sp, ids, component)

mkNetDecl :: (Id, Term) -> NetlistMonad (Maybe Declaration)
mkNetDecl (id_,tm) = preserveVarEnv $ do
  let typ             = varType id_
  hwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) typ
  wr   <- termToWireOrReg tm
  rIM  <- getResInit (id_,tm)
  if isVoid hwTy
     then return Nothing
     else return . Just $ NetDecl' (addSrcNote sp)
             wr
             (id2identifier id_)
             (Right hwTy)
             rIM

  where
    nm = varName id_
    sp = case tm of {Tick (SrcSpan s) _ -> s; _ -> nameLoc nm}

    termToWireOrReg :: Term -> NetlistMonad WireOrReg
    termToWireOrReg (stripTicks -> Case scrut _ alts0@(_:_:_)) = do
      tcm <- Lens.use tcCache
      let scrutTy = termType tcm scrut
      scrutHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) scrutTy
      ite <- Lens.use backEndITE
      case iteAlts scrutHTy alts0 of
        Just _ | ite -> return Wire
        _ -> return Reg
    termToWireOrReg (collectArgs -> (Prim p,_)) = do
      bbM <- HashMap.lookup (primName p) <$> Lens.use primitives
      case bbM of
        Just (extractPrim -> Just BlackBox {..}) | outputReg -> return Reg
        _ | primName p == "Clash.Explicit.SimIO.mealyIO" -> return Reg
        _ -> return Wire
    termToWireOrReg _ = return Wire

    addSrcNote loc = if isGoodSrcSpan loc
                        then Just (StrictText.pack (showSDocUnsafe (ppr loc)))
                        else Nothing

    -- Set the initialization value of a signal when a primitive wants to set it
    getResInit
      :: (Id,Term) -> NetlistMonad (Maybe Expr)
    getResInit (i,collectArgsTicks -> (k,args,ticks)) = case k of
      Prim p -> extractPrimWarnOrFail (primName p) >>= go (primName p)
      _ -> return Nothing
     where
      go pNm (BlackBox {resultInit = Just nmD}) = withTicks ticks $ \_ -> do
        (bbCtx,_) <- mkBlackBoxContext pNm i args
        (bbTempl,templDecl) <- prepareBlackBox pNm nmD bbCtx
        case templDecl of
          [] -> return (Just (BlackBoxE pNm [] [] [] bbTempl bbCtx False))
          _  -> do
            (_,sloc) <- Lens.use curCompNm
            throw (ClashException sloc
                    (unwords [ $(curLoc)
                             , "signal initialization requires declarations:\n"
                             , show templDecl
                             ])
                    Nothing)
      go _ _ = return Nothing

-- | Generate a list of concurrent Declarations for a let-binder, return an
-- empty list if the bound expression is represented by 0 bits
mkDeclarations
  :: HasCallStack
  => Id
  -- ^ LHS of the let-binder
  -> Term
  -- ^ RHS of the let-binder
  -> NetlistMonad [Declaration]
mkDeclarations = mkDeclarations' Concurrent

-- | Generate a list of Declarations for a let-binder, return an empty list if
-- the bound expression is represented by 0 bits
mkDeclarations'
  :: HasCallStack
  => DeclarationType
  -- ^ Concurrent of sequential declaration
  -> Id
  -- ^ LHS of the let-binder
  -> Term
  -- ^ RHS of the let-binder
  -> NetlistMonad [Declaration]
mkDeclarations' _declType bndr (collectTicks -> (Var v,ticks)) =
  withTicks ticks $ \tickDecls -> do
  mkFunApp (id2identifier bndr) v [] tickDecls

mkDeclarations' _declType _bndr e@(collectTicks -> (Case _ _ [],_)) = do
  (_,sp) <- Lens.use curCompNm
  throw $ ClashException
          sp
          ( unwords [ $(curLoc)
                    , "Not in normal form: Case-decompositions with an"
                    , "empty list of alternatives not supported:\n\n"
                    , showPpr e
                    ])
          Nothing

mkDeclarations' declType bndr (collectTicks -> (Case scrut altTy alts@(_:_:_),ticks)) =
  withTicks ticks $ \tickDecls -> do
  mkSelection declType (CoreId bndr) scrut altTy alts tickDecls

mkDeclarations' declType bndr app = do
  let (appF,args0,ticks) = collectArgsTicks app
      (args,tyArgs) = partitionEithers args0
  case appF of
    Var f
      | null tyArgs -> withTicks ticks (mkFunApp (id2identifier bndr) f args)
      | otherwise   -> do
        (_,sp) <- Lens.use curCompNm
        throw (ClashException sp ($(curLoc) ++ "Not in normal form: Var-application with Type arguments:\n\n" ++ showPpr app) Nothing)
    _ -> do
      (exprApp,declsApp0) <- mkExpr False declType (CoreId bndr) app
      let dstId = id2identifier bndr
          assn  =
            case exprApp of
              Identifier _ Nothing ->
                -- Supplied 'bndr' was used to assign a result to, so we
                -- don't have to manually turn it into a declaration
                []
              Noop ->
                -- Rendered expression rendered a "noop" - a list of
                -- declarations without a result. Used for things like
                -- mealy IO / inline assertions.
                []
              _ ->
                -- Turn returned expression into declaration by assigning
                -- it to 'dstId'
                [Assignment dstId exprApp]
      declsApp1 <- if null declsApp0
                   then withTicks ticks return
                   else pure declsApp0
      return (declsApp1 ++ assn)

-- | Generate a declaration that selects an alternative based on the value of
-- the scrutinee
mkSelection
  :: DeclarationType
  -> NetlistId
  -> Term
  -> Type
  -> [Alt]
  -> [Declaration]
  -> NetlistMonad [Declaration]
mkSelection declType bndr scrut altTy alts0 tickDecls = do
  let dstId = netlistId1 id id2identifier bndr
  tcm <- Lens.use tcCache
  let scrutTy = termType tcm scrut
  scrutHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) scrutTy
  scrutId  <- extendIdentifier Extended dstId "_selection"
  (_,sp) <- Lens.use curCompNm
  ite <- Lens.use backEndITE
  altHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) altTy
  case iteAlts scrutHTy alts0 of
    Just (altT,altF)
      | ite
      , Concurrent <- declType
      -> do
      (scrutExpr,scrutDecls) <- case scrutHTy of
        SP {} -> first (mkScrutExpr sp scrutHTy (fst (last alts0))) <$>
                   mkExpr True declType (NetlistId scrutId scrutTy) scrut
        _ -> mkExpr False declType (NetlistId scrutId scrutTy) scrut
      altTId <- extendIdentifier Extended dstId "_sel_alt_t"
      altFId <- extendIdentifier Extended dstId "_sel_alt_f"
      (altTExpr,altTDecls) <- mkExpr False declType (NetlistId altTId altTy) altT
      (altFExpr,altFDecls) <- mkExpr False declType (NetlistId altFId altTy) altF
      -- This logic (and the same logic a few lines below) is faulty in the
      -- sense that it won't generate "void decls" if the alternatives' type
      -- is void, but the type of the scrut isn't. Ideally, we'd like to pass
      -- a boolean to 'mkExpr' indicating that it should only render "void decls"
      -- but that it should skip any others.
      --
      -- TODO: Fix ^
      if | isVoid altHTy && isVoid scrutHTy
          -> return $! scrutDecls ++ altTDecls ++ altFDecls
         | isVoid altHTy
          -> return $! altTDecls ++ altFDecls
         | otherwise
          -> return $! scrutDecls ++ altTDecls ++ altFDecls ++ tickDecls ++
                [Assignment dstId (IfThenElse scrutExpr altTExpr altFExpr)]
    _ -> do
      reprs <- Lens.use customReprs
      let alts1 = (reorderDefault . reorderCustom tcm reprs scrutTy) alts0
      (scrutExpr,scrutDecls) <- first (mkScrutExpr sp scrutHTy (fst (head alts1))) <$>
                                  mkExpr True declType (NetlistId scrutId scrutTy) scrut
      (exprs,altsDecls)      <- unzip <$> mapM (mkCondExpr scrutHTy) alts1
      case declType of
        Sequential -> do
          -- Assign to the result in every branch
          let (altNets,exprAlts) = unzip (zipWith (altAssign dstId)
                                                  exprs altsDecls)
          return $! scrutDecls ++ tickDecls ++ concat altNets ++
                    [Seq [Branch scrutExpr scrutHTy exprAlts]]
        Concurrent ->
          if | isVoid altHTy && isVoid scrutHTy
              -> return $! concat altsDecls ++ scrutDecls
             | isVoid altHTy
              -> return $! concat altsDecls
             | otherwise
              -> return $! scrutDecls ++ concat altsDecls ++ tickDecls
                    ++ [CondAssignment dstId altHTy scrutExpr scrutHTy exprs]
 where
  mkCondExpr :: HWType -> (Pat,Term) -> NetlistMonad ((Maybe HW.Literal,Expr),[Declaration])
  mkCondExpr scrutHTy (pat,alt) = do
    altId <- extendIdentifier Extended
               (netlistId1 id id2identifier bndr)
               "_sel_alt"
    (altExpr,altDecls) <- mkExpr False declType (NetlistId altId altTy) alt
    (,altDecls) <$> case pat of
      DefaultPat           -> return (Nothing,altExpr)
      DataPat dc _ _ -> return (Just (dcToLiteral scrutHTy (dcTag dc)),altExpr)
      LitPat  (IntegerLiteral i) -> return (Just (NumLit i),altExpr)
      LitPat  (IntLiteral i) -> return (Just (NumLit i), altExpr)
      LitPat  (WordLiteral w) -> return (Just (NumLit w), altExpr)
      LitPat  (CharLiteral c) -> return (Just (NumLit . toInteger $ ord c), altExpr)
      LitPat  (Int64Literal i) -> return (Just (NumLit i), altExpr)
      LitPat  (Word64Literal w) -> return (Just (NumLit w), altExpr)
      LitPat  (NaturalLiteral n) -> return (Just (NumLit n), altExpr)
      _  -> do
        (_,sp) <- Lens.use curCompNm
        throw (ClashException sp ($(curLoc) ++ "Not an integer literal in LitPat:\n\n" ++ showPpr pat) Nothing)

  mkScrutExpr :: SrcSpan -> HWType -> Pat -> Expr -> Expr
  mkScrutExpr sp scrutHTy pat scrutE = case pat of
    DataPat dc _ _ -> let modifier = Just (DC (scrutHTy,dcTag dc - 1))
                      in case scrutE of
                          Identifier scrutId Nothing -> Identifier scrutId modifier
                          _ -> throw (ClashException sp ($(curLoc) ++ "Not in normal form: Not a variable reference or primitive as subject of a case-statement:\n\n" ++ show scrutE) Nothing)
    _ -> scrutE

  altAssign :: Identifier -> (Maybe HW.Literal,Expr) -> [Declaration]
            -> ([Declaration],(Maybe HW.Literal,[Seq]))
  altAssign i (m,expr) ds =
    let (nets,rest) = partition isNet ds
        assn = case expr of { Noop -> []; _ -> [SeqDecl (Assignment i expr)] }
    in  (nets,(m,map SeqDecl rest ++ assn))
   where
    isNet NetDecl' {} = True
    isNet _ = False

-- GHC puts default patterns in the first position, we want them in the
-- last position.
reorderDefault
  :: [(Pat, Term)]
  -> [(Pat, Term)]
reorderDefault ((DefaultPat,e):alts') = alts' ++ [(DefaultPat,e)]
reorderDefault alts'                  = alts'

reorderCustom
  :: TyConMap
  -> CustomReprs
  -> Type
  -> [(Pat, Term)]
  -> [(Pat, Term)]
reorderCustom tcm reprs (coreView1 tcm -> Just ty) alts =
  reorderCustom tcm reprs ty alts
reorderCustom _tcm reprs (coreToType' -> Right typeName) alts =
  case getDataRepr typeName reprs of
    Just (DataRepr' _name _size _constrReprs) ->
      sortOn (patPos reprs . fst) alts
    Nothing ->
      alts
reorderCustom _tcm _reprs _type alts =
  alts

patPos
  :: CustomReprs
  -> Pat
  -> Int
patPos _reprs DefaultPat = -1
patPos _reprs (LitPat _) = 0
patPos reprs pat@(DataPat dataCon _ _) =
  -- We sort data patterns by their syntactical order
  let name = nameOcc $ dcName dataCon in
  case getConstrRepr name reprs of
    Nothing ->
      -- TODO: err
      error $ $(curLoc) ++ (show pat)
    Just (ConstrRepr' _name n _mask _value _anns) ->
      n


-- | Generate a list of Declarations for a let-binder where the RHS is a function application
mkFunApp
  :: HasCallStack
  => Identifier -- ^ LHS of the let-binder
  -> Id -- ^ Name of the applied function
  -> [Term] -- ^ Function arguments
  -> [Declaration] -- ^ Tick declarations
  -> NetlistMonad [Declaration]
mkFunApp dstId fun args tickDecls = do
  topAnns <- Lens.use topEntityAnns
  tcm     <- Lens.use tcCache
  case lookupVarEnv fun topAnns of
    Just topEntity
      | let ty = varType (topId topEntity)
      , let (fArgTys0,fResTy) = splitFunTys tcm ty
      -- Take into account that clocks and stuff are split off from any product
      -- types containing them
      , let fArgTys1 = splitShouldSplit tcm fArgTys0
      , length fArgTys1 == length args
      -> do
        let annM = topAnnotation topEntity
        argHWTys <- mapM (unsafeCoreTypeToHWTypeM' $(curLoc)) fArgTys1
        (argExprs, concat -> argDecls) <- unzip <$>
          mapM (\(e,t) -> mkExpr False Concurrent (NetlistId dstId t) e)
                                 (zip args fArgTys1)

        -- Filter void arguments, but make sure to render their declarations:
        let
          filteredTypeExprs = filter (not . isVoid . fst) (zip argHWTys argExprs)
          (hWTysFiltered, argExprsFiltered) = unzip filteredTypeExprs

        dstHWty  <- unsafeCoreTypeToHWTypeM' $(curLoc) fResTy
        env  <- Lens.use hdlDir
        mkId <- Lens.use mkIdentifierFn
        prefixM <- Lens.use componentPrefix
        newInlineStrat <- opt_newInlineStrat <$> Lens.use clashOpts
        let topName = StrictText.unpack
                      (genTopComponentName newInlineStrat mkId prefixM annM fun)
            modName = takeWhile (/= '.')
                                (StrictText.unpack (nameOcc (varName fun)))
        manFile <- case annM of
          Just _  -> return (env </> ".." </> modName </> topName </> topName <.> "manifest")
          Nothing -> return (env </> topName <.> "manifest")
        Just man <- readMaybe <$> liftIO (readFile manFile)
        instDecls <- mkTopUnWrapper fun annM man (dstId,dstHWty)
                       (zip argExprsFiltered hWTysFiltered)
                       tickDecls
        return (argDecls ++ instDecls)

      | otherwise -> error $ $(curLoc) ++ "under-applied TopEntity"
    _ -> do
      normalized <- Lens.use bindings
      case lookupVarEnv fun normalized of
        Just _ -> do
          (_,_,_,Component compName compInps co _) <- preserveVarEnv $ genComponent fun
          let argTys = map (termType tcm) args
          argHWTys <- mapM coreTypeToHWTypeM' argTys

          (argExprs, concat -> argDecls) <- unzip <$>
            mapM (\(e,t) -> mkExpr False Concurrent (NetlistId dstId t) e)
                 (zip args argTys)

          -- Filter void arguments, but make sure to render their declarations:
          let
            argTypeExprs = zip argHWTys (zip argTys argExprs)
            filteredTypeExprs = filterOnFst (not . isVoidMaybe True) argTypeExprs
            (argTysFiltered, argsFiltered) = unzip filteredTypeExprs

          let compOutp = (\(_,x,_) -> x) <$> listToMaybe co
          if length argTysFiltered == length compInps
            then do
              (argExprs',argDecls') <- (second concat . unzip) <$> mapM (toSimpleVar dstId) (zip argsFiltered argTysFiltered)
              let inpAssigns    = zipWith (\(i,t) e -> (Identifier i Nothing,In,t,e)) compInps argExprs'
                  outpAssign    = case compOutp of
                    Nothing -> []
                    Just (id_,hwtype) -> [(Identifier id_ Nothing,Out,hwtype,Identifier dstId Nothing)]
              instLabel0 <- extendIdentifier Basic compName (StrictText.pack "_" `StrictText.append` dstId)
              instLabel1 <- fromMaybe instLabel0 <$> Lens.view setName
              instLabel2 <- affixName instLabel1
              instLabel3 <- mkUniqueIdentifier Basic instLabel2
              let instDecl = InstDecl Entity Nothing compName instLabel3 [] (outpAssign ++ inpAssigns)
              return (argDecls ++ argDecls' ++ tickDecls ++ [instDecl])
            else error $ $(curLoc) ++ "under-applied normalized function: " ++ showPpr fun
                                   ++ ". Applied arguments: \n\n" ++ showPpr args
                                   ++ "\n\nApplied filtered arguments:\n\n" ++ show argsFiltered
                                   ++ "\n\nComp inputs:\n\n " ++ show compInps
        Nothing -> case args of
          -- TODO: Figure out what to do with zero-width constructs
          [] -> return [Assignment dstId (Identifier (nameOcc $ varName fun) Nothing)]
          _ -> error $ $(curLoc) ++ "Unknown function: " ++ showPpr fun

toSimpleVar :: Identifier
            -> (Expr,Type)
            -> NetlistMonad (Expr,[Declaration])
toSimpleVar _ (e@(Identifier _ _),_) = return (e,[])
toSimpleVar dstId (e,ty) = do
  argNm <- extendIdentifier Extended
             dstId
             "_fun_arg"
  argNm' <- mkUniqueIdentifier Extended argNm
  hTy <- unsafeCoreTypeToHWTypeM' $(curLoc) ty
  let argDecl         = NetDecl Nothing argNm' hTy
      argAssn         = Assignment argNm' e
  return (Identifier argNm' Nothing,[argDecl,argAssn])

-- | Generate an expression for a term occurring on the RHS of a let-binder
mkExpr :: HasCallStack
       => Bool -- ^ Treat BlackBox expression as declaration
       -> DeclarationType
       -- ^ Should the returned declarations be concurrent or sequential?
       -> NetlistId -- ^ Id to assign the result to
       -> Term -- ^ Term to convert to an expression
       -> NetlistMonad (Expr,[Declaration]) -- ^ Returned expression and a list of generate BlackBox declarations
mkExpr _ _ _ (stripTicks -> Core.Literal l) = do
  iw <- Lens.use intWidth
  case l of
    IntegerLiteral i -> return (HW.Literal (Just (Signed iw,iw)) $ NumLit i, [])
    IntLiteral i     -> return (HW.Literal (Just (Signed iw,iw)) $ NumLit i, [])
    WordLiteral w    -> return (HW.Literal (Just (Unsigned iw,iw)) $ NumLit w, [])
    Int64Literal i   -> return (HW.Literal (Just (Signed 64,64)) $ NumLit i, [])
    Word64Literal w  -> return (HW.Literal (Just (Unsigned 64,64)) $ NumLit w, [])
    CharLiteral c    -> return (HW.Literal (Just (Unsigned 21,21)) . NumLit . toInteger $ ord c, [])
    FloatLiteral r   -> let f = fromRational r :: Float
                            i = toInteger (floatToWord f)
                        in  return (HW.Literal (Just (BitVector 32,32)) (NumLit i), [])
    DoubleLiteral r  -> let d = fromRational r :: Double
                            i = toInteger (doubleToWord d)
                        in  return (HW.Literal (Just (BitVector 64,64)) (NumLit i), [])
    NaturalLiteral n -> return (HW.Literal (Just (Unsigned iw,iw)) $ NumLit n, [])
    ByteArrayLiteral (PV.Vector _ _ (ByteArray ba)) -> return (HW.Literal Nothing (NumLit (Jp# (BN# ba))),[])
    _ -> error $ $(curLoc) ++ "not an integer or char literal"

mkExpr bbEasD declType bndr app =
 let (appF,args,ticks) = collectArgsTicks app
     (tmArgs,tyArgs) = partitionEithers args
 in  withTicks ticks $ \tickDecls -> do
  hwTys  <- mapM (unsafeCoreTypeToHWTypeM' $(curLoc)) (netlistTypes bndr)
  (_,sp) <- Lens.use curCompNm
  let hwTyA = head hwTys
  case appF of
    Data dc -> mkDcApplication hwTys bndr dc tmArgs
    Prim pInfo -> mkPrimitive False bbEasD bndr pInfo args tickDecls
    Var f
      | null tmArgs ->
          if isVoid hwTyA then
            return (Noop, [])
          else
            return (Identifier (nameOcc $ varName f) Nothing, [])
      | not (null tyArgs) ->
          throw (ClashException sp ($(curLoc) ++ "Not in normal form: "
            ++ "Var-application with Type arguments:\n\n" ++ showPpr app) Nothing)
      | otherwise -> do
          argNm0 <- extendIdentifier Extended (netlistId1 id id2identifier bndr)
                                     "_fun_arg"
          argNm1 <- mkUniqueIdentifier Extended argNm0
          decls  <- mkFunApp argNm1 f tmArgs tickDecls
          if isVoid hwTyA then
            return (Noop, decls)
          else
            return ( Identifier argNm1 Nothing
                   , NetDecl' Nothing Wire argNm1 (Right hwTyA) Nothing:decls)
    Case scrut ty' [alt] -> mkProjection bbEasD bndr scrut ty' alt
    Case scrut tyA alts -> do
      tcm <- Lens.use tcCache
      let scrutTy = termType tcm scrut
      scrutHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) scrutTy
      ite <- Lens.use backEndITE
      let wr = case iteAlts scrutHTy alts of
                 Just _ | ite -> Wire
                 _ -> Reg
      argNm0 <- extendIdentifier Extended (netlistId1 id id2identifier bndr) "_sel_arg"
      argNm1 <- mkUniqueIdentifier Extended argNm0
      decls  <- mkSelection declType (NetlistId argNm1 (netlistTypes1 bndr))
                            scrut tyA alts tickDecls
      if isVoid hwTyA then
        return (Noop, decls)
      else
        return ( Identifier argNm1 Nothing
               , NetDecl' Nothing wr argNm1 (Right hwTyA) Nothing:decls)
    Letrec binders body -> do
      netDecls <- fmap catMaybes $ mapM mkNetDecl binders
      decls    <- concat <$> mapM (uncurry mkDeclarations) binders
      (bodyE,bodyDecls) <- mkExpr bbEasD declType bndr (mkApps (mkTicks body ticks) args)
      return (bodyE,netDecls ++ decls ++ bodyDecls)
    _ -> throw (ClashException sp ($(curLoc) ++ "Not in normal form: application of a Lambda-expression\n\n" ++ showPpr app) Nothing)

-- | Generate an expression that projects a field out of a data-constructor.
--
-- Works for both product types, as sum-of-product types.
mkProjection
  :: Bool
  -- ^ Projection must bind to a simple variable
  -> NetlistId
  -- ^ The signal to which the projection is (potentially) assigned
  -> Term
  -- ^ The subject/scrutinee of the projection
  -> Type
  -- ^ The type of the result
  -> Alt
  -- ^ The field to be projected
  -> NetlistMonad (Expr, [Declaration])
mkProjection mkDec bndr scrut altTy alt@(pat,v) = do
  tcm <- Lens.use tcCache
  let scrutTy = termType tcm scrut
      e = Case scrut scrutTy [alt]
  (_,sp) <- Lens.use curCompNm
  varTm <- case v of
    (Var n) -> return n
    _ -> throw (ClashException sp ($(curLoc) ++
                "Not in normal form: RHS of case-projection is not a variable:\n\n"
                 ++ showPpr e) Nothing)
  sHwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) scrutTy
  vHwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) altTy
  scrutRendered <- do
    scrutNm <-
      netlistId1
        return
        (\b -> extendIdentifier Extended (id2identifier b) "_projection")
        bndr
    (scrutExpr,newDecls) <- mkExpr False Concurrent (NetlistId scrutNm scrutTy) scrut
    case scrutExpr of
      Identifier newId modM ->
        pure (Right (newId, modM, newDecls))
      Noop ->
        -- Scrutinee was a zero-width / void construct. We need to render its
        -- declarations, but it's no use assigning it to a new variable.
        -- TODO: Figure out whether we need to render alternatives too.
        -- TODO: seems useless?
        pure (Left newDecls)
      _ -> do
        scrutNm' <- mkUniqueIdentifier Extended scrutNm
        let scrutDecl = NetDecl Nothing scrutNm' sHwTy
            scrutAssn = Assignment scrutNm' scrutExpr
        pure (Right (scrutNm', Nothing, newDecls ++ [scrutDecl, scrutAssn]))

  case scrutRendered of
    Left newDecls -> pure (Noop, newDecls)
    Right (selId, modM, decls) -> do
      let altVarId = nameOcc (varName varTm)
      modifier <- case pat of
        DataPat dc exts tms -> do
          let
            tms' =
              if bindsExistentials exts tms then
                throw (ClashException sp ($(curLoc)
                  ++ "Not in normal form: Pattern binds existential variables:\n\n"
                  ++ showPpr e) Nothing)
              else
                tms
          argHWTys <- mapM coreTypeToHWTypeM' (map varType tms)
          let tmsBundled   = zip argHWTys tms'
              tmsFiltered  = filter (maybe False (not . isVoid) . fst) tmsBundled
              tmsFiltered' = map snd tmsFiltered
          case elemIndex varTm {varType = altTy} tmsFiltered' of
            Nothing -> pure Nothing
            Just fI
              | sHwTy /= vHwTy ->
                pure $ nestModifier modM (Just (Indexed (sHwTy,dcTag dc - 1,fI)))
              -- When element and subject have the same HW-type,
              -- then the projections is just the identity
              | otherwise ->
                pure $ nestModifier modM (Just (DC (Void Nothing,0)))
        _ -> throw (ClashException sp ($(curLoc)
               ++ "Not in normal form: Unexpected pattern in case-projection:\n\n"
               ++ showPpr e) Nothing)
      let extractExpr = Identifier (maybe altVarId (const selId) modifier) modifier
      case bndr of
        NetlistId scrutNm _ | mkDec -> do
          scrutNm' <- mkUniqueIdentifier Extended scrutNm
          let scrutDecl = NetDecl Nothing scrutNm' vHwTy
              scrutAssn = Assignment scrutNm' extractExpr
          return (Identifier scrutNm' Nothing,scrutDecl:scrutAssn:decls)
        MultiId {} -> error "mkProjection: MultiId"
        _ -> return (extractExpr,decls)
  where
    nestModifier Nothing  m          = m
    nestModifier m Nothing           = m
    nestModifier (Just m1) (Just m2) = Just (Nested m1 m2)

-- | Generate an expression for a DataCon application occurring on the RHS of a let-binder
mkDcApplication
    :: HasCallStack
    => [HWType]
    -- ^ HWType of the LHS of the let-binder, can multiple types when we're
    -- creating a "split" product type (e.g. a tuple of a Clock and Reset)
    -> NetlistId
    -- ^ Id to assign the result to
    -> DataCon
    -- ^ Applied DataCon
    -> [Term]
    -- ^ DataCon Arguments
    -> NetlistMonad (Expr,[Declaration])
    -- ^ Returned expression and a list of generate BlackBox declarations
mkDcApplication [dstHType] bndr dc args = do
  let dcNm = nameOcc (dcName dc)
  tcm <- Lens.use tcCache
  let argTys = map (termType tcm) args
  argNm <- netlistId1 return (\b -> extendIdentifier Extended (nameOcc (varName b)) "_dc_arg") bndr
  argHWTys <- mapM coreTypeToHWTypeM' argTys

  (argExprs, concat -> argDecls) <- unzip <$>
    mapM (\(e,t) -> mkExpr False Concurrent (NetlistId argNm t) e) (zip args argTys)

  -- Filter void arguments, but make sure to render their declarations:
  let
    filteredTypeExprDecls =
      filter
        (not . isVoidMaybe True . fst)
        (zip argHWTys argExprs)

    (hWTysFiltered, argExprsFiltered) = unzip filteredTypeExprDecls

  fmap (,argDecls) $! case (hWTysFiltered,argExprsFiltered) of
    -- Is the DC just a newtype wrapper?
    ([Just argHwTy],[argExpr]) | argHwTy == dstHType ->
      return (HW.DataCon dstHType (DC (Void Nothing,-1)) [argExpr])
    _ -> case dstHType of
      SP _ dcArgPairs -> do
        let dcI      = dcTag dc - 1
            dcArgs   = snd $ indexNote ($(curLoc) ++ "No DC with tag: " ++ show dcI) dcArgPairs dcI
        case compare (length dcArgs) (length argExprsFiltered) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,dcI)) argExprsFiltered)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"
      Product _ _ dcArgs ->
        case compare (length dcArgs) (length argExprsFiltered) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,0)) argExprsFiltered)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"
      CustomProduct _ _ _ _ dcArgs ->
        case compare (length dcArgs) (length argExprsFiltered) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,0)) argExprsFiltered)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"
      Sum _ _ ->
        return (HW.DataCon dstHType (DC (dstHType,dcTag dc - 1)) [])
      CustomSP _ _ _ dcArgsTups -> do
        -- Safely get item from list, or err with note
        let dcI    = dcTag dc - 1
        let note   = $(curLoc) ++ "No DC with tag: " ++ show dcI
        let argTup = indexNote note dcArgsTups dcI
        let (_, _, dcArgs) = argTup

        case compare (length dcArgs) (length argExprsFiltered) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType, dcI)) argExprsFiltered)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"

      CustomSum _ _ _ _ ->
        return (HW.DataCon dstHType (DC (dstHType, dcTag dc - 1)) [])
      Bool ->
        let dc' = case dcTag dc of
                   1  -> HW.Literal Nothing (BoolLit False)
                   2  -> HW.Literal Nothing (BoolLit True)
                   tg -> error $ $(curLoc) ++ "unknown bool literal: " ++ showPpr dc ++ "(tag: " ++ show tg ++ ")"
        in  return dc'
      Vector 0 _ -> return (HW.DataCon dstHType VecAppend [])
      Vector 1 _ -> case argExprsFiltered of
                      [e] -> return (HW.DataCon dstHType VecAppend [e])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `Cons`: " ++ showPpr args
      Vector _ _ -> case argExprsFiltered of
                      [e1,e2] -> return (HW.DataCon dstHType VecAppend [e1,e2])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `Cons`: " ++ showPpr args
      RTree 0 _ -> case argExprsFiltered of
                      [e] -> return (HW.DataCon dstHType RTreeAppend [e])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `LR`: " ++ showPpr args
      RTree _ _ -> case argExprsFiltered of
                      [e1,e2] -> return (HW.DataCon dstHType RTreeAppend [e1,e2])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `BR`: " ++ showPpr args
      String ->
        let dc' = case dcTag dc of
                    1 -> HW.Literal Nothing (StringLit "")
                    _ -> error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,dcTag dc,args,argHWTys)
        in  return dc'
      Void {} -> return Noop
      Signed _
        | dcNm == "GHC.Integer.Type.S#"
        -> pure (head argExprsFiltered)
        -- ByteArray# are non-translatable / void, except when they're literals
        | dcNm == "GHC.Integer.Type.Jp#"
        , HW.Literal Nothing (NumLit _) <- head argExprs
        -> pure (head argExprs)
        | dcNm == "GHC.Integer.Type.Jn#"
        -- ByteArray# are non-translatable / void, except when they're literals
        , HW.Literal Nothing (NumLit i) <- head argExprs
        -> pure (HW.Literal Nothing (NumLit (negate i)))
      Unsigned _
        | dcNm == "GHC.Natural.NatS#"
        -> pure (head argExprsFiltered)
        | dcNm == "GHC.Natural.NatJ#"
        -- ByteArray# are non-translatable / void, except when they're literals
        , HW.Literal Nothing (NumLit _) <- head argExprs
        -> pure (head argExprs)
      _ ->
        error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,args,argHWTys)

-- Handle MultiId assignment
mkDcApplication dstHTypes (MultiId argNms) _ args = do
  tcm                 <- Lens.use tcCache
  let argTys          = map (termType tcm) args
  argHWTys            <- mapM coreTypeToHWTypeM' argTys
  -- Filter out the arguments of hwtype `Void` and only translate
  -- them to the intermediate HDL afterwards
  let argsBundled   = zip argHWTys (zipEqual (map CoreId argNms) args)
      (_hWTysFiltered,argsFiltered) = unzip
        (filter (maybe True (not . isVoid) . fst) argsBundled)
  (argExprs,argDecls) <- fmap (second concat . unzip) $!
                         mapM (uncurry (mkExpr False Concurrent)) argsFiltered
  if length dstHTypes == length argExprs then do
    let assns = mapMaybe
                  (\case (_,Noop) -> Nothing
                         (dstId,e) -> let nm = netlistId1 id id2identifier dstId
                                      in  case e of
                                            Identifier nm0 Nothing
                                              | nm == nm0 -> Nothing
                                            _ -> Just (Assignment nm e))
                  (zip (map CoreId argNms) argExprs)
    return (Noop,argDecls ++ assns)
  else
    error "internal error"

mkDcApplication _ _ _ _ = error "internal error"
