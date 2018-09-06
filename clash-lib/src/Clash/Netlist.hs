{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2018, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Create Netlists out of normalized CoreHW Terms
-}

{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

module Clash.Netlist where

import           Control.Exception                (throw)
import           Control.Lens                     ((.=),(^.),_1,_3)
import qualified Control.Lens                     as Lens
import           Control.Monad                    (join)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.State.Strict       (runStateT)
import           Data.Binary.IEEE754              (floatToWord, doubleToWord)
import           Data.Char                        (ord)
import           Data.Either                      (lefts,partitionEithers)
import           Data.HashMap.Lazy                (HashMap)
import qualified Data.HashMap.Lazy                as HashMap
import           Data.List                        (elemIndex, sortOn)
import           Data.Maybe
  (catMaybes, fromMaybe, listToMaybe)
import           Data.Primitive.ByteArray         (ByteArray (..))
import qualified Data.Text                        as StrictText
import qualified Data.Text.Lazy                   as Text
import qualified Data.Vector.Primitive            as PV
import           GHC.Integer.GMP.Internals        (Integer (..), BigNat (..))
import           System.FilePath                  ((</>), (<.>))
import           Text.Read                        (readMaybe)
import           Unbound.Generics.LocallyNameless
  (Embed (..), runFreshMT, unbind, unembed, unrebind, embed)

import           Outputable                       (ppr, showSDocUnsafe)
import           SrcLoc                           (SrcSpan,isGoodSrcSpan,noSrcSpan)

import           Clash.Annotations.BitRepresentation.ClashLib
  (coreToType')
import           Clash.Annotations.BitRepresentation.Internal
  (CustomReprs, DataRepr'(..), ConstrRepr'(..), getDataRepr, getConstrRepr)
import           Clash.Annotations.TopEntity      (TopEntity (..))
import           Clash.Core.DataCon               (DataCon (..))
import           Clash.Core.FreeVars              (typeFreeVars)
import           Clash.Core.Literal               (Literal (..))
import           Clash.Core.Name                  (Name(..), name2String)
import           Clash.Core.Pretty                (showDoc)
import           Clash.Core.Term
  (Alt, Pat (..), Term (..), TmName, TmOccName)
import qualified Clash.Core.Term                  as Core
import           Clash.Core.Type
  (Type (..), coreView, getFunResult, splitFunTys)
import           Clash.Core.TyCon
  (TyCon, TyConOccName)
import           Clash.Core.Util                  (collectArgs, termType)
import           Clash.Core.Var                   (Id, Var (..))
import           Clash.Driver.Types
  (BindingMap, ClashException (..))
import           Clash.Netlist.BlackBox
import           Clash.Netlist.Id
import           Clash.Netlist.Types              as HW
import           Clash.Netlist.Util
import           Clash.Primitives.Types           as P
import           Clash.Util


-- | Generate a hierarchical netlist out of a set of global binders with
-- @topEntity@ at the top.
genNetlist :: CustomReprs
           -> BindingMap
           -- ^ Global binders
           -> [(TmName,Type,Maybe TopEntity,Maybe TmName)]
           -- ^ All the TopEntities
           -> CompiledPrimMap
           -- ^ Primitive definitions
           -> HashMap TyConOccName TyCon
           -- ^ TyCon cache
           -> (CustomReprs -> HashMap TyConOccName TyCon -> Bool -> Type -> Maybe (Either String HWType))
           -- ^ Hardcoded Type -> HWType translator
           -> Int
           -- ^ Int/Word/Integer bit-width
           -> (IdType -> Identifier -> Identifier)
           -- ^ valid identifiers
           -> (IdType -> Identifier -> Identifier -> Identifier)
           -- ^ extend valid identifiers
           -> [Identifier]
           -- ^ Seen components
           -> FilePath
           -- ^ HDL dir
           -> (Maybe Identifier,Maybe Identifier)
           -- ^ Component name prefix
           -> TmOccName
           -- ^ Name of the @topEntity@
           -> IO ([(SrcSpan,[Identifier],Component)],[Identifier])
genNetlist reprs globals tops primMap tcm typeTrans iw mkId extId seen env prefixM topEntity = do
  (_,s) <- runNetlistMonad reprs globals (mkTopEntityMap tops) primMap tcm typeTrans
             iw mkId extId seen env prefixM $ genComponent topEntity
  return ( HashMap.elems $ _components s
         , _seenComps s
         )
  where
    mkTopEntityMap
      :: [(TmName,Type,Maybe TopEntity,Maybe TmName)]
      -> HashMap TmOccName (Type, Maybe TopEntity)
    mkTopEntityMap = HashMap.fromList . map (\(a,b,c,_) -> (nameOcc a,(b,c)))

-- | Run a NetlistMonad action in a given environment
runNetlistMonad :: CustomReprs
                -> BindingMap
                -- ^ Global binders
                -> HashMap TmOccName (Type, Maybe TopEntity)
                -- ^ TopEntity annotations
                -> CompiledPrimMap
                -- ^ Primitive Definitions
                -> HashMap TyConOccName TyCon
                -- ^ TyCon cache
                -> (CustomReprs -> HashMap TyConOccName TyCon -> Bool -> Type -> Maybe (Either String HWType))
                -- ^ Hardcode Type -> HWType translator
                -> Int
                -- ^ Int/Word/Integer bit-width
                -> (IdType -> Identifier -> Identifier)
                -- ^ valid identifiers
                -> (IdType -> Identifier -> Identifier -> Identifier)
                -- ^ extend valid identifiers
                -> [Identifier]
                -- ^ Seen components
                -> FilePath
                -- ^ HDL dir
                -> (Maybe Identifier,Maybe Identifier)
                -- ^ Component name prefix
                -> NetlistMonad a
                -- ^ Action to run
                -> IO (a, NetlistState)
runNetlistMonad reprs s tops p tcm typeTrans iw mkId extId seenIds_ env prefixM
  = runFreshMT
  . flip runStateT s'
  . runNetlist
  where
    s' = NetlistState s 0 HashMap.empty p typeTrans tcm (Text.empty,noSrcSpan) iw mkId extId [] seenIds' names tops env 0 prefixM reprs
    (seenIds',names) = genNames mkId prefixM seenIds_ HashMap.empty (HashMap.elems (HashMap.map (^. _1) s))

genNames :: (IdType -> Identifier -> Identifier)
         -> (Maybe Identifier,Maybe Identifier)
         -> [Identifier]
         -> HashMap TmOccName Identifier
         -> [TmName]
         -> ([Identifier], HashMap TmOccName Identifier)
genNames mkId prefixM = go
  where
    go s m []       = (s,m)
    go s m (nm:nms) = let nm' = genComponentName s mkId prefixM nm
                          s'  = nm':s
                          m'  = HashMap.insert (nameOcc nm) nm' m
                      in  go s' m' nms

-- | Generate a component for a given function (caching)
genComponent
  :: TmOccName
  -- ^ Name of the function
  -> NetlistMonad (SrcSpan,[Identifier],Component)
genComponent compName = do
  compExprM <- fmap (HashMap.lookup compName) $ Lens.use bindings
  case compExprM of
    Nothing -> do
      (_,sp) <- Lens.use curCompNm
      throw (ClashException sp ($(curLoc) ++ "No normalized expression found for: " ++ show compName) Nothing)
    Just (_,_,_,_,expr_) -> do
      makeCached compName components $ genComponentT compName expr_

-- | Generate a component for a given function
genComponentT
  :: TmOccName
  -- ^ Name of the function
  -> Term
  -- ^ Corresponding term
  -> NetlistMonad (SrcSpan,[Identifier],Component)
genComponentT compName componentExpr = do
  varCount .= 0
  componentName1 <- (HashMap.! compName) <$> Lens.use componentNames
  topEntMM <- fmap snd . HashMap.lookup compName <$> Lens.use topEntityAnns
  prefixM <- Lens.use componentPrefix
  let componentName2 = case (prefixM,join topEntMM) of
                         ((Just p,_),Just ann) -> p `Text.append` Text.pack ('_':t_name ann)
                         (_,Just ann) -> Text.pack (t_name ann)
                         _ -> componentName1
  sp <- ((^. _3) . (HashMap.! compName)) <$> Lens.use bindings
  curCompNm .= (componentName2,sp)

  tcm <- Lens.use tcCache

  -- HACK: Determine resulttype of this function by looking at its definition
  -- in topEntityAnns, instead of looking at its last binder (which obscure
  -- any attributes [see: Clash.Annotations.SynthesisAttributes]).
  topEntityType <- ((fst <$>) . HashMap.lookup compName) <$> Lens.use topEntityAnns

  seenIds .= []
  (compInps,argWrappers,compOutps,resUnwrappers,binders,resultM) <- do
    normalizedM <- splitNormalized tcm componentExpr
    case normalizedM of
      Right (args, binds, res) -> do
        let resultType = (\te -> fromMaybe te (getFunResult te)) <$> topEntityType
            varType'   = maybe (varType res) embed resultType
        mkUniqueNormalized topEntMM ((args, binds, res{varType=varType'}))
      Left err ->
        throw (ClashException sp err Nothing)

  netDecls <- fmap catMaybes . mapM mkNetDecl $ filter (maybe (const True) (/=) resultM . varName . fst) binders
  decls    <- concat <$> mapM (uncurry mkDeclarations . second unembed) binders

  case resultM of
    Just result -> do
      Just (NetDecl' _ rw _ _) <- mkNetDecl . head $ filter ((==result) . varName . fst) binders

      let (compOutps',resUnwrappers') = case compOutps of
            [oport] -> ([(rw,oport)],resUnwrappers)
            _       -> let NetDecl n res resTy = head resUnwrappers
                       in  (map (Wire,) compOutps
                           ,NetDecl' n rw res (Right resTy):tail resUnwrappers
                           )
          component      = Component componentName2 compInps compOutps'
                             (netDecls ++ argWrappers ++ decls ++ resUnwrappers')
      ids <- Lens.use seenIds
      return (sp, ids, component)
    -- No result declaration means that the result is empty, this only happens
    -- when the TopEntity has an empty result. We just create an empty component
    -- in this case.
    Nothing -> do
      let component = Component componentName2 compInps [] (netDecls ++ argWrappers ++ decls)
      ids <- Lens.use seenIds
      return (sp, ids, component)

mkNetDecl :: (Id, Embed Term) -> NetlistMonad (Maybe Declaration)
mkNetDecl (id_,tm) = do
  let typ             = unembed (varType id_)
  hwTy <- unsafeCoreTypeToHWTypeM $(curLoc) typ
  wr   <- termToWireOrReg (unembed tm)
  if isVoid hwTy
     then return Nothing
     else return . Just $ NetDecl' (addSrcNote (nameLoc nm))
             wr
             (id2identifier id_)
             (Right hwTy)

  where
    nm = varName id_

    termToWireOrReg :: Term -> NetlistMonad WireOrReg
    termToWireOrReg (Case _ _ (_:_:_)) = return Reg
    termToWireOrReg (collectArgs -> (Prim nm' _,_)) = do
      bbM <- HashMap.lookup nm' <$> Lens.use primitives
      case bbM of
        Just (BlackBox {..}) | outputReg -> return Reg
        _ -> return Wire
    termToWireOrReg _ = return Wire

    addSrcNote loc = if isGoodSrcSpan loc
                        then Just (Text.pack (showSDocUnsafe (ppr loc)))
                        else Nothing


isWriteToBiSignalPrimitive :: Term -> Bool
isWriteToBiSignalPrimitive e = case collectArgs e of
  (Prim nm _,_) -> nm == StrictText.pack "Clash.Signal.BiSignal.writeToBiSignal#"
  _             -> False

-- | Generate a list of Declarations for a let-binder, return an empty list
-- if the bound expression is represented by 0 bits
mkDeclarations
  :: Id
  -- ^ LHS of the let-binder
  -> Term
  -- ^ RHS of the let-binder
  -> NetlistMonad [Declaration]
mkDeclarations bndr e = do
  hty <- unsafeCoreTypeToHWTypeM $(curLoc) (unembed (varType bndr))
  if isVoid hty && not (isBiSignalOut hty)
     then return []
     else mkDeclarations' bndr e

-- | Generate a list of Declarations for a let-binder
mkDeclarations'
  :: Id
  -- ^ LHS of the let-binder
  -> Term
  -- ^ RHS of the let-binder
  -> NetlistMonad [Declaration]
mkDeclarations' bndr (Var _ v) = mkFunApp bndr v []

mkDeclarations' _ e@(Case _ _ []) = do
  (_,sp) <- Lens.use curCompNm
  throw $ ClashException
          sp
          ( unwords [ $(curLoc)
                    , "Not in normal form: Case-decompositions with an"
                    , "empty list of alternatives not supported:\n\n"
                    , showDoc e
                    ])
          Nothing

mkDeclarations' bndr (Case scrut altTy alts@(_:_:_)) =
  mkSelection bndr scrut altTy alts

mkDeclarations' bndr app =
  let (appF,(args,tyArgs)) = second partitionEithers $ collectArgs app
  in case appF of
    Var _ f
      | null tyArgs -> mkFunApp bndr f args
      | otherwise   -> do
        (_,sp) <- Lens.use curCompNm
        throw (ClashException sp ($(curLoc) ++ "Not in normal form: Var-application with Type arguments:\n\n" ++ showDoc app) Nothing)
    -- Do not generate any assignments writing to a BiSignalOut, as these
    -- do not have any significance in a HDL. The single exception occurs
    -- when writing to a BiSignal using the primitive 'writeToBiSignal'. In
    -- the generate HDL it will write to an inout port, NOT the variable
    -- having the actual type BiSignalOut.
    -- _ | isBiSignalOut (id2type bndr) && (not $ isWriteToBiSignalPrimitive app) ->
    --     return []
    _ -> do
      hwTy <- unsafeCoreTypeToHWTypeM $(curLoc) (id2type bndr)
      if isBiSignalOut hwTy && not (isWriteToBiSignalPrimitive app)
         then return []
         else do
          (exprApp,declsApp) <- mkExpr False (Right bndr) (unembed $ varType bndr) app
          let dstId = id2identifier bndr
              assn  = case exprApp of
                        Identifier _ Nothing -> []
                        _ -> [Assignment dstId exprApp]
          return (declsApp ++ assn)

-- | Generate a declaration that selects an alternative based on the value of
-- the scrutinee
mkSelection
  :: Id
  -> Term
  -> Type
  -> [Alt]
  -> NetlistMonad [Declaration]
mkSelection bndr scrut altTy alts = do
  tcm                    <- Lens.use tcCache
  reprs                  <- Lens.use customReprs
  scrutTy                <- termType tcm scrut
  alts'                  <- (reorderDefault . reorderCustom tcm reprs scrutTy)
                            <$> mapM unbind alts
  scrutHTy               <- unsafeCoreTypeToHWTypeM $(curLoc) scrutTy
  altHTy                 <- unsafeCoreTypeToHWTypeM $(curLoc) altTy
  scrutId                <- extendIdentifier Extended
                               (id2identifier bndr)
                               (Text.pack "_selection")
  (_,sp)                 <- Lens.use curCompNm
  (scrutExpr,scrutDecls) <- first (mkScrutExpr sp scrutHTy (fst (head alts'))) <$> mkExpr True (Left scrutId) scrutTy scrut
  (exprs,altsDecls)      <- (second concat . unzip) <$> mapM (mkCondExpr scrutHTy) alts'

  let dstId = id2identifier bndr
  return $! scrutDecls ++ altsDecls ++ [CondAssignment dstId altHTy scrutExpr scrutHTy exprs]
  where
    mkCondExpr :: HWType -> (Pat,Term) -> NetlistMonad ((Maybe HW.Literal,Expr),[Declaration])
    mkCondExpr scrutHTy (pat,alt) = do
      altId <- extendIdentifier Extended
                 (id2identifier bndr)
                 (Text.pack "_sel_alt")
      (altExpr,altDecls) <- mkExpr False (Left altId) altTy alt
      (,altDecls) <$> case pat of
        DefaultPat           -> return (Nothing,altExpr)
        DataPat (Embed dc) _ -> return (Just (dcToLiteral scrutHTy (dcTag dc)),altExpr)
        LitPat  (Embed (IntegerLiteral i)) -> return (Just (NumLit i),altExpr)
        LitPat  (Embed (IntLiteral i)) -> return (Just (NumLit i), altExpr)
        LitPat  (Embed (WordLiteral w)) -> return (Just (NumLit w), altExpr)
        LitPat  (Embed (CharLiteral c)) -> return (Just (NumLit . toInteger $ ord c), altExpr)
        LitPat  (Embed (Int64Literal i)) -> return (Just (NumLit i), altExpr)
        LitPat  (Embed (Word64Literal w)) -> return (Just (NumLit w), altExpr)
        LitPat  (Embed (NaturalLiteral n)) -> return (Just (NumLit n), altExpr)
        _  -> do
          (_,sp) <- Lens.use curCompNm
          throw (ClashException sp ($(curLoc) ++ "Not an integer literal in LitPat:\n\n" ++ showDoc pat) Nothing)

    mkScrutExpr :: SrcSpan -> HWType -> Pat -> Expr -> Expr
    mkScrutExpr sp scrutHTy pat scrutE = case pat of
      DataPat (Embed dc) _ -> let modifier = Just (DC (scrutHTy,dcTag dc - 1))
                              in case scrutE of
                                  Identifier scrutId Nothing -> Identifier scrutId modifier
                                  _ -> throw (ClashException sp ($(curLoc) ++ "Not in normal form: Not a variable reference or primitive as subject of a case-statement:\n\n" ++ show scrutE) Nothing)
      _ -> scrutE

-- GHC puts default patterns in the first position, we want them in the
-- last position.
reorderDefault
  :: [(Pat, Term)]
  -> [(Pat, Term)]
reorderDefault ((DefaultPat,e):alts') = alts' ++ [(DefaultPat,e)]
reorderDefault alts'                  = alts'

reorderCustom
  :: HashMap TyConOccName TyCon
  -> CustomReprs
  -> Type
  -> [(Pat, Term)]
  -> [(Pat, Term)]
reorderCustom tcm reprs (coreView tcm -> Just ty) alts =
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
patPos reprs pat@(DataPat (Embed dataCon) _) =
  -- We sort data patterns by their syntactical order
  let name = Text.pack $ name2String $ dcName dataCon in
  case getConstrRepr name reprs of
    Nothing ->
      -- TODO: err
      error $ $(curLoc) ++ (show pat)
    Just (ConstrRepr' _name n _mask _value _anns) ->
      n


-- | Generate a list of Declarations for a let-binder where the RHS is a function application
mkFunApp
  :: Id -- ^ LHS of the let-binder
  -> TmName -- ^ Name of the applied function
  -> [Term] -- ^ Function arguments
  -> NetlistMonad [Declaration]
mkFunApp dst fun args = do
  topAnns <- Lens.use topEntityAnns
  tcm     <- Lens.use tcCache
  case HashMap.lookup (nameOcc fun) topAnns of
    Just (ty, annM)
      | let (fArgTys,fResTy) = splitFunTys tcm ty
      , length fArgTys == length args
      -> do
        let dstId = id2identifier dst
        argHWTys <- mapM (unsafeCoreTypeToHWTypeM $(curLoc)) fArgTys
        -- Filter out the arguments of hwtype `Void` and only translate them
        -- to the intermediate HDL afterwards
        let argsBundled   = zip argHWTys (zip args fArgTys)
            argsFiltered  = filter (not . isVoid . fst) argsBundled
            argsFiltered' = map snd argsFiltered
            hWTysFiltered = filter (not . isVoid) argHWTys
        (argExprs,argDecls) <- second concat . unzip <$>
                                 mapM (\(e,t) -> mkExpr False (Left dstId) t e)
                                 argsFiltered'
        dstHWty  <- unsafeCoreTypeToHWTypeM $(curLoc) fResTy
        env  <- Lens.use hdlDir
        mkId <- Lens.use mkIdentifierFn
        prefixM <- Lens.use componentPrefix
        let topName = Text.unpack (genTopComponentName mkId prefixM annM fun)
        manFile <- case annM of
          Just _  -> return (env </> topName </> topName <.> "manifest")
          Nothing -> return (env </> topName <.> "manifest")
        Just man <- readMaybe <$> liftIO (readFile manFile)
        instDecls <- mkTopUnWrapper fun annM man (dstId,dstHWty)
                       (zip argExprs hWTysFiltered)
        return (argDecls ++ instDecls)

      | otherwise -> error $ $(curLoc) ++ "under-applied TopEntity"
    _ -> do
      normalized <- Lens.use bindings
      case HashMap.lookup (nameOcc fun) normalized of
        Just _ -> do
          (_,_,Component compName compInps co _) <- preserveVarEnv $ genComponent (nameOcc fun)
          argTys   <- mapM (termType tcm) args
          argHWTys <- mapM coreTypeToHWTypeM argTys
          -- Filter out the arguments of hwtype `Void` and only translate
          -- them to the intermediate HDL afterwards
          let argsBundled   = zip argHWTys (zip args argTys)
              argsFiltered  = filter (maybe True (not . isVoid) . fst) argsBundled
              argsFiltered' = map snd argsFiltered
              tysFiltered   = map snd argsFiltered'
              compOutp      = snd <$> listToMaybe co
          if length tysFiltered == length compInps
            then do
              let dstId = Text.pack . name2String $ varName dst
              (argExprs,argDecls)   <- fmap (second concat . unzip) $! mapM (\(e,t) -> mkExpr False (Left dstId) t e) argsFiltered'
              (argExprs',argDecls') <- (second concat . unzip) <$> mapM (toSimpleVar dst) (zip argExprs tysFiltered)
              let inpAssigns    = zipWith (\(i,t) e -> (Identifier i Nothing,In,t,e)) compInps argExprs'
                  outpAssign    = case compOutp of
                    Nothing -> []
                    Just (id_,hwtype) -> [(Identifier id_ Nothing,Out,hwtype,Identifier dstId Nothing)]
              instLabel <- extendIdentifier Basic compName (Text.pack "_" `Text.append` dstId)
              let instDecl      = InstDecl Entity Nothing compName instLabel (outpAssign ++ inpAssigns)
              return (argDecls ++ argDecls' ++ [instDecl])
            else error $ $(curLoc) ++ "under-applied normalized function"
        Nothing -> case args of
          [] -> do
            let dstId = id2identifier dst
            return [Assignment dstId (Identifier (Text.pack $ name2String fun) Nothing)]
          _ -> error $ $(curLoc) ++ "Unknown function: " ++ showDoc fun

toSimpleVar :: Id
            -> (Expr,Type)
            -> NetlistMonad (Expr,[Declaration])
toSimpleVar _ (e@(Identifier _ _),_) = return (e,[])
toSimpleVar dst (e,ty) = do
  argNm <- extendIdentifier Extended
             (id2identifier dst)
             (Text.pack "_fun_arg")
  argNm' <- mkUniqueIdentifier Extended argNm
  hTy <- unsafeCoreTypeToHWTypeM $(curLoc) ty
  let argDecl         = NetDecl Nothing argNm' hTy
      argAssn         = Assignment argNm' e
  return (Identifier argNm' Nothing,[argDecl,argAssn])

-- | Generate an expression for a term occurring on the RHS of a let-binder
mkExpr :: Bool -- ^ Treat BlackBox expression as declaration
       -> (Either Identifier Id) -- ^ Id to assign the result to
       -> Type -- ^ Type of the LHS of the let-binder
       -> Term -- ^ Term to convert to an expression
       -> NetlistMonad (Expr,[Declaration]) -- ^ Returned expression and a list of generate BlackBox declarations
mkExpr _ _ _ (Core.Literal l) = do
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

mkExpr bbEasD bndr ty app = do
  let (appF,args) = collectArgs app
      tmArgs      = lefts args
  hwTy    <- unsafeCoreTypeToHWTypeM $(curLoc) ty
  (_,sp) <- Lens.use curCompNm
  case appF of
    Data dc -> mkDcApplication hwTy bndr dc tmArgs
    Prim nm _ -> mkPrimitive False bbEasD bndr nm args ty
    Var _ f
      | null tmArgs -> return (Identifier (Text.pack $ name2String f) Nothing,[])
      | otherwise ->
        throw (ClashException sp ($(curLoc) ++ "Not in normal form: top-level binder in argument position:\n\n" ++ showDoc app) Nothing)
    Case scrut ty' [alt] -> mkProjection bbEasD bndr scrut ty' alt
    _ -> throw (ClashException sp ($(curLoc) ++ "Not in normal form: application of a Let/Lam/Case:\n\n" ++ showDoc app) Nothing)

-- | Generate an expression that projects a field out of a data-constructor.
--
-- Works for both product types, as sum-of-product types.
mkProjection
  :: Bool
  -- ^ Projection must bind to a simple variable
  -> Either Identifier Id
  -- ^ The signal to which the projection is (potentially) assigned
  -> Term
  -- ^ The subject/scrutinee of the projection
  -> Type
  -- ^ The type of the result
  -> Alt
  -- ^ The field to be projected
  -> NetlistMonad (Expr, [Declaration])
mkProjection mkDec bndr scrut altTy alt = do
  tcm <- Lens.use tcCache
  scrutTy <- termType tcm scrut
  let e = Case scrut scrutTy [alt]
  (pat,v) <- unbind alt
  (_,sp) <- Lens.use curCompNm
  varTm <- case v of
    (Var _ n) -> return n
    _ -> throw (ClashException sp ($(curLoc) ++
                "Not in normal form: RHS of case-projection is not a variable:\n\n"
                 ++ showDoc e) Nothing)
  sHwTy <- unsafeCoreTypeToHWTypeM $(curLoc) scrutTy
  vHwTy <- unsafeCoreTypeToHWTypeM $(curLoc) altTy
  (selId,modM,decls) <- do
    scrutNm <- either return
                 (\b -> extendIdentifier Extended
                          (id2identifier b)
                          (Text.pack ("_projection")))
                 bndr
    (scrutExpr,newDecls) <- mkExpr False (Left scrutNm) scrutTy scrut
    case scrutExpr of
      Identifier newId modM -> return (newId,modM,newDecls)
      _ -> do
        scrutNm' <- mkUniqueIdentifier Extended scrutNm
        let scrutDecl = NetDecl Nothing scrutNm' sHwTy
            scrutAssn = Assignment scrutNm' scrutExpr
        return (scrutNm',Nothing,newDecls ++ [scrutDecl,scrutAssn])

  let altVarId = Text.pack $ name2String varTm
  modifier <- case pat of
        DataPat (Embed dc) ids -> do
          let (exts,tms) = unrebind ids
              tmsTys     = map (unembed . varType) tms
              tmsFVs     = concatMap (Lens.toListOf typeFreeVars) tmsTys
              extNms     = map (nameOcc.varName) exts
              tms'       = if any (`elem` tmsFVs) extNms
                              then throw (ClashException sp ($(curLoc) ++ "Not in normal form: Pattern binds existential variables:\n\n" ++ showDoc e) Nothing)
                              else tms
          argHWTys <- mapM coreTypeToHWTypeM tmsTys
          let tmsBundled   = zip argHWTys tms'
              tmsFiltered  = filter (maybe False (not . isVoid) . fst) tmsBundled
              tmsFiltered' = map snd tmsFiltered
          case elemIndex (Id varTm (Embed altTy)) tmsFiltered' of
               Nothing -> pure Nothing
               Just fI
                | sHwTy /= vHwTy -> pure $ nestModifier modM (Just (Indexed (sHwTy,dcTag dc - 1,fI)))
                -- When element and subject have the same HW-type,
                -- then the projections is just the identity
                | otherwise      -> pure $ nestModifier modM (Just (DC (Void Nothing,0)))
        _ -> throw (ClashException sp ($(curLoc) ++ "Not in normal form: Unexpected pattern in case-projection:\n\n" ++ showDoc e) Nothing)
  let extractExpr = Identifier (maybe altVarId (const selId) modifier) modifier
  case bndr of
    Left scrutNm | mkDec -> do
      scrutNm' <- mkUniqueIdentifier Extended scrutNm
      let scrutDecl = NetDecl Nothing scrutNm' vHwTy
          scrutAssn = Assignment scrutNm' extractExpr
      return (Identifier scrutNm' Nothing,scrutDecl:scrutAssn:decls)
    _ -> return (extractExpr,decls)
  where
    nestModifier Nothing  m          = m
    nestModifier m Nothing           = m
    nestModifier (Just m1) (Just m2) = Just (Nested m1 m2)


-- | Generate an expression for a DataCon application occurring on the RHS of a let-binder
mkDcApplication
    :: HWType
    -- ^ HWType of the LHS of the let-binder
    -> (Either Identifier Id)
    -- ^ Id to assign the result to
    -> DataCon
    -- ^ Applied DataCon
    -> [Term]
    -- ^ DataCon Arguments
    -> NetlistMonad (Expr,[Declaration])
    -- ^ Returned expression and a list of generate BlackBox declarations
mkDcApplication dstHType bndr dc args = do
  let dcNm = name2String (dcName dc)
  tcm                 <- Lens.use tcCache
  argTys              <- mapM (termType tcm) args
  argNm <- either return (\b -> extendIdentifier Extended (Text.pack (name2String (varName b))) (Text.pack "_dc_arg")) bndr
  argHWTys            <- mapM coreTypeToHWTypeM argTys
  -- Filter out the arguments of hwtype `Void` and only translate
  -- them to the intermediate HDL afterwards
  let argsBundled   = zip argHWTys (zip args argTys)
      (hWTysFiltered,argsFiltered) = unzip
        (filter (maybe True (not . isVoid) . fst) argsBundled)
  (argExprs,argDecls) <- fmap (second concat . unzip) $! mapM (\(e,t) -> mkExpr False (Left argNm) t e) argsFiltered
  fmap (,argDecls) $! case (hWTysFiltered,argExprs) of
    -- Is the DC just a newtype wrapper?
    ([Just argHwTy],[argExpr]) | argHwTy == dstHType ->
      return (HW.DataCon dstHType (DC (Void Nothing,-1)) [argExpr])
    _ -> case dstHType of
      SP _ dcArgPairs -> do
        let dcI      = dcTag dc - 1
            dcArgs   = snd $ indexNote ($(curLoc) ++ "No DC with tag: " ++ show dcI) dcArgPairs dcI
        case compare (length dcArgs) (length argExprs) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,dcI)) argExprs)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"
      Product _ dcArgs ->
        case compare (length dcArgs) (length argExprs) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,0)) argExprs)
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

        case compare (length dcArgs) (length argExprs) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType, dcI)) argExprs)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"

      CustomSum _ _ _ _ ->
        return (HW.DataCon dstHType (DC (dstHType, dcTag dc - 1)) [])
      Bool ->
        let dc' = case dcTag dc of
                   1  -> HW.Literal Nothing (BoolLit False)
                   2  -> HW.Literal Nothing (BoolLit True)
                   tg -> error $ $(curLoc) ++ "unknown bool literal: " ++ showDoc dc ++ "(tag: " ++ show tg ++ ")"
        in  return dc'
      Vector 0 _ -> return (HW.DataCon dstHType VecAppend [])
      Vector 1 _ -> case argExprs of
                      [e] -> return (HW.DataCon dstHType VecAppend [e])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `Cons`: " ++ showDoc args
      Vector _ _ -> case argExprs of
                      [e1,e2] -> return (HW.DataCon dstHType VecAppend [e1,e2])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `Cons`: " ++ showDoc args
      RTree 0 _ -> case argExprs of
                      [e] -> return (HW.DataCon dstHType RTreeAppend [e])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `LR`: " ++ showDoc args
      RTree _ _ -> case argExprs of
                      [e1,e2] -> return (HW.DataCon dstHType RTreeAppend [e1,e2])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `BR`: " ++ showDoc args
      String ->
        let dc' = case dcTag dc of
                    1 -> HW.Literal Nothing (StringLit "")
                    _ -> error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,dcTag dc,args,argHWTys)
        in  return dc'
      Void {} -> return (Identifier (Text.pack "__VOID__") Nothing)
      Signed _
        | dcNm == "GHC.Integer.Type.S#"
        -> pure (head argExprs)
        | dcNm == "GHC.Integer.Type.Jp#"
        -> pure (head argExprs)
        | dcNm == "GHC.Integer.Type.Jn#"
        , HW.Literal Nothing (NumLit i) <- head argExprs
        -> pure (HW.Literal Nothing (NumLit (negate i)))
      Unsigned _
        | dcNm == "GHC.Natural.NatS#"
        -> pure (head argExprs)
        | dcNm == "GHC.Natural.NatJ#"
        -> pure (head argExprs)
      _ -> error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,args,argHWTys)
