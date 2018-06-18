{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Functions to create BlackBox Contexts and fill in BlackBox templates
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Netlist.BlackBox where

import           Control.Exception             (throw)
import           Control.Lens                  ((.=),(<<%=))
import qualified Control.Lens                  as Lens
import           Data.Char                     (ord)
import           Data.Either                   (lefts)
import qualified Data.HashMap.Lazy             as HashMap
import qualified Data.IntMap                   as IntMap
import           Data.Maybe                    (catMaybes)
import           Data.Semigroup.Monad
import           Data.Text.Lazy                (fromStrict, pack)
import qualified Data.Text.Lazy                as Text
import           Data.Text                     (unpack)
import qualified Data.Text                     as TextS
import           Unbound.Generics.LocallyNameless (embed, unbind, unembed)

-- import           Clash.Backend                 as N
import           Clash.Core.DataCon            as D (dcTag)
import           Clash.Core.Literal            as L (Literal (..))
import           Clash.Core.Name
  (Name (..), NameSort (..), name2String, string2SystemName)
import           Clash.Core.Pretty             (showDoc)
import           Clash.Core.Subst              (substTm)
import           Clash.Core.Term               as C (Term (..))
import           Clash.Core.Type               as C (Type (..), ConstTy (..),
                                                splitFunTys)
import           Clash.Core.TyCon              as C (tyConDataCons)
import           Clash.Core.Util               (collectArgs, isFun, termType)
import           Clash.Core.Var                as V (Id, Var (..))
import           Clash.Driver.Types            (ClashException (..))
import {-# SOURCE #-} Clash.Netlist
  (genComponent, mkDcApplication, mkDeclarations, mkExpr, mkNetDecl,
   mkProjection, mkSelection)
import           Clash.Netlist.BlackBox.Types  as B
import           Clash.Netlist.BlackBox.Util   as B
import           Clash.Netlist.Id              (IdType (..))
import           Clash.Netlist.Types           as N
import           Clash.Netlist.Util            as N
import           Clash.Normalize.Util          (isConstant)
import           Clash.Primitives.Types        as P
import           Clash.Util

-- | Generate the context for a BlackBox instantiation.
mkBlackBoxContext :: Id -- ^ Identifier binding the primitive/blackbox application
                  -> [Term] -- ^ Arguments of the primitive/blackbox application
                  -> NetlistMonad (BlackBoxContext,[Declaration])
mkBlackBoxContext resId args = do
    -- Make context inputs
    tcm             <- Lens.use tcCache
    let resNm = Text.pack . name2String $ varName resId
    (imps,impDecls) <- unzip <$> mapM (mkArgument resNm) args
    (funs,funDecls) <- mapAccumLM (addFunction tcm) IntMap.empty (zip args [0..])

    -- Make context result
    let res = Identifier resNm Nothing
    resTy <- unsafeCoreTypeToHWTypeM $(curLoc) (unembed $ V.varType resId)

    return ( Context (res,resTy) imps funs []
           , concat impDecls ++ concat funDecls
           )
  where
    addFunction tcm im (arg,i) = do
      isF <- isFun tcm arg
      if isF
         then do (f,d) <- mkFunInput resId arg
                 let im' = IntMap.insert i f im
                 return (im',d)
         else return (im,[])

prepareBlackBox :: TextS.Text
                -> BlackBoxTemplate
                -> BlackBoxContext
                -> NetlistMonad (BlackBoxTemplate,[Declaration])
prepareBlackBox pNm templ bbCtx =
  if verifyBlackBoxContext bbCtx templ
     then do
        t1 <- instantiateCompName templ
        (t2,decls) <- setSym bbCtx t1
        t3 <- collectFilePaths bbCtx t2
        return (t3,decls)
     else do
       (_,sp) <- Lens.use curCompNm
       templ' <- getMon (prettyBlackBox templ)
       let msg = $(curLoc) ++ "Can't match template for " ++ show pNm ++ " :\n\n" ++ Text.unpack templ' ++
                "\n\nwith context:\n\n" ++ show bbCtx
       throw (ClashException sp msg Nothing)

mkArgument :: Identifier -- ^ LHS of the original let-binder
           -> Term
           -> NetlistMonad ( (Expr,HWType,Bool)
                           , [Declaration]
                           )
mkArgument bndr e = do
    tcm   <- Lens.use tcCache
    ty    <- termType tcm e
    iw    <- Lens.use intWidth
    hwTyM <- N.termHWTypeM e
    let eTyMsg = "(" ++ showDoc e ++ " :: " ++ showDoc ty ++ ")"
    ((e',t,l),d) <- case hwTyM of
      Nothing   ->
        return ((Identifier (error ($(curLoc) ++ "Forced to evaluate untranslatable type: " ++ eTyMsg)) Nothing
                ,Void Nothing,False),[])
      Just hwTy -> case collectArgs e of
        (C.Var _ v,[]) -> return ((Identifier (pack (name2String v)) Nothing,hwTy,False),[])
        (C.Literal (IntegerLiteral i),[]) -> return ((N.Literal (Just (Signed iw,iw)) (N.NumLit i),hwTy,True),[])
        (C.Literal (IntLiteral i), []) -> return ((N.Literal (Just (Signed iw,iw)) (N.NumLit i),hwTy,True),[])
        (C.Literal (WordLiteral w), []) -> return ((N.Literal (Just (Unsigned iw,iw)) (N.NumLit w),hwTy,True),[])
        (C.Literal (CharLiteral c), []) -> return ((N.Literal (Just (Unsigned 21,21)) (N.NumLit . toInteger $ ord c),hwTy,True),[])
        (C.Literal (StringLiteral s),[]) -> return ((N.Literal Nothing (N.StringLit s),hwTy,True),[])
        (C.Literal (Int64Literal i), []) -> return ((N.Literal (Just (Signed 64,64)) (N.NumLit i),hwTy,True),[])
        (C.Literal (Word64Literal i), []) -> return ((N.Literal (Just (Unsigned 64,64)) (N.NumLit i),hwTy,True),[])
        (C.Literal (NaturalLiteral n), []) -> return ((N.Literal (Just (Unsigned iw,iw)) (N.NumLit n),hwTy,True),[])
        (Prim f _,args) -> do
          (e',d) <- mkPrimitive True False (Left bndr) f args ty
          case e' of
            (Identifier _ _) -> return ((e',hwTy,False), d)
            _                -> return ((e',hwTy,isConstant e), d)
        (Data dc, args) -> do
          (exprN,dcDecls) <- mkDcApplication hwTy (Left bndr) dc (lefts args)
          return ((exprN,hwTy,isConstant e),dcDecls)
        (Case scrut ty' [alt],[]) -> do
          (projection,decls) <- mkProjection False (Left bndr) scrut ty' alt
          return ((projection,hwTy,False),decls)
        _ ->
          return ((Identifier (error ($(curLoc) ++ "Forced to evaluate unexpected function argument: " ++ eTyMsg)) Nothing
                  ,hwTy,False),[])
    return ((e',t,l),d)

mkPrimitive :: Bool -- ^ Put BlackBox expression in parenthesis
            -> Bool -- ^ Treat BlackBox expression as declaration
            -> (Either Identifier Id) -- ^ Id to assign the result to
            -> TextS.Text
            -> [Either Term Type]
            -> Type
            -> NetlistMonad (Expr,[Declaration])
mkPrimitive bbEParen bbEasD dst nm args ty = do
  bbM <- HashMap.lookup nm <$> Lens.use primitives
  case bbM of
    Just p@(P.BlackBox {outputReg = wr}) -> do
      case template p of
        (Left tempD) -> do
          let pNm = name p
              wr' = if wr then Reg else Wire
          resM <- resBndr True wr' dst
          case resM of
            Just (dst',dstNm,dstDecl) -> do
              (bbCtx,ctxDcls)   <- mkBlackBoxContext dst' (lefts args)
              (templ,templDecl) <- prepareBlackBox pNm tempD bbCtx
              let bbDecl = N.BlackBoxD pNm (libraries p) (imports p)
                                       (includes p) templ bbCtx
              return (Identifier dstNm Nothing,dstDecl ++ ctxDcls ++ templDecl ++ [bbDecl])
            Nothing -> return (Identifier "__VOID__" Nothing,[])
        (Right tempE) -> do
          let pNm = name p
          if bbEasD
            then do
              resM <- resBndr True Wire dst
              case resM of
                Just (dst',dstNm,dstDecl) -> do
                  (bbCtx,ctxDcls)     <- mkBlackBoxContext dst' (lefts args)
                  (bbTempl,templDecl) <- prepareBlackBox pNm tempE bbCtx
                  let tmpAssgn = Assignment dstNm
                                    (BlackBoxE pNm (libraries p) (imports p)
                                               (includes p) bbTempl bbCtx
                                               bbEParen)
                  return (Identifier dstNm Nothing, dstDecl ++ ctxDcls ++ templDecl ++ [tmpAssgn])
                Nothing -> return (Identifier "__VOID__" Nothing,[])
            else do
              resM <- resBndr False Wire dst
              case resM of
                Just (dst',_,_) -> do
                  (bbCtx,ctxDcls)     <- mkBlackBoxContext dst' (lefts args)
                  (bbTempl,templDecl) <- prepareBlackBox pNm tempE bbCtx
                  return (BlackBoxE pNm (libraries p) (imports p) (includes p) bbTempl bbCtx bbEParen,ctxDcls ++ templDecl)
                Nothing -> return (Identifier "__VOID__" Nothing,[])
    Just (P.Primitive pNm _)
      | pNm == "GHC.Prim.tagToEnum#" -> do
          hwTy <- N.unsafeCoreTypeToHWTypeM $(curLoc) ty
          case args of
            [Right (ConstTy (TyCon tcN)), Left (C.Literal (IntLiteral i))] -> do
              tcm <- Lens.use tcCache
              let dcs = tyConDataCons (tcm HashMap.! nameOcc tcN)
                  dc  = dcs !! fromInteger i
              (exprN,dcDecls) <- mkDcApplication hwTy dst dc []
              return (exprN,dcDecls)
            [Right _, Left scrut] -> do
              tcm     <- Lens.use tcCache
              scrutTy <- termType tcm scrut
              (scrutExpr,scrutDecls) <- mkExpr False (Left "#tte_rhs") scrutTy scrut
              case scrutExpr of
                Identifier id_ Nothing -> return (DataTag hwTy (Left id_),scrutDecls)
                _ -> do
                  scrutHTy <- unsafeCoreTypeToHWTypeM $(curLoc) scrutTy
                  tmpRhs <- mkUniqueIdentifier Extended (pack "#tte_rhs")
                  let netDeclRhs   = NetDecl Nothing tmpRhs scrutHTy
                      netAssignRhs = Assignment tmpRhs scrutExpr
                  return (DataTag hwTy (Left tmpRhs),[netDeclRhs,netAssignRhs] ++ scrutDecls)
            _ -> error $ $(curLoc) ++ "tagToEnum: " ++ show (map (either showDoc showDoc) args)
      | pNm == "GHC.Prim.dataToTag#" -> case args of
          [Right _,Left (Data dc)] -> do
            iw <- Lens.use intWidth
            return (N.Literal (Just (Signed iw,iw)) (NumLit $ toInteger $ dcTag dc - 1),[])
          [Right _,Left scrut] -> do
            tcm      <- Lens.use tcCache
            scrutTy  <- termType tcm scrut
            scrutHTy <- unsafeCoreTypeToHWTypeM $(curLoc) scrutTy
            (scrutExpr,scrutDecls) <- mkExpr False (Left "#dtt_rhs") scrutTy scrut
            case scrutExpr of
              Identifier id_ Nothing -> return (DataTag scrutHTy (Right id_),scrutDecls)
              _ -> do
                tmpRhs  <- mkUniqueIdentifier Extended "#dtt_rhs"
                let netDeclRhs   = NetDecl Nothing tmpRhs scrutHTy
                    netAssignRhs = Assignment tmpRhs scrutExpr
                return (DataTag scrutHTy (Right tmpRhs),[netDeclRhs,netAssignRhs] ++ scrutDecls)
          _ -> error $ $(curLoc) ++ "dataToTag: " ++ show (map (either showDoc showDoc) args)
      | otherwise -> return (BlackBoxE "" [] [] [] [C $ mconcat ["NO_TRANSLATION_FOR:",fromStrict pNm]] emptyBBContext False,[])
    _ -> do
      (_,sp) <- Lens.use curCompNm
      throw (ClashException sp ($(curLoc) ++ "No blackbox found for: " ++ unpack nm) Nothing)
  where
    resBndr
      :: Bool
      -> WireOrReg
      -> (Either Identifier Id)
      -> NetlistMonad (Maybe (Id,Identifier,[Declaration]))
      -- Nothing when the binder would have type `Void`
    resBndr mkDec wr dst' = case dst' of
      Left dstL -> case mkDec of
        False -> do
          let nm' = Text.unpack dstL
              id_ = Id (string2SystemName nm') (embed ty)
          return (Just (id_,dstL,[]))
        True -> do
          nm'  <- extendIdentifier Extended dstL "_res"
          nm'' <- mkUniqueIdentifier Extended nm'
          let nm3 = (string2SystemName (Text.unpack nm'')) { nameSort = Internal }
          hwTy <- N.unsafeCoreTypeToHWTypeM $(curLoc) ty
          let id_ = Id nm3 (embed ty)
              idDecl = NetDecl' Nothing wr nm'' (Right hwTy)
          case hwTy of
            Void {} -> return Nothing
            _       -> return (Just (id_,nm'',[idDecl]))
      Right dstR -> return (Just (dstR,Text.pack . name2String . varName $ dstR,[]))

-- | Create an template instantiation text and a partial blackbox content for an
-- argument term, given that the term is a function. Errors if the term is not
-- a function
mkFunInput
  :: Id
  -- ^ Identifier binding the encompassing primitive/blackbox application
  -> Term
  -- ^ The function argument term
  -> NetlistMonad
      ((Either BlackBoxTemplate (Identifier,[Declaration])
       ,WireOrReg
       ,[BlackBoxTemplate]
       ,[BlackBoxTemplate]
       ,[((TextS.Text,TextS.Text),BlackBoxTemplate)]
       ,BlackBoxContext)
      ,[Declaration])
mkFunInput resId e = do
  let (appE,args) = collectArgs e
  (bbCtx,dcls) <- mkBlackBoxContext resId (lefts args)
  templ <- case appE of
            Prim nm _ -> do
              bbM <- fmap (HashMap.lookup nm) $ Lens.use primitives
              (_,sp) <- Lens.use curCompNm
              let templ = case bbM of
                            Just (P.BlackBox {..}) -> Left (outputReg,libraries,imports,includes,template)
                            _ -> throw (ClashException sp ($(curLoc) ++ "No blackbox found for: " ++ unpack nm) Nothing)
              return templ
            Data dc -> do
              tcm <- Lens.use tcCache
              eTy <- termType tcm e
              let (_,resTy) = splitFunTys tcm eTy
              resHTyM <- coreTypeToHWTypeM resTy
              case resHTyM of
                Just resHTy@(SP _ dcArgPairs) -> do
                  let dcI      = dcTag dc - 1
                      dcArgs   = snd $ indexNote ($(curLoc) ++ "No DC with tag: " ++ show dcI) dcArgPairs dcI
                      dcInps   = [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..(length dcArgs - 1)]]
                      dcApp    = DataCon resHTy (DC (resHTy,dcI)) dcInps
                      dcAss    = Assignment (pack "~RESULT") dcApp
                  return (Right (("",[dcAss]),Wire))
                Just resHTy@(Product _ dcArgs) -> do
                  let dcInps = [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..(length dcArgs - 1)]]
                      dcApp  = DataCon resHTy (DC (resHTy,0)) dcInps
                      dcAss  = Assignment (pack "~RESULT") dcApp
                  return (Right (("",[dcAss]),Wire))
                Just resHTy@(Vector _ _) -> do
                  let dcInps = [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(1::Int)..2] ]
                      dcApp  = DataCon resHTy (DC (resHTy,1)) dcInps
                      dcAss  = Assignment (pack "~RESULT") dcApp
                  return (Right (("",[dcAss]),Wire))
                -- The following happens for things like `Maybe ()`
                Just resHTy@(Sum _ _) -> do
                  let dcI   = dcTag dc - 1
                      dcApp = DataCon resHTy (DC (resHTy,dcI)) []
                      dcAss = Assignment (pack "~RESULT") dcApp
                  return (Right (("",[dcAss]),Wire))
                -- The following happens for things like `(1,())`
                Just _ -> do
                  let inp   = Identifier (pack ("~ARG[0]")) Nothing
                      assgn = Assignment (pack "~RESULT") inp
                  return (Right (("",[assgn]),Wire))
                _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
            C.Var _ (nameOcc -> fun) -> do
              normalized <- Lens.use bindings
              case HashMap.lookup fun normalized of
                Just _ -> do
                  (_,Component compName compInps [snd -> compOutp] _) <- preserveVarEnv $ genComponent fun
                  let inpAssigns    = zipWith (\(i,t) e' -> (Identifier i Nothing,In,t,e')) compInps [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..] ]
                      outpAssign    = (Identifier (fst compOutp) Nothing,Out,snd compOutp,Identifier (pack "~RESULT") Nothing)
                  i <- varCount <<%= (+1)
                  let instLabel     = Text.concat [compName,pack ("_" ++ show i)]
                      instDecl      = InstDecl Nothing compName instLabel (outpAssign:inpAssigns)
                  return (Right (("",[instDecl]),Wire))
                Nothing -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
            C.Lam _ -> go 0 appE
            _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
  case templ of
    Left (oreg,libs,imps,inc,Left templ') -> do
      l   <- instantiateCompName templ'
      (l',templDecl)  <- setSym bbCtx l
      l'' <- collectFilePaths bbCtx l'
      return ((Left l'',if oreg then Reg else Wire,libs,imps,inc,bbCtx),dcls ++ templDecl)
    Left (_,libs,imps,inc,Right templ') -> do
      templ'' <- getMon $ prettyBlackBox templ'
      let ass = Assignment (pack "~RESULT") (Identifier templ'' Nothing)
      return ((Right ("",[ass]),Wire,libs,imps,inc,bbCtx),dcls)
    Right (decl,wr) ->
      return ((Right decl,wr,[],[],[],bbCtx),dcls)
  where
    go n (Lam b) = do
      (id_,e') <- unbind b
      let nm  = varName id_
          e'' = substTm (nameOcc nm)
                        (C.Var (unembed (varType id_))
                               (string2SystemName ("~ARG[" ++ show n ++ "]")))
                        e'
      go (n+(1::Int)) e''

    go _ (C.Var _ nm) = do
      let assn = Assignment (pack "~RESULT") (Identifier (pack (name2String nm)) Nothing)
      return (Right (("",[assn]),Wire))

    go _ (Case scrut ty [alt]) = do
      (projection,decls) <- mkProjection False (Left "#bb_res") scrut ty alt
      let assn = Assignment (pack "~RESULT") projection
      nm <- if null decls
               then return ""
               else mkUniqueIdentifier Basic "projection"
      return (Right ((nm,decls ++ [assn]),Wire))

    go _ (Case scrut ty alts@(_:_:_)) = do
      let resId'  = resId {varName = string2SystemName "~RESULT"}
      selectionDecls <- mkSelection resId' scrut ty alts
      nm <- mkUniqueIdentifier Basic "selection"
      return (Right ((nm,selectionDecls),Reg))

    go _ e'@(App _ _) = do
      tcm <- Lens.use tcCache
      eType <- termType tcm e'
      (appExpr,appDecls) <- mkExpr False (Left "#bb_res") eType e'
      let assn = Assignment (pack "~RESULT") appExpr
      nm <- if null appDecls
               then return ""
               else mkUniqueIdentifier Basic "block"
      return (Right ((nm,appDecls ++ [assn]),Wire))

    go _ e'@(Letrec _) = do
      tcm <- Lens.use tcCache
      normE <- splitNormalized tcm e'
      ([],[],_,[],binders,result)  <- case normE of
        Right norm -> mkUniqueNormalized Nothing norm
        Left err -> error err
      let binders' = map (\(id_,tm) -> (goR result id_,tm)) binders
      netDecls <- fmap catMaybes . mapM mkNetDecl $ filter ((/= result) . varName . fst) binders
      decls    <- concat <$> mapM (uncurry mkDeclarations . second unembed) binders'
      Just (NetDecl' _ rw _ _) <- mkNetDecl . head $ filter ((==result) . varName . fst) binders
      nm <- mkUniqueIdentifier Basic "fun"
      return (Right ((nm,netDecls ++ decls),rw))
      where
        goR r id_ | varName id_ == r = id_ {varName = string2SystemName "~RESULT"}
                  | otherwise        = id_

    go _ e' = error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e'


instantiateCompName :: BlackBoxTemplate
                    -> NetlistMonad BlackBoxTemplate
instantiateCompName l = do
  (nm,_) <- Lens.use curCompNm
  return (setCompName nm l)

collectFilePaths :: BlackBoxContext
                 -> BlackBoxTemplate
                 -> NetlistMonad BlackBoxTemplate
collectFilePaths bbCtx l = do
  fs <- Lens.use dataFiles
  let (fs',l') = findAndSetDataFiles bbCtx fs l
  dataFiles .= fs'
  return l'
