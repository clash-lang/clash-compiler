{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                         2017, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Functions to create BlackBox Contexts and fill in BlackBox templates
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module CLaSH.Netlist.BlackBox where

import           Control.Exception             (throw)
import           Control.Lens                  ((.=),(<<%=))
import qualified Control.Lens                  as Lens
import           Data.Char                     (ord)
import           Data.Either                   (lefts)
import qualified Data.HashMap.Lazy             as HashMap
import qualified Data.IntMap                   as IntMap
import           Data.Text.Lazy                (fromStrict, pack)
import qualified Data.Text.Lazy                as Text
import           Data.Text                     (unpack)
import qualified Data.Text                     as TextS
import           Unbound.Generics.LocallyNameless (embed, unembed)

-- import           CLaSH.Backend                 as N
import           CLaSH.Core.DataCon            as D (dcTag)
import           CLaSH.Core.Literal            as L (Literal (..))
import           CLaSH.Core.Name
  (Name (..), NameSort (..), name2String, string2SystemName)
import           CLaSH.Core.Pretty             (showDoc)
import           CLaSH.Core.Term               as C (Term (..))
import           CLaSH.Core.Type               as C (Type (..), ConstTy (..),
                                                splitFunTys)
import           CLaSH.Core.TyCon              as C (tyConDataCons)
import           CLaSH.Core.Util               (collectArgs, isFun, termType)
import           CLaSH.Core.Var                as V (Id, Var (..))
import           CLaSH.Driver.Types            (CLaSHException (..))
import {-# SOURCE #-} CLaSH.Netlist            (genComponent, mkDcApplication,
                                                mkExpr)
import           CLaSH.Netlist.BlackBox.Types  as B
import           CLaSH.Netlist.BlackBox.Util   as B
import           CLaSH.Netlist.Id              (IdType (..))
import           CLaSH.Netlist.Types           as N
import           CLaSH.Netlist.Util            as N
import           CLaSH.Normalize.Util          (isConstant)
import           CLaSH.Primitives.Types        as P
import           CLaSH.Util

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
    res   <- (`N.Identifier` Nothing) <$> mkIdentifier Extended (pack $ name2String (V.varName resId))
    resTy <- unsafeCoreTypeToHWTypeM $(curLoc) (unembed $ V.varType resId)

    return ( Context (res,resTy) imps funs Nothing
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
                -> NetlistMonad BlackBoxTemplate
prepareBlackBox pNm templ bbCtx =
  if verifyBlackBoxContext bbCtx templ
     then instantiateCompName >=>
          setSym bbCtx >=>
          collectFilePaths bbCtx $ templ
     else do
       (_,sp) <- Lens.use curCompNm
       templ' <- prettyBlackBox templ
       let msg = $(curLoc) ++ "Can't match template for " ++ show pNm ++ " :\n\n" ++ Text.unpack templ' ++
                "\n\nwith context:\n\n" ++ show bbCtx
       throw (CLaSHException sp msg Nothing)

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
                ,Void,False),[])
      Just hwTy -> case collectArgs e of
        (Var _ v,[]) -> do vT <- (`Identifier` Nothing) <$> mkIdentifier Extended (pack $ name2String v)
                           return ((vT,hwTy,False),[])
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
    Just p@(P.BlackBox {}) -> do
      case template p of
        (Left tempD) -> do
          let pNm = name p
          (dst',dstNm,dstDecl) <- resBndr True dst
          (bbCtx,ctxDcls) <- mkBlackBoxContext dst' (lefts args)
          bbDecl <- N.BlackBoxD pNm (library p) (imports p) (qsysInclude p) <$> prepareBlackBox pNm tempD bbCtx <*> pure bbCtx
          return (Identifier dstNm Nothing,dstDecl ++ ctxDcls ++ [bbDecl])
        (Right tempE) -> do
          let pNm = name p
          if bbEasD
            then do
              (dst',dstNm,dstDecl) <- resBndr True dst
              (bbCtx,ctxDcls) <- mkBlackBoxContext dst' (lefts args)
              bbTempl <- prepareBlackBox pNm tempE bbCtx
              let tmpAssgn = Assignment dstNm (BlackBoxE pNm (library p) (imports p) (qsysInclude p) bbTempl bbCtx bbEParen)
              return (Identifier dstNm Nothing, dstDecl ++ ctxDcls ++ [tmpAssgn])
            else do
              (dst',_,_) <- resBndr False dst
              (bbCtx,ctxDcls) <- mkBlackBoxContext dst' (lefts args)
              bbTempl <- prepareBlackBox pNm tempE bbCtx
              return (BlackBoxE pNm (library p) (imports p) (qsysInclude p) bbTempl bbCtx bbEParen,ctxDcls)
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
                  let netDeclRhs   = NetDecl tmpRhs scrutHTy
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
                let netDeclRhs   = NetDecl tmpRhs scrutHTy
                    netAssignRhs = Assignment tmpRhs scrutExpr
                return (DataTag scrutHTy (Right tmpRhs),[netDeclRhs,netAssignRhs] ++ scrutDecls)
          _ -> error $ $(curLoc) ++ "dataToTag: " ++ show (map (either showDoc showDoc) args)
      | otherwise -> return (BlackBoxE "" [] [] Nothing [C $ mconcat ["NO_TRANSLATION_FOR:",fromStrict pNm]] emptyBBContext False,[])
    _ -> do
      (_,sp) <- Lens.use curCompNm
      throw (CLaSHException sp ($(curLoc) ++ "No blackbox found for: " ++ unpack nm) Nothing)
  where
    resBndr :: Bool -> (Either Identifier Id) -> NetlistMonad (Id,Identifier,[Declaration])
    resBndr mkDec dst' = case dst' of
      Left dstL -> case mkDec of
        False -> do
          let nm' = Text.unpack dstL
              id_ = Id (string2SystemName nm') (embed ty)
          return (id_,dstL,[])
        True -> do
          nm'  <- extendIdentifier Extended dstL "_app_arg"
          nm'' <- mkUniqueIdentifier Extended nm'
          let nm3 = (string2SystemName (Text.unpack nm'')) { nameSort = Internal }
          hwTy <- N.unsafeCoreTypeToHWTypeM $(curLoc) ty
          let id_ = Id nm3 (embed ty)
              idDecl = NetDecl nm'' hwTy
          return (id_,nm'',[idDecl])
      Right dstR -> return (dstR,Text.pack . name2String . varName $ dstR,[])

-- | Create an template instantiation text and a partial blackbox content for an
-- argument term, given that the term is a function. Errors if the term is not
-- a function
mkFunInput :: Id   -- ^ Identifier binding the encompassing primitive/blackbox application
           -> Term -- ^ The function argument term
           -> NetlistMonad ((Either BlackBoxTemplate Declaration,BlackBoxContext),[Declaration])
mkFunInput resId e = do
  let (appE,args) = collectArgs e
  (bbCtx,dcls) <- mkBlackBoxContext resId (lefts args)
  templ <- case appE of
            Prim nm _ -> do
              bbM <- fmap (HashMap.lookup nm) $ Lens.use primitives
              (_,sp) <- Lens.use curCompNm
              let templ = case bbM of
                            Just p@(P.BlackBox {}) -> Left (name p, template p)
                            _ -> throw (CLaSHException sp ($(curLoc) ++ "No blackbox found for: " ++ unpack nm) Nothing)
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
                  return (Right dcAss)
                Just resHTy@(Product _ dcArgs) -> do
                  let dcInps = [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..(length dcArgs - 1)]]
                      dcApp  = DataCon resHTy (DC (resHTy,0)) dcInps
                      dcAss  = Assignment (pack "~RESULT") dcApp
                  return (Right dcAss)
                Just resHTy@(Vector _ _) -> do
                  let dcInps = [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(1::Int)..2] ]
                      dcApp  = DataCon resHTy (DC (resHTy,1)) dcInps
                      dcAss  = Assignment (pack "~RESULT") dcApp
                  return (Right dcAss)
                _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
            Var _ (nameOcc -> fun) -> do
              normalized <- Lens.use bindings
              case HashMap.lookup fun normalized of
                Just _ -> do
                  (_,Component compName compInps [compOutp] _) <- preserveVarEnv $ genComponent fun
                  let inpAssigns    = zipWith (\(i,t) e' -> (Identifier i Nothing,In,t,e')) compInps [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..] ]
                      outpAssign    = (Identifier (fst compOutp) Nothing,Out,snd compOutp,Identifier (pack "~RESULT") Nothing)
                  i <- varCount <<%= (+1)
                  let instLabel     = Text.concat [compName,pack ("_" ++ show i)]
                      instDecl      = InstDecl compName instLabel (outpAssign:inpAssigns)
                  return (Right instDecl)
                Nothing -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
            _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
  case templ of
    Left (_, Left templ') -> do
      l   <- instantiateCompName templ'
      l'  <- setSym bbCtx l
      l'' <- collectFilePaths bbCtx l'
      return ((Left l'',bbCtx),dcls)
    Left (_, Right templ') -> do
      templ'' <- prettyBlackBox templ'
      let ass = Assignment (pack "~RESULT") (Identifier templ'' Nothing)
      return ((Right ass, bbCtx),dcls)
    Right decl ->
      return ((Right decl,bbCtx),dcls)

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
