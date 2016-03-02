{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Functions to create BlackBox Contexts and fill in BlackBox templates
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module CLaSH.Netlist.BlackBox where

import           Control.Lens                  ((.=),(<<%=))
import qualified Control.Lens                  as Lens
import           Control.Monad                 (filterM)
import           Data.Char                     (ord)
import           Data.Either                   (lefts)
import qualified Data.HashMap.Lazy             as HashMap
import qualified Data.IntMap                   as IntMap
import           Data.Text.Lazy                (append,fromStrict, pack)
import qualified Data.Text.Lazy                as Text
import           Data.Text                     (unpack)
import qualified Data.Text                     as TextS
import           Unbound.Generics.LocallyNameless (embed, name2String, string2Name,
                                                unembed)

-- import           CLaSH.Backend                 as N
import           CLaSH.Core.DataCon            as D (dcTag)
import           CLaSH.Core.Literal            as L (Literal (..))
import           CLaSH.Core.Pretty             (showDoc)
import           CLaSH.Core.Term               as C (Term (..))
import           CLaSH.Core.Type               as C (Type (..), ConstTy (..),
                                                splitFunTys)
import           CLaSH.Core.TyCon              as C (tyConDataCons)
import           CLaSH.Core.Util               (collectArgs, isFun, termType)
import           CLaSH.Core.Var                as V (Id, Var (..))
import {-# SOURCE #-} CLaSH.Netlist            (genComponent, mkDcApplication,
                                                mkExpr)
import           CLaSH.Netlist.BlackBox.Types  as B
import           CLaSH.Netlist.BlackBox.Util   as B
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
    res   <- case synchronizedClk tcm (unembed $ V.varType resId) of
                Just clk -> Right . (,clk) . (`N.Identifier` Nothing) . pack <$> mkBasicId (name2String (V.varName resId))
                Nothing  -> Left . (`N.Identifier` Nothing) . pack <$> mkBasicId (name2String (V.varName resId))
    resTy <- unsafeCoreTypeToHWTypeM $(curLoc) (unembed $ V.varType resId)

    return ( Context (res,resTy) imps funs
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
     then instantiateSym >=>
          setClocks bbCtx >=>
          collectFilePaths bbCtx >=>
          instantiateCompName $ templ
     else
       error $ $(curLoc) ++ "\nCan't match template for " ++ show pNm ++ " :\n" ++ show templ ++
               "\nwith context:\n" ++ show bbCtx

mkArgument :: Identifier -- ^ LHS of the original let-binder
           -> Term
           -> NetlistMonad ( (SyncExpr,HWType,Bool)
                           , [Declaration]
                           )
mkArgument bndr e = do
    tcm   <- Lens.use tcCache
    ty    <- termType tcm e
    iw    <- Lens.use intWidth
    hwTyM <- N.termHWTypeM e
    ((e',t,l),d) <- case hwTyM of
      Nothing   -> return ((Identifier "__VOID__" Nothing,Void,False),[])
      Just hwTy -> case collectArgs e of
        (Var _ v,[]) -> do vT <- (`Identifier` Nothing) . pack <$> mkBasicId (name2String v)
                           return ((vT,hwTy,False),[])
        (C.Literal (IntegerLiteral i),[]) -> return ((N.Literal (Just (Signed iw,iw)) (N.NumLit i),hwTy,True),[])
        (C.Literal (IntLiteral i), []) -> return ((N.Literal (Just (Signed iw,iw)) (N.NumLit i),hwTy,True),[])
        (C.Literal (WordLiteral w), []) -> return ((N.Literal (Just (Unsigned iw,iw)) (N.NumLit w),hwTy,True),[])
        (C.Literal (CharLiteral c), []) -> return ((N.Literal (Just (Unsigned 21,21)) (N.NumLit . toInteger $ ord c),hwTy,True),[])
        (C.Literal (StringLiteral s),[]) -> return ((N.Literal Nothing (N.StringLit s),hwTy,True),[])
        (C.Literal (Int64Literal i), []) -> return ((N.Literal (Just (Signed 64,64)) (N.NumLit i),hwTy,True),[])
        (C.Literal (Word64Literal i), []) -> return ((N.Literal (Just (Unsigned 64,64)) (N.NumLit i),hwTy,True),[])
        (Prim f _,args) -> do
          (e',d) <- mkPrimitive True False (Left bndr) f args ty
          case e' of
            (Identifier _ _) -> return ((e',hwTy,False), d)
            _                -> return ((e',hwTy,isConstant e), d)
        (Data dc, args) -> do
            typeTrans <- Lens.use typeTranslator
            args' <- filterM (fmap (representableType typeTrans tcm) . termType tcm) (lefts args)
            (exprN,dcDecls) <- mkDcApplication hwTy (Left bndr) dc args'
            return ((exprN,hwTy,isConstant e),dcDecls)
        _ -> return ((Identifier "__VOID__" Nothing,hwTy,False),[])
    return ((addClock tcm ty e',t,l),d)
  where
    addClock tcm ty e' = case synchronizedClk tcm ty of
                           Just clk -> Right (e',clk)
                           _        -> Left  e'

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
          bbDecl <- N.BlackBoxD pNm <$> prepareBlackBox pNm tempD bbCtx <*> pure bbCtx
          return (Identifier dstNm Nothing,dstDecl ++ ctxDcls ++ [bbDecl])
        (Right tempE) -> do
          let pNm = name p
          if bbEasD
            then do
              (dst',dstNm,dstDecl) <- resBndr True dst
              (bbCtx,ctxDcls) <- mkBlackBoxContext dst' (lefts args)
              bbTempl <- prepareBlackBox pNm tempE bbCtx
              let tmpAssgn = Assignment dstNm (BlackBoxE pNm bbTempl bbCtx bbEParen)
              return (Identifier dstNm Nothing, dstDecl ++ ctxDcls ++ [tmpAssgn])
            else do
              (dst',_,_) <- resBndr False dst
              (bbCtx,ctxDcls) <- mkBlackBoxContext dst' (lefts args)
              bbTempl <- prepareBlackBox pNm tempE bbCtx
              return (BlackBoxE pNm bbTempl bbCtx bbEParen,ctxDcls)
    Just (P.Primitive pNm _)
      | pNm == "GHC.Prim.tagToEnum#" -> do
          hwTy <- N.unsafeCoreTypeToHWTypeM $(curLoc) ty
          case args of
            [Right (ConstTy (TyCon tcN)), Left (C.Literal (IntLiteral i))] -> do
              tcm <- Lens.use tcCache
              let dcs = tyConDataCons (tcm HashMap.! tcN)
                  dc  = dcs !! fromInteger i
              (exprN,dcDecls) <- mkDcApplication hwTy dst dc []
              return (exprN,dcDecls)
            [Right _, Left scrut] -> do
              tcm     <- Lens.use tcCache
              scrutTy <- termType tcm scrut
              scrutHTy <- unsafeCoreTypeToHWTypeM $(curLoc) scrutTy
              (scrutExpr,scrutDecls) <- mkExpr False (Left "tte_rhs") scrutTy scrut
              tmpRhs <- mkUniqueIdentifier (pack "tte_rhs")
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
            (scrutExpr,scrutDecls) <- mkExpr False (Left "dtt_rhs") scrutTy scrut
            tmpRhs  <- mkUniqueIdentifier "dtt_rhs"
            let netDeclRhs   = NetDecl tmpRhs scrutHTy
                netAssignRhs = Assignment tmpRhs scrutExpr
            return (DataTag scrutHTy (Right tmpRhs),[netDeclRhs,netAssignRhs] ++ scrutDecls)
          _ -> error $ $(curLoc) ++ "dataToTag: " ++ show (map (either showDoc showDoc) args)
      | otherwise -> return (BlackBoxE "" [C $ mconcat ["NO_TRANSLATION_FOR:",fromStrict pNm]] emptyBBContext False,[])
    _ -> error $ $(curLoc) ++ "No blackbox found for: " ++ unpack nm
  where
    resBndr :: Bool -> (Either Identifier Id) -> NetlistMonad (Id,Identifier,[Declaration])
    resBndr mkDec dst' = case dst' of
      Left dstL -> case mkDec of
        False -> do
          let nm' = Text.unpack dstL
              id_ = Id (string2Name nm') (embed ty)
          return (id_,dstL,[])
        True -> do
          let nm' = append dstL "_app_arg"
          nm'' <- mkUniqueIdentifier nm'
          hwTy <- N.unsafeCoreTypeToHWTypeM $(curLoc) ty
          let id_ = Id (string2Name (Text.unpack nm'')) (embed ty)
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
              let templ = case bbM of
                            Just p@(P.BlackBox {}) -> Left (name p, template p)
                            _ -> error $ $(curLoc) ++ "No blackbox found for: " ++ unpack nm
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
                  let dcInps = [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..1] ]
                      dcApp  = DataCon resHTy (DC (resHTy,1)) dcInps
                      dcAss  = Assignment (pack "~RESULT") dcApp
                  return (Right dcAss)
                _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
            Var _ fun -> do
              normalized <- Lens.use bindings
              case HashMap.lookup fun normalized of
                Just _ -> do
                  (Component compName hidden compInps [compOutp] _) <- preserveVarEnv $ genComponent fun Nothing
                  let hiddenAssigns = map (\(i,t) -> (i,In,t,Identifier i Nothing)) hidden
                      inpAssigns    = zipWith (\(i,t) e' -> (i,In,t,e')) compInps [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..] ]
                      outpAssign    = (fst compOutp,Out,snd compOutp,Identifier (pack "~RESULT") Nothing)
                  i <- varCount <<%= (+1)
                  let instLabel     = Text.concat [compName,pack ("_" ++ show i)]
                      instDecl      = InstDecl compName instLabel (outpAssign:hiddenAssigns ++ inpAssigns)
                  return (Right instDecl)
                Nothing -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
            _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
  case templ of
    Left (_, Left templ') -> do
      l'  <- instantiateSym templ'
      l'' <- setClocks bbCtx l'
      l3  <- instantiateCompName l''
      return ((Left l3,bbCtx),dcls)
    Left (_, Right templ') -> do
      templ'' <- prettyBlackBox templ'
      let ass = Assignment (pack "~RESULT") (Identifier templ'' Nothing)
      return ((Right ass, bbCtx),dcls)
    Right decl ->
      return ((Right decl,bbCtx),dcls)

-- | Instantiate symbols references with a new symbol and increment symbol counter
instantiateSym :: BlackBoxTemplate
               -> NetlistMonad BlackBoxTemplate
instantiateSym l = do
  i <- Lens.use varCount
  let (l',i') = setSym i l
  varCount .= i'
  return l'

instantiateCompName :: BlackBoxTemplate
                    -> NetlistMonad BlackBoxTemplate
instantiateCompName l = do
  nm <- Lens.use curCompNm
  return (setCompName nm l)

collectFilePaths :: BlackBoxContext
                 -> BlackBoxTemplate
                 -> NetlistMonad BlackBoxTemplate
collectFilePaths bbCtx l = do
  fs <- Lens.use dataFiles
  let (fs',l') = findAndSetDataFiles bbCtx fs l
  dataFiles .= fs'
  return l'
