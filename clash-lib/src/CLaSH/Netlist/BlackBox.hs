{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Functions to create BlackBox Contexts and fill in BlackBox templates
module CLaSH.Netlist.BlackBox where

import           Control.Lens                  ((.=),(<<%=))
import qualified Control.Lens                  as Lens
import           Control.Monad                 (filterM)
import           Data.Either                   (lefts)
import qualified Data.HashMap.Lazy             as HashMap
import qualified Data.IntMap                   as IntMap
import           Data.Monoid                   (mconcat)
import           Data.Text.Lazy                (Text, fromStrict, pack)
import qualified Data.Text.Lazy                as Text
import           Data.Text                     (unpack)
import qualified Data.Text                     as TextS
import           Unbound.LocallyNameless       (embed, name2String, string2Name,
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
import           CLaSH.Netlist.BlackBox.Parser as B
import           CLaSH.Netlist.BlackBox.Types  as B
import           CLaSH.Netlist.BlackBox.Util   as B
import           CLaSH.Netlist.Id              as N
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
    (imps,impDecls) <- unzip <$> mapM mkArgument args
    (funs,funDecls) <- mapAccumLM (addFunction tcm) IntMap.empty (zip args [0..])

    -- Make context result
    let res = case synchronizedClk tcm (unembed $ V.varType resId) of
                Just clk -> Right . (,clk) . (`N.Identifier` Nothing) . mkBasicId . pack $ name2String (V.varName resId)
                Nothing  -> Left . (`N.Identifier` Nothing) . mkBasicId . pack $ name2String (V.varName resId)
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
                -> Text
                -> BlackBoxContext
                -> NetlistMonad BlackBoxTemplate
prepareBlackBox pNm t bbCtx =
  let (templ,err) = runParse t
  in  if null err && verifyBlackBoxContext bbCtx templ
         then do
           templ'  <- instantiateSym templ
           templ'' <- setClocks bbCtx templ'
           return $! templ''
         else
           error $ $(curLoc) ++ "\nCan't match template for " ++ show pNm ++ " :\n" ++ show t ++
                   "\nwith context:\n" ++ show bbCtx ++ "\ngiven errors:\n" ++
                   show err

mkArgument :: Term
           -> NetlistMonad ( (SyncExpr,HWType,Bool)
                           , [Declaration]
                           )
mkArgument e = do
    tcm   <- Lens.use tcCache
    ty    <- termType tcm e
    hwTyM <- N.termHWTypeM e
    ((e',t,l),d) <- case hwTyM of
      Nothing   -> return ((Identifier "__VOID__" Nothing,Void,False),[])
      Just hwTy -> case collectArgs e of
        (Var _ v,[]) -> let vT = Identifier (mkBasicId . pack $ name2String v) Nothing
                        in  return ((vT,hwTy,False),[])
        (C.Literal (IntegerLiteral i),[]) -> return ((N.Literal Nothing (N.NumLit i),hwTy,True),[])
        (Prim f _,args) -> do
          (e',d) <- mkPrimitive True False f args ty
          case e' of
            (Identifier _ _) -> return ((e',hwTy,False), d)
            _                -> return ((e',hwTy,isConstant e), d)
        (Data dc, args) -> do
            typeTrans <- Lens.use typeTranslator
            args' <- filterM (fmap (representableType typeTrans tcm) . termType tcm) (lefts args)
            (exprN,dcDecls) <- mkDcApplication hwTy dc args'
            return ((exprN,hwTy,isConstant e),dcDecls)
        _ -> return ((Identifier "__VOID__" Nothing,hwTy,False),[])
    return ((addClock tcm ty e',t,l),d)
  where
    addClock tcm ty e' = case synchronizedClk tcm ty of
                           Just clk -> Right (e',clk)
                           _        -> Left  e'

mkPrimitive :: Bool -- ^ Put BlackBox expression in parenthesis
            -> Bool -- ^ Treat BlackBox expression as declaration
            -> TextS.Text
            -> [Either Term Type]
            -> Type
            -> NetlistMonad (Expr,[Declaration])
mkPrimitive bbEParen bbEasD nm args ty = do
  bbM <- HashMap.lookup nm <$> Lens.use primitives
  case bbM of
    Just p@(P.BlackBox {}) -> do
      i   <- varCount <<%= (+1)
      let tmpNm   = "tmp_" ++ show i
          tmpNmT  = pack tmpNm
          tmpId   = Id (string2Name tmpNm) (embed ty)
      (bbCtx,ctxDcls) <- mkBlackBoxContext tmpId (lefts args)
      let hwTy    = snd $ bbResult bbCtx
      case template p of
        (Left tempD) -> do
          let tmpDecl = NetDecl tmpNmT hwTy
              pNm     = name p
          bbDecl <- N.BlackBoxD pNm <$> prepareBlackBox pNm tempD bbCtx <*> pure bbCtx
          return (Identifier tmpNmT Nothing,ctxDcls ++ [tmpDecl,bbDecl])
        (Right tempE) -> do
          let pNm = name p
          bbTempl <- prepareBlackBox pNm tempE bbCtx
          if bbEasD
            then let tmpDecl  = NetDecl tmpNmT hwTy
                     tmpAssgn = Assignment tmpNmT (BlackBoxE pNm bbTempl bbCtx bbEParen)
                 in  return (Identifier tmpNmT Nothing, ctxDcls ++ [tmpDecl,tmpAssgn])
            else return (BlackBoxE pNm bbTempl bbCtx bbEParen,ctxDcls)
    Just (P.Primitive pNm _)
      | pNm == "GHC.Prim.tagToEnum#" -> do
          hwTy <- N.unsafeCoreTypeToHWTypeM $(curLoc) ty
          case args of
            [Right (ConstTy (TyCon tcN)), Left (C.Literal (IntegerLiteral i))] -> do
              tcm <- Lens.use tcCache
              let dcs = tyConDataCons (tcm HashMap.! tcN)
                  dc  = dcs !! fromInteger i
              (exprN,dcDecls) <- mkDcApplication hwTy dc []
              return (exprN,dcDecls)
            [Right _, Left scrut] -> do
              i <- varCount <<%= (+1)
              tcm     <- Lens.use tcCache
              scrutTy <- termType tcm scrut
              scrutHTy <- unsafeCoreTypeToHWTypeM $(curLoc) scrutTy
              (scrutExpr,scrutDecls) <- mkExpr False scrutTy scrut
              let tmpRhs       = pack ("tmp_tte_rhs_" ++ show i)
                  tmpS         = pack ("tmp_tte_" ++ show i)
                  netDeclRhs   = NetDecl tmpRhs scrutHTy
                  netDeclS     = NetDecl tmpS hwTy
                  netAssignRhs = Assignment tmpRhs scrutExpr
                  netAssignS   = Assignment tmpS (DataTag hwTy (Left tmpRhs))
              return (Identifier tmpS Nothing,[netDeclRhs,netDeclS,netAssignRhs,netAssignS] ++ scrutDecls)
            _ -> error $ $(curLoc) ++ "tagToEnum: " ++ show (map (either showDoc showDoc) args)
      | pNm == "GHC.Prim.dataToTag#" -> case args of
          [Right _,Left (Data dc)] -> return (N.Literal Nothing (NumLit $ toInteger $ dcTag dc - 1),[])
          [Right _,Left scrut] -> do
            i <- varCount <<%= (+1)
            j <- varCount <<%= (+1)
            tcm      <- Lens.use tcCache
            scrutTy  <- termType tcm scrut
            scrutHTy <- unsafeCoreTypeToHWTypeM $(curLoc) scrutTy
            (scrutExpr,scrutDecls) <- mkExpr False scrutTy scrut
            let tmpRhs       = pack ("tmp_dtt_rhs_" ++ show i)
                tmpS         = pack ("tmp_dtt_" ++ show j)
                netDeclRhs   = NetDecl tmpRhs scrutHTy
                netDeclS     = NetDecl tmpS Integer
                netAssignRhs = Assignment tmpRhs scrutExpr
                netAssignS   = Assignment tmpS   (DataTag scrutHTy (Right tmpRhs))
            return (Identifier tmpS Nothing,[netDeclRhs,netDeclS,netAssignRhs,netAssignS] ++ scrutDecls)
          _ -> error $ $(curLoc) ++ "dataToTag: " ++ show (map (either showDoc showDoc) args)
      | otherwise -> return (BlackBoxE "" [C $ mconcat ["NO_TRANSLATION_FOR:",fromStrict pNm]] emptyBBContext False,[])
    _ -> error $ $(curLoc) ++ "No blackbox found for: " ++ unpack nm

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
                _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
            Var _ fun -> do
              normalized <- Lens.use bindings
              case HashMap.lookup fun normalized of
                Just _ -> do
                  (Component compName hidden compInps compOutp _) <- preserveVarEnv $ genComponent fun Nothing
                  let hiddenAssigns = map (\(i,_) -> (i,Identifier i Nothing)) hidden
                      inpAssigns    = zip (map fst compInps) [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..] ]
                      outpAssign    = (fst compOutp,Identifier (pack "~RESULT") Nothing)
                  i <- varCount <<%= (+1)
                  let instLabel     = Text.concat [compName,pack ("_" ++ show i)]
                      instDecl      = InstDecl compName instLabel (outpAssign:hiddenAssigns ++ inpAssigns)
                  return (Right instDecl)
                Nothing -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
            _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
  case templ of
    Left (_, Left templ') -> let (l,err) = runParse templ'
                             in  if null err
                                    then do
                                      l'  <- instantiateSym l
                                      l'' <- setClocks bbCtx l'
                                      return ((Left l'',bbCtx),dcls)
                                    else error $ $(curLoc) ++ "\nTemplate:\n" ++ show templ ++ "\nHas errors:\n" ++ show err
    Left (_, Right templ') -> let ass = Assignment (pack "~RESULT") (Identifier templ' Nothing)
                              in  return ((Right ass, bbCtx),dcls)
    Right decl -> return ((Right decl,bbCtx),dcls)

-- | Instantiate symbols references with a new symbol and increment symbol counter
instantiateSym :: BlackBoxTemplate
               -> NetlistMonad BlackBoxTemplate
instantiateSym l = do
  i <- Lens.use varCount
  let (l',i') = setSym i l
  varCount .= i'
  return l'
