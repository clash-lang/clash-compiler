{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Functions to create BlackBox Contexts and fill in BlackBox templates
module CLaSH.Netlist.BlackBox where

import           Control.Lens                  ((.=),(<<%=))
import qualified Control.Lens                  as Lens
import           Control.Monad                 (filterM, mzero)
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Maybe     (MaybeT (..))
import           Data.Either                   (lefts, partitionEithers)
import qualified Data.HashMap.Lazy             as HashMap
import           Data.List                     (partition)
import           Data.Maybe                    (catMaybes)
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
import           CLaSH.Core.Term               as C (Term (..), TmName)
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
    tcm                   <- Lens.use tcCache
    args'                 <- fmap (zip args) $ mapM (isFun tcm) args
    (varInps,declssV)     <- fmap (unzip . catMaybes)  $ mapM (runMaybeT . mkInput) args'
    let (_,otherArgs)     = partitionEithers $ map unVar args'
        (litArgs,funArgs) = partition (\(t,b) -> not b && isConstant t) otherArgs
    (litInps,declssL)     <- fmap (unzip . catMaybes) $ mapM (runMaybeT . mkLitInput . fst) litArgs
    (funInps,declssF)     <- fmap (unzip . catMaybes) $ mapM (runMaybeT . mkFunInput resId . fst) funArgs

    -- Make context result
    let res = case synchronizedClk tcm (unembed $ V.varType resId) of
                Just clk -> Right . (,clk) . (`N.Identifier` Nothing) . mkBasicId . pack $ name2String (V.varName resId)
                Nothing  -> Left . (`N.Identifier` Nothing) . mkBasicId . pack $ name2String (V.varName resId)
    resTy <- N.unsafeCoreTypeToHWTypeM $(curLoc) (unembed $ V.varType resId)

    return ( Context (res,resTy) varInps (map fst litInps) funInps
           , concat declssV ++ concat declssL ++ concat declssF
           )
  where
    unVar :: (Term, Bool) -> Either TmName (Term, Bool)
    unVar (Var _ v, False) = Left v
    unVar t                = Right t

prepareBlackBox :: Text
                -> BlackBoxContext
                -> NetlistMonad BlackBoxTemplate
prepareBlackBox t bbCtx =
  let (templ,err) = runParse t
  in  if null err && verifyBlackBoxContext templ bbCtx
         then do
           templ'  <- instantiateSym templ
           templ'' <- setClocks bbCtx templ'
           return $! templ''
         else
           error $ $(curLoc) ++ "\nCan't match template:\n" ++ show templ ++
                   "\nwith context:\n" ++ show bbCtx ++ "\ngiven errors:\n" ++
                   show err


-- | Create an template instantiation text for an argument term
mkInput :: (Term, Bool)
        -> MaybeT NetlistMonad ((SyncExpr,HWType),[Declaration])
mkInput (_, True) = return ((Left $ Identifier (pack "__FUN__") Nothing, Void),[])

mkInput (Var ty v, False) = do
  let vT = Identifier (mkBasicId . pack $ name2String v) Nothing
  tcm <- Lens.use tcCache
  hwTy <- lift $ N.unsafeCoreTypeToHWTypeM $(curLoc) ty
  return $ case synchronizedClk tcm ty of
    Just clk -> ((Right (vT,clk), hwTy), [])
    Nothing  -> ((Left  vT,       hwTy), [])

mkInput (e, False) = case collectArgs e of
  (Prim f _, args) -> do
    tcm <- Lens.use tcCache
    ty  <- termType tcm e
    ((exprN,hwTy),decls) <- lift $ mkPrimitive True False f args ty
    return ((Left exprN,hwTy),decls)
  _ -> fmap (first (first Left)) $ mkLitInput e

mkPrimitive :: Bool -- ^ Put BlackBox expression in parenthesis
            -> Bool -- ^ Treat BlackBox expression as declaration
            -> TextS.Text
            -> [Either Term Type]
            -> Type
            -> NetlistMonad ((Expr,HWType),[Declaration])
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
          let tmpDecl = NetDecl tmpNmT hwTy Nothing
          bbDecl <- N.BlackBoxD (name p) <$> prepareBlackBox tempD bbCtx <*> pure bbCtx
          return ((Identifier tmpNmT Nothing,hwTy),ctxDcls ++ [tmpDecl,bbDecl])
        (Right tempE) -> do
          bbTempl <- prepareBlackBox tempE bbCtx
          if bbEasD
            then let tmpDecl  = NetDecl tmpNmT hwTy Nothing
                     tmpAssgn = Assignment tmpNmT (BlackBoxE (name p) bbTempl bbCtx bbEParen Nothing)
                 in  return ((Identifier tmpNmT Nothing, hwTy), ctxDcls ++ [tmpDecl,tmpAssgn])
            else return ((BlackBoxE (name p) bbTempl bbCtx bbEParen Nothing,hwTy),ctxDcls)
    Just (P.Primitive pNm _)
      | pNm == "GHC.Prim.tagToEnum#" -> do
          hwTy <- N.unsafeCoreTypeToHWTypeM $(curLoc) ty
          case args of
            [Right (ConstTy (TyCon tcN)), Left (C.Literal (IntegerLiteral i))] -> do
              tcm <- Lens.use tcCache
              let dcs = tyConDataCons (tcm HashMap.! tcN)
                  dc  = dcs !! fromInteger i
              (exprN,dcDecls) <- mkDcApplication hwTy dc []
              return ((exprN,hwTy),dcDecls)
            [Right _, Left scrut] -> do
              i <- varCount <<%= (+1)
              tcm     <- Lens.use tcCache
              scrutTy <- termType tcm scrut
              (scrutExpr,scrutDecls) <- mkExpr False scrutTy scrut
              let tmpNm     = "tmp_tte_" ++ show i
                  tmpS      = pack tmpNm
                  netDecl   = NetDecl tmpS hwTy Nothing
                  netAssign = Assignment tmpS (DataTag hwTy (Left scrutExpr))
              return ((Identifier tmpS Nothing,hwTy),netDecl:netAssign:scrutDecls)
            _ -> error $ $(curLoc) ++ "tagToEnum: " ++ show (map (either showDoc showDoc) args)
      | pNm == "GHC.Prim.dataToTag#" -> case args of
          [Right _,Left (Data dc)] -> return ((N.Literal Nothing (NumLit $ toInteger $ dcTag dc - 1),Integer),[])
          [Right _,Left scrut] -> do
            i <- varCount <<%= (+1)
            tcm      <- Lens.use tcCache
            scrutTy  <- termType tcm scrut
            scrutHTy <- unsafeCoreTypeToHWTypeM $(curLoc) scrutTy
            (scrutExpr,scrutDecls) <- mkExpr False scrutTy scrut
            let tmpNm     = "tmp_dtt_" ++ show i
                tmpS      = pack tmpNm
                netDecl   = NetDecl tmpS Integer Nothing
                netAssign = Assignment tmpS (DataTag scrutHTy (Right scrutExpr))
            return ((Identifier tmpS Nothing,Integer),netDecl:netAssign:scrutDecls)
          _ -> error $ $(curLoc) ++ "dataToTag: " ++ show (map (either showDoc showDoc) args)
      | otherwise -> return ((BlackBoxE "" [C $ mconcat ["NO_TRANSLATION_FOR:",fromStrict pNm]] emptyBBContext False Nothing,Void),[])
    _ -> error $ $(curLoc) ++ "No blackbox found for: " ++ unpack nm

-- | Create an template instantiation text for an argument term, given that
-- the term is a literal. Returns 'Nothing' if the term is not a literal.
mkLitInput :: Term -- ^ The literal argument term
           -> MaybeT NetlistMonad ((Expr,HWType),[Declaration])
mkLitInput (C.Literal (IntegerLiteral i))     = return ((N.Literal Nothing (N.NumLit i),Integer),[])
mkLitInput e@(collectArgs -> (Data dc, args)) = lift $ do
  typeTrans <- Lens.use typeTranslator
  tcm   <- Lens.use tcCache
  args' <- filterM (fmap (representableType typeTrans tcm) . termType tcm) (lefts args)
  hwTy  <- N.termHWType $(curLoc) e
  (exprN,dcDecls) <- mkDcApplication hwTy dc args'
  return ((exprN,hwTy),dcDecls)
mkLitInput _ = mzero

-- | Create an template instantiation text and a partial blackbox content for an
-- argument term, given that the term is a function. Errors if the term is not
-- a function
mkFunInput :: Id -- ^ Identifier binding the encompassing primitive/blackbox application
           -> Term -- ^ The function argument term
           -> MaybeT NetlistMonad ((Either BlackBoxTemplate Declaration,BlackBoxContext),[Declaration])
mkFunInput resId e = do
  let (appE,args) = collectArgs e
  (bbCtx,dcls) <- lift $ mkBlackBoxContext resId (lefts args)
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
              resHTyM <- lift $ coreTypeToHWTypeM resTy
              case resHTyM of
                Just resHTy@(SP _ dcArgPairs) -> do
                  let dcI      = dcTag dc - 1
                      dcArgs   = snd $ indexNote ($(curLoc) ++ "No DC with tag: " ++ show dcI) dcArgPairs dcI
                      dcInps   = [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..(length dcArgs - 1)]]
                      dcApp    = DataCon resHTy (Just $ DC (resHTy,dcI)) dcInps
                      dcAss    = Assignment (pack "~RESULT") dcApp
                  return (Right dcAss)
                Just resHTy@(Product _ dcArgs) -> do
                  let dcInps = [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..(length dcArgs - 1)]]
                      dcApp  = DataCon resHTy (Just $ DC (resHTy,0)) dcInps
                      dcAss  = Assignment (pack "~RESULT") dcApp
                  return (Right dcAss)
                _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
            Var _ fun -> do
              normalized <- Lens.use bindings
              case HashMap.lookup fun normalized of
                Just _ -> do
                  (Component compName hidden compInps compOutp _) <- lift $ preserveVarEnv $ genComponent fun Nothing
                  let hiddenAssigns = map (\(i,_) -> (i,Identifier i Nothing)) hidden
                      inpAssigns    = zip (map fst compInps) [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..] ]
                      outpAssign    = (fst compOutp,Identifier (pack "~RESULT") Nothing)
                  i <- varCount <<%= (+1)
                  let instLabel     = Text.concat [compName,pack ("_" ++ show i)]
                      instDecl      = InstDecl compName instLabel (outpAssign:hiddenAssigns ++ inpAssigns)
                  return (Right instDecl)
                Nothing -> return $ error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
            _ -> return $ error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
  case templ of
    Left (nm, Left templ') -> let (l,err) = runParse templ'
                              in  if null err
                                     then do
                                       l'  <- lift $ instantiateSym l
                                       l'' <- setClocks bbCtx l'
                                       return ((Left l'',bbCtx),dcls)
                                     else error $ $(curLoc) ++ "\nTemplate:\n" ++ show templ ++ "\nHas errors:\n" ++ show err
    Left (nm, Right templ') -> let ass = Assignment (pack "~RESULT") (Identifier templ' Nothing)
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
