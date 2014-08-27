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
import           Control.Monad.State           (state)
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Maybe     (MaybeT (..))
import           Control.Monad.Writer          (tell)
import           Data.Either                   (lefts, partitionEithers)
import qualified Data.HashMap.Lazy             as HashMap
import           Data.List                     (partition)
import           Data.Maybe                    (catMaybes, fromJust)
import           Data.Monoid                   (mconcat)
import           Data.Text.Lazy                (Text, fromStrict, pack)
import qualified Data.Text.Lazy                as Text
import           Data.Text                     (unpack)
import qualified Data.Text                     as TextS
import           Unbound.LocallyNameless       (embed, name2String, string2Name,
                                                unembed)

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
import           CLaSH.Netlist.VHDL            as N
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
                Just clk -> Right . (,clk) . mkBasicId . pack $ name2String (V.varName resId)
                Nothing  -> Left . mkBasicId . pack $ name2String (V.varName resId)
    resTy <- N.unsafeCoreTypeToHWTypeM $(curLoc) (unembed $ V.varType resId)

    return ( Context (res,resTy) varInps (map fst litInps) funInps
           , concat declssV ++ concat declssL ++ concat declssF
           )
  where
    unVar :: (Term, Bool) -> Either TmName (Term, Bool)
    unVar (Var _ v, False) = Left v
    unVar t                = Right t

-- | Instantiate a BlackBox template according to the given context
mkBlackBox :: Text -- ^ Template to instantiate
           -> BlackBoxContext -- ^ Context to instantiate template with
           -> NetlistMonad Text
mkBlackBox templ bbCtx =
  let (l,err) = runParse templ
  in if null err && verifyBlackBoxContext l bbCtx
    then do
      l'        <- instantiateSym l
      (bb,clks) <- liftState vhdlMState $ state $ renderBlackBox l' bbCtx
      tell clks
      return $! bb
    else error $ $(curLoc) ++ "\nCan't match template:\n" ++ show templ ++ "\nwith context:\n" ++ show bbCtx ++ "\ngiven errors:\n" ++ show err

-- | Create an template instantiation text for an argument term
mkInput :: (Term, Bool)
        -> MaybeT NetlistMonad ((SyncIdentifier,HWType),[Declaration])
mkInput (_, True) = return ((Left $ pack "__FUN__", Void),[])

mkInput (Var ty v, False) = do
  let vT = mkBasicId . pack $ name2String v
  tcm  <- Lens.use tcCache
  hwTy <- lift $ N.unsafeCoreTypeToHWTypeM $(curLoc) ty
  case synchronizedClk tcm ty of
    Just clk -> return ((Right (vT,clk), hwTy),[])
    Nothing  -> return ((Left vT, hwTy),[])

mkInput (e, False) = case collectArgs e of
  (Prim f _, args) -> do
    tcm <- Lens.use tcCache
    ty  <- termType tcm e
    ((exprN,hwTy),decls) <- lift $ mkPrimitive True f args ty
    exprV <- fmap (pack . show) $ liftState vhdlMState $ N.expr False exprN
    return ((Left exprV,hwTy),decls)
  _ -> fmap (first (first Left)) $ mkLitInput e

mkPrimitive :: Bool
            -> TextS.Text
            -> [Either Term Type]
            -> Type
            -> NetlistMonad ((Expr,HWType),[Declaration])
mkPrimitive bbEParen nm args ty = do
  bbM <- HashMap.lookup nm <$> Lens.use primitives
  case bbM of
    Just p@(P.BlackBox {}) -> do
      i   <- varCount <<%= (+1)
      let tmpNm   = "tmp_" ++ show i
          tmpNmT  = pack tmpNm
          tmpId   = Id (string2Name tmpNm) (embed ty)
      (bbCtx,ctxDcls) <- mkBlackBoxContext tmpId (lefts args)
      let hwTy    = snd $ result bbCtx
      case template p of
        (Left tempD) -> do
          let tmpDecl = NetDecl tmpNmT hwTy Nothing
          bbDecl <- N.BlackBoxD <$> mkBlackBox tempD bbCtx
          return ((Identifier tmpNmT Nothing,hwTy),ctxDcls ++ [tmpDecl,bbDecl])
        (Right tempE) -> do
          bbExpr <- mkBlackBox tempE bbCtx
          let bbExpr' = if bbEParen then mconcat ["(",bbExpr,")"] else bbExpr
          return ((BlackBoxE bbExpr' Nothing,hwTy),ctxDcls)
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
              (scrutExpr,scrutDecls) <- mkExpr scrutTy scrut
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
            (scrutExpr,scrutDecls) <- mkExpr scrutTy scrut
            let tmpNm     = "tmp_dtt_" ++ show i
                tmpS      = pack tmpNm
                netDecl   = NetDecl tmpS Integer Nothing
                netAssign = Assignment tmpS (DataTag scrutHTy (Right scrutExpr))
            return ((Identifier tmpS Nothing,Integer),netDecl:netAssign:scrutDecls)
          _ -> error $ $(curLoc) ++ "tagToEnum: " ++ show (map (either showDoc showDoc) args)
      | pNm == "CLaSH.Sized.Internal.BitVector.fromInteger#" -> case lefts args of
          largs@[C.Literal (IntegerLiteral i),arg] ->
            let sz = fromInteger i
            in case arg of
              C.Literal (IntegerLiteral j) ->
                return ((N.Literal (Just sz) (NumLit $ fromInteger j), BitVector sz),[])
              _ -> do
                (bbCtx,ctxDcls) <- mkBlackBoxContext (Id (string2Name "_ERROR_") (embed ty)) largs
                bb <- mkBlackBox (pack "std_logic_vector(to_unsigned(~ARG[1],~LIT[0]))") bbCtx
                return ((BlackBoxE bb Nothing,BitVector sz),ctxDcls)
          _ -> error $ $(curLoc) ++ "CLaSH.Sized.Internal.Signed.fromInteger#: " ++ show (map (either showDoc showDoc) args)
      | pNm == "CLaSH.Sized.Internal.Signed.fromInteger#" -> case lefts args of
          largs@[C.Literal (IntegerLiteral i),arg] ->
            let sz = fromInteger i
            in case arg of
              C.Literal (IntegerLiteral j)
                | i > 32 -> return ((N.Literal (Just sz) (NumLit $ fromInteger j), Signed sz),[])
              _ -> do
                (bbCtx,ctxDcls) <- mkBlackBoxContext (Id (string2Name "_ERROR_") (embed ty)) largs
                bb <- mkBlackBox (pack "to_signed(~ARG[1],~LIT[0])") bbCtx
                return ((BlackBoxE bb Nothing,Signed sz),ctxDcls)
          _ -> error $ $(curLoc) ++ "CLaSH.Sized.Internal.Signed.fromInteger#: " ++ show (map (either showDoc showDoc) args)
      | pNm == "CLaSH.Sized.Internal.Unsigned.fromInteger#" -> case lefts args of
          largs@[C.Literal (IntegerLiteral i),arg] ->
            let sz = fromInteger i
            in case arg of
              C.Literal (IntegerLiteral j)
                | i > 31 -> return ((N.Literal (Just sz) (NumLit $ fromInteger j), Unsigned sz),[])
              _ -> do
                (bbCtx,ctxDcls) <- mkBlackBoxContext (Id (string2Name "_ERROR_") (embed ty)) largs
                bb <- mkBlackBox (pack "to_unsigned(~ARG[1],~LIT[0])") bbCtx
                return ((BlackBoxE bb Nothing,Unsigned sz),ctxDcls)
          _ -> error $ $(curLoc) ++ "CLaSH.Sized.Internal.Unsigned.fromInteger#: " ++ show (map (either showDoc showDoc) args)
      | otherwise -> return ((BlackBoxE (mconcat ["NO_TRANSLATION_FOR:",fromStrict pNm]) Nothing,Void),[])
    _ -> error $ $(curLoc) ++ "No blackbox found for: " ++ unpack nm

-- | Create an template instantiation text for an argument term, given that
-- the term is a literal. Returns 'Nothing' if the term is not a literal.
mkLitInput :: Term -- ^ The literal argument term
           -> MaybeT NetlistMonad ((Identifier,HWType),[Declaration])
mkLitInput (C.Literal (IntegerLiteral i))     = return ((pack $ show i,Integer),[])
mkLitInput e@(collectArgs -> (Data dc, args)) = lift $ do
  typeTrans <- Lens.use typeTranslator
  tcm   <- Lens.use tcCache
  args' <- filterM (fmap (representableType typeTrans tcm) . termType tcm) (lefts args)
  hwTy  <- N.termHWType $(curLoc) e
  (exprN,dcDecls) <- mkDcApplication hwTy dc args'
  exprV <- fmap (pack . show) $ liftState vhdlMState $ N.expr False exprN
  return ((exprV,hwTy),dcDecls)
mkLitInput _ = mzero

-- | Create an template instantiation text and a partial blackbox content for an
-- argument term, given that the term is a function. Errors if the term is not
-- a function
mkFunInput :: Id -- ^ Identifier binding the encompassing primitive/blackbox application
           -> Term -- ^ The function argument term
           -> MaybeT NetlistMonad ((BlackBoxTemplate,BlackBoxContext),[Declaration])
mkFunInput resId e = do
  let (appE,args) = collectArgs e
  (bbCtx,dcls) <- lift $ mkBlackBoxContext resId (lefts args)
  templ <- case appE of
            Prim nm _ -> do
              bbM <- fmap (HashMap.lookup nm) $ Lens.use primitives
              let templ = case bbM of
                            Just p@(P.BlackBox {}) -> template p
                            Just (P.Primitive pNm _)
                              | pNm == "CLaSH.Sized.Internal.BitVector.fromInteger#" -> Right "std_logic_vector(to_unsigned(~ARG[1],~LIT[0]))"
                              | pNm == "CLaSH.Sized.Internal.Signed.fromInteger#"    -> Right "to_signed(~ARG[1],~LIT[0])"
                              | pNm == "CLaSH.Sized.Internal.Unsigned.fromInteger#"  -> Right "to_unsigned(~ARG[1],~LIT[0])"
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
                  templ <- fmap (pack . show . fromJust) $ liftState vhdlMState $ inst dcAss
                  return (Left templ)
                Just resHTy@(Product _ dcArgs) -> do
                  let dcInps = [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..(length dcArgs - 1)]]
                      dcApp  = DataCon resHTy (Just $ DC (resHTy,0)) dcInps
                      dcAss  = Assignment (pack "~RESULT") dcApp
                  templ <- fmap (pack . show . fromJust) $ liftState vhdlMState $ inst dcAss
                  return (Left templ)
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
                  templ <- fmap (pack . show . fromJust) $ liftState vhdlMState $ inst instDecl
                  return (Left templ)
                Nothing -> return $ error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
            _ -> return $ error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
  let (l,err) = either runParse (first (([O,C " <= "] ++) . (++ [C ";"])) . runParse) templ
  if null err
    then do
      l' <- lift $ instantiateSym l
      return ((l',bbCtx),dcls)
    else error $ $(curLoc) ++ "\nTemplate:\n" ++ show templ ++ "\nHas errors:\n" ++ show err

-- | Instantiate symbols references with a new symbol and increment symbol counter
instantiateSym :: BlackBoxTemplate
               -> NetlistMonad BlackBoxTemplate
instantiateSym l = do
  i <- Lens.use varCount
  let (l',i') = setSym i l
  varCount .= i'
  return l'
