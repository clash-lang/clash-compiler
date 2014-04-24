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
import           Data.Text.Lazy                (Text, pack)
import           Data.Text                     (unpack)
import           Unbound.LocallyNameless       (embed, name2String, string2Name,
                                                unembed)

import           CLaSH.Core.DataCon            as D (dcTag)
import           CLaSH.Core.Literal            as L (Literal (..))
import           CLaSH.Core.Pretty             (showDoc)
import           CLaSH.Core.Term               as C (Term (..), TmName)
import           CLaSH.Core.Type               as C (Type (..), ConstTy (..))
import           CLaSH.Core.TyCon              as C (tyConDataCons)
import           CLaSH.Core.Util               (collectArgs, isFun, termType)
import           CLaSH.Core.Var                as V (Id, Var (..))
import {-# SOURCE #-} CLaSH.Netlist            (genComponent, mkDcApplication)
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
  (Prim f _, args) -> mkInput' f args
  _                -> fmap (first (first Left)) $ mkLitInput e
  where
    mkInput' nm args = do
      bbM <- fmap (HashMap.lookup nm) $ Lens.use primitives
      case bbM of
        Just p@(P.BlackBox {}) -> do
          i           <- lift $ varCount <<%= (+1)
          tcm         <- Lens.use tcCache
          ty          <- termType tcm e
          let dstNm   = "bb_sig_" ++ show i
              dstId   = pack dstNm
              resId   = Id (string2Name dstNm) (embed ty)
          (bbCtx,ctxDecls) <- lift $ mkBlackBoxContext resId (lefts args)
          let hwTy = snd $ result bbCtx
          case template p of
            (Left tempD)  -> do
              let netDecl = N.NetDecl dstId hwTy Nothing
                  bbCtx'  = bbCtx { result = first (either (Left . const dstId)
                                                           (Right . first (const dstId)))
                                                           (result bbCtx) }
              bbDecl      <- fmap N.BlackBoxD $ lift $ mkBlackBox tempD bbCtx'
              return ((Left dstId, hwTy),ctxDecls ++ [netDecl,bbDecl])
            (Right tempE) -> do
              bb   <- lift $ mkBlackBox tempE bbCtx
              let bb' = mconcat [pack "(",bb,pack ")"]
              return ((Left bb', hwTy),ctxDecls)
        Just (P.Primitive pNm _)
          | pNm == "GHC.Prim.tagToEnum#" -> case collectArgs (head $ fst $ partitionEithers args) of
              (C.Literal (IntegerLiteral i), _) -> do
                tcm <- Lens.use tcCache
                let ConstTy (TyCon tcN) = head . snd $ partitionEithers args
                    dcs = tyConDataCons (tcm HashMap.! tcN)
                    dc  = dcs !! (fromInteger $ i - 1)
                ((id_,hwTy),decs) <- mkLitInput (Data dc)
                return ((Left id_,hwTy),decs)
              _ -> error $ $(curLoc) ++ "tagToEnum: " ++ showDoc e
          | pNm == "GHC.Prim.dataToTag#" -> case collectArgs (head $ fst $ partitionEithers args) of
              (Data dc, _) -> return ((Left . pack . show . subtract 1 $ dcTag dc,Integer),[])
              _ -> error $ $(curLoc) ++ "dataToTag: " ++ showDoc e
          | otherwise -> mzero
        Nothing -> error $ $(curLoc) ++ "No blackbox found: " ++ unpack nm

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
mkFunInput resId e = case collectArgs e of
  (Prim nm _, args) -> do
    bbM <- fmap (HashMap.lookup nm) $ Lens.use primitives
    case bbM of
      Just p@(P.BlackBox {}) -> do
        (bbCtx,dcls) <- lift $ mkBlackBoxContext resId (lefts args)
        let (l,err) = either runParse (first (([O,C " <= "] ++) . (++ [C ";"])) . runParse) (template p)
        if null err
          then do
            l' <- lift $ instantiateSym l
            return ((l',bbCtx),dcls)
          else error $ $(curLoc) ++ "\nTemplate:\n" ++ show (template p) ++ "\nHas errors:\n" ++ show err
      _ -> error $ "No blackbox found: " ++ unpack nm
  (Var _ fun, args) -> do
    normalized <- Lens.use bindings
    case HashMap.lookup fun normalized of
      Just _ -> do
        (bbCtx,dcls) <- lift $ mkBlackBoxContext resId (lefts args)
        (Component compName hidden compInps compOutp _) <- lift $ preserveVarEnv $ genComponent fun Nothing
        let hiddenAssigns = map (\(i,_) -> (i,Identifier i Nothing)) hidden
            inpAssigns    = zip (map fst compInps) [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..] ]
            outpAssign    = (fst compOutp,Identifier (pack "~RESULT") Nothing)
        i <- varCount <<%= (+1)
        let instDecl      = InstDecl compName (pack ("comp_inst_" ++ show i)) (outpAssign:hiddenAssigns ++ inpAssigns)
        templ <- fmap (pack . show . fromJust) $ liftState vhdlMState $ inst instDecl
        let (line,err)    = runParse templ
        if null err
          then return ((line,bbCtx),dcls)
          else error $ $(curLoc) ++ "\nTemplate:\n" ++ show templ ++ "\nHas errors:\n" ++ show err
      Nothing -> return $ error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
  _ -> return $ error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e

-- | Instantiate symbols references with a new symbol and increment symbol counter
instantiateSym :: BlackBoxTemplate
               -> NetlistMonad BlackBoxTemplate
instantiateSym l = do
  i <- Lens.use varCount
  let (l',i') = setSym i l
  varCount .= i'
  return l'
