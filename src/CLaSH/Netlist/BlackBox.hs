{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns  #-}
module CLaSH.Netlist.BlackBox where

import           Control.Lens ((.=),_2)
import qualified Control.Lens as Lens
import           Control.Monad (filterM,mzero)
import           Control.Monad.State (state)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Writer (tell)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Either       (lefts,partitionEithers)
import qualified Data.HashMap.Lazy as HashMap
import           Data.List         (partition)
import           Data.Maybe        (catMaybes,fromJust)
import           Data.Text.Lazy    (Text,pack)
import qualified Data.Text.Lazy    as Text
import           Unbound.LocallyNameless (name2String,unembed)

import CLaSH.Core.DataCon            (dcName)
import CLaSH.Core.Literal            as L (Literal(..))
import CLaSH.Core.Pretty             (showDoc)
import CLaSH.Core.Prim               (Prim (..))
import CLaSH.Core.Term               as C (Term(..),TmName)
import CLaSH.Core.Util               (collectArgs,isFun,termType)
import CLaSH.Core.Var                as V (Id,Var(..))
import CLaSH.Normalize.Util          (isConstant)
import {-# SOURCE #-} CLaSH.Netlist  (genComponent,mkDcApplication)
import CLaSH.Netlist.BlackBox.Parser as B
import CLaSH.Netlist.BlackBox.Types  as B
import CLaSH.Netlist.BlackBox.Util   as B
import CLaSH.Netlist.Id              as N
import CLaSH.Netlist.Types           as N
import CLaSH.Netlist.Util            as N
import CLaSH.Netlist.VHDL            as N
import CLaSH.Primitives.Types        as P
import CLaSH.Util

mkBlackBoxContext ::
  Id
  -> [Term]
  -> NetlistMonad BlackBoxContext
mkBlackBoxContext resId args = do
  let res               = Left . mkBasicId . pack $ name2String (V.varName resId)
  let resTy             = N.coreTypeToHWType_fail $ unembed $ V.varType resId
  isFunArgs             <- mapM isFun args
  let args'             = zip args isFunArgs
  varInps               <- mapM (runMaybeT . mkInput resId) args'
  let (_,otherArgs)     = partitionEithers $ map unVar args'
  let (litArgs,funArgs) = partition (\(t,b) -> not b && isConstant t) otherArgs
  litInps               <- mapM (runMaybeT . fmap fst . mkLitInput . fst) litArgs
  funInps               <- mapM (runMaybeT . mkFunInput resId . fst) funArgs
  return $ Context (res,resTy) (catMaybes varInps)
                               (catMaybes litInps)
                               (catMaybes funInps)

unVar :: (Term, Bool) -> Either TmName (Term, Bool)
unVar (Var _ v, False) = Left v
unVar t                = Right t

mkBlackBoxDecl ::
  Text
  -> BlackBoxContext
  -> NetlistMonad [Declaration]
mkBlackBoxDecl templ bbCtx = do
  let (l,err) = runParse templ
  case (null err && verifyBlackBoxContext l bbCtx) of
    True -> do
      i <- Lens.use varCount
      let (l',i') = setSym (fromInteger i) l
      varCount .= (toInteger i')
      (tmpl,clks) <- liftState vhdlMState $ state $ renderBlackBox l' bbCtx
      tell clks
      return [N.BlackBox tmpl]
    False -> error $ $(curLoc) ++ "\nCan't match context:\n" ++ show bbCtx ++ "\nwith template:\n" ++ show templ ++ "\ngiven errors:\n" ++ show err

mkInput ::
  Id
  -> (Term, Bool)
  -> MaybeT NetlistMonad (SyncIdentifier,HWType)
mkInput _ (_, True) = return (Left $ pack "__FUN__", Void)

mkInput _ ((Var ty v), False) = do
  let vT = mkBasicId . pack $ name2String v
  let hwTy = N.coreTypeToHWType_fail ty
  case synchronizedClk ty of
    Just clk -> return ((Right (vT,clk)), hwTy)
    Nothing  -> return ((Left vT), hwTy)

mkInput resId (e, False) = case collectArgs e of
  (Prim (PrimCon dc), args)  -> mkInput' (dcName dc) args
  (Prim (PrimFun f _), args) -> mkInput' f args
  _                          -> fmap (first Left) $ mkLitInput e
  where
    mkInput' nm args = do
      bbM <- fmap (HashMap.lookup . BSL.pack $ name2String nm) $ Lens.use primitives
      case bbM of
        Just p@(P.BlackBox {}) -> do
          bbCtx <- lift $ mkBlackBoxContext resId (lefts args)
          case (Text.null $ templateI p) of
            True -> error $ $(curLoc) ++ "Can't create inlined blackbox: " ++ show p
            False -> do
              (N.BlackBox bb:_) <- lift $ mkBlackBoxDecl (templateI p) bbCtx
              return (Left bb, Void)
        _ -> error $ $(curLoc) ++ "No blackbox found: " ++ name2String nm

mkLitInput ::
  Term
  -> MaybeT NetlistMonad (Identifier,HWType)
mkLitInput (C.Literal (IntegerLiteral i))       = return (pack $ show i,Integer)
mkLitInput e@(collectArgs -> (Data _ dc, args)) = lift $ do
  args' <- filterM (fmap representableType . termType) (lefts args)
  hwTy  <- N.termHWType e
  exprN <- mkDcApplication hwTy dc args'
  exprV <- fmap (pack . show) $ liftState vhdlMState $ N.expr exprN
  return (exprV,hwTy)
mkLitInput _                                    = mzero

mkFunInput ::
  Id
  -> Term
  -> MaybeT NetlistMonad (Line,BlackBoxContext)
mkFunInput resId e = case (collectArgs e) of
  (Prim (PrimFun nm _), args) -> do
    bbM <- fmap (HashMap.lookup . BSL.pack $ name2String nm) $ Lens.use primitives
    case bbM of
      Just p@(P.BlackBox {}) -> do
        bbCtx <- lift $ mkBlackBoxContext resId (lefts args)
        let (l,err) = runParse (template p)
        if null err
          then do
            i <- Lens.use varCount
            let (l',i') = setSym (fromInteger i) l
            varCount .= (toInteger i')
            return (l',bbCtx)
          else error $ $(curLoc) ++ "\nTemplate:\n" ++ show (template p) ++ "\nHas errors:\n" ++ show err
      _ -> error $ "No blackbox found: " ++ name2String nm
  (Var ty fun, args) -> do
    normalized <- Lens.use bindings
    case HashMap.lookup fun normalized of
      Just _ -> do
        bbCtx <- lift $ mkBlackBoxContext resId (lefts args)
        (Component compName hidden compInps compOutp _) <- lift $
          do vCnt <- Lens.use varCount
             vEnv <- Lens.use varEnv
             cN   <- Lens.use (vhdlMState . _2)
             comp <- genComponent fun Nothing
             varCount .= vCnt
             varEnv .= vEnv
             (vhdlMState . _2) .= cN
             return comp
        let hiddenAssigns = map (\(i,_) -> (i,Identifier i Nothing)) hidden
            inpAssigns    = zip (map fst compInps) [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..] ]
            outpAssign    = (fst compOutp,Identifier (pack "~RESULT") Nothing)
        i <- varCount <%= (+1)
        let instDecl      = InstDecl compName (pack ("comp_inst_" ++ show i)) (outpAssign:hiddenAssigns ++ inpAssigns)
        templ <- fmap (pack . show . fromJust) $ liftState vhdlMState $ inst instDecl
        let (line,err)    = runParse templ
        if (null err)
          then return (line,bbCtx)
          else error $ $(curLoc) ++ "\nTemplate:\n" ++ show templ ++ "\nHas errors:\n" ++ show err
      Nothing -> return $ error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
  _ -> return $ error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
