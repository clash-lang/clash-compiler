{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module CLaSH.Netlist.BlackBox where

import           Control.Lens               ((.=))
import qualified Control.Lens               as Lens
import           Control.Monad              (filterM,mzero)
import           Control.Monad.State        (state)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Writer       (tell)
import           Control.Monad.Trans.Maybe  (MaybeT(..))
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Either                (lefts,partitionEithers)
import qualified Data.HashMap.Lazy          as HashMap
import           Data.List                  (partition)
import           Data.Maybe                 (catMaybes,fromJust)
import           Data.Monoid                (mconcat)
import           Data.Text.Lazy             (Text,pack)
import           Unbound.LocallyNameless    (embed,name2String,string2Name,unembed)

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
  -> NetlistMonad (BlackBoxContext,[Declaration])
mkBlackBoxContext resId args = do
  -- Make context inputs
  args'                 <- fmap (zip args) $ mapM isFun args
  (varInps,declssV)     <- fmap (unzip . catMaybes)  $ mapM (runMaybeT . mkInput) args'
  let (_,otherArgs)     = partitionEithers $ map unVar args'
      (litArgs,funArgs) = partition (\(t,b) -> not b && isConstant t) otherArgs
  (litInps,declssL)     <- fmap (unzip . catMaybes) $ mapM (runMaybeT . mkLitInput . fst) litArgs
  (funInps,declssF)     <- fmap (unzip . catMaybes) $ mapM (runMaybeT . mkFunInput resId . fst) funArgs

  -- Make context result
  let res   = Left . mkBasicId . pack $ name2String (V.varName resId)
  resTy <- N.coreTypeToHWTypeM_unsafe (unembed $ V.varType resId)

  return ((Context (res,resTy) varInps (map fst litInps) funInps),concat declssV ++ concat declssL ++ concat declssF)

unVar :: (Term, Bool) -> Either TmName (Term, Bool)
unVar (Var _ v, False) = Left v
unVar t                = Right t

mkBlackBox ::
  Text
  -> BlackBoxContext
  -> NetlistMonad Text
mkBlackBox templ bbCtx = do
  let (l,err) = runParse templ
  case (null err && verifyBlackBoxContext l bbCtx) of
    True -> do
      i           <- Lens.use varCount
      let (l',i') =  setSym i l
      varCount    .= i'
      (bb,clks)   <- liftState vhdlMState $ state $ renderBlackBox l' bbCtx
      tell clks
      return $! bb
    False -> error $ $(curLoc) ++ "\nCan't match context:\n" ++ show bbCtx ++ "\nwith template:\n" ++ show templ ++ "\ngiven errors:\n" ++ show err

mkInput ::
  (Term, Bool)
  -> MaybeT NetlistMonad ((SyncIdentifier,HWType),[Declaration])
mkInput (_, True) = return ((Left $ pack "__FUN__", Void),[])

mkInput ((Var ty v), False) = do
  let vT = mkBasicId . pack $ name2String v
  hwTy <- lift $ N.coreTypeToHWTypeM_unsafe ty
  case synchronizedClk ty of
    Just clk -> return (((Right (vT,clk)), hwTy),[])
    Nothing  -> return (((Left vT), hwTy),[])

mkInput (e, False) = case collectArgs e of
  (Prim (PrimCon dc), args)  -> mkInput' (dcName dc) args
  (Prim (PrimFun f _), args) -> mkInput' f args
  _                          -> fmap (first (first Left)) $ mkLitInput e
  where
    mkInput' nm args = do
      bbM <- fmap (HashMap.lookup . BSL.pack $ name2String nm) $ Lens.use primitives
      case bbM of
        Just p@(P.BlackBox {}) -> do
          i           <- lift $ varCount <%= (+1)
          ty          <- termType e
          let dstNm   = "bb_sig_" ++ show i
              dstId   = pack dstNm
              resId   = Id (string2Name dstNm) (embed ty)
          (bbCtx,ctxDecls) <- lift $ mkBlackBoxContext resId (lefts args)
          let hwTy = snd $ result bbCtx
          case (template p) of
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
        _ -> error $ $(curLoc) ++ "No blackbox found: " ++ name2String nm

mkLitInput ::
  Term
  -> MaybeT NetlistMonad ((Identifier,HWType),[Declaration])
mkLitInput (C.Literal (IntegerLiteral i))       = return ((pack $ show i,Integer),[])
mkLitInput e@(collectArgs -> (Data dc, args)) = lift $ do
  typeTrans <- Lens.use typeTranslator
  args' <- filterM (fmap (representableType typeTrans) . termType) (lefts args)
  hwTy  <- N.termHWType e
  (exprN,dcDecls) <- mkDcApplication hwTy dc args'
  exprV <- fmap (pack . show) $ liftState vhdlMState $ N.expr False exprN
  return ((exprV,hwTy),dcDecls)
mkLitInput _ = mzero

mkFunInput ::
  Id
  -> Term
  -> MaybeT NetlistMonad ((Line,BlackBoxContext),[Declaration])
mkFunInput resId e = case (collectArgs e) of
  (Prim (PrimFun nm _), args) -> do
    bbM <- fmap (HashMap.lookup . BSL.pack $ name2String nm) $ Lens.use primitives
    case bbM of
      Just p@(P.BlackBox {}) -> do
        (bbCtx,dcls) <- lift $ mkBlackBoxContext resId (lefts args)
        let (l,err) = either runParse (first (([O,C " <= "] ++) . (++ [C ";"])) . runParse) (template p)
        if null err
          then do
            i <- Lens.use varCount
            let (l',i') = setSym i l
            varCount .= i'
            return ((l',bbCtx),dcls)
          else error $ $(curLoc) ++ "\nTemplate:\n" ++ show (template p) ++ "\nHas errors:\n" ++ show err
      _ -> error $ "No blackbox found: " ++ name2String nm
  (Var ty fun, args) -> do
    normalized <- Lens.use bindings
    case HashMap.lookup fun normalized of
      Just _ -> do
        (bbCtx,dcls) <- lift $ mkBlackBoxContext resId (lefts args)
        (Component compName hidden compInps compOutp _) <- lift $ preserveVHDLState $ genComponent fun Nothing
        let hiddenAssigns = map (\(i,_) -> (i,Identifier i Nothing)) hidden
            inpAssigns    = zip (map fst compInps) [ Identifier (pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..] ]
            outpAssign    = (fst compOutp,Identifier (pack "~RESULT") Nothing)
        i <- varCount <%= (+1)
        let instDecl      = InstDecl compName (pack ("comp_inst_" ++ show i)) (outpAssign:hiddenAssigns ++ inpAssigns)
        templ <- fmap (pack . show . fromJust) $ liftState vhdlMState $ inst instDecl
        let (line,err)    = runParse templ
        if (null err)
          then return ((line,bbCtx),dcls)
          else error $ $(curLoc) ++ "\nTemplate:\n" ++ show templ ++ "\nHas errors:\n" ++ show err
      Nothing -> return $ error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
  _ -> return $ error $ $(curLoc) ++ "Cannot make function input for: " ++ showDoc e
