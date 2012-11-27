{-# LANGUAGE PatternGuards #-}
module CLaSH.Netlist.BlackBox where

import           Control.Monad (mzero)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Writer (tell)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Either       (lefts,partitionEithers)
import qualified Data.HashMap.Lazy as HashMap
import           Data.List         (partition)
import qualified Data.Label.PureM  as LabelM
import           Data.Maybe        (catMaybes)
import           Data.Text.Lazy    (Text,pack)
import qualified Data.Text.Lazy    as Text
import           Unbound.LocallyNameless (name2String)

import CLaSH.Core.Literal            as L (Literal(..))
import CLaSH.Core.Prim               (Prim (..))
import CLaSH.Core.Term               as C (Term(..),TmName)
import CLaSH.Core.Util               (collectArgs,isFun)
import CLaSH.Core.Var                as V (Id,Var(..))
import CLaSH.Normalize.Util          (isSimple)
import CLaSH.Netlist.BlackBox.Parser as B
import CLaSH.Netlist.BlackBox.Types  as B
import CLaSH.Netlist.BlackBox.Util   as B
import CLaSH.Netlist.Id              as N
import CLaSH.Netlist.Types           as N
import CLaSH.Netlist.Util            as N
import CLaSH.Primitives.Types        as P
import CLaSH.Util

mkBlackBoxContext ::
  Id
  -> [Term]
  -> NetlistMonad BlackBoxContext
mkBlackBoxContext resId args = do
  let res               = Left . mkBasicId . pack $ name2String (V.varName resId)
  gamma                 <- LabelM.gets varEnv
  isFunArgs             <- mapM (isFun gamma) args
  let args'             = zip args isFunArgs
  varInps               <- mapM (runMaybeT . mkInput resId) args'
  let (_,otherArgs)     = partitionEithers $ map unVar args'
  let (litArgs,funArgs) = partition (\(t,b) -> not b && isSimple t) otherArgs
  litInps               <- mapM (runMaybeT. mkLitInput . fst) litArgs
  let funInps           = map (mkFunInput . fst) funArgs
  return $ Context res (catMaybes varInps)
                       (catMaybes litInps)
                       (catMaybes funInps)

unVar :: (Term, Bool) -> Either TmName (Term, Bool)
unVar (Var v, False) = Left v
unVar t              = Right t

verifyBlackBoxContext ::
  Line
  -> BlackBoxContext
  -> Bool
verifyBlackBoxContext tmpl bbCtx =
  ((length (B.inputs bbCtx) - 1)    == B.countArgs tmpl) &&
  ((length (B.litInputs bbCtx) - 1) >= B.countLits tmpl) &&
  ((length (B.funInputs bbCtx) - 1) >= B.countFuns tmpl)

mkBlackBoxDecl ::
  Text
  -> BlackBoxContext
  -> NetlistMonad [Declaration]
mkBlackBoxDecl templ bbCtx = do
  let (l,err) = runParse templ
  case (null err && verifyBlackBoxContext l bbCtx) of
    True -> do
      let (tmpl,clks) = renderBlackBox l bbCtx
      tell clks
      return [N.BlackBox tmpl]
    False -> error $ $(curLoc) ++ "\nCan't match context:\n" ++ show bbCtx ++ "\nwith template:\n" ++ show templ ++ "\ngiven errors:\n" ++ show err

mkInput ::
  Id
  -> (Term, Bool)
  -> MaybeT NetlistMonad SyncIdentifier
mkInput _ ((Var v), False) = do
  let vT = mkBasicId . pack $ name2String v
  ty <- fmap (HashMap.! v) $ LabelM.gets varEnv
  case synchronizedClk ty of
    Just clk -> return (Right (vT,clk))
    Nothing  -> return (Left vT)

mkInput resId (e@(App _ _), False)
  | (Prim (PrimFun nm _), args) <- collectArgs e
  = do
    bbM <- fmap (HashMap.lookup . BSL.pack $ name2String nm) $ LabelM.gets primitives
    case bbM of
      Just p@(P.BlackBox {}) -> do
        bbCtx <- lift $ mkBlackBoxContext resId (lefts args)
        case (Text.null $ templateI p) of
          True -> error $ $(curLoc) ++ "Can't create inlined blackbox: " ++ show p
          False -> do
            (N.BlackBox bb:_) <- lift $ mkBlackBoxDecl (templateI p) bbCtx
            return (Left bb)
      _ -> error $ "No blackbox found: " ++ show bbM

mkInput _ (e, _) = Left <$> mkLitInput e

mkLitInput ::
  Monad m
  => Term
  -> MaybeT m Identifier
mkLitInput (C.Literal (IntegerLiteral i)) = return (pack $ show i)
mkLitInput _                              = mzero

mkFunInput ::
  Term
  -> Maybe Identifier
mkFunInput e@(App _ _)
  | (Prim (PrimFun nm _), _) <- collectArgs e
  = let nmT = pack $ name2String nm
    in Just nmT

mkFunInput _ = Nothing
