{-# LANGUAGE PatternGuards #-}
module CLaSH.Netlist.BlackBox where

import           Control.Monad.State (evalStateT)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LZ
import           Data.Either       (partitionEithers,lefts)
import qualified Data.HashMap.Lazy as HashMap
import           Data.List         (partition)
import qualified Data.Label.PureM  as LabelM
import           Data.Maybe        (catMaybes)
import           Data.Text.Lazy    (pack)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Text.Hastache     as Hastache
import qualified Text.Hastache.Context as Hastache
import           Unbound.LocallyNameless (name2String,unembed)

import CLaSH.Core.Literal as L (Literal(..))
import CLaSH.Core.Prim (Prim (..))
import CLaSH.Core.Term as C (Term(..),TmName)
import CLaSH.Core.Type (Type)
import CLaSH.Core.Util (collectArgs, isFun)
import CLaSH.Core.Var  as V (Id,Var(..))
import CLaSH.Normalize.Util (isSimple)
import CLaSH.Netlist.BlackBox.Types as B
import CLaSH.Netlist.BlackBox.Util as B
import CLaSH.Netlist.Id
import CLaSH.Netlist.Types as HW
import CLaSH.Netlist.Util
import CLaSH.Primitives.Types as P
import CLaSH.Util

mkBlackBoxContext ::
  Id
  -> [Term]
  -> NetlistMonad BlackBoxContext
mkBlackBoxContext resId args = do
  let res = mkBasicId . pack $ name2String (V.varName resId)
  gamma     <- LabelM.gets varEnv
  isFunArgs <- mapM (isFun gamma) args
  let args' = zip args isFunArgs
  varInps   <- mapM (mkInput resId) args'
  let (_,otherArgs)     = partitionEithers $ map unVar args'
  let (litArgs,funArgs) = partition (\(t,b) -> not b && isSimple t) otherArgs
  let litInps           = map (mkLitInput . fst) litArgs
  let funInps           = map (mkFunInput . fst) funArgs
  return $ Context res (catMaybes varInps) (catMaybes litInps) (catMaybes funInps) addContext addInput addOutput renderFun


unVar :: (Term, Bool) -> Either TmName (Term, Bool)
unVar (Var v, False) = Left v
unVar t              = Right t

verifyBlackBoxContext ::
  Primitive
  -> BlackBoxContext
  -> Bool
verifyBlackBoxContext p bbCtx =
  (length (B.inputs bbCtx)    == P.inputs p) &&
  (length (B.litInputs bbCtx) >= P.litInputs p) &&
  (length (B.funInputs bbCtx) >= P.funInputs p)

mkBlackBoxDecl ::
  Primitive
  -> BlackBoxContext
  -> NetlistMonad [Declaration]
mkBlackBoxDecl p bbCtx = case (verifyBlackBoxContext p bbCtx) of
  True -> do
    prims <- LabelM.gets primitives
    tmpl <- liftIO $ evalStateT (renderBlackBox p bbCtx) (BBState prims bbCtx Nothing)
    return [HW.BlackBox tmpl]
  False -> error $ $(curLoc) ++ "\nCan't match context:\n" ++ show bbCtx ++ "\nwith template:\n" ++ show p

mkBlackBoxI ::
  Primitive
  -> BlackBoxContext
  -> NetlistMonad LZ.ByteString
mkBlackBoxI p bbCtx = case (verifyBlackBoxContext p bbCtx) of
  True -> do
    prims <- LabelM.gets primitives
    liftIO $ evalStateT (renderBlackBox p bbCtx) (BBState prims bbCtx Nothing)
  False -> error $ $(curLoc) ++ "\nCan't match context:\n" ++ show bbCtx ++ "\nwith template:\n" ++ show p

mkInput ::
  Id
  -> (Term, Bool)
  -> NetlistMonad (Maybe Identifier)
mkInput _ ((Var v), _) = do
  let vT = mkBasicId . pack $ name2String v
  return $! Just vT

mkInput resId (e@(App _ _), False)
  | (Prim (PrimFun nm _), args) <- collectArgs e
  = do
    bbM <- fmap (HashMap.lookup . LZ.pack $ name2String nm) $ LabelM.gets primitives
    case bbM of
      Just p@(P.BlackBox {}) -> do
        bbCtx <- mkBlackBoxContext resId (lefts args)
        bb <- mkBlackBoxI p bbCtx
        return $! Just (decodeUtf8 bb)
      _ -> error $ "No blackbox found: " ++ show bbM

mkInput resId (e@(App _ _), True)
  | (Prim (PrimFun nm _), _) <- collectArgs e
  = let nmT = pack $ name2String nm
    in return $! Just nmT

mkInput _ (e, _) = return $! mkLitInput e

mkLitInput ::
  Term
  -> Maybe Identifier
mkLitInput (C.Literal (IntegerLiteral i)) = Just (pack $ show i)
mkLitInput _ = Nothing

mkFunInput ::
  Term
  -> Maybe Identifier
mkFunInput e@(App _ _)
  | (Prim (PrimFun nm _), _) <- collectArgs e
  = let nmT = pack $ name2String nm
    in Just nmT

mkFunInput _ = Nothing
