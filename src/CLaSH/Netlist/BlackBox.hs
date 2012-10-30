module CLaSH.Netlist.BlackBox where

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
import CLaSH.Core.Util (collectArgs)
import CLaSH.Core.Var  as V (Id,Var(..))
import CLaSH.Normalize.Util (isSimple)
import CLaSH.Netlist.BlackBox.Types as B
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
  varInps   <- mapM (mkInput resId) args
  case mkVarInput (V.varName resId) (unembed $ varType resId) of
    Nothing  -> error "can't make blackbox"
    Just res -> return $ Context res (catMaybes varInps) (catMaybes litInps)
  where
    (_,otherArgs) = partitionEithers $ map unVar args
    (litArgs,_)   = partition isSimple otherArgs
    litInps       = map mkLitInput litArgs

unVar :: Term -> Either TmName Term
unVar (Var v) = Left v
unVar t       = Right t

verifyBlackBoxContext ::
  Primitive
  -> BlackBoxContext
  -> Bool
verifyBlackBoxContext p bbCtx =
  (length (B.inputs bbCtx)    == P.inputs p) &&
  (length (B.litInputs bbCtx) >= P.litInputs p)

mkBlackBoxDecl ::
  Primitive
  -> BlackBoxContext
  -> NetlistMonad [Declaration]
mkBlackBoxDecl p bbCtx = case (verifyBlackBoxContext p bbCtx) of
  True -> liftIO $ do
    let hastacheCtx = Hastache.mkGenericContext bbCtx
    tmpl <- Hastache.hastacheStr (Hastache.defaultConfig)
              (template p) hastacheCtx
    return [HW.BlackBox tmpl]
  False -> error $ $(curLoc) ++ "\nCan't match context:\n" ++ show bbCtx ++ "\nwith template:\n" ++ show p

mkBlackBoxI ::
  Primitive
  -> BlackBoxContext
  -> NetlistMonad LZ.ByteString
mkBlackBoxI p bbCtx = case (verifyBlackBoxContext p bbCtx) of
  True -> liftIO $ do
    let hastacheCtx = Hastache.mkGenericContext bbCtx
    tmplI <- Hastache.hastacheStr (Hastache.defaultConfig)
              (templateI p) hastacheCtx
    return tmplI
  False -> error $ $(curLoc) ++ "\nCan't match context:\n" ++ show bbCtx ++ "\nwith template:\n" ++ show p

mkInput ::
  Id
  -> Term
  -> NetlistMonad (Maybe VarInput)
mkInput _ (Var v) = do
  gamma     <- LabelM.gets varEnv
  return $! mkVarInput v (gamma HashMap.! v)

mkInput resId e@(App _ _)
  | (Prim (PrimFun nm _), args) <- collectArgs e
  = do
    bbM <- fmap (HashMap.lookup . LZ.pack $ name2String nm) $ LabelM.gets primitives
    case bbM of
      Just p@(P.BlackBox {}) -> do
        bbCtx <- mkBlackBoxContext resId (lefts args)
        bb <- mkBlackBoxI p bbCtx
        return $! (Just (VarInput (decodeUtf8 bb) 0 0))
      _ -> error $ "No blackbox found: " ++ show bbM
mkInput _ e       = return $! mkVarLitInput e

mkVarInput ::
  TmName
  -> Type
  -> Maybe VarInput
mkVarInput nm ty = do
  let nmT = mkBasicId . pack $ name2String nm
  case typeToHWType ty of
    Left errMsg -> traceIf True errMsg Nothing
    Right hwTy  -> Just (VarInput nmT (typeSize hwTy) (typeLength hwTy))

mkVarLitInput ::
  Term
  -> Maybe VarInput
mkVarLitInput (C.Literal (IntegerLiteral i)) = Just (VarInput (pack $ show i) 0 0)
mkVarLitInput _ = Nothing

mkLitInput ::
  Term
  -> Maybe LitInput
mkLitInput (C.Literal (IntegerLiteral i)) = Just (LitInput i)
mkLitInput _ = Nothing
