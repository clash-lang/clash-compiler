module CLaSH.Netlist.BlackBox where

import           Control.Monad.IO.Class (liftIO)
import           Data.Either       (partitionEithers)
import qualified Data.HashMap.Lazy as HashMap
import           Data.List         (partition)
import qualified Data.Label.PureM  as LabelM
import           Data.Maybe        (catMaybes)
import           Data.Text.Lazy    (pack)
import qualified Text.Hastache     as Hastache
import qualified Text.Hastache.Context as Hastache
import           Unbound.LocallyNameless (name2String,unembed)

import CLaSH.Core.Literal as L (Literal(..))
import CLaSH.Core.Term as C (Term(..),TmName)
import CLaSH.Core.Type (Type)
import CLaSH.Core.Util (Gamma)
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
  let (_,otherArgs) = partitionEithers $ map unVar args
  let (litArgs,_)   = partition isSimple otherArgs
  gamma <- LabelM.gets varEnv
  let varInps = map (mkInput gamma) args
  let litInps = map mkLitInput litArgs
  case mkVarInput (V.varName resId) (unembed $ varType resId) of
    Nothing  -> error "can't make blackbox"
    Just res -> return $ Context res (catMaybes varInps) (catMaybes litInps)

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

mkInput ::
  Gamma
  -> Term
  -> Maybe VarInput
mkInput g (Var v) = mkVarInput v (g HashMap.! v)
mkInput _ e       = mkVarLitInput e

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
