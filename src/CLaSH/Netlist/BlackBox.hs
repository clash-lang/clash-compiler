module CLaSH.Netlist.BlackBox where

import           Control.Monad.IO.Class (liftIO)
import           Data.Either       (partitionEithers)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Label.PureM  as LabelM
import           Data.Maybe        (catMaybes)
import           Data.Text.Lazy    (pack)
import qualified Text.Hastache     as Hastache
import qualified Text.Hastache.Context as Hastache
import           Unbound.LocallyNameless (name2String,unembed)

import CLaSH.Core.Term (Term(..),TmName)
import CLaSH.Core.Type (Type)
import CLaSH.Core.Var  as V (Id,Var(..))
import CLaSH.Netlist.BlackBox.Types as B
import CLaSH.Netlist.Types as HW
import CLaSH.Netlist.Util
import CLaSH.Netlist.VHDL
import CLaSH.Primitives.Types as P
import CLaSH.Util

mkBlackBoxContext ::
  Id
  -> [Term]
  -> NetlistMonad BlackBoxContext
mkBlackBoxContext resId args = do
  let (varArgs,_) = partitionEithers $ map unVar args
  gamma <- LabelM.gets varEnv
  let varInps = zipWith mkVarInput varArgs (map (gamma HashMap.!) varArgs)
  case mkVarInput (V.varName resId) (unembed $ varType resId) of
    Nothing  -> error "can't make blackbox"
    Just res -> return $ Context res (catMaybes varInps) []

unVar :: Term -> Either TmName Term
unVar (Var v) = Left v
unVar t       = Right t

verifyBlackBoxContext ::
  Primitive
  -> BlackBoxContext
  -> Bool
verifyBlackBoxContext p bbCtx =
  (length (B.varInputs bbCtx) == P.varInputs p) &&
  (length (B.litInputs bbCtx) == P.litInputs p)

mkBlackBoxDecl ::
  Primitive
  -> BlackBoxContext
  -> NetlistMonad [Declaration]
mkBlackBoxDecl p bbCtx = case (verifyBlackBoxContext p bbCtx) of
  True  -> liftIO $ do
    let hastacheCtx = Hastache.mkGenericContext bbCtx
    tmpl <- Hastache.hastacheStr (Hastache.defaultConfig)
              (template p) hastacheCtx
    return [HW.BlackBox tmpl]
  False -> error "Can't match context against template"

mkVarInput ::
  TmName
  -> Type
  -> (Maybe VarInput)
mkVarInput nm ty = do
  let nmT = pack . mkVHDLBasicId $ name2String nm
  case typeToHWType ty of
    Left errMsg -> traceIf True errMsg Nothing
    Right hwTy  -> Just (VarInput nmT (typeSize hwTy) (typeLength hwTy))
