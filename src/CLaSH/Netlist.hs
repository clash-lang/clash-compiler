module CLaSH.Netlist where

import qualified Control.Monad           as Monad
import           Control.Monad.State     (evalStateT)
import           Data.Text.Lazy          (pack)
import           Data.Either             (partitionEithers)
import           Data.HashMap.Lazy       (HashMap)
import qualified Data.HashMap.Lazy       as HashMap
import qualified Data.Label.PureM        as LabelM
import           Data.List               (elemIndex)
import           Data.Maybe              (fromMaybe)
import           Control.Monad.Identity  (runIdentity)
import           Unbound.LocallyNameless (Embed(..),name2String,runFreshMT,unembed)

import CLaSH.Core.DataCon   (DataCon(..))
import CLaSH.Core.Term      (Term(..),TmName)
import CLaSH.Core.Type      (Type)
import CLaSH.Core.Util      (collectArgs,isVar,termType)
import CLaSH.Core.Var       (Id,Var(..))
import CLaSH.Netlist.Types
import CLaSH.Netlist.Util
import CLaSH.Util

runNetlistMonad ::
  HashMap TmName (Type,Term)
  -> NetlistMonad a
  -> a
runNetlistMonad s
  = runIdentity
  . runFreshMT
  . (flip evalStateT) s'
  . runNetlist
  where
    s' = NetlistState s HashMap.empty

genComponent ::
  TmName
  -> Term
  -> NetlistMonad Component
genComponent componentName componentExpr = do
  (arguments,binders,result) <- splitNormalized componentExpr
  let ids = HashMap.fromList
          $ map (\(Id v (Embed t)) -> (v,t))
          $ arguments ++ (map fst binders)

  gamma <- (ids `HashMap.union`) . (HashMap.map fst)
           <$> LabelM.gets bindings

  LabelM.puts varEnv gamma

  let toHWType = either error id . typeToHWType

  let resType  = toHWType $ ids HashMap.! result
  let argTypes = map (\(Id _ (Embed t)) -> toHWType t) arguments


  decls <- concat <$> mapM (uncurry mkConcSm . second unembed) binders

  let componentName' = pack $ name2String componentName
  let compInps       = zip (map (pack . name2String . varName) arguments) argTypes
  let compOutp       = (pack $ name2String result, resType)
  let component      = Component componentName' compInps compOutp decls

  return component

mkConcSm ::
  Id
  -> Term
  -> NetlistMonad [Declaration]
mkConcSm bndr (Var v) = mkApplication bndr v []

mkConcSm bndr app@(App _ _) = do
  let (appF,(args,[])) = second partitionEithers $ collectArgs app
  gamma <- LabelM.gets varEnv
  args' <- Monad.filterM (fmap representableType . termType gamma) args
  case appF of
    Var f
      | all isVar args' -> mkApplication bndr f args'
      | otherwise       -> error "Not in normal form: Var-application with non-Var arguments"
    Data dc
      | all isVar args' -> mkDcApplication bndr dc args'
      | otherwise       -> error "Not in normal form: DataCon-application with non-Var arguments"
    _ -> error "Not in normal form: application of a Let/Lam/Case"

mkConcSm _ _ = error "Not in normal form: let-bound expr is a Let or TyApp"

mkApplication ::
  Id
  -> TmName
  -> [Term]
  -> NetlistMonad [Declaration]
mkApplication dst fun args = do
  error "mkApplication undefined"

mkDcApplication ::
  Id
  -> DataCon
  -> [Term]
  -> NetlistMonad [Declaration]
mkDcApplication dst dc args = do
  let dstHType = either error id . typeToHWType . unembed $ varType dst
  let dstId    = pack . show $ varName dst
  case dstHType of
    SP _ dcArgPairs -> do
      let dcNameBS = pack . show $ dcName dc
      let dcI      = fromMaybe (error "dc not found") $ elemIndex dcNameBS $ map fst dcArgPairs
      let argTys   = snd $ dcArgPairs !! dcI
      nonEmptyArgs <- Monad.filterM ( return
                                    . not
                                    . isEmptyType
                                    <=< termHWType
                                    ) args
      let nonEmptyArgs' = map varToExpr nonEmptyArgs
      case (compare (length argTys) (length nonEmptyArgs)) of
        EQ -> return [Assignment dstId (Just $ DC dcI) dstHType nonEmptyArgs']
        LT -> error "Over-applied constructor"
        GT -> error "Under-applied constructor"
    _ -> error "mkDcApplication undefined"
