{-# LANGUAGE PatternGuards #-}
module CLaSH.Netlist.Util where

import qualified Control.Monad as Monad
import Data.Text.Lazy (pack)
import Data.Either             (partitionEithers)
import qualified Data.Label.PureM as LabelM
import Unbound.LocallyNameless (bind,embed,makeName,name2String,name2Integer,unbind,unembed,unrec)

import CLaSH.Core.DataCon      (DataCon(..),dcRepArgTys)
import CLaSH.Core.FreeVars     (typeFreeVars)
import CLaSH.Core.Subst        (substTys)
import CLaSH.Core.Term         (LetBinding,Term(..),TmName)
import CLaSH.Core.TyCon        (TyCon(..),tyConDataCons)
import CLaSH.Core.Type         (Type,splitTyConAppM)
import CLaSH.Core.Util         (collectBndrs,termType)
import CLaSH.Core.Var          (Var(..),Id,modifyVarName)
import CLaSH.Netlist.Types
import CLaSH.Util

splitNormalized ::
  Term
  -> NetlistMonad ([Id],[LetBinding],TmName)
splitNormalized expr = do
  (args,letExpr) <- fmap (first partitionEithers) $ collectBndrs expr
  case (letExpr) of
    Letrec b
      | (tmArgs,[]) <- args -> do
          (xes,e) <- unbind b
          case e of
            Var v -> return (tmArgs,unrec xes,v)
            _ -> error "Not in normal form: res not simple var"
      | otherwise -> error "Not in normal form: tyArgs"
    _ -> error "Not in normal from: no Letrec"

typeToHWType_fail ::
  Type
  -> HWType
typeToHWType_fail = either error id . typeToHWType

typeToHWType ::
  Type
  -> Either String HWType
typeToHWType ty
  | (_:_) <- typeFreeVars ty = Left "Can't translate types with free type variables"
  | otherwise = do
    case splitTyConAppM ty of
      Just (tyCon, args) -> do
        case (name2String $ tyConName tyCon) of
          "GHC.Integer.Type.Integer" -> return Integer
          "GHC.Prim.Int#" -> return Integer
          "GHC.Prim.ByteArray#" -> return Integer
          "GHC.Types.Bool" -> return Bool
          _      -> mkADT tyCon args
      Nothing -> Left "Can't translate type"

mkADT ::
  TyCon
  -> [Type]
  -> Either String HWType
mkADT tc args = case tyConDataCons tc of
  []  -> Left $ "There are no DataCons for the type: " ++ (show (tc,args))
  dcs -> do
    let argTyss = map dcRepArgTys dcs
    let sumTy   = Sum tcName $ map (pack . show . dcName) dcs
    case (concat argTyss) of
      [] -> return sumTy
      _  -> do
        let substArgTyss = (map . map) (substTys tvsArgsMap) argTyss
        argHTyss <- mapM (mapM typeToHWType) substArgTyss
        case (dcs, map (filter (not . isEmptyType)) argHTyss) of
          ([_],[[elemTy]]) -> return elemTy
          ([_],[elemTys])  -> return $ Product tcName elemTys
          (_  ,elemHTys)    -> return $ SP tcName
                                      $ zipWith (\dc tys ->
                                                  ( pack . show $ dcName dc
                                                  , tys
                                                  )
                                                ) dcs elemHTys
  where
    tcName       = pack . show $ tyConName tc
    tvs        = tyConTyVars tc
    tvsArgsMap = zip tvs args

translatableType ::
  Type
  -> Bool
translatableType = (either (\s -> traceIf True s False) (const True)) . typeToHWType

representableType ::
  Type
  -> Bool
representableType = translatableType

isEmptyType ::
  HWType
  -> Bool
isEmptyType (Sum _ (_:[])) = True
isEmptyType _              = False

typeSize ::
  HWType
  -> Int
typeSize Bool = 1
typeSize Integer = 32
typeSize (SP _ cons) =
  (ceiling . logBase (2 :: Float) . fromIntegral $ length cons) +
  (maximum $ map (sum . map typeSize . snd) cons)
typeSize _ = error "typeSize"

typeLength ::
  HWType
  -> Int
typeLength (Vector n _) = n
typeLength _            = 0

termHWType ::
  Term
  -> NetlistMonad HWType
termHWType e = do
  eType <- (`termType` e) =<< (LabelM.gets varEnv)
  return $ typeToHWType_fail eType

varToExpr ::
  Term
  -> Expr
varToExpr (Var var) = Identifier (pack $ name2String var) Nothing
varToExpr _         = error "not a var"

mkUniqueNormalized ::
  ([Id],[LetBinding],TmName)
  -> NetlistMonad ([Id],[LetBinding],TmName)
mkUniqueNormalized (args,binds,res) = do
  let args' = zipWith (\n s -> modifyVarName (`appendToName` s) n)
                args ["_i" ++ show i | i <- [(1::Integer)..]]
  let res'  = appendToName res "_o"
  let bndrs = map fst binds
  let exprs = map (unembed . snd) binds
  bndrs' <- mapM (mkUnique (res,res')) bndrs
  let repl = (zip args args') ++ (zip bndrs bndrs')
  exprs' <- fmap (map embed) $ Monad.foldM subsBndrs exprs repl
  return (args',zip bndrs' exprs',res')

  where
    mkUnique :: (TmName,TmName) -> Id -> NetlistMonad Id
    mkUnique (find,repl) v = case (find == varName v) of
      True  -> return $ modifyVarName (const repl) v
      False -> do
        varCnt <- getAndModify varCount (+1)
        let v' = modifyVarName (`appendToName` ("_" ++ show varCnt)) v
        return v'

    subsBndrs :: [Term] -> (Id,Id) -> NetlistMonad [Term]
    subsBndrs es (f,r) = mapM (subsBndr f r) es

    subsBndr :: Id -> Id -> Term -> NetlistMonad Term
    subsBndr f r e = case e of
      Var v | v == (varName f) -> return . Var $ varName r
      App e1 e2                -> App <$> subsBndr f r e1
                                      <*> subsBndr f r e2
      Case scrut ty alts       -> Case <$> (subsBndr f r scrut)
                                       <*> pure ty
                                       <*> mapM ( return
                                                . uncurry bind
                                                <=< secondM (subsBndr f r)
                                                <=< unbind
                                                ) alts
      _ -> return e

appendToName ::
  TmName
  -> String
  -> TmName
appendToName n s = makeName (name2String n ++ s) (name2Integer n)
