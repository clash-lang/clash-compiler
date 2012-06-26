{-# LANGUAGE PatternGuards #-}
module CLaSH.Netlist.Util where

import Data.Text.Lazy (pack)
import Data.Either             (partitionEithers)
import qualified Data.Label.PureM as LabelM
import Unbound.LocallyNameless (name2String,unbind,unrec)

import CLaSH.Core.DataCon      (DataCon(..),dcRepArgTys)
import CLaSH.Core.FreeVars     (typeFreeVars)
import CLaSH.Core.Subst        (substTys)
import CLaSH.Core.Term         (LetBinding,Term(..),TmName)
import CLaSH.Core.TyCon        (TyCon(..),tyConDataCons)
import CLaSH.Core.Type         (Type,splitTyConAppM)
import CLaSH.Core.Util         (collectBndrs,termType)
import CLaSH.Core.Var          (Id)
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

typeToHWType ::
  Type
  -> Either String HWType
typeToHWType ty
  | (_:_) <- typeFreeVars ty = fail "Can't translate types with free type variables"
  | otherwise = do
    case splitTyConAppM ty of
      Just (tyCon, args) -> do
        case (name2String $ tyConName tyCon) of
          "Bit"  -> return Bit
          "Bool" -> return Bool
          _      -> mkADT tyCon args
      Nothing -> fail "Can't translate type"

mkADT ::
  TyCon
  -> [Type]
  -> Either String HWType
mkADT tc args = case tyConDataCons tc of
  []  -> fail "There are no DataCons for the type"
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
translatableType = (either (const False) (const True)) . typeToHWType

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
  -> Integer
typeSize Bool = 1
typeSize (SP _ cons) = (ceiling . logBase 2 . fromIntegral $ length cons) +
                       (maximum $ map (sum . map typeSize . snd) cons)
typeSize _ = error "typeSize"

termHWType ::
  Term
  -> NetlistMonad HWType
termHWType e = do
  eType <- (`termType` e) =<< (LabelM.gets varEnv)
  either error return $ typeToHWType eType

varToExpr ::
  Term
  -> Expr
varToExpr (Var var) = Identifier (pack $ name2String var) Nothing
varToExpr _         = error "not a var"
