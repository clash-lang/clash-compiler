{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns  #-}
module CLaSH.Netlist.Util where

import qualified Control.Monad as Monad
import Data.Text.Lazy (pack)
import Data.Either             (partitionEithers)
import Data.Maybe              (catMaybes)
import Unbound.LocallyNameless (Fresh,bind,embed,makeName,name2String,name2Integer,unbind,unembed,unrec)

import CLaSH.Core.DataCon      (DataCon(..))
import CLaSH.Core.FreeVars     (typeFreeVars)
import CLaSH.Core.Pretty       (showDoc)
import CLaSH.Core.Subst        (substTys)
import CLaSH.Core.Term         (LetBinding,Term(..),TmName)
import CLaSH.Core.TyCon        (TyCon(..),tyConDataCons)
import CLaSH.Core.Type         (Type(..),TypeView(..),LitTy(..),tyView,splitTyConAppM)
import CLaSH.Core.Util         (collectBndrs,termType)
import CLaSH.Core.Var          (Var(..),Id,modifyVarName)
import CLaSH.Netlist.Types
import CLaSH.Util

splitNormalized ::
  (Fresh m,Functor m)
  => Term
  -> m ([Id],[LetBinding],TmName)
splitNormalized expr = do
  (args,letExpr) <- fmap (first partitionEithers) $ collectBndrs expr
  case (letExpr) of
    Letrec b
      | (tmArgs,[]) <- args -> do
          (xes,e) <- unbind b
          case e of
            Var _ v -> return (tmArgs,unrec xes,v)
            _ -> error "Not in normal form: res not simple var"
      | otherwise -> error "Not in normal form: tyArgs"
    _ -> error ("Not in normal from: no Letrec: " ++ showDoc expr)

coreTypeToHWType_fail ::
  Type
  -> HWType
coreTypeToHWType_fail = either error id . coreTypeToHWType

synchronizedClk ::
  Type
  -> Maybe Identifier
synchronizedClk ty
  | not . null . typeFreeVars $ ty = Nothing
  | Just (tyCon,args) <- splitTyConAppM ty
  = case (name2String $ tyConName tyCon) of
      "CLaSH.Signal.Sync"       -> Just (pack "clk")
      "CLaSH.Sized.VectorZ.Vec" -> synchronizedClk (args!!1)
      "CLaSH.Signal.Packed"     -> synchronizedClk (head args)
      _                         -> Nothing
  | otherwise
  = Nothing

coreTypeToHWType ::
  Type
  -> Either String HWType
coreTypeToHWType ty@(tyView -> TyConApp tc args) =
  case (name2String $ tyConName tc) of
    "GHC.Integer.Type.Integer"  -> Left $ "Can't translate type: " ++ showDoc ty -- return Integer
    "GHC.Prim.Int#"             -> Left $ "Can't translate type: " ++ showDoc ty -- return Integer
    "GHC.Prim.ByteArray#"       -> Left $ "Can't translate type: " ++ showDoc ty -- return Integer
    "GHC.Types.Bool"            -> return Bool
    "GHC.TypeLits.Sing"         -> Left $ "Can't translate type: " ++ showDoc ty -- singletonToHWType (head args)
    "GHC.Prim.~#"               -> Left $ "Can't translate type: " ++ showDoc ty
    "CLaSH.Bit.Bit"             -> return Bit
    "CLaSH.Signal.Sync"         -> coreTypeToHWType (head args)
    "CLaSH.Signal.Packed"       -> coreTypeToHWType (head args)
    "CLaSH.Signed.Pack"         -> Left $ "Can't translate type: " ++ showDoc ty
    "CLaSH.Sized.Signed.Signed" -> Signed <$> (tyNatSize $ head args)
    "CLaSH.Sized.VectorZ.Vec"   -> do
      let [szTy,elTy] = args
      sz     <- tyNatSize szTy
      elHWTy <- coreTypeToHWType elTy
      return $ Vector sz elHWTy
    _ -> mkADT (showDoc ty) tc args

coreTypeToHWType ty = Left $ "Can't translate type: " ++ showDoc ty

singletonToHWType ::
  Type
  -> Either String HWType
singletonToHWType (tyView -> TyConApp tc [])
  | (name2String $ tyConName tc) == "GHC.TypeLits.Nat"
  = return Integer

singletonToHWType ty = Left $ "Can't translate singleton type: " ++ showDoc ty

mkADT ::
  String
  -> TyCon
  -> [Type]
  -> Either String HWType
mkADT tyString tc args
  | isRecursiveTy tc args
  = Left $ $(curLoc) ++ "Can't translate recursive type: " ++ tyString

mkADT _ tc args = case tyConDataCons tc of
  []  -> return Void
  dcs -> do
    let argTyss      = map dcArgTys dcs
    let substArgTyss = (map . map) (substTys tvsArgsMap) argTyss
    argHTyss         <- mapM (mapM coreTypeToHWType) substArgTyss
    let nonEmptyArgs = map (filter (not . isEmptyType)) argHTyss
    case (dcs,nonEmptyArgs) of
      ([_],[[elemTy]])   -> return elemTy
      ([_],[elemTys])    -> return $ Product tcName elemTys
      (_  ,concat -> []) -> return $ Sum tcName $ map (pack . name2String . dcName) dcs
      (_  ,elemHTys)     -> return $ SP tcName
                                   $ zipWith (\dc tys ->
                                               ( pack . name2String $ dcName dc
                                               , tys
                                               )
                                             ) dcs elemHTys
  where
    tcName     = pack . name2String $ tyConName tc
    tvs        = tyConTyVars tc
    tvsArgsMap = zip tvs args

isRecursiveTy :: TyCon -> [Type] -> Bool
isRecursiveTy tc args = case tyConDataCons tc of
    []  -> False
    dcs -> let argTyss      = map dcArgTys dcs
               tvs          = tyConTyVars tc
               tvsArgsMap   = zip tvs args
               substArgTyss = (map . map) (substTys tvsArgsMap) argTyss
               argTcs       = map fst . catMaybes . map splitTyConAppM $ concat substArgTyss
           in tc `elem` argTcs

tyNatSize ::
  Type
  -> Either String Int
tyNatSize (LitTy (NumTy i)) = return i
tyNatSize ((tyView -> TyConApp tc [ty1,ty2]))
                            = case (name2String $ tyConName tc) of
                                "GHC.TypeLits.+" -> (+) <$> tyNatSize ty1 <*> tyNatSize ty2
                                _ -> Left $ $(curLoc) ++ "Can't convert tyNatOp: " ++ show tc
tyNatSize t                 = Left $ $(curLoc) ++ "Can't convert tyNat: " ++ show t

representableType ::
  Type
  -> Bool
representableType = (either (\s -> traceIf True s False) (const True)) . coreTypeToHWType

isEmptyType ::
  HWType
  -> Bool
isEmptyType Void           = True
isEmptyType (Sum _ (_:[])) = True
isEmptyType (Vector 0 _)   = True
isEmptyType _              = False

typeSize ::
  HWType
  -> Int
typeSize Void = 0
typeSize Bool = 1
typeSize Integer = 32
typeSize (Signed i) = i
typeSize (Vector n el) = n * (typeSize el)
typeSize (SP _ cons) =
  (ceiling . logBase (2 :: Float) . fromIntegral $ length cons) +
  (maximum $ map (sum . map typeSize . snd) cons)
typeSize (Sum _ dcs) = ceiling . logBase (2 :: Float) . fromIntegral $ length dcs
typeSize (Product _ tys) = sum $ map typeSize tys
typeSize _ = 0

typeLength ::
  HWType
  -> Int
typeLength (Vector n _) = n
typeLength _            = 0

termHWType ::
  Term
  -> NetlistMonad HWType
termHWType e = coreTypeToHWType_fail <$> termType e

varToExpr ::
  Term
  -> Expr
varToExpr (Var _ var) = Identifier (pack $ name2String var) Nothing
varToExpr _           = error "not a var"

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
        varCnt <- varCount <%= (+1)
        let v' = modifyVarName (`appendToName` ("_" ++ show varCnt)) v
        return v'

    subsBndrs :: [Term] -> (Id,Id) -> NetlistMonad [Term]
    subsBndrs es (f,r) = mapM (subsBndr f r) es

    subsBndr :: Id -> Id -> Term -> NetlistMonad Term
    subsBndr f r e = case e of
      Var t v | v == (varName f) -> return . Var t $ varName r
      App e1 e2                  -> App <$> subsBndr f r e1
                                        <*> subsBndr f r e2
      Case scrut ty alts         -> Case <$> (subsBndr f r scrut)
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
