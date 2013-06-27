{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns  #-}
module CLaSH.Netlist.Util where

import           Control.Lens  ((.=),_2)
import qualified Control.Lens  as Lens
import qualified Control.Monad as Monad
import Data.Text.Lazy          (pack)
import Data.Either             (partitionEithers)
import Unbound.LocallyNameless (Fresh,bind,embed,makeName,name2String,name2Integer,unbind,unembed,unrec)

import CLaSH.Core.DataCon      (DataCon(..))
import CLaSH.Core.FreeVars     (typeFreeVars,termFreeIds)
import CLaSH.Core.Pretty       (showDoc)
import CLaSH.Core.Subst        (substTys)
import CLaSH.Core.Term         (LetBinding,Term(..),TmName)
import CLaSH.Core.TyCon        (TyCon(..),tyConDataCons)
import CLaSH.Core.Type         (Type(..),TypeView(..),LitTy(..),mkTyConApp,tyView,splitTyConAppM)
import CLaSH.Core.Util         (collectBndrs,termType)
import CLaSH.Core.Var          (Var(..),Id,modifyVarName)
import CLaSH.Netlist.Types
import CLaSH.Util

splitNormalized ::
  (Fresh m,Functor m)
  => Term
  -> m (Either String ([Id],[LetBinding],Id))
splitNormalized expr = do
  (args,letExpr) <- fmap (first partitionEithers) $ collectBndrs expr
  case (letExpr) of
    Letrec b
      | (tmArgs,[]) <- args -> do
          (xes,e) <- unbind b
          case e of
            Var t v -> return $! Right (tmArgs,unrec xes,Id v (embed t))
            _ -> return $! Left ($(curLoc) ++ "Not in normal form: res not simple var")
      | otherwise -> return $! Left ($(curLoc) ++ "Not in normal form: tyArgs")
    _ -> return $! Left ($(curLoc) ++ "Not in normal from: no Letrec: " ++ showDoc expr)

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
      "CLaSH.Signal.Packed"     -> Just (pack "clk")
      _                         -> Nothing
  | otherwise
  = Nothing

coreTypeToHWType ::
  Type
  -> Either String HWType
coreTypeToHWType ty@(tyView -> TyConApp tc args) =
  case (name2String $ tyConName tc) of
    "GHC.Integer.Type.Integer"      -> return Integer
    "GHC.Prim.Int#"                 -> return Integer
    "GHC.Prim.Int"                  -> return Integer
    "GHC.Prim.ByteArray#"           -> Left $ "Can't translate type: " ++ showDoc ty
    "GHC.Types.Bool"                -> return Bool
    "GHC.TypeLits.Sing"             -> singletonToHWType (head args) -- Left $ "Can't translate type: " ++ showDoc ty
    "GHC.Prim.~#"                   -> Left $ "Can't translate type: " ++ showDoc ty
    "CLaSH.Bit.Bit"                 -> return Bit
    "CLaSH.Signal.Pack"             -> Left $ "Can't translate type: " ++ showDoc ty
    "CLaSH.Signal.Sync"             -> coreTypeToHWType (head args)
    "CLaSH.Signal.Packed"           -> coreTypeToHWType (head args)
    "CLaSH.Sized.Signed.Signed"     -> Signed <$> (tyNatSize $ head args)
    "CLaSH.Sized.Unsigned.Unsigned" -> Unsigned <$> (tyNatSize $ head args)
    "CLaSH.Sized.VectorZ.Vec"       -> do
      let [szTy,elTy] = args
      sz     <- tyNatSize szTy
      elHWTy <- coreTypeToHWType elTy
      return $ Vector sz elHWTy
    _ -> mkADT (showDoc ty) tc args

coreTypeToHWType ty = Left $ "Can't translate non tycon-type: " ++ showDoc ty

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
    let argTVs       = map dcUnivTyVars dcs
    let substArgTyss = (map . map) (substTys (tvsArgsMap argTVs)) argTyss
    argHTyss         <- mapM (mapM coreTypeToHWType) substArgTyss
    case (dcs,argHTyss) of
      (_:[],[elemTys@(_:_)]) -> return $ Product tcName elemTys
      (_   ,concat -> [])    -> return $ Sum tcName $ map (pack . name2String . dcName) dcs
      (_   ,elemHTys)        -> return $ SP tcName
                                      $ zipWith (\dc tys ->
                                                  ( pack . name2String $ dcName dc
                                                  , tys
                                                  )
                                                ) dcs elemHTys
  where
    tcName     = pack . name2String $ tyConName tc
    tvs        = tyConTyVars tc
    tvsArgsMap dcTVs = [ (a,t) | a <- concat dcTVs , (a',t) <- (zip tvs args), name2String a == name2String a' ]

isRecursiveTy :: TyCon -> [Type] -> Bool
isRecursiveTy tc args = case tyConDataCons tc of
    []  -> False
    dcs -> let argTyss      = map dcArgTys dcs
               tvs          = tyConTyVars tc
               tvsArgsMap   = zip tvs args
               substArgTyss = (concatMap . map) (substTys tvsArgsMap) argTyss
           in (mkTyConApp tc args) `elem` substArgTyss

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
representableType = (either (const False) (const True)) . coreTypeToHWType

typeSize ::
  HWType
  -> Int
typeSize Void = 0
typeSize Bool = 1
typeSize (Clock _) = 1
typeSize (Reset _) = 1
typeSize Integer = 32
typeSize (Signed i) = i
typeSize (Unsigned i) = i
typeSize (Vector n el) = n * (typeSize el)
typeSize t@(SP _ cons) = conSize t +
  (maximum $ map (sum . map typeSize . snd) cons)
typeSize (Sum _ dcs) = ceiling . logBase (2 :: Float) . fromIntegral $ length dcs
typeSize (Product _ tys) = sum $ map typeSize tys
typeSize _ = 0

conSize :: HWType
        -> Int
conSize (SP _ cons) = ceiling . logBase (2 :: Float) . fromIntegral $ length cons
conSize t           = typeSize t

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
  ([Id],[LetBinding],Id)
  -> NetlistMonad ([Id],[LetBinding],TmName)
mkUniqueNormalized (args,binds,res) = do
  let args' = zipWith (\n s -> modifyVarName (`appendToName` s) n)
                args ["_i" ++ show i | i <- [(1::Integer)..]]
  let res1  = appendToName (varName res) "_o"
  let bndrs = map fst binds
  let exprs = map (unembed . snd) binds
  let usesOutput = concatMap (filter (== (varName res)) . termFreeIds) exprs
  let (res2,extraBndr) = case usesOutput of
                            [] -> (res1,[])
                            _  -> let res3 = appendToName (varName res) "_o_sig"
                                  in (res3,[(Id res1 (varType res),embed $ Var (unembed $ varType res) res3)])
  bndrs' <- mapM (mkUnique (varName res,res2)) bndrs
  let repl = (zip args args') ++ (zip bndrs bndrs')
  exprs' <- fmap (map embed) $ Monad.foldM subsBndrs exprs repl
  return (args',zip bndrs' exprs' ++ extraBndr,res1)

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

preserveVHDLState ::
  NetlistMonad a
  -> NetlistMonad a
preserveVHDLState action = do
  vCnt <- Lens.use varCount
  vEnv <- Lens.use varEnv
  cN   <- Lens.use (vhdlMState . _2)
  val  <- action
  varCount          .= vCnt
  varEnv            .= vEnv
  (vhdlMState . _2) .= cN
  return val
