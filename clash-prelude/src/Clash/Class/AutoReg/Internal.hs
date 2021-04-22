{-|
  Copyright   :  (C) 2019, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- needed for constraint on the Fixed instance

module Clash.Class.AutoReg.Internal
  ( AutoReg (..)
  , deriveAutoReg
  , deriveAutoRegTuples
  )
  where

import           Data.List                    (nub,zipWith4)
import           Data.Maybe                   (fromMaybe,isJust)

import           GHC.Stack                    (HasCallStack)
import           GHC.TypeNats                 (KnownNat,Nat,type (+))
import           Clash.Explicit.Signal
import           Clash.Promoted.Nat
import           Clash.Magic
import           Clash.XException             (NFDataX, deepErrorX)

import           Clash.Sized.BitVector
import           Clash.Sized.Fixed
import           Clash.Sized.Index
import           Clash.Sized.RTree
import           Clash.Sized.Signed
import           Clash.Sized.Unsigned
import           Clash.Sized.Vector           (Vec, lazyV, smap)

import           Data.Int
import           Data.Word
import           Foreign.C.Types              (CUShort)
import           Numeric.Half                 (Half)

import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Ppr

import           Control.Lens.Internal.TH     (conAppsT)
#if !(MIN_VERSION_th_abstraction(0,4,0))
import           Control.Lens.Internal.TH     (bndrName)
#endif

-- $setup
-- >>> import Data.Maybe
-- >>> import Clash.Class.BitPack (pack)
-- >>> :set -fplugin GHC.TypeLits.Normalise
-- >>> :set -fplugin GHC.TypeLits.KnownNat.Solver

-- | 'autoReg' is a "smart" version of 'register'. It does two things:
--
--   1. It splits product types over their fields. For example, given a 3-tuple,
--   the corresponding HDL will end up with three instances of a register (or
--   more if the three fields can be split up similarly).
--
--   2. Given a data type where a constructor indicates (parts) of the data will
--   (not) be updated a given cycle, it will split the data in two parts. The
--   first part will contain the "always interesting" parts (the constructor
--   bits). The second holds the "potentially uninteresting" data (the rest).
--   Both parts will be stored in separate registers. The register holding the
--   "potentially uninteresting" part will only be enabled if the constructor
--   bits indicate they're interesting.
--
--   The most important example of this is 'Maybe'. Consider @Maybe (Signed 16)@;
--   when viewed as bits, a 'Nothing' would look like:
--
--     >>> pack @(Maybe (Signed 16)) Nothing
--     0b0_...._...._...._....
--
--   and 'Just'
--
--     >>> pack @(Maybe (Signed 16)) (Just 3)
--     0b1_0000_0000_0000_0011
--
--   In the first case, Nothing, we don't particularly care about updating the
--   register holding the @Signed 16@ field, as they'll be unknown anyway. We
--   can therefore deassert its enable line.
--
-- Making Clash lay it out like this increases the chances of synthesis tools
-- clock gating the registers, saving energy.
--
-- This version of 'autoReg' will split the given data type up recursively. For
-- example, given @a :: Maybe (Maybe Int, Maybe Int)@, a total of five registers
-- will be rendered. Both the "interesting" and "uninteresting" enable lines of
-- the inner Maybe types will be controlled by the outer one, in addition to
-- the inner parts controlling their "uninteresting" parts as described in (2).
--
-- The default implementation is just 'register'. If you don't need or want
-- the special features of "AutoReg", you can use that by writing an empty instance.
--
-- > data MyDataType = ...
-- > instance AutoReg MyDataType
--
-- If you have a product type you can use 'deriveAutoReg' to derive an instance.
--
class NFDataX a => AutoReg a where
  -- | For documentation see class 'AutoReg'.
  --
  -- This is version with explicit clock/reset/enable,
  -- "Clash.Prelude" exports an implicit version of this: 'Clash.Prelude.autoReg'
  autoReg
    :: (HasCallStack, KnownDomain dom)
    => Clock dom -> Reset dom -> Enable dom
    -> a  -- ^ Reset value
    -> Signal dom a
    -> Signal dom a
  autoReg = register
  {-# INLINE autoReg #-}

instance AutoReg ()
instance AutoReg Bool

instance AutoReg Double
instance AutoReg Float
instance AutoReg CUShort
instance AutoReg Half

instance AutoReg Char

instance AutoReg Integer
instance AutoReg Int
instance AutoReg Int8
instance AutoReg Int16
instance AutoReg Int32
instance AutoReg Int64
instance AutoReg Word
instance AutoReg Word8
instance AutoReg Word16
instance AutoReg Word32
instance AutoReg Word64

instance AutoReg Bit
instance KnownNat n => AutoReg (BitVector n)
instance AutoReg (Signed n)
instance AutoReg (Unsigned n)
instance AutoReg (Index n)
instance NFDataX (rep (int + frac)) => AutoReg (Fixed rep int frac)

instance AutoReg a => AutoReg (Maybe a) where
  autoReg clk rst en initVal input =
    createMaybe <$> tagR <*> valR
   where
     tag = isJust <$> input
     tagInit = isJust initVal
     tagR = register clk rst en tagInit tag

     val = fromMaybe (deepErrorX "autoReg'.val") <$> input
     valInit = fromMaybe (deepErrorX "autoReg'.valInit") initVal

     valR = autoReg clk rst (enable en tag) valInit val

     createMaybe t v = case t of
       True -> Just v
       False -> Nothing
  {-# INLINE autoReg #-}

instance (KnownNat n, AutoReg a) => AutoReg (Vec n a) where
  autoReg
    :: forall dom. (HasCallStack, KnownDomain dom)
    => Clock dom -> Reset dom -> Enable dom
    -> Vec n a -- ^ Reset value
    -> Signal dom (Vec n a)
    -> Signal dom (Vec n a)
  autoReg clk rst en initVal xs =
    bundle $ smap go (lazyV initVal) <*> unbundle xs
   where
    go :: forall (i :: Nat). SNat i -> a  -> Signal dom a -> Signal dom a
    go SNat = suffixNameFromNatP @i . autoReg clk rst en
  {-# INLINE autoReg #-}

instance (KnownNat d, AutoReg a) => AutoReg (RTree d a) where
  autoReg clk rst en initVal xs =
    bundle $ (autoReg clk rst en) <$> lazyT initVal <*> unbundle xs
  {-# INLINE autoReg #-}


-- | Decompose an applied type into its individual components. For example, this:
--
-- @
-- Either Int Char
-- @
--
-- would be unfolded to this:
--
-- @
-- ('ConT' ''Either, ['ConT' ''Int, 'ConT' ''Char])
-- @
--
-- This function ignores explicit parentheses and visible kind applications.
--
-- NOTE: Copied from "Control.Lens.Internal.TH".
-- TODO: Remove this function. Can be removed once we can upgrade to lens 4.18.
-- TODO: This is currently difficult due to issue with nix.
unfoldType :: Type -> (Type, [Type])
unfoldType = go []
  where
    go :: [Type] -> Type -> (Type, [Type])
    go acc (ForallT _ _ ty) = go acc ty
    go acc (AppT ty1 ty2)   = go (ty2:acc) ty1
    go acc (SigT ty _)      = go acc ty
    go acc (ParensT ty)     = go acc ty
#if MIN_VERSION_template_haskell(2,15,0)
    go acc (AppKindT ty _)  = go acc ty
#endif
    go acc ty               = (ty, acc)

-- | Automatically derives an 'AutoReg' instance for a product type
--
-- Usage:
--
-- > data Pair a b = MkPair { getA :: a, getB :: b } deriving (Generic, NFDataX)
-- > data Tup3 a b c = MkTup3 { getAB :: Pair a b, getC :: c } deriving (Generic, NFDataX)
-- > deriveAutoReg ''Pair
-- > deriveAutoReg ''Tup3
--
-- __NB__: Because of the way template haskell works the order here matters,
-- if you try to @deriveAutoReg ''Tup3@ before @Pair@ it will complain
-- about missing an @instance AutoReg (Pair a b)@.
deriveAutoReg :: Name -> DecsQ
deriveAutoReg tyNm = do
  tyInfo <- reifyDatatype tyNm
  case datatypeCons tyInfo of
    [] -> fail "Can't deriveAutoReg for empty types"
    [conInfo] -> deriveAutoRegProduct tyInfo conInfo
    _ -> fail "Can't deriveAutoReg for sum types"



{-
For a type like:
   data Product a b .. = MkProduct { getA :: a, getB :: b, .. }
This generates the following instance:

instance (AutoReg a, AutoReg b, ..) => AutoReg (Product a b ..) where
  autoReg clk rst en initVal input =
    MkProduct <$> sig0 <*> sig1 ...
    where
      field0 = (\(MkProduct x _ ...) -> x) <$> input
      field1 = (\(MkProduct _ x ...) -> x) <$> input
      ...
      MkProduct initVal0 initVal1 ... = initVal
      sig0 = suffixNameP @"getA" autoReg clk rst en initVal0 field0
      sig1 = suffixNameP @"getB" autoReg clk rst en initVal1 field1
      ...
-}
deriveAutoRegProduct :: DatatypeInfo -> ConstructorInfo -> DecsQ
deriveAutoRegProduct tyInfo conInfo = go (constructorName conInfo) fieldInfos
 where
  tyNm = datatypeName tyInfo
  tyVarBndrs = datatypeVars tyInfo

#if MIN_VERSION_th_abstraction(0,4,0)
  toTyVar = VarT . tvName
#elif MIN_VERSION_th_abstraction(0,3,0)
  toTyVar = VarT . bndrName
#else
  toTyVar t = case t of
    VarT _ -> t
    SigT t' _ -> toTyVar t'
    _ -> error "deriveAutoRegProduct.toTv"
#endif

  tyVars = map toTyVar tyVarBndrs
  ty = conAppsT tyNm tyVars

  fieldInfos =
    zip fieldNames (constructorFields conInfo)
   where
    fieldNames =
      case constructorVariant conInfo of
        RecordConstructor nms -> map Just nms
        _ -> repeat Nothing

  go :: Name -> [(Maybe Name,Type)] -> Q [Dec]
  go dcNm fields = do
    args <- mapM newName ["clk", "rst", "en", "initVal", "input"]
    let
      [clkE, rstE, enE, initValE, inputE] = map varE args
      argsP = map varP args
      fieldNames = map fst fields

      field :: Name -> Int -> DecQ
      field nm nr =
        valD (varP nm) (normalB [| $fieldSel <$> $inputE |]) []
       where
        fieldSel = do
          xNm <- newName "x"
          let fieldP = [ if nr == n then varP xNm else wildP
                       | (n,_) <- zip [0..] fields]
          lamE [conP dcNm fieldP] (varE xNm)   -- "\(Dc _ _ .. x _ ..) -> x"

    parts <- generateNames "field" fields
    fieldDecls <- sequence $ zipWith field parts [0..]
    sigs <- generateNames "sig" fields
    initVals <- generateNames "initVal" fields
    let initPat = conP dcNm (map varP initVals)
    initDecl <- valD initPat (normalB initValE) []

    let
      genAutoRegDecl :: PatQ -> ExpQ -> ExpQ -> Maybe Name -> DecsQ
      genAutoRegDecl s v i nameM =
        [d| $s = $nameMe autoReg $clkE $rstE $enE $i $v |]
       where
        nameMe = case nameM of
          Nothing -> [| id |]
          Just nm -> let nmSym = litT $ strTyLit (nameBase nm)
                     in [| suffixNameP @($nmSym) |]

    partDecls <- concat <$> (sequence $ zipWith4 genAutoRegDecl
                                                 (varP <$> sigs)
                                                 (varE <$> parts)
                                                 (varE <$> initVals)
                                                 (fieldNames)
                            )
    let
        decls :: [DecQ]
        decls = map pure (initDecl : fieldDecls ++ partDecls)
        tyConE = conE dcNm
        body =
          case map varE sigs of
            (sig0:rest) -> foldl
                             (\acc sigN -> [| $acc <*> $sigN |])
                             [| $tyConE <$> $sig0 |]
                             rest
            [] -> [| $tyConE |]

    autoRegDec <- funD 'autoReg [clause argsP (normalB body) decls]
    ctx <- calculateRequiredContext conInfo
    return [InstanceD Nothing ctx (AppT (ConT ''AutoReg) ty)
              [ autoRegDec
              , PragmaD (InlineP 'autoReg Inline FunLike AllPhases) ]]

-- Calculate the required constraint to call autoReg on all the fields of a
-- given constructor
calculateRequiredContext :: ConstructorInfo -> Q Cxt
calculateRequiredContext conInfo = do
  let fieldTys = constructorFields conInfo
  wantedInstances <- mapM (\ty -> constraintsWantedFor ''AutoReg [ty]) (nub fieldTys)
  return $ nub (concat wantedInstances)

constraintsWantedFor :: Name -> [Type] -> Q Cxt
constraintsWantedFor clsNm tys
  | show clsNm == "GHC.TypeNats.KnownNat" = do
  -- KnownNat is special, you can't just lookup instances with reifyInstances.
  -- So we just pass KnownNat constraints.
  -- This will most likely require UndecidableInstances.
    return [conAppsT clsNm tys]

constraintsWantedFor clsNm [ty] = case ty of
  VarT _ -> return [AppT (ConT clsNm) ty]
  ConT _ -> return []
  _ -> do
    insts <- reifyInstances clsNm [ty]
    case insts of
      [InstanceD _ cxtInst (AppT autoRegCls instTy) _]
        | autoRegCls == ConT clsNm -> do
          let substs = findTyVarSubsts instTy ty
              cxt2 = map (applyTyVarSubsts substs) cxtInst
              okCxt = filter isOk cxt2
              recurseCxt = filter needRecurse cxt2
          recursed <- mapM recurse recurseCxt
          return (okCxt ++ concat recursed)
      []      -> fail $ "Missing instance " ++ show clsNm ++ " (" ++ pprint ty ++ ")"
      (_:_:_) -> fail $ "There are multiple " ++ show clsNm ++ " instances for "
                     ++ pprint ty ++ ":\n" ++ pprint insts
      _ -> fail $ "Got unexpected instance: " ++ pprint insts
 where
  isOk :: Type -> Bool
  isOk (unfoldType -> (_cls,tys)) =
    case tys of
      [VarT _] -> True
      [_] -> False
      _ -> True -- see [NOTE: MultiParamTypeClasses]
  needRecurse :: Type -> Bool
  needRecurse (unfoldType -> (cls,tys)) =
    case tys of
      [AppT _ _] -> True
      [VarT _] -> False  -- gets copied by "filter isOk" above
      [ConT _] -> False  -- we can just drop constraints like: "AutoReg Bool => ..."
      [LitT _] -> False  -- or "KnownNat 4 =>"
      [_] -> error ( "Error while deriveAutoReg: don't know how to handle: "
                  ++ pprint cls ++ " (" ++ pprint tys ++ ")" )
      _ -> False  -- see [NOTE: MultiParamTypeClasses]

  recurse :: Type -> Q Cxt
  recurse (unfoldType -> (ConT cls,tys)) = constraintsWantedFor cls tys
  recurse t =
    fail ("Expected a class applied to some arguments but got " ++ pprint t)

constraintsWantedFor clsNm tys =
  return [conAppsT clsNm tys] -- see [NOTE: MultiParamTypeClasses]

-- [NOTE: MultiParamTypeClasses]
-- The constraint calculation code doesn't handle MultiParamTypeClasses
-- "properly", but it will try to pass them on, so the resulting instance should
-- still compile with UndecidableInstances enabled.


-- | Find tyVar substitutions between a general type and a second possibly less
-- general type. For example:
--
-- @
-- findTyVarSubsts "Either a b" "Either c [Bool]"
--   == "[(a,c), (b,[Bool])]"
-- @
findTyVarSubsts :: Type -> Type -> [(Name,Type)]
findTyVarSubsts = go
 where
  go ty1 ty2 = case (ty1,ty2) of
    (VarT nm1       , VarT nm2) | nm1 == nm2 -> []
    (VarT nm        , t)                     -> [(nm,t)]
    (ConT _         , ConT _)                -> []
    (AppT x1 y1     , AppT x2 y2)            -> go x1 x2 ++ go y1 y2
    (SigT t1 k1     , SigT t2 k2)            -> go t1 t2 ++ go k1 k2
    (InfixT x1 _ y1 , InfixT x2 _ y2)        -> go x1 x2 ++ go y1 y2
    (UInfixT x1 _ y1, UInfixT x2 _ y2)       -> go x1 x2 ++ go y1 y2
    (ParensT x1     , ParensT x2)            -> go x1 x2

#if __GLASGOW_HASKELL__ >= 808
    (AppKindT t1 k1     , AppKindT t2 k2)      -> go t1 t2 ++ go k1 k2
    (ImplicitParamT _ x1, ImplicitParamT _ x2) -> go x1 x2
#endif

    (PromotedT _          , PromotedT _          ) -> []
    (TupleT _             , TupleT _             ) -> []
    (UnboxedTupleT _      , UnboxedTupleT _      ) -> []
    (UnboxedSumT _        , UnboxedSumT _        ) -> []
    (ArrowT               , ArrowT               ) -> []
    (EqualityT            , EqualityT            ) -> []
    (ListT                , ListT                ) -> []
    (PromotedTupleT _     , PromotedTupleT _     ) -> []
    (PromotedNilT         , PromotedNilT         ) -> []
    (PromotedConsT        , PromotedConsT        ) -> []
    (StarT                , StarT                ) -> []
    (ConstraintT          , ConstraintT          ) -> []
    (LitT _               , LitT _               ) -> []
    (WildCardT            , WildCardT            ) -> []
    _ -> error $ unlines [ "findTyVarSubsts: Unexpected types"
                         , "ty1:", pprint ty1,"ty2:", pprint ty2]

applyTyVarSubsts :: [(Name,Type)] -> Type -> Type
applyTyVarSubsts substs ty = go ty
  where
    go ty' = case ty' of
      VarT n -> case lookup n substs of
                  Nothing -> ty'
                  Just m  -> m
      ConT _ -> ty'
      AppT ty1 ty2 -> AppT (go ty1) (go ty2)
      _ -> error $ "TODO applyTyVarSubsts: " ++ show ty'


-- | Generate a list of fresh Name's:
-- prefix0_.., prefix1_.., prefix2_.., ..
generateNames :: String -> [a] -> Q [Name]
generateNames prefix xs =
  sequence (zipWith (\n _ -> newName $ prefix ++ show @Int n) [0..] xs)

deriveAutoRegTuples :: [Int] -> DecsQ
deriveAutoRegTuples xs = concat <$> mapM deriveAutoRegTuple xs

deriveAutoRegTuple :: Int -> DecsQ
deriveAutoRegTuple n
  | n < 2 = fail $ "deriveAutoRegTuple doesn't work for " ++ show n ++ "-tuples"
  | otherwise = deriveAutoReg tupN
  where
    tupN = mkName $ "(" ++ replicate (n-1) ',' ++ ")"
