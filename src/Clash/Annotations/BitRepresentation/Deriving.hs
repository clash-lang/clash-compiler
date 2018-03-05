{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift #-}

module Clash.Annotations.BitRepresentation.Deriving
  ( deriveDefaultAnnotation
  ) where

import GHC.Exts
import GHC.Integer.Logarithms

import Data.Bits (shiftL)
import Data.Proxy (Proxy(..))

import qualified Data.Map as Map

import Language.Haskell.TH (listE, pragAnnD)
import Language.Haskell.TH.Syntax
import GHC.TypeLits (natVal)

import Clash.Class.BitPack
import Clash.Annotations.BitRepresentation ( DataReprAnn(..)
                                           , ConstrRepr(..)
                                           , reprType
                                           )

type NameMap = Map.Map Name Type


integerLog2Ceil :: Integer -> Integer
integerLog2Ceil n =
  let nlog2 = fromIntegral $ I# (integerLog2# n) in
  if n > 2^nlog2 then nlog2 + 1 else nlog2

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV n) = n
tyVarBndrName (KindedTV n _k) = n

-- | Replace Vars types given in mapping
resolve :: NameMap -> Type -> Type
resolve nmap (VarT n) = nmap Map.! n
resolve nmap (AppT t1 t2) = AppT (resolve nmap t1) (resolve nmap t2)
resolve _nmap t@(ConT _) = t
resolve _nmap t = error $ {-$(curLoc) ++-} "Unexpected type: " ++ show t

resolveCon :: NameMap -> Con -> Con
resolveCon nmap (NormalC t (unzip -> (bangs, fTypes))) =
  NormalC t $ zip bangs $ map (resolve nmap) fTypes
resolveCon _name constr =
  error $ {-$(curLoc) ++-} "Unexpected constructor: " ++ show constr

collectTypeArgs :: Type -> (Type, [Type])
collectTypeArgs t@(ConT _name) = (t, [])
collectTypeArgs (AppT t1 t2) =
  let (base, args) = collectTypeArgs t1 in
  (base, args ++ [t2])
collectTypeArgs t =
  error $ {-$(curLoc) ++-} "Unexpected type: " ++ show t

typeSize :: Type -> Q Exp
typeSize typ = do
  bitSizeInstances <- reifyInstances ''BitSize [typ]
  case bitSizeInstances of
    [] ->
      error $ {-$(curLoc) ++ -} unwords [
          "Could not find custom bit representation nor BitSize instance"
        , "for", show typ ++ "." ]
    [TySynInstD _ (TySynEqn _ (LitT (NumTyLit n)))] ->
      [| n |]
    [_impl] ->
      [| natVal (Proxy :: Proxy (BitSize $(return typ))) |]
    unexp ->
      error $ {-$(curLoc) ++ -} "Unexpected result from reifyInstances: " ++ show unexp

bitmask
  :: Integer
  -> Integer
  -> Integer
bitmask _start 0    = 0
bitmask start  size = shiftL (2^size - 1) $ fromIntegral (start - (size - 1))

buildConstrRepr
  :: Q Exp
  -- ^ Data size
  -> Integer
  -- ^ Number of bits reserved for constructor
  -> Name
  -- ^ Constr name
  -> [Exp]
  -- ^ Field sizes
  -> Integer
  -- ^ Constructor number
  -> Q Exp
buildConstrRepr dataSize constrSize constrName fieldSizes constrN = [|
  ConstrRepr
    constrName
    $mask
    $value
    $(ListE <$> fanns)
  |]
  where
    mask  = [| bitmask ($dataSize - 1) constrSize |]
    value = [| shiftL constrN (fromIntegral $ $dataSize - 1)|]
    fanns =
      sequence $ snd
               $ mapAccumL
                    (\start size -> ([| $start - $size |], [| bitmask $start $size |]))
                    [| $dataSize - constrSize - 1 |]
                    (map return fieldSizes)


fieldTypes :: Con -> [Type]
fieldTypes (NormalC _nm bTys) =
  [ty | (_, ty) <- bTys]
fieldTypes (RecC _nm bTys) =
  [ty | (_, _, ty) <- bTys]
fieldTypes (InfixC (_, ty1) _nm (_, ty2)) =
  [ty1, ty2]
fieldTypes con =
  error $ {-$(curLoc) ++-} "Unexpected constructor type: " ++ show con

conName :: Con -> Name
conName c = case c of
  NormalC nm _  -> nm
  RecC    nm _  -> nm
  InfixC _ nm _ -> nm
  _ -> error $ {-$(curLoc) ++-} "No GADT support"

constrFieldSizes
  :: Con
  -> Q (Name, [Exp])
constrFieldSizes con = do
  fieldSizes <- mapM typeSize (fieldTypes con)
  return (conName con, fieldSizes)

-- | Derive DataRepr' for a specific type.
deriveDataRepr :: Type -> Q Exp
deriveDataRepr typ = do
  info <- reify tyConstrName
  case info of
    (TyConI (DataD [] _constrName vars _kind dConstructors _clauses)) ->
      let varMap = Map.fromList $ zip (map tyVarBndrName vars) typeArgs in
      let resolvedConstructors = map (resolveCon varMap) dConstructors in do

      -- Get sizes and names of all constructors
      (constrNames, fieldSizess) <-
        unzip <$> (mapM constrFieldSizes resolvedConstructors)

      let fieldSizess'  = ListE <$> fieldSizess
      let fieldSizess'' = ListE fieldSizess'

      -- Determine size of whole datatype
      let constructorSizes = [| map sum $(return fieldSizess'') |]
      let constrSize = integerLog2Ceil (fromIntegral $ length dConstructors)
      let dataSize = [| constrSize + (maximum $constructorSizes) |]

      -- Determine at which bits various fields start
      let constrReprs = zipWith3
                          (buildConstrRepr dataSize constrSize)
                          constrNames
                          fieldSizess
                          [0..]

      [| DataReprAnn $(reprType $ return typ)  $dataSize $(listE constrReprs)  |]
    _ ->
      error $ {-$(curLoc) ++-} "Could not derive dataRepr for: " ++ show info

    where
      (ConT tyConstrName, typeArgs) = collectTypeArgs typ

-- | Derives BitPack instances for given type. Will account for custom bit
-- representation annotations in the module where the splice is ran, and all
-- modules of the types used in the data type. Note that the generated instance
-- might conflict with existing implementations (for example, an instance for
-- /Maybe a/ exists, yielding conflicts for any alternative implementations).
--
deriveDefaultAnnotation
  :: Q Type
  -> Q [Dec]
deriveDefaultAnnotation typQ = do
  typ <- typQ

  typDataRepr <- deriveDataRepr typ

  return <$> pragAnnD ModuleAnnotation (return typDataRepr)


