{-|
Copyright  :  (C) 2018, Google Inc.,
                  2022, QBayLogic B.V.
                  2022, LUMI GUIDE FIETSDETECTIE B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

This module contains:

  * Template Haskell functions for deriving 'BitPack' instances given a
    custom bit representation as those defined in
    "Clash.Annotations.BitRepresentation".

  * Template Haskell functions for deriving custom bit representations,
    e.g. one-hot, for a data type.

-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Annotations.BitRepresentation.Deriving
  (
  -- * Derivation functions
    deriveAnnotation
  , deriveBitPack
  , deriveDefaultAnnotation
  , derivePackedAnnotation
  , derivePackedMaybeAnnotation
  , deriveBlueSpecAnnotation
  -- * Derivators
  , defaultDerivator
  , blueSpecDerivator
  , packedDerivator
  , packedMaybeDerivator
  , simpleDerivator
  -- * Util functions
  , dontApplyInHDL
  -- * Types associated with various functions
  , ConstructorType(..)
  , FieldsType(..)
  -- * Convenience type synonyms
  , Derivator
  , DataReprAnnExp
  ) where

import Clash.Annotations.BitRepresentation
  (DataReprAnn(..), ConstrRepr(..), BitMask, Value, Size, liftQ)
import Clash.Annotations.BitRepresentation.Internal
  (dataReprAnnToDataRepr', constrReprToConstrRepr', DataRepr'(..))
import Clash.Annotations.BitRepresentation.Util
  (bitOrigins, bitOrigins', BitOrigin(..), bitRanges, Bit)
import qualified Clash.Annotations.BitRepresentation.Util
  as Util

import           Clash.Annotations.Primitive  (hasBlackBox)
import           Clash.Class.BitPack
  (BitPack, BitSize, pack, packXWith, unpack)
import           Clash.Class.Resize           (resize)
import           Language.Haskell.TH.Compat   (mkTySynInstD)
import           Clash.Sized.BitVector        (BitVector, low, (++#))
import           Clash.Sized.Internal.BitVector (undefined#)
import           Control.Applicative          (liftA3)
import           Control.DeepSeq              (NFData)
import           Control.Monad                (forM)
import           Data.Bits
  (shiftL, shiftR, complement, (.&.), (.|.), zeroBits, popCount, bit, testBit,
   Bits, setBit)
import           Data.Data                    (Data)
import           Data.Containers.ListUtils    (nubOrd)
import           Data.List
  (mapAccumL, zipWith4, sortOn, partition, uncons)
#if __GLASGOW_HASKELL__ < 912
import           Data.Typeable                (Typeable)
#endif
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe)
import qualified Data.Set                     as Set
import           Data.Proxy                   (Proxy(..))
import           GHC.Exts                     (Int(I#))
import           GHC.Generics                 (Generic)
import           GHC.Integer.Logarithms       (integerLog2#)
import           GHC.TypeLits                 (natVal)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Datatype (resolveTypeSynonyms)

-- | Used to track constructor bits in packed derivation
data BitMaskOrigin
  = External
  -- ^ Constructor bit should be stored externally
  | Embedded BitMask Value
  -- ^ Constructor bit should be stored in one of the constructor's fields
    deriving (Show, Data, Lift)
#if __GLASGOW_HASKELL__ < 912
    deriving (Typeable)
#endif

isExternal :: BitMaskOrigin -> Bool
isExternal External = True
isExternal _        = False

type ReprAnnCache = Map.Map Type DataReprAnn

type NameMap = Map.Map Name Type

-- | DataReprAnn as template haskell expression
type DataReprAnnExp = Exp

-- | A derivator derives a bit representation given a type
type Derivator = Type -> Q DataReprAnnExp

-- | Indicates how to pack constructor for simpleDerivator
data ConstructorType
  = Binary
  -- ^ First constructor will be encoded as 0b0, the second as 0b1, the third
  -- as 0b10, etc.
  | OneHot
  -- ^ Reserve a single bit for each constructor marker.

-- | Indicates how to pack (constructor) fields for simpleDerivator
data FieldsType
  = OverlapL
  -- ^ Store fields of different constructors at (possibly) overlapping bit
  -- positions. That is, a data type with two constructors with each two fields
  -- of each one bit will take /two/ bits for its whole representation (plus
  -- constructor bits). Overlap is left-biased, i.e. don't care bits are padded
  -- to the right.
  --
  -- This is the default behavior of Clash.
  | OverlapR
  -- ^ Store fields of different constructors at (possibly) overlapping bit
  -- positions. That is, a data type with two constructors with each two fields
  -- of each one bit will take /two/ bits for its whole representation (plus
  -- constructor bits). Overlap is right biased, i.e. don't care bits are padded
  -- between between the constructor bits and the field bits.
  | Wide
  -- ^ Store fields of different constructs at non-overlapping positions. That
  -- is, a data type with two constructors with each two fields of each one bit
  -- will take /four/ bits for its whole representation (plus constructor bits).

-- | Determine most significant bit set for given integer.
--
-- TODO: Current complexity is O(n). We could probably use machine instructions
-- for ~constant complexity.
msb :: Integer -> Int
msb 0 = error $ "Most significant bit does not exist for zero."
msb 1 = 0
msb n = 1 + msb (shiftR n 1)

mkReprAnnCache :: [DataReprAnn] -> ReprAnnCache
mkReprAnnCache anns =
  Map.fromList [(typ, rAnn) | rAnn@(DataReprAnn typ _ _) <- anns]

-- | Integer version of (ceil . log2). Can handle arguments up to 2^(2^WORDWIDTH).
integerLog2Ceil :: Integer -> Int
integerLog2Ceil n =
  let nlog2 = fromIntegral $ I# (integerLog2# n) in
  if n > 2^nlog2 then nlog2 + 1 else nlog2

-- | Determine number of bits needed to represent /n/ options. Alias for
-- integerLog2Ceil to increase readability of programmer intentention.
bitsNeeded :: Integer -> Int
bitsNeeded = integerLog2Ceil

#if MIN_VERSION_template_haskell(2,17,0)
tyVarBndrName :: TyVarBndr f -> Name
tyVarBndrName (PlainTV n _f) = n
tyVarBndrName (KindedTV n _f _k) = n
#else
tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV n) = n
tyVarBndrName (KindedTV n _k) = n
#endif

-- | Replace Vars types given in mapping
resolve :: NameMap -> Type -> Type
resolve nmap (VarT n) = nmap Map.! n
resolve nmap (AppT t1 t2) = AppT (resolve nmap t1) (resolve nmap t2)
resolve _nmap t@(ConT _) = t
resolve _nmap t@(LitT _) = t
resolve _nmap t@(TupleT _) = t
resolve _nmap t = error $ "Unexpected type: " ++ show t

resolveCon :: NameMap -> Con -> Con
resolveCon nmap (NormalC t (unzip -> (bangs, fTypes))) =
  NormalC t $ zip bangs $ map (resolve nmap) fTypes
resolveCon nmap (RecC t (unzip3 -> (name, bangs, fTypes))) =
  RecC t $ zip3 name bangs $ map (resolve nmap) fTypes
resolveCon nmap (InfixC (leftB, leftTy) t (rightB, rightTy)) =
  InfixC (leftB, resolve nmap leftTy) t (rightB, resolve nmap rightTy)
resolveCon _name constr =
  error $ "Unexpected constructor: " ++ show constr

collectTypeArgs :: Type -> (Type, [Type])
collectTypeArgs t@(ConT _name) = (t, [])
collectTypeArgs (AppT t1 t2) =
  let (base, args) = collectTypeArgs t1 in
  (base, args ++ [t2])
collectTypeArgs t =
  error $ "Unexpected type: " ++ show t

-- | Returns size in number of bits of given type. Relies on the presence of a
-- BitSize implementation. Tries to recognize literal values and return a simple
-- expression.
typeSize :: Type -> Q Exp
typeSize typ = do
  bitSizeInstances <- reifyInstances ''BitSize [typ]
  case bitSizeInstances of
    [] ->
      fail $ unwords [
          "Could not find custom bit representation nor BitSize instance"
        , "for", show typ ++ "." ]
#if MIN_VERSION_template_haskell(2,15,0)
    [TySynInstD (TySynEqn _ _ (LitT (NumTyLit n)))] ->
#else
    [TySynInstD _ (TySynEqn _ (LitT (NumTyLit n)))] ->
#endif
      [| n |]
    [_impl] ->
      [| fromIntegral $ natVal (Proxy :: Proxy (BitSize $(return typ))) |]
    unexp ->
      fail $ "Unexpected result from reifyInstances: " ++ show unexp

-- | Generate bitmask from a given bit, with a certain size
bitmask
  :: Int
  -- ^ Bitmask starts at bit /n/
  -> Int
  -- ^ Bitmask has size /m/
  -> Integer
bitmask _start 0    = 0
bitmask start  size
  | start < 0        = error $ "Start cannot be <0. Was: " ++ show start
  | size < 0         = error $ "Size cannot be <0. Was: " ++ show size
  | start + 1 < size = error $ "Start + 1 (" ++ show start ++ " - 1) cannot be smaller than size (" ++ show size ++  ")."
  | otherwise        = shiftL (2^(toInteger size) - 1) (start - (size - 1))


fieldTypes :: Con -> [Type]
fieldTypes (NormalC _nm bTys) =
  [ty | (_, ty) <- bTys]
fieldTypes (RecC _nm bTys) =
  [ty | (_, _, ty) <- bTys]
fieldTypes (InfixC (_, ty1) _nm (_, ty2)) =
  [ty1, ty2]
fieldTypes con =
  error $ "Unexpected constructor type: " ++ show con

conName :: Con -> Name
conName c = case c of
  NormalC nm _  -> nm
  RecC    nm _  -> nm
  InfixC _ nm _ -> nm
  _ -> error $ "No GADT support"

mkLet :: String -> Q Exp -> (Q Dec, Q Exp)
mkLet nm qe = do
  let nm' = mkName nm
  (valD (varP nm') (normalB qe) [], varE nm')

fieldSizeLets :: [[Type]] -> ([Q Dec], [[Q Exp]])
fieldSizeLets fieldtypess = (fieldSizeDecls, fieldSizessExps)
  where
    nums = map show [(0 :: Int)..]
    uqFieldTypes = nubOrd (concat fieldtypess)
    uqFieldSizes = map typeSize uqFieldTypes
    (fieldSizeDecls, szVars) = unzip $ zipWith
                                 (\i sz -> mkLet ("_f" ++ i) sz)
                                 nums
                                 uqFieldSizes
    tySizeMap = Map.fromList (zip uqFieldTypes szVars)
    fieldSizessExps = map (map (tySizeMap Map.!)) fieldtypess

complementInteger :: Int -> Integer -> Integer
complementInteger 0 _i = 0
complementInteger size i =
  let size' = size - 1 in
  if testBit i size' then
    complementInteger size' i
  else
    (.|.) (bit size') (complementInteger size' i)

deriveAnnotation :: Derivator -> Q Type -> Q [Dec]
deriveAnnotation deriv typ =
  return <$> pragAnnD ModuleAnnotation (deriv =<< typ)

--------------------------------------------
------------ SIMPLE DERIVATIONS ------------
--------------------------------------------
buildConstrRepr
  :: Q Exp
  -- ^ Data size (excluding constructor size)
  -> Name
  -- ^ Constr name
  -> [Q Exp]
  -- ^ Field masks
  -> BitMask
  -- ^ Constructor mask
  -> Value
  -- ^ Constructor value
  -> Q Exp
buildConstrRepr dataSize constrName fieldAnns constrMask constrValue = [|
  ConstrRepr
    constrName
    $mask
    $value
    $(listE fieldAnns)
  |]
  where
    mask  = [| shiftL constrMask  ($dataSize)|]
    value = [| shiftL constrValue ($dataSize)|]

countConstructor :: [Int] -> [(BitMask, Value)]
countConstructor ns = zip (repeat mask) (map toInteger ns)
  where
    maskSize = bitsNeeded $ toInteger $ maximum ns + 1
    mask = 2^maskSize - 1

oneHotConstructor :: [Int] -> [(BitMask, Value)]
oneHotConstructor ns = zip values values
  where
    values = [shiftL 1 n | n <- ns]

overlapFieldAnnsL :: [[Q Exp]] -> ([Q Dec], [[Q Exp]])
overlapFieldAnnsL fieldSizess = ([maxDecl],  resExp)
  where
    (maxDecl, maxExp) = mkLet "_maxf" maxConstrSize
    resExp = map go fieldSizess
    fieldSizess' = listE $ map listE fieldSizess
    constructorSizes = [| map (sum @[] @Int) $fieldSizess' |]
    maxConstrSize = [| maximum $constructorSizes - 1 |]
    go fieldsizes =
      snd $
      mapAccumL
        (\start size -> ([| $start - $size |], [| bitmask $start $size |]))
        maxExp
        fieldsizes

overlapFieldAnnsR :: [[Q Exp]] -> ([Q Dec], [[Q Exp]])
overlapFieldAnnsR fieldSizess = (sumFieldDecl, resExp)
  where
    resExp = zipWith go fieldSizess sumFieldExp

    nums = map show [(0 :: Int) ..]

    (sumFieldDecl, sumFieldExp)
      = unzip $ zipWith
          (\fs i -> mkLet ("_sumf" ++ i) [|sum @[] @Int $(listE fs)|])
          fieldSizess
          nums

    go fieldSizes sumFieldsSize =
      snd $
      mapAccumL
        (\start size -> ([| $start - $size |], [| bitmask $start $size |]))
        [| $sumFieldsSize - 1 |]
        fieldSizes

wideFieldAnns :: [[Q Exp]] -> ([Q Dec], [[Q Exp]])
wideFieldAnns fieldSizess = (decs, resExp)
  where
    decs = (dataSizeDec:constrSizeDecs) ++ constrOffsetDecs
    resExp = zipWith id (map go constrOffsetsExps) fieldSizess
    nums = map show [(0 :: Int) ..]

    constrSizeExps :: [Q Exp]
    (constrSizeDecs, constrSizeExps)
      = unzip $ zipWith
          (\fs i -> mkLet ("_sumf" ++ i) [|sum @[] @Int $(listE fs)|])
          fieldSizess
          nums

    constrOffsetsExps :: [Q Exp]
    (last -> constrOffsetDecs, constrOffsetsExps) =
      unzip $ init $ scanl
        (\(ds, offset) (size, i) ->
          let e = [| $offset + $size |]
              (d, ve) = mkLet ("_constroffset" ++ i) e
          in (d:ds, ve)
        )
        ([], [| 0 |])
        (zip constrSizeExps nums)

    dataSizeExp :: Q Exp
    (dataSizeDec, dataSizeExp)
      = mkLet "_widedatasize" [| sum @[] @Int $(listE constrSizeExps) - 1 |]
    go :: Q Exp -> [Q Exp] -> [Q Exp]
    go offset fieldSizes =
      snd $
      mapAccumL
        (\start size -> ([| $start - $size |], [| bitmask $start $size |]))
        [| $dataSizeExp - $offset |]
        fieldSizes

-- | Derive DataRepr' for a specific type.
deriveDataRepr
  :: ([Int] -> [(BitMask, Value)])
  -- ^ Constructor derivator
  -> ([[Q Exp]] -> ([Q Dec], [[Q Exp]]) )
  -- ^ Field derivator
  -> Derivator
deriveDataRepr constrDerivator fieldsDerivator typ = do
  let (fun, typeArgs) = collectTypeArgs typ
      tyConstrName = case fun of
        ConT t -> t
        _ -> error ("deriveDataRep: expecting type constructor, but got: " <> show fun)
  info <- reify tyConstrName
  case info of
    (TyConI (DataD [] _constrName vars _kind dConstructors _clauses)) ->
      let varMap = Map.fromList $ zip (map tyVarBndrName vars) typeArgs in
      let resolvedConstructors = map (resolveCon varMap) dConstructors in do
      let nums = map show [(0 :: Int)..]
      let fieldtypess = map fieldTypes resolvedConstructors

      let (fieldSzDecs, fieldSizess) = fieldSizeLets fieldtypess

      -- Get sizes and names of all constructors
      let constrNames = map conName resolvedConstructors

      let
        (constrMasks, constrValues) =
          unzip $ constrDerivator [0..length dConstructors - 1]

      let constrSize = 1 + (msb $ maximum @[] @Integer constrMasks)
      let (fieldDecs, fieldAnns) = fieldsDerivator fieldSizess

      -- extract field annotations into declarations
      let mkAnnDecl i j an = mkLet ("_fa" ++ i ++ "_" ++ j) an
      let
        fieldAnnTup =
          zipWith (\i -> zipWith (mkAnnDecl i) nums) nums fieldAnns

      let
        (fieldAnnDecs, fieldAnnVars) =
          (concat $ map (map fst) fieldAnnTup, map (map snd) fieldAnnTup)

      let fieldAnnsFlat = listE $ concat fieldAnnVars

      let dataSize | null $ concat fieldAnns = [| 0 |]
                   | otherwise = [| 1 + (msb $ maximum @[] @Integer $ $fieldAnnsFlat) |]

      -- Extract data size into a declaration
      let (dataSizeDec, dataSizeExp) = mkLet "_datasize" dataSize

      let decls = (dataSizeDec:fieldSzDecs) ++ fieldDecs ++ fieldAnnDecs

      -- Determine at which bits various fields start
      let constrReprs = zipWith4
                          (buildConstrRepr dataSizeExp)
                          constrNames
                          fieldAnnVars
                          constrMasks
                          constrValues

      resolvedType <- resolveTypeSynonyms typ

      letE decls [| DataReprAnn
          $(liftQ $ return resolvedType)
          ($dataSizeExp + constrSize)
          $(listE constrReprs) |]
    _ ->
      fail $ "Could not derive dataRepr for: " ++ show info

-- | Simple derivators change the (default) way Clash stores data types. It
-- assumes no overlap between constructors and fields.
simpleDerivator :: ConstructorType -> FieldsType -> Derivator
simpleDerivator ctype ftype = deriveDataRepr constrDerivator fieldsDerivator
  where
    constrDerivator =
      case ctype of
        Binary -> countConstructor
        OneHot -> oneHotConstructor

    fieldsDerivator =
      case ftype of
        OverlapL -> overlapFieldAnnsL
        OverlapR -> overlapFieldAnnsR
        Wide -> wideFieldAnns

-- | Derives bit representation corresponding to the default manner in which
-- Clash stores types.
defaultDerivator :: Derivator
defaultDerivator = simpleDerivator Binary OverlapL

-- | Derives bit representation corresponding to the default manner in which
-- BlueSpec stores types.
blueSpecDerivator :: Derivator
blueSpecDerivator = simpleDerivator Binary OverlapR

-- | Derives bit representation corresponding to the default manner in which
-- Clash stores types.
deriveDefaultAnnotation :: Q Type -> Q [Dec]
deriveDefaultAnnotation = deriveAnnotation defaultDerivator


-- | Derives bit representation corresponding to the default manner in which
-- BlueSpec stores types.
deriveBlueSpecAnnotation :: Q Type -> Q [Dec]
deriveBlueSpecAnnotation = deriveAnnotation blueSpecDerivator

---------------------------------------------------------------
------------ DERIVING PACKED MAYBE REPRESENTATIONS ------------
---------------------------------------------------------------
toBits'
  :: Bits a
  => Size
  -> a
  -> [Bit']
toBits' 0 _ = []
toBits' size bits = bit' : toBits' (size - 1) bits
  where bit' = if testBit bits (size - 1) then H else L

bitsToInteger' :: (Bit' -> Bool) -> [Bit'] -> Integer
bitsToInteger' predFunc bits = foldl setBit 0 toSet
  where
    toSet = [n | (n, b) <- zip [0..] (reverse bits), predFunc b]

bitsToInteger :: [Bit'] -> Integer
bitsToInteger = bitsToInteger' (==H)

bitsToMask :: [Bit'] -> Integer
bitsToMask = bitsToInteger' (\b -> b == H || b == L)

data Bit'
  = X
  -- ^ Could be both 1 or 0
  | L
  -- ^ 0
  | H
  -- ^ 1
  | U
  -- ^ Unused
    deriving (Show, Eq, Generic, NFData)

-- | Given a number of possible values, construct a list of all complement values.
-- For example, Given a list:
--
-- @
-- [[HH, HH], [LL, LL]]
-- @
--
-- then:
--
-- @
-- [[HH, LL], [LL, HH]]
-- @
--
-- would be complements.
complementValues
  :: Size
  -> [[Bit']]
  -> [[Bit']]
complementValues 0 _ = []
complementValues 1 xs
  | X `elem` xs'                 = []
  | H `elem` xs' && L `elem` xs' = []
  | H `elem` xs'                 = [[L]]
  | otherwise                    = [[H]]
  where
    xs' = map (maybe (error "complementValues: expected at least 1 bit") fst . uncons) xs
complementValues size [] = [replicate size U]
complementValues size values =
  if | all (maybe False ((==U) . fst) . uncons) values' -> map (U:) (recc (map (drop 1) values'))
     | any (maybe False ((==X) . fst) . uncons) values' -> map (X:) (recc (map (drop 1) values'))
     | otherwise ->
        (map (L:) (recc (map (drop 1) lows))) ++
        (map (H:) (recc (map (drop 1) highs')))
  where
    values'       = filter (any (/= U)) values
    recc          = complementValues (size - 1)
    (highs, lows) = partition (maybe False ((== H) . fst) . uncons) values'
    highs'        = highs ++ filter (maybe False ((`elem` [X, U]) . fst) . uncons) values'

-- | Generate all bitvalues the given type can assume.
possibleValues
  :: ReprAnnCache
  -> Type
  -> Size
  -> Q [[Bit']]
possibleValues typeMap typ size =
  let typeName = case fst (collectTypeArgs typ) of
        ConT t -> t
        fun -> error ("possibleValues: expected a type constructor, but got" <> show fun)
  in case Map.lookup typ typeMap of
    -- No custom data representation found.
    Nothing -> do
      info <- reify typeName
      case info of
        -- TODO: check if fields have custom bit representations
        (TyConI (DataD [] _constrName _vars _kind dConstructors _clauses)) ->
          let nConstrBits = bitsNeeded (toInteger $ length dConstructors) in
          let fieldBits = replicate (size - nConstrBits) X in
          let constrBits = [toBits' nConstrBits n | n <- [0..length dConstructors - 1]] in
          return $ zipWith (++) constrBits (repeat fieldBits)
        _ ->
          return [replicate size X]

    Just (dataReprAnnToDataRepr' -> dataRepr) ->
      -- TODO: check if fields have custom bit representations
      let (DataRepr' _name _size constrs) = dataRepr in
      forM constrs $ \constr -> do
        return $
          map
            (\case { Lit [Util.H] -> H;
                     Lit [Util.L] -> L;
                     Lit [Util.U] -> U;
                     Field _ _ _  -> X;
                     c -> error $ "possibleValues (2): unexpected: " ++ show c; })
            (bitOrigins' dataRepr constr)

packedMaybe :: Size -> Type -> Q (Maybe DataReprAnn)
packedMaybe size typ = do
  cache <- mkReprAnnCache <$> collectDataReprs
  values <- possibleValues cache typ size
  return $ case complementValues size values of
             (value:_) ->
               Just $ DataReprAnn
                        (AppT (ConT ''Maybe) typ)
                        size
                        [ ConstrRepr
                            'Nothing
                            (bitsToMask value)
                            (bitsToInteger value)
                            []
                        , ConstrRepr
                            'Just
                            0
                            0
                            [bitmask (size - 1) size] ]
             [] ->
               Nothing


packedMaybeDerivator :: DataReprAnn -> Derivator
packedMaybeDerivator (DataReprAnn _ size _) typ =
  case maybeCon of
    ConT nm ->
      if nm == ''Maybe then do
        let err = unwords [ "Could not derive packed maybe for:", show typ
                          , ";", "Does its subtype have any space left to store"
                          , "the constructor in?" ]
        packedM <- packedMaybe (size - 1)
                    (maybe (error "Maybe type without argument") fst (uncons maybeTyps))
        (fromMaybe (fail err) . fmap lift) packedM
      else
        fail $ unwords [ "You can only pass Maybe types to packedMaybeDerivator,"
                        , "not", show nm]
    unexpected ->
      fail $ "packedMaybeDerivator: unexpected constructor: " ++ show unexpected
  where
    (maybeCon, maybeTyps) = collectTypeArgs typ

-- | Derive a compactly represented version of @Maybe a@.
derivePackedMaybeAnnotation :: DataReprAnn -> Q [Dec]
derivePackedMaybeAnnotation defaultDataRepr@(DataReprAnn typ _ _) = do
  deriveAnnotation (packedMaybeDerivator defaultDataRepr) (return typ)

---------------------------------------------------------
------------ DERIVING PACKED REPRESENTATIONS ------------
---------------------------------------------------------
packedConstrRepr
  :: Int
  -- ^ Data width
  -> Int
  -- ^ External constructor width
  -> Int
  -- ^ nth External so far
  -> [(BitMaskOrigin, ConstrRepr)]
  -> [ConstrRepr]
packedConstrRepr _ _ _ [] = []
packedConstrRepr dataWidth constrWidth n ((External, ConstrRepr name _ _ anns) : constrs) =
  constr : packedConstrRepr dataWidth constrWidth (n+1) constrs
  where
    constr =
      ConstrRepr
        name
        (shiftL (2^constrWidth - 1) dataWidth)
        (shiftL (toInteger n) dataWidth)
        anns

packedConstrRepr dataWidth constrWidth n ((Embedded mask value, ConstrRepr name _ _ anns) : constrs) =
  constr : packedConstrRepr dataWidth constrWidth n constrs
  where
    constr =
      ConstrRepr
        name
        mask
        value
        anns

packedDataRepr
  :: Type
  -> Size
  -> [(BitMaskOrigin, ConstrRepr)]
  -> DataReprAnn
packedDataRepr typ dataWidth constrs =
  DataReprAnn
    typ
    (dataWidth + constrWidth)
    (packedConstrRepr dataWidth constrWidth 0 constrs)
  where
    external    = filter isExternal (map fst constrs)
    constrWidth = bitsNeeded $ toInteger $ min (length external + 1) (length constrs)

-- | Try to distribute constructor bits over fields
storeInFields
  :: Int
  -- ^ data width
  -> BitMask
  -- ^ Additional mask gathered so far
  -> [BitMask]
  -- ^ Repr bitmasks to try and pack
  -> [BitMaskOrigin]
storeInFields _dataWidth _additionalMask [] = []
storeInFields _dataWidth _additionalMask [_] =
  -- Last constructor is implict
  [Embedded 0 0]
storeInFields dataWidth additionalMask constrs@(constr:constrRest) =
  if commonMask == fullMask then
    -- We can't store the constructor anywhere special, so we need a special
    -- constructor bit stored besides fields
    External : storeInFields dataWidth additionalMask constrRest
  else
    -- Hooray, we can store it somewhere.
    maskOrigins ++ (storeInFields dataWidth additionalMask' (drop storeSize constrs))

  where
    headMask   = constr
    commonMask = (.|.) headMask additionalMask

    -- Variables for the case that we can store something:
    storeMask       = complementInteger dataWidth commonMask
    additionalMask' = (.|.) additionalMask storeMask
    storeSize       = 2^(popCount storeMask) - 1
    maskOrigins     = [Embedded storeMask (toInteger n) | n <- [1..storeSize]]

    -- BitMask which spans the complete data size
    fullMask = 2^dataWidth - 1

derivePackedAnnotation' :: DataReprAnn -> DataReprAnn
derivePackedAnnotation' (DataReprAnn typ size constrs) =
  dataRepr
  where
    constrWidth = bitsNeeded $ toInteger $ length constrs
    dataWidth   = size - constrWidth
    fieldMasks  = [foldl (.|.) zeroBits anns | ConstrRepr _ _ _ anns <- constrs]

    -- Default annotation will overlap "to the left", so sorting on size will
    -- actually provide us with the 'fullest' constructors first and the
    -- 'empties' last.
    sortedMasks = reverse $ sortOn fst $ zip fieldMasks constrs
    origins     = storeInFields dataWidth zeroBits (map fst sortedMasks)
    constrs'    = zip origins $ map snd sortedMasks
    dataRepr    = packedDataRepr typ dataWidth constrs'

-- | This derivator tries to distribute its constructor bits over space left
-- by the difference in constructor sizes. Example:
--
-- @
-- type SmallInt = Unsigned 2
--
-- data Train
--    = Passenger SmallInt
--    | Freight SmallInt SmallInt
--    | Maintenance
--    | Toy
-- @
--
-- The packed representation of this data type needs only a single constructor
-- bit. The first bit discriminates between @Freight@ and non-@Freight@
-- constructors. All other constructors do not use their last two bits; the
-- packed representation will store the rest of the constructor bits there.
packedDerivator :: Derivator
packedDerivator typ =
  [| derivePackedAnnotation' $(defaultDerivator typ ) |]

derivePackedAnnotation :: Q Type -> Q [Dec]
derivePackedAnnotation = deriveAnnotation packedDerivator

----------------------------------------------------
------------ DERIVING BITPACK INSTANCES ------------
----------------------------------------------------

-- | Collect data reprs of current module
collectDataReprs :: Q [DataReprAnn]
collectDataReprs = do
  thisMod <- thisModule
  unresolved <- go [thisMod] Set.empty []
  mapM resolveTyps unresolved
  where
    resolveTyps (DataReprAnn t s c)
      = liftA3 DataReprAnn (resolveTypeSynonyms t) (pure s) (pure c)
    go []     _visited acc = return acc
    go (x:xs) visited  acc
      | x `Set.member` visited = go xs visited acc
      | otherwise = do
          ModuleInfo newMods <- reifyModule x
          newAnns <- reifyAnnotations $ AnnLookupModule x
          go (newMods ++ xs) (x `Set.insert` visited) (newAnns ++ acc)

group :: [Bit] -> [(Int, Bit)]
group [] = []
group bs@(b:_) = (length head', b) : rest
  where
    tail' = dropWhile (==b) bs
    head' = takeWhile (==b) bs
    rest  = group tail'

bitToExpr' :: (Int, Bit) -> Q Exp -- BitVector n
bitToExpr' (0, _) = fail $ "Unexpected group length: 0"
bitToExpr' (numTyLit' -> n, Util.H) =
  [| complement (resize (pack low) :: BitVector $n) |]
bitToExpr' (numTyLit' -> n, Util.L) =
  [| resize (pack low) :: BitVector $n |]
bitToExpr' (numTyLit' -> n, _) =
  [| undefined# :: BitVector $n |]

bitsToExpr :: [Bit] -> Q Exp -- BitVector n
bitsToExpr [] = fail $ "Unexpected empty bit list"
bitsToExpr bits =
  foldl1
    (\v1 v2 -> [| $v1 ++# $v2 |])
    (map bitToExpr' $ group bits)

numTyLit' :: Integral a => a -> Q Type
numTyLit' n = LitT <$> (numTyLit $ toInteger n)

-- | Select a list of ranges from a bitvector expression
select'
  :: Exp
  -> [(Int, Int)]
  -> Q Exp
select' _vec [] =
  fail $ "Unexpected empty list of intervals"
select' vec ranges =
  foldl1 (\v1 v2 -> [| $v1 ++# $v2 |]) $ map (return . select'') ranges
    where
      select'' :: (Int, Int) -> Exp
      select'' (from, downto) =
        let size = from - downto + 1 in
        let
          shifted
            | downto == 0 =
                vec
            | otherwise =
                AppE
                  (AppE (VarE 'shiftR) vec)
                  (LitE $ IntegerL $ toInteger downto) in

        SigE
          -- Select from whole vector
          (AppE (VarE 'resize) shifted)
          -- Type signature:
          (AppT (ConT ''BitVector) (LitT $ NumTyLit $ toInteger size))

-- | Select a range (bitorigin) from a bitvector
select
  :: [Exp]
  -- ^ BitVectors of fields
  -> BitOrigin
  -- ^ Select bits
  -> Q Exp
select _fields (Lit []) =
  fail $ "Unexpected empty literal."
select _fields (Lit lits) = do
  let size = length lits
  vec <- bitsToExpr lits
  return $ SigE
            -- Apply bLit to literal string
            vec
            -- Type signature:
            (AppT (ConT ''BitVector) (LitT $ NumTyLit $ toInteger size))

select fields (Field fieldn from downto) =
  select' (fields !! fieldn) [(from, downto)]

buildPackMatch
  :: DataReprAnn
  -> ConstrRepr
  -> Q Match
buildPackMatch dataRepr cRepr@(ConstrRepr name _ _ fieldanns) = do
  fieldNames <-
    mapM (\n -> newName $ "field" ++ show n) [0..length fieldanns-1]
  fieldPackedNames <-
    mapM (\n -> newName $ "fieldPacked" ++ show n) [0..length fieldanns-1]

  let packed fName = AppE (VarE 'pack) (VarE fName)
  let pack' pName fName = ValD (VarP pName) (NormalB $ packed fName) []
  let fieldPackedDecls = zipWith pack' fieldPackedNames fieldNames
  let origins = bitOrigins
                  (dataReprAnnToDataRepr' dataRepr)
                  (constrReprToConstrRepr' undefined cRepr)

  vec <- foldl1
              (\v1 v2 -> [| $v1 ++# $v2 |])
              (map (select $ map VarE fieldPackedNames) origins)

#if MIN_VERSION_template_haskell(2,18,0)
  return $ Match (ConP name [] (VarP <$> fieldNames)) (NormalB vec) fieldPackedDecls
#else
  return $ Match (ConP name (VarP <$> fieldNames)) (NormalB vec) fieldPackedDecls
#endif

-- | Build a /pack/ function corresponding to given DataRepr
buildPack
  :: DataReprAnn
  -> Q [Dec]
buildPack dataRepr@(DataReprAnn _name _size constrs) = do
  argNameIn    <- newName "toBePackedIn"
  argName      <- newName "toBePacked"
  constrs'     <- mapM (buildPackMatch dataRepr) constrs
  let packBody    = CaseE (VarE argName) constrs'
  let packLambda  = LamE [VarP argName] packBody
  let packApplied = (VarE 'dontApplyInHDL) `AppE` (VarE 'packXWith `AppE` packLambda) `AppE` (VarE argNameIn)
  let func        = FunD 'pack [Clause [VarP argNameIn] (NormalB packApplied) []]
  return [func]


-- | In Haskell apply the first argument to the second argument,
--   in HDL just return the second argument.
--
-- This is used in the generated pack/unpack to not do anything in HDL.
dontApplyInHDL :: (a -> b) -> a -> b
dontApplyInHDL f a = f a
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE dontApplyInHDL #-}
{-# ANN dontApplyInHDL hasBlackBox #-}

buildUnpackField
  :: Name
  -> Integer
  -> Q Exp
buildUnpackField valueName mask =
  let ranges = bitRanges mask in
  let vec = select' (VarE valueName) ranges in
  [| unpack $vec |]

buildUnpackIfE
  :: Name
  -> ConstrRepr
  -> Q (Guard, Exp)
buildUnpackIfE valueName (ConstrRepr name mask value fieldanns) = do
  let valueName' = return $ VarE valueName
  guard  <- NormalG <$> [| ((.&.) $valueName' mask) == value |]
  fields <- mapM (buildUnpackField valueName) fieldanns
  return (guard, foldl AppE (ConE name) fields)

-- | Build an /unpack/ function corresponding to given DataRepr
buildUnpack
  :: DataReprAnn
  -> Q [Dec]
buildUnpack (DataReprAnn _name _size constrs) = do
  argNameIn   <- newName "toBeUnpackedIn"
  argName     <- newName "toBeUnpacked"
  matches     <- mapM (buildUnpackIfE argName) constrs
  let fallThroughLast []      = []
      fallThroughLast [(_,e)] = [(NormalG (ConE 'True), e)]
      fallThroughLast (x:xs)  = x:fallThroughLast xs

  let unpackBody    = MultiIfE (fallThroughLast matches)
  let unpackLambda  = LamE [VarP argName] unpackBody
  let unpackApplied = (VarE 'dontApplyInHDL) `AppE` unpackLambda `AppE` (VarE argNameIn)
  let func          = FunD 'unpack [Clause [VarP argNameIn] (NormalB unpackApplied) []]
  return [func]

-- | Derives BitPack instances for given type. Will account for custom bit
-- representation annotations in the module where the splice is ran. Note that
-- the generated instance might conflict with existing implementations (for
-- example, an instance for /Maybe a/ exists, yielding conflicts for any
-- alternative implementations).
--
--
-- Usage:
--
-- @
-- data Color = R | G | B
-- {-# ANN module (DataReprAnn
--                   $(liftQ [t|Color|])
--                   2
--                   [ ConstrRepr 'R 0b11 0b00 []
--                   , ConstrRepr 'G 0b11 0b01 []
--                   , ConstrRepr 'B 0b11 0b10 []
--                   ]) #-}
-- deriveBitPack [t| Color |]
--
-- data MaybeColor = JustColor Color
--                 | NothingColor deriving (Generic,BitPack)
--
-- @
--
-- __NB__: Because of the way template haskell works the order here matters,
-- if you try to derive MaybeColor before deriveBitPack Color it will complain
-- about missing an instance BitSize Color.
deriveBitPack :: Q Type -> Q [Dec]
deriveBitPack typQ = do
  anns <- collectDataReprs
  typ  <- typQ
  rTyp <- resolveTypeSynonyms typ

  ann <- case filter (\(DataReprAnn t _ _) -> t == rTyp) anns of
              [a] -> return a
              []  -> fail "No custom bit annotation found."
              _   -> fail "Overlapping bit annotations found."

  packFunc   <- buildPack ann
  unpackFunc <- buildUnpack ann

  let (DataReprAnn _name dataSize _constrs) = ann

  let bitSizeInst = mkTySynInstD ''BitSize [typ] (LitT (NumTyLit $ toInteger dataSize))

  let bpInst = [ InstanceD
                   (Just Overlapping)
                   -- Overlap
                   []
                   -- Context
                   (AppT (ConT ''BitPack) typ)
                   -- Type
                   (bitSizeInst : packFunc ++ unpackFunc)
                   -- Declarations
               ]
  alreadyIsInstance <- isInstance ''BitPack [typ]
  if alreadyIsInstance then
    fail $ show typ ++ " already has a BitPack instance."
  else
    return bpInst
