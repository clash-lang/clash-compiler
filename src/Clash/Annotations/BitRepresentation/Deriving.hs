{-|
Copyright  :  (C) 2017, Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

This module houses functions deriving BitPack instances given a custom bit
representation and function to derive some (alternative) representations
automatically.

For example, we can pack a Maybe Color (where Color is a type with 3 possible
values) in only 2 bits using:

@
data Color
  = Red
  | Green
  | Blue

{-# ANN module (
  DataReprAnn
    $(reprType [t| Color |])
    2
    [ ConstrRepr
        'Red
        0b11
        0b00
        []
    , ConstrRepr
        'Green
        0b11
        0b01
        []
    , ConstrRepr
        'Blue
        0b11
        0b10
        []
    ]) #-}

{-# ANN module (
  DataReprAnn
    $(reprType [t| Maybe Color |])
    -- ^ Type to annotate
    2
    -- ^ Size of whole data type
    [ ConstrRepr
        'Nothing
        0b11
        -- ^ Mask for this constructor
        0b11
        -- ^ Value to match this constructor on
        []
    , ConstrRepr
        'Just
        0b00
        -- ^ Mask for this constructor
        0b00
        -- ^ Value to match this constructor on
        [0b11]
        -- ^ Masks of fields of Just
    ]) #-}
@

To derive BitPack instances from these annotations, see
Clash.Annotations.BitRepresentation.Deriving.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift #-}

module Clash.Annotations.BitRepresentation.Deriving
  ( deriveDefaultAnnotation
  , deriveAnnotation
  , deriveBitPack
  , simpleDerivator
  , Derivator
  , DataReprAnnExp
  , ConstructorType(..)
  , FieldsType(..)
  ) where

import GHC.Exts
import GHC.Integer.Logarithms

import Data.Bits ((.&.))
import Data.List (mapAccumL, zipWith4)
import Data.Bits (shiftL, shiftR, complement)
import Data.Proxy (Proxy(..))
import Data.Maybe (fromJust)

import qualified Data.Map as Map
import qualified Data.Text.Lazy as Text

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import GHC.TypeLits (natVal)

import Clash.Sized.BitVector (BitVector, high, low, (++#))
import Clash.Class.Resize  (resize)
import Clash.Class.BitPack (BitPack, BitSize, pack, unpack)
import Clash.Annotations.BitRepresentation ( DataReprAnn(..)
                                           , DataRepr'(..)
                                           , ConstrRepr'(..)
                                           , ConstrRepr(..)
                                           , BitMask
                                           , Value
                                           , reprType
                                           , thTypeToType'
                                           , dataReprAnnToDataRepr'
                                           )

import Clash.Annotations.BitRepresentation.Util ( bitOrigins
                                                , BitOrigin(..)
                                                , bitRanges
                                                , Bit(..)
                                                )

type NameMap = Map.Map Name Type

-- | DataReprAnn as template haskell expression
type DataReprAnnExp = Exp

-- | A derivator derives a bit representation given a type
type Derivator = Type -> Q DataReprAnnExp

-- | Indicates how to pack constructor for simpleDerivator
data ConstructorType
  = Count
  -- ^ First constructor will be encoded as 0b0, the second as 0b1, the third
  -- as 0b10, etc.
  | OneHot
  -- ^ Reserve a single bit for each constructor marker.

-- | Indicates how to pack (constructor) fields for simpleDerivator
data FieldsType
  = Overlap
  -- ^ Store fields of different constructors at (possibly) overlapping bit
  -- positions. That is, a data type with two constructors with each two fields
  -- of each one bit will take /two/ bits for its whole representation (plus
  -- constructor bits). This is the default behaviour of Clash.
  | Wide
  -- ^ Store fields of different constructs at non-overlapping positions. That
  -- is, a data type with two constructors with each two fields of each one bit
  -- will take /four/ bits for its whole representation (plus constructor bits).

-- | Determine most significant bit set for given integer.
--
-- TODO: Current complexity is O(n). We could probably use machine instructions
-- for ~constant complexity.
msb :: Integer -> Integer
msb 0 = error $ "Most significant bit does not exist for zero."
msb 1 = 0
msb n = 1 + msb (shiftR n 1)

-- | Integer version of (ceil . log2). Can handle arguments up to 2^(2^WORDWIDTH).
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

-- | Returns size in number of bits of given type. Relies on the presence of a
-- BitSize implementation. Tries to recognize literal values and return a simple
-- expression.
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

-- | Generate bitmask from a given bit, with a certain size
bitmask
  :: Integer
  -- ^ Bitmask starts at bit /n/
  -> Integer
  -- ^ Bitmask has size /m/
  -> Integer
bitmask _start 0    = 0
bitmask start  size
  | start < 0        = error $ "Start cannot be <0. Was: " ++ show start
  | size < 0         = error $ "Size cannot be <0. Was: " ++ show size
  | start + 1 < size = error $ "Start + 1 (" ++ show start ++ " - 1) cannot be smaller than size (" ++ show size ++  ")."
  | otherwise        = shiftL (2^size - 1) $ fromIntegral (start - (size - 1))

buildConstrRepr
  :: Q Exp
  -- ^ Data size (excluding constructor size)
  -> Name
  -- ^ Constr name
  -> [Exp]
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
    $(return $ ListE fieldAnns)
  |]
  where
    mask  = [| shiftL constrMask  (fromIntegral $ $dataSize)|]
    value = [| shiftL constrValue (fromIntegral $ $dataSize)|]

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

countConstructor :: [Integer] -> [(BitMask, Value)]
countConstructor ns = zip (repeat mask) ns
  where
    maskSize = integerLog2Ceil $ maximum ns
    mask = 2^maskSize - 1

oneHotConstructor :: [Integer] -> [(BitMask, Value)]
oneHotConstructor ns = zip values values
  where
    values = [shiftL 1 (fromIntegral n) | n <- ns]

overlapFieldAnns :: [[Exp]] -> [Q [Exp]]
overlapFieldAnns fieldSizess = map go fieldSizess
  where
    fieldSizess'  = ListE $ map ListE fieldSizess
    constructorSizes = [| map sum $(return fieldSizess') |]
    go fieldSizes =
      sequence $
      snd $
      mapAccumL
        (\start size -> ([| $start - $size |], [| bitmask $start $size |]))
        [| maximum $constructorSizes - 1 |]
        (map return fieldSizes)

wideFieldAnns :: [[Exp]] -> [Q [Exp]]
wideFieldAnns fieldSizess = zipWith id (map go constructorOffsets) fieldSizess
  where
    constructorSizes =
      map (AppE (VarE 'sum)) (map ListE fieldSizess)

    constructorOffsets :: [Q Exp]
    constructorOffsets =
      init $
      scanl
        (\offset size -> [| $offset + $size |])
        [| 0 |]
        (map return constructorSizes)

    dataSize = [| sum $(return $ ListE constructorSizes) |]

    go :: Q Exp -> [Exp] -> Q [Exp]
    go offset fieldSizes =
      sequence $
      snd $
      mapAccumL
        (\start size -> ([| $start - $size |], [| bitmask $start $size |]))
        [| $dataSize - 1 - $offset |]
        (map return fieldSizes)

-- | Derive DataRepr' for a specific type.
deriveDataRepr
  :: ([Integer] -> [(BitMask, Value)])
  -- ^ Constructor derivator
  -> ([[Exp]] -> [Q [Exp]])
  -- ^ Field derivator
  -> Derivator
deriveDataRepr constrDerivator fieldsDerivator typ = do
  info <- reify tyConstrName
  case info of
    (TyConI (DataD [] _constrName vars _kind dConstructors _clauses)) ->
      let varMap = Map.fromList $ zip (map tyVarBndrName vars) typeArgs in
      let resolvedConstructors = map (resolveCon varMap) dConstructors in do

      -- Get sizes and names of all constructors
      (constrNames, fieldSizess) <-
        unzip <$> (mapM constrFieldSizes resolvedConstructors)

      let
        (constrMasks, constrValues) =
          unzip $ constrDerivator [0..fromIntegral $ length dConstructors - 1]

      let constrSize    = 1 + (msb $ maximum constrMasks)
      fieldAnns        <- sequence $ fieldsDerivator fieldSizess
      let fieldAnnsFlat = return $ ListE $ concat fieldAnns

      let dataSize | null $ concat fieldAnns = [| 0 |]
                   | otherwise = [| 1 + (msb $ maximum $ $fieldAnnsFlat) |]


      -- Determine at which bits various fields start
      let constrReprs = zipWith4
                          (buildConstrRepr dataSize)
                          constrNames
                          fieldAnns
                          constrMasks
                          constrValues

      [| DataReprAnn
          $(reprType $ return typ)
          ($dataSize + constrSize)
          $(listE constrReprs) |]
    _ ->
      error $ {-$(curLoc) ++-} "Could not derive dataRepr for: " ++ show info

    where
      (ConT tyConstrName, typeArgs) = collectTypeArgs typ

-- | Simple derivators change the (default) way Clash stores data types. It
-- assumes no overlap between constructors and fields.
simpleDerivator :: ConstructorType -> FieldsType -> Derivator
simpleDerivator ctype ftype = deriveDataRepr constrDerivator fieldsDerivator
  where
    constrDerivator =
      case ctype of
        Count -> countConstructor
        OneHot -> oneHotConstructor

    fieldsDerivator =
      case ftype of
        Overlap -> overlapFieldAnns
        Wide -> wideFieldAnns

-- | Derives bit representation corresponding to the default manner in which
-- Clash stores types.
defaultDerivator :: Derivator
defaultDerivator = simpleDerivator Count Overlap

-- | Derives bit representation corresponding to the default manner in which
-- Clash stores types.
deriveDefaultAnnotation :: Q Type -> Q [Dec]
deriveDefaultAnnotation = deriveAnnotation defaultDerivator

deriveAnnotation :: Derivator -> Q Type -> Q [Dec]
deriveAnnotation deriv typ =
  return <$> pragAnnD ModuleAnnotation (deriv =<< typ)

----------------------------------------------------
------------ DERIVING BITPACK INSTANCES ------------
----------------------------------------------------

-- | Collect data reprs of current module
collectDataReprs :: Q [DataRepr']
collectDataReprs = do
  thisMod <- thisModule
  map dataReprAnnToDataRepr' <$> reifyAnnotations (AnnLookupModule thisMod)

group :: [Bit] -> [(Int, Bit)]
group [] = []
group bs = (length head', head bs) : rest
  where
    tail' = dropWhile (==head bs) bs
    head' = takeWhile (==head bs) bs
    rest  = group tail'

bitToExpr' :: (Int, Bit) -> Q Exp
bitToExpr' (0, _) = error $ {-$(curLoc) ++-} "Unexpected group length: 0"
bitToExpr' (1, H) = lift high
bitToExpr' (1, L) = lift low
bitToExpr' (1, _) = lift low
bitToExpr' (numTyLit' -> n, H) =
  [| complement (resize $(lift low) :: BitVector $n) |]
bitToExpr' (numTyLit' -> n, L) =
  [| resize $(lift low) :: BitVector $n |]
bitToExpr' (numTyLit' -> n, _) =
  [| resize $(lift low) :: BitVector $n |]

bitsToExpr :: [Bit] -> Q Exp
bitsToExpr [] = error $ {-$(curLoc) ++-} "Unexpected empty bit list"
bitsToExpr bits =
  foldl1
    (\v1 v2 -> [| $v1 ++# $v2 |])
    (map bitToExpr' $ group bits)

numTyLit' :: Integral a => a -> Q Type
numTyLit' n = LitT <$> (numTyLit $ fromIntegral n)

-- | Select a list of ranges from a bitvector expression
select'
  :: Exp
  -> [(Int, Int)]
  -> Q Exp
select' _vec [] =
  error $ {-$(curLoc) ++ -}"Unexpected empty list of intervals"
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
                  (LitE $ IntegerL $ fromIntegral downto) in

        SigE
          -- Select from whole vector
          (AppE (VarE 'resize) shifted)
          -- Type signature:
          (AppT (ConT ''BitVector) (LitT $ NumTyLit $ fromIntegral size))

-- | Select a range (bitorigin) from a bitvector
select
  :: [Exp]
  -- ^ BitVectors of fields
  -> BitOrigin
  -- ^ Select bits
  -> Q Exp
select _fields (Lit []) =
  error $ {-$(curLoc) ++-} "Unexpected empty literal."
select _fields (Lit lits) = do
  let size = fromIntegral $ length lits
  vec <- bitsToExpr lits
  return $ SigE
            -- Apply bLit to literal string
            vec
            -- Type signature:
            (AppT (ConT ''BitVector) (LitT $ NumTyLit size))

select fields (Field fieldn from downto) =
  select' (fields !! fieldn) [(from, downto)]

buildPackMatch
  :: Integer
  -> ConstrRepr'
  -> Q Match
buildPackMatch dataSize (ConstrRepr' qName _constrN mask value fieldanns) = do
  constr <- fromJust <$> lookupValueName (Text.unpack qName)

  fieldNames <-
    mapM (\n -> newName $ "field" ++ show n) [0..length fieldanns-1]
  fieldPackedNames <-
    mapM (\n -> newName $ "fieldPacked" ++ show n) [0..length fieldanns-1]

  let packed fName = AppE (VarE 'pack) (VarE fName)
  let pack' pName fName = ValD (VarP pName) (NormalB $ packed fName) []
  let fieldPackedDecls = zipWith pack' fieldPackedNames fieldNames
  let origins = bitOrigins (fromIntegral dataSize) (mask, value, fieldanns)

  vec <- foldl1
              (\v1 v2 -> [| $v1 ++# $v2 |])
              (map (select $ map VarE fieldPackedNames) origins)

  return $ Match (ConP constr (VarP <$> fieldNames)) (NormalB vec) fieldPackedDecls

-- | Build a /pack/ function corresponding to given DataRepr
buildPack
  :: Type
  -> DataRepr'
  -> Q [Dec]
buildPack argTy (DataRepr' _name size constrs) = do
  argName      <- newName "toBePacked"
  let resTy     = AppT (ConT ''BitVector) (LitT $ NumTyLit size)
  let funcSig   = SigD 'pack (AppT (AppT ArrowT argTy) resTy)
  constrs'     <- mapM (buildPackMatch size) constrs
  let body      = CaseE (VarE argName) constrs'
  let func      = FunD 'pack [Clause [VarP argName] (NormalB body) []]
  return $ [funcSig, func]

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
  -> Integer
  -> ConstrRepr'
  -> Q (Guard, Exp)
buildUnpackIfE valueName _dataSize (ConstrRepr' qName _constrN mask value fieldanns) = do
  let valueName' = return $ VarE valueName
  constr <- ConE <$> (fromJust <$> (lookupValueName (Text.unpack qName)))
  guard  <- NormalG <$> [| ((.&.) $valueName' mask) == value |]
  fields <- mapM (buildUnpackField valueName) fieldanns
  return $ (guard, foldl AppE constr fields)

---- | Build an /unpack/ function corresponding to given DataRepr
buildUnpack
  :: Type
  -> DataRepr'
  -> Q [Dec]
buildUnpack resTy (DataRepr' _name size constrs) = do
  argName <- newName "toBeUnpacked"
  let argTy    = AppT (ConT ''BitVector) (LitT $ NumTyLit size)
  let funcSig  = SigD 'unpack (AppT (AppT ArrowT argTy) resTy)
  matches     <- mapM (buildUnpackIfE argName size) constrs
  err         <- [| error $ "Could not match constructor for: " ++ show $(varE argName) |]
  let body     = MultiIfE $ matches ++ [(NormalG (ConE 'True), err)]
  let func     = FunD 'unpack [Clause [VarP argName] (NormalB body) []]
  return $ [funcSig, func]

-- | Derives BitPack instances for given type. Will account for custom bit
-- representation annotations in the module where the splice is ran. Note that
-- the generated instance might conflict with existing implementations (for
-- example, an instance for /Maybe a/ exists, yielding conflicts for any
-- alternative implementations).
deriveBitPack :: Q Type -> Q [Dec]
deriveBitPack typQ = do
  anns <- collectDataReprs
  typ  <- typQ
  typ' <- (return . thTypeToType') =<< typQ

  let ann = case filter (\(DataRepr' t _ _) -> t == typ') anns of
              [a] -> a
              []  -> error $ {-$(curLoc) ++-} "No custom bit annotation found."
              _   -> error $ {-$(curLoc) ++-} "Overlapping bit annotations found."

  packFunc   <- buildPack typ ann
  unpackFunc <- buildUnpack typ ann

  let (DataRepr' _name dataSize _constrs) = ann

  let bitSizeInst = TySynInstD
                      ''BitSize
                      (TySynEqn
                        [typ]
                        (LitT (NumTyLit $ fromIntegral dataSize)))

  let bpInst = [ InstanceD
                   (Just Overlapping)
                   -- ^ Overlap
                   []
                   -- ^ Context
                   (AppT (ConT ''BitPack) typ)
                   -- ^ Type
                   (bitSizeInst : packFunc ++ unpackFunc)
                   -- ^ Declarations
               ]
  alreadyIsInstance <- isInstance ''BitPack [typ]
  if alreadyIsInstance then
    error $ show typ ++ " already has a BitPack instance."
  else
    return bpInst
