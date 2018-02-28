{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MagicHash #-}


module Clash.Annotations.BitRepresentation.Deriving where

import Prelude

import Clash.Annotations.BitRepresentation
import Clash.Class.BitPack

import Data.Bits
import Data.List (mapAccumL)
import Data.Proxy

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import GHC.Exts (Int(I#))
import GHC.Integer.Logarithms (integerLog2#)

deriveEncodingOneHot :: Q Type -> Q [Dec]
deriveEncodingOneHot = generateModuleAnn encodingOneHot
deriveEncodingOneHot' :: Q Type -> Q [Dec]
deriveEncodingOneHot' = generateModuleAnn' encodingOneHot'

deriveEncodingNormal :: Q Type -> Q [Dec]
deriveEncodingNormal = generateModuleAnn encodingNormal

deriveEncodingWide :: Q Type -> Q [Dec]
deriveEncodingWide = generateModuleAnn encodingWide

generateModuleAnn
  :: (Q Type -> Q Exp)
  -> Q Type
  -> Q [Dec]
generateModuleAnn derivationFunc typ
  = do
       let repr = derivationFunc typ
       pragma <- pragAnnD ModuleAnnotation repr

       -- we only really need the pragma, but the declaration is helpful
       -- for debugging
       typ' <- typ
       nm <- newName $ "myRepr__" ++ typeToSafeName typ'
       decl <- valD (varP nm) (normalB repr) []
       tySig <- sigD nm [t| DataReprAnn |]
       return [pragma, decl, tySig]

generateModuleAnn'
  :: (Q Type -> Q (Exp, [Dec]))
  -> Q Type -> Q [Dec]
generateModuleAnn' f q
  = do
       (repr,extras) <- f q
       pragma <- pragAnnD ModuleAnnotation (return repr)

       -- we only really need the pragma, but the declaration is helpful
       -- for debugging
       ty <- q
       nm <- newName $ "myRepr__" ++ typeToSafeName ty
       decl <- valD (varP nm) (normalB $ return repr) []
       tySig <- sigD nm [t| DataReprAnn |]
       return $ pragma : decl : tySig : extras

typeToSafeName :: Type -> String
typeToSafeName = map convertChars . show . ppr
  where
    convertChars c | c `elem` ". \t\n(),-" = '_'
                   | otherwise = c

encodingOneHot
  :: Q Type
  -> Q Exp
encodingOneHot qty
  = do ty <- qty
       let tyNm = getTyNm ty
       (TyConI tyCon) <- reify tyNm
       let
           constructors = getCons tyCon
           width :: Integer
           width = fromIntegral $ length constructors
           f :: Int -> Con -> Q Exp
           f n con = do
             let conNm = conName con
                 mask = 1 `shiftL` n :: Integer
                 value = mask
             [| ConstrRepr conNm mask value $(listE []) |]

           reps :: [Q Exp]
           reps = zipWith f [0..] constructors

       [| DataReprAnn ty width $(listE reps) |]

-- hack that adds BitPack instance (without functional pack/unpack, just BitSize)
encodingOneHot'
  :: Q Type
  -> Q (Exp,[Dec])
encodingOneHot' qty = do
  ty <- qty
  let tyNm = getTyNm ty
  (TyConI tyCon) <- reify tyNm
  let
      cs = getCons tyCon
      width :: Integer
      width = fromIntegral $ length cs
      f :: Int -> Con -> Q Exp
      f n con = do
        let conNm = conName con
            mask = 1 `shiftL` n :: Integer
            value = mask
        [| ConstrRepr conNm mask value $(listE []) |]

      reps :: [Q Exp]
      reps = zipWith f [0..] cs
      body = [| DataReprAnn ty width $(listE reps) |]

      extra = instanceD (return []) [t| BitPack $(qty) |] [declSize,declPack,declUnpack]
      declPack = funD 'pack [clause [] (normalB $ varE 'undefined) []] -- TODO pack implementation
      declUnpack = funD 'unpack [clause [] (normalB $ varE 'undefined) []] -- TODO idem for unpack
      declSize = tySynInstD ''BitSize (tySynEqn [qty] (litT $ numTyLit width))

  bodyE <- body
  extraE <- extra
  return (bodyE, [extraE])


encodingNormal
  :: Q Type
  -> Q Exp
encodingNormal qty = do
  ty <- qty
  let tyNm = getTyNm ty
  (TyConI tyCon) <- reify tyNm
  let
    cs = getCons tyCon
    consWidth :: Integer
    consWidth = integerLog2Ceil $ fromIntegral $ length cs

    fieldsOfConstrs = map fieldTypes cs

    allWidths :: [[Q Exp]]
    allWidths = map (map typeSize) fieldsOfConstrs
    allWidths' :: Q [[Exp]]
    allWidths' = sequence $ map sequence allWidths
    allWidths'' :: Q [Exp]
    allWidths'' = fmap (fmap ListE) allWidths'

  allWidths''' <- allWidths'

  let
    allWidths2 :: Q Exp
    allWidths2 = [| map sum $(fmap ListE allWidths'') |]

    fieldBits = [| maximum $(allWidths2) |]

    width :: Q Exp
    width = [| consWidth + $(fieldBits) |]

    consMask = [| let w = $(width) in maskFromDownto (w-1) (w-consWidth) |]

    f :: Integer -> Con -> [Exp] -> Q Exp
    f n con fieldWidths = do
      let conNm = conName con
          value = [| n `shiftL` (fromIntegral $fieldBits)|]
      [| ConstrRepr
           conNm
           $consMask
           $value
           (fieldAnnsNormal $(width) consWidth $(return $ ListE fieldWidths))
           |]

    reps :: [Q Exp]
    reps = zipWith3 f [0..] cs allWidths'''

  [| DataReprAnn ty $(width) $(listE reps) |]

-- | Construct field bitmasks for normal encoding
fieldAnnsNormal :: Integer -> Integer -> [Integer] -> [Integer]
fieldAnnsNormal totalWidth consWidth fieldWidths = go (totalWidth-consWidth) fieldWidths
  where
    go n (f:fs) = maskFromDownto (n-1) (n-f) : go (n-f) fs
    go _ [] = []


encodingWide :: Q Type -> Q Exp
encodingWide qty = do
  ty <- qty
  let tyNm = getTyNm ty
  (TyConI tyCon) <- reify tyNm
  let
    cs = getCons tyCon
    consWidth :: Integer
    consWidth = integerLog2Ceil $ fromIntegral $ length cs

    fieldsOfConstrs = map fieldTypes cs

    allWidths :: [[Q Exp]]
    allWidths = map (map typeSize) fieldsOfConstrs
    allWidths' :: Q [[Exp]]
    allWidths' = sequence $ map sequence allWidths
    allWidths'' :: Q [Exp]
    allWidths'' = fmap (fmap ListE) allWidths'

    allWidths2 :: Q Exp
    allWidths2 = [| map sum $(fmap ListE allWidths'') |]

    fieldBits = [| sum $allWidths2 |]

    width :: Q Exp
    width = [| consWidth + $fieldBits |]

    consMask = [| let w = $width in maskFromDownto (w-1) (w-consWidth) |]

    fieldMaskss :: Q Exp
    fieldMaskss = [| fieldMasksWide $(width) consWidth $(fmap (ListE . fmap ListE) allWidths') |]

    conNms = map conName cs
    body  = [|
      \consMaskE fieldBitsE fieldMaskssE ->
        let reps = consRepsWide consMaskE fieldBitsE [0..] conNms fieldMaskssE in
        DataReprAnn ty $width reps
      |]

  appsE [body, consMask, fieldBits, fieldMaskss]


consRepWide
  :: Integer
  -> Integer
  -> Integer
  -> Name
  -> [Integer]
  -> ConstrRepr
consRepWide consMask fieldBits n conNm fieldMasks =
 let value = n `shiftL` (fromIntegral fieldBits)
 in ConstrRepr conNm consMask value fieldMasks

consRepsWide
  :: Integer
  -> Integer
  -> [Integer]
  -> [Name]
  -> [[Integer]]
  -> [ConstrRepr]
consRepsWide consMask fieldBits conCodes conNms fieldMasks =
    zipWith3 (consRepWide consMask fieldBits) conCodes conNms fieldMasks


type BitSpan = (Integer,Integer)
type BitMask = Integer

-- | Construct field bitmasks for wide encoding
fieldMasksWide
  :: Integer
  -> Integer
  -> [[Integer]]
  -> [[Integer]]
fieldMasksWide totalWidth consWidth fieldWidthss =
  map (map maskSpan) $ fieldSpansWide totalWidth consWidth fieldWidthss

fieldSpansWide
  :: Integer
  -> Integer
  -> [[Integer]]
  -> [[BitSpan]]
fieldSpansWide totalWidth consWidth fieldWidthss =
  let go a xss = snd $ mapAccumL (mapAccumL (\n w -> (n-w, (n,n-w+1)))) a xss in
  go (totalWidth-consWidth-1) fieldWidthss

typeSize :: Type -> Q Exp
typeSize ty = [| natVal (Proxy :: Proxy (BitSize $(return ty))) |]

-- | Construct bitmask from i downto j (inclusive)
maskFromDownto :: Integer -> Integer -> BitMask
maskFromDownto i j
  | i < j = error $ {-$(curLoc) ++-} show i ++ " must not be smaller then " ++ show j
  | j < 0 = error $ {-$(curLoc) ++-} "lowerbound " ++ show j ++ " is smaller than 0"
  | otherwise = maskBits i - maskBits (j-1)

maskSpan :: BitSpan -> BitMask
maskSpan (i,j) = maskFromDownto i j

-- | Construct bitmask with the n lowest bits set
maskBits :: Integer -> BitMask
maskBits n = (1 `shiftL` (fromIntegral $ n+1)) - 1

getCons :: Dec -> [Con]
getCons tyCon = case tyCon of
  DataD _ _nm _tyVars _ cs _   -> cs
  NewtypeD _ _nm _tyVars _ c _ -> [c]
  _ -> error $ {-$(curLoc) ++-} "Can't derive encoding for a type synonym"

fieldTypes :: Con -> [Type]
fieldTypes con = case con of
  NormalC _nm bTys -> map snd bTys
  RecC    _nm vbTys -> map (\(_,_,ty) -> ty) vbTys
  InfixC (_,ty1) _nm (_,ty2) -> [ty1,ty2]
  _ -> error $ {-$(curLoc) ++-} "No support for constructors like: " ++ show con

collectTyArgs :: Type -> Maybe (Name,[Type])
collectTyArgs = go []
  where
    go args (AppT ty1 ty2) = go (ty2:args) ty1
    go args (ConT nm) = Just (nm,args)
    go _    _         = Nothing

conName :: Con -> Name
conName c = case c of
  NormalC nm _  -> nm
  RecC    nm _  -> nm
  InfixC _ nm _ -> nm
  _ -> error $ {-$(curLoc) ++-} "No GADT support"

getTyNm :: Type -> Name
getTyNm ty =
  case go ty of
    Just t -> t
    Nothing ->
      error $ {-$(curLoc) ++-} unwords [ "can only handle type constants and"
                                       , "type application, not:"
                                       , show (ppr ty) ]

  where
    go :: Type -> Maybe Name
    go (ConT tyNm)  = Just tyNm
    go (AppT ty1 _) = go ty1
    go _            = Nothing

integerLog2Ceil :: Integer -> Integer
integerLog2Ceil n =
  let nlog2 = fromIntegral $ I# (integerLog2# n) in
  if n > 2^nlog2 then nlog2 + 1 else nlog2
