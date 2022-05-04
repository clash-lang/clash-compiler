{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Clash.Netlist.Ast.Type
  ( HWType(..)
  , FilteredHWType(..)
  , PortDirection(..)
  , Size
  , DomainName
  , IsVoid
  , hwTypeDomain
  , hwTypeAttrs
  , stripFiltered
  , flattenFiltered
  , stripVoid
  , stripAttributes
  , isVoidMaybe
  , isVoid
  , isFilteredVoid
  , typeSize
  , conSize
  , isBiSignalIn
  , isBiSignalOut
  , containsBiSignalIn
  ) where

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

import Clash.Annotations.BitRepresentation (FieldAnn)
import Clash.Annotations.BitRepresentation.Internal (ConstrRepr', DataRepr')
import Clash.Signal.Internal
  (ActiveEdge, InitBehavior, ResetKind, ResetPolarity)

import Clash.Core.Var (Attr')
import Clash.Util (clogBase)

type Size = Int
type DomainName = Text
type IsVoid = Bool

-- | Tree structure indicating which constructor fields were filtered from
-- a type due to them being void. We need this information to generate stable
-- and/or user-defined port mappings.
data FilteredHWType =
  FilteredHWType HWType [[(IsVoid, FilteredHWType)]]
  deriving stock (Eq, Show)

-- TODO BiDirectional shouldn't be a type, directionality should be a property
-- of some other part of the AST, like a port, or a function parameter etc.

data PortDirection = In | Out
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)

-- | Representable hardware types
data HWType
  = Void (Maybe HWType)
  -- ^ Empty type. @Just Size@ for "empty" Vectors so we can still have
  -- primitives that can traverse e.g. Vectors of unit and know the length of
  -- that vector.
  | String
  -- ^ String type
  | Integer
  -- ^ Integer type (for parameters only)
  | Bool
  -- ^ Boolean type
  | Bit
  -- ^ Bit type
  | BitVector !Size
  -- ^ BitVector of a specified size
  | Index !Integer
  -- ^ Unsigned integer with specified (exclusive) upper bounder
  | Signed !Size
  -- ^ Signed integer of a specified size
  | Unsigned !Size
  -- ^ Unsigned integer of a specified size
  | Vector !Size !HWType
  -- ^ Vector type
  | MemBlob !Size !Size
  -- ^ MemBlob type
  | RTree !Size !HWType
  -- ^ RTree type
  | Sum !Text [Text]
  -- ^ Sum type: Name and Constructor names
  | Product !Text (Maybe [Text]) [HWType]
  -- ^ Product type: Name, field names, and field types. Field names will be
  -- populated when using records.
  | SP !Text [(Text, [HWType])]
  -- ^ Sum-of-Product type: Name and Constructor names + field types
  | Clock !DomainName
  -- ^ Clock type corresponding to domain /DomainName/
  | Reset !DomainName
  -- ^ Reset type corresponding to domain /DomainName/
  | Enable !DomainName
  -- ^ Enable type corresponding to domain /DomainName/
  | BiDirectional !PortDirection !HWType
  -- ^ Tagging type indicating a bidirectional (inout) port
  | CustomSP !Text !DataRepr' !Size [(ConstrRepr', Text, [HWType])]
  -- ^ Same as Sum-Of-Product, but with a user specified bit representation. For
  -- more info, see: Clash.Annotations.BitRepresentations.
  | CustomSum !Text !DataRepr' !Size [(ConstrRepr', Text)]
  -- ^ Same as Sum, but with a user specified bit representation. For more info,
  -- see: Clash.Annotations.BitRepresentations.
  | CustomProduct !Text !DataRepr' !Size (Maybe [Text]) [(FieldAnn, HWType)]
  -- ^ Same as Product, but with a user specified bit representation. For more
  -- info, see: Clash.Annotations.BitRepresentations.
  | Annotated [Attr'] !HWType
  -- ^ Annotated with HDL attributes
  | KnownDomain !DomainName !Integer !ActiveEdge !ResetKind !InitBehavior !ResetPolarity
  -- ^ Domain name, period, active edge, reset kind, initial value behavior
  | FileType
  -- ^ File type for simulation-level I/O
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)

hwTypeDomain :: HWType -> Maybe DomainName
hwTypeDomain = \case
  Clock dom -> Just dom
  Reset dom -> Just dom
  Enable dom -> Just dom
  KnownDomain dom _ _ _ _ _ -> Just dom
  _ -> Nothing

-- | Extract hardware attributes from Annotated. Returns an empty list if
-- non-Annotated given or if Annotated has an empty list of attributes.
hwTypeAttrs :: HWType -> [Attr']
hwTypeAttrs = \case
  Annotated attrs _ -> attrs
  _ -> []

-- | Throw away information indicating which constructor fields were filtered
-- due to being void.
stripFiltered :: FilteredHWType -> HWType
stripFiltered (FilteredHWType hwty _) = hwty

flattenFiltered :: FilteredHWType -> [[Bool]]
flattenFiltered (FilteredHWType _ filtered) = map (map fst) filtered

-- | Strip as many "Void" layers as possible. Might still return a Void if the
-- void doesn't contain a hwtype.
stripVoid :: HWType -> HWType
stripVoid = \case
  Void (Just e) -> stripVoid e
  ty -> ty

-- | Strips one or more layers of attributes from a HWType; stops at first
-- non-Annotated. Accumulates all attributes of nested annotations.
stripAttributes :: HWType -> ([Attr'], HWType)
stripAttributes = go
 where
  go = \case
    Annotated attrs ty ->
      first (attrs <>) (go ty)

    ty ->
      ([], ty)

isVoidMaybe :: IsVoid -> Maybe HWType -> IsVoid
isVoidMaybe dflt = maybe dflt isVoid

-- | Determines if type is a zero-width construct ("void")
isVoid :: HWType -> IsVoid
isVoid = \case
  Void{} -> True
  _ -> False

-- | Same as @isVoid@, but on @FilteredHWType@ instead of @HWType@
isFilteredVoid :: FilteredHWType -> IsVoid
isFilteredVoid = isVoid . stripFiltered

-- | Determines the bitsize of a type. For types that don't get turned
-- into real values in hardware (string, integer) the size is 0.
typeSize :: HWType -> Int
typeSize Void{} = 0
typeSize FileType = 32 -- (ref. page 287 of IEEE 1364-2005)
typeSize String = 0
typeSize Integer = 0
typeSize KnownDomain{} = 0
typeSize Bool = 1
typeSize Bit = 1
typeSize Clock{} = 1
typeSize Reset{} = 1
typeSize Enable{} = 1
typeSize (BitVector i) = i
typeSize (Index 0) = 0
typeSize (Index 1) = 1
typeSize (Index u) = fromMaybe 0 (clogBase 2 u)
typeSize (Signed i) = i
typeSize (Unsigned i) = i
typeSize (Vector n el) = n * typeSize el
typeSize (MemBlob n m) = n * m
typeSize (RTree d el) = (2^d) * typeSize el
typeSize t@(SP _ cons) = conSize t +
  maximum (map (sum . map typeSize . snd) cons)
typeSize (Sum _ dcs) = fromMaybe 0 . clogBase 2 . toInteger $ length dcs
typeSize (Product _ _ tys) = sum $ map typeSize tys
typeSize (BiDirectional In h) = typeSize h
typeSize (BiDirectional Out _) = 0
typeSize (CustomSP _ _ size _) = fromIntegral size
typeSize (CustomSum _ _ size _) = fromIntegral size
typeSize (CustomProduct _ _ size _ _) = fromIntegral size
typeSize (Annotated _ ty) = typeSize ty

-- | Determines the bitsize of the constructor of a type
conSize :: HWType -> Int
conSize = \case
  SP _ cons -> fromMaybe 0 . clogBase 2 . toInteger $ length cons
  t -> typeSize t

isBiSignalIn :: HWType -> Bool
isBiSignalIn = \case
  BiDirectional In _ -> True
  _ -> False

isBiSignalOut :: HWType -> Bool
isBiSignalOut = \case
  BiDirectional In _ -> True
  _ -> False

containsBiSignalIn :: HWType -> Bool
containsBiSignalIn = go
 where
  go = \case
    Product _ _ tys -> any go tys
    SP _ tyss -> any (any go . snd) tyss
    Vector _ ty -> go ty
    RTree _ ty -> go ty
    ty -> isBiSignalIn ty
