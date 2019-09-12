{-|

This module can automatically generate TopEntity definitions from 'Clash.NamedTypes'
annotations. Type/data families will resolve a single step (non-recusive).

See "Clash.Tests.TopEntityGeneration" for more examples.

@
import Clash.Annotations.TopEntityGen

data Named
  = Named
  { name1 :: "named1" ::: BitVector 3
  , name2 :: "named2" ::: BitVector 5
  }

topEntity :: "tup1" ::: Signal System (Int, Bool)
          -> "tup2" ::: (Signal System Int, Signal System Bool)
          -> "tup3" ::: Signal System ("int":::Int, "bool":::Bool)
          -> "tup4" ::: ("int":::Signal System Int, "bool":::Signal System Bool)
          -> "custom" ::: Signal System Named
          -> "outTup" ::: Signal System ("outint":::Int, "outbool":::Bool)
topEntity = undefined
makeTopEntity 'topEntity
-- ===>
--  {-# ANN topEntity Synthesize "topEntity3"
--     [ PortName "tup1"
--     , PortName "tup2"
--     , PortProduct "tup3" [PortName "int",PortName "bool"]
--     , PortProduct "tup4" [PortName "int",PortName "bool"]
--     , PortProduct "custom" [PortName "named1",PortName "named2"]
--     ]
--     (PortProduct "outTup" [PortName "outint",PortName "outbool"])
--     #-}
@

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- Required to 'makeBaseFunctor' of 'Language.Haskell.TH.Syntax.Type'

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Clash.Annotations.TH
  ( -- * To create a Synthesize annotation pragma
    makeTopEntity
  , makeTopEntityWithName
  , makeTopEntityWithName'
    -- * To create a TopEntity value
  , buildTopEntity
  , buildTopEntity'
  )
where

import           Data.Foldable                  ( fold
                                                , find
                                                )
import qualified Data.Set                      as Set
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup                as Semigroup
#endif
import           Language.Haskell.TH

import           Data.Functor.Foldable          ( cata, para, embed )
import           Data.Functor.Foldable.TH
import           Control.Lens                   ( view
                                                , _3
                                                )
import           Language.Haskell.TH.Instances  ( )

import           Clash.Annotations.TopEntity    ( PortName(..)
                                                , TopEntity(..)
                                                )
import           Clash.NamedTypes

$(makeBaseFunctor ''Type)

-- | A datatype to track failing naming in a subtree.
data Naming a = Complete [a] | HasFail String

instance Semigroup (Naming a) where
  Complete a <> Complete b = Complete $ a ++ b
  HasFail e1 <> HasFail e2 = HasFail $ e1 ++ e2
  _ <> HasFail e           = HasFail e
  HasFail e <> _           = HasFail e

instance Monoid (Naming a) where
  mempty = Complete []
#if !(MIN_VERSION_base(4,11,0))
  mappend = (Semigroup.<>)
#endif

-- | A template haskell helper function. Get the 'Type's from a 'Con'.
getTypes :: Con -> [Type]
getTypes (NormalC _ xs)    = map snd xs
getTypes (RecC _ xs)       = map (view _3) xs
getTypes (InfixC x _ y)    = [snd x, snd y]
getTypes (ForallC _ _ c)   = getTypes c
getTypes (GadtC _ xs _)    = map snd xs
getTypes (RecGadtC _ xs _) = map (view _3) xs

-- | A template haskell helper function. Get the 'Name' of a 'Con'. These names
-- are only used in type/data family resolution which is best effort, 
-- so for Gadts we just take the first name.
getName :: Con -> Name
getName (NormalC n _)          = n
getName (RecC    n _)          = n
getName (InfixC  _ n _)        = n
getName (ForallC _ _ c)        = getName c
getName (GadtC    (n : _) _ _) = n
getName (RecGadtC (n : _) _ _) = n
getName (GadtC    [] _ _) = error "Encountered GADT with no constructors?"
getName (RecGadtC [] _ _) = error "Encountered GADT with no constructors?"

-- | Flag constructors with partially named fields as failing.
namesFromConstructor
  :: Set.Set Name
  -> Type
  -> Info
  -> Con
  -> Q (Naming PortName)
namesFromConstructor seen split info (getTypes -> xs) =
  (mconcat <$> mapM (expandFamiliesAndGatherNames seen split) xs)
  >>= \case
    Complete names | length names > 0 && length names /= length xs ->
      return $ HasFail $ "Partially named constructor arguments in "
                               ++ pprint info ++ "\n"
    x -> return x

-- | Flag sum types as failing if they have any constructors with names.
possibleNamedSum
  :: Set.Set Name
  -> Type
  -> Info
  -> [Con]
  -> Q (Naming PortName)
possibleNamedSum seen split info xs =
  (fold <$> mapM (namesFromConstructor seen split info) xs) >>= \case
    Complete [] -> return $ Complete []
    x -> return $ x <> HasFail ("Sum types not supported!"
                               ++ "\n at " ++ pprint info ++ "\n")

-- | Build a list of 'PortName's from a template haskell 'Name'
nameToPorts
  :: Set.Set Name
  -- ^ Locally track seen names to handle recursive datatypes
  -> Type
  -- ^ Type to split at
  -> Name
  -- ^ Name to investigate
  -> Q (Naming PortName)
nameToPorts seen split name = do
  if Set.member name seen
  then return $ Complete []
  else do
    let seen' = Set.insert name seen
    -- Recur on types by reifying the name.
    -- This allows us to find splits defined in data types.
    info <- reify name

    case info of
      TyConI (getConstructors -> [x]) ->
        namesFromConstructor seen' split info x
      TyConI (getConstructors -> xs)  ->
        possibleNamedSum seen split info xs
      DataConI _ (AppT (AppT ArrowT ty) _) _ ->
        expandFamiliesAndGatherNames seen split ty
      PrimTyConI _ _ _ -> return $ Complete []
      _ -> return $ HasFail $ "Unhandled " ++ pprint info ++ "\n"
 where
   getConstructors (DataD _ _ _ _ x _)        = x
   getConstructors (NewtypeD _ _ _ _ x _)     = [x]
   getConstructors (DataInstD _ _ _ _ x _)    = x
   getConstructors (NewtypeInstD _ _ _ _ x _) = [x]
   getConstructors _                          = []

-- | A helper function for recursively walking a 'Type' tree and
-- building a list of 'PortName's.
gatherNames
  :: Set.Set Name
  -- ^ Locally track seen names to handle recursive datatypes
  -> Type
  -- ^ Type to split at
  -> TypeF (Type, Q (Naming PortName))
  -- ^ Case under scrutiny, paramorphism style
  -> Q (Naming PortName)
gatherNames _ split (AppTF (AppT split' (LitT (StrTyLit name)), _) (_,c))
  -- Is there a '<String> ::: <something>' annotation?
  | split' == split
  -- We found our split. If:
  -- - We only have no names from children: use split name as PortName
  -- - We have children reporting names: use split name as name to PortProduct
  = c >>= \case
    Complete [] -> return $ Complete [PortName name]
    Complete xs -> return $ Complete [PortProduct name xs]
    HasFail err -> return $ HasFail err
gatherNames seen split (ConTF name) = nameToPorts seen split name
gatherNames _    _     f            = do
  -- Just collect names
  f' <- mapM snd f
  return $ fold f'

-- | Helper algebra to expand first level (non-recursive) type/data family
-- instances in a best effort manner. Data family instances with sum type
-- constructors are ignored.
expandFamilies :: TypeF (Q Type) -> Q (Type)
expandFamilies (AppTF a b) = do
  a' <- a
  b' <- b
  case unApp (AppT a' b') [] of
    (ConT x : xs) -> do
      info <- reify x
      case info of
        -- Closed type families have to be handled separately
        FamilyI (ClosedTypeFamilyD (TypeFamilyHead _ bds _ _) eqs) _ ->
          if length bds == length xs then
            case find ((==) xs . tySynArgs) eqs of
#if MIN_VERSION_template_haskell(2,15,0)
              Just (TySynEqn _ _ r) -> return r
#else
              Just (TySynEqn _ r) -> return r
#endif
              _ -> return (AppT a' b')
                -- ^ We didn't find a matching instance so give up.
          else return (AppT a' b')
                -- ^ We don't yet have all the arguments.
        -- Now for open type families and data families
        _ ->
          case familyArity info of
            Just i | i == length xs -> do
              inst <- reifyInstances x xs
              case inst of
#if MIN_VERSION_template_haskell(2,15,0)
                [TySynInstD (TySynEqn _ _ t)] -> return t
#else
                [TySynInstD _ (TySynEqn _ t)] -> return t
#endif
                [NewtypeInstD _ _ _ _ c _] -> return $ ConT (getName c)
                [DataInstD    _ _ _ _ cs _] -> do
                  case cs of
                    [c] -> return $ ConT (getName c)
                    _ -> return $ PromotedTupleT 0
                      -- ^ Ignore sum type in a data family by replacing with
                      -- empty tuple. We don't want to fail because this subtree
                      -- might not be relevant to naming.
                z -> fail
                   $ "Encountered\n"
                   ++ pprint z
                   ++ "\nduring family application! Perhaps a missing instance?"
            _ -> return (AppT a' b')
    _ -> return (AppT a' b')
 where
  unApp (AppT l r) = unApp l . (r :)
  unApp t = (t :)

#if MIN_VERSION_template_haskell(2,15,0)
  tySynArgs (TySynEqn _ args _) = tail (unApp args [])
#else
  tySynArgs (TySynEqn args _) = args
#endif

  familyArity (FamilyI (OpenTypeFamilyD (TypeFamilyHead _ xs _ _)) _) =
    Just (length xs)
  familyArity (FamilyI (DataFamilyD _ xs _) _) = Just (length xs)
  familyArity _ = Nothing
expandFamilies t = embed <$> sequence t

-- | Runs 'expandFamilies' and then 'gatherNames'
expandFamiliesAndGatherNames
  :: Set.Set Name
  -- ^ Locally track seen names to handle recursive datatypes
  -> Type
  -- ^ Type to split at
  -> Type
  -- ^ Type to investigate
  -> Q (Naming PortName)
expandFamiliesAndGatherNames seen split x =
  cata expandFamilies x >>= para (gatherNames seen split)

-- | Build a possible failing 'PortName' tree and unwrap the 'Naming' result.
buildPorts
  :: Type
  -- ^ Type to split at
  -> Type
  -- ^ Type to investigate
  -> Q [PortName]
buildPorts split x = do
  expandFamiliesAndGatherNames Set.empty split x >>= \case
    Complete xs -> return xs
    HasFail err -> fail err

-- | Matches a type `a -> b`
pattern ArrowTy :: Type -> Type -> Type
pattern ArrowTy a b = AppT (AppT ArrowT a) b

-- | Get the result 'PortName' from a function type
toReturnName :: Type -> Type -> Q PortName
toReturnName split (ArrowTy _ b) = toReturnName split b
toReturnName split b             =
  buildPorts split b
  >>= \case
     [] -> fail "No return name specified!"
     [x] -> return x
     xs -> return $ PortProduct "" xs

-- | Get the argument 'PortName's from a function type
toArgNames :: Type -> Type -> Q [PortName]
toArgNames split ty = go (0::Int) ty []
  where
    go i (ArrowTy x y) acc = do
        v    <- go (i+1) y acc
        name <- buildPorts split x
        -- Fail if we didn't get a single PortName
        case name of
          [a] -> return (a:v)
          f   -> fail $ "Failed to get a single name for argument " ++ show i
                      ++ "\n a.k.a. " ++ pprint x
                      ++ "\n got name " ++ show f
    go _ _ acc = return acc

-- | Return a typed expression for a 'TopEntity' of a given @('Name', 'Type')@.
buildTopEntity' :: Maybe String -> (Name, Type) -> TExpQ TopEntity
buildTopEntity' topName (name, ty) = do
    -- get a Name for this type operator so we can check it
    -- in the ArrowTy case
    split <- [t| (:::) |]
    ins   <- toArgNames split ty
    out   <- toReturnName split ty

    let outName = case topName of
          Just name' -> name'          -- user specified name
          Nothing    -> nameBase name  -- auto-generated from haskell name

    [|| Synthesize
        { t_name   = outName
        , t_inputs = ins
        , t_output = out
        } ||]

-- | Return an untyped expression for a 'TopEntity' of a given 'Name'.
buildTopEntity :: Maybe String -> Name -> ExpQ
buildTopEntity topName name =
  fmap unType $ getNameBinding name >>= buildTopEntity' topName

-- | Turn the 'Name' of a value to a @('Name', 'Type')@
getNameBinding :: Name -> Q (Name, Type)
getNameBinding n = reify n >>= \case
  VarI name ty _ -> return (name, ty)
  _ -> fail "getNameBinding: Invalid Name, must be a top-level binding!"

-- | Wrap a 'TopEntity' expression in an annotation pragma
makeTopEntityWithName' :: Name -> Maybe String -> DecQ
makeTopEntityWithName' n topName = do
  (name,ty) <- getNameBinding n
  topEntity <- buildTopEntity' topName (name,ty)
  let prag t = PragmaD (AnnP (valueAnnotation name) t)
  return $ prag $ unType topEntity

-- | Automatically create a @'TopEntity'@ for a given @'Name'@, using the given
-- @'String'@ to specify the name of the generated RTL entity.
--
-- The function arguments and return values of the function specified by the
-- given @'Name'@ must be annotated with @'(:::)'@. This annotation provides the
-- given name of the port.
makeTopEntityWithName :: Name -> String -> DecsQ
makeTopEntityWithName nam top = pure <$> makeTopEntityWithName' nam (Just top)

-- | Automatically create a @'TopEntity'@ for a given @'Name'@. The name of the
-- generated RTL entity will be the name of the function that has been
-- specified; e.g. @'makeTopEntity' 'foobar@ will generate a @foobar@ module.
--
-- The function arguments and return values of the function specified by the
-- given @'Name'@ must be annotated with @'(:::)'@. This annotation provides the
-- given name of the port.
makeTopEntity :: Name -> DecsQ
makeTopEntity nam = pure <$> makeTopEntityWithName' nam Nothing
