{-|

This module can automatically generate TopEntity definitions from 'Clash.NamedTypes'
annotations.

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

module Clash.Annotations.TopEntityGen
  ( makeTopEntity
  , makeTopEntityWithName
  , makeTopEntityWithName'
  , buildTopEntity
  , buildTopEntity'
  )
where

import           Clash.Annotations.TopEntity (PortName(..), TopEntity(..))
import           Clash.NamedTypes

import           Language.Haskell.TH
-- import           Language.Haskell.TH.Syntax

import Data.Functor.Foldable (para)
import Data.Functor.Foldable.TH
import Data.Foldable (fold)

$(makeBaseFunctor ''Type)

-- A helper function for recursively walking a 'Type' tree and
-- building a list of 'PortName's
buildPorts :: Type                                        -- Type to split at
           -> TypeF (Type, Q [PortName]) -> Q [PortName]  -- Fold step
buildPorts split (AppTF (AppT split' (LitT (StrTyLit name)), _) (_,c))
  -- Is there a '<String> ::: <something>' annotation?
  | split' == split
  -- We found our split. If:
  -- - We only have no names from children: use split name as PortName
  -- - We have children reporting names: use split name as name to PortProduct
  -- - !! Inconsistent children names currently unhandled
  = c >>= \case
    [] -> return [PortName name]
    xs -> return [PortProduct name xs]
buildPorts split (ConTF name) = do
  -- Recur on types by reifying the name.
  -- This allows us to find splits defined in data types.
  info <- reify name
  case info of
    TyConI (DataD _ _ _ _ [RecC _ xs] _) ->
      buildFromConstructor $ map (\(_,_,c)->c) xs
    TyConI (DataD _ _ _ _ [NormalC _ xs] _) ->
      buildFromConstructor $ map snd xs
    _ -> return []
  where buildFromConstructor =
          fmap mconcat . mapM (para (buildPorts split))
buildPorts _ f = do
  -- Just collect names
  f' <- mapM snd f
  return $ fold f'

-- matches a type `a -> b`
pattern ArrowTy :: Type -> Type -> Type
pattern ArrowTy a b = AppT (AppT ArrowT a) b

-- Get the return 'PortName' from a splitter and function type
toReturnName :: Type -> Type -> Q PortName
toReturnName split (ArrowTy _ b) = toReturnName split b
toReturnName split b             =
  para (buildPorts split) b
  >>= \case
     [] -> fail "No return name specified!"
     [x] -> return x
     xs -> return $ PortProduct "" xs

-- Get the argument 'PortName's from a splitter and function type
toArgNames :: Type -> Type -> Q [PortName]
toArgNames split ty = go (0::Int) ty []
  where
    go i (ArrowTy x y) acc = do
        v    <- go (i+1) y acc
        name <- para (buildPorts split) x
        -- Fail if we didn't get a single PortName
        case name of
          [a] -> return (a:v)
          f   -> fail $ "Failed to get a single name for argument " ++ show i
                      ++ "\n a.k.a. " ++ show x
                      ++ "\n got name " ++ show f
    go _ _ acc = return acc

buildTopEntity' :: Maybe String -> (Name, Type) -> TExpQ TopEntity
buildTopEntity' topName (name, ty) = do
    -- get a Name for this type operator so we can check it
    -- in the ArrowTy case
    split <- [t| (:::) |]
    ins <- toArgNames split ty
    out <- toReturnName split ty

    let outName = case topName of
          Just name' -> name'          -- user specified name
          Nothing    -> nameBase name  -- auto-generated from haskell name

    [|| Synthesize
        { t_name   = outName
        , t_inputs = ins
        , t_output = out
        } ||]

buildTopEntity :: Maybe String -> Name -> ExpQ
buildTopEntity topName name =
  fmap unType $ getNameBinding name >>= buildTopEntity' topName

getNameBinding :: Name -> Q (Name, Type)
getNameBinding n = reify n >>= \case
  VarI name ty _ -> return (name, ty)
  _ -> fail "getNameBinding: Invalid Name, must be a top-level binding!"

-- Wrap a 'TopEntity' expression in an annotation pragma
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
