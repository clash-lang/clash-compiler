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
  )
where

import           Clash.Annotations.TopEntity (PortName(..), TopEntity(..))
import           Clash.NamedTypes

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import Data.Functor.Foldable (para)
import Data.Functor.Foldable.TH
import Data.Foldable (fold)

$(makeBaseFunctor ''Type)

-- A helper function for recursively walking a 'Type' tree and
-- building a list of 'PortName's
buildPorts :: Type -> TypeF (Type, Q [PortName]) -> Q [PortName]
-- Is there a ' <String> ::: <something>' annotation?
buildPorts split (AppTF (AppT split' (LitT (StrTyLit name)), _) (_,c))
  | split' == split
  -- We found our splitter. If:
  -- - We only have no names from children: use split name as PortName
  -- - We have children reporting names: use split name as name to PortProduct
  -- - !! Inconsistent children names currently unhandled
  = c >>= \case
    [] -> return [PortName name]
    xs -> return [PortProduct name xs]
-- Recur on types by reifying the name.
-- This allows us to find splitters defined in data types.
buildPorts split (ConTF name) = do
  info <- reify name
  case info of
    TyConI (DataD _ _ _ _ [RecC _ xs] _) ->
      buildFromConstructor $ map (\(_,_,c)->c) xs
    TyConI (DataD _ _ _ _ [NormalC _ xs] _) ->
      buildFromConstructor $ map snd xs
    _ -> return []
  where buildFromConstructor =
          fmap mconcat . sequence . fmap (para (buildPorts split))
-- Just collect names
buildPorts _ f = do
  f' <- mapM snd f
  return $ fold f'

-- matches a type `a -> b`
pattern ArrowTy :: Type -> Type -> Type
pattern ArrowTy a b = AppT (AppT ArrowT a) b

-- | Given an multi-arity function type @f :: a -> b -> c -> ...@, get
-- the final return type.
getReturnTy :: Type -> Q Type
getReturnTy (ArrowTy _ b) = getReturnTy b
getReturnTy b             = return b

makeTopEntityWithName' :: Name -> Maybe String -> DecsQ
makeTopEntityWithName' n topName = reify n >>= \case
  VarI nam typ _ -> do
    -- helpers
    let prag t = PragmaD (AnnP (valueAnnotation nam) t)

    -- get a Name for this type operator so we can check it
    -- in the ArrowTy case
    nty <- [t| (:::) |]

    -- examine the arguments
    let examine ty = go 0 ty [] where
          go i (ArrowTy x y) xs
            = do
              v <- go (i+1) y xs
              name <- para (buildPorts nty) x
              -- Fail if we didn't get a single PortName
              case name of
                (a:[]) -> return (a:v)
                f -> fail $ "Failed to get a single name for argument " ++ show i
                            ++ "\n a.k.a. " ++ show x
                            ++ "\n got name " ++ show f

          go _ _ xs = return xs

    ins <- examine typ
    out <- getReturnTy typ
           >>= para (buildPorts nty)
           >>= \case
              [] -> fail "No return name specified!"
              [x] -> return x
              xs -> return $ PortProduct "" xs

    -- Return the annotation
    let realName = case topName of
          Just nn -> nn             -- user specified name
          Nothing -> nameBase nam   -- auto-generated name
    top <- lift $ Synthesize
                    { t_name   = realName
                    , t_inputs = ins
                    , t_output = out
                    }
    return [prag top]

  -- failure case: we weren't provided with the name of a binder
  _ -> fail "makeTopEntity: invalid Name, must be a top-level binding!"

-- | Automatically create a @'TopEntity'@ for a given @'Name'@, using the given
-- @'String'@ to specify the name of the generated RTL entity.
--
-- The function arguments and return values of the function specified by the
-- given @'Name'@ must be annotated with @'(:::)'@. This annotation provides the
-- given name of the port.
makeTopEntityWithName :: Name -> String -> DecsQ
makeTopEntityWithName nam top = makeTopEntityWithName' nam (Just top)

-- | Automatically create a @'TopEntity'@ for a given @'Name'@. The name of the
-- generated RTL entity will be the name of the function that has been
-- specified; e.g. @'makeTopEntity' 'foobar@ will generate a @foobar@ module.
--
-- The function arguments and return values of the function specified by the
-- given @'Name'@ must be annotated with @'(:::)'@. This annotation provides the
-- given name of the port.
makeTopEntity :: Name -> DecsQ
makeTopEntity nam = makeTopEntityWithName' nam Nothing
