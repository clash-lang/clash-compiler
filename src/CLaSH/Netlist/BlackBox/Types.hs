{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
module CLaSH.Netlist.BlackBox.Types where

import Control.Monad.State
import Data.Data
import Data.ByteString
import Data.Hashable
import Data.HashMap.Lazy
import qualified Data.ByteString.Lazy as BSL

import Text.Hastache.Context

import CLaSH.Netlist.Types
import CLaSH.Primitives.Types
import CLaSH.Util

data BlackBoxContext
  = Context
  { result    :: Identifier
  , inputs    :: [Identifier]
  , litInputs :: [Identifier]
  , funInputs :: [Identifier]
  , start     :: ByteString -> BlackBoxMonad ByteString
  , addI      :: ByteString -> BlackBoxMonad ByteString
  , addO      :: ByteString -> BlackBoxMonad ByteString
  , render    :: ByteString -> BlackBoxMonad ByteString
  } deriving (Data,Typeable)

instance Show BlackBoxContext where
  show (Context res inps litInps funImps _ _ _ _) =
    "Context :: Result :" ++ show res ++
    ", Inputs: " ++ show inps ++
    ", Literals: " ++ show litInps ++
    ", Functions: " ++ show funImps

data BBState
  = BBState
  { _prims         :: HashMap BSL.ByteString Primitive
  , _topLevelCtx   :: BlackBoxContext
  , _renderContext :: Maybe (Primitive, BlackBoxContext)
  } deriving (Data,Typeable)

type BlackBoxMonad a = StateT BBState IO a

instance (Typeable st, Typeable1 m) => Typeable1 (StateT st m) where
  typeOf1 x =
    mkTyConApp
      (mkTyCon3 "transformers" "Control.Monad.Trans.State.Lazy" "StateT")
      [ typeOf  (st x)
      , typeOf1 (m x)
      ]
    where
      m :: StateT st m a -> m a
      m = undefined
      st :: StateT st m a -> st
      st = undefined

instance (Typeable a, Typeable st, Typeable1 m) => Data (StateT st m a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "Control.Monad.Trans.State.Lazy.StateT"


instance (Data k, Data v, Eq k, Hashable k) => Data (HashMap k v) where
    gfoldl f z m   = z fromList `f` toList m
    toConstr _     = fromListConstr
    gunfold k z c  = case constrIndex c of
        1 -> k (z fromList)
        _ -> error "gunfold"
    dataTypeOf _   = hashMapDataType
    dataCast2 f    = gcast2 f

fromListConstr :: Constr
fromListConstr = mkConstr hashMapDataType "fromList" [] Prefix

hashMapDataType :: DataType
hashMapDataType = mkDataType "Data.HashMap.Base.HashMap" [fromListConstr]

mkLabels [''BBState]
