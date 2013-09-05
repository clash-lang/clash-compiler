{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Type and instance definitions for Primitive
module CLaSH.Primitives.Types where

import           Control.Applicative  ((<$>), (<*>), (<|>))
import           Data.Aeson           (FromJSON (..), Value (..), (.:))
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Lazy    (HashMap)
import qualified Data.HashMap.Strict  as H
import           Data.Text.Lazy       (Text)

-- | Primitive Definitions
type PrimMap = HashMap ByteString Primitive

-- | Externally defined primitive
data Primitive
  -- | A primitive that has a template that can be filled out by the backend render
  = BlackBox
  { name     :: ByteString -- ^ Name of the primitive
  , template :: Either Text Text -- ^ Either a /declaration/ or an /expression/ template.
  }
  -- | A primitive that carries additional information
  | Primitive
  { name     :: ByteString -- ^ Name of the primitive
  , primType :: Text -- ^ Additional information
  }

instance FromJSON Primitive where
  parseJSON (Object v) = case H.toList v of
    [(conKey,Object conVal)] -> case conKey of
      "BlackBox"  -> BlackBox <$> conVal .: "name" <*> ((Left <$> conVal .: "templateD") <|> (Right <$> conVal .: "templateE"))
      "Primitive" -> Primitive <$> conVal .: "name" <*> conVal .: "primType"
      _ -> error "Expected: BlackBox or Primitive object"
    _ -> error "Expected: BlackBox or Primitive object"
  parseJSON _ = error "Expected: BlackBox or Primitive object"
