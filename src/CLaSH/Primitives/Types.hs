{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module CLaSH.Primitives.Types where

import           Control.Applicative  ((<$>), (<*>), (<|>))
import           Data.Aeson           (FromJSON (..), Value (..), (.:))
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Lazy    (HashMap)
import qualified Data.HashMap.Strict  as H
import           Data.Text.Lazy       (Text)

type PrimMap = HashMap ByteString Primitive

data Primitive
  = BlackBox
  { name     :: ByteString
  , template :: Either Text Text
  }
  | Primitive
  { name     :: ByteString
  , primType :: Text
  }

instance FromJSON Primitive where
  parseJSON (Object v) = case H.toList v of
    [(conKey,Object conVal)] -> case conKey of
      "BlackBox"  -> BlackBox <$> conVal .: "name" <*> ((Left <$> conVal .: "templateD") <|> (Right <$> conVal .: "templateE"))
      "Primitive" -> Primitive <$> conVal .: "name" <*> conVal .: "primType"
      _ -> error "Expected: BlackBox or Primitive object"
    _ -> error "Expected: BlackBox or Primitive object"
  parseJSON _ = error "Expected: BlackBox or Primitive object"
