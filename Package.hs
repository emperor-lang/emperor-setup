{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Package where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, Value(Object), (.:), (.=), object, parseJSON, toJSON)
import Data.Map (Map)

data Package =
    Package { name :: String, author :: Author, dependencies :: Map String String, version :: String, files :: [String], license :: String}
    deriving (Generic, Show)

data Author =
    Author String String String
    deriving (Show)

instance ToJSON Author where
    toJSON (Author n e u) = object ["name" .= n, "email" .= e, "url" .= u]

instance FromJSON Author where
    parseJSON (Object v) = Author <$> v .: "name" <*> v .: "email" <*> v .: "url"
    parseJSON _ = fail "Expected object when parsing author"

instance ToJSON Package
instance FromJSON Package

authorName :: Author -> String
authorName (Author n _ _) = n

authorEmail :: Author -> String
authorEmail (Author _ e _) = e

authorURL :: Author -> String
authorURL (Author _ _ u) = u