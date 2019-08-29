{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Package where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import GHC.Generics (Generic)

data Package =
    Package
        { name :: String
        , author :: Author
        , dependencies :: Map String String
        , version :: String
        , files :: [String]
        , license :: String
        }
    deriving (Generic, Show)

data Author = 
    Author
        { authorName :: String
        , email :: String
        , url :: String
        }
    deriving (Generic, Show)

instance ToJSON Package

instance FromJSON Package

instance ToJSON Author

instance FromJSON Author
