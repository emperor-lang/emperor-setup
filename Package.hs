{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Package (Package(..), Author(..), Dependency(..), hasDependency, insertDependency, parseDependencyString) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import Control.Lens ()

data Package =
    Package
        { _packageName :: String
        , _packageAuthor :: Author
        , _packageDependencies :: [Dependency]
        , _packageVersion :: String
        , _packageFiles :: [String]
        , _packageLicense :: String
        }
    deriving (Generic, Show)
makeFields ''Package

data Author =
    Author
        { _authorName :: String
        , _authorEmail :: String
        , _authorUrl :: String
        }
    deriving (Generic, Show)
makeFields ''Author

data Dependency =
    Dependency
        { _dependencyPackage :: String
        , _dependencyVersion :: String
        }
    deriving (Generic)
makeFields ''Dependency

instance Show Dependency where
    show d = package d ++ ':' : packageVersion d

instance ToJSON Package

instance FromJSON Package

instance ToJSON Author

instance FromJSON Author

instance ToJSON Dependency

instance FromJSON Dependency

hasDependency :: Package -> Dependency -> Bool
hasDependency p d = let n = package d in 
    (not . null) $ filter (== n) $ package <$> dependencies p

insertDependency :: Package -> Dependency -> Package
insertDependency p d = p { dependencies = d : dependencies p}

parseDependencyString :: String -> (String, Maybe String)
parseDependencyString [] = ([], Nothing)
parseDependencyString (s:ss)
    | s == ':' = ([], Just ss)
    | otherwise = (s:p, v)
        where
            (p,v) = parseDependencyString ss
