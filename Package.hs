{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-|
Module      : Package
Description : Data-types to represent emperor packages
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

Defines data types to represent and manipulate emperor packages. Defines
functions to translate packages in to JSON and back.
-}
module Package (Package(..), Author(..), Dependency(..), HasName, HasVersion, hasDependency, insertDependency, parseDependencyString, version, name, getPackage, getPackageFromDirectory, getPackageMeta) where

import           Args                 (Args, input)
import           Data.Aeson           (FromJSON, ToJSON, Value(Object), eitherDecode, object, parseJSON, toJSON, (.:),
                                       (.=))
import           Data.ByteString.Lazy (ByteString, getContents, readFile)
import           Prelude              hiding (getContents, readFile)
import           System.Directory     (doesFileExist)
import           System.Exit          (exitFailure)
import           System.IO            (hPutStrLn, stderr)

-- | Defines an emperor package
data Package =
    Package
        { packageName :: String
        , author :: Author
        , dependencies :: [Dependency]
        , packageVersion :: String
        , files :: [String]
        , license :: String
        }
    deriving (Show)

-- | An author of an emperor package
data Author =
    Author
        { authorName :: String
        , email :: String
        , url :: String
        }
    deriving (Show)

-- | A dependency of a given package
data Dependency =
    Dependency
        { dependencyName :: String
        , dependencyVersion :: String
        }

-- | Defines data items which have a version
class HasVersion a t | a -> t where
    -- | Obtain the version of a specified data item
    version :: a -> t

instance HasVersion Package String where
    version = packageVersion

instance HasVersion Dependency String where
    version = dependencyVersion

-- | Defines data items which have a name
class HasName a t | a -> t where
    -- | Obtain the name of a specified data item
    name :: a -> t

instance HasName Package String where
    name = packageName

instance HasName Author String where
    name = authorName

instance HasName Dependency String where
    name = dependencyName

instance Show Dependency where
    show d = name d ++ ':' : version d

instance ToJSON Package where
    toJSON (Package n a d v f l) = object [ "name" .= n, "author" .= a, "dependencies" .= d, "version" .= v, "files" .= f, "license" .= l ]

instance FromJSON Package where
    parseJSON (Object v) = Package <$> v .: "name" <*> v .: "author" <*> v .: "dependencies" <*> v .: "version" <*> v .: "files" <*> v .: "license"
    parseJSON _ = fail "Expected object when parsing author"

instance ToJSON Author where
    toJSON (Author n e u) = object ["name" .= n, "email" .= e, "url" .= u]

instance FromJSON Author where
    parseJSON (Object v) = Author <$> v .: "name" <*> v .: "email" <*> v .: "url"
    parseJSON _ = fail "Expected object when parsing author"

instance ToJSON Dependency where
    toJSON (Dependency n v) = object ["name" .= n, "version" .= v]

instance FromJSON Dependency where
    parseJSON (Object v) = Dependency <$> v .: "name" <*> v .: "version"
    parseJSON _ = fail "Expected object when parsing dependency"

-- | Returns whether a given package depends on some version of a given dependency
hasDependency :: Package -> Dependency -> Bool
hasDependency p d = (not . null) $ filter (== name d) $ name <$> dependencies p

-- | Adds a dependency to the list associated with a package
insertDependency :: Package -> Dependency -> Package
insertDependency p d = p { dependencies = d : dependencies p}

-- | Parse a dependency string in to a package name and maybe a version
parseDependencyString :: String -> (String, Maybe String)
parseDependencyString [] = ([], Nothing)
parseDependencyString (s:ss)
    | s == ':' = ([], Just ss)
    | otherwise = (s:p, v)
        where
            (p,v) = parseDependencyString ss

-- | Obtain the package located at a given directory
getPackageFromDirectory :: FilePath -> IO (Maybe Package)
getPackageFromDirectory d = getPackage $ d ++ (if last d == '/' then "" else "/") ++ "manifest.json"

-- | Get the package specified by a given manifest
getPackage :: FilePath -> IO (Maybe Package)
getPackage f = do
        r <- doesFileExist f
        if r then do
            c <- readFile f
            getPackageMeta' c
        else
            return Nothing

-- | Obtain the package given by the command-line arguments
getPackageMeta :: Args -> IO (Maybe Package)
getPackageMeta args =
    if (not . null) (input args) then
        if input args == "-" then do
            c <- getContents
            getPackageMeta' c
        else
            getPackage $ input args
    else
        getPackage "./manifest.json"

-- | Parse a byte-string in to a package
getPackageMeta' :: ByteString -> IO (Maybe Package)
getPackageMeta' c = case eitherDecode c of
    Left m -> do
        hPutStrLn stderr $ "Failed to parse json from input: " ++ m
        exitFailure
    Right p -> return $ Just p
