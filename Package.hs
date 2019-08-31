{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Package (Package(..), Author(..), Dependency(..), HasName, HasVersion, hasDependency, insertDependency, parseDependencyString, version, name, getPackage, getPackageFromDirectory, getPackageMeta) where

import           Args                 (Args, input)
import           Data.Aeson           (FromJSON, ToJSON, Value(Object), eitherDecode, object, parseJSON, toJSON, (.:),
                                       (.=))
import           Data.ByteString.Lazy (ByteString, getContents, readFile)
import           Prelude              hiding (getContents, readFile)
import           System.Directory     (doesFileExist)
import           System.Exit          (exitFailure)
import           System.IO            (hPutStrLn, stderr)


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

data Author =
    Author
        { authorName :: String
        , email :: String
        , url :: String
        }
    deriving (Show)

data Dependency =
    Dependency
        { dependencyName :: String
        , dependencyVersion :: String
        }

class HasVersion a t | a -> t where
    version :: a -> t

instance HasVersion Package String where
    version = packageVersion

instance HasVersion Dependency String where
    version = dependencyVersion

class HasName a t | a -> t where
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

hasDependency :: Package -> Dependency -> Bool
hasDependency p d = (not . null) $ filter (== name d) $ name <$> dependencies p

insertDependency :: Package -> Dependency -> Package
insertDependency p d = p { dependencies = d : dependencies p}

parseDependencyString :: String -> (String, Maybe String)
parseDependencyString [] = ([], Nothing)
parseDependencyString (s:ss)
    | s == ':' = ([], Just ss)
    | otherwise = (s:p, v)
        where
            (p,v) = parseDependencyString ss

getPackageFromDirectory :: FilePath -> IO (Maybe Package)
getPackageFromDirectory d = getPackage $ d ++ if last d == '/' then "package.json" else "/package.json"

getPackage :: FilePath -> IO (Maybe Package)
getPackage f = do
        r <- doesFileExist f
        if r then do
            c <- readFile f
            getPackageMeta' c
        else
            return Nothing

getPackageMeta :: Args -> IO (Maybe Package)
getPackageMeta args =
    if (not . null) (input args) then
        if input args == "-" then do
            c <- getContents
            getPackageMeta' c
        else
            getPackage $ input args
    else do
        getPackage "./package.json"

getPackageMeta' :: ByteString -> IO (Maybe Package)
getPackageMeta' c = case eitherDecode c of
    Left m -> do
        hPutStrLn stderr $ "Failed to parse json from input: " ++ m
        exitFailure
    Right p -> return $ Just p
