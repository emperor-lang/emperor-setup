{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : PackageRepo
Description : Describes the known package repository on disk
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

Provides functions to define and operate upon the known package list
-}
module PackageRepo (getMostRecentVersion, getPackageLocation, packageRepoDefaultLocation) where

import           Data.Aeson           (FromJSON, eitherDecode)
import           Data.ByteString.Lazy (readFile)
import           GHC.Generics         (Generic)
import           Prelude              hiding (readFile)
import           System.Directory     (doesFileExist)

-- | Defines the package repository
newtype PackageRepo = PackageRepo [PackageRepoRecord]
    deriving (Generic, Show)

-- | Returns a list of packages present in a given repo
packages :: PackageRepo -> [PackageRepoRecord]
packages (PackageRepo ps) = ps

-- | Describes a single item in the package repository
data PackageRepoRecord =
    PackageRepoRecord
        { package :: String
        , version :: String
        , location :: String
        }
    deriving (Eq, Generic, Show)

instance FromJSON PackageRepo

instance FromJSON PackageRepoRecord

-- | Package repo records are ordered by their versions
instance Ord PackageRepoRecord where
    p <= p' = version p <= version p'

-- | Return the location where the package repository is expected to be found
packageRepoDefaultLocation :: String
packageRepoDefaultLocation = "/usr/share/emperor/packages.json"

-- | Get the location where a given package at a specified version should be found on disk
getPackageLocation :: String -> String -> IO (Either String String)
getPackageLocation p v = do
    pr <- getPackages
    case pr of
        Right ps -> do
            let r = filter (\p' -> package p' == p && version p' == v) $ packages ps
            if null r then
                return . Left $ "Could not find package " ++ p ++ ":" ++ v
            else if length r >= 2 then
                return . Left $ "There are multiple records corresponding to " ++ p ++ ":" ++ v
            else case r of
                [p'] -> return $ (Right . location) p'
                _ -> error "Managed to get a non-unique number of packages after checking cases?"
        Left m -> return $ Left m

-- | Return the mst up-to-date known version of a specified package
getMostRecentVersion :: String -> IO (Either String String)
getMostRecentVersion p = do
    pr <- getPackages
    case pr of
        Right ps -> do
            let vs = filter (\p' -> package p' == p) $ packages ps
            if null vs then
                return . Left $ "Package not present in the repo: " ++ p
            else
                return . Right $ (version . maximum) vs
        Left m -> return $ Left m

-- | Get the package repo
getPackages :: IO (Either String PackageRepo)
getPackages = do
    lr <- doesFileExist packageRepoDefaultLocation
    if lr then do
        c <- readFile packageRepoDefaultLocation
        return $ eitherDecode c
    else
        return . Left $ "Could not find package repository at expected location, " ++ packageRepoDefaultLocation
