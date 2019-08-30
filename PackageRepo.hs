{-# LANGUAGE DeriveGeneric #-}

module PackageRepo (getMostRecentVersion, getPackageLocation) where

import           Data.Aeson           (FromJSON, eitherDecode)
import           Data.ByteString.Lazy (readFile)
import           GHC.Generics         (Generic)
import           Prelude              hiding (readFile)
import           System.Directory     (doesFileExist)

newtype PackageRepo = PackageRepo [PackageRepoRecord]
    deriving (Generic, Show)

packages :: PackageRepo -> [PackageRepoRecord]
packages (PackageRepo ps) = ps

data PackageRepoRecord =
    PackageRepoRecord
        { package :: String
        , version :: String
        , location :: String
        }
    deriving (Eq, Generic, Show)

instance FromJSON PackageRepo

instance FromJSON PackageRepoRecord

instance Ord PackageRepoRecord where
    p <= p' = version p <= version p'

packageRepoDefaultLocation :: String
packageRepoDefaultLocation = "/usr/share/emperor/packages.json"

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

getPackages :: IO (Either String PackageRepo)
getPackages = do
    lr <- doesFileExist packageRepoDefaultLocation
    if lr then do
        c <- readFile packageRepoDefaultLocation
        return $ eitherDecode c
    else
        return . Left $ "Could not find package repository at expected location, " ++ packageRepoDefaultLocation
