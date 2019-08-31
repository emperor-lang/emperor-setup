module Locations (getBinLoc, getIncludeInstallLoc, getPackageInstallLoc) where

import           System.Directory (getHomeDirectory)

getHomeLoc :: String -> IO String
getHomeLoc f = do
    h <- getHomeDirectory
    return $ h ++ "/" ++ f

getBinLoc :: IO String
getBinLoc = return "/usr/bin/"

getIncludeInstallLoc :: IO String
getIncludeInstallLoc = getHomeLoc ".emperor/include/"

getPackageInstallLoc :: IO String
getPackageInstallLoc = getHomeLoc ".emperor/packages/"
