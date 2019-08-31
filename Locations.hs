{-|
Module      : Locations
Description : Defines significant locations for installed emperor packages
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This defines locations where binaries, libraries, data and headers may be located
for installed packages.
-}
module Locations (getBinLoc, getIncludeInstallLoc, getPackageInstallLoc) where

import           System.Directory (getHomeDirectory)

-- | Prepend the user's home directory to the start of a given string
getHomeLoc :: String -> IO String
getHomeLoc f = do
    h <- getHomeDirectory
    return $ h ++ "/" ++ f

-- | Obtain the location where binaries are found
getBinLoc :: IO String
getBinLoc = return "/usr/bin/"

-- | Obtain the location where language headers are found
getIncludeInstallLoc :: IO String
getIncludeInstallLoc = getHomeLoc ".emperor/include/"

-- | Obtain the location where packages are installed
getPackageInstallLoc :: IO String
getPackageInstallLoc = getHomeLoc ".emperor/packages/"
