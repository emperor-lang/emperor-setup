module Locations (getBinLoc, getLibLoc, getIncludeLoc, getDataLoc) where

import           System.Directory (getHomeDirectory)

getBinLoc :: IO String
getBinLoc = do
    h <- getHomeDirectory
    return $ h ++ "/usr/bin/"

getLibLoc :: IO String
getLibLoc = do
    h <- getHomeDirectory
    return $ h ++ "/.emperor/packages/"-- "/usr/lib/emperor/"

getIncludeLoc :: IO String
getIncludeLoc = do
    h <- getHomeDirectory
    return $ h ++ "/.emperor/packages/"-- "/usr/include/emperor/"

getDataLoc :: IO String
getDataLoc = do
    h <- getHomeDirectory
    return $ h ++ "/.emperor/packages/"-- "/usr/share/emperor/"
