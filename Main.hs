module Main where

import Args
    ( Args
    , addDependency
    , binaryInstallLocation
    , cFlags
    , dataInstallLocation
    , includeLocation
    , input
    , installDependencies
    , libraryInstallLocation
    , libs
    , parseArgv
    )
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (readFile) -- , writeFile)
import Data.List (intercalate)
import Data.Map (assocs)
import Package (Package, dependencies)
import Prelude hiding (readFile, writeFile)
import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- parseArgv

    -- Go through the actions, perform each if necessary
    if addDependency args
        then addDependencyAction args
        else if installDependencies args
                 then installDependenciesAction args
                 else if cFlags args
                          then cFlagsAction args
                          else if libs args
                                   then libsAction args
                                   else if binaryInstallLocation args
                                            then putStrLn binLoc
                                            else if libraryInstallLocation args
                                                     then putStrLn libLoc
                                                     else if dataInstallLocation args
                                                              then putStrLn dataLoc
                                                              else if includeLocation args
                                                                then putStrLn includeLoc
                                                                else do
                                                                  progname <- getProgName
                                                                  hPutStrLn stderr $
                                                                      "Please use one flag per call, use '" ++
                                                                      progname ++ " -h' for more information"
                                                                  exitFailure

addDependencyAction :: Args -> IO ()
addDependencyAction _ = putStrLn "Adding dependency"

installDependenciesAction :: Args -> IO ()
installDependenciesAction _ = putStrLn "Installing dependencies"

cFlagsAction :: Args -> IO ()
cFlagsAction args = do
    p <- getPackageMeta args
    -- TODO: Fix shell injection problem here and in libsAction
    let standardOptions = "-Wall -Wextra -Wpedantic -Werror -pedantic-errors -O3 -g -I."
    let libraryLocations =
            intercalate " " $ (\(d, v) -> "-L" ++ libLoc ++ d ++ "/" ++ v ++ "/") <$> (assocs . dependencies) p
    let includeLocations =
            intercalate " " $ (\(d, v) -> "-I" ++ includeLoc ++ d ++ "/" ++ v ++ "/") <$> (assocs . dependencies) p
    putStrLn $ standardOptions ++ ' ' : libraryLocations ++ ' ' : includeLocations

libsAction :: Args -> IO ()
libsAction args = do
    p <- getPackageMeta args
    putStrLn $ intercalate " " $ (\(d, _) -> "-l" ++ d) <$> (assocs . dependencies) p

getPackageMeta :: Args -> IO Package
getPackageMeta args = do
    c <- readFile (input args)
    case eitherDecode c of
        Left m -> error m
        Right p -> return p

binLoc :: String
binLoc = "/usr/bin/"

libLoc :: String
libLoc = "/usr/lib/emperor/"

includeLoc :: String
includeLoc = "/usr/include/emperor/"

dataLoc :: String
dataLoc = "/usr/share/emperor/"
