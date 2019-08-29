module Main where

import Args (parseArgv, Args, input, addDependency, installDependencies, cFlags, libs, binaryInstallLocation, libraryInstallLocation, dataInstallLocation)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (readFile) -- , writeFile)
import Data.List (intercalate)
import Package (Package, dependencies)
import Prelude hiding (readFile, writeFile)
import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Data.Map (assocs)

main :: IO ()
main = do
    args <- parseArgv
    checkUnique args

    if addDependency args
        then addDependencyAction args
        else if installDependencies args
            then installDependenciesAction args
            else if cFlags args
                then cFlagsAction args
                else if libs args
                    then libsAction args
                    else if binaryInstallLocation args
                        then binaryInstallLocationAction args
                        else if libraryInstallLocation args
                            then libraryInstallLocationAction args
                            else if dataInstallLocation args
                                then libraryInstallLocationAction args
                                else do
                                    progname <- getProgName
                                    hPutStrLn stderr $ "Please use one flag per call, use '" ++ progname ++ " -h' for more information"
                                    exitFailure

addDependencyAction :: Args -> IO ()
addDependencyAction _ = putStrLn "Adding dependency"

installDependenciesAction :: Args -> IO ()
installDependenciesAction _ = putStrLn "Installing dependencies"

cFlagsAction :: Args -> IO ()
cFlagsAction args = do
    p <- getPackageMeta args
    -- TODO: Fix shell injection problem here and in libsAction
    let libraryLocations = intercalate " " $ (\(d,v) -> "-L" ++ libLoc ++ d ++ "/" ++ v ++ "/") <$> (assocs . dependencies) p
    let includeLocations = intercalate " " $ (\(d,v) -> "-I" ++ includeLoc ++ d ++ "/" ++ v ++ "/") <$> (assocs . dependencies) p
    putStrLn $ "-Wall -Werror -Wpedantic -pedantic-errors -O3 -g -I. " ++ libraryLocations ++ " " ++ includeLocations

libsAction :: Args -> IO ()
libsAction args = do
    p <- getPackageMeta args
    putStrLn $ intercalate " " $ (\(d,_) -> "-l" ++ d) <$> (assocs . dependencies) p

getPackageMeta :: Args -> IO Package
getPackageMeta args = do
    c <- readFile (input args)
    let r = eitherDecode c
    case r of
        Left m -> error m
        Right p -> return p


binaryInstallLocationAction :: Args -> IO ()
binaryInstallLocationAction _ = putStrLn "/usr/bin/"

libraryInstallLocationAction :: Args -> IO ()
libraryInstallLocationAction _ = putStrLn libLoc

libLoc :: String
libLoc = "/usr/lib/emperor/"

includeLoc :: String
includeLoc = "/usr/include/emperor/"

dataInstallLocationAction :: Args -> IO ()
dataInstallLocationAction _ = putStrLn "/usr/share/emperor/"

checkUnique :: Args -> IO ()
checkUnique args =
    if length
           (filter
                id
                [ addDependency args
                , installDependencies args
                , cFlags args
                , libs args
                , binaryInstallLocation args
                , libraryInstallLocation args
                , dataInstallLocation args
                ]) == 1
        then return ()
        else do
            progname <- getProgName
            hPutStrLn stderr $ "Please use one flag per call, use '" ++ progname ++ " -h' for more information"
            exitFailure
