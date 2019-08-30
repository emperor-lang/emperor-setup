{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Args                 (Args, addDependency, binaryInstallLocation, cFlags, dataInstallLocation,
                                       includeLocation, input, installDependencies, libraryInstallLocation, libs,
                                       parseArgv)
import           Data.Aeson           (eitherDecode)
import           Data.ByteString.Lazy (ByteString, getContents, readFile)
import           Data.Map             (assocs)
import           Package              (Package, dependencies)
import           Prelude              hiding (getContents, readFile, writeFile)
import           System.Directory     (doesFileExist)
import           System.Environment   (getProgName)
import           System.Exit          (exitFailure)
import           System.IO            (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- parseArgv

    -- Go through the actions, perform each if necessary
    if addDependency args then
        addDependencyAction args
    else if installDependencies args then
        installDependenciesAction args
    else if cFlags args then
        cFlagsAction args
    else if libs args then
        libsAction args
    else if binaryInstallLocation args then
        putStrLn binLoc
    else if libraryInstallLocation args then
        putStrLn libLoc
    else if dataInstallLocation args then
        putStrLn dataLoc
    else if includeLocation args then
        putStrLn includeLoc
    else do
        progname <- getProgName
        hPutStrLn stderr $
            "Please use one flag per call, use '" ++ progname ++ " -h' for more information"
        exitFailure

addDependencyAction :: Args -> IO ()
addDependencyAction args = do
    putStrLn "Adding dependency"
    installDependenciesAction args

installDependenciesAction :: Args -> IO ()
installDependenciesAction _ = putStrLn "Refreshing dependencies"

cFlagsAction :: Args -> IO ()
cFlagsAction args = do
    r <- getPackageMeta args
    let standardOptions = "-Wall -Wextra -Wpedantic -Werror -pedantic-errors -O3 -g -I."
    case r of
        Nothing -> putStrLn standardOptions
        Just p -> do
            let libraryLocations = unwords $ (\(d, v) -> "-L" ++ libLoc ++ d ++ "/" ++ v ++ "/") . (\(d,v) -> (sanitiseShellString d, sanitiseShellString v)) <$> (assocs . dependencies) p
            let includeLocations = unwords $ (\(d, v) -> "-I" ++ includeLoc ++ d ++ "/" ++ v ++ "/") . (\(d,v) -> (sanitiseShellString d, sanitiseShellString v)) <$> (assocs . dependencies) p
            putStrLn $ standardOptions ++ ' ' : libraryLocations ++ ' ' : includeLocations

libsAction :: Args -> IO ()
libsAction args = do
    r <- getPackageMeta args
    case r of
        Nothing -> return ()
        Just p -> putStrLn . unwords $ (\(d, _) -> "-l" ++ sanitiseShellString d) <$> (assocs . dependencies) p

getPackageMeta :: Args -> IO (Maybe Package)
getPackageMeta args =
    if (not . null) (input args) then do
        if input args == "-" then do
            c <- getContents
            getPackageMeta' c
        else do
            c <- readFile $ input args
            getPackageMeta' c
    else do
        r <- doesFileExist "./package.json"
        if r then do
            c <- readFile "./package.json"
            getPackageMeta' c
        else
            return Nothing
    where
        getPackageMeta' :: ByteString -> IO (Maybe Package)
        getPackageMeta' c = case eitherDecode c of
            Left m -> do
                hPutStrLn stderr $ "Failed to parse json from input: " ++ m
                exitFailure
            Right p -> return $ Just p

sanitiseShellString :: String -> String
sanitiseShellString = (replace <$>)
    where
        replace :: Char -> Char
        replace '\'' = '+'
        replace '"' = '-'
        replace '/' = '_'
        replace x = x

binLoc :: String
binLoc = "/usr/bin/"

libLoc :: String
libLoc = "/usr/lib/emperor/"

includeLoc :: String
includeLoc = "/usr/include/emperor/"

dataLoc :: String
dataLoc = "/usr/share/emperor/"
