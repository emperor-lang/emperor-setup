{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Args                 (Args, addDependency, binaryInstallLocation, cFlags, dataInstallLocation,
                                       includeLocation, input, installDependencies, libraryInstallLocation, libs,
                                       parseArgv)
import           Install              (doInstallDependencies, installPackageDependencies)
import           Data.Aeson           (encode)
import           Data.ByteString.Lazy (writeFile)
import           Locations (binLoc, includeLoc, dataLoc, libLoc) 
import           Package              (Dependency(..), Package(dependencies), hasDependency, insertDependency, name,
                                       parseDependencyString, version, getPackageMeta)
import           PackageRepo          (getMostRecentVersion)
import           Prelude              hiding (getContents, readFile, writeFile)
import           System.Environment   (getProgName)
import           System.Exit          (exitFailure)
import           System.IO            (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- parseArgv

    -- Go through the actions, perform each if necessary
    if (not . null) (addDependency args) then
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
        hPutStrLn stderr $ "Please use one flag per call, use '" ++ progname ++ " -h' for more information"
        exitFailure

addDependencyAction :: Args -> IO ()
addDependencyAction args = do
    dep <- case parseDependencyString $ addDependency args of
        (d, Nothing) -> do
            vr <- getMostRecentVersion d
            case vr of
                Left m -> do
                    hPutStrLn stderr m
                    exitFailure
                Right v -> return (Dependency { dependencyName = d, dependencyVersion = v })
        (d, Just v) -> return (Dependency { dependencyName = d, dependencyVersion = v })
    r <- getPackageMeta args
    case r of
        Nothing -> do
            hPutStrLn stderr "Cannot add dependencies with no package.json/manifest"
            exitFailure
        Just p -> do
            if p `hasDependency` dep then do
                hPutStrLn stderr $ "This project already depends on " ++ show dep
                exitFailure
            else do
                putStrLn $ "Adding dependency " ++ show dep
                writePackageMeta args $ insertDependency p dep
                installPackageDependencies args p

installDependenciesAction :: Args -> IO ()
installDependenciesAction = doInstallDependencies

cFlagsAction :: Args -> IO ()
cFlagsAction args = do
    r <- getPackageMeta args
    let standardOptions = "-Wall -Wextra -Wpedantic -Werror -pedantic-errors -O3 -g -I."
    case r of
        Nothing -> putStrLn standardOptions
        Just p -> do
            let libraryLocations = unwords $ (\d -> "-L" ++ libLoc ++ (name d) ++ "/" ++ (version d) ++ "/") . sanitise <$> dependencies p
            let includeLocations = unwords $ [includeLoc ++ "/banned/"] ++ $ (\d -> "-I" ++ includeLoc ++ (name d) ++ "/" ++ (version d) ++ "/") . sanitise <$> dependencies p
            putStrLn $ standardOptions ++ ' ' : libraryLocations ++ ' ' : includeLocations

sanitise :: Dependency -> Dependency
sanitise d = Dependency { dependencyName = (sanitiseShellString . name) d, dependencyVersion = (sanitiseShellString . version) d }

libsAction :: Args -> IO ()
libsAction args = do
    r <- getPackageMeta args
    case r of
        Nothing -> return ()
        Just p -> putStrLn . unwords $ (\d -> "-l" ++ (sanitiseShellString . name) d) <$> (dependencies) p

writePackageMeta :: Args -> Package -> IO ()
writePackageMeta args p = do
    let c = encode p
    if (not . null) (input args) then do
        if input args == "-" then do
            print c
        else do
            writeFile (input args) c
    else
        writeFile "./package.json" c


sanitiseShellString :: String -> String
sanitiseShellString = (replace <$>)
    where
        replace :: Char -> Char
        replace '\'' = '_'
        replace '"' = '_'
        replace ';' = '_'
        replace '#' = '_'
        replace '*' = '_'
        replace '!' = '_'
        replace '~' = '_'
        replace '|' = '_'
        replace '>' = '_'
        replace '<' = '_'
        replace x = x
