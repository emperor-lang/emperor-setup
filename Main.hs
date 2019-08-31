{-# LANGUAGE OverloadedStrings #-}
module Main (main, getPackageLocationAction, addDependencyAction, installDependenciesAction, cFlagsAction, libsAction) where

import           Args                 (Args, addDependency, binaryInstallLocation, cFlags, dataInstallLocation,
                                       entryPoint, getPackageLocation, includeLocation, input, installDependencies,
                                       languageHeaderLocation, libraryInstallLocation, libs, parseArgv,
                                       updatePackageRepo)
import           Data.Aeson           (encode)
import           Data.ByteString.Lazy (writeFile)
import           Install              (doInstallDependencies, ensurePackageRepoExists, installPackageDependencies)
import           Locations            (getBinLoc, getIncludeInstallLoc, getPackageInstallLoc)
import           Package              (Dependency(..), Package(dependencies), getPackageMeta, hasDependency,
                                       insertDependency, name, parseDependencyString, version)
import           PackageRepo          (getMostRecentVersion)
import           Prelude              hiding (writeFile)
import           System.Environment   (getProgName)
import           System.Exit          (exitFailure)
import           System.IO            (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- parseArgv

    -- Go through the actions, perform each if necessary
    if (not . null) (getPackageLocation args) then
        getPackageLocationAction args
    else if (not . null) (addDependency args) then
        addDependencyAction args
    else if installDependencies args then
        installDependenciesAction args
    else if cFlags args then
        cFlagsAction args
    else if libs args then
        libsAction args
    else if binaryInstallLocation args then do
        binLoc <- getBinLoc
        putStrLn binLoc
    else if libraryInstallLocation args then do
        libLoc <- getPackageInstallLoc
        putStrLn libLoc
    else if dataInstallLocation args then do
        dataLoc <- getPackageInstallLoc
        putStrLn dataLoc
    else if includeLocation args then do
        includeLoc <- getPackageInstallLoc
        putStrLn includeLoc
    else if updatePackageRepo args then
        ensurePackageRepoExists args
    else if languageHeaderLocation args then do
        includeInstallLoc <- getIncludeInstallLoc
        putStrLn includeInstallLoc
    else do
        progname <- getProgName
        hPutStrLn stderr $ "Please specify a command flag\nTry '" ++ progname ++ " -h' for more information"
        exitFailure

getPackageLocationAction :: Args -> IO ()
getPackageLocationAction args = do
        let pn = getPackageLocation args
        r <- getPackageMeta args
        case r of
            Nothing -> do
                vr <- getMostRecentVersion pn
                case vr of
                    Left m -> do
                        hPutStrLn stderr m
                        exitFailure
                    Right v -> do
                        getPackageLocationAction' Dependency { dependencyName = pn, dependencyVersion = v }

            Just p -> do
                let ds = filter (\d -> name d == pn) $ dependencies p
                if null ds then do
                    hPutStrLn stderr $ show pn ++ " is not a documented dependency of this project, please add it to the manifest with 'emperor-setup -a " ++ show pn ++ "'"
                    exitFailure
                else if length ds >= 2 then do
                    hPutStrLn stderr $ "There are multiple named dependencies named " ++ show pn ++ ", please reduce this down to one"
                    exitFailure
                else do
                    let d = head ds
                    getPackageLocationAction' d
    where
        getPackageLocationAction' :: Dependency -> IO ()
        getPackageLocationAction' d = do
            packageInstallLoc <- getPackageInstallLoc
            putStrLn $ packageInstallLoc ++ name d ++ '/' : version d ++ "/"

addDependencyAction :: Args -> IO ()
addDependencyAction args = do
    dep <- case parseDependencyString $ addDependency args of
        (d, Nothing) -> do
            vr <- getMostRecentVersion d
            case vr of
                Left m -> do
                    hPutStrLn stderr m
                    exitFailure
                Right v -> return Dependency { dependencyName = d, dependencyVersion = v }
        (d, Just v) -> return Dependency { dependencyName = d, dependencyVersion = v }
    r <- getPackageMeta args
    case r of
        Nothing -> do
            hPutStrLn stderr "Cannot add dependencies with no manifest.json"
            exitFailure
        Just p ->
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
    let standardOptions = "-Wall -Wextra -Wpedantic -Werror -pedantic-errors -O3 -g -I." ++ if entryPoint args then "" else " -c"
    case r of
        Nothing -> putStrLn standardOptions
        Just p -> do
            packageInstallLoc <- getPackageInstallLoc
            includeInstallLoc <- getIncludeInstallLoc
            let libraryLocations = unwords $ (\d -> "-L" ++ packageInstallLoc ++ name d ++ "/" ++ version d ++ "/") . sanitise <$> dependencies p
            let includeLocations = unwords $ (["-I" ++ includeInstallLoc] ++) $ (\d -> "-I" ++ packageInstallLoc ++ name d ++ "/" ++ version d ++ "/") . sanitise <$> dependencies p
            putStrLn $ standardOptions ++ ' ' : libraryLocations ++ ' ' : includeLocations

sanitise :: Dependency -> Dependency
sanitise d = Dependency { dependencyName = (sanitiseShellString . name) d, dependencyVersion = (sanitiseShellString . version) d }

libsAction :: Args -> IO ()
libsAction args = do
    r <- getPackageMeta args
    case r of
        Nothing -> return ()
        Just p -> putStrLn . unwords $ (\d -> "-l" ++ (sanitiseShellString . name) d) <$> dependencies p

writePackageMeta :: Args -> Package -> IO ()
writePackageMeta args p = do
    let c = encode p
    if (not . null) (input args) then
        if input args == "-" then
            print c
        else
            writeFile (input args) c
    else
        writeFile "./manifest.json" c


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
