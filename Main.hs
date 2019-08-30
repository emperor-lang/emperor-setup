{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Args                 (Args, addDependency, binaryInstallLocation, cFlags, dataInstallLocation, force,
                                       includeLocation, input, installDependencies, libraryInstallLocation, libs,
                                       parseArgv)
import           Control.Monad        (when)
import           Data.Aeson           (eitherDecode, encode)
import           Data.ByteString.Lazy (ByteString, getContents, readFile, writeFile)
import           Package              (Dependency(..), Package(dependencies), hasDependency, insertDependency, parseDependencyString)
import           PackageRepo          (getMostRecentVersion)
import           Prelude              hiding (getContents, readFile, writeFile)
import           System.Directory     (createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
                                       removeDirectoryRecursive)
import           System.Environment   (getProgName)
import           System.Exit          (exitFailure)
import           System.IO            (hPutStrLn, stderr)
import           System.Posix.Files   (fileAccess)

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
                Right v -> return (Dependency { package = d, packageVersion = v })
        (d, Just v) -> return (Dependency { package = d, packageVersion = v })
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
                installDependenciesAction' args p

installDependenciesAction :: Args -> IO ()
installDependenciesAction args = do
    r <- getPackageMeta args
    case r of
        Nothing -> do
            hPutStrLn stderr "Cannot install dependencies without dependency list"
            exitFailure
        Just p -> installDependenciesAction' args p

installDependenciesAction' :: Args -> Package -> IO ()
installDependenciesAction' args pkg = do
    let ds = dependencies pkg
    sufficientPermissions <- fileAccess libLoc True True False 
    if not sufficientPermissions then do
        pn <- getProgName
        hPutStrLn stderr $ pn ++ " has been run with insufficient permissions"
        exitFailure
    else if force args then do
        installDependenciesAction'' ds
    else do
        mdsr <- missingDependencies ds
        case mdsr of
            Left m -> do
                hPutStrLn stderr m
                exitFailure
            Right mds -> installDependenciesAction'' mds
    where
        installDependenciesAction'' :: [Dependency] -> IO ()
        installDependenciesAction'' [] = return ()
        installDependenciesAction'' (d:ds) = do
            putStrLn $ "Installing " ++ show d
            let dir = libLoc ++ package d ++ '/' : packageVersion d ++ "/"
            e <- doesDirectoryExist dir
            when e $ removeDirectoryRecursive dir
            createDirectoryIfMissing True dir

            

            installDependenciesAction'' ds

missingDependencies :: [Dependency] -> IO (Either String [Dependency])
missingDependencies [] = return . Right $ []
missingDependencies (d:ds) = do
    dsr <- missingDependencies ds
    case dsr of
        Left m -> return . Left $ m
        Right ds' -> do
            r <- doesDirectoryExist $ libLoc ++ package d ++ '/' : packageVersion d ++ "/"
            return . Right $ if r then ds' else d:ds'

cFlagsAction :: Args -> IO ()
cFlagsAction args = do
    r <- getPackageMeta args
    let standardOptions = "-Wall -Wextra -Wpedantic -Werror -pedantic-errors -O3 -g -I."
    case r of
        Nothing -> putStrLn standardOptions
        Just p -> do
            let libraryLocations = unwords $ (\d -> "-L" ++ libLoc ++ (package d) ++ "/" ++ (packageVersion d) ++ "/") . sanitise <$> dependencies p
            let includeLocations = unwords $ (\d -> "-I" ++ includeLoc ++ (package d) ++ "/" ++ (packageVersion d) ++ "/") . sanitise <$> dependencies p
            putStrLn $ standardOptions ++ ' ' : libraryLocations ++ ' ' : includeLocations

sanitise :: Dependency -> Dependency
sanitise d = Dependency { package = (sanitiseShellString . package) d, packageVersion = (sanitiseShellString . packageVersion) d }

libsAction :: Args -> IO ()
libsAction args = do
    r <- getPackageMeta args
    case r of
        Nothing -> return ()
        Just p -> putStrLn . unwords $ (\d -> "-l" ++ (sanitiseShellString . package) d) <$> (dependencies) p

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

binLoc :: String
binLoc = "/usr/bin/"

libLoc :: String
libLoc = "./" -- "/usr/lib/emperor/"

includeLoc :: String
includeLoc = "/usr/include/emperor/"

dataLoc :: String
dataLoc = "/usr/share/emperor/"
