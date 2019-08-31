module Install (doInstallDependencies, installPackageDependencies) where

import Args (Args, force)
import Control.Monad (when)
import Locations (libLoc)
import Package (Package, Dependency, dependencies, getPackageMeta, name, version)
import           System.Directory     (createDirectoryIfMissing, doesDirectoryExist,
                                       removeDirectoryRecursive)
import           System.Environment   (getProgName)
import           System.Exit          (exitFailure)
import           System.IO            (hPutStrLn, stderr)
import           System.Posix.Files   (fileAccess)
import System.Process (readProcessWithExitCode)

doInstallDependencies :: Args -> IO ()
doInstallDependencies args = do
    r <- getPackageMeta args
    case r of
        Nothing -> do
            hPutStrLn stderr "Cannot install dependencies without dependency list"
            exitFailure
        Just p -> installPackageDependencies args p

installPackageDependencies :: Args -> Package -> IO ()
installPackageDependencies args pkg = do
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
            
            refreshDir $ libLoc ++ name d ++ '/' : version d ++ "/"
            refreshDir $ includeLoc ++ name d ++ '/' : version d ++ "/"
            refreshDir $ dataLoc ++ name d ++ '/' : version d ++ "/"

            let dependencyCloneDirectory = name d ++ '-' : version d

            r <- getPackageLocation $ name d $ version d
            case r of
                Left m -> do
                    hPutStrLn stderr m
                    exitFailure
                Right u ->
                    readProcessWithExitCode "git" [ "clone", u, dependencyCloneDirectory ] ""


            installDependenciesAction'' ds
        refreshDir :: String -> IO ()
        refreshDir dir' = do
            e <- doesDirectoryExist dir
            when e $ removeDirectoryRecursive dir
            createDirectoryIfMissing True dir

missingDependencies :: [Dependency] -> IO (Either String [Dependency])
missingDependencies [] = return . Right $ []
missingDependencies (d:ds) = do
    dsr <- missingDependencies ds
    case dsr of
        Left m -> return . Left $ m
        Right ds' -> do
            r <- doesDirectoryExist $ libLoc ++ name d ++ '/' : version d ++ "/"
            return . Right $ if r then ds' else d:ds'

