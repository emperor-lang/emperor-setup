module Install (doInstallDependencies, installPackageDependencies) where

import Args (Args, force, dryRun)
import Control.Monad (when)
import Locations (getDataLoc, getIncludeLoc, getLibLoc)
import Package (Package, Dependency, dependencies, getPackageMeta, name, version)
import           System.Directory     (createDirectoryIfMissing, doesDirectoryExist,
                                       removeDirectoryRecursive)
import           System.Exit          (ExitCode(..), exitFailure)
import           System.IO            (hPutStrLn, stderr)
import System.Process (CreateProcess(..), CmdSpec(..), StdStream(CreatePipe), readCreateProcessWithExitCode, readProcessWithExitCode)
import PackageRepo (getPackageLocation)

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
    if force args then do
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

            libLoc <- getLibLoc
            includeLoc <- getIncludeLoc
            dataLoc <- getDataLoc

            -- ! Do not refresh binLoc! This may have bad consequences :(
            refreshDir $ libLoc ++ name d ++ '/' : version d ++ "/"
            refreshDir $ includeLoc ++ name d ++ '/' : version d ++ "/"
            refreshDir $ dataLoc ++ name d ++ '/' : version d ++ "/"

            let dependencyCloneDirectory = name d ++ '-' : version d

            r <- getPackageLocation (name d) (version d)
            case r of
                Left m -> do
                    hPutStrLn stderr m
                    exitFailure
                Right u -> do
                    putStrLn $ "git clone " ++ show u ++ ' ' : show dependencyCloneDirectory
                    (c, out, err) <- if not . dryRun $ args then
                            readProcessWithExitCode "git" [ "clone", u, dependencyCloneDirectory ] ""
                        else 
                            return (ExitSuccess, "", "")
                    print (c, out, err)

                    let gitCmd = RawCommand "pwd" []
                    let gitProc = CreateProcess { cwd = Just dependencyCloneDirectory
                        , cmdspec = gitCmd
                        , env = Nothing
                        , std_in = CreatePipe
                        , std_err = CreatePipe
                        , std_out = CreatePipe
                        , close_fds = True
                        , create_group = False
                        , delegate_ctlc = False
                        , detach_console = False
                        , create_new_console = False
                        , new_session = False
                        , child_group = Nothing
                        , child_user = Nothing
                        }
                    print gitCmd
                    (c', out', err') <- if not . dryRun $ args then
                            readCreateProcessWithExitCode gitProc ""
                        else 
                            return (ExitSuccess, "", "")
                    print c
                    hPutStrLn stderr err
                    putStrLn out
                    print c'
                    hPutStrLn stderr err'
                    putStrLn out'

            installDependenciesAction'' ds
        refreshDir :: String -> IO ()
        refreshDir dir = do
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
            libLoc <- getLibLoc
            r <- doesDirectoryExist $ libLoc ++ name d ++ '/' : version d ++ "/"
            return . Right $ if r then ds' else d:ds'

