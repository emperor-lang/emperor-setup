module Install (doInstallDependencies, installPackageDependencies) where

import           Args             (Args, dryRun, force)
import           Control.Monad    (unless, when)
import           Locations        (getPackageInstallLoc)
import           Package          (Dependency, Package, dependencies, files, getPackageFromDirectory, getPackageMeta,
                                   name, version)
import           PackageRepo      (getPackageLocation)
import           System.Directory (copyFileWithMetadata, createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
                                   removeDirectoryRecursive)
import           System.Exit      (ExitCode(..), exitFailure)
import           System.IO        (hPutStr, hPutStrLn, stderr)
import           System.Process   (CmdSpec(..), CreateProcess(..), StdStream(CreatePipe), readCreateProcessWithExitCode,
                                   readProcessWithExitCode)

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
    if force args then
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

            packageInstallLoc <- getPackageInstallLoc
            let installDirectory = packageInstallLoc ++ name d ++ '/' : version d ++ "/"
            refreshDir installDirectory

            let dependencyCloneDirectory = name d ++ '-' : version d

            r <- getPackageLocation (name d) (version d)
            case r of
                Left m -> do
                    hPutStrLn stderr m
                    exitFailure
                Right u -> do
                    e <- doesDirectoryExist dependencyCloneDirectory
                    when e $ removeDirectoryRecursive dependencyCloneDirectory

                    putStrLn $ "git clone " ++ show u ++ ' ' : show dependencyCloneDirectory
                    (c, _, err) <- if not . dryRun $ args then
                            readProcessWithExitCode "git" [ "clone", "--depth=1", u, dependencyCloneDirectory ] ""
                        else
                            return (ExitSuccess, "", "")
                    hPutStr stderr err
                    if c /= ExitSuccess then do
                        hPutStrLn stderr $ "Failed to get dependency " ++ show d
                        exitFailure
                    else do
                        mr <- doesFileExist $ dependencyCloneDirectory ++ "/manifest.json"
                        unless mr $ do
                            hPutStrLn stderr "Could not find manifest in downloaded source"
                            exitFailure

                        let gitCmd = RawCommand "make" []
                        let gitProc = createProcessInDirectory gitCmd dependencyCloneDirectory
                        putStrLn "make"
                        (c', out', err') <- if not . dryRun $ args then
                                readCreateProcessWithExitCode gitProc ""
                            else
                                return (ExitSuccess, "", "")
                        if c' /= ExitSuccess then do
                            hPutStr stderr err'
                            putStr out'
                            hPutStrLn stderr $ "Running make failed for " ++ show d
                            exitFailure
                        else do
                            hPutStr stderr err'
                            putStr out'

                        if dryRun args then do
                            putStrLn "Package files would now be installed... (dry run)"
                            putStrLn "Package files would now be cleaned... (dry run)"
                        else do
                            pr <- getPackageFromDirectory dependencyCloneDirectory
                            case pr of
                                Nothing -> do
                                    hPutStrLn stderr $ "Could not find package manifest in " ++ show d
                                    exitFailure
                                Just p -> do
                                    let fs = ((dependencyCloneDirectory ++ "/") ++) <$> files p
                                    validateMakeResult fs
                                    createDirectoryIfMissing True installDirectory
                                    distributeFiles installDirectory $ zip (files p) fs
                            hPutStrLn stderr $ "rm -rf " ++ dependencyCloneDirectory
                            removeDirectoryRecursive dependencyCloneDirectory

            installDependenciesAction'' ds
        refreshDir :: String -> IO ()
        refreshDir dir = do
            e <- doesDirectoryExist dir
            when e $ removeDirectoryRecursive dir
            createDirectoryIfMissing True dir

        validateMakeResult :: [FilePath] -> IO ()
        validateMakeResult [] = return ()
        validateMakeResult (f':fs') = do
            e <- doesFileExist f'
            if e then
                validateMakeResult fs'
            else do
                hPutStrLn stderr $ "Compiled module did not produce file " ++ show f' ++ " as was promised by its manifest"
                exitFailure

        distributeFiles :: FilePath -> [(FilePath, FilePath)] -> IO ()
        distributeFiles _ [] = return ()
        distributeFiles t ((f,pf):fs) = do
            putStrLn $ "install " ++ show pf ++ ' ' : show (t ++ f)
            copyFileWithMetadata pf (t ++ f)
            distributeFiles t fs

createProcessInDirectory :: CmdSpec -> String -> CreateProcess
createProcessInDirectory c d = CreateProcess { cwd = Just d
    , cmdspec = c
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

missingDependencies :: [Dependency] -> IO (Either String [Dependency])
missingDependencies [] = return . Right $ []
missingDependencies (d:ds) = do
    dsr <- missingDependencies ds
    case dsr of
        Left m -> return . Left $ m
        Right ds' -> do
            packageInstallLoc <- getPackageInstallLoc
            r <- doesDirectoryExist $ packageInstallLoc ++ name d ++ '/' : version d ++ "/"
            return . Right $ if r then ds' else d:ds'

