{-|
Module      : Install
Description : Package installer for the emperor package manager
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This installs dependencies of a package and where necessary will download a list
of the available packages.
-}
module Install (doInstallDependencies, ensurePackageRepoExists, installPackageDependencies) where

import           Args             (Args, dryRun, force, updatePackageRepo)
import           Control.Monad    (unless, when)
import           Defaults         (getDefaultDependencies)
import           Locations        (getPackageInstallLoc)
import           Package          (Dependency, Package, dependencies, files, getPackageFromDirectory, getPackageMeta,
                                   name, version)
import           PackageRepo      (getPackageLocation, packageRepoDefaultLocation)
import           System.Directory (copyFileWithMetadata, createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
                                   removeDirectoryRecursive)
import           System.Exit      (ExitCode(..), exitFailure)
import           System.IO        (hPutStrLn, stderr, stdout)
import           System.Process   (CmdSpec(..), CreateProcess(..), StdStream(..), createProcess, waitForProcess)
-- | Install the dependencies required by the command-line arguments
doInstallDependencies :: Args -> IO ()
doInstallDependencies args = do
    ensurePackageRepoExists args
    r <- getPackageMeta args
    case r of
        Nothing -> do
            hPutStrLn stderr "No package list found, updating default libraries"
            dsr <- getDefaultDependencies
            case dsr of
                Left m -> do
                    hPutStrLn stderr m
                    exitFailure
                Right ds -> installDependenciesAction'' args ds
        Just p -> installPackageDependencies' args p

-- | Install the dependencies of a given package
installPackageDependencies :: Args -> Package -> IO ()
installPackageDependencies args pkg = do
    ensurePackageRepoExists args
    installPackageDependencies' args pkg

-- | Install the dependencies of a given package, under the assumption that the
-- package repository already exists
installPackageDependencies' :: Args -> Package -> IO ()
installPackageDependencies' args pkg = do
    let ds = dependencies pkg
    if force args then
        installDependenciesAction'' args ds
    else do
        mdsr <- missingDependencies ds
        case mdsr of
            Left m -> do
                hPutStrLn stderr m
                exitFailure
            Right mds -> installDependenciesAction'' args mds

installDependenciesAction'' :: Args -> [Dependency] -> IO ()
installDependenciesAction'' _ [] = return ()
installDependenciesAction'' args (d:ds) = do
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

                -- putStrLn $ "git clone " ++ show u ++ ' ' : show dependencyCloneDirectory
                let packageFetchCmd = RawCommand "git" [ "clone", "--depth=1", u, dependencyCloneDirectory ]
                let packageFetchProc = createProcessInDirectory packageFetchCmd "."
                c <- execute args packageFetchCmd packageFetchProc
                if c /= ExitSuccess then do
                    hPutStrLn stderr $ "Failed to get dependency " ++ show d
                    exitFailure
                else do
                    unless (dryRun args) $ do
                        mr <- doesFileExist $ dependencyCloneDirectory ++ "/manifest.json"
                        unless mr $ do
                            hPutStrLn stderr "Could not find manifest in downloaded source"
                            exitFailure

                    let gitCmd = RawCommand "make" []
                    let gitProc = createProcessInDirectory gitCmd dependencyCloneDirectory
                    c' <- execute args gitCmd gitProc
                    when (c' /= ExitSuccess) $ do
                        hPutStrLn stderr $ "Running make failed for " ++ show d
                        exitFailure

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

        installDependenciesAction'' args ds
    where
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

-- | Installs the package repository if necessary
ensurePackageRepoExists :: Args -> IO ()
ensurePackageRepoExists args =
        if updatePackageRepo args then
            ensurePackageRepoExists'
        else do
            e <- doesFileExist packageRepoDefaultLocation
            unless e ensurePackageRepoExists'
    where
        ensurePackageRepoExists' :: IO ()
        ensurePackageRepoExists' = do
            -- Clean if folder already present
            let cloneLocation = "./.emperor-known-packages/"
            e' <- doesDirectoryExist cloneLocation
            when e' $ removeDirectoryRecursive cloneLocation

            -- Clone the known packages repo
            let packageRepoCloneCmd = RawCommand "git" [ "clone", "--depth=1", "https://github.com/emperor-lang/known-packages.git", cloneLocation ]
            let packageRepoCloneProc = createProcessInDirectory packageRepoCloneCmd "."
            c <- execute args packageRepoCloneCmd packageRepoCloneProc
            if c /= ExitSuccess then do
                hPutStrLn stderr "Failed to clone repo of known packages"
                exitFailure
            else do
                -- Install the package repo
                let packageRepoBuildCmd = RawCommand "make" ["install"]
                let packageRepoBuildProc = createProcessInDirectory packageRepoBuildCmd cloneLocation
                c' <- execute args packageRepoBuildCmd packageRepoBuildProc
                unless (c' == ExitSuccess) $ do
                    hPutStrLn stderr "Failed to install list of known packages"
                    exitFailure
            -- Clean once done 
            e'' <- doesDirectoryExist cloneLocation
            when e'' $ removeDirectoryRecursive cloneLocation

execute :: Args -> CmdSpec -> CreateProcess -> IO ExitCode
execute args cmd proc = do
        putStrLn $ showCmd cmd
        if not . dryRun $ args then do
            (_, _, _, h) <- createProcess proc
            waitForProcess h
        else
            return ExitSuccess
    where
        showCmd :: CmdSpec -> String
        showCmd cmd' = case cmd' of
            ShellCommand s -> s
            RawCommand s ss -> s ++ ' ' : unwords ss

createProcessInDirectory :: CmdSpec -> String -> CreateProcess
createProcessInDirectory c d = CreateProcess { cwd = Just d
    , cmdspec = c
    , env = Nothing
    , std_in = CreatePipe
    , std_err = UseHandle stderr
    , std_out = UseHandle stdout
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

