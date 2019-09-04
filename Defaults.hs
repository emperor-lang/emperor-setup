{-|
Module      : Defaults
Description : Default emperor libraries
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

Obtains a list of default emperor libraries for installation at the latest known
version.
-}
module Defaults (getDefaultDependencies) where

import           Package (Dependency(..))
import           PackageRepo (getMostRecentVersion)

-- | Obtain a list of default dependencies (with their versions) to be used when a manifest.json has neither been
-- specified nor found.
getDefaultDependencies :: IO (Either String [Dependency])
getDefaultDependencies = getDefaultDependencies' ["base", "std"]
    where
        getDefaultDependencies' :: [String] -> IO (Either String [Dependency])
        getDefaultDependencies' [] = return . Right $ []
        getDefaultDependencies' (s:ss) = do
            vr <- getMostRecentVersion s
            case vr of
                Left m -> return . Left $ m
                Right v -> do
                    rs <- getDefaultDependencies' ss
                    case rs of
                        Left m -> return . Left $ m
                        Right ds -> return . Right $ Dependency s v : ds
