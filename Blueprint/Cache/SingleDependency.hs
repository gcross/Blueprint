-- @+leo-ver=4-thin
-- @+node:gcross.20091123114318.1356:@thin SingleDependency.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091123114318.1357:<< Language extensions >>
-- @-node:gcross.20091123114318.1357:<< Language extensions >>
-- @nl

module Blueprint.Cache.SingleDependency where

-- @<< Import needed modules >>
-- @+node:gcross.20091123114318.1358:<< Import needed modules >>
import Control.Monad
import Control.Parallel.Strategies

import Data.Binary
import Data.Digest.Pure.MD5
import Data.Either.Unwrap
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map

import System.Directory
import System.FilePath
import System.IO.Unsafe

import Blueprint.Error
import Blueprint.Resources
-- @-node:gcross.20091123114318.1358:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091123114318.1359:Types
-- @+node:gcross.20091123114318.1360:CachedExplicitDependencies
data (Binary a, Eq a) => CachedDependency a = CachedDependency
        {   digestOfDependency :: MD5Digest
        ,   digestsOfProducts :: [MD5Digest]
        ,   cachedMiscellaneousInformation :: a
        }

instance (Binary a, Eq a) => Binary (CachedDependency a) where
    put (CachedDependency a b c) = put a >> put b >> put c
    get = liftM3 CachedDependency get get get
-- @-node:gcross.20091123114318.1360:CachedExplicitDependencies
-- @+node:gcross.20091123114318.1361:Builder
type Builder = IO (Maybe ErrorMessage)
-- @-node:gcross.20091123114318.1361:Builder
-- @-node:gcross.20091123114318.1359:Types
-- @+node:gcross.20091123114318.1362:Function
-- @+node:gcross.20091123114318.1363:analyzeExplicitDependenciesAndRebuildIfNecessary
analyzeDependencyAndRebuildIfNecessary ::
    (Binary a, Eq a) =>
    Builder ->
    FilePath ->
    [FilePath] ->
    a ->
    Resource ->
    Either ErrorMessage [MD5Digest]

analyzeDependencyAndRebuildIfNecessary
    builder
    cache_filepath
    product_filepaths
    miscellaneous_cache_information
    source_resource
  = case resourceDigest source_resource of
        Left error_message -> Left error_message
        Right source_digest -> unsafePerformIO $ do
            let rebuildIt = rebuild source_digest
            file_exists <- doesFileExist cache_filepath
            if not file_exists
                then rebuildIt
                else do
                    cached_digests <- decodeFile cache_filepath
                    if source_digest /= digestOfDependency cached_digests
                        then rebuildIt
                        else if miscellaneous_cache_information /= cachedMiscellaneousInformation cached_digests
                            then rebuildIt
                            else return . Right . digestsOfProducts $ cached_digests
  where
    rebuild source_digest = do
        build_result <- builder
        case build_result of
            Just error_message -> return . Left $ error_message
            Nothing -> do
                let product_digests = map digestOf product_filepaths
                    cached_dependencies = CachedDependency
                        {   digestOfDependency = source_digest
                        ,   digestsOfProducts = product_digests
                        ,   cachedMiscellaneousInformation = miscellaneous_cache_information
                        }
                createDirectoryIfMissing True . takeDirectory $ cache_filepath
                encodeFile cache_filepath cached_dependencies
                return . Right $ product_digests
-- @-node:gcross.20091123114318.1363:analyzeExplicitDependenciesAndRebuildIfNecessary
-- @-node:gcross.20091123114318.1362:Function
-- @-others
-- @-node:gcross.20091123114318.1356:@thin SingleDependency.hs
-- @-leo
