-- @+leo-ver=4-thin
-- @+node:gcross.20091122100142.1369:@thin ExplicitDependencies.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091122100142.1372:<< Language extensions >>
-- @-node:gcross.20091122100142.1372:<< Language extensions >>
-- @nl

module Blueprint.Cache.ExplicitDependencies where

-- @<< Import needed modules >>
-- @+node:gcross.20091122100142.1374:<< Import needed modules >>
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
-- @-node:gcross.20091122100142.1374:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091122100142.1379:Types
-- @+node:gcross.20091122100142.1380:CachedExplicitDependencies
data (Binary a, Eq a) => CachedExplicitDependencies a = CachedExplicitDependencies
        {   digestsOfDependencies :: Map ResourceId MD5Digest
        ,   digestsOfProducts :: [MD5Digest]
        ,   cachedMiscellaneousInformation :: a
        }

instance (Binary a, Eq a) => Binary (CachedExplicitDependencies a) where
    put (CachedExplicitDependencies a b c) = put a >> put b >> put c
    get = liftM3 CachedExplicitDependencies get get get
-- @-node:gcross.20091122100142.1380:CachedExplicitDependencies
-- @+node:gcross.20091122100142.1385:Builder
type Builder = IO (Maybe ErrorMessage)
-- @-node:gcross.20091122100142.1385:Builder
-- @-node:gcross.20091122100142.1379:Types
-- @+node:gcross.20091122100142.1386:Function
-- @+node:gcross.20091122100142.1387:analyzeExplicitDependenciesAndRebuildIfNecessary
analyzeExplicitDependenciesAndRebuildIfNecessary ::
    (Binary a, Eq a) =>
    Builder ->
    FilePath ->
    [FilePath] ->
    a ->
    [Resource] ->
    Either ErrorMessage [MD5Digest]

analyzeExplicitDependenciesAndRebuildIfNecessary
    builder
    cache_filepath
    product_filepaths
    miscellaneous_cache_information
    source_resources
  = case attemptGetDigests source_resources of
        Left error_message -> Left error_message
        Right source_digests -> unsafePerformIO $ do
            let source_digests_as_map = Map.fromList $ zip (map resourceId source_resources) source_digests
                rebuildIt = rebuild source_digests_as_map
            file_exists <- doesFileExist cache_filepath
            if not file_exists
                then rebuildIt
                else do
                    cached_digests <- decodeFile cache_filepath
                    let cached_digests_as_map = digestsOfDependencies cached_digests
                        compared_digests = Map.intersectionWith (==) source_digests_as_map cached_digests_as_map 
                    if    (Map.size source_digests_as_map /= Map.size compared_digests)
                       || (Map.size cached_digests_as_map /= Map.size compared_digests)
                       || not (Map.fold (&&) True compared_digests)
                        then rebuildIt
                        else if miscellaneous_cache_information /= cachedMiscellaneousInformation cached_digests
                            then rebuildIt
                            else return . Right . digestsOfProducts $ cached_digests
  where
    rebuild source_digests_as_map = do
        build_result <- builder
        case build_result of
            Just error_message -> return . Left $ error_message
            Nothing -> do
                let product_digests = map digestOf product_filepaths
                    cached_dependencies = CachedExplicitDependencies
                        {   digestsOfDependencies = source_digests_as_map
                        ,   digestsOfProducts = product_digests
                        ,   cachedMiscellaneousInformation = miscellaneous_cache_information
                        }
                createDirectoryIfMissing True . takeDirectory $ cache_filepath
                encodeFile cache_filepath cached_dependencies
                return . Right $ product_digests
-- @-node:gcross.20091122100142.1387:analyzeExplicitDependenciesAndRebuildIfNecessary
-- @-node:gcross.20091122100142.1386:Function
-- @-others
-- @-node:gcross.20091122100142.1369:@thin ExplicitDependencies.hs
-- @-leo
