-- @+leo-ver=4-thin
-- @+node:gcross.20091122100142.1310:@thin ImplicitDependencies.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091122100142.1370:<< Language extensions >>
-- @-node:gcross.20091122100142.1370:<< Language extensions >>
-- @nl

module Blueprint.Cache.ImplicitDependencies where

-- @<< Import needed modules >>
-- @+node:gcross.20091122100142.1313:<< Import needed modules >>
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

import Blueprint.Resources
-- @-node:gcross.20091122100142.1313:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091122100142.1314:Types
-- @+node:gcross.20091122100142.1321:CachedImplicitDependencies
data (Binary a, Eq a) => CachedImplicitDependencies a = CachedImplicitDependencies
        {   digestOfSourceFile :: MD5Digest
        ,   digestsOfProducedFiles :: [MD5Digest]
        ,   digestOfDependentModules :: [(ResourceId,MD5Digest)]
        ,   cachedMiscellaneousInformation :: a
        }

instance (Binary a, Eq a) => Binary (CachedImplicitDependencies a) where
    put (CachedImplicitDependencies a b c d) = put a >> put b >> put c >> put d
    get = liftM4 CachedImplicitDependencies get get get get
-- @-node:gcross.20091122100142.1321:CachedImplicitDependencies
-- @+node:gcross.20091122100142.1323:Builder
type Builder = IO (Maybe ErrorMessage)
-- @-node:gcross.20091122100142.1323:Builder
-- @+node:gcross.20091122100142.1324:Scanner
type Scanner = IO (Either ErrorMessage [ResourceId])
-- @-node:gcross.20091122100142.1324:Scanner
-- @-node:gcross.20091122100142.1314:Types
-- @+node:gcross.20091122100142.1317:Function
-- @+node:gcross.20091122100142.1328:reportUnknownResources
reportUnknownResources :: String -> [ResourceId] -> Map String String
reportUnknownResources filepath =
    Map.singleton filepath
    .
    unlines
    .
    map show
-- @-node:gcross.20091122100142.1328:reportUnknownResources
-- @+node:gcross.20091122100142.1322:analyzeImplicitDependenciesAndRebuildIfNecessary
analyzeImplicitDependenciesAndRebuildIfNecessary ::
    (Binary a, Eq a) =>
    Builder ->
    Scanner ->
    Resources ->
    FilePath ->
    [FilePath] ->
    a ->
    Resource ->
    Either ErrorMessage [MD5Digest]

analyzeImplicitDependenciesAndRebuildIfNecessary
    builder
    scanner
    resources
    cache_filepath
    product_filepaths
    miscellaneous_cache_information
    source_resource
  = unsafePerformIO $ do
    file_exists <- doesFileExist cache_filepath
    if not file_exists
        then rescanAndRebuild
        else do
            cached_digests <- decodeFile cache_filepath
            if source_digest /= digestOfSourceFile cached_digests
                then rescanAndRebuild
                else let (resource_ids,previously_seen_digests) = unzip . digestOfDependentModules $ cached_digests
                     in attemptGetResourceDigestsAndThenRun resource_ids $
                        \current_resource_digests ->
                            let rebuildIt = rebuild (zip resource_ids current_resource_digests)
                            in if any (uncurry (/=)) $ zip current_resource_digests previously_seen_digests
                                then rebuildIt
                                else do
                                    product_files_exist <- mapM doesFileExist product_filepaths
                                    if not (and product_files_exist)
                                        then rebuildIt
                                        else if miscellaneous_cache_information /= cachedMiscellaneousInformation cached_digests
                                            then rebuildIt
                                            else return . Right . digestsOfProducedFiles $ cached_digests
  where
    source_digest = (fromRight . resourceDigest) source_resource

    attemptGetResourceDigestsAndThenRun ::
        [ResourceId] ->
        ([MD5Digest] -> IO (Either ErrorMessage [MD5Digest])) ->
        IO (Either ErrorMessage [MD5Digest])
    attemptGetResourceDigestsAndThenRun resource_ids nextStep =
        case attemptGetResourceDigests resources resource_ids of
            Left (Left unknown_resource_ids) ->
                return
                .
                Left
                .
                reportUnknownResources (resourceFilePath source_resource)
                $
                unknown_resource_ids
            Left (Right error_message) -> return . Left $ error_message
            Right current_resource_digests -> nextStep current_resource_digests

    rescanAndRebuild :: IO (Either ErrorMessage [MD5Digest])
    rescanAndRebuild = do
        scan_result <- scanner
        case scan_result of
            Left error_message -> return . Left $ error_message
            Right resource_ids ->
                attemptGetResourceDigestsAndThenRun resource_ids $
                    \current_resource_digests -> rebuild (zip resource_ids current_resource_digests)

    rebuild :: [(ResourceId,MD5Digest)] -> IO (Either ErrorMessage [MD5Digest])
    rebuild dependent_module_digests = do
        build_result <- builder
        case build_result of
            Just error_message -> return . Left $ error_message
            Nothing -> do
                let product_digests = map digestOf product_filepaths
                    cached_dependencies = CachedImplicitDependencies
                        {   digestOfSourceFile = source_digest
                        ,   digestsOfProducedFiles = product_digests
                        ,   digestOfDependentModules = dependent_module_digests
                        ,   cachedMiscellaneousInformation = miscellaneous_cache_information
                        }
                createDirectoryIfMissing True . takeDirectory $ cache_filepath
                encodeFile cache_filepath cached_dependencies
                return . Right $ product_digests
-- @-node:gcross.20091122100142.1322:analyzeImplicitDependenciesAndRebuildIfNecessary
-- @-node:gcross.20091122100142.1317:Function
-- @-others
-- @-node:gcross.20091122100142.1310:@thin ImplicitDependencies.hs
-- @-leo
