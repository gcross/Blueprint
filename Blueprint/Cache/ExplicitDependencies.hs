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
import Control.Monad.Error
import Control.Monad.Trans
import Control.Parallel.Strategies

import Data.Binary
import Data.Digest.Pure.MD5
import Data.Either.Unwrap
import Data.ErrorMessage
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map

import System.Directory
import System.FilePath
import System.IO.Unsafe

import Blueprint.Resources

-- @-node:gcross.20091122100142.1374:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091122100142.1379:Types
-- @+node:gcross.20091122100142.1380:CachedExplicitDependencies
data (Binary a, Eq a) => CachedExplicitDependencies a = CachedExplicitDependencies
        {   digestsOfDependencies :: Map ResourceId MD5Digest
        ,   digestsOfMandatoryProducts :: [MD5Digest]
        ,   digestsOfOptionalProducts :: [Maybe MD5Digest]
        ,   cachedMiscellaneousInformation :: a
        }

instance (Binary a, Eq a) => Binary (CachedExplicitDependencies a) where
    put (CachedExplicitDependencies a b c d) = put a >> put b >> put c >> put d
    get = liftM4 CachedExplicitDependencies get get get get
-- @-node:gcross.20091122100142.1380:CachedExplicitDependencies
-- @+node:gcross.20091122100142.1385:Builder
type Builder = ErrorT ErrorMessage IO ()
-- @-node:gcross.20091122100142.1385:Builder
-- @-node:gcross.20091122100142.1379:Types
-- @+node:gcross.20091122100142.1386:Function
-- @+node:gcross.20091122100142.1387:analyzeExplicitDependenciesAndRebuildIfNecessary_
analyzeExplicitDependenciesAndRebuildIfNecessary_ ::
    (Binary a, Eq a) =>
    Builder ->
    FilePath ->
    [FilePath] ->
    [FilePath] ->
    a ->
    [Resource] ->
    ErrorMessageOr ([MD5Digest],[Maybe MD5Digest])
analyzeExplicitDependenciesAndRebuildIfNecessary_ _ _ _ _ _ [] = error "Must supply at least one resource for build!"
analyzeExplicitDependenciesAndRebuildIfNecessary_
    builder
    cache_filepath
    mandatory_product_filepaths
    optional_product_filepaths
    miscellaneous_cache_information
    source_resources
  = attemptGetDigests source_resources
    >>=
    \source_digests -> unsafePerformIO $ do
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
                    else do
                        product_files_exist <- mapM doesFileExist mandatory_product_filepaths
                        if not (and product_files_exist)
                            then rebuildIt
                            else if miscellaneous_cache_information /= cachedMiscellaneousInformation cached_digests
                                then rebuildIt
                                else return . Right $
                                    (digestsOfMandatoryProducts cached_digests
                                    ,digestsOfOptionalProducts cached_digests
                                    )
  where
    rebuild source_digests_as_map = runErrorT $
        builder
        >>
        let mandatory_product_digests = map digestOf mandatory_product_filepaths
            optional_product_digests = map maybeDigestOf optional_product_filepaths
            cached_dependencies = CachedExplicitDependencies
                {   digestsOfDependencies = source_digests_as_map
                ,   digestsOfMandatoryProducts = mandatory_product_digests
                ,   digestsOfOptionalProducts = optional_product_digests
                ,   cachedMiscellaneousInformation = miscellaneous_cache_information
                }
        in (liftIO $ do
            createDirectoryIfMissing True . takeDirectory $ cache_filepath
            encodeFile cache_filepath cached_dependencies
           )
           >>
           return (mandatory_product_digests,optional_product_digests)
-- @-node:gcross.20091122100142.1387:analyzeExplicitDependenciesAndRebuildIfNecessary_
-- @+node:gcross.20091215083221.1611:analyzeExplicitDependenciesAndRebuildIfNecessary
analyzeExplicitDependenciesAndRebuildIfNecessary ::
    (Binary a, Eq a) =>
    Builder ->
    FilePath ->
    [FilePath] ->
    [FilePath] ->
    a ->
    [Resource] ->
    ([ErrorMessageOr MD5Digest],[Maybe (ErrorMessageOr MD5Digest)])
analyzeExplicitDependenciesAndRebuildIfNecessary
    builder
    cache_filepath
    mandatory_product_filepaths
    optional_product_filepaths
    miscellaneous_cache_information
    source_resources
  = case build_result of
        Left error_message ->
            (replicate (length mandatory_product_filepaths) (Left error_message)
            ,replicate (length optional_product_filepaths) (Just $ Left error_message)
            )
        Right (mandatory_product_digests,optional_product_digests) ->
                (map Right mandatory_product_digests
                ,map (fmap Right) optional_product_digests
                )
  where
    build_result =
        analyzeExplicitDependenciesAndRebuildIfNecessary_
            builder
            cache_filepath
            mandatory_product_filepaths
            optional_product_filepaths
            miscellaneous_cache_information
            source_resources
-- @-node:gcross.20091215083221.1611:analyzeExplicitDependenciesAndRebuildIfNecessary
-- @-node:gcross.20091122100142.1386:Function
-- @-others
-- @-node:gcross.20091122100142.1369:@thin ExplicitDependencies.hs
-- @-leo
