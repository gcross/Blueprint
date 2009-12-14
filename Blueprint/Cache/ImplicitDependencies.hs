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
import Control.Arrow
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
import Data.Set (Set)
import qualified Data.Set as Set

import System.Directory
import System.FilePath
import System.IO.Unsafe

import Text.PrettyPrint.ANSI.Leijen

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
        ,   resourceIdsOfLinkDependencies :: [ResourceId]
        ,   cachedMiscellaneousInformation :: a
        }

instance (Binary a, Eq a) => Binary (CachedImplicitDependencies a) where
    put (CachedImplicitDependencies a b c d e) = put a >> put b >> put c >> put d >> put e
    get = liftM5 CachedImplicitDependencies get get get get get
-- @-node:gcross.20091122100142.1321:CachedImplicitDependencies
-- @+node:gcross.20091122100142.1323:Builder
type Builder = ErrorT ErrorMessage IO ()
-- @-node:gcross.20091122100142.1323:Builder
-- @+node:gcross.20091122100142.1324:Scanner
type Scanner = ErrorT ErrorMessage IO ([ResourceId],[ResourceId])
-- @-node:gcross.20091122100142.1324:Scanner
-- @-node:gcross.20091122100142.1314:Types
-- @+node:gcross.20091122100142.1317:Function
-- @+node:gcross.20091122100142.1322:analyzeImplicitDependenciesAndRebuildIfNecessary_
analyzeImplicitDependenciesAndRebuildIfNecessary_ ::
    (Binary a, Eq a) =>
    Builder ->
    Scanner ->
    Resources ->
    FilePath ->
    [FilePath] ->
    a ->
    Resource ->
    ErrorMessageOr ([MD5Digest],[ResourceId])

analyzeImplicitDependenciesAndRebuildIfNecessary_
    builder
    scanner
    resources
    cache_filepath
    product_filepaths
    miscellaneous_cache_information
    source_resource
  = unsafePerformIO . runErrorT $
    (liftIO . doesFileExist $ cache_filepath) >>=
        \file_exists -> if not file_exists
            then rescanAndRebuild
            else (liftIO . decodeFile $ cache_filepath) >>=
                \cached_digests -> if source_digest /= digestOfSourceFile cached_digests
                    then rescanAndRebuild
                    else let (build_dependency_resource_ids, previous_build_dependency_resource_digests) =
                                unzip . digestOfDependentModules $ cached_digests
                             link_dependency_resource_ids = resourceIdsOfLinkDependencies cached_digests
                         in getResourceDigests build_dependency_resource_ids >>=
                            \build_dependency_resource_digests ->
                                let rebuildIt = rebuild build_dependency_resource_ids build_dependency_resource_digests link_dependency_resource_ids
                                in if build_dependency_resource_digests /= previous_build_dependency_resource_digests
                                    then rebuildIt
                                    else liftIO (mapM doesFileExist product_filepaths) >>=
                                        \files_exist -> if (any not) files_exist
                                            then rebuildIt
                                            else if miscellaneous_cache_information /= cachedMiscellaneousInformation cached_digests
                                                then rebuildIt
                                                else return (digestsOfProducedFiles cached_digests, link_dependency_resource_ids)
  where
    source_digest = (fromRight . resourceDigest) source_resource

    getResourceDigests :: [ResourceId] -> ErrorT ErrorMessage IO [MD5Digest]
    getResourceDigests = ErrorT . return .
        attemptGetResourceDigests
            ("fetching the resources associated with the implicit dependencies of " ++ resourceName source_resource)
            resources

    getLinkResources :: [ResourceId] -> ErrorT ErrorMessage IO [Resource]
    getLinkResources = ErrorT . return .
        attemptGetResources
            ("fetching the resources associated with the link dependencies of " ++ resourceName source_resource)
            resources

    rescanAndRebuild =
        scanner
        >>=
        \(build_dependency_resource_ids,link_dependency_resource_ids) ->
            getResourceDigests build_dependency_resource_ids
            >>=
            \build_dependency_resource_digests ->
                rebuild build_dependency_resource_ids build_dependency_resource_digests link_dependency_resource_ids

    rebuild build_dependency_resource_ids build_dependency_resource_digests link_dependency_resource_ids =
        builder
        >>
        let product_digests = map digestOf product_filepaths
            cached_dependencies = CachedImplicitDependencies
                {   digestOfSourceFile = source_digest
                ,   digestsOfProducedFiles = product_digests
                ,   digestOfDependentModules =
                        zip build_dependency_resource_ids
                            build_dependency_resource_digests
                ,   resourceIdsOfLinkDependencies = link_dependency_resource_ids
                ,   cachedMiscellaneousInformation = miscellaneous_cache_information
                }
        in  liftIO $ do
                createDirectoryIfMissing True . takeDirectory $ cache_filepath
                encodeFile cache_filepath cached_dependencies
            >>
            return (product_digests,link_dependency_resource_ids)
-- @-node:gcross.20091122100142.1322:analyzeImplicitDependenciesAndRebuildIfNecessary_
-- @+node:gcross.20091214124713.1584:analyzeImplicitDependenciesAndRebuildIfNecessary
analyzeImplicitDependenciesAndRebuildIfNecessary ::
    (Binary a, Eq a) =>
    Builder ->
    Scanner ->
    Resources ->
    FilePath ->
    [FilePath] ->
    a ->
    Resource ->
    ([ErrorMessageOr MD5Digest],ErrorMessageOr (Set Resource))

analyzeImplicitDependenciesAndRebuildIfNecessary
    builder
    scanner
    resources
    cache_filepath
    product_filepaths
    miscellaneous_cache_information
    source_resource
    =
    either
        (repeat . Left &&& Left)
        (
            map Right
            ***
            (attemptGetResources
                ("fetching resources with the following ids that are link dependencies of "
                    ++ (show.resourceId) source_resource
                )
                resources
             >=>
             return . Set.fromList
            )
        )
    $
    analyzeImplicitDependenciesAndRebuildIfNecessary_
        builder
        scanner
        resources
        cache_filepath
        product_filepaths
        miscellaneous_cache_information
        source_resource
-- @-node:gcross.20091214124713.1584:analyzeImplicitDependenciesAndRebuildIfNecessary
-- @-node:gcross.20091122100142.1317:Function
-- @-others
-- @-node:gcross.20091122100142.1310:@thin ImplicitDependencies.hs
-- @-leo
