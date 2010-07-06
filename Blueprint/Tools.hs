-- @+leo-ver=4-thin
-- @+node:gcross.20100624100717.2132:@thin Tools.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100624100717.2133:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100624100717.2133:<< Language extensions >>
-- @nl

module Blueprint.Tools where

-- @<< Import needed modules >>
-- @+node:gcross.20100624100717.2134:<< Import needed modules >>
import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Goto
import Control.Parallel.Strategies

import Data.Binary
import Data.DeriveTH
import Data.Digest.Pure.MD5
import Data.Either
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Record
import Data.Typeable
import Data.Vec ((:.)(..))

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

import Blueprint.Dependency
import Blueprint.Fields.DeferredDependencies
import Blueprint.Fields.Digest
import Blueprint.Identifier
import Blueprint.Miscellaneous
import Blueprint.Jobs
-- @-node:gcross.20100624100717.2134:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100630111926.1894:Exceptions
-- @+node:gcross.20100630111926.1895:ProductionError
data ProductionError =
    ProductionCommandFailed String String
  | FailedToProduceMandatoryOutputs [FilePath]
  deriving (Typeable)

instance Show ProductionError where
    show (ProductionCommandFailed production_command error_message) =
        "Error executing command '" ++ production_command ++ "':\n"
        ++
        error_message
    show (FailedToProduceMandatoryOutputs non_existing_mandatory_outputs) =
        "The following output files were expected but not produced: " ++ show non_existing_mandatory_outputs

instance Exception ProductionError
-- @-node:gcross.20100630111926.1895:ProductionError
-- @+node:gcross.20100705150931.1953:UnknownObjects
data UnknownObjects = UnknownObjects [String] deriving Typeable

instance Show UnknownObjects where
    show (UnknownObjects unknown_objects) =
        ("The following objects are unrecognized:" ++)
        .
        intercalate "\n\t"
        $
        unknown_objects

instance Exception UnknownObjects
-- @-node:gcross.20100705150931.1953:UnknownObjects
-- @+node:gcross.20100705150931.1955:UnknownRuntimes
data UnrecognizedRuntimes = UnrecognizedRuntimes String [String] deriving Typeable

instance Show UnrecognizedRuntimes where
    show (UnrecognizedRuntimes actor_name unrecognized_runtimes) =
        (("The following run-times are unrecognized by " ++ actor_name) ++)
        .
        intercalate "\n\t"
        $
        unrecognized_runtimes

instance Exception UnrecognizedRuntimes
-- @-node:gcross.20100705150931.1955:UnknownRuntimes
-- @-node:gcross.20100630111926.1894:Exceptions
-- @+node:gcross.20100624100717.2146:Types
-- @+node:gcross.20100630111926.1896:AnalyzeAndRebuildJobRunner
type AnalyzeAndRebuildJobRunner a = JobRunner JobId Record (CachedDependencies,a)
-- @-node:gcross.20100630111926.1896:AnalyzeAndRebuildJobRunner
-- @+node:gcross.20100705132935.1955:FetchAllDependenciesAndRebuildJobRunner
type FetchAllDependenciesAndRebuildJobRunner a = JobRunner JobId Record (Map Dependency (Maybe MD5Digest),[MD5Digest],a)
-- @-node:gcross.20100705132935.1955:FetchAllDependenciesAndRebuildJobRunner
-- @+node:gcross.20100624100717.2148:CachedDependencies
data CachedDependencies = CachedDependencies
        {   cachedSourceDigests :: [MD5Digest]
        ,   cachedExplicitDependencies :: [UnresolvedDependency]
        ,   cachedImplicitDependencies :: [UnresolvedDependency]
        ,   cachedDependencyDigests :: [MD5Digest]
        ,   cachedProductDigests :: [MD5Digest]
        } deriving (Show,Eq);  $( derive makeBinary ''CachedDependencies )
-- @-node:gcross.20100624100717.2148:CachedDependencies
-- @-node:gcross.20100624100717.2146:Types
-- @+node:gcross.20100624100717.2135:Functions
-- @+node:gcross.20100624100717.2137:analyzeDependenciesAndRebuildIfNecessary
analyzeDependenciesAndRebuildIfNecessary ::
    (Binary a, Eq a) ⇒
    JobTask JobId Record [UnresolvedDependency] →
    JobTask JobId Record [MD5Digest] →
    ([MD5Digest] → JobTask JobId Record Bool) →
    DependencyResolver →
    a →
    [JobId] →
    [UnresolvedDependency] →
    JobRunner JobId Record (CachedDependencies,a)
analyzeDependenciesAndRebuildIfNecessary
    scanner
    builder
    checkProducts
    resolveDependency
    miscellaneous_information
    source_dependency_job_ids
    explicit_dependencies
    maybe_cache
 = case maybe_cache of
    Nothing → digestSources >>= rescanAndRebuild
    Just (cache@CachedDependencies{..},cached_miscellaneous_information) → runGotoT $ do
        -- Check whether the source file has changed
        source_digests ← lift digestSources
        unless (source_digests == cachedSourceDigests) $
            goto
            .
            lift
            .
            rescanAndRebuild
            $
            source_digests

        -- Resolve all dependencies
        (dependency_digests,deferred_dependencies) ← lift . resolveAllDependencies $ cachedImplicitDependencies
        let rebuildIt =
                goto
                .
                lift
                $
                rebuild
                    cachedSourceDigests
                    cachedImplicitDependencies
                    dependency_digests
                    deferred_dependencies

        -- Check if anything else on which the build depends has changed
        unless (
            and [explicit_dependencies == cachedExplicitDependencies
                ,dependency_digests == cachedDependencyDigests
                ,miscellaneous_information == cached_miscellaneous_information
                ]
            ) $ rebuildIt

        -- Check whether the product files either don't exist or have been corrupted
        lift (checkProducts cachedProductDigests) >>= flip unless rebuildIt

        -- If we reach here, then nothing more needs to be done. 
        lift (declareVictory cache deferred_dependencies)

  where
    -- @    @+others
    -- @+node:gcross.20100705150931.1973:declareVictory
    declareVictory (cache@CachedDependencies{..}) deferred_dependencies =
        let values =
                map (\digest →
                    withFields (
                        (_digest,digest)
                     :. (_deferred_dependencies,deferred_dependencies)
                     :. ()
                    )
                )
                $
                cachedProductDigests
        in returnValuesAndCache values (cache,miscellaneous_information)
    -- @nonl
    -- @-node:gcross.20100705150931.1973:declareVictory
    -- @+node:gcross.20100705150931.1969:digestSources
    digestSources =
        fmap (map getDigest)
        .
        request
        $
        source_dependency_job_ids
    -- @nonl
    -- @-node:gcross.20100705150931.1969:digestSources
    -- @+node:gcross.20100705150931.1972:rebuild
    rebuild source_digests implicit_dependencies dependency_digests deferred_dependencies = do
        product_digests ← builder
        declareVictory
            CachedDependencies
            {   cachedSourceDigests = source_digests
            ,   cachedExplicitDependencies = explicit_dependencies
            ,   cachedImplicitDependencies = implicit_dependencies
            ,   cachedDependencyDigests = dependency_digests
            ,   cachedProductDigests = product_digests
            }
            deferred_dependencies
    -- @nonl
    -- @-node:gcross.20100705150931.1972:rebuild
    -- @+node:gcross.20100705150931.1971:rescanAndRebuild
    rescanAndRebuild source_digests = do
        implicit_dependencies ← scanner
        (dependency_digests,deferred_dependencies) ← resolveAllDependencies implicit_dependencies
        rebuild
            source_digests
            implicit_dependencies
            dependency_digests
            deferred_dependencies
    -- @nonl
    -- @-node:gcross.20100705150931.1971:rescanAndRebuild
    -- @+node:gcross.20100705150931.1970:resolveAllDependencies
    resolveAllDependencies implicit_dependencies = do
        ResolvedDependencies{..} ←
                fmap extractAndConcatenateDependencies
                .
                mapM resolveDependency
                .
                (explicit_dependencies ++)
                $
                implicit_dependencies
        dependency_digests ←
            fmap (map getDigest)
            .
            request
            $
            resolvedImmediateDependencies
        return (dependency_digests,resolvedDeferredDependencies)
    -- @nonl
    -- @-node:gcross.20100705150931.1970:resolveAllDependencies
    -- @-others

-- @-node:gcross.20100624100717.2137:analyzeDependenciesAndRebuildIfNecessary
-- @+node:gcross.20100705100731.1951:fetchAllDeferredDependenciesAndRebuildIfNecessary
fetchAllDeferredDependenciesAndRebuildIfNecessary ::
    (Binary a, Eq a) ⇒
    ([Dependency] → [(Dependency,Maybe JobId)]) →
    ([MD5Digest] → JobTask JobId Record Bool) →
    ([Dependency] → JobTask JobId Record [MD5Digest]) →
    a →
    [Dependency] →
    JobRunner JobId Record (Map Dependency (Maybe MD5Digest),[MD5Digest],a)
fetchAllDeferredDependenciesAndRebuildIfNecessary
    lookupDependencyJobIds
    checkProducts
    builder
    miscellaneous_information
    starting_dependencies
    maybe_cache
 = runGotoT $ do
    -- Scan recursively for all dependencies
    all_dependencies ← lift $ fetchAllDependencies Map.empty starting_dependencies
    let rebuildIt =
            goto
            .
            lift
            .
            rebuild
            $
            all_dependencies

    -- See if we have cached results from a previous build;  if not, then build the product.
    (cached_dependencies,cached_product_digests,cached_miscellaneous_information) ←
        maybe rebuildIt return maybe_cache

    -- Check if anything on which the build depends has changed
    unless (
        and [all_dependencies == cached_dependencies
            ,miscellaneous_information == cached_miscellaneous_information
            ]
        ) $ rebuildIt

    -- Check whether the product files either don't exist or have been corrupted
    lift (checkProducts cached_product_digests) >>= flip unless rebuildIt

    -- If we reach here, then nothing more needs to be done. 
    lift (declareVictory all_dependencies cached_product_digests)
  where
    -- @    @+others
    -- @+node:gcross.20100705150931.1977:declareVictory
    declareVictory dependencies product_digests =
        let values =
                map (\digest →
                    withFields (
                        (_digest,digest)
                     :. ()
                    )
                )
                $
                product_digests
        in returnValuesAndCache values (dependencies,product_digests,miscellaneous_information)
    -- @-node:gcross.20100705150931.1977:declareVictory
    -- @+node:gcross.20100705150931.1965:fetchAllDependencies
    fetchAllDependencies all_dependencies [] = return all_dependencies
    fetchAllDependencies seen_dependencies additional_dependencies = do
        let (new_dependencies_without_job_ids,(new_dependencies_with_job_ids,job_ids)) =
                second unzip
                .
                partitionEithers
                .
                map (\(dependency,maybe_job_id) →
                    case maybe_job_id of
                        Nothing → Left dependency
                        Just job_id → Right (dependency,job_id)
                )
                .
                lookupDependencyJobIds
                .
                filter (`Map.notMember` seen_dependencies)
                .
                nub
                $
                additional_dependencies
        results ← request job_ids
        let new_seen_dependencies =
                Map.unions
                    [seen_dependencies
                    ,Map.fromList
                        [ (dependency,Nothing)
                        | dependency ← new_dependencies_without_job_ids
                        ]
                    ,Map.fromList
                        [ (dependency,Just digest)
                        | dependency ← new_dependencies_with_job_ids
                        | digest ← map getDigest results
                        ]
                    ]
            new_additional_dependencies =
                concat
                .
                map getDeferredDependencies
                $
                results
        fetchAllDependencies new_seen_dependencies new_additional_dependencies
    -- @nonl
    -- @-node:gcross.20100705150931.1965:fetchAllDependencies
    -- @+node:gcross.20100705150931.1975:rebuild
    rebuild dependencies = builder (Map.keys dependencies) >>= declareVictory dependencies
    -- @-node:gcross.20100705150931.1975:rebuild
    -- @-others
-- @-node:gcross.20100705100731.1951:fetchAllDeferredDependenciesAndRebuildIfNecessary
-- @+node:gcross.20100630111926.1893:runProductionCommandAndDigestOutputs
runProductionCommandAndDigestOutputs ::
    [FilePath] →
    [FilePath] →
    String →
    [String] →
    IO [MD5Digest]
runProductionCommandAndDigestOutputs
    mandatory_product_filepaths
    optional_product_filepaths
    command
    arguments
  = do
    mapM_ (createDirectoryIfMissing True . takeDirectory) $
        mandatory_product_filepaths ++ optional_product_filepaths
    (exit_code,_,output) ←
        readProcessWithExitCode
            command
            arguments
            ""
    when (exit_code /= ExitSuccess) . throwIO $
        ProductionCommandFailed (unwords (command:arguments)) output
    mandatory_products_not_existing ←
        filterM (fmap not . doesFileExist) mandatory_product_filepaths
    when (not . null $ mandatory_products_not_existing) . throwIO $
        FailedToProduceMandatoryOutputs mandatory_products_not_existing
    existing_optional_products ← filterM doesFileExist optional_product_filepaths
    digestFiles (mandatory_product_filepaths ++ optional_product_filepaths)
-- @-node:gcross.20100630111926.1893:runProductionCommandAndDigestOutputs
-- @-node:gcross.20100624100717.2135:Functions
-- @+node:gcross.20100630111926.1884:Dependency Types
runtime_dependency_type = identifier "a4d4ac42-4ae6-4afd-90b1-9984589b5360" "run-time"
object_dependency_type = identifier "b14f6d22-7d47-48ff-887f-d17cff428f22" "object"
library_dependency_type = identifier "946cec33-e3ba-42da-a4c7-bced83710e9f" "library"
-- @-node:gcross.20100630111926.1884:Dependency Types
-- @-others
-- @-node:gcross.20100624100717.2132:@thin Tools.hs
-- @-leo
