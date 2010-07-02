-- @+leo-ver=4-thin
-- @+node:gcross.20100624100717.2132:@thin Tools.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100624100717.2133:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100624100717.2133:<< Language extensions >>
-- @nl

module Blueprint.Tools where

-- @<< Import needed modules >>
-- @+node:gcross.20100624100717.2134:<< Import needed modules >>
import Control.Exception
import Control.Monad
import Control.Monad.Goto
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Parallel.Strategies

import Data.Binary
import Data.DeriveTH
import Data.Digest.Pure.MD5
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
-- @-node:gcross.20100630111926.1894:Exceptions
-- @+node:gcross.20100624100717.2146:Types
-- @+node:gcross.20100624100717.2148:CachedDependencies
data CachedDependencies = CachedDependencies
        {   cachedSourceDigests :: [MD5Digest]
        ,   cachedExplicitDependencies :: [UnresolvedDependency]
        ,   cachedImplicitDependencies :: [UnresolvedDependency]
        ,   cachedDependencyDigests :: [MD5Digest]
        ,   cachedProductDigests :: [MD5Digest]
        } deriving (Show,Eq);  $( derive makeBinary ''CachedDependencies )
-- @-node:gcross.20100624100717.2148:CachedDependencies
-- @+node:gcross.20100630111926.1896:AnalyzeAndRebuildJobRunner
type AnalyzeAndRebuildJobRunner a = JobRunner JobId Record (CachedDependencies,a)
-- @-node:gcross.20100630111926.1896:AnalyzeAndRebuildJobRunner
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
    digestSources =
        fmap (map getDigest)
        .
        request
        $
        source_dependency_job_ids

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

    rescanAndRebuild source_digests = do
        implicit_dependencies ← scanner
        (dependency_digests,deferred_dependencies) ← resolveAllDependencies implicit_dependencies
        rebuild
            source_digests
            implicit_dependencies
            dependency_digests
            deferred_dependencies

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
-- @-node:gcross.20100624100717.2137:analyzeDependenciesAndRebuildIfNecessary
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
-- @+node:gcross.20100630111926.1885:runtime_dependency_type
runtime_dependency_type = identifier "a4d4ac42-4ae6-4afd-90b1-9984589b5360" "run-time"
-- @-node:gcross.20100630111926.1885:runtime_dependency_type
-- @-node:gcross.20100630111926.1884:Dependency Types
-- @-others
-- @-node:gcross.20100624100717.2132:@thin Tools.hs
-- @-leo
