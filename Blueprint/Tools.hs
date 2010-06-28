-- @+leo-ver=4-thin
-- @+node:gcross.20100624100717.2132:@thin Tools.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100624100717.2133:<< Language extensions >>
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100624100717.2133:<< Language extensions >>
-- @nl

module Blueprint.Tools where

-- @<< Import needed modules >>
-- @+node:gcross.20100624100717.2134:<< Import needed modules >>
import Control.Monad
import Control.Monad.Goto
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Parallel.Strategies

import Data.Binary
import Data.DeriveTH
import Data.Digest.Pure.MD5
import Data.Record
import Data.Vec ((:.)(..))

import System.Directory
import System.IO

import Blueprint.Dependency
import Blueprint.Fields.DeferredDependencies
import Blueprint.Fields.Digest
import Blueprint.Fields.FilePath
import Blueprint.Miscellaneous
import Blueprint.Jobs
-- @-node:gcross.20100624100717.2134:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100624100717.2146:Types
-- @+node:gcross.20100624100717.2148:CachedDependencies
data CachedDependencies = CachedDependencies
        {   cachedSourceDigests :: [MD5Digest]
        ,   cachedExplicitDependencies :: [UnresolvedDependency]
        ,   cachedImplicitDependencies :: [UnresolvedDependency]
        ,   cachedDependencyDigests :: [MD5Digest]
        ,   cachedDeferredDependencies :: [Dependency]
        ,   cachedProductDigests :: [MD5Digest]
        };  $( derive makeBinary ''CachedDependencies )
-- @-node:gcross.20100624100717.2148:CachedDependencies
-- @-node:gcross.20100624100717.2146:Types
-- @+node:gcross.20100624100717.2135:Functions
-- @+node:gcross.20100624100717.2137:analyzeDependenciesAndRebuildIfNecessary
analyzeDependenciesAndRebuildIfNecessary ::
    (Binary a, Eq a) ⇒
    JobTask JobId Record [UnresolvedDependency] →
    JobTask JobId Record () →
    DependencyResolver →
    a →
    [JobId] →
    [UnresolvedDependency] →
    [FilePath] →
    JobRunner JobId Record (CachedDependencies,a)
analyzeDependenciesAndRebuildIfNecessary
    scanner
    builder
    resolveDependency
    miscellaneous_information
    source_dependency_job_ids
    explicit_dependencies
    product_file_paths
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
                ,deferred_dependencies == cachedDeferredDependencies
                ,dependency_digests == cachedDependencyDigests
                ,miscellaneous_information == cached_miscellaneous_information
                ]
            ) $ rebuildIt

        -- Check whether the product files still exist
        (fmap and
         .
         liftIO
         .
         mapM doesFileExist
         $
         product_file_paths
         ) >>= flip unless rebuildIt

        -- Check whether the products have been corrupted
        fmap (== cachedProductDigests) (lift digestProducts)
           >>= flip unless rebuildIt

        -- If we reach here, then nothing more needs to be done. 
        lift (declareVictory cache)

  where
    digestSources =
        fmap (map getDigest)
        .
        request
        $
        source_dependency_job_ids

    resolveAllDependencies implicit_dependencies = do
        let ResolvedDependencies{..} =
                resolveAndExtractAndConcatenateDependenciesUsing resolveDependency
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


    digestProducts =
        fmap (withStrategy (parList rwhnf))
        .
        liftIO
        .
        digestFiles
        $
        product_file_paths

    rescanAndRebuild source_digests = do
        implicit_dependencies ← scanner
        (dependency_digests,deferred_dependencies) ← resolveAllDependencies implicit_dependencies
        rebuild
            source_digests
            implicit_dependencies
            dependency_digests
            deferred_dependencies

    rebuild source_digests implicit_dependencies dependency_digests deferred_dependencies = do
        builder
        product_digests ← digestProducts
        declareVictory
            CachedDependencies
            {   cachedSourceDigests = source_digests
            ,   cachedExplicitDependencies = explicit_dependencies
            ,   cachedImplicitDependencies = implicit_dependencies
            ,   cachedDependencyDigests = dependency_digests
            ,   cachedDeferredDependencies = deferred_dependencies
            ,   cachedProductDigests = product_digests
            }

    declareVictory (cache@CachedDependencies{..}) =
        let values =
                zipWith (\file_path digest →
                    withFields (
                        (_file_path,file_path)
                     :. (_digest,digest)
                     :. (_deferred_dependencies,cachedDeferredDependencies)
                     :. ()
                    )
                ) product_file_paths cachedProductDigests
        in returnValuesAndCache values (cache,miscellaneous_information)
-- @-node:gcross.20100624100717.2137:analyzeDependenciesAndRebuildIfNecessary
-- @-node:gcross.20100624100717.2135:Functions
-- @-others
-- @-node:gcross.20100624100717.2132:@thin Tools.hs
-- @-leo
