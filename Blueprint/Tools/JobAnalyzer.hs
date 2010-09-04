-- @+leo-ver=4-thin
-- @+node:gcross.20100705185804.2032:@thin JobAnalyzer.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100705185804.2033:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100705185804.2033:<< Language extensions >>
-- @nl

module Blueprint.Tools.JobAnalyzer where

-- @<< Import needed modules >>
-- @+node:gcross.20100705185804.2034:<< Import needed modules >>
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS

import Data.Binary
import Data.DeriveTH
import Data.Digest.Pure.MD5
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import Data.Vec ((:.)(..))

import Blueprint.Record

import Blueprint.Dependency
import Blueprint.Fields.DeferredDependencies
import Blueprint.Fields.Digest
import Blueprint.Jobs
import Blueprint.Tools
-- @nonl
-- @-node:gcross.20100705185804.2034:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100705185804.2035:Types
-- @+node:gcross.20100902134026.2095:IncompleteJobAnalyzer
type IncompleteJobAnalyzer α β = α → JobAnalyzer β
-- @nonl
-- @-node:gcross.20100902134026.2095:IncompleteJobAnalyzer
-- @+node:gcross.20100705185804.2036:JobAnalyzer
type JobAnalyzer = RWST SerializableRecord SerializableRecord () (JobTask JobId Record)
-- @-node:gcross.20100705185804.2036:JobAnalyzer
-- @-node:gcross.20100705185804.2035:Types
-- @+node:gcross.20100705185804.2037:Functions
-- @+node:gcross.20100705185804.2038:analyzeImplicitDependenciesAndRebuildIfNecessary
analyzeImplicitDependenciesAndRebuildIfNecessary ::
    (Typeable α, Binary α, Eq α, Typeable β, Binary β, Eq β) ⇒
    JobTask JobId Record β →
    (β → JobTask JobId Record RequiredDependencies) →
    ([Dependency] → JobTask JobId Record [MD5Digest]) →
    ([MD5Digest] → JobTask JobId Record Bool) →
    α →
    [JobId] →
    JobAnalyzer [Record]
analyzeImplicitDependenciesAndRebuildIfNecessary
    scanner
    resolve
    builder
    checkIfProductsMatch
    miscellaneous_information
    source_dependency_job_ids
 = do
    -- See whethether the sources have changed
    sources_have_changed ←
        fetchDigestsAndCheckForChanges
            source_digests_field
            source_dependency_job_ids

    -- Determine what the implicit dependencies are
    dependencies ←  
        if sources_have_changed
            then runTaskAndCacheResult implicit_dependencies_field scanner
            else readRequiredAndCache implicit_dependencies_field

    -- Resolve all dependencies, and fetch the relevent digests
    required_dependencies@RequiredDependencies{..} ←
        lift
        .
        resolve
        $
        dependencies
    dependency_digests ←
        lift
        .
        fetchDigestsFor
        .
        catMaybes
        .
        map snd
        $
        requiredImmediateDependencies

    -- Check whether the dependencies have changed
    dependencies_have_changed ←
        if sources_have_changed
            then do
                writeToCache dependencies_and_digests_field (dependencies,dependency_digests)
                return True
            else
                checkForChangesIn
                    dependencies_and_digests_field
                    (dependencies,dependency_digests)

    -- Check whether the miscellaneous information has changed
    miscellaneous_information_has_changed ←
        checkForChangesInMiscellaneousInformation miscellaneous_information

    -- Rebuild the product if necessary
    product_digests ←
        rebuildProductsIfNecessary
            (builder . map fst $ requiredImmediateDependencies)
            checkIfProductsMatch
            (dependencies_have_changed || miscellaneous_information_has_changed)

    -- Return the results
    return
        .
        map (\digest →
            withFields (
                (_digest,digest)
             :. (_deferred_dependencies,requiredDeferredDependencies)
             :. ()
            )
        )
        $
        product_digests
 where
    source_digests_field = field "source digests" "da0f7975-5565-43ba-a253-746d37cf5ca8"
    implicit_dependencies_field = field "implicit dependencies" "65b0972e-c17f-426b-a00d-8cc8cb52dc4b"
    dependencies_and_digests_field = field "dependencies and digests" "4292b8d0-5d15-49de-8032-92be8e67680f"
-- @nonl
-- @-node:gcross.20100705185804.2038:analyzeImplicitDependenciesAndRebuildIfNecessary
-- @+node:gcross.20100901221002.2070:analyzeExplicitDependenciesAndRebuildIfNecessary
analyzeExplicitDependenciesAndRebuildIfNecessary ::
    (Typeable α, Binary α, Eq α) ⇒
    JobTask JobId Record [MD5Digest] →
    ([MD5Digest] → JobTask JobId Record Bool) →
    α →
    [JobId] →
    JobAnalyzer [Record]
analyzeExplicitDependenciesAndRebuildIfNecessary
    builder
    checkIfProductsMatch
    miscellaneous_information
    dependency_job_ids
 = do
    -- See whethether the dependencies have changed
    dependencies_have_changed ←
        fetchDigestsAndCheckForChanges
            dependency_digests_field
            dependency_job_ids

    -- Check whether the miscellaneous information has changed
    miscellaneous_information_has_changed ←
        checkForChangesInMiscellaneousInformation miscellaneous_information

    -- Rebuild the product if necessary
    product_digests ←
        rebuildProductsIfNecessary
            builder
            checkIfProductsMatch
            (dependencies_have_changed || miscellaneous_information_has_changed)

    -- Return the results
    return
        .
        map (\digest →
            withFields (
                (_digest,digest)
             :. ()
            )
        )
        $
        product_digests
 where
    dependency_digests_field = field "depedency digests" "da0f7975-5565-43ba-a253-746d37cf5ca8"
-- @-node:gcross.20100901221002.2070:analyzeExplicitDependenciesAndRebuildIfNecessary
-- @+node:gcross.20100705185804.2045:checkForChangesIn
checkForChangesIn ::
    (Typeable a, Binary a, Eq a) ⇒
    Field a →
    a →
    JobAnalyzer Bool
checkForChangesIn field value = do
    writeToCache field value
    maybe_old_value ← asks (getField field)
    return $
        case maybe_old_value of
            Nothing → True
            Just old_value → value /= old_value
-- @-node:gcross.20100705185804.2045:checkForChangesIn
-- @+node:gcross.20100705185804.2041:checkForChangesInMiscellaneousInformation
checkForChangesInMiscellaneousInformation ::
    (Typeable a, Binary a, Eq a) ⇒
    a →
    JobAnalyzer Bool
checkForChangesInMiscellaneousInformation =
    checkForChangesIn miscellaneous_information_field
  where
    miscellaneous_information_field = field "miscellaneous information" "b77f1940-ac94-4a22-980f-e0f52c26af28"
-- @-node:gcross.20100705185804.2041:checkForChangesInMiscellaneousInformation
-- @+node:gcross.20100902134026.2120:compareToCacheAndRebuildIfNecessary
compareToCacheAndRebuildIfNecessary ::
    (Typeable α, Binary α, Eq α) ⇒
    JobTask JobId Record [MD5Digest] →
    ([MD5Digest] → JobTask JobId Record Bool) →
    α →
    JobAnalyzer [Record]
compareToCacheAndRebuildIfNecessary
    builder
    checkIfProductsMatch
    cached_information
 = do
    -- Check whether the miscellaneous information has changed
    cached_information_has_changed ←
        checkForChangesInMiscellaneousInformation cached_information

    -- Rebuild the product if necessary
    product_digests ←
        rebuildProductsIfNecessary
            builder
            checkIfProductsMatch
            cached_information_has_changed

    -- Return the results
    return
        .
        map (\digest →
            withFields (
                (_digest,digest)
             :. ()
            )
        )
        $
        product_digests
-- @-node:gcross.20100902134026.2120:compareToCacheAndRebuildIfNecessary
-- @+node:gcross.20100705185804.2044:fetchDigestsAndCheckForChanges
fetchDigestsAndCheckForChanges ::
    Field [MD5Digest] →
    [JobId] →
    JobAnalyzer Bool
fetchDigestsAndCheckForChanges digest_field job_ids = do
    digests ←
        runTaskAndCacheResult digest_field
        .
        fetchDigestsFor
        $
        job_ids
    checkForChangesIn
        digest_field
        digests
-- @-node:gcross.20100705185804.2044:fetchDigestsAndCheckForChanges
-- @+node:gcross.20100705185804.2042:readAndCache
readAndCache ::
    (Typeable a, Binary a) ⇒
    Field a →
    JobAnalyzer (Maybe a)
readAndCache field = do
    maybe_value ← asks . getField $ field
    case maybe_value of
        Nothing → return ()
        Just value → writeToCache field value
    return maybe_value
-- @-node:gcross.20100705185804.2042:readAndCache
-- @+node:gcross.20100705185804.2050:readRequiredAndCache
readRequiredAndCache ::
    (Typeable a, Binary a) ⇒
    Field a →
    JobAnalyzer a
readRequiredAndCache field = do
    value ← asks . getRequiredField $ field
    writeToCache field value
    return value
-- @-node:gcross.20100705185804.2050:readRequiredAndCache
-- @+node:gcross.20100705185804.2040:rebuildProductsIfNecessary
rebuildProductsIfNecessary ::
    ToolJobTask [MD5Digest] →
    ([MD5Digest] → ToolJobTask Bool) →
    Bool →
    JobAnalyzer [MD5Digest]
rebuildProductsIfNecessary
    builder
    checkIfProductsMatch
    always_rebuild
  = (if always_rebuild
        then return True
        else do
            maybe_old_digests ← asks (getField product_digests_field)
            case maybe_old_digests of
                Nothing → return True
                Just old_digests → fmap not . lift . checkIfProductsMatch $ old_digests
    )
    >>=
    rerunTaskAndCacheResultOnlyIf
        product_digests_field
        builder
  where
    product_digests_field = field "product digests" "101ba0a4-35a9-44f7-98c0-87d26027a375"
-- @-node:gcross.20100705185804.2040:rebuildProductsIfNecessary
-- @+node:gcross.20100705185804.2047:rerunTaskAndCacheResultOnlyIf
rerunTaskAndCacheResultOnlyIf ::
    (Typeable a, Binary a) ⇒
    Field a →
    ToolJobTask a →
    Bool →
    JobAnalyzer a
rerunTaskAndCacheResultOnlyIf field _ False = readRequiredAndCache field
rerunTaskAndCacheResultOnlyIf field action True = do
    result ← lift action
    writeToCache field result
    return result
-- @-node:gcross.20100705185804.2047:rerunTaskAndCacheResultOnlyIf
-- @+node:gcross.20100705185804.2048:runJobAnalyzer
runJobAnalyzer analyzer maybe_cache = do
    (results,(),cache) ← runRWST analyzer (fromMaybe emptyTable maybe_cache) ()
    returnValuesAndCache results cache
-- @-node:gcross.20100705185804.2048:runJobAnalyzer
-- @+node:gcross.20100705185804.2046:runTaskAndCacheResult
runTaskAndCacheResult ::
    (Typeable a, Binary a) ⇒
    Field a →
    ToolJobTask a →
    JobAnalyzer a
runTaskAndCacheResult field action = do
    result ← lift action
    tell (withField field result)
    return result
-- @-node:gcross.20100705185804.2046:runTaskAndCacheResult
-- @+node:gcross.20100705185804.2043:writeToCache
writeToCache ::
    (Typeable a, Binary a) ⇒
    Field a →
    a →
    JobAnalyzer ()
writeToCache field value = tell (withField field value)
-- @-node:gcross.20100705185804.2043:writeToCache
-- @-node:gcross.20100705185804.2037:Functions
-- @-others
-- @-node:gcross.20100705185804.2032:@thin JobAnalyzer.hs
-- @-leo
