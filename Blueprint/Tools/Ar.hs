-- @+leo-ver=4-thin
-- @+node:gcross.20100906112631.1936:@thin Ar.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100906112631.1937:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100906112631.1937:<< Language extensions >>
-- @nl

module Blueprint.Tools.Ar where

-- @<< Import needed modules >>
-- @+node:gcross.20100906112631.1938:<< Import needed modules >>
import Control.Arrow
import Control.Applicative
import Control.Monad.IO.Class

import Data.Binary
import Data.DeriveTH
import Data.Digest.Pure.MD5
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable

import System.Log.Logger

import Blueprint.Dependency
import Blueprint.Fields.DeferredDependencies
import Blueprint.Fields.FilePath
import Blueprint.Identifier
import Blueprint.Jobs
import Blueprint.Jobs.Combinators
import Blueprint.Miscellaneous
import Blueprint.Record
import Blueprint.Tools
import Blueprint.Tools.JobAnalyzer
-- @nonl
-- @-node:gcross.20100906112631.1938:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100906112631.1942:Types
-- @+node:gcross.20100906112631.1952:ArchiveComponents
data ArchiveComponents = ArchiveComponents
    {   archiveComponentObjectFilePaths :: [FilePath]
    ,   archiveComponentObjectDigests :: [MD5Digest]
    ,   archiveComponentDeferredDependencies :: [Dependency]
    } deriving (Eq, Typeable)

$(derive makeBinary ''ArchiveComponents)
-- @-node:gcross.20100906112631.1952:ArchiveComponents
-- @+node:gcross.20100906112631.1945:BuiltArchive
data BuiltArchive = BuiltArchive
    {   builtArchiveFilePath :: FilePath
    ,   builtArchiveJobId :: JobId
    }
-- @-node:gcross.20100906112631.1945:BuiltArchive
-- @-node:gcross.20100906112631.1942:Types
-- @+node:gcross.20100906112631.1939:Functions
-- @+node:gcross.20100906112631.1951:archiveJobId
archiveJobId :: FilePath → String → JobId
archiveJobId = identifierInNamespace archive_namespace
-- @-node:gcross.20100906112631.1951:archiveJobId
-- @+node:gcross.20100906112631.1949:builtArchive
builtArchive :: FilePath → BuiltArchive
builtArchive archive_filepath =
    BuiltArchive
        archive_filepath
        (archiveJobId archive_filepath ("Building archive " ++ archive_filepath))
-- @-node:gcross.20100906112631.1949:builtArchive
-- @+node:gcross.20100906112631.1941:createArMakeArchiveIncompleteJob
createArMakeArchiveIncompleteJob ::
    FilePath →
    [String] →
    BuiltArchive →
    IncompleteToolJob ArchiveComponents
createArMakeArchiveIncompleteJob
    path_to_ar
    options_arguments
    BuiltArchive{..}
    =
    incompleteJobWithCache [builtArchiveJobId]
    $
    \archive_components@ArchiveComponents{..} →
        let ar_arguments = "cqs":archiveComponentObjectFilePaths
            builder = liftIO $ do
                noticeM "Blueprint.Tools.Ar" $
                    "(GHC) Creating archive "
                    ++ builtArchiveFilePath
                infoM "Blueprint.Tools.Ar" $
                    "(GHC) Executing '" ++ (unwords (path_to_ar:ar_arguments)) ++ "'"
                runProductionCommandAndDigestOutputs
                    [builtArchiveFilePath]
                    []
                    path_to_ar
                    ar_arguments
        in  runJobAnalyzer
            .
            fmap (
                zipWith ($)
                .
                (:[])
                $
                (
                    setDeferredDependencies archiveComponentDeferredDependencies
                    .
                    setFilePath builtArchiveFilePath
                )
            )
            $
            compareToCacheAndRebuildIfNecessary
                builder
                (liftIO . checkDigestsOfFilesIfExisting [builtArchiveFilePath])
                (archiveComponentObjectFilePaths,archiveComponentObjectDigests)
-- @-node:gcross.20100906112631.1941:createArMakeArchiveIncompleteJob
-- @+node:gcross.20100906112631.1956:createArFetchDeferredDependencesAndMakeLibraryJobs
createArFetchDeferredDependencesAndMakeArchiveJobs ::
    FilePath →
    [String] →
    BuiltArchive →
    (FilePath → Maybe JobId) →
    [FilePath] →
    [ToolJob]
createArFetchDeferredDependencesAndMakeArchiveJobs
    path_to_ar
    options_arguments
    built_archive@BuiltArchive{..}
    lookupObjectJobId
    starting_objects
    =
    fmap
        (computeArchiveComponents . Map.toList)
        (fetchAllDeferredDependenciesAndTheirDigests
            builtArchiveFilePath
            (\Dependency{..} →
                if dependencyType == object_dependency_type
                    then lookupObjectJobId dependencyName
                    else Nothing
            )
            (map objectDependency starting_objects)
        )
    ➠
    [createArMakeArchiveIncompleteJob
        path_to_ar
        options_arguments
        built_archive
    ]
-- @-node:gcross.20100906112631.1956:createArFetchDeferredDependencesAndMakeLibraryJobs
-- @+node:gcross.20100906112631.1957:computeArchiveComponents
computeArchiveComponents :: [(Dependency,Maybe MD5Digest)] → ArchiveComponents
computeArchiveComponents =
    (liftA3 ArchiveComponents
        (fst . fst)
        (snd . fst)
        snd
    )
    .
    ((map dependencyName *** catMaybes) . unzip *** map fst)
    .
    span ((== object_dependency_type) . dependencyType . fst)
-- @-node:gcross.20100906112631.1957:computeArchiveComponents
-- @-node:gcross.20100906112631.1939:Functions
-- @+node:gcross.20100906112631.1947:Namespaces
archive_namespace = uuid "a21dbf30-4e1b-4d1a-a9a5-af0658570a65"
-- @-node:gcross.20100906112631.1947:Namespaces
-- @-others
-- @-node:gcross.20100906112631.1936:@thin Ar.hs
-- @-leo
