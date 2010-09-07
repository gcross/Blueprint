-- @+leo-ver=4-thin
-- @+node:gcross.20100906112631.1936:@thin Ar.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100906112631.1937:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
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

import Blueprint.Configuration.Tools
import Blueprint.Dependency
import Blueprint.Dependency.File.Object
import Blueprint.Fields.DeferredDependencies
import Blueprint.Fields.FilePath
import Blueprint.Identifier
import Blueprint.Jobs
import Blueprint.Jobs.Combinators
import Blueprint.Miscellaneous
import Blueprint.Product
import Blueprint.Record
import Blueprint.Tools
import Blueprint.Tools.JobAnalyzer
-- @nonl
-- @-node:gcross.20100906112631.1938:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100906112631.2156:Program
data Ar deriving Typeable; instance ProgramName Ar where { programNameFrom _ = "ar" }
-- @-node:gcross.20100906112631.2156:Program
-- @+node:gcross.20100906112631.2158:Options
arOptions = unwrapOptions (programOptions :: OptionsFor Ar)
-- @-node:gcross.20100906112631.2158:Options
-- @+node:gcross.20100906112631.2160:Products
createProductDeclarations "3b4bfe07-3388-4a73-915b-09afca271efb" "archive"
-- @-node:gcross.20100906112631.2160:Products
-- @+node:gcross.20100906112631.1942:Types
-- @+node:gcross.20100906112631.1952:ArchiveComponents
data ArchiveComponents = ArchiveComponents
    {   archiveComponentObjectFilePaths :: [FilePath]
    ,   archiveComponentObjectDigests :: [MD5Digest]
    ,   archiveComponentDeferredDependencies :: [Dependency]
    } deriving (Eq, Typeable)

$(derive makeBinary ''ArchiveComponents)
-- @-node:gcross.20100906112631.1952:ArchiveComponents
-- @-node:gcross.20100906112631.1942:Types
-- @+node:gcross.20100906112631.1939:Functions
-- @+node:gcross.20100906112631.1941:createArMakeArchiveIncompleteJob
createArMakeArchiveIncompleteJob ::
    ProgramConfiguration Ar →
    BuiltProduct Archive →
    IncompleteToolJob ArchiveComponents
createArMakeArchiveIncompleteJob
    ProgramConfiguration{..}
    BuiltProduct{..}
    =
    incompleteJobWithCache [builtProductJobId]
    $
    \archive_components@ArchiveComponents{..} →
        let ar_arguments = "cqs":builtProductName:(archiveComponentObjectFilePaths ++ programExtraArguments)
            builder = liftIO $ do
                noticeM "Blueprint.Tools.Ar" $
                    "(GHC) Creating archive "
                    ++ builtProductName
                infoM "Blueprint.Tools.Ar" $
                    "(GHC) Executing '" ++ (unwords (programFilePath:ar_arguments)) ++ "'"
                runProductionCommandAndDigestOutputs
                    [builtProductName]
                    []
                    programFilePath
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
                    setFilePath builtProductName
                )
            )
            $
            compareToCacheAndRebuildIfNecessary
                builder
                (liftIO . checkDigestsOfFilesIfExisting [builtProductName])
                (archiveComponentObjectFilePaths,archiveComponentObjectDigests)
-- @nonl
-- @-node:gcross.20100906112631.1941:createArMakeArchiveIncompleteJob
-- @+node:gcross.20100906112631.1956:createArFetchDeferredDependencesAndMakeLibraryJobs
createArFetchDeferredDependencesAndMakeArchiveJobs ::
    ProgramConfiguration Ar →
    BuiltProduct Archive →
    (FilePath → Maybe JobId) →
    [FilePath] →
    [ToolJob]
createArFetchDeferredDependencesAndMakeArchiveJobs
    program_configuration
    built_archive@BuiltProduct{..}
    lookupObjectJobId
    starting_objects
    =
    fmap
        (computeArchiveComponents . Map.toList)
        (fetchAllDeferredDependenciesAndTheirDigests
            builtProductName
            (\Dependency{..} →
                if dependencyType == object_dependency_type
                    then lookupObjectJobId dependencyName
                    else Nothing
            )
            (map objectDependency starting_objects)
        )
    ➠
    [createArMakeArchiveIncompleteJob
        program_configuration
        built_archive
    ]
-- @nonl
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
-- @-others
-- @-node:gcross.20100906112631.1936:@thin Ar.hs
-- @-leo
