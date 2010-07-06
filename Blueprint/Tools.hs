-- @+leo-ver=4-thin
-- @+node:gcross.20100624100717.2132:@thin Tools.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100624100717.2133:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ParallelListComp #-}
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
import Control.Parallel.Strategies

import Data.Digest.Pure.MD5
import Data.Either
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Record
import Data.Typeable

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
-- @+node:gcross.20100705185804.1971:ToolJobTask
type ToolJobTask result = JobTask JobId Record result
-- @-node:gcross.20100705185804.1971:ToolJobTask
-- @+node:gcross.20100705185804.2005:ToolJobRunner
type ToolJobRunner cache = JobRunner JobId Record cache

-- @-node:gcross.20100705185804.2005:ToolJobRunner
-- @-node:gcross.20100624100717.2146:Types
-- @+node:gcross.20100624100717.2135:Functions
-- @+node:gcross.20100705185804.1978:fetchDigestsFor
fetchDigestsFor :: [JobId] → ToolJobTask [MD5Digest]
fetchDigestsFor = fmap (map getDigest) . request
-- @-node:gcross.20100705185804.1978:fetchDigestsFor
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
-- @+node:gcross.20100705185804.1961:fetchAllDeferredDependencies
fetchAllDeferredDependencies ::
    ([Dependency] → [(Dependency,Maybe JobId)]) →
    [Dependency] →
    ToolJobTask (Map Dependency (Maybe MD5Digest))
fetchAllDeferredDependencies lookupDependencyJobIds =
    go Map.empty
  where
    go all_dependencies [] = return all_dependencies
    go seen_dependencies additional_dependencies = do
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
        go new_seen_dependencies new_additional_dependencies
-- @nonl
-- @-node:gcross.20100705185804.1961:fetchAllDeferredDependencies
-- @-node:gcross.20100624100717.2135:Functions
-- @+node:gcross.20100630111926.1884:Dependency Types
runtime_dependency_type = identifier "a4d4ac42-4ae6-4afd-90b1-9984589b5360" "run-time"
object_dependency_type = identifier "b14f6d22-7d47-48ff-887f-d17cff428f22" "object"
library_dependency_type = identifier "946cec33-e3ba-42da-a4c7-bced83710e9f" "library"
-- @-node:gcross.20100630111926.1884:Dependency Types
-- @-others
-- @-node:gcross.20100624100717.2132:@thin Tools.hs
-- @-leo
