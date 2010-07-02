-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1610:@thin GHC.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1611:<< Language extensions >>
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1611:<< Language extensions >>
-- @nl

module Blueprint.Tools.Compilers.GHC where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1612:<< Import needed modules >>
import Control.Exception
import Control.Monad.IO.Class

import qualified Data.ByteString.Lazy as L
import Data.Either.Unwrap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Record
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable

import System.Exit
import System.Log.Logger
import System.Process

import Text.Regex.PCRE
import Text.Regex.PCRE.String

import Blueprint.Configuration.Tools
import Blueprint.Dependency
import Blueprint.Fields.DeferredDependencies
import Blueprint.Identifier
import Blueprint.Jobs
import Blueprint.Language.Programming.Haskell
import Blueprint.Miscellaneous
import Blueprint.Tools
-- @-node:gcross.20100611224425.1612:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100630111926.1861:Types
-- @+node:gcross.20100630111926.1862:KnownModules
type KnownModules = Map String ResolvedDependencies
-- @-node:gcross.20100630111926.1862:KnownModules
-- @+node:gcross.20100630111926.1875:GHCOptions
newtype GHCOptions = GHCOptions { unwrapGHCOptions :: Record }
-- @nonl
-- @-node:gcross.20100630111926.1875:GHCOptions
-- @-node:gcross.20100630111926.1861:Types
-- @+node:gcross.20100628115452.1853:Functions
-- @+node:gcross.20100628115452.1859:compilation/linking arguments
-- @+node:gcross.20100628115452.1854:computeGHCCompileToObjectArguments
computeGHCCompileToObjectArguments :: GHCOptions → FilePath → FilePath → FilePath → [String]
computeGHCCompileToObjectArguments options =
    let options_arguments = computeGHCOptions options
    in \source_filepath object_filepath interface_filepath →
         "-c":source_filepath
        :"-o":object_filepath
        :"-ohi":interface_filepath
        :options_arguments
-- @-node:gcross.20100628115452.1854:computeGHCCompileToObjectArguments
-- @+node:gcross.20100628115452.1856:computeGHCCompileToProgramArguments
computeGHCCompileToProgramArguments :: GHCOptions → FilePath → FilePath → [String]
computeGHCCompileToProgramArguments options =
    let options_arguments = computeGHCOptions options
    in \source_filepath program_filepath →
         source_filepath
        :"-o":program_filepath
        :options_arguments
-- @-node:gcross.20100628115452.1856:computeGHCCompileToProgramArguments
-- @+node:gcross.20100628115452.1858:computeGHCLinkToProgramArguments
computeGHCLinkToProgramArguments :: GHCOptions → [FilePath] → FilePath → [String]
computeGHCLinkToProgramArguments options =
    let options_arguments = computeGHCOptions options
    in \object_filepaths program_filepath →
         object_filepaths
         ++
        ("-o":program_filepath
        :options_arguments
        )
-- @-node:gcross.20100628115452.1858:computeGHCLinkToProgramArguments
-- @-node:gcross.20100628115452.1859:compilation/linking arguments
-- @+node:gcross.20100628115452.1899:dependency options
-- @+node:gcross.20100628115452.1895:computeGHCLinkDependencyOptions
computeGHCLinkDependencyOptions :: [Dependency] → [String]
computeGHCLinkDependencyOptions dependencies =
    if Set.null unrecognized_dependency_types
        then throw $ UnrecognizedDependencyTypes "the GHC linker" (Set.toList unrecognized_dependency_types)
        else concat
             .
             map (\(bin_id,computeOptionsFrom) → (computeOptionsFrom . getBin bin_id) binned_dependencies)
             $
             [(haskell_package_dependency_type,computeGHCPackageDependencyOptions)
             ]
 where
    binned_dependencies = binDependencies dependencies
    recognized_dependency_types = Set.fromList [haskell_package_dependency_type]
    unrecognized_dependency_types = (Map.keysSet binned_dependencies) `Set.difference` recognized_dependency_types
-- @-node:gcross.20100628115452.1895:computeGHCLinkDependencyOptions
-- @+node:gcross.20100628115452.1900:computeGHCPackageDependencyOptions
computeGHCPackageDependencyOptions [] = []
computeGHCPackageDependencyOptions (package:rest_packages) =
    "-package":package:computeGHCPackageDependencyOptions rest_packages
-- @-node:gcross.20100628115452.1900:computeGHCPackageDependencyOptions
-- @-node:gcross.20100628115452.1899:dependency options
-- @+node:gcross.20100630111926.1860:dependency resolution
-- @+node:gcross.20100630111926.1863:resolveGHCModuleDependencies
resolveModuleDependency :: FilePath → KnownModules → DependencyResolver
resolveModuleDependency path_to_ghc_pkg known_modules UnresolvedDependency{..}
  | dependencyType == haskell_module_dependency_type
    = case Map.lookup dependencyName known_modules of
        Nothing → 
            liftIO (findPackagesExposingModule path_to_ghc_pkg dependencyName)
            >>=
            return
                .
                Left
                .
                UnknownDependency unresolvedDependency
                .
                Just
                .
                DependencyExporters haskell_package_dependency_type
  | dependencyType == runtime_dependency_type
    = return . Right $ ResolvedDependencies [] [unresolvedDependency]      
  | otherwise
    = throw $ UnrecognizedDependencyType "the GHC compiler" dependencyType
  where
    Dependency{..} = unresolvedDependency
-- @-node:gcross.20100630111926.1863:resolveGHCModuleDependencies
-- @+node:gcross.20100630111926.1868:findPackagesExposingModule
findPackagesExposingModule :: FilePath -> String -> IO [String]
findPackagesExposingModule path_to_ghc_pkg module_name =
    fmap words
    .
    readProcess path_to_ghc_pkg ["--simple-output","find-module",module_name]
    $
    ""
-- @-node:gcross.20100630111926.1868:findPackagesExposingModule
-- @-node:gcross.20100630111926.1860:dependency resolution
-- @+node:gcross.20100628115452.1860:options
-- @+node:gcross.20100628115452.1864:computeGHCOptions
computeGHCOptions =
    computeFieldOptions _deferred_dependencies computeGHCLinkDependencyOptions
    .
    unwrapGHCOptions
-- @-node:gcross.20100628115452.1864:computeGHCOptions
-- @+node:gcross.20100628115452.1901:computeFieldOptions
computeFieldOptions :: Typeable a => Field a → (a → [b]) → Record → [b]
computeFieldOptions field computeOptionsFromField =
    maybe [] computeOptionsFromField
    .
    getField field
-- @-node:gcross.20100628115452.1901:computeFieldOptions
-- @-node:gcross.20100628115452.1860:options
-- @+node:gcross.20100630111926.1869:package queries
-- @+node:gcross.20100630111926.1872:queryPackage
queryPackage :: FilePath → String → String → IO (Maybe [String])
queryPackage path_to_ghc_pkg field_name package_name =
    fmap (\exit_code →
        case exit_code of
            (ExitSuccess,response,_) → Just . filter (/= (field_name ++ ":")) . words $ response 
            _ → Nothing
    )
    $
    readProcessWithExitCode path_to_ghc_pkg ["field",package_name,field_name] ""
-- @-node:gcross.20100630111926.1872:queryPackage
-- @+node:gcross.20100630111926.1870:fetchPackageModules
fetchPackageModules :: FilePath -> String -> IO (Maybe [String])
fetchPackageModules path_to_ghc qualified_package_name =
    queryPackage path_to_ghc "exposed-modules" qualified_package_name
-- @-node:gcross.20100630111926.1870:fetchPackageModules
-- @-node:gcross.20100630111926.1869:package queries
-- @+node:gcross.20100630111926.1873:tasks
-- @+node:gcross.20100630111926.1874:createGHCCompileToObjectTask
createGHCCompileToObjectTask ::
    FilePath →
    FilePath →
    KnownModules →
    GHCOptions →
    FilePath →
    JobId →
    FilePath →
    JobId →
    FilePath →
    JobId →
    AnalyzeAndRebuildJobRunner [String]

createGHCCompileToObjectTask
    path_to_ghc
    path_to_ghc_pkg
    known_modules
    ghc_options
    source_file_path
    source_job_id
    object_file_path
    object_job_id
    interface_file_path
    interface_job_id
    = 
    analyzeDependenciesAndRebuildIfNecessary
        scanner
        builder
        (liftIO . checkDigestsOfFilesIfExisting [object_file_path,interface_file_path])
        (resolveModuleDependency path_to_ghc_pkg known_modules)
        ghc_arguments
        [source_job_id]
        [ghc_runtime_unresolved_dependency]
  where
    ghc_arguments =
        computeGHCCompileToObjectArguments
            ghc_options
            source_file_path
            object_file_path
            interface_file_path

    scanner =
        fmap extractDependenciesFromHaskellSource
        .
        liftIO
        .
        L.readFile
        $
        source_file_path

    builder = liftIO $ do
        noticeM "Blueprint.Tools.Compilers.GHC" $
            "(GHC) Compiling "
            ++ source_file_path ++
            " --> "
            ++ object_file_path ++
            " / "
            ++ interface_file_path
        infoM "Blueprint.Tools.Compilers.GHC" $
            "(GHC) Executing '" ++ (unwords (path_to_ghc:ghc_arguments)) ++ "'"
        runProductionCommandAndDigestOutputs
            [object_file_path,interface_file_path]
            []
            path_to_ghc
            ghc_arguments
-- @-node:gcross.20100630111926.1874:createGHCCompileToObjectTask
-- @-node:gcross.20100630111926.1873:tasks
-- @-node:gcross.20100628115452.1853:Functions
-- @+node:gcross.20100611224425.1613:Values
-- @+node:gcross.20100611224425.1614:ghc_probe
ghc_version_regex = makeRegex "version ([0-9.]*)" :: Regex
-- @-node:gcross.20100611224425.1614:ghc_probe
-- @+node:gcross.20100630111926.1887:ghc_runtime_dependency
ghc_runtime_dependency = Dependency runtime_dependency_type "Haskell (GHC)"
-- @-node:gcross.20100630111926.1887:ghc_runtime_dependency
-- @+node:gcross.20100630111926.1889:ghc_runtime_unresolved_dependency
ghc_runtime_unresolved_dependency = UnresolvedDependency Nothing ghc_runtime_dependency
-- @-node:gcross.20100630111926.1889:ghc_runtime_unresolved_dependency
-- @-node:gcross.20100611224425.1613:Values
-- @-others
-- @-node:gcross.20100611224425.1610:@thin GHC.hs
-- @-leo
