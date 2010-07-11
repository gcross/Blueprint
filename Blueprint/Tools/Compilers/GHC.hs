-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1610:@thin GHC.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1611:<< Language extensions >>
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1611:<< Language extensions >>
-- @nl

module Blueprint.Tools.Compilers.GHC where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1612:<< Import needed modules >>
import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import qualified Data.ByteString.Lazy as L
import Data.Either
import Data.Either.Unwrap
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Record
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Data.Version

import System.Exit
import System.FilePath
import System.Log.Logger
import System.Process

import Text.Regex.PCRE
import Text.Regex.PCRE.String

import Blueprint.Dependency
import Blueprint.Fields.DeferredDependencies
import Blueprint.Identifier
import Blueprint.Jobs
import Blueprint.Language.Programming.Haskell
import Blueprint.Miscellaneous
import Blueprint.SourceFile
import Blueprint.Tools
import Blueprint.Tools.JobAnalyzer
-- @-node:gcross.20100611224425.1612:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100630111926.1861:Types
-- @+node:gcross.20100630111926.1862:KnownModules
type KnownModules = Map String ResolvedDependencies
-- @-node:gcross.20100630111926.1862:KnownModules
-- @+node:gcross.20100708215239.2085:BuiltModule
data BuiltModule = BuiltModule
    {   builtModuleName :: String
    ,   builtModuleSourceFilePath :: FilePath
    ,   builtModuleSourceJobId :: JobId
    ,   builtModuleObjectFilePath :: FilePath
    ,   builtModuleObjectJobId :: JobId
    ,   builtModuleInterfaceFilePath :: FilePath
    ,   builtModuleInterfaceJobId :: JobId
    }
-- @-node:gcross.20100708215239.2085:BuiltModule
-- @+node:gcross.20100630111926.1875:GHCOptions
newtype GHCOptions = GHCOptions { unwrapGHCOptions :: Record }
-- @nonl
-- @-node:gcross.20100630111926.1875:GHCOptions
-- @-node:gcross.20100630111926.1861:Types
-- @+node:gcross.20100709210816.2105:Instances
-- @+node:gcross.20100709210816.2106:BuiltModule
instance Show BuiltModule where
    show BuiltModule{..} =
        builtModuleName ++ ": " ++ builtModuleSourceFilePath ++ " -> " ++ builtModuleObjectFilePath ++ ", " ++ builtModuleInterfaceFilePath
-- @-node:gcross.20100709210816.2106:BuiltModule
-- @-node:gcross.20100709210816.2105:Instances
-- @+node:gcross.20100628115452.1853:Functions
-- @+node:gcross.20100628115452.1859:compilation/linking arguments
-- @+node:gcross.20100628115452.1854:computeGHCCompileToObjectArguments
computeGHCCompileToObjectArguments :: [String] → FilePath → FilePath → FilePath → [String]
computeGHCCompileToObjectArguments
    options_arguments
    source_filepath
    object_filepath
    interface_filepath
    =
     "-c":source_filepath
    :"-o":object_filepath
    :"-ohi":interface_filepath
    :options_arguments
-- @-node:gcross.20100628115452.1854:computeGHCCompileToObjectArguments
-- @+node:gcross.20100628115452.1856:computeGHCCompileToProgramArguments
computeGHCCompileToProgramArguments :: [String] → FilePath → FilePath → [String]
computeGHCCompileToProgramArguments
    options_arguments
    source_filepath
    program_filepath
    =
     source_filepath
    :"-o":program_filepath
    :options_arguments
-- @-node:gcross.20100628115452.1856:computeGHCCompileToProgramArguments
-- @+node:gcross.20100628115452.1858:computeGHCLinkToProgramArguments
computeGHCLinkToProgramArguments :: [String] → [Dependency] → FilePath → [String]
computeGHCLinkToProgramArguments
    options_arguments
    dependencies
    program_filepath
    =
    concat
        [options_arguments
        ,computeGHCLinkDependencyArguments dependencies
        ,["-o",program_filepath]
        ]
-- @-node:gcross.20100628115452.1858:computeGHCLinkToProgramArguments
-- @-node:gcross.20100628115452.1859:compilation/linking arguments
-- @+node:gcross.20100628115452.1899:dependency arguments
-- @+node:gcross.20100628115452.1895:computeGHCLinkDependencyArguments
computeGHCLinkDependencyArguments :: [Dependency] → [String]
computeGHCLinkDependencyArguments =
    concat
    .
    zipWith ($)
        [computeGHCRuntimeDependencyArguments
        ,computeGHCPackageDependencyArguments
        ,computeGHCLibraryDependencyArguments
        ,computeGHCObjectDependencyArguments
        ]
    .
    separateDependenciesByType
        "the GHC linker"
        [runtime_dependency_type
        ,library_dependency_type
        ,object_dependency_type
        ,haskell_package_dependency_type
        ]
-- @-node:gcross.20100628115452.1895:computeGHCLinkDependencyArguments
-- @+node:gcross.20100705150931.1957:computeGHCLibraryDependencyArguments
computeGHCLibraryDependencyArguments = map ("-l"++)
-- @-node:gcross.20100705150931.1957:computeGHCLibraryDependencyArguments
-- @+node:gcross.20100705150931.1959:computeGHCObjectDependencyArguments
computeGHCObjectDependencyArguments = id
-- @-node:gcross.20100705150931.1959:computeGHCObjectDependencyArguments
-- @+node:gcross.20100628115452.1900:computeGHCPackageDependencyArguments
computeGHCPackageDependencyArguments [] = []
computeGHCPackageDependencyArguments (package:rest_packages) =
    "-package":package:computeGHCPackageDependencyArguments rest_packages
-- @-node:gcross.20100628115452.1900:computeGHCPackageDependencyArguments
-- @+node:gcross.20100705150931.1961:computeGHCObjectDependencyArguments
computeGHCRuntimeDependencyArguments runtime_dependencies =
    case delete ghc_runtime_dependency_name runtime_dependencies of
        [] → []
        unrecognized_runtime_dependenceies →
            throw
            .
            UnrecognizedRuntimes ghc_linker_actor_name
            $
            unrecognized_runtime_dependenceies
-- @-node:gcross.20100705150931.1961:computeGHCObjectDependencyArguments
-- @-node:gcross.20100628115452.1899:dependency arguments
-- @+node:gcross.20100630111926.1860:dependency resolution
-- @+node:gcross.20100708102250.2756:builtModulesToKnownModules
builtModulesToKnownModules :: [BuiltModule] → KnownModules
builtModulesToKnownModules =
    Map.fromList
    .
    map (
        builtModuleName
        &&&
        liftA2 ResolvedDependencies
            ((:[]) . builtModuleInterfaceJobId)
            ((:[]) . objectDependency . builtModuleObjectFilePath)
    )

-- @-node:gcross.20100708102250.2756:builtModulesToKnownModules
-- @+node:gcross.20100630111926.1868:findPackagesExposingModule
findPackagesExposingModule :: FilePath -> String -> IO [String]
findPackagesExposingModule path_to_ghc_pkg module_name =
    fmap words
    .
    readProcess path_to_ghc_pkg ["--simple-output","find-module",module_name]
    $
    ""
-- @-node:gcross.20100630111926.1868:findPackagesExposingModule
-- @+node:gcross.20100708192404.2001:haskellSourceToBuiltModule
haskellSourceToBuiltModule :: FilePath → FilePath → HaskellSource → BuiltModule
haskellSourceToBuiltModule object_subdirectory interface_subdirectory HaskellSource{..} =
    BuiltModule
    {   builtModuleName = haskellSourceModuleName
    ,   builtModuleSourceFilePath = haskellSourceFilePath
    ,   builtModuleSourceJobId = haskellSourceDigestJobId
    ,   builtModuleObjectFilePath = object_file_path
    ,   builtModuleObjectJobId = objectJobId object_file_path display_name
    ,   builtModuleInterfaceFilePath = interface_file_path
    ,   builtModuleInterfaceJobId = interfaceJobId interface_file_path display_name
    }
  where
    haskell_module_path = hierarchalPathToFilePath haskellSourceHierarchalPath
    object_file_path = object_subdirectory </> haskell_module_path <.> "o"
    interface_file_path = interface_subdirectory </> haskell_module_path <.> "o"
    display_name = "Compile " ++ haskellSourceModuleName
-- @-node:gcross.20100708192404.2001:haskellSourceToBuiltModule
-- @+node:gcross.20100708215239.2093:interfaceJobId
interfaceJobId = identifierInNamespace interface_namespace
-- @-node:gcross.20100708215239.2093:interfaceJobId
-- @+node:gcross.20100630111926.1863:resolveModuleDependencies
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
        Just resolution → return . Right $ resolution
  | dependencyType == runtime_dependency_type
    = return . Right $ ResolvedDependencies [] [unresolvedDependency]      
  | otherwise
    = throw $ UnrecognizedDependencyType "the GHC compiler" dependencyType
  where
    Dependency{..} = unresolvedDependency
-- @-node:gcross.20100630111926.1863:resolveModuleDependencies
-- @-node:gcross.20100630111926.1860:dependency resolution
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
-- @+node:gcross.20100709210816.2201:resolvePackage
resolvePackage :: FilePath → String → (Version → Bool) → IO (Either [Version] String)
resolvePackage path_to_ghc_pkg package_name checkVersion =
    fmap (
        (\versions →
            case find checkVersion versions of
                Nothing → Left versions
                Just version → Right (package_name ++ "-" ++ showVersion version)
        )
        .
        map readVersion
        .
        fromMaybe []
    ) (queryPackage path_to_ghc_pkg "version" package_name)
-- @-node:gcross.20100709210816.2201:resolvePackage
-- @+node:gcross.20100709210816.2208:resolvePackages
resolvePackages :: FilePath → [(String,Version → Bool)] → IO (Either [(String,[Version])] [String])
resolvePackages path_to_ghc_pkg =
    fmap (\resolutions →
        case partitionEithers resolutions of
            ([],package_ids) → Right package_ids
            (unresolved,_) → Left unresolved
    )
    .
    mapM (\(package_name,checkVersion) →
        fmap (mapLeft (package_name,)) (resolvePackage path_to_ghc_pkg package_name checkVersion)
    )
-- @-node:gcross.20100709210816.2208:resolvePackages
-- @+node:gcross.20100709210816.2209:fetchKnownModulesFromPackage
fetchKnownModulesFromPackage :: FilePath → String → IO (Maybe KnownModules)
fetchKnownModulesFromPackage path_to_ghc_pkg package_id =
    runMaybeT
    .
    fmap (Map.fromList . map (,resolution))
    .
    MaybeT
    .
    fetchPackageModules path_to_ghc_pkg
    $
    package_id
  where
    resolution =
        ResolvedDependencies []
        .
        (:[])
        .
        haskellPackageDependency
        $
        package_id
-- @-node:gcross.20100709210816.2209:fetchKnownModulesFromPackage
-- @+node:gcross.20100709210816.2213:fetchKnownModulesFromPackages
fetchKnownModulesFromPackages :: FilePath → [String] → IO (Maybe KnownModules)
fetchKnownModulesFromPackages path_to_ghc_pkg =
    runMaybeT
    .
    fmap mconcat
    .
    mapM (MaybeT . fetchKnownModulesFromPackage path_to_ghc_pkg)
-- @-node:gcross.20100709210816.2213:fetchKnownModulesFromPackages
-- @-node:gcross.20100630111926.1869:package queries
-- @+node:gcross.20100630111926.1873:jobs
-- @+node:gcross.20100630111926.1874:createGHCCompileToObjectJob
createGHCCompileToObjectJob ::
    FilePath →
    FilePath →
    KnownModules →
    [String] →
    BuiltModule →
    ToolJob

createGHCCompileToObjectJob
    path_to_ghc
    path_to_ghc_pkg
    known_modules
    options_arguments
    BuiltModule{..}
    =
    Job [builtModuleObjectJobId,builtModuleInterfaceJobId]
    .
    runJobAnalyzer
    .
    fmap (zipWith ($) [postprocessInterface,postprocessObject])
    $
    analyzeImplicitDependenciesAndRebuildIfNecessary
        scanner
        builder
        (liftIO . checkDigestsOfFilesIfExisting [builtModuleObjectFilePath,builtModuleInterfaceFilePath])
        (resolveModuleDependency path_to_ghc_pkg known_modules)
        ghc_arguments
        [builtModuleSourceJobId]
        [ghc_runtime_unresolved_dependency]
  where
    -- @    @+others
    -- @+node:gcross.20100705150931.1980:builder
    builder = liftIO $ do
        noticeM "Blueprint.Tools.Compilers.GHC" $
            "(GHC) Compiling "
            ++ builtModuleSourceFilePath ++
            " --> "
            ++ builtModuleObjectFilePath ++
            " / "
            ++ builtModuleInterfaceFilePath
        infoM "Blueprint.Tools.Compilers.GHC" $
            "(GHC) Executing '" ++ (unwords (path_to_ghc:ghc_arguments)) ++ "'"
        runProductionCommandAndDigestOutputs
            [builtModuleObjectFilePath,builtModuleInterfaceFilePath]
            []
            path_to_ghc
            ghc_arguments
    -- @-node:gcross.20100705150931.1980:builder
    -- @+node:gcross.20100705150931.1978:ghc_arguments
    ghc_arguments =
        computeGHCCompileToObjectArguments
            options_arguments
            builtModuleSourceFilePath
            builtModuleObjectFilePath
            builtModuleInterfaceFilePath
    -- @-node:gcross.20100705150931.1978:ghc_arguments
    -- @+node:gcross.20100708102250.2008:postprocessX
    postprocessInterface = addDeferredDependency (objectDependency builtModuleObjectFilePath)
    postprocessObject = addDeferredDependency ghc_runtime_dependency
    -- @-node:gcross.20100708102250.2008:postprocessX
    -- @+node:gcross.20100705150931.1979:scanner
    scanner =
        fmap extractDependenciesFromHaskellSource
        .
        liftIO
        .
        L.readFile
        $
        builtModuleSourceFilePath

    -- @-node:gcross.20100705150931.1979:scanner
    -- @-others
-- @-node:gcross.20100630111926.1874:createGHCCompileToObjectJob
-- @+node:gcross.20100705132935.1938:createGHCLinkProgramTask
createGHCLinkProgramTask ::
    FilePath →
    [String] →
    (String → Maybe JobId) →
    [FilePath] →
    FilePath →
    JobAnalysisRunner
createGHCLinkProgramTask
    path_to_ghc
    options_arguments
    lookupObjectJobId
    object_file_paths
    program_file_path
    =
    runJobAnalyzer
    $
    fetchAllDeferredDependenciesAndRebuildIfNecessary
        lookupDependencyJobIds
        (liftIO . checkDigestsOfFilesIfExisting [program_file_path])
        builder
        options_arguments
        (map (Dependency object_dependency_type) object_file_paths)
  where
    -- @    @+others
    -- @+node:gcross.20100705150931.1943:builder
    builder dependencies = liftIO $ do
        let ghc_arguments =
                computeGHCLinkToProgramArguments
                    options_arguments
                    dependencies
                    program_file_path
        noticeM "Blueprint.Tools.Compilers.GHC" $
            "(GHC) Linking program "
            ++ program_file_path
        infoM "Blueprint.Tools.Compilers.GHC" $
            "(GHC) Executing '" ++ (unwords (path_to_ghc:ghc_arguments)) ++ "'"
        runProductionCommandAndDigestOutputs
            [program_file_path]
            []
            path_to_ghc
            ghc_arguments
    -- @-node:gcross.20100705150931.1943:builder
    -- @+node:gcross.20100705150931.1942:lookupDependencyJobIds
    lookupDependencyJobIds dependencies
      | (not . null) unrecognized_dependency_types
        = throw $ UnrecognizedDependencyTypes my_actor_name unrecognized_dependency_types
      | (not . null) unrecognized_objects
        = throw $ UnknownObjects unrecognized_objects
      | otherwise
        = job_ids
      where
        ((unrecognized_dependency_types,unrecognized_objects),job_ids) =
            first partitionEithers
            .
            partitionEithers
            .
            map lookupDependencyJobId
            $
            dependencies

        lookupDependencyJobId (dependency@Dependency{..})
          | dependencyType `Set.member` recognized_dependency_types_without_job_ids
            = Right (dependency,Nothing)
          | dependencyType == object_dependency_type
            = case lookupObjectJobId dependencyName of
                Nothing → Left (Right dependencyName)
                Just job_id → Right (dependency,Just job_id)
          | otherwise
            = Left (Left dependencyType)
          where
            recognized_dependency_types_without_job_ids = Set.fromList
                [haskell_package_dependency_type
                ,runtime_dependency_type
                ,library_dependency_type
                ]
    -- @-node:gcross.20100705150931.1942:lookupDependencyJobIds
    -- @+node:gcross.20100705150931.1982:my_actor_name
    my_actor_name = ghc_linker_actor_name
    -- @nonl
    -- @-node:gcross.20100705150931.1982:my_actor_name
    -- @-others
-- @-node:gcross.20100705132935.1938:createGHCLinkProgramTask
-- @-node:gcross.20100630111926.1873:jobs
-- @-node:gcross.20100628115452.1853:Functions
-- @+node:gcross.20100708215239.2092:Namespaces
interface_namespace = uuid "9f1b88df-e2cf-4020-8a44-655aacfbacbb"
-- @-node:gcross.20100708215239.2092:Namespaces
-- @+node:gcross.20100611224425.1613:Values
-- @+node:gcross.20100705150931.1962:ghc_linker_actor_name
ghc_linker_actor_name = "the GHC linker"
-- @-node:gcross.20100705150931.1962:ghc_linker_actor_name
-- @+node:gcross.20100611224425.1614:ghc_probe
ghc_version_regex = makeRegex "version ([0-9.]*)" :: Regex
-- @-node:gcross.20100611224425.1614:ghc_probe
-- @+node:gcross.20100705150931.1951:ghc_runtime_dependency_name
ghc_runtime_dependency_name = "Haskell (GHC)"
-- @-node:gcross.20100705150931.1951:ghc_runtime_dependency_name
-- @+node:gcross.20100630111926.1887:ghc_runtime_dependency
ghc_runtime_dependency = Dependency runtime_dependency_type ghc_runtime_dependency_name
-- @-node:gcross.20100630111926.1887:ghc_runtime_dependency
-- @+node:gcross.20100630111926.1889:ghc_runtime_unresolved_dependency
ghc_runtime_unresolved_dependency = UnresolvedDependency Nothing ghc_runtime_dependency
-- @-node:gcross.20100630111926.1889:ghc_runtime_unresolved_dependency
-- @-node:gcross.20100611224425.1613:Values
-- @-others
-- @-node:gcross.20100611224425.1610:@thin GHC.hs
-- @-leo
