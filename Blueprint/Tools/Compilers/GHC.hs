-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1610:@thin GHC.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1611:<< Language extensions >>
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1611:<< Language extensions >>
-- @nl

module Blueprint.Tools.Compilers.GHC where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1612:<< Import needed modules >>
import Control.Applicative
import Control.Applicative.Infix
import Control.Arrow
import qualified Control.Concurrent.Thread as Thread
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Data.Binary
import qualified Data.ByteString.Lazy as L
import Data.DeriveTH
import Data.Dynamic
import Data.Either
import Data.Either.Unwrap
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Record
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable

import qualified Distribution.Compiler as Compiler
import Distribution.InstalledPackageInfo
import Distribution.License
import Distribution.ModuleName
import Distribution.Package (PackageId(..),PackageName(..),PackageIdentifier(..),InstalledPackageId(..))
import qualified Distribution.Package as Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.System
import Distribution.Text
import Distribution.Version
import Distribution.Verbosity

import System.Directory
import System.Exit
import System.FilePath
import System.Log.Logger
import System.Process

import Text.Regex.PCRE
import Text.Regex.PCRE.String

import Blueprint.Configuration.Tools
import Blueprint.Dependency
import Blueprint.Fields.DeferredDependencies
import Blueprint.Fields.FilePath
import Blueprint.Identifier
import Blueprint.Jobs
import Blueprint.Jobs.Combinators
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
-- @+node:gcross.20100901145855.2053:BuildEnvironment
data BuildEnvironment = BuildEnvironment
    {   buildEnvironmentGHC :: GHC
    ,   buildEnvironmentKnownModules :: KnownModules
    ,   buildEnvironmentBuiltModules :: [BuiltModule]
    ,   buildEnvironmentObjectLookup :: String → Maybe JobId
    ,   buildEnvironmentCompileOptions :: [String]
    ,   buildEnvironmentLinkOptions :: [String]
    }
-- @-node:gcross.20100901145855.2053:BuildEnvironment
-- @+node:gcross.20100901145855.2055:BuildTargetType
data BuildTargetType = LibraryTarget | ExecutableTarget

-- @-node:gcross.20100901145855.2055:BuildTargetType
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
-- @+node:gcross.20100709210816.2222:BuiltProgram
data BuiltProgram = BuiltProgram
    {   builtProgramFilePath :: FilePath
    ,   builtProgramObjectFilePaths :: [FilePath]
    ,   builtProgramJobId :: JobId
    }
-- @-node:gcross.20100709210816.2222:BuiltProgram
-- @+node:gcross.20100830091258.2027:GHC
data GHC = GHC
    {   ghcVersion :: Version
    ,   ghcDirectory :: FilePath
    ,   pathToGHC :: FilePath
    ,   pathToGHCPkg :: FilePath
    } deriving Typeable

$( derive makeBinary ''GHC )
-- @-node:gcross.20100830091258.2027:GHC
-- @+node:gcross.20100831211145.2168:GHCEnvironment
data GHCEnvironment = GHCEnvironment
    {   ghcEnvironmentGHC :: GHC
    ,   ghcEnvironmentPackageDatabase :: PackageDatabase
    } deriving Typeable

-- @-node:gcross.20100831211145.2168:GHCEnvironment
-- @+node:gcross.20100630111926.1875:GHCOptions
newtype GHCOptions = GHCOptions { unwrapGHCOptions :: Record }
-- @nonl
-- @-node:gcross.20100630111926.1875:GHCOptions
-- @+node:gcross.20100831211145.2134:PackageDatabase
data PackageDatabase = PackageDatabase
    {   packageDatabaseIndexedByInstalledPackageId :: Map InstalledPackageId InstalledPackageInfo
    ,   packageDatabaseIndexedByPackageNameAndVersion :: Map PackageName [(Version,[InstalledPackageInfo])]
    } deriving Typeable
-- @-node:gcross.20100831211145.2134:PackageDatabase
-- @-node:gcross.20100630111926.1861:Types
-- @+node:gcross.20100709210816.2105:Instances
-- @+node:gcross.20100709210816.2106:Show BuiltModule
instance Show BuiltModule where
    show BuiltModule{..} =
        builtModuleName ++ ": " ++ builtModuleSourceFilePath ++ " -> " ++ builtModuleObjectFilePath ++ ", " ++ builtModuleInterfaceFilePath
-- @-node:gcross.20100709210816.2106:Show BuiltModule
-- @+node:gcross.20100831211145.2151:Binary PackageDatabase
$(derive makeBinary ''PackageDatabase)
$(derive makeBinary ''InstalledPackageInfo_)
$(derive makeBinary ''PackageName)
$(derive makeBinary ''InstalledPackageId)
$(derive makeBinary ''PackageIdentifier)
$(derive makeBinary ''License)

instance Binary ModuleName where
    put module_name = put (show module_name)
    get = fmap read get
-- @-node:gcross.20100831211145.2151:Binary PackageDatabase
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
-- @+node:gcross.20100830091258.2028:configuration
-- @+node:gcross.20100830091258.2029:lookForGHCInPaths
lookForGHCInPaths :: [FilePath] → IO [GHC]
lookForGHCInPaths paths =
    fmap (
        sortBy (compare `on` ghcVersion)
        .
        map (\(path,version) → GHC version path (path </> "ghc") (path </> "ghc-pkg"))
        .
        Map.toList
    )
    .
    liftIO
    $
    (
        liftM2 Map.intersection
            (lookForVersionedProgramInPaths ghc_program paths)
            (lookForVersionedProgramInPaths ghc_pkg_program paths)
    )
-- @-node:gcross.20100830091258.2029:lookForGHCInPaths
-- @+node:gcross.20100831154015.2037:configureGHC
configureGHC ::
    (Binary α, Eq α) =>
    String →
    [FilePath] →
    α →
    ([GHC] → GHC) →
    JobApplicative JobId Dynamic GHC
configureGHC job_distinguisher search_paths selection_criteria_identifier selectGHC =
    JobApplicative
    {   jobApplicativeJobs = [job]
    ,   jobApplicativeResultJobNames = job_names
    ,   jobApplicativeResultExtractorJobName = identifierInNamespace ghc_configuration_namespace "configure GHC" "configure GHC"
    ,   jobApplicativeResultExtractor = fromJust . fromDynamic . head
    }
  where
    job@(Job job_names job_runner) = createFindGHCJob job_distinguisher search_paths selection_criteria_identifier selectGHC
-- @-node:gcross.20100831154015.2037:configureGHC
-- @+node:gcross.20100831211145.2170:configureGHCEnvironment
configureGHCEnvironment ::
    (Binary α, Eq α) =>
    String →
    [FilePath] →
    α →
    ([GHC] → GHC) →
    JobApplicative JobId Dynamic GHCEnvironment
configureGHCEnvironment distinguisher search_paths selection_criteria_identifier selectGHC =
    configureGHC distinguisher search_paths selection_criteria_identifier selectGHC
    ➤
    proc ghc → do
        package_database <- configurePackageDatabase distinguisher -< ghc
        returnA -< GHCEnvironment ghc package_database
-- @-node:gcross.20100831211145.2170:configureGHCEnvironment
-- @+node:gcross.20100831211145.2153:configurePackageDatabase
configurePackageDatabase ::
    String →
    JobArrow JobId Dynamic GHC PackageDatabase
configurePackageDatabase distinguisher =
    JobArrow
    {   jobArrowDependentJobs = [job]
    ,   jobArrowIndependentJobs = []
    ,   jobArrowResultJobNames = job_names
    ,   jobArrowResultExtractorJobName = (`mappend` identifierInNamespace ghc_configuration_namespace "configure package database" "configure package database")
    ,   jobArrowResultExtractor = const (fromJust . fromDynamic . head)
    }
  where
    job@(IncompleteJob job_names _) = cofmap pathToGHCPkg (createLoadGHCPackageDatabaseIncompleteJob distinguisher)
-- @-node:gcross.20100831211145.2153:configurePackageDatabase
-- @+node:gcross.20100901145855.2056:extractPackageDependencies
extractPackageDependencies :: PackageDescription → BuildTargetType → [Package.Dependency]
extractPackageDependencies PackageDescription{..} build_target_type = buildDepends ++
    case build_target_type of
        LibraryTarget → targetBuildDepends . libBuildInfo . fromJust $ library
        ExecutableTarget → concat . map (targetBuildDepends . buildInfo) $ executables
-- @-node:gcross.20100901145855.2056:extractPackageDependencies
-- @+node:gcross.20100901145855.2054:computeBuildEnvironment
computeBuildEnvironment ::
    GHCEnvironment →
    PackageDescription →
    BuildTargetType →
    [BuiltModule] →
    [String] →
    [String] →
    FilePath →
    BuildEnvironment
computeBuildEnvironment
    GHCEnvironment{..}
    package_description@PackageDescription{..}
    build_target_type
    built_modules
    additional_compile_options
    additional_link_options
    interface_directory
    =
    BuildEnvironment
    {   buildEnvironmentGHC = ghcEnvironmentGHC
    ,   buildEnvironmentKnownModules =
            Map.unions
                (builtModulesToKnownModules built_modules
                :map extractKnownModulesFromInstalledPackage installed_package_dependencies
                )
    ,   buildEnvironmentBuiltModules = built_modules
    ,   buildEnvironmentObjectLookup = buildModulesToObjectLookup built_modules
    ,   buildEnvironmentCompileOptions = shared_options ++ additional_compile_options
    ,   buildEnvironmentLinkOptions = shared_options ++ additional_link_options
    }
  where
    build_for_package_options = 
        case build_target_type of
            LibraryTarget → ["-package-name",display package]
            _ → []
    installed_package_dependencies =
        map (fromJust . findSatisfyingPackage ghcEnvironmentPackageDatabase)
            (extractPackageDependencies package_description build_target_type)
    package_dependency_options =
        concat
            [ ["-package",display sourcePackageId]
            | InstalledPackageInfo{..} ← installed_package_dependencies
            ]
    shared_options =
        ("-i" ++ interface_directory)
        :
        build_for_package_options
        ++
        package_dependency_options
-- @-node:gcross.20100901145855.2054:computeBuildEnvironment
-- @+node:gcross.20100901145855.2063:extractKnownModulesFromInstalledPackage
extractKnownModulesFromInstalledPackage :: InstalledPackageInfo → KnownModules
extractKnownModulesFromInstalledPackage InstalledPackageInfo{..} =
    Map.fromList
    .
    map ((,resolved_dependencies) . display)
    $
    exposedModules
  where
    resolved_dependencies = ResolvedDependencies [] [haskellPackageDependency (display sourcePackageId)]
-- @-node:gcross.20100901145855.2063:extractKnownModulesFromInstalledPackage
-- @+node:gcross.20100901145855.2073:readAndConfigurePackageDescription
readAndConfigurePackageDescription ::
    GHCEnvironment →
    FlagAssignment →
    FilePath →
    IO (PackageDescription, FlagAssignment)
readAndConfigurePackageDescription GHCEnvironment{..} flags =
    readPackageDescription silent
    >=>
    either
        (throwIO . UnresolvedPackageDependenciesError ghcEnvironmentPackageDatabase)
        return
    .
    finalizePackageDescription
        flags
        (checkForSatisfyingPackage ghcEnvironmentPackageDatabase)
        buildPlatform
        (Compiler.CompilerId Compiler.GHC (ghcVersion ghcEnvironmentGHC))
        []
-- @nonl
-- @-node:gcross.20100901145855.2073:readAndConfigurePackageDescription
-- @-node:gcross.20100830091258.2028:configuration
-- @+node:gcross.20100628115452.1899:dependency arguments
-- @+node:gcross.20100628115452.1895:computeGHCLinkDependencyArguments
computeGHCLinkDependencyArguments :: [Dependency] → [String]
computeGHCLinkDependencyArguments =
    concat
    .
    zipWith ($)
        (map snd dependency_types_and_processors)
    .
    separateDependenciesByType
        "the GHC linker"
        (map fst dependency_types_and_processors)
  where
    dependency_types_and_processors =
        [(runtime_dependency_type,computeGHCRuntimeDependencyArguments)
        ,(haskell_package_dependency_type,computeGHCPackageDependencyArguments)
        ,(library_dependency_type,computeGHCLibraryDependencyArguments)
        ,(object_dependency_type,computeGHCObjectDependencyArguments)
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
-- @+node:gcross.20100901145855.2058:buildModulesToObjectLookup
buildModulesToObjectLookup :: [BuiltModule] → (String → Maybe JobId)
buildModulesToObjectLookup =
    flip Map.lookup
    .
    Map.fromList
    .
    map (builtModuleObjectFilePath &&& builtModuleObjectJobId)
-- @-node:gcross.20100901145855.2058:buildModulesToObjectLookup
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
-- @+node:gcross.20100630111926.1863:resolveModuleDependency
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
-- @-node:gcross.20100630111926.1863:resolveModuleDependency
-- @+node:gcross.20100709210816.2223:builtProgram
builtProgram :: FilePath → [FilePath] → BuiltProgram
builtProgram program_file_path program_object_file_paths =
    BuiltProgram
    {   builtProgramFilePath = program_file_path
    ,   builtProgramObjectFilePaths = program_object_file_paths
    ,   builtProgramJobId = programJobId program_file_path ("Linking " ++ program_file_path)
    }
-- @-node:gcross.20100709210816.2223:builtProgram
-- @-node:gcross.20100630111926.1860:dependency resolution
-- @+node:gcross.20100831211145.2135:package database
-- @+node:gcross.20100831211145.2139:loadInstalledPackageInformation
loadInstalledPackageInformation :: FilePath → String → IO InstalledPackageInfo
loadInstalledPackageInformation path_to_ghc_pkg package_atom = do
    package_description ← readProcess path_to_ghc_pkg ["describe",package_atom] ""
    case parseInstalledPackageInfo package_description of
        ParseOk _ installed_package_info → return installed_package_info
        ParseFailed parse_error → error $ "Error parsing description of package " ++ package_atom ++ ": " ++ show parse_error
-- @-node:gcross.20100831211145.2139:loadInstalledPackageInformation
-- @+node:gcross.20100901145855.2050:findSatisfyingPackage
findSatisfyingPackage :: PackageDatabase → Package.Dependency → Maybe InstalledPackageInfo
findSatisfyingPackage PackageDatabase{..} (Package.Dependency name version_range) =
    Map.lookup name packageDatabaseIndexedByPackageNameAndVersion
    >>=
    find (flip withinRange version_range . fst)
    >>=
    return . head . snd
-- @-node:gcross.20100901145855.2050:findSatisfyingPackage
-- @+node:gcross.20100901145855.2052:checkForSatsifyingPackage
checkForSatisfyingPackage :: PackageDatabase → Package.Dependency → Bool
checkForSatisfyingPackage package_database dependency = isJust (findSatisfyingPackage package_database dependency)
-- @-node:gcross.20100901145855.2052:checkForSatsifyingPackage
-- @+node:gcross.20100901145855.2088:lookupPackageNamed
lookupPackageNamed :: PackageDatabase → PackageName → Maybe [(Version,[InstalledPackageInfo])]
lookupPackageNamed PackageDatabase{..} package_name = Map.lookup package_name packageDatabaseIndexedByPackageNameAndVersion
-- @-node:gcross.20100901145855.2088:lookupPackageNamed
-- @-node:gcross.20100831211145.2135:package database
-- @+node:gcross.20100630111926.1873:jobs
-- @+node:gcross.20100830091258.2033:createFindGHCJob
createFindGHCJob ::
    (Binary α, Eq α) =>
    String →
    [FilePath] →
    α →
    ([GHC] → GHC) →
    Job JobId Dynamic
createFindGHCJob job_distinguisher search_paths selection_criteria_identifier selectGHC =
    jobWithCache [identifierInNamespace ghc_configuration_namespace job_distinguisher "GHC configuration"]
    $
    \maybe_old_search_paths →
        case maybe_old_search_paths of
            Just (old_cache@(old_search_paths,old_selection_criteria_identifier,ghc))
              | old_search_paths == search_paths
              , old_selection_criteria_identifier == selection_criteria_identifier 
              → returnValueAndCache (toDyn ghc) old_cache
            _ → liftIO (lookForGHCInPaths search_paths)
                 >>=
                 \ghcs →
                    let ghc = selectGHC ghcs
                    in returnValueAndCache (toDyn ghc) (search_paths,selection_criteria_identifier,ghc)
-- @-node:gcross.20100830091258.2033:createFindGHCJob
-- @+node:gcross.20100831211145.2141:createLoadGHCPackageDatabaseIncompleteJob
createLoadGHCPackageDatabaseIncompleteJob ::
    String →
    IncompleteJob JobId Dynamic FilePath
createLoadGHCPackageDatabaseIncompleteJob job_distinguisher =
    incompleteJobWithCache [identifierInNamespace ghc_package_database_namespace job_distinguisher "package database"]
    $
    \path_to_ghc_pkg maybe_cache → do
        list_of_package_atoms ← fmap words (liftIO $ readProcess path_to_ghc_pkg ["--simple-output","list"] "")
        case maybe_cache of
            Just cache@(old_list_of_package_atoms,old_package_database)
                | old_list_of_package_atoms == list_of_package_atoms → returnDynamicValueAndCache old_package_database cache
            _ → do
                installed_packages ← liftIO $
                    mapM (Thread.forkIO . loadInstalledPackageInformation path_to_ghc_pkg) list_of_package_atoms
                    >>=
                    mapM ((>>= Thread.unsafeResult) . snd)
                let package_database = 
                        PackageDatabase
                            (Map.fromList [(installedPackageId,package_info) | package_info@InstalledPackageInfo{..} ← installed_packages])
                            (Map.fromList
                                [ ((pkgName . sourcePackageId . head) installed_package_info
                                  ,[((pkgVersion . sourcePackageId . head) installed_package_info_
                                    ,installed_package_info_
                                    )
                                   | installed_package_info_ ← installed_package_info
                                   , then group by (pkgVersion . sourcePackageId $ installed_package_info_)
                                   ]
                                  )
                                | installed_package_info ← installed_packages
                                , then group by (pkgName . sourcePackageId $ installed_package_info)
                                ]
                            )
                returnDynamicValueAndCache package_database (list_of_package_atoms,package_database)
-- @-node:gcross.20100831211145.2141:createLoadGHCPackageDatabaseIncompleteJob
-- @+node:gcross.20100831211145.2155:createLoadGHCPackageDatabaseJob
createLoadGHCPackageDatabaseJob ::
    String →
    FilePath →
    Job JobId Dynamic
createLoadGHCPackageDatabaseJob = flip completeJobWith . createLoadGHCPackageDatabaseIncompleteJob
-- @-node:gcross.20100831211145.2155:createLoadGHCPackageDatabaseJob
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
    postprocessInterface =
        setFilePath builtModuleInterfaceFilePath
        .
        addDeferredDependency (objectDependency builtModuleObjectFilePath)
    postprocessObject =
        setFilePath builtModuleObjectFilePath
        .
        addDeferredDependency ghc_runtime_dependency
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
-- @+node:gcross.20100901145855.2080:createGHCCompileToObjectJobsFromBuildEnvironment
createGHCCompileToObjectJobsFromBuildEnvironment :: BuildEnvironment → [ToolJob]
createGHCCompileToObjectJobsFromBuildEnvironment BuildEnvironment{..} =
    map (
        createGHCCompileToObjectJob
            (pathToGHC buildEnvironmentGHC)
            (pathToGHCPkg buildEnvironmentGHC)
            buildEnvironmentKnownModules
            buildEnvironmentCompileOptions
    ) buildEnvironmentBuiltModules
-- @-node:gcross.20100901145855.2080:createGHCCompileToObjectJobsFromBuildEnvironment
-- @+node:gcross.20100705132935.1938:createGHCLinkProgramJob
createGHCLinkProgramJob ::
    FilePath →
    [String] →
    (String → Maybe JobId) →
    BuiltProgram →
    ToolJob
createGHCLinkProgramJob
    path_to_ghc
    options_arguments
    lookupObjectJobId
    BuiltProgram{..}
    =
    Job [builtProgramJobId]
    .
    runJobAnalyzer
    .
    fmap (zipWith ($) [setFilePath builtProgramFilePath])
    $
    fetchAllDeferredDependenciesAndRebuildIfNecessary
        lookupDependencyJobIds
        (liftIO . checkDigestsOfFilesIfExisting [builtProgramFilePath])
        builder
        options_arguments
        (map (Dependency object_dependency_type) builtProgramObjectFilePaths)
  where
    -- @    @+others
    -- @+node:gcross.20100705150931.1943:builder
    builder dependencies = liftIO $ do
        let ghc_arguments =
                computeGHCLinkToProgramArguments
                    options_arguments
                    dependencies
                    builtProgramFilePath
        noticeM "Blueprint.Tools.Compilers.GHC" $
            "(GHC) Linking program "
            ++ builtProgramFilePath
        infoM "Blueprint.Tools.Compilers.GHC" $
            "(GHC) Executing '" ++ (unwords (path_to_ghc:ghc_arguments)) ++ "'"
        runProductionCommandAndDigestOutputs
            [builtProgramFilePath]
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
-- @-node:gcross.20100705132935.1938:createGHCLinkProgramJob
-- @+node:gcross.20100901145855.2085:createGHCLinkProgramJobUsingBuildEnvironment
createGHCLinkProgramJobUsingBuildEnvironment :: BuildEnvironment → BuiltProgram → ToolJob
createGHCLinkProgramJobUsingBuildEnvironment BuildEnvironment{..} =
    createGHCLinkProgramJob
        (pathToGHC buildEnvironmentGHC)
        buildEnvironmentLinkOptions
        buildEnvironmentObjectLookup
-- @-node:gcross.20100901145855.2085:createGHCLinkProgramJobUsingBuildEnvironment
-- @-node:gcross.20100630111926.1873:jobs
-- @+node:gcross.20100709210816.2233:options
-- @+node:gcross.20100709210816.2234:computeGHCInterfaceDirectoryArguments
computeGHCInterfaceDirectoryArguments :: FilePath → [String]
computeGHCInterfaceDirectoryArguments = (:[]) . ("-i" ++)
-- @-node:gcross.20100709210816.2234:computeGHCInterfaceDirectoryArguments
-- @+node:gcross.20100709210816.2235:computeGHCPackageNameArguments
computeGHCCompileAsPackageNameArguments :: String → [String]
computeGHCCompileAsPackageNameArguments = ("-package-name":) . (:[])
-- @-node:gcross.20100709210816.2235:computeGHCPackageNameArguments
-- @-node:gcross.20100709210816.2233:options
-- @-node:gcross.20100628115452.1853:Functions
-- @+node:gcross.20100901145855.2091:Exceptions
-- @+node:gcross.20100901145855.2092:UnresolvedPackageDependenciesError
data UnresolvedPackageDependenciesError =
    UnresolvedPackageDependenciesError PackageDatabase [Package.Dependency]
  deriving Typeable

instance Show UnresolvedPackageDependenciesError where
    show (UnresolvedPackageDependenciesError package_database unresolved_dependencies) =
        intercalate "\n"
        .
        ("Unable to resolve the following package dependencies:":)
        .
        map (\dependency@(Package.Dependency package_name _) →
            '\t' -- '
            :
            display dependency
            ++
            case fmap (map fst) (lookupPackageNamed package_database package_name) of
                Nothing → ""
                Just [] → ""
                Just [version] → " (version " ++ display version ++ " is installed)"
                Just [version1,version2] → " (versions " ++ display version1 ++ " and " ++ display version2 ++ " are installed)"
                Just versions → " (versions " ++ intercalate ", " (map display versions) ++ " are installed)"
        )
        $
        unresolved_dependencies

instance Exception UnresolvedPackageDependenciesError
-- @-node:gcross.20100901145855.2092:UnresolvedPackageDependenciesError
-- @-node:gcross.20100901145855.2091:Exceptions
-- @+node:gcross.20100708215239.2092:Namespaces
interface_namespace = uuid "9f1b88df-e2cf-4020-8a44-655aacfbacbb"
ghc_configuration_namespace = uuid "dc71e9fc-8917-4263-869b-bde953f56300"
ghc_package_database_namespace = uuid "bb0914b4-46fd-4fde-8277-08c5167c1f22"
-- @-node:gcross.20100708215239.2092:Namespaces
-- @+node:gcross.20100611224425.1613:Values
-- @+node:gcross.20100705150931.1962:ghc_linker_actor_name
ghc_linker_actor_name = "the GHC linker"
-- @-node:gcross.20100705150931.1962:ghc_linker_actor_name
-- @+node:gcross.20100611224425.1614:ghc_version_regex
ghc_version_regex = makeRegex "version ([0-9.]*)" :: Regex
-- @-node:gcross.20100611224425.1614:ghc_version_regex
-- @+node:gcross.20100705150931.1951:ghc_runtime_dependency_name
ghc_runtime_dependency_name = "Haskell (GHC)"
-- @-node:gcross.20100705150931.1951:ghc_runtime_dependency_name
-- @+node:gcross.20100630111926.1887:ghc_runtime_dependency
ghc_runtime_dependency = Dependency runtime_dependency_type ghc_runtime_dependency_name
-- @-node:gcross.20100630111926.1887:ghc_runtime_dependency
-- @+node:gcross.20100630111926.1889:ghc_runtime_unresolved_dependency
ghc_runtime_unresolved_dependency = UnresolvedDependency Nothing ghc_runtime_dependency
-- @-node:gcross.20100630111926.1889:ghc_runtime_unresolved_dependency
-- @+node:gcross.20100830091258.2031:ghc_version_extractor
ghc_version_extractor = VersionExtractor ["--version"] (extractVersion ghc_version_regex)

-- @-node:gcross.20100830091258.2031:ghc_version_extractor
-- @+node:gcross.20100830091258.2032:ghc_program
ghc_program = VersionedProgram "ghc" ghc_version_extractor
-- @nonl
-- @-node:gcross.20100830091258.2032:ghc_program
-- @+node:gcross.20100830091258.2036:ghc_pkg_program
ghc_pkg_program = VersionedProgram "ghc-pkg" ghc_version_extractor
-- @nonl
-- @-node:gcross.20100830091258.2036:ghc_pkg_program
-- @-node:gcross.20100611224425.1613:Values
-- @-others
-- @-node:gcross.20100611224425.1610:@thin GHC.hs
-- @-leo
