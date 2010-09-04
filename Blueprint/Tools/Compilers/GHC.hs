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
import Data.Digest.Pure.MD5
import Data.Dynamic
import Data.Either
import Data.Either.Unwrap
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Blueprint.Record
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable

import qualified Distribution.Compiler as Compiler
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import Distribution.Package (InstalledPackageId(..))
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
-- @nonl
-- @-node:gcross.20100611224425.1612:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100630111926.1861:Types
-- @+node:gcross.20100903104106.2083:InstalledPackage
data InstalledPackage = InstalledPackage
    {   installedPackageId :: InstalledPackageId
    ,   installedPackageQualifiedName :: String
    ,   installedPackageName :: String
    ,   installedPackageVersion :: Version
    ,   installedPackageModules :: [String]
    } deriving (Typeable, Eq);

$( derive makeBinary ''InstalledPackageId )
$( derive makeBinary ''InstalledPackage )
-- @-node:gcross.20100903104106.2083:InstalledPackage
-- @+node:gcross.20100902134026.2122:PackageDatabase
data PackageDatabase = PackageDatabase
    {   packageDatabaseIndexedByInstalledPackageId :: Map InstalledPackageId InstalledPackage
    ,   packageDatabaseIndexedByPackageNameAndVersion :: Map String [(Version,[InstalledPackage])]
    ,   packageDatabaseIndexedByModuleName :: Map String [InstalledPackage]
    } deriving Typeable
-- @-node:gcross.20100902134026.2122:PackageDatabase
-- @+node:gcross.20100902134026.2105:KnownModule
data KnownModule =
    KnownModuleInExternalPackage String
 |  KnownModuleInProject
    {   knownModuleInterfaceFilePath :: FilePath
    ,   knownModuleInterfaceJobId :: JobId
    ,   knownModuleObjectFilePath :: FilePath
    }
-- @-node:gcross.20100902134026.2105:KnownModule
-- @+node:gcross.20100630111926.1862:KnownModules
type KnownModules = Map String KnownModule
-- @-node:gcross.20100630111926.1862:KnownModules
-- @+node:gcross.20100901145855.2053:BuildEnvironment
data BuildEnvironment = BuildEnvironment
    {   buildEnvironmentGHC :: GHC
    ,   buildEnvironmentPackageDatabase :: PackageDatabase
    ,   buildEnvironmentKnownModules :: KnownModules
    ,   buildEnvironmentBuiltModules :: [BuiltModule]
    ,   buildEnvironmentLookupDependencyJobId :: Dependency → Maybe JobId
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
-- @+node:gcross.20100902220655.2075:BuiltProgram
data BuiltProgram = BuiltProgram
    {   builtProgramFilePath :: FilePath
    ,   builtProgramJobId :: JobId
    }
-- @-node:gcross.20100902220655.2075:BuiltProgram
-- @+node:gcross.20100709210816.2222:ProgramComponents
data ProgramComponents = ProgramComponents
    {   programComponentObjectFilePaths :: [FilePath]
    ,   programComponentLibraries :: [String]
    ,   programComponentPackages :: [String]
    ,   programComponentDependencyDigests :: [MD5Digest]
    } deriving (Eq, Typeable)
-- @-node:gcross.20100709210816.2222:ProgramComponents
-- @+node:gcross.20100830091258.2027:GHC
data GHC = GHC
    {   ghcVersion :: Version
    ,   ghcDirectory :: FilePath
    ,   pathToGHC :: FilePath
    ,   pathToGHCPkg :: FilePath
    } deriving Typeable; $( derive makeBinary ''GHC )
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
-- @-node:gcross.20100630111926.1861:Types
-- @+node:gcross.20100709210816.2105:Instances
-- @+node:gcross.20100709210816.2106:Show BuiltModule
instance Show BuiltModule where
    show BuiltModule{..} =
        builtModuleName ++ ": " ++ builtModuleSourceFilePath ++ " -> " ++ builtModuleObjectFilePath ++ ", " ++ builtModuleInterfaceFilePath
-- @-node:gcross.20100709210816.2106:Show BuiltModule
-- @+node:gcross.20100902134026.2123:Binary ProgramComponents
$( derive makeBinary ''ProgramComponents )
-- @-node:gcross.20100902134026.2123:Binary ProgramComponents
-- @-node:gcross.20100709210816.2105:Instances
-- @+node:gcross.20100628115452.1853:Functions
-- @+node:gcross.20100708102250.2756:builtModulesToKnownModules
builtModulesToKnownModules :: [BuiltModule] → KnownModules
builtModulesToKnownModules =
    Map.fromList
    .
    map (
        builtModuleName
        &&&
        liftA3 KnownModuleInProject
            builtModuleInterfaceFilePath
            builtModuleInterfaceJobId
            builtModuleObjectFilePath
    )
-- @-node:gcross.20100708102250.2756:builtModulesToKnownModules
-- @+node:gcross.20100902220655.2076:builtProgram
builtProgram :: FilePath → BuiltProgram
builtProgram program_filepath =
    BuiltProgram
        program_filepath
        (programJobId program_filepath ("Building program " ++ program_filepath))
-- @-node:gcross.20100902220655.2076:builtProgram
-- @+node:gcross.20100901145855.2052:checkForSatisfyingPackage
checkForSatisfyingPackage :: PackageDatabase → Package.Dependency → Bool
checkForSatisfyingPackage package_database dependency = isJust (findSatisfyingPackage package_database dependency)
-- @-node:gcross.20100901145855.2052:checkForSatisfyingPackage
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
    ,   buildEnvironmentPackageDatabase = ghcEnvironmentPackageDatabase
    ,   buildEnvironmentKnownModules =
            Map.unions
                (builtModulesToKnownModules built_modules
                :map extractKnownModulesFromInstalledPackage installed_package_dependencies
                )
    ,   buildEnvironmentBuiltModules = built_modules
    ,   buildEnvironmentLookupDependencyJobId = lookupDependencyJobIdInBuiltModules built_modules
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
    shared_options =
        ("-i" ++ interface_directory)
        :
        build_for_package_options
-- @-node:gcross.20100901145855.2054:computeBuildEnvironment
-- @+node:gcross.20100709210816.2223:computeProgramComponents
computeProgramComponents :: [(Dependency,Maybe MD5Digest)] → ProgramComponents
computeProgramComponents dependencies
  | (not . null) unrecognized_runtime_dependencies =
        throw
        $
        UnrecognizedRuntimes
            "GHC program linker"
            unrecognized_runtime_dependencies
  | otherwise =
        ProgramComponents
        {   programComponentObjectFilePaths = map fst object_dependencies
        ,   programComponentLibraries = map fst library_dependencies
        ,   programComponentPackages = map fst haskell_package_dependencies
        ,   programComponentDependencyDigests = catMaybes . map snd . concat $ all_dependencies
        }

  where
    all_dependencies@[runtime_dependencies,object_dependencies,library_dependencies,haskell_package_dependencies] =
        classifyTaggedDependenciesAndRejectUnrecognizedTypes
            "GHC program linker"
            [runtime_dependency_type,object_dependency_type,library_dependency_type,haskell_package_dependency_type]
            dependencies

    unrecognized_runtime_dependencies =
        [ dependency_name
        | (dependency_name,_) ← runtime_dependencies
        , dependency_name /= ghc_runtime_dependency_name
        ]
-- @-node:gcross.20100709210816.2223:computeProgramComponents
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
-- @+node:gcross.20100903104106.2084:constructPackageDatabaseFromInstalledPackages
constructPackageDatabaseFromInstalledPackages :: [InstalledPackage] → PackageDatabase
constructPackageDatabaseFromInstalledPackages =
    liftA3 PackageDatabase
        (Map.fromList . map (installedPackageId &&& id))
        (Map.fromList
            .
            map (second (gather . map (installedPackageVersion &&& id)))
            .
            gather
            .
            map (installedPackageName &&& id)
        )
        (\installed_packages →
            Map.fromList
            .
            gather
            $
            [ (module_name,installed_package)
            | installed_package@InstalledPackage{..} ← installed_packages
            , module_name ← installedPackageModules
            ]
        )
-- @-node:gcross.20100903104106.2084:constructPackageDatabaseFromInstalledPackages
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
    \maybe_old_search_paths → do
        liftIO . noticeM "Blueprint.Tools.Compilers.GHC" $ "Looking for GHC..."
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
-- @+node:gcross.20100630111926.1874:createGHCCompileToObjectJob
createGHCCompileToObjectJob ::
    FilePath →
    FilePath →
    PackageDatabase →
    KnownModules →
    [String] →
    BuiltModule →
    ToolJob
createGHCCompileToObjectJob
    path_to_ghc
    path_to_ghc_pkg
    package_database
    known_modules
    options_arguments
    BuiltModule{..}
    =
    jobWithCache [builtModuleObjectJobId,builtModuleInterfaceJobId]
    .
    runJobAnalyzer
    .
    fmap (zipWith ($) [postprocessInterface,postprocessObject])
    $
    analyzeImplicitDependenciesAndRebuildIfNecessary
        scanner
        resolve
        builder
        (liftIO . checkDigestsOfFilesIfExisting [builtModuleObjectFilePath,builtModuleInterfaceFilePath])
        options_arguments
        [builtModuleSourceJobId]
  where
    builder dependencies = liftIO $ do
        let [package_names,_] =
                classifyDependenciesAndRejectUnrecognizedTypes
                    "GHC object compiler"
                    [haskell_package_dependency_type,haskell_interface_dependency_type]
                    dependencies
        noticeM "Blueprint.Tools.Compilers.GHC" $
            "(GHC) Compiling "
            ++ builtModuleSourceFilePath ++
            " --> "
            ++ builtModuleObjectFilePath ++
            " / "
            ++ builtModuleInterfaceFilePath
        let ghc_arguments =
                 "-c":builtModuleSourceFilePath
                :"-o":builtModuleObjectFilePath
                :"-ohi":builtModuleInterfaceFilePath
                :
                concatMap (("-package":) . (:[])) package_names
                ++
                options_arguments
        infoM "Blueprint.Tools.Compilers.GHC" $
            "(GHC) Executing '" ++ (unwords (path_to_ghc:ghc_arguments)) ++ "'"
        runProductionCommandAndDigestOutputs
            [builtModuleObjectFilePath,builtModuleInterfaceFilePath]
            []
            path_to_ghc
            ghc_arguments

    postprocessInterface =
        setFilePath builtModuleInterfaceFilePath
        .
        addDeferredDependency (objectDependency builtModuleObjectFilePath)
    postprocessObject =
        setFilePath builtModuleObjectFilePath
        .
        addDeferredDependency ghc_runtime_dependency

    scanner =
        fmap extractImportedModulesFromHaskellSource
        .
        liftIO
        .
        L.readFile
        $
        builtModuleSourceFilePath

    resolve =
        return
        .
        extractRequiredDependenciesOrError
        .
        map (resolveModuleDependency package_database known_modules)
-- @-node:gcross.20100630111926.1874:createGHCCompileToObjectJob
-- @+node:gcross.20100901145855.2080:createGHCCompileToObjectJobsFromBuildEnvironment
createGHCCompileToObjectJobsFromBuildEnvironment :: BuildEnvironment → [ToolJob]
createGHCCompileToObjectJobsFromBuildEnvironment BuildEnvironment{..} =
    map (
        createGHCCompileToObjectJob
            (pathToGHC buildEnvironmentGHC)
            (pathToGHCPkg buildEnvironmentGHC)
            buildEnvironmentPackageDatabase
            buildEnvironmentKnownModules
            buildEnvironmentCompileOptions
    ) buildEnvironmentBuiltModules
-- @-node:gcross.20100901145855.2080:createGHCCompileToObjectJobsFromBuildEnvironment
-- @+node:gcross.20100903104106.2080:createGHCFetchDeferredDependencesAndLinkProgramJobs
createGHCFetchDeferredDependencesAndLinkProgramJobs ::
    FilePath →
    [String] →
    BuiltProgram →
    (Dependency → Maybe JobId) →
    [Dependency] →
    [ToolJob]
createGHCFetchDeferredDependencesAndLinkProgramJobs
    path_to_ghc
    options_arguments
    built_program@BuiltProgram{..}
    lookupDependencyJobIds
    starting_dependencies
    =
    fmap
        (computeProgramComponents . Map.toList)
        (fetchAllDeferredDependenciesAndTheirDigests
            builtProgramFilePath
            lookupDependencyJobIds
            starting_dependencies
        )
    ➠
    [createGHCLinkProgramIncompleteJob
        path_to_ghc
        options_arguments
        built_program
    ]
-- @-node:gcross.20100903104106.2080:createGHCFetchDeferredDependencesAndLinkProgramJobs
-- @+node:gcross.20100705132935.1938:createGHCLinkProgramIncompleteJob
createGHCLinkProgramIncompleteJob ::
    FilePath →
    [String] →
    BuiltProgram →
    IncompleteToolJob ProgramComponents
createGHCLinkProgramIncompleteJob
    path_to_ghc
    options_arguments
    BuiltProgram{..}
    =
    incompleteJobWithCache [builtProgramJobId]
    $
    \program_components@ProgramComponents{..} →
        let ghc_arguments =
                programComponentObjectFilePaths
                ++
                map (('-':) . ('l':)) programComponentLibraries -- '
                ++
                concat [["-package",package] | package ← programComponentPackages]
                ++
                ["-o",builtProgramFilePath]
                ++
                options_arguments
            builder = liftIO $ do
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
        in  runJobAnalyzer
            .
            fmap (zipWith ($) [setFilePath builtProgramFilePath])
            $
            compareToCacheAndRebuildIfNecessary
                builder
                (liftIO . checkDigestsOfFilesIfExisting [builtProgramFilePath])
                program_components
-- @-node:gcross.20100705132935.1938:createGHCLinkProgramIncompleteJob
-- @+node:gcross.20100831211145.2141:createLoadGHCPackageDatabaseIncompleteJob
createLoadGHCPackageDatabaseIncompleteJob ::
    String →
    IncompleteJob JobId Dynamic FilePath
createLoadGHCPackageDatabaseIncompleteJob job_distinguisher =
    incompleteJobWithCache [identifierInNamespace ghc_package_database_namespace job_distinguisher "package database"]
    $
    \path_to_ghc_pkg maybe_cache → do
        liftIO . infoM "Blueprint.Tools.Compilers.GHC" $
            "(GHC) Reading package atoms..."
        liftIO . noticeM "Blueprint.Tools.Compilers.GHC" $
            unwords ("(GHC) Executing":path_to_ghc_pkg:["--simple-output","list"])
        list_of_package_atoms ← fmap words (liftIO $ readProcess path_to_ghc_pkg ["--simple-output","list"] "")
        installed_packages ← case maybe_cache of
            Just cache@(old_list_of_package_atoms,installed_packages)
                | old_list_of_package_atoms == list_of_package_atoms → return installed_packages
            _ → liftIO $
                    infoM "Blueprint.Tools.Compilers.GHC" "Loading package database..."
                    >>
                    mapM (Thread.forkIO . loadInstalledPackageInformation path_to_ghc_pkg) list_of_package_atoms
                    >>=
                    mapM ((>>= Thread.unsafeResult) . snd)
        let package_database = constructPackageDatabaseFromInstalledPackages installed_packages
        returnWrappedValueAndCache package_database (list_of_package_atoms,installed_packages)
-- @-node:gcross.20100831211145.2141:createLoadGHCPackageDatabaseIncompleteJob
-- @+node:gcross.20100831211145.2155:createLoadGHCPackageDatabaseJob
createLoadGHCPackageDatabaseJob ::
    String →
    FilePath →
    Job JobId Dynamic
createLoadGHCPackageDatabaseJob = flip completeJobWith . createLoadGHCPackageDatabaseIncompleteJob
-- @-node:gcross.20100831211145.2155:createLoadGHCPackageDatabaseJob
-- @+node:gcross.20100901145855.2063:extractKnownModulesFromInstalledPackage
extractKnownModulesFromInstalledPackage :: InstalledPackage → KnownModules
extractKnownModulesFromInstalledPackage InstalledPackage{..} =
    Map.fromList
    .
    map (,KnownModuleInExternalPackage installedPackageQualifiedName)
    $
    installedPackageModules
-- @nonl
-- @-node:gcross.20100901145855.2063:extractKnownModulesFromInstalledPackage
-- @+node:gcross.20100901145855.2056:extractPackageDependencies
extractPackageDependencies :: PackageDescription → BuildTargetType → [Package.Dependency]
extractPackageDependencies PackageDescription{..} build_target_type = buildDepends ++
    case build_target_type of
        LibraryTarget → targetBuildDepends . libBuildInfo . fromJust $ library
        ExecutableTarget → concat . map (targetBuildDepends . buildInfo) $ executables
-- @-node:gcross.20100901145855.2056:extractPackageDependencies
-- @+node:gcross.20100901145855.2050:findSatisfyingPackage
findSatisfyingPackage :: PackageDatabase → Package.Dependency → Maybe InstalledPackage
findSatisfyingPackage PackageDatabase{..} (Package.Dependency name version_range) =
    Map.lookup (display name) packageDatabaseIndexedByPackageNameAndVersion
    >>=
    find (flip withinRange version_range . fst)
    >>=
    return . head . snd
-- @-node:gcross.20100901145855.2050:findSatisfyingPackage
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
    interface_file_path = interface_subdirectory </> haskell_module_path <.> "hi"
    display_name = "Compile " ++ haskellSourceModuleName
-- @-node:gcross.20100708192404.2001:haskellSourceToBuiltModule
-- @+node:gcross.20100708215239.2093:interfaceJobId
interfaceJobId = identifierInNamespace interface_namespace
-- @-node:gcross.20100708215239.2093:interfaceJobId
-- @+node:gcross.20100831211145.2139:loadInstalledPackageInformation
loadInstalledPackageInformation :: FilePath → String → IO InstalledPackage
loadInstalledPackageInformation path_to_ghc_pkg package_atom = do
    package_description ← readProcess path_to_ghc_pkg ["describe",package_atom] ""
    InstalledPackageInfo.InstalledPackageInfo{..} ←
        case InstalledPackageInfo.parseInstalledPackageInfo package_description of
            ParseOk _ installed_package_info → return installed_package_info
            ParseFailed parse_error → error $ "Error parsing description of package " ++ package_atom ++ ": " ++ show parse_error
    return $
        InstalledPackage
        {   installedPackageId = installedPackageId
        ,   installedPackageQualifiedName = display sourcePackageId
        ,   installedPackageName = (display . Package.pkgName) sourcePackageId
        ,   installedPackageVersion = Package.pkgVersion sourcePackageId
        ,   installedPackageModules = map display exposedModules
        }
-- @-node:gcross.20100831211145.2139:loadInstalledPackageInformation
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
-- @+node:gcross.20100901145855.2058:lookupDependencyJobIdInBuiltModules
lookupDependencyJobIdInBuiltModules :: [BuiltModule] → (Dependency → Maybe JobId)
lookupDependencyJobIdInBuiltModules =
    flip Map.lookup
    .
    Map.fromList
    .
    liftA2 (++)
        (map ((objectDependency . builtModuleObjectFilePath) &&& builtModuleObjectJobId))
        (map ((haskellInterfaceDependency . builtModuleInterfaceFilePath) &&& builtModuleInterfaceJobId))
-- @-node:gcross.20100901145855.2058:lookupDependencyJobIdInBuiltModules
-- @+node:gcross.20100901145855.2088:lookupPackageNamed
lookupPackageNamed :: PackageDatabase → Package.PackageName → Maybe [(Version,[InstalledPackage])]
lookupPackageNamed PackageDatabase{..} =
    flip Map.lookup packageDatabaseIndexedByPackageNameAndVersion
    .
    display
-- @-node:gcross.20100901145855.2088:lookupPackageNamed
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
-- @+node:gcross.20100630111926.1863:resolveModuleDependency
resolveModuleDependency :: PackageDatabase → KnownModules → String → Either UnknownDependency RequiredDependencies
resolveModuleDependency PackageDatabase{..} known_modules module_name =
    case Map.lookup module_name known_modules of
        Just (KnownModuleInExternalPackage package_name) →
            Right
            $
            RequiredDependencies
                [(haskellPackageDependency package_name,Nothing)]
                [haskellPackageDependency package_name]
        Just KnownModuleInProject{..} →
            Right
            $
            RequiredDependencies
                [(haskellInterfaceDependency knownModuleInterfaceFilePath,Just knownModuleInterfaceJobId)]
                [objectDependency knownModuleObjectFilePath]
        Nothing →
            Left
            .
            UnknownDependency (haskellModuleDependency module_name)
            .
            fmap (DependencyExporters haskell_package_dependency_type . map installedPackageQualifiedName)
            $
            Map.lookup module_name packageDatabaseIndexedByModuleName
-- @-node:gcross.20100630111926.1863:resolveModuleDependency
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
