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
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Monad.Trans.Abort
import Control.Monad.Trans.Goto
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
import Data.List.Split
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

import Text.Printf
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
import Blueprint.Options
import Blueprint.SourceFile
import Blueprint.Tools
import Blueprint.Tools.JobAnalyzer
-- @nonl
-- @-node:gcross.20100611224425.1612:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100905161144.1949:Exceptions
-- @+node:gcross.20100905161144.1951:GHCConfigurationException
data GHCConfigurationException =
    GHCVersionParseException FilePath String
  | GHCNotFoundAt FilePath
  | GHCPkgNotFoundAt FilePath
  | GHCUnableToLocateGHC [FilePath]
  | GHCUnableToLocateGHCPkg [FilePath]
  | GHCVersionsDontMatch FilePath Version FilePath Version
  | GHCVersionIsNotDesiredVersion FilePath Version Version
  | GHCUnknownException SomeException
  | GHCMultipleExceptions [GHCConfigurationException]
  deriving (Typeable)

instance Show GHCConfigurationException where
    show (GHCVersionParseException program output) =
        printf "Version string output by %s was \"%s\", which does not parse."
            program
            output
    show (GHCNotFoundAt filepath) =
         "No file for ghc was found at " ++ filepath
    show (GHCPkgNotFoundAt filepath) =
         "No file for ghc-pkg was found at " ++ filepath
    show (GHCUnableToLocateGHC search_paths) =
        "Unable to find ghc in " ++ show search_paths
    show (GHCUnableToLocateGHCPkg search_paths) =
        "Unable to find ghc-pkg in " ++ show search_paths
    show (GHCVersionsDontMatch path_to_ghc ghc_version path_to_ghc_pkg ghc_pkg_version) =
        printf "ghc (at %s) reported version %s, but ghc-pkg (at %s) reported version %s"
            path_to_ghc
            (display ghc_version)
            path_to_ghc_pkg
            (display ghc_pkg_version)
    show (GHCVersionIsNotDesiredVersion path_to_ghc desired_version found_version) =
        printf "GHC (at %s) reported version %s, which is not the required version (%s)"
            path_to_ghc
            (display desired_version)
            (display found_version)
    show (GHCUnknownException e) =
        "Unknown exception: " ++ show e
    show (GHCMultipleExceptions exceptions) =
        "Unable to configure GHC:\n" ++ concat ["* " ++ show e ++ "\n" | e ← exceptions]

instance Exception GHCConfigurationException
-- @-node:gcross.20100905161144.1951:GHCConfigurationException
-- @-node:gcross.20100905161144.1949:Exceptions
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
-- @+node:gcross.20100905161144.1955:PackageLocality
data PackageLocality = Global | User deriving (Typeable,Eq)

$(derive makeBinary ''PackageLocality)

instance Show PackageLocality where
    show User = "user"
    show Global = "global"
-- @-node:gcross.20100905161144.1955:PackageLocality
-- @+node:gcross.20100830091258.2027:GHC
data GHC = GHC
    {   ghcVersion :: Version
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
-- @+node:gcross.20100905161144.1935:GHCOptions
data GHCOptions = GHCOptions
    {   ghcOptionPathToGHC :: Maybe FilePath
    ,   ghcOptionPathToGHCPkg :: Maybe FilePath
    ,   ghcOptionDesiredVersion :: Maybe Version
    ,   ghcOptionSearchPaths :: [FilePath]
    } deriving (Eq,Show,Typeable)

$(derive makeBinary ''GHCOptions)
-- @nonl
-- @-node:gcross.20100905161144.1935:GHCOptions
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
    String →
    GHCOptions →
    JobApplicative JobId Dynamic GHC
configureGHC job_distinguisher search_options =
    JobApplicative
    {   jobApplicativeJobs = [job]
    ,   jobApplicativeResultJobNames = job_names
    ,   jobApplicativeResultExtractorJobName = identifierInNamespace ghc_configuration_namespace "configure GHC" "configure GHC"
    ,   jobApplicativeResultExtractor = fromJust . fromDynamic . head
    }
  where
    job@(Job job_names job_runner) = createGHCConfigurationJob job_distinguisher search_options
-- @nonl
-- @-node:gcross.20100831154015.2037:configureGHC
-- @+node:gcross.20100905161144.1956:configureGHCUsingOptions
configureGHCUsingOptions :: OptionValues → JobApplicative JobId Dynamic GHC
configureGHCUsingOptions = configureGHC "" . extractGHCOptions
-- @-node:gcross.20100905161144.1956:configureGHCUsingOptions
-- @+node:gcross.20100831211145.2170:configureGHCEnvironment
configureGHCEnvironment ::
    String →
    GHCOptions →
    JobApplicative JobId Dynamic GHCEnvironment
configureGHCEnvironment job_distinguisher search_options =
    configureGHC job_distinguisher search_options
    ➤
    proc ghc → do
        package_database <- configurePackageDatabase job_distinguisher -< ghc
        returnA -< GHCEnvironment ghc package_database
-- @-node:gcross.20100831211145.2170:configureGHCEnvironment
-- @+node:gcross.20100905161144.1957:configureGHCEnvironmentUsingOptions
configureGHCEnvironmentUsingOptions :: OptionValues → JobApplicative JobId Dynamic GHCEnvironment
configureGHCEnvironmentUsingOptions = configureGHCEnvironment "". extractGHCOptions
-- @-node:gcross.20100905161144.1957:configureGHCEnvironmentUsingOptions
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
-- @+node:gcross.20100830091258.2033:createGHCConfigurationJob
createGHCConfigurationJob ::
    String →
    GHCOptions →
    Job JobId Dynamic
createGHCConfigurationJob
    job_distinguisher
    search_options@GHCOptions{..}
    =
    jobWithCache
        [identifierInNamespace ghc_configuration_namespace job_distinguisher "GHC configuration"]
        (liftIO . configureIt >=> \ghc → returnWrappedValueAndCache ghc (search_options,ghc))
 where
    configureIt :: Maybe (GHCOptions,GHC) → IO GHC
    configureIt maybe_old_search_paths
      | Just (old_search_options,old_ghc) ← maybe_old_search_paths
      , old_search_options == search_options
        = return old_ghc
      | Just path_to_ghc ← ghcOptionPathToGHC
      , Just path_to_ghc_pkg ← ghcOptionPathToGHCPkg
        = configureUsingBothPaths path_to_ghc path_to_ghc_pkg
      | Just path_to_ghc ← ghcOptionPathToGHC
        = configureUsingGHCPath path_to_ghc
      | Just path_to_ghc_pkg ← ghcOptionPathToGHCPkg
        = configureUsingGHCPkgPath path_to_ghc_pkg
      | [] ← ghcOptionSearchPaths
        = getSearchPath >>= configureUsingSearchPaths
      | otherwise
        = configureUsingSearchPaths ghcOptionSearchPaths

    configureUsingSearchPaths :: [FilePath] → IO GHC
    configureUsingSearchPaths search_paths = runAbortT $ do
        exceptions ←
            forM search_paths $ \search_path → do
                ghc_or_error ← liftIO . try . configureUsingGHCPath . (</> "ghc") $ search_path
                case ghc_or_error of
                    Right (ghc :: GHC) → abort ghc
                    Left e
                      | Just (e_ :: GHCConfigurationException) ← fromException e → return e_
                      | otherwise → liftIO $ throwIO e
        let (ghc_not_found_locations,other_exceptions) =
                partitionEithers
                .
                map (\e →
                    case e of
                        GHCNotFoundAt path → Left path
                        e → Right e
                )
                $
                exceptions
            final_exceptions =
                (case ghc_not_found_locations of
                    [] → id
                    search_path → (GHCUnableToLocateGHC search_paths:)
                )
                other_exceptions
        liftIO . throwIO $
            case final_exceptions of
                [e] → e
                _ → GHCMultipleExceptions final_exceptions

    configureUsingGHCPath :: FilePath → IO GHC
    configureUsingGHCPath path_to_ghc = runGotoT $ do
        exists ← liftIO . doesFileExist $ path_to_ghc
        unless exists . liftIO . throwIO . GHCNotFoundAt $ path_to_ghc
        mapM_ tryGHCPkgPath paths_to_try >> (liftIO . throwIO . GHCUnableToLocateGHCPkg $ paths_to_try)
      where
        paths_to_try =
            [replaceFileName path_to_ghc "ghc-pkg"
            ,path_to_ghc ++ "-pkg"
            ]
        tryGHCPkgPath path_to_ghc_pkg = do
            exists ← liftIO . doesFileExist $ path_to_ghc_pkg
            when exists . goto . liftIO $ configureUsingBothPaths path_to_ghc path_to_ghc_pkg

    configureUsingGHCPkgPath :: FilePath → IO GHC
    configureUsingGHCPkgPath path_to_ghc_pkg = runGotoT $ do
        exists ← liftIO . doesFileExist $ path_to_ghc_pkg
        unless exists . liftIO . throwIO . GHCPkgNotFoundAt $ path_to_ghc_pkg
        mapM_ tryGHCPath paths_to_try >> (liftIO . throwIO . GHCUnableToLocateGHC $ paths_to_try)
      where
        paths_to_try =
            replaceFileName path_to_ghc_pkg "ghc"
            :
            case split (onSublist "-pkg") path_to_ghc_pkg of
                (x:y:rest) → [x ++ concat rest]
                _ → []
        tryGHCPath path_to_ghc = do
            exists ← liftIO . doesFileExist $ path_to_ghc
            when exists . goto . liftIO $ configureUsingBothPaths path_to_ghc path_to_ghc_pkg

    configureUsingBothPaths :: FilePath → FilePath → IO GHC
    configureUsingBothPaths path_to_ghc path_to_ghc_pkg = do
        ghc_version ← determineGHCVersionOrRethrow path_to_ghc
        ghc_pkg_version ← determineGHCVersionOrRethrow path_to_ghc_pkg
        unless (ghc_version == ghc_pkg_version) $
            throwIO (GHCVersionsDontMatch path_to_ghc ghc_version path_to_ghc_pkg ghc_pkg_version)
        case ghcOptionDesiredVersion of
            Just desired_version
              | desired_version /= ghc_version
                → throwIO (GHCVersionIsNotDesiredVersion path_to_ghc desired_version ghc_version )
            _ → return ()
        return $
            GHC ghc_version
                path_to_ghc
                path_to_ghc_pkg

    determineGHCVersionOrRethrow :: FilePath → IO Version
    determineGHCVersionOrRethrow filepath = do
        result_or_error ← try (determineGHCVersion filepath)
        case result_or_error of
            Left (BadProgramVersionException _ output) →
                throwIO (GHCVersionParseException filepath output)
            Right version →
                return version
-- @nonl
-- @-node:gcross.20100830091258.2033:createGHCConfigurationJob
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
        -- let arguments = ["--simple-output",("--" ++ show locality),"list"]
        let arguments = ["--simple-output","list"]
        liftIO . noticeM "Blueprint.Tools.Compilers.GHC" $
            unwords ("(GHC) Executing":path_to_ghc_pkg:arguments)
        list_of_package_atoms ← fmap words (liftIO $ readProcess path_to_ghc_pkg arguments "")
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
-- @+node:gcross.20100905161144.1952:extractGHCOptions
extractGHCOptions :: OptionValues → GHCOptions
extractGHCOptions =
    GHCOptions
        <$> Map.lookup ghc_search_option_path_to_ghc
        <*> Map.lookup ghc_search_option_path_to_ghc_pkg
        <*> fmap readVersion . Map.lookup ghc_search_option_desired_version
        <*> maybe [] splitSearchPath . Map.lookup ghc_search_option_search_paths
-- @-node:gcross.20100905161144.1952:extractGHCOptions
-- @+node:gcross.20100905161144.1958:extractGHCPackageLocality
extractGHCPackageLocality :: OptionValues → PackageLocality
extractGHCPackageLocality option_values =
    case Map.lookup ghc_package_database_option_locality option_values of
        Nothing → Global
        Just locality
          | locality == "user" → User
          | locality == "global" → Global
          | otherwise → error $ "GHC package locality must be 'user' or 'global', not '" ++ locality ++ "'."
-- @-node:gcross.20100905161144.1958:extractGHCPackageLocality
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
-- @+node:gcross.20100905161144.1936:parseGHCVersion
parseGHCVersion = extractVersion ghc_version_regex
-- @-node:gcross.20100905161144.1936:parseGHCVersion
-- @+node:gcross.20100905161144.1937:determineGHCVersion
determineGHCVersion :: FilePath → IO Version
determineGHCVersion = determineProgramVersion parseGHCVersion ["--version"]
-- @-node:gcross.20100905161144.1937:determineGHCVersion
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
-- @-node:gcross.20100611224425.1613:Values
-- @+node:gcross.20100905161144.1953:Options
ghc_search_option_path_to_ghc = identifier "8a0b2f67-9ff8-417d-aed0-372149d791d6" "path to ghc"
ghc_search_option_path_to_ghc_pkg = identifier "b832666c-f42f-4dc0-8ef9-561987334c37" "path to ghc-pkg"
ghc_search_option_desired_version = identifier "87cab19e-c87a-4480-8ed3-af04e4c4f6bc" "desired GHC version"
ghc_search_option_search_paths = identifier "fcb54ad5-4a10-419a-9e8c-12261952cfd9" "path to search for ghc"
ghc_package_database_option_locality = identifier "4b38e6c5-8162-49ea-9ca0-5a23e58c44b1" "should the local or global GHC package database be used"

ghcOptions =
    Options
        Map.empty
        (Map.fromList
            [("with-ghc",(ghc_search_option_path_to_ghc,RequiredArgument "PATH"))
            ,("with-ghc-pkg",(ghc_search_option_path_to_ghc_pkg,RequiredArgument "PATH"))
            ,("with-ghc-version",(ghc_search_option_desired_version,RequiredArgument "VERSION"))
            ,("with-ghc-located-in",(ghc_search_option_search_paths,RequiredArgument "DIRECTORY"))
            -- ,("user",(ghc_package_database_option_locality,NoArgument "user"))
            -- ,("global",(ghc_package_database_option_locality,NoArgument "global"))
            ]
        )
        (Map.fromList
            [("tools.ghc.paths.ghc",ghc_search_option_path_to_ghc)
            ,("tools.ghc.paths.ghc-pkg",ghc_search_option_path_to_ghc_pkg)
            ,("tools.ghc.paths.search",ghc_search_option_search_paths)
            ,("tools.ghc.version",ghc_search_option_desired_version)
            -- ,("tools.ghc.package-locality",ghc_package_database_option_locality)
            ]
        )
        (Map.fromList
            [(ghc_search_option_search_paths,Right . intercalate [searchPathSeparator])
            ]
        )
        (Map.fromList
            [(ghc_search_option_path_to_ghc,("GHC","Path to ghc"))
            ,(ghc_search_option_path_to_ghc_pkg,("GHC","Path to ghc-pkg"))
            ,(ghc_search_option_desired_version,("GHC","Required version of GHC"))
            ,(ghc_search_option_search_paths,("GHC","Directories to search for ghc (separated by " ++ [searchPathSeparator] ++ ")"))
            -- ,(ghc_package_database_option_locality,("GHC","Use the (user|global) package database for both configuration and package installation;  defaults to global."))
            ]
        )
-- @-node:gcross.20100905161144.1953:Options
-- @-others
-- @-node:gcross.20100611224425.1610:@thin GHC.hs
-- @-leo
