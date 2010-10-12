-- @+leo-ver=4-thin
-- @+node:gcross.20100927123234.1428:@thin GHC.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100927123234.1429:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100927123234.1429:<< Language extensions >>
-- @nl

module Blueprint.Tools.GHC where

-- @<< Import needed modules >>
-- @+node:gcross.20100927123234.1430:<< Import needed modules >>
import Prelude hiding (sequence)

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad hiding (sequence,mapM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Abort
import Control.Monad.Trans.Goto

import Data.Array
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as LU
import Data.Char
import Data.DeriveTH
import Data.Digest.Pure.MD5
import Data.Either
import Data.Function
import Data.List
import Data.List.Split
import Data.List.Tagged (TaggedList(..),fromT,toT)
import qualified Data.List.Tagged as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (traverse,sequenceA,sequence,mapM)
import Data.Typeable

import qualified Distribution.Compiler as Compiler
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import qualified Distribution.ModuleName as ModuleName
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
import System.FilePath
import System.Log.Logger
import System.Process

import Text.Printf
import Text.Regex.PCRE
import Text.Regex.PCRE.String

import TypeLevel.NaturalNumber (Two)

import Blueprint.Cache
import Blueprint.Configuration
import Blueprint.Identifier
import Blueprint.Job
import Blueprint.Miscellaneous
import Blueprint.Options
import Blueprint.Tools
import Blueprint.Tools.Ar
-- @nonl
-- @-node:gcross.20100927123234.1430:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20101010201506.1505:File Types
declareFileType "HaskellInterface"
declareFileType "HaskellObject"
-- @nonl
-- @-node:gcross.20101010201506.1505:File Types
-- @+node:gcross.20100927123234.1433:Types
-- @+node:gcross.20100929213846.1453:CompileCache
data CompileCache = CompileCache
    {   compileCacheSourceDigest :: MD5Digest
    ,   compileCacheImportedModules :: [String]
    ,   compileCacheDependencyDigests :: Map FilePath MD5Digest
    ,   compileCacheAdditionalOptions :: [String]
    ,   compileCacheInterfaceDigest :: MD5Digest
    ,   compileCacheObjectDigest :: MD5Digest
    } deriving Typeable; $( derive makeBinary ''CompileCache )
-- @nonl
-- @-node:gcross.20100929213846.1453:CompileCache
-- @+node:gcross.20101009103525.1721:ForLibrary
data ForLibrary
-- @nonl
-- @-node:gcross.20101009103525.1721:ForLibrary
-- @+node:gcross.20101009103525.1723:ForPrograms
data ForPrograms
-- @nonl
-- @-node:gcross.20101009103525.1723:ForPrograms
-- @+node:gcross.20100927123234.1441:GHC
data GHC = GHC
    {   ghcVersion :: Version
    ,   pathToGHC :: FilePath
    ,   pathToGHCPkg :: FilePath
    } deriving Typeable; $( derive makeBinary ''GHC )
-- @-node:gcross.20100927123234.1441:GHC
-- @+node:gcross.20100927123234.1445:GHCOptions
data GHCOptions = GHCOptions
    {   ghcOptionPathToGHC :: Maybe FilePath
    ,   ghcOptionPathToGHCPkg :: Maybe FilePath
    ,   ghcOptionDesiredVersion :: Maybe Version
    ,   ghcOptionSearchPaths :: [FilePath]
    } deriving (Eq,Show,Typeable)

$(derive makeBinary ''GHCOptions)
-- @-node:gcross.20100927123234.1445:GHCOptions
-- @+node:gcross.20101009103525.1726:HaskellLibrary
data HaskellLibrary = HaskellLibrary
    {   haskellLibraryModuleInterfaces :: Map String HaskellInterfaceFile
    ,   haskellLibraryArchive :: ArchiveFile
    ,   haskellLibraryDigest :: MD5Digest
    ,   haskellLibraryDependencyPackages :: [InstalledPackage]
    ,   haskellLibraryIsExposed :: Bool
    ,   haskellLibraryExposedModules :: [String]
    } deriving Typeable
-- @-node:gcross.20101009103525.1726:HaskellLibrary
-- @+node:gcross.20101009103525.1724:HaskellModule
data HaskellModule = HaskellModule
    {   haskellModuleName :: String
    ,   haskellModuleInterface :: HaskellInterfaceFile
    ,   haskellModuleObject :: HaskellObjectFile
    ,   haskellModuleLinkDependencyModules :: Map String HaskellModule
    ,   haskellModuleLinkDependencyPackages :: Map String InstalledPackage
    } deriving Typeable
-- @-node:gcross.20101009103525.1724:HaskellModule
-- @+node:gcross.20101009103525.1725:HaskellModuleJobs
type HaskellModuleJobs = Map String (Job HaskellModule)
-- @nonl
-- @-node:gcross.20101009103525.1725:HaskellModuleJobs
-- @+node:gcross.20100927222551.1452:HaskellSource
data HaskellSource = HaskellSource
    {   haskellSourceModuleName :: String
    ,   haskellSourceFile :: HaskellSourceFile
    } deriving Typeable
type HaskellSourceFile = FileOfType HaskellSource
-- @nonl
-- @-node:gcross.20100927222551.1452:HaskellSource
-- @+node:gcross.20100927123234.1435:InstalledPackage
data InstalledPackage = InstalledPackage
    {   installedPackageId :: InstalledPackageId
    ,   installedPackageQualifiedName :: String
    ,   installedPackageName :: String
    ,   installedPackageVersion :: Version
    ,   installedPackageModules :: [String]
    } deriving (Typeable, Eq);

$( derive makeBinary ''InstalledPackageId )
$( derive makeBinary ''InstalledPackage )
-- @-node:gcross.20100927123234.1435:InstalledPackage
-- @+node:gcross.20100927222551.1434:KnownModule
data KnownModule =
    KnownModuleInExternalPackage InstalledPackage
  | KnownModuleInProject (Job HaskellModule)
-- @-node:gcross.20100927222551.1434:KnownModule
-- @+node:gcross.20100927222551.1436:KnownModules
type KnownModules = Map String KnownModule
-- @-node:gcross.20100927222551.1436:KnownModules
-- @+node:gcross.20100927123234.1437:PackageDatabase
data PackageDatabase = PackageDatabase
    {   packageDatabaseIndexedByInstalledPackageId :: Map InstalledPackageId InstalledPackage
    ,   packageDatabaseIndexedByPackageNameAndVersion :: Map String [(Version,[InstalledPackage])]
    ,   packageDatabaseIndexedByModuleName :: Map String [InstalledPackage]
    } deriving Typeable
-- @-node:gcross.20100927123234.1437:PackageDatabase
-- @+node:gcross.20100927123234.1439:PackageLocality
data PackageLocality = Global | User deriving (Typeable,Eq)

$(derive makeBinary ''PackageLocality)

instance Show PackageLocality where
    show User = "user"
    show Global = "global"
-- @-node:gcross.20100927123234.1439:PackageLocality
-- @+node:gcross.20100927123234.1443:GHCEnvironment
data GHCEnvironment = GHCEnvironment
    {   ghcEnvironmentGHC :: GHC
    ,   ghcEnvironmentPackageDatabase :: PackageDatabase
    } deriving Typeable

-- @-node:gcross.20100927123234.1443:GHCEnvironment
-- @+node:gcross.20100927222551.1446:BuildEnvironment
data BuildEnvironment α = BuildEnvironment
    {   buildEnvironmentGHC :: GHC
    ,   buildEnvironmentPackageDatabase :: PackageDatabase
    ,   buildEnvironmentKnownModules :: KnownModules
    ,   buildEnvironmentCompileOptions :: [String]
    ,   buildEnvironmentLinkOptions :: [String]
    ,   buildEnvironmentInterfaceDirectory :: FilePath
    ,   buildEnvironmentObjectDirectory :: FilePath
    }
-- @nonl
-- @-node:gcross.20100927222551.1446:BuildEnvironment
-- @+node:gcross.20101009103525.1720:BuildEnvironments
data BuildEnvironments = BuildEnvironments
    {   buildEnvironmentForLibrary :: BuildEnvironment ForLibrary
    ,   buildEnvironmentForPrograms :: BuildEnvironment ForPrograms
    }
-- @-node:gcross.20101009103525.1720:BuildEnvironments
-- @-node:gcross.20100927123234.1433:Types
-- @+node:gcross.20101005111309.1477:Exceptions
-- @+node:gcross.20101010201506.1499:BadLibraryExportList
data BadLibraryExportList = BadLibraryExportList [Either String String] deriving Typeable

instance Show BadLibraryExportList where
    show (BadLibraryExportList bad_exports) =
        case unknown_modules of
            [] → ""
            _ → unlines
                    ("The following modules in the exposed list are unknown:"
                    :map ('\t':) unknown_modules -- '
                    )
                ++ "\n"
        ++
        case external_modules of
            [] → ""
            _ → unlines
                    ("The following modules in the exposed list are external to the project:"
                    :map ('\t':) external_modules -- '
                    )
                ++ "\n"
      where
        (unknown_modules,external_modules) = partitionEithers bad_exports

instance Exception BadLibraryExportList
-- @-node:gcross.20101010201506.1499:BadLibraryExportList
-- @+node:gcross.20101005111309.1478:GHCConfigurationException
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
-- @-node:gcross.20101005111309.1478:GHCConfigurationException
-- @+node:gcross.20101010201506.1496:LibraryMissingFromPackageDescription
data LibraryMissingFromPackageDescription = LibraryMissingFromPackageDescription PackageDescription deriving Typeable

instance Show LibraryMissingFromPackageDescription where
    show (LibraryMissingFromPackageDescription package_description) =
        "A library section is missing from the package description:\n" ++ show package_description

instance Exception LibraryMissingFromPackageDescription
-- @-node:gcross.20101010201506.1496:LibraryMissingFromPackageDescription
-- @+node:gcross.20101010201506.1498:MissingModuleNames
data MissingExposedModules = MissingExposedModules (Set String) deriving Typeable

instance Show MissingExposedModules where
    show (MissingExposedModules missing_module_names) =
        unlines
        .
        ("The following exposed modules are missing:":)
        .
        map ('\t':) -- '
        .
        Set.elems
        $
        missing_module_names

instance Exception MissingExposedModules
-- @nonl
-- @-node:gcross.20101010201506.1498:MissingModuleNames
-- @+node:gcross.20101005111309.1479:UnknownModulesException
data UnknownModulesException = UnknownModulesException [(String,[String])] deriving Typeable

instance Show UnknownModulesException where
    show (UnknownModulesException unknown_modules_with_exporters) =
        intercalate "\n"
        .
        nub
        .
        map (\(module_name,exporters) →
            "Unable to find module '" ++ module_name ++ "'" ++
            case exporters of
                [] → ""
                exporters → " (but it is exported by the " ++ show exporters ++ ")"
        )
        $
        unknown_modules_with_exporters

instance Exception UnknownModulesException
-- @nonl
-- @-node:gcross.20101005111309.1479:UnknownModulesException
-- @+node:gcross.20101005111309.1480:UnresolvedPackageDependenciesError
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
-- @-node:gcross.20101005111309.1480:UnresolvedPackageDependenciesError
-- @-node:gcross.20101005111309.1477:Exceptions
-- @+node:gcross.20100927123234.1459:Values
-- @+node:gcross.20100927123234.1461:ghc_version_regex
ghc_version_regex = makeRegex "version ([0-9.]*)" :: Regex
-- @-node:gcross.20100927123234.1461:ghc_version_regex
-- @+node:gcross.20100927222551.1456:import_regex
import_regex :: Regex
import_regex = makeRegex "^\\s*import\\s+(?:qualified\\s+)?([A-Z][A-Za-z0-9_.]*)"
-- @-node:gcross.20100927222551.1456:import_regex
-- @-node:gcross.20100927123234.1459:Values
-- @+node:gcross.20100927123234.1448:Functions
-- @+node:gcross.20101009103525.1729:buildLibrary
buildLibrary ::
    ProgramConfiguration Ar →
    KnownModules →
    Library →
    FilePath →
    Job HaskellLibrary
buildLibrary
    ar_configuration
    known_modules
    Library{..}
    archive_filepath
    =
    once (inMyNamespace archive_filepath) $ do
        let exposed_modules = map display exposedModules
            (bad_exports,good_exports) =
                partitionEithers
                .
                map (\module_name →
                    case Map.lookup module_name known_modules of
                        Nothing → Left (Left module_name)
                        Just (KnownModuleInExternalPackage _) → Left (Right module_name)
                        Just (KnownModuleInProject job) → Right job
                )
                $
                exposed_modules
        when (not . null $ bad_exports) $
            liftIO . throwIO . BadLibraryExportList $ bad_exports                
        modules ← sequenceA good_exports
        let (collected_modules,collected_packages) = collectAllLinkDependencies modules
            collected_object_digests =
                Map.map (fileDigest . haskellModuleObject) collected_modules
            digest =
                md5
                .
                encode
                .
                map (
                    fileDigest
                    .
                    haskellModuleInterface
                )
                $
                modules
        archive ← makeArchive ar_configuration collected_object_digests archive_filepath
        return $
            HaskellLibrary
            {   haskellLibraryModuleInterfaces = Map.map haskellModuleInterface collected_modules
            ,   haskellLibraryArchive = archive
            ,   haskellLibraryDigest = digest
            ,   haskellLibraryDependencyPackages = Map.elems collected_packages
            ,   haskellLibraryIsExposed = libExposed
            ,   haskellLibraryExposedModules = exposed_modules
            }
  where
    inMyNamespace = inNamespace (uuid "dd31d5fd-b093-43c9-bde5-8ea43ece1224")
-- @-node:gcross.20101009103525.1729:buildLibrary
-- @+node:gcross.20101010201506.1511:buildLibraryUsingBuildEnvironment
buildLibraryUsingBuildEnvironment ::
    ProgramConfiguration Ar →
    BuildEnvironment ForLibrary →
    Library →
    FilePath →
    Job HaskellLibrary
buildLibraryUsingBuildEnvironment ar_configuration BuildEnvironment{..} =
    buildLibrary ar_configuration buildEnvironmentKnownModules
-- @nonl
-- @-node:gcross.20101010201506.1511:buildLibraryUsingBuildEnvironment
-- @+node:gcross.20101005111309.1482:checkForSatisfyingPackage
checkForSatisfyingPackage :: PackageDatabase → Package.Dependency → Bool
checkForSatisfyingPackage package_database dependency = isJust (findSatisfyingPackage package_database dependency)
-- @-node:gcross.20101005111309.1482:checkForSatisfyingPackage
-- @+node:gcross.20100927222551.1451:compileToObject
compileToObject ::
    FilePath →
    PackageDatabase →
    KnownModules →
    [String] →
    FilePath →
    FilePath →
    HaskellSource →
    Job HaskellModule
compileToObject
    path_to_ghc
    package_database
    known_modules
    additional_options
    interface_filepath
    object_filepath
    HaskellSource{..}
  = once my_uuid
    .
    fmap postProcess
    $
    runIfImplicitDependencyOrProductHasChanged
        my_uuid
        (fileDigest haskellSourceFile)
        scan
        computeDependency
        productHasChangedFrom
        build
  where
    my_uuid =
        inNamespace
            (uuid "a807f1d2-c62d-4e44-9e8b-4c53e8410dee")
            (filePath haskellSourceFile ++ interface_filepath ++ object_filepath)

    scan :: Job [String]
    scan =
        fmap extractImportedModulesFromHaskellSource
        .
        liftIO
        .
        L.readFile
        .
        filePath
        $
        haskellSourceFile

    productHasChangedFrom :: TaggedList Two MD5Digest → Job Bool
    productHasChangedFrom old_digests =
        fmap ((== Just old_digests) . sequence)
        .
        traverse digestFileIfExists
        .
        fromT
        $
        (interface_filepath,object_filepath)

    computeDependency imported_modules = do
        (haskell_module_dependencies,package_dependencies) ←
            resolveModuleDependencies
                package_database
                known_modules
                imported_modules
        let package_digests =
                Map.fromList
                .
                map (installedPackageQualifiedName &&& installedPackageId)
                $
                package_dependencies
            dependency_digests =
                Map.fromList
                .
                map (filePath &&& fileDigest)
                .
                map haskellModuleInterface
                $
                haskell_module_dependencies
            (collected_module_dependencies,collected_package_dependencies) =
                collectAllLinkDependencies haskell_module_dependencies
        return
            ((package_digests,dependency_digests,additional_options)
            ,(collected_module_dependencies,collected_package_dependencies)
            )

    build (package_digests,_,_) = do
        liftIO . noticeM "Blueprint.Tools.Compilers.GHC" $ "(GHC) Compiling " ++ filePath haskellSourceFile
        let ghc_arguments =
                 "-c":filePath haskellSourceFile
                :"-o":object_filepath
                :"-ohi":interface_filepath
                :
                (concatMap (("-package":) . (:[])) . Map.keys $ package_digests)
                ++
                additional_options
        liftIO . infoM "Blueprint.Tools.Compilers.GHC" $ "(GHC) Executing '" ++ (unwords (path_to_ghc:ghc_arguments)) ++ "'"
        runProductionCommandAndDigestOutputs
            (object_filepath :. interface_filepath :. E)
            path_to_ghc
            ghc_arguments

    postProcess ::
        (TaggedList Two MD5Digest,(Map String HaskellModule,Map String InstalledPackage)) →
        HaskellModule
    postProcess
        ((interface_digest :. object_digest :. E)
        ,(module_dependencies,package_dependencies)
        )
      = HaskellModule
            haskellSourceModuleName
            (File interface_filepath interface_digest)
            (File object_filepath object_digest)
            module_dependencies
            package_dependencies
-- @nonl
-- @-node:gcross.20100927222551.1451:compileToObject
-- @+node:gcross.20101004145951.1474:compileToObjectUsingBuildEnvironment
compileToObjectUsingBuildEnvironment ::
    BuildEnvironment α →
    FilePath →
    FilePath →
    HaskellSource →
    Job HaskellModule
compileToObjectUsingBuildEnvironment BuildEnvironment{..} =
    compileToObject
        (pathToGHC buildEnvironmentGHC)
        buildEnvironmentPackageDatabase
        buildEnvironmentKnownModules
        buildEnvironmentCompileOptions
-- @nonl
-- @-node:gcross.20101004145951.1474:compileToObjectUsingBuildEnvironment
-- @+node:gcross.20100927222551.1438:computeBuildEnvironments
computeBuildEnvironments ::
    GHCEnvironment →
    PackageDescription →
    HaskellModuleJobs →
    [String] →
    [String] →
    FilePath →
    FilePath →
    BuildEnvironments
computeBuildEnvironments
    GHCEnvironment{..}
    package_description@PackageDescription{..}
    built_modules
    additional_compile_options
    additional_link_options
    interface_directory
    object_directory
    =
    BuildEnvironments
      ( BuildEnvironment
        {   buildEnvironmentGHC = ghcEnvironmentGHC
        ,   buildEnvironmentPackageDatabase = ghcEnvironmentPackageDatabase
        ,   buildEnvironmentKnownModules = known_modules
        ,   buildEnvironmentCompileOptions = "-package-name":package_name:compile_options
        ,   buildEnvironmentLinkOptions = "-package-name":package_name:link_options
        ,   buildEnvironmentInterfaceDirectory = interface_directory </> "library"
        ,   buildEnvironmentObjectDirectory = object_directory </> "library"
        }
      )
      ( BuildEnvironment
        {   buildEnvironmentGHC = ghcEnvironmentGHC
        ,   buildEnvironmentPackageDatabase = ghcEnvironmentPackageDatabase
        ,   buildEnvironmentKnownModules = known_modules
        ,   buildEnvironmentCompileOptions = compile_options
        ,   buildEnvironmentLinkOptions = link_options
        ,   buildEnvironmentInterfaceDirectory = interface_directory </> "program"
        ,   buildEnvironmentObjectDirectory = object_directory </> "progam"
        }
      )
  where
    installed_package_dependencies =
        map (fromJust . findSatisfyingPackage ghcEnvironmentPackageDatabase) buildDepends
    known_modules =
        Map.unions
            (Map.map KnownModuleInProject built_modules
            :map extractKnownModulesFromInstalledPackage installed_package_dependencies
            )
    interface_directory_option = "-i" ++ interface_directory
    compile_options = interface_directory_option:additional_compile_options
    link_options = interface_directory_option:additional_link_options
    package_name = display package
-- @nonl
-- @-node:gcross.20100927222551.1438:computeBuildEnvironments
-- @+node:gcross.20100929125042.1466:collectAllLinkDependencies
collectAllLinkDependencies :: [HaskellModule] → (Map String HaskellModule,Map String InstalledPackage)
collectAllLinkDependencies haskell_modules =
    (Map.union
        (Map.fromList . map (haskellModuleName &&& id) $ haskell_modules)
        (Map.unions . map haskellModuleLinkDependencyModules $ haskell_modules)
    ,Map.unions
        .
        map haskellModuleLinkDependencyPackages
        $
        haskell_modules
    )
-- @-node:gcross.20100929125042.1466:collectAllLinkDependencies
-- @+node:gcross.20100927123234.1450:configureGHC
configureGHC :: GHCOptions → Job GHC
configureGHC search_options@GHCOptions{..} =
    onceAndCached my_uuid
    $
    (liftIO . configureIt >=> \ghc → return (Just (search_options,ghc),ghc))
 where
    my_uuid = uuid "92f13c03-9512-4b97-a7eb-8637b78840ad"

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
-- @-node:gcross.20100927123234.1450:configureGHC
-- @+node:gcross.20100927161850.1438:configureGHCEnvironment
configureGHCEnvironment :: GHCOptions → Job GHCEnvironment
configureGHCEnvironment options = do
    ghc@GHC{..} ← configureGHC options
    package_database ← configurePackageDatabase pathToGHCPkg
    return $ GHCEnvironment ghc package_database
-- @-node:gcross.20100927161850.1438:configureGHCEnvironment
-- @+node:gcross.20100927161850.1444:configureGHCEnvironmentUsingOptions
configureGHCEnvironmentUsingOptions :: OptionValues → Job GHCEnvironment
configureGHCEnvironmentUsingOptions = configureGHCEnvironment . extractGHCOptions
-- @-node:gcross.20100927161850.1444:configureGHCEnvironmentUsingOptions
-- @+node:gcross.20100927161850.1440:configureGHCUsingOptions
configureGHCUsingOptions :: OptionValues → Job GHC
configureGHCUsingOptions = configureGHC . extractGHCOptions
-- @-node:gcross.20100927161850.1440:configureGHCUsingOptions
-- @+node:gcross.20100927161850.1432:configurePackageDatabase
configurePackageDatabase :: FilePath → Job PackageDatabase
configurePackageDatabase path_to_ghc_pkg =
    onceAndCached my_uuid
    $
    \maybe_cache → do
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
                    traverse (liftIO . loadInstalledPackageInformation path_to_ghc_pkg) list_of_package_atoms
        let package_database = constructPackageDatabaseFromInstalledPackages installed_packages
        return (Just (list_of_package_atoms,installed_packages), package_database)
  where
    my_uuid = uuid "734f6cec-8a79-4aa2-ad3b-ebe0937cd125"
-- @-node:gcross.20100927161850.1432:configurePackageDatabase
-- @+node:gcross.20100927161850.1434:constructPackageDatabaseFromInstalledPackages
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
-- @-node:gcross.20100927161850.1434:constructPackageDatabaseFromInstalledPackages
-- @+node:gcross.20101010201506.1514:createBuildLibraryTarget
createBuildLibraryTarget ::
    ProgramConfiguration Ar →
    BuildEnvironment ForLibrary →
    PackageDescription →
    Job HaskellLibrary
createBuildLibraryTarget
    ar_configuration
    build_environment
    PackageDescription{..}
  = traverse scanForModulesIn (if null hsSourceDirs then ["."] else hsSourceDirs)
    >>=
    return
        .
        snd
        .
        updateBuildEnvironmentToIncludeModules build_environment
        .
        Map.unions
        .
        reverse
    >>=
    \new_build_environment →
        buildLibraryUsingBuildEnvironment
            ar_configuration
            new_build_environment
            (fromJust library)
            ("HS" ++ display package <.> "a")
  where

    Just (Library{libBuildInfo=BuildInfo{..}}) = library
-- @-node:gcross.20101010201506.1514:createBuildLibraryTarget
-- @+node:gcross.20101006110010.1483:createCompilationJobsForModules
createCompilationJobsForModules ::
    FilePath →
    PackageDatabase →
    KnownModules →
    [String] →
    FilePath →
    FilePath →
    Map String (Job HaskellSource) →
    (HaskellModuleJobs,KnownModules)
createCompilationJobsForModules
    path_to_ghc
    package_database
    known_modules
    additional_options
    interface_directory
    object_directory
    module_sources
  = (jobs,new_known_modules)
  where
    jobs = Map.mapWithKey
        (\module_name module_source_job →
            module_source_job
            >>=
            compileToObject 
                path_to_ghc
                package_database
                new_known_modules
                additional_options
                (interface_directory </> module_name <.> "hi")
                (object_directory </> module_name <.> "o")
        ) module_sources

    new_known_modules =
        Map.union
            (Map.map KnownModuleInProject jobs)
            known_modules
-- @nonl
-- @-node:gcross.20101006110010.1483:createCompilationJobsForModules
-- @+node:gcross.20100927123234.1456:determineGHCVersion
determineGHCVersion :: FilePath → IO Version
determineGHCVersion = determineProgramVersion parseGHCVersion ["--version"]
-- @-node:gcross.20100927123234.1456:determineGHCVersion
-- @+node:gcross.20101009103525.1736:dotsToPath
dotsToPath = map (\c → if c == '.' then pathSeparator else c)
-- @nonl
-- @-node:gcross.20101009103525.1736:dotsToPath
-- @+node:gcross.20100927161850.1442:extractGHCOptions
extractGHCOptions :: OptionValues → GHCOptions
extractGHCOptions =
    GHCOptions
        <$> Map.lookup ghc_search_option_path_to_ghc
        <*> Map.lookup ghc_search_option_path_to_ghc_pkg
        <*> fmap readVersion . Map.lookup ghc_search_option_desired_version
        <*> maybe [] splitSearchPath . Map.lookup ghc_search_option_search_paths
-- @-node:gcross.20100927161850.1442:extractGHCOptions
-- @+node:gcross.20100927222551.1454:extractImportedModulesFromHaskellSource
extractImportedModulesFromHaskellSource :: L.ByteString → [String]
extractImportedModulesFromHaskellSource =
    map (
        LU.toString
        .
        fst
        .
        (! 1)
    )
    .
    matchAllText import_regex
-- @-node:gcross.20100927222551.1454:extractImportedModulesFromHaskellSource
-- @+node:gcross.20100927222551.1440:extractKnownModulesFromInstalledPackage
extractKnownModulesFromInstalledPackage :: InstalledPackage → KnownModules
extractKnownModulesFromInstalledPackage installed_package@InstalledPackage{..} =
    Map.fromList
    .
    map (,KnownModuleInExternalPackage installed_package)
    $
    installedPackageModules
-- @-node:gcross.20100927222551.1440:extractKnownModulesFromInstalledPackage
-- @+node:gcross.20100927222551.1449:findSatisfyingPackage
findSatisfyingPackage :: PackageDatabase → Package.Dependency → Maybe InstalledPackage
findSatisfyingPackage PackageDatabase{..} (Package.Dependency name version_range) =
    Map.lookup (display name) packageDatabaseIndexedByPackageNameAndVersion
    >>=
    find (flip withinRange version_range . fst)
    >>=
    return . head . snd
-- @-node:gcross.20100927222551.1449:findSatisfyingPackage
-- @+node:gcross.20101009103525.1735:installPackage
installPackage ::
    FilePath →
    PackageDescription →
    HaskellLibrary →
    FilePath →
    Job InstalledPackageInfo
installPackage
    path_to_ghc_pkg
    package_description@PackageDescription{..}
    HaskellLibrary{..}
    destination_directory
  | isNothing library
    = liftIO . throwIO . LibraryMissingFromPackageDescription $ package_description
  | not (Set.null missing_modules)
    = liftIO . throwIO . MissingExposedModules $ missing_modules
  | otherwise
    = liftIO $ do
    createDirectoryIfMissing True destination_directory
    noticeM "Blueprint.Tools.Compilers.GHC" "(GHC) Installing package..."
    forM_ (Map.assocs haskellLibraryModuleInterfaces) $ \(module_name,File{..}) → do
        let interface_destination = destination_directory </> dotsToPath module_name <.> "hi"
        createDirectoryIfMissing True (takeDirectory interface_destination)
        infoM "Blueprint.Tools.Compilers.GHC" $
            ("(GHC) Copying " ++ filePath ++ " -> " ++ interface_destination)
        copyFile filePath interface_destination
    let archive_source = filePath haskellLibraryArchive
        archive_destination = destination_directory </> library_name <.> (takeExtension archive_source)
    infoM "Blueprint.Tools.Compilers.GHC" $
        ("(GHC) Copying " ++ archive_source ++ " -> " ++ archive_destination)
    copyFile archive_source archive_destination
    runProductionCommand
        path_to_ghc_pkg
        ["register","––auto-ghci-libs","-"]
        (show installed_package_info)
    return installed_package_info
  where
    Just Library{..} = library
    all_modules = Map.keysSet haskellLibraryModuleInterfaces
    exposed_modules = Set.fromList . map display $ exposedModules
    missing_modules = Set.difference exposed_modules all_modules
    library_name = "HS" ++ display package
    installed_package_info =
        InstalledPackageInfo.InstalledPackageInfo
        {   installedPackageId = InstalledPackageId (display package ++ "-" ++ show haskellLibraryDigest)
        ,   sourcePackageId = package
        ,   license = license
        ,   copyright = copyright
        ,   maintainer = maintainer
        ,   author = author
        ,   stability = stability
        ,   homepage = homepage
        ,   pkgUrl = pkgUrl
        ,   description = description
        ,   category = category
        ,   exposed = libExposed
        ,   exposedModules = exposedModules
        ,   hiddenModules =
                map ModuleName.fromString
                .
                Set.elems
                $
                Set.difference
                    all_modules
                    exposed_modules
        ,   importDirs = [destination_directory]
        ,   libraryDirs = [destination_directory]
        ,   hsLibraries = [library_name]
        ,   extraLibraries = []
        ,   extraGHCiLibraries = []
        ,   includeDirs = []
        ,   includes = []
        ,   depends = map installedPackageId haskellLibraryDependencyPackages
        ,   hugsOptions = []
        ,   ccOptions = []
        ,   ldOptions = []
        ,   frameworkDirs = []
        ,   frameworks = []
        ,   haddockInterfaces = []
        ,   haddockHTMLs = []
    }
-- @-node:gcross.20101009103525.1735:installPackage
-- @+node:gcross.20101010201506.1510:installPackageUsingBuildEnvironment
installPackageUsingBuildEnvironment ::
    BuildEnvironment ForLibrary →
    PackageDescription →
    HaskellLibrary →
    FilePath →
    Job InstalledPackageInfo
installPackageUsingBuildEnvironment = installPackage . pathToGHCPkg . buildEnvironmentGHC
-- @nonl
-- @-node:gcross.20101010201506.1510:installPackageUsingBuildEnvironment
-- @+node:gcross.20101004145951.1467:linkProgram
linkProgram ::
    FilePath →
    [String] →
    [HaskellModule] →
    FilePath →
    Job ProgramFile
linkProgram
    path_to_ghc
    additional_options
    haskell_objects
    program_filepath
  = once my_uuid
    .
    fmap (File program_filepath)
    $
    runIfDependencyOrProductHasChanged
        my_uuid
        (additional_options,dependency_digests,package_digests)
        (\old_digest → fmap (/= Just old_digest) (digestFileIfExists program_filepath))
        build
  where
    my_uuid = (inNamespace (uuid "eb95ef18-e0c3-476e-894c-aefb8e5b931a") program_filepath)
    (haskell_module_dependencies,package_dependencies) = collectAllLinkDependencies haskell_objects
    haskell_object_dependencies = Map.map haskellModuleObject haskell_module_dependencies
    dependency_digests = Map.map fileDigest haskell_object_dependencies
    package_digests = Map.map installedPackageId package_dependencies
    ghc_arguments =
        Map.keys haskell_object_dependencies
        ++
        concat [["-package",package_name] | package_name ← Map.keys package_dependencies]
        ++
        ["-o",program_filepath]
        ++
        additional_options
    build = do
        liftIO . noticeM "Blueprint.Tools.Compilers.GHC" $ "(GHC) Linking program " ++ program_filepath
        liftIO . infoM "Blueprint.Tools.Compilers.GHC" $ "(GHC) Executing '" ++ (unwords (path_to_ghc:ghc_arguments)) ++ "'"
        fmap toT $
            runProductionCommandAndDigestOutputs
                (program_filepath :. E)
                path_to_ghc
                ghc_arguments
-- @-node:gcross.20101004145951.1467:linkProgram
-- @+node:gcross.20101004145951.1475:linkProgramUsingBuildEnvironment
linkProgramUsingBuildEnvironment ::
    BuildEnvironment ForPrograms →
    [HaskellModule] →
    FilePath →
    Job ProgramFile
linkProgramUsingBuildEnvironment BuildEnvironment{..} =
    linkProgram
        (pathToGHC buildEnvironmentGHC)
        buildEnvironmentLinkOptions
-- @-node:gcross.20101004145951.1475:linkProgramUsingBuildEnvironment
-- @+node:gcross.20100927161850.1436:loadInstalledPackageInformation
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
-- @-node:gcross.20100927161850.1436:loadInstalledPackageInformation
-- @+node:gcross.20101005111309.1484:lookupPackageNamed
lookupPackageNamed :: PackageDatabase → Package.PackageName → Maybe [(Version,[InstalledPackage])]
lookupPackageNamed PackageDatabase{..} =
    flip Map.lookup packageDatabaseIndexedByPackageNameAndVersion
    .
    display
-- @-node:gcross.20101005111309.1484:lookupPackageNamed
-- @+node:gcross.20100927123234.1458:parseGHCVersion
parseGHCVersion = extractVersion ghc_version_regex
-- @-node:gcross.20100927123234.1458:parseGHCVersion
-- @+node:gcross.20101005111309.1462:readAndConfigurePackageDescription
readAndConfigurePackageDescription ::
    GHCEnvironment →
    FlagAssignment →
    FilePath →
    Job (PackageDescription, FlagAssignment)
readAndConfigurePackageDescription GHCEnvironment{..} flags =
    liftIO . readPackageDescription silent
    >=>
    either
        (liftIO . throwIO . UnresolvedPackageDependenciesError ghcEnvironmentPackageDatabase)
        return
    .
    finalizePackageDescription
        flags
        (checkForSatisfyingPackage ghcEnvironmentPackageDatabase)
        buildPlatform
        (Compiler.CompilerId Compiler.GHC (ghcVersion ghcEnvironmentGHC))
        []
-- @nonl
-- @-node:gcross.20101005111309.1462:readAndConfigurePackageDescription
-- @+node:gcross.20100928173417.1463:resolveModuleDependencies
resolveModuleDependencies ::
    PackageDatabase →
    KnownModules →
    [String] →
    Job ([HaskellModule],[InstalledPackage])
resolveModuleDependencies PackageDatabase{..} known_modules module_names =
    case partitionEithers resolved_modules of
        ([],resolutions) → do
            let (package_names,jobs) = partitionEithers resolutions
            haskell_modules ← sequenceA jobs
            return (haskell_modules,package_names)
        (unknown_modules,_) → liftIO . throwIO . UnknownModulesException $ unknown_modules
  where
    resolved_modules =
        map (\module_name →
            case Map.lookup module_name known_modules of
                Just (KnownModuleInExternalPackage package_name) → Right . Left $ package_name
                Just (KnownModuleInProject job) → Right . Right $ job
                Nothing →
                    Left
                    .
                    (module_name,)
                    .
                    maybe [] (map installedPackageQualifiedName)
                    $
                    Map.lookup module_name packageDatabaseIndexedByModuleName
        ) module_names
-- @-node:gcross.20100928173417.1463:resolveModuleDependencies
-- @+node:gcross.20101006110010.1481:scanForModulesIn
scanForModulesIn :: FilePath → Job (Map String (Job HaskellSource))
scanForModulesIn root = once (inMyNamespace root) $ scanForModulesWithParentIn Nothing root
  where
    inMyNamespace = inNamespace (uuid "cd195cae-ff8f-4d16-9884-9fb924af2a7f")
-- @nonl
-- @-node:gcross.20101006110010.1481:scanForModulesIn
-- @+node:gcross.20101006110010.1480:scanForModulesWithParentIn
scanForModulesWithParentIn ::
    Maybe String →
    FilePath →
    Job (Map String (Job HaskellSource))
scanForModulesWithParentIn maybe_parent root =
    liftIO (getDirectoryContents root)
    >>=
    fmap Map.unions
    .
    sequenceA
    .
    map (\entry → do
        if isUpper (head entry)
            then do
                let filepath = root </> entry
                is_directory ← liftIO (doesDirectoryExist entry)
                case is_directory of
                    True →
                        scanForModulesWithParentIn
                            (Just . appendToParent $ entry)
                            filepath
                    False | takeExtension entry == ".hs" →
                        return
                        $
                        let module_name = appendToParent . dropExtension $ entry
                        in Map.singleton
                            module_name
                            (once (inMyNamespace filepath) $
                                fmap (HaskellSource module_name . File filepath) (digestFile filepath)
                            )
                    _ → return Map.empty
            else return Map.empty
    )
  where
    inMyNamespace = inNamespace (uuid "4bbdf77f-d4db-423c-bedb-06f12aae0792")
    appendToParent child = maybe child (<.> child) maybe_parent
-- @-node:gcross.20101006110010.1480:scanForModulesWithParentIn
-- @+node:gcross.20101006110010.1487:updateBuildEnvironmentToIncludeModules
updateBuildEnvironmentToIncludeModules ::
    BuildEnvironment α →
    Map String (Job HaskellSource) →
    (HaskellModuleJobs,BuildEnvironment α)
updateBuildEnvironmentToIncludeModules build_environment@BuildEnvironment{..}
  = second (\new_known_modules → build_environment { buildEnvironmentKnownModules = new_known_modules })
    .
    createCompilationJobsForModules
        (pathToGHC buildEnvironmentGHC)
        buildEnvironmentPackageDatabase
        buildEnvironmentKnownModules
        buildEnvironmentCompileOptions
        buildEnvironmentInterfaceDirectory
        buildEnvironmentObjectDirectory
-- @nonl
-- @-node:gcross.20101006110010.1487:updateBuildEnvironmentToIncludeModules
-- @-node:gcross.20100927123234.1448:Functions
-- @+node:gcross.20100927123234.1432:Options
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
-- @-node:gcross.20100927123234.1432:Options
-- @-others
-- @-node:gcross.20100927123234.1428:@thin GHC.hs
-- @-leo
