-- @+leo-ver=4-thin
-- @+node:gcross.20100927123234.1428:@thin GHC.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100927123234.1429:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Data.List.Tagged (TaggedList(..),fromTuple,toTuple)
import qualified Data.List.Tagged as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
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
import qualified Text.Read as Read
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Regex.PCRE
import Text.Regex.PCRE.String

import TypeLevel.NaturalNumber (Two)

import Blueprint.Cache
import Blueprint.Configuration
import Blueprint.Identifier
import Blueprint.Job
import qualified Blueprint.Main as Main
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
-- @+node:gcross.20101012145613.1542:BuildTargets
data BuildTargets = BuildTargets
    {   libraryBuildTarget :: Maybe Library
    ,   executableBuildTargets :: [Executable]
    }
-- @-node:gcross.20101012145613.1542:BuildTargets
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
-- @+node:gcross.20101010201506.1521:InstallationEnvironment
data InstallationEnvironment = InstallationEnvironment
    {   installationLibraryDirectory :: FilePath
    ,   installationExecutableDirectory :: FilePath
    }
-- @-node:gcross.20101010201506.1521:InstallationEnvironment
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
-- @+node:gcross.20101012145613.1543:InstallTargets
data InstallTargets = InstallTargets
    {   libraryInstallTarget :: Maybe HaskellLibrary
    ,   executableInstallTargets :: [ProgramFile]
    }
-- @-node:gcross.20101012145613.1543:InstallTargets
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

instance Read.Read PackageLocality where
    readPrec = ReadPrec.lift . ReadP.choice $
        [ReadP.string "user" >> return User
        ,ReadP.string "global" >> return Global
        ]
-- @-node:gcross.20100927123234.1439:PackageLocality
-- @+node:gcross.20101010201506.1528:GHCOptions
data GHCOptions = GHCOptions
    {   ghcOptionPathToGHC :: Maybe FilePath
    ,   ghcOptionPathToGHCPkg :: Maybe FilePath
    ,   ghcOptionDesiredVersion :: Maybe Version
    ,   ghcOptionSearchPaths :: [FilePath]
    ,   ghcOptionPackageLocality :: PackageLocality
    } deriving (Eq,Show,Typeable)

$(derive makeBinary ''GHCOptions)
-- @-node:gcross.20101010201506.1528:GHCOptions
-- @+node:gcross.20100927123234.1443:GHCEnvironment
data GHCEnvironment = GHCEnvironment
    {   ghcEnvironmentGHC :: GHC
    ,   ghcEnvironmentPackageDatabase :: PackageDatabase
    ,   ghcEnvironmentPackageLocality :: PackageLocality
    } deriving Typeable

-- @-node:gcross.20100927123234.1443:GHCEnvironment
-- @+node:gcross.20100927222551.1446:BuildEnvironment
data BuildEnvironment = BuildEnvironment
    {   buildEnvironmentGHC :: GHC
    ,   buildEnvironmentPackageDatabase :: PackageDatabase
    ,   buildEnvironmentPackageLocality :: PackageLocality
    ,   buildEnvironmentKnownModules :: KnownModules
    ,   buildEnvironmentLibraryCompileOptions :: [String]
    ,   buildEnvironmentProgramCompileOptions :: [String]
    ,   buildEnvironmentProgramLinkOptions :: [String]
    ,   buildEnvironmentLibraryInterfaceDirectory :: FilePath
    ,   buildEnvironmentLibraryObjectDirectory :: FilePath
    ,   buildEnvironmentProgramInterfaceDirectory :: FilePath
    ,   buildEnvironmentProgramObjectDirectory :: FilePath
    }
-- @nonl
-- @-node:gcross.20100927222551.1446:BuildEnvironment
-- @+node:gcross.20101012145613.1551:Configuration
data Configuration = Configuration
    {   configurationAr :: Maybe (ProgramConfiguration Ar)
    ,   configurationBuildEnvironment :: BuildEnvironment
    ,   configurationPackageDescription :: PackageDescription
    ,   configurationInstallationEnvironment :: InstallationEnvironment
    }
-- @-node:gcross.20101012145613.1551:Configuration
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
-- @+node:gcross.20101012145613.1517:BadMainModule
data BadMainModule =
    MissingMainModule String
  | ExternalMainModule String InstalledPackage
  | NoExecutableWithNameInPackageDescription String PackageDescription
  deriving Typeable

instance Show BadMainModule where
    show (MissingMainModule missing_module_name) = "Unable to find the main module " ++ missing_module_name
    show (ExternalMainModule missing_module_name InstalledPackage{..}) = "The main module " ++ missing_module_name ++ " is not part of this project but rather is in the external package " ++ installedPackageQualifiedName
    show (NoExecutableWithNameInPackageDescription executable_name package_description) = "There is no executable with the name " ++ executable_name ++ " in the package description:" ++ show package_description

instance Exception BadMainModule
-- @nonl
-- @-node:gcross.20101012145613.1517:BadMainModule
-- @+node:gcross.20101012145613.1525:CabalFileException
data CabalFileException =
    NoCabalFile
  | MultipleCabalFiles [FilePath]
  deriving Typeable

instance Show CabalFileException where
    show NoCabalFile = "No .cabal file is present in this directory."
    show (MultipleCabalFiles cabal_file_names) = "Multiple .cabal files are present " ++ show cabal_file_names

instance Exception CabalFileException
-- @nonl
-- @-node:gcross.20101012145613.1525:CabalFileException
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
-- @+node:gcross.20101010201506.1498:MissingExposedModules
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
-- @-node:gcross.20101010201506.1498:MissingExposedModules
-- @+node:gcross.20101012145613.1558:NonExistentTargets
data NonExistentTargets = NonExistentTargets PackageDescription [String] deriving Typeable

instance Show NonExistentTargets where
    show (NonExistentTargets PackageDescription{..} targets) =
        unlines
        .
        (("The following targets are not part of this project: " ++ unwords targets):)
        .
        map ('\t':) -- '
        .
        (case library of
            Nothing → id
            Just _ → ("library":)
        )
        .
        sort
        .
        map exeName
        $
        executables

instance Exception NonExistentTargets
-- @-node:gcross.20101012145613.1558:NonExistentTargets
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
-- @+node:gcross.20101012145613.1516:buildExecutable
buildExecutable ::
    FilePath →
    [String] →
    [String] →
    PackageDatabase →
    KnownModules →
    FilePath →
    FilePath →
    FilePath →
    Executable →
    Job ProgramFile
buildExecutable
    path_to_ghc
    additional_compiler_options
    additional_linker_options
    package_database
    known_modules
    interface_directory
    object_directory
    executable_directory
    Executable{..}
    =
    once my_id $
        scanForAndCompileModulesInAllOf
            path_to_ghc
            additional_compiler_options
            package_database
            known_modules
            interface_directory
            object_directory
            (extractHaskellSourceDirectoriesFrom buildInfo)
        >>=
        \(_,known_modules) →
            case Map.lookup modulePath known_modules of
                Nothing → liftIO . throwIO . MissingMainModule $ modulePath
                Just (KnownModuleInExternalPackage package) → liftIO . throwIO $ ExternalMainModule modulePath package
                Just (KnownModuleInProject main_module_job) → main_module_job
        >>=
        linkProgram
            path_to_ghc
            additional_linker_options
            program_filepath
  where
    program_filepath = executable_directory </> exeName
    my_id =
        identifierInNamespace
            (uuid "0cbcbb1f-e695-4612-b2b9-d3a9a125e04f")
            ("building executable " ++ program_filepath)
-- @nonl
-- @-node:gcross.20101012145613.1516:buildExecutable
-- @+node:gcross.20101012145613.1519:buildExecutableUsingBuildEnvironment
buildExecutableUsingBuildEnvironment ::
    BuildEnvironment →
    FilePath →
    Executable →
    Job ProgramFile
buildExecutableUsingBuildEnvironment =
    buildExecutable
        <$> (pathToGHC . buildEnvironmentGHC)
        <*> buildEnvironmentProgramCompileOptions
        <*> buildEnvironmentProgramLinkOptions
        <*> buildEnvironmentPackageDatabase
        <*> buildEnvironmentKnownModules
        <*> buildEnvironmentProgramInterfaceDirectory
        <*> buildEnvironmentProgramObjectDirectory
-- @nonl
-- @-node:gcross.20101012145613.1519:buildExecutableUsingBuildEnvironment
-- @+node:gcross.20101015124156.1546:buildExecutableUsingConfiguration
buildExecutableUsingConfiguration ::
    Configuration →
    FilePath →
    Executable →
    Job ProgramFile
buildExecutableUsingConfiguration = buildExecutableUsingBuildEnvironment . configurationBuildEnvironment
-- @nonl
-- @-node:gcross.20101015124156.1546:buildExecutableUsingConfiguration
-- @+node:gcross.20101009103525.1729:buildLibrary
buildLibrary ::
    ProgramConfiguration Ar →
    String →
    FilePath →
    [String] →
    PackageDatabase →
    KnownModules →
    FilePath →
    FilePath →
    FilePath →
    Library →
    Job HaskellLibrary
buildLibrary
    ar_configuration
    package_name
    path_to_ghc
    additional_compiler_options
    package_database
    known_modules
    interface_directory
    object_directory
    library_directory
    Library{..}
    =
    once my_id $ do
        (_,known_modules) ←
            scanForAndCompileModulesInAllOf
                path_to_ghc
                additional_compiler_options
                package_database
                known_modules
                interface_directory
                object_directory
                (extractHaskellSourceDirectoriesFrom libBuildInfo)
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
        archive ←
            makeArchive
                ar_configuration
                collected_object_digests
                archive_filepath
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
    archive_filepath = library_directory </> "HS" ++ package_name <.> "a"
    my_id =
        identifierInNamespace
            (uuid "dd31d5fd-b093-43c9-bde5-8ea43ece1224")
            ("building library " ++ archive_filepath)
-- @nonl
-- @-node:gcross.20101009103525.1729:buildLibrary
-- @+node:gcross.20101010201506.1511:buildLibraryUsingBuildEnvironment
buildLibraryUsingBuildEnvironment ::
    ProgramConfiguration Ar →
    String →
    BuildEnvironment →
    FilePath →
    Library →
    Job HaskellLibrary
buildLibraryUsingBuildEnvironment ar_configuration package_name =
    buildLibrary ar_configuration package_name
        <$> (pathToGHC . buildEnvironmentGHC)
        <*> buildEnvironmentLibraryCompileOptions
        <*> buildEnvironmentPackageDatabase
        <*> buildEnvironmentKnownModules
        <*> buildEnvironmentLibraryInterfaceDirectory
        <*> buildEnvironmentLibraryObjectDirectory
-- @nonl
-- @-node:gcross.20101010201506.1511:buildLibraryUsingBuildEnvironment
-- @+node:gcross.20101012145613.1545:buildLibraryUsingConfiguration
buildLibraryUsingConfiguration ::
    Configuration →
    FilePath →
    Library →
    Job HaskellLibrary
buildLibraryUsingConfiguration =
    buildLibraryUsingBuildEnvironment
        <$> (fromJust . configurationAr)
        <*> (display . package . configurationPackageDescription)
        <*> configurationBuildEnvironment
-- @nonl
-- @-node:gcross.20101012145613.1545:buildLibraryUsingConfiguration
-- @+node:gcross.20101012145613.1540:buildTargets
buildTargets :: Configuration → BuildTargets → Job InstallTargets
buildTargets configuration BuildTargets{..} =
    liftA2 InstallTargets
        (case libraryBuildTarget of
            Nothing → return Nothing
            Just library →
                fmap Just
                $
                buildLibraryUsingConfiguration
                    configuration
                    "libraries"
                    library
        )
        (traverse
            (buildExecutableUsingConfiguration configuration "programs")
            executableBuildTargets
        )
-- @-node:gcross.20101012145613.1540:buildTargets
-- @+node:gcross.20101005111309.1482:checkForSatisfyingPackage
checkForSatisfyingPackage :: PackageDatabase → Package.Dependency → Bool
checkForSatisfyingPackage package_database dependency = isJust (findSatisfyingPackage package_database dependency)
-- @-node:gcross.20101005111309.1482:checkForSatisfyingPackage
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
  = once my_id
    .
    fmap postProcess
    $
    runIfImplicitDependencyOrProductHasChanged
        my_id
        (fileDigest haskellSourceFile)
        scan
        computeDependency
        productHasChangedFrom
        build
  where
    my_id =
        identifierInNamespace
            (uuid "a807f1d2-c62d-4e44-9e8b-4c53e8410dee")
            ("compiling " ++ filePath haskellSourceFile ++ " --> " ++ object_filepath)

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
        fromTuple
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
-- @+node:gcross.20100927222551.1438:computeBuildEnvironment
computeBuildEnvironment ::
    GHCEnvironment →
    PackageDescription →
    HaskellModuleJobs →
    [String] →
    [String] →
    FilePath →
    FilePath →
    BuildEnvironment
computeBuildEnvironment
    GHCEnvironment{..}
    package_description@PackageDescription{..}
    built_modules
    additional_compile_options
    additional_link_options
    interface_directory
    object_directory
    =
    BuildEnvironment
    {   buildEnvironmentGHC = ghcEnvironmentGHC
    ,   buildEnvironmentPackageDatabase = ghcEnvironmentPackageDatabase
    ,   buildEnvironmentPackageLocality = ghcEnvironmentPackageLocality
    ,   buildEnvironmentKnownModules = known_modules
    ,   buildEnvironmentLibraryCompileOptions = "-package-name":package_name:compile_options
    ,   buildEnvironmentProgramCompileOptions = compile_options
    ,   buildEnvironmentProgramLinkOptions = link_options
    ,   buildEnvironmentLibraryInterfaceDirectory = interface_directory </> "library"
    ,   buildEnvironmentLibraryObjectDirectory = object_directory </> "library"
    ,   buildEnvironmentProgramInterfaceDirectory = interface_directory </> "program"
    ,   buildEnvironmentProgramObjectDirectory = object_directory </> "progam"
    }
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
-- @-node:gcross.20100927222551.1438:computeBuildEnvironment
-- @+node:gcross.20101012145613.1556:computeDefaultTargets
computeDefaultBuildTargetsIn :: PackageDescription → BuildTargets
computeDefaultBuildTargetsIn PackageDescription{..} =
    BuildTargets
        (case library of
            Just Library{..} | buildable libBuildInfo → library
            _ → Nothing
        )
        (filter (buildable . buildInfo) executables)
-- @-node:gcross.20101012145613.1556:computeDefaultTargets
-- @+node:gcross.20101012145613.1524:configure
configure :: OptionValues → Job Configuration
configure options = do
    cabal_file ← findDefaultCabalFile
    ghc_environment ← configureGHCEnvironmentUsingOptions options
    (package_description,_) ←
        readAndConfigurePackageDescription
            ghc_environment
            []
            cabal_file
    maybe_ar_configuration ←
        if isNothing (library package_description)
            then return Nothing
            else fmap Just (configureProgramUsingOptions options)
    let build_environment =
            computeBuildEnvironment
                ghc_environment
                package_description
                Map.empty
                []
                []
                "interfaces"
                "objects"
    installation_environment ← configureInstallationEnvironmentUsingOptions options
    return $
        Configuration
            maybe_ar_configuration
            build_environment
            package_description
            installation_environment
-- @nonl
-- @-node:gcross.20101012145613.1524:configure
-- @+node:gcross.20100927123234.1450:configureGHC
configureGHC :: GHCOptions → Job GHC
configureGHC search_options@GHCOptions{..} =
    onceAndCached my_id
    $
    (liftIO . configureIt >=> \ghc → return (Just (search_options,ghc),ghc))
 where
    my_id = identifier "92f13c03-9512-4b97-a7eb-8637b78840ad" "configuring GHC"

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
configureGHCEnvironment options@GHCOptions{..} = do
    ghc@GHC{..} ← configureGHC options
    package_database ← configurePackageDatabase pathToGHCPkg ghcOptionPackageLocality
    return $ GHCEnvironment ghc package_database ghcOptionPackageLocality
-- @-node:gcross.20100927161850.1438:configureGHCEnvironment
-- @+node:gcross.20100927161850.1444:configureGHCEnvironmentUsingOptions
configureGHCEnvironmentUsingOptions :: OptionValues → Job GHCEnvironment
configureGHCEnvironmentUsingOptions = configureGHCEnvironment . extractGHCOptions
-- @-node:gcross.20100927161850.1444:configureGHCEnvironmentUsingOptions
-- @+node:gcross.20100927161850.1440:configureGHCUsingOptions
configureGHCUsingOptions :: OptionValues → Job GHC
configureGHCUsingOptions = configureGHC . extractGHCOptions
-- @-node:gcross.20100927161850.1440:configureGHCUsingOptions
-- @+node:gcross.20101010201506.1526:configureInstallationEnvironmentUsingOptions
configureInstallationEnvironmentUsingOptions :: OptionValues → Job InstallationEnvironment
configureInstallationEnvironmentUsingOptions option_values = do
    prefix ←
        case (Map.lookup installation_option_prefix
              &&&
              fmap read . Map.lookup ghc_package_database_option_locality
             ) option_values
        of  (Just prefix,_) → return prefix
            (_,Just User) → liftIO $ fmap (</> ".cabal") getHomeDirectory
            _ → return "/usr/local"
    return
        .
        liftA2 InstallationEnvironment
            (fromMaybe (prefix </> "lib") . Map.lookup installation_option_libdir)
            (fromMaybe (prefix </> "bin") . Map.lookup installation_option_bindir)
        $
        option_values
-- @-node:gcross.20101010201506.1526:configureInstallationEnvironmentUsingOptions
-- @+node:gcross.20100927161850.1432:configurePackageDatabase
configurePackageDatabase :: FilePath → PackageLocality → Job PackageDatabase
configurePackageDatabase path_to_ghc_pkg locality =
    onceAndCached my_id
    $
    \maybe_cache → do
        liftIO . infoM "Blueprint.Tools.Compilers.GHC" $
            "(GHC) Reading package atoms..."
        let arguments =
                case locality of
                    Global → ["--simple-output","--global","list"]
                    User → ["--simple-output","list"]
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
    my_id = identifier "734f6cec-8a79-4aa2-ad3b-ebe0937cd125" "configuring GHC package database"
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
-- @+node:gcross.20101012145613.1572:defaultMain
defaultMain =
    Main.defaultMain
        (ghc_options `mappend` installation_options)
        "configuration.cfg"
        "configuration.cache"
        "build.cache"
        configure
        runTarget
        displayModesMessage
-- @-node:gcross.20101012145613.1572:defaultMain
-- @+node:gcross.20100927123234.1456:determineGHCVersion
determineGHCVersion :: FilePath → IO Version
determineGHCVersion = determineProgramVersion parseGHCVersion ["--version"]
-- @-node:gcross.20100927123234.1456:determineGHCVersion
-- @+node:gcross.20101012145613.1567:displayModesMessage
displayModesMessage :: MonadIO m ⇒ m ()
displayModesMessage = liftIO $ do
    putStrLn "The possible modes of operation are:"
    mapM_ (putStrLn . ('\t':)) -- '
        ["configure"
        ,"build [target1 target2...]"
        ,"install [target1 target2...]"
        ]
-- @-node:gcross.20101012145613.1567:displayModesMessage
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
        <*> maybe User read . Map.lookup ghc_package_database_option_locality
-- @-node:gcross.20100927161850.1442:extractGHCOptions
-- @+node:gcross.20101015124156.1543:extractHaskellSourceDirectoriesFrom
extractHaskellSourceDirectoriesFrom :: BuildInfo → [FilePath]
extractHaskellSourceDirectoriesFrom BuildInfo{hsSourceDirs=[]} = ["."]
extractHaskellSourceDirectoriesFrom BuildInfo{hsSourceDirs} = hsSourceDirs
-- @nonl
-- @-node:gcross.20101015124156.1543:extractHaskellSourceDirectoriesFrom
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
-- @+node:gcross.20101012145613.1557:fetchBuildTargetsIn
fetchBuildTargetsIn :: PackageDescription → [String] → BuildTargets
fetchBuildTargetsIn package_description@PackageDescription{..} targets =
    case (library_target,executable_targets_and_errors) of
        (Just library,([],executables)) → BuildTargets library executables
        (_,(non_existent_targets,_)) →
            throw
            .
            NonExistentTargets package_description
            .
            (if isNothing library_target then ("library":) else id)
            $
            non_existent_targets
  where
    library_target
      | "library" `elem` targets
        = case library of
            Just library → Just (Just library)
            Nothing → Nothing
      | otherwise = Just Nothing
    executable_targets_and_errors =
        partitionEithers
        .
        map (\name →
            case find ((==name) . exeName) executables of
                Nothing → Left name
                Just executable → Right executable
        )
        .
        delete "library"
        $
        targets
-- @-node:gcross.20101012145613.1557:fetchBuildTargetsIn
-- @+node:gcross.20101012145613.1568:findDefaultCabalFile
findDefaultCabalFile :: MonadIO m ⇒ m FilePath
findDefaultCabalFile = liftIO $ do
    cabal_files ← fmap (filter ((== ".cabal") . takeExtension)) (getDirectoryContents ".")
    case cabal_files of
        [cabal_file] → return cabal_file
        [] → throwIO NoCabalFile
        _ → throwIO $ MultipleCabalFiles cabal_files
-- @-node:gcross.20101012145613.1568:findDefaultCabalFile
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
    PackageLocality →
    PackageDescription →
    HaskellLibrary →
    FilePath →
    Job InstalledPackageInfo
installPackage
    path_to_ghc_pkg
    package_locality
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
        ["register","––auto-ghci-libs","--"++show package_locality,"-"]
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
-- @+node:gcross.20101010201506.1510:installPackageUsingEnvironments
installPackageUsingEnvironments ::
    BuildEnvironment →
    InstallationEnvironment →
    PackageDescription →
    HaskellLibrary →
    Job InstalledPackageInfo
installPackageUsingEnvironments
    BuildEnvironment{..}
    InstallationEnvironment{..}
    package_description
    library
  = installPackage
        (pathToGHCPkg buildEnvironmentGHC)
        buildEnvironmentPackageLocality
        package_description
        library
        installationLibraryDirectory
-- @nonl
-- @-node:gcross.20101010201506.1510:installPackageUsingEnvironments
-- @+node:gcross.20101012145613.1564:installPackageUsingConfiguration
installPackageUsingConfiguration ::
    Configuration →
    HaskellLibrary →
    Job InstalledPackageInfo
installPackageUsingConfiguration =
    liftA3 installPackageUsingEnvironments
        configurationBuildEnvironment
        configurationInstallationEnvironment
        configurationPackageDescription
-- @-node:gcross.20101012145613.1564:installPackageUsingConfiguration
-- @+node:gcross.20101012145613.1561:installProgram
installProgram :: FilePath → ProgramFile → Job ()
installProgram program_directory File{..} = liftIO $ do
    noticeM "Blueprint.Tools.Compilers.GHC" $ "(GHC) Installing program " ++ takeFileName filePath ++ "..."
    infoM "Blueprint.Tools.Compilers.GHC" $ "(GHC) Copying " ++ filePath ++ " -> " ++ program_destination
    copyFile filePath program_destination
  where
    program_destination = replaceDirectory filePath program_directory
-- @-node:gcross.20101012145613.1561:installProgram
-- @+node:gcross.20101012145613.1563:installProgramUsingInstallationEnvironment
installProgramUsingInstallationEnvironment :: InstallationEnvironment → ProgramFile → Job ()
installProgramUsingInstallationEnvironment = installProgram . installationExecutableDirectory
-- @-node:gcross.20101012145613.1563:installProgramUsingInstallationEnvironment
-- @+node:gcross.20101012145613.1566:installProgramUsingConfiguration
installProgramUsingConfiguration :: Configuration → ProgramFile → Job ()
installProgramUsingConfiguration = installProgramUsingInstallationEnvironment . configurationInstallationEnvironment
-- @-node:gcross.20101012145613.1566:installProgramUsingConfiguration
-- @+node:gcross.20101012145613.1560:installTargets
installTargets :: Configuration → InstallTargets → Job ()
installTargets configuration InstallTargets{..} = do
    case libraryInstallTarget of
        Nothing → return ()
        Just library → installPackageUsingConfiguration configuration library >> return ()
    mapM_ (installProgramUsingConfiguration configuration) executableInstallTargets
-- @-node:gcross.20101012145613.1560:installTargets
-- @+node:gcross.20101004145951.1467:linkProgram
linkProgram ::
    FilePath →
    [String] →
    FilePath →
    HaskellModule →
    Job ProgramFile
linkProgram
    path_to_ghc
    additional_options
    program_filepath
    HaskellModule{haskellModuleLinkDependencyModules,haskellModuleLinkDependencyPackages}
  = once my_id
    .
    fmap (File program_filepath)
    $
    runIfDependencyOrProductHasChanged
        my_id
        (additional_options,dependency_digests,package_digests)
        (\old_digest → fmap (/= Just old_digest) (digestFileIfExists program_filepath))
        build
  where
    my_id =
        identifierInNamespace
            (uuid "eb95ef18-e0c3-476e-894c-aefb8e5b931a")
            ("linking program " ++ program_filepath)
    haskell_object_dependencies = Map.map haskellModuleObject haskellModuleLinkDependencyModules
    dependency_digests = Map.map fileDigest haskell_object_dependencies
    package_digests = Map.map installedPackageId haskellModuleLinkDependencyPackages
    ghc_arguments =
        Map.keys haskell_object_dependencies
        ++
        concat [["-package",package_name] | package_name ← Map.keys haskellModuleLinkDependencyPackages]
        ++
        ["-o",program_filepath]
        ++
        additional_options
    build = do
        liftIO . noticeM "Blueprint.Tools.Compilers.GHC" $ "(GHC) Linking program " ++ program_filepath
        liftIO . infoM "Blueprint.Tools.Compilers.GHC" $ "(GHC) Executing '" ++ (unwords (path_to_ghc:ghc_arguments)) ++ "'"
        fmap toTuple $
            runProductionCommandAndDigestOutputs
                (program_filepath :. E)
                path_to_ghc
                ghc_arguments
-- @nonl
-- @-node:gcross.20101004145951.1467:linkProgram
-- @+node:gcross.20101004145951.1475:linkProgramUsingBuildEnvironment
linkProgramUsingBuildEnvironment ::
    BuildEnvironment →
    FilePath →
    HaskellModule →
    Job ProgramFile
linkProgramUsingBuildEnvironment BuildEnvironment{..} =
    linkProgram
        (pathToGHC buildEnvironmentGHC)
        buildEnvironmentProgramLinkOptions
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
-- @+node:gcross.20101012145613.1571:loadDefaultPackageDescription
loadDefaultPackageDescription :: MonadIO m ⇒ m GenericPackageDescription
loadDefaultPackageDescription = findDefaultCabalFile >>= liftIO . readPackageDescription silent
-- @-node:gcross.20101012145613.1571:loadDefaultPackageDescription
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
-- @+node:gcross.20101012145613.1553:runTarget
runTarget :: [String] → Configuration → Job ()
runTarget ("build":[]) configuration@Configuration{..} =
    buildTargets configuration (computeDefaultBuildTargetsIn configurationPackageDescription) >> return ()
runTarget ("build":targets) configuration@Configuration{..} =
    buildTargets configuration (fetchBuildTargetsIn configurationPackageDescription targets) >> return ()
runTarget ("install":[]) configuration@Configuration{..} =
    buildTargets configuration (computeDefaultBuildTargetsIn configurationPackageDescription)
    >>=
    installTargets configuration
runTarget ("install":targets) configuration@Configuration{..} =
    buildTargets configuration (fetchBuildTargetsIn configurationPackageDescription targets)
    >>=
    installTargets configuration
runTarget [] _ = displayModesMessage
runTarget mode _ = liftIO $ do
    putStrLn $ "Unknown mode of operation: " ++ unwords mode
    putStrLn ""
    displayModesMessage
-- @-node:gcross.20101012145613.1553:runTarget
-- @+node:gcross.20101015124156.1544:scanForAndCompileModulesInAllOf
scanForAndCompileModulesInAllOf ::
    FilePath →
    [String] →
    PackageDatabase →
    KnownModules →
    FilePath →
    FilePath →
    [FilePath] →
    Job (HaskellModuleJobs,KnownModules)
scanForAndCompileModulesInAllOf
    path_to_ghc
    additional_compiler_options
    package_database
    known_modules
    interface_directory
    object_directory
    =
    fmap (
        createCompilationJobsForModules
            path_to_ghc
            package_database
            Map.empty
            additional_compiler_options
            interface_directory
            object_directory
    )
    .
    scanForModulesInAllOf
-- @-node:gcross.20101015124156.1544:scanForAndCompileModulesInAllOf
-- @+node:gcross.20101006110010.1481:scanForModulesIn
scanForModulesIn :: FilePath → Job (Map String (Job HaskellSource))
scanForModulesIn root = once my_id $ scanForModulesWithParentIn Nothing root
  where
    my_id =
        identifierInNamespace
            (uuid "cd195cae-ff8f-4d16-9884-9fb924af2a7f")
            ("scanning for haskell modules in " ++ show root)
-- @nonl
-- @-node:gcross.20101006110010.1481:scanForModulesIn
-- @+node:gcross.20101015124156.1542:scanForModulesInAllOf
scanForModulesInAllOf :: [FilePath] → Job (Map String (Job HaskellSource))
scanForModulesInAllOf roots = once my_id . fmap Map.unions . traverse scanForModulesIn $ roots
  where
    my_id =
        identifierInNamespace
            (uuid "ce7a5c4a-2e7b-49c3-8d31-0bbc29fceb23")
            ("scanning for haskell modules in " ++ show roots)
-- @-node:gcross.20101015124156.1542:scanForModulesInAllOf
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
                is_directory ← liftIO (doesDirectoryExist filepath)
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
                            (fmap (HaskellSource module_name . File filepath) (digestFile filepath))
                    _ → return Map.empty
            else return Map.empty
    )
  where
    appendToParent child = maybe child (<.> child) maybe_parent
-- @-node:gcross.20101006110010.1480:scanForModulesWithParentIn
-- @-node:gcross.20100927123234.1448:Functions
-- @+node:gcross.20101010201506.1518:Options
-- @+node:gcross.20100927123234.1432:GHC
ghc_search_option_path_to_ghc = identifier "8a0b2f67-9ff8-417d-aed0-372149d791d6" "path to ghc"
ghc_search_option_path_to_ghc_pkg = identifier "b832666c-f42f-4dc0-8ef9-561987334c37" "path to ghc-pkg"
ghc_search_option_desired_version = identifier "87cab19e-c87a-4480-8ed3-af04e4c4f6bc" "desired GHC version"
ghc_search_option_search_paths = identifier "fcb54ad5-4a10-419a-9e8c-12261952cfd9" "path to search for ghc"
ghc_package_database_option_locality = identifier "4b38e6c5-8162-49ea-9ca0-5a23e58c44b1" "should the local or global GHC package database be used"

ghc_options =
    Options
        Map.empty
        (Map.fromList
            [("with-ghc",(ghc_search_option_path_to_ghc,RequiredArgument "PATH"))
            ,("with-ghc-pkg",(ghc_search_option_path_to_ghc_pkg,RequiredArgument "PATH"))
            ,("with-ghc-version",(ghc_search_option_desired_version,RequiredArgument "VERSION"))
            ,("with-ghc-located-in",(ghc_search_option_search_paths,RequiredArgument "DIRECTORY"))
            ,("user",(ghc_package_database_option_locality,NoArgument "user"))
            ,("global",(ghc_package_database_option_locality,NoArgument "global"))
            ]
        )
        (Map.fromList
            [("tools.ghc.paths.ghc",ghc_search_option_path_to_ghc)
            ,("tools.ghc.paths.ghc-pkg",ghc_search_option_path_to_ghc_pkg)
            ,("tools.ghc.paths.search",ghc_search_option_search_paths)
            ,("tools.ghc.version",ghc_search_option_desired_version)
            ,("tools.ghc.package-locality",ghc_package_database_option_locality)
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
            ,(ghc_package_database_option_locality,("GHC","Use the (user|global) package database for both configuration and package installation;  defaults to global."))
            ]
        )
-- @-node:gcross.20100927123234.1432:GHC
-- @+node:gcross.20101010201506.1519:Installation
installation_option_prefix = identifier "d85dcf36-aab1-4618-8b9a-b07caf347d41" "installation directory prefix"
installation_option_bindir = identifier "90055f6f-9ba5-43d6-b59f-441f66ee0d1c" "installation directory for executables"
installation_option_libdir = identifier "1963880f-3f8d-4142-9132-a5efdc1f7264" "installation directory for libraries"

installation_options =
    Options
        Map.empty
        (Map.fromList
            [("prefix",(installation_option_prefix,RequiredArgument "PATH"))
            ,("bindir",(installation_option_bindir,RequiredArgument "PATH"))
            ,("libdir",(installation_option_libdir,RequiredArgument "PATH"))
            ]
        )
        (Map.fromList
            [("tools.ghc.installation.prefix",installation_option_prefix)
            ,("tools.ghc.installation.bindir",installation_option_bindir)
            ,("tools.ghc.installation.libdir",installation_option_libdir)
            ]
        )
        Map.empty
        (Map.fromList
            [(installation_option_prefix,("Installation","Installation prefix directory"))
            ,(installation_option_bindir,("Installation","Installation directory for executables"))
            ,(installation_option_libdir,("Installation","Installation directory for libraries"))
            ]
        )
-- @nonl
-- @-node:gcross.20101010201506.1519:Installation
-- @-node:gcross.20101010201506.1518:Options
-- @-others
-- @-node:gcross.20100927123234.1428:@thin GHC.hs
-- @-leo
