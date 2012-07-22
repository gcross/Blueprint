-- Language extensions {{{
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
-- }}}

module Blueprint.Tools.GHC where

-- Imports {{{
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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (traverse,sequenceA,sequence,mapM)
import Data.Typeable

import qualified Distribution.Compiler as Compiler
import Distribution.InstalledPackageInfo (InstalledPackageInfo,showInstalledPackageInfo)
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
import System.Exit
import System.FilePath
import System.Log.Logger
import System.Process

import Text.PrettyPrint
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
-- }}}

-- File types {{{
declareFileType "HaskellInterface"
declareFileType "HaskellObject"
-- }}}

-- Types {{{
data BuildTargets = BuildTargets -- {{{
    {   libraryBuildTarget :: Maybe Library
    ,   executableBuildTargets :: [Executable]
    }
-- }}}
data CompileCache = CompileCache -- {{{
    {   compileCacheSourceDigest :: MD5Digest
    ,   compileCacheImportedModules :: [String]
    ,   compileCacheDependencyDigests :: Map FilePath MD5Digest
    ,   compileCacheAdditionalOptions :: [String]
    ,   compileCacheInterfaceDigest :: MD5Digest
    ,   compileCacheObjectDigest :: MD5Digest
    } deriving Typeable; $( derive makeBinary ''CompileCache )
-- }}}
data ForLibrary
data ForPrograms
data GHC = GHC -- {{{
    {   ghcVersion :: Version
    ,   pathToGHC :: FilePath
    ,   pathToGHCPkg :: FilePath
    } deriving Typeable; $( derive makeBinary ''GHC )
-- }}}
data HaskellLibrary = HaskellLibrary -- {{{
    {   haskellLibraryModuleInterfaces :: Map String HaskellInterfaceFile
    ,   haskellLibraryArchive :: ArchiveFile
    ,   haskellLibraryDigest :: MD5Digest
    ,   haskellLibraryDependencyPackages :: [InstalledPackage]
    ,   haskellLibraryIsExposed :: Bool
    ,   haskellLibraryExposedModules :: [String]
    } deriving Typeable
-- }}}
data HaskellModule = HaskellModule -- {{{
    {   haskellModuleName :: String
    ,   haskellModuleInterface :: HaskellInterfaceFile
    ,   haskellModuleObject :: HaskellObjectFile
    ,   haskellModuleLinkDependencyModules :: Map String HaskellModule
    ,   haskellModuleLinkDependencyPackages :: Map String InstalledPackage
    } deriving Typeable
-- }}}
type HaskellModuleJobs = Map String (Job HaskellModule)
data HaskellSource = HaskellSource -- {{{
    {   haskellSourceModuleName :: String
    ,   haskellSourceFile :: HaskellSourceFile
    } deriving Typeable
type HaskellSourceFile = FileOfType HaskellSource
-- }}}
data InstallationEnvironment = InstallationEnvironment -- {{{
    {   installationLibraryDirectory :: FilePath
    ,   installationExecutableDirectory :: FilePath
    }
-- }}}
data InstalledPackage = InstalledPackage -- {{{
    {   installedPackageId :: InstalledPackageId
    ,   installedPackageQualifiedName :: String
    ,   installedPackageName :: String
    ,   installedPackageVersion :: Version
    ,   installedPackageModules :: [String]
    } deriving (Typeable, Eq, Show)

$( derive makeBinary ''InstalledPackageId )
$( derive makeBinary ''InstalledPackage )
-- }}}
data InstallTargets = InstallTargets -- {{{
    {   libraryInstallTarget :: Maybe HaskellLibrary
    ,   executableInstallTargets :: [ProgramFile]
    }
-- }}}
data KnownModule = -- {{{
    KnownModuleInExternalPackage InstalledPackage
  | KnownModuleInProject (Job HaskellModule)
-- }}}
type KnownModules = Map String KnownModule
data PackageDatabase = PackageDatabase -- {{{
    {   packageDatabaseIndexedByInstalledPackageId :: Map InstalledPackageId InstalledPackage
    ,   packageDatabaseIndexedByPackageNameAndVersion :: Map String [(Version,[InstalledPackage])]
    ,   packageDatabaseIndexedByModuleName :: Map String [InstalledPackage]
    } deriving Typeable
-- }}}
data PackageLocality = Global | User deriving (Typeable,Eq) -- {{{

$(derive makeBinary ''PackageLocality)

instance Show PackageLocality where
    show User = "user"
    show Global = "global"

instance Read.Read PackageLocality where
    readPrec = ReadPrec.lift . ReadP.choice $
        [ReadP.string "user" >> return User
        ,ReadP.string "global" >> return Global
        ]
-- }}}
data GHCOptions = GHCOptions -- {{{
    {   ghcOptionPathToGHC :: Maybe FilePath
    ,   ghcOptionPathToGHCPkg :: Maybe FilePath
    ,   ghcOptionDesiredVersion :: Maybe Version
    ,   ghcOptionSearchPaths :: [FilePath]
    ,   ghcOptionPackageLocality :: PackageLocality
    ,   ghcOptionFlags :: [String]
    } deriving (Eq,Show,Typeable)

$(derive makeBinary ''GHCOptions)
-- }}}
data GHCEnvironment = GHCEnvironment -- {{{
    {   ghcEnvironmentGHC :: GHC
    ,   ghcEnvironmentPackageDatabase :: PackageDatabase
    ,   ghcEnvironmentPackageLocality :: PackageLocality
    ,   ghcEnvironmentFlags :: [String]
    } deriving Typeable
-- }}}
data BuildEnvironment = BuildEnvironment -- {{{
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
-- }}}
data Configuration = Configuration -- {{{
    {   configurationAr :: Maybe (ProgramConfiguration Ar)
    ,   configurationBuildEnvironment :: BuildEnvironment
    ,   configurationPackageDescription :: PackageDescription
    ,   configurationInstallationEnvironment :: InstallationEnvironment
    }
-- }}}
-- }}}

-- Exceptions {{{
data BadLibraryExportList = BadLibraryExportList [Either String String] deriving Typeable -- {{{

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
-- }}}
data BadMainModule = -- {{{
    MissingMainModule String
  | ExternalMainModule String InstalledPackage
  | NoExecutableWithNameInPackageDescription String PackageDescription
  deriving Typeable

instance Show BadMainModule where
    show (MissingMainModule missing_module_name) = "Unable to find the main module " ++ missing_module_name
    show (ExternalMainModule missing_module_name InstalledPackage{..}) = "The main module " ++ missing_module_name ++ " is not part of this project but rather is in the external package " ++ installedPackageQualifiedName
    show (NoExecutableWithNameInPackageDescription executable_name package_description) = "There is no executable with the name " ++ executable_name ++ " in the package description:" ++ show package_description

instance Exception BadMainModule
-- }}}
data BadTargets = BadTargets PackageDescription [String] [String] deriving Typeable -- {{{

instance Show BadTargets where
    show (BadTargets PackageDescription{..} missing_targets unbuildable_targets) =
        render
        .
        (text "Some of the specified targets are invalid:" $$)
        .
        (case missing_targets of
            [] → id
            _ → ((nest 4
                  $
                  indentedListWithHeading 6
                    "*) The following targets are not part of this project:"
                    missing_targets
                )$$)
        )
        .
        (case unbuildable_targets of
            [] → id
            _ → ((nest 4
                  $
                  indentedListWithHeading 6
                    "*) The following targets are marked as being unbuildable:"
                    unbuildable_targets
                )$$)
        )
        .
        indentedListWithHeading 4 "The following is the list of buildable targets in this project:"
        .
        (case library of
            Just Library{..} | buildable libBuildInfo → ("library":)
            Nothing → id
        )
        .
        sort
        .
        map exeName
        .
        filter (buildable . buildInfo)
        $
        executables

instance Exception BadTargets
-- }}}
data CabalFileException = -- {{{
    NoCabalFile
  | MultipleCabalFiles [FilePath]
  deriving Typeable

instance Show CabalFileException where
    show NoCabalFile = "No .cabal file is present in this directory."
    show (MultipleCabalFiles cabal_file_names) = "Multiple .cabal files are present " ++ show cabal_file_names

instance Exception CabalFileException
-- }}}
data GHCConfigurationException = -- {{{
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

-- }}}
data LibraryMissingFromPackageDescription = LibraryMissingFromPackageDescription PackageDescription deriving Typeable -- {{{

instance Show LibraryMissingFromPackageDescription where
    show (LibraryMissingFromPackageDescription package_description) =
        "A library section is missing from the package description:\n" ++ show package_description

instance Exception LibraryMissingFromPackageDescription
-- }}}
data MissingExposedModules = MissingExposedModules (Set String) deriving Typeable -- {{{

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
-- }}}
data UnknownModulesException = UnknownModulesException [(String,[String])] deriving Typeable -- {{{

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
-- }}}
data UnresolvedPackageDependenciesError = -- {{{
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
-- }}}
-- }}}

-- Values {{{
ghc_version_regex = makeRegex "version ([0-9.]*)" :: Regex

import_regex :: Regex
import_regex = makeRegex "^\\s*import\\s+(?:qualified\\s+)?([A-Z][A-Za-z0-9_.]*)"
-- }}}

-- Functions {{{
buildExecutable :: -- {{{
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
        .
        (:[])
  where
    program_filepath = executable_directory </> exeName
    my_id =
        identifierInNamespace
            (uuid "0cbcbb1f-e695-4612-b2b9-d3a9a125e04f")
            ("building executable " ++ program_filepath)
-- }}}
buildExecutableUsingBuildEnvironment :: -- {{{
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
-- }}}
buildExecutableUsingConfiguration :: -- {{{
    Configuration →
    FilePath →
    Executable →
    Job ProgramFile
buildExecutableUsingConfiguration = buildExecutableUsingBuildEnvironment . configurationBuildEnvironment
-- }}}
buildLibrary :: -- {{{
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
                Map.fromList
                .
                map ((filePath &&& fileDigest) . haskellModuleObject)
                .
                Map.elems
                $
                collected_modules
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
    archive_filepath = library_directory </> "libHS" ++ package_name <.> "a"
    my_id =
        identifierInNamespace
            (uuid "dd31d5fd-b093-43c9-bde5-8ea43ece1224")
            ("building library " ++ archive_filepath)
-- }}}
buildLibraryUsingBuildEnvironment :: -- {{{
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
-- }}}
buildLibraryUsingConfiguration :: -- {{{
    Configuration →
    FilePath →
    Library →
    Job HaskellLibrary
buildLibraryUsingConfiguration =
    buildLibraryUsingBuildEnvironment
        <$> (fromJust . configurationAr)
        <*> (display . package . configurationPackageDescription)
        <*> configurationBuildEnvironment
-- }}}
buildTargets :: Configuration → BuildTargets → Job InstallTargets -- {{{
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
-- }}}
checkForSatisfyingPackage :: PackageDatabase → Package.Dependency → Bool -- {{{
checkForSatisfyingPackage package_database dependency = isJust (findSatisfyingPackage package_database dependency)
-- }}}
collectAllLinkDependencies :: [HaskellModule] → (Map String HaskellModule,Map String InstalledPackage) -- {{{
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
-- }}}
compileToObject :: -- {{{
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
        identifierInNamespaceWithDifferentDisplayName
            (uuid "a807f1d2-c62d-4e44-9e8b-4c53e8410dee")
            (filePath haskellSourceFile ++ interface_filepath ++ object_filepath)
            ("compiling " ++ filePath haskellSourceFile)

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
            final_package_dependencies =
                Map.union collected_package_dependencies
                .
                Map.fromList
                .
                map (installedPackageQualifiedName &&& id)
                $
                package_dependencies
        return
            ((package_digests,dependency_digests,additional_options)
            ,(collected_module_dependencies
             ,final_package_dependencies)
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
-- }}}
computeBuildEnvironment :: -- {{{
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
    compile_options
    link_options
    interface_directory
    object_directory
    =
    BuildEnvironment
    {   buildEnvironmentGHC = ghcEnvironmentGHC
    ,   buildEnvironmentPackageDatabase = ghcEnvironmentPackageDatabase
    ,   buildEnvironmentPackageLocality = ghcEnvironmentPackageLocality
    ,   buildEnvironmentKnownModules = known_modules
    ,   buildEnvironmentLibraryCompileOptions =
             "-i"
            :("-i"++library_interface_directory)
            :"-hidir":library_interface_directory
            :"-odir":library_object_directory
            :"-package-name"
            :package_name
            :compile_options
    ,   buildEnvironmentProgramCompileOptions =
             "-i"
            :("-i"++program_interface_directory)
            :"-hidir":program_interface_directory
            :"-odir":program_object_directory
            :compile_options
    ,   buildEnvironmentProgramLinkOptions =
             "-i"
            :("-i"++program_interface_directory)
            :"-hidir":program_interface_directory
            :"-odir":program_object_directory
            :link_options
    ,   buildEnvironmentLibraryInterfaceDirectory = library_interface_directory
    ,   buildEnvironmentLibraryObjectDirectory = library_object_directory
    ,   buildEnvironmentProgramInterfaceDirectory = program_interface_directory
    ,   buildEnvironmentProgramObjectDirectory = program_object_directory
    }
  where
    installed_package_dependencies =
        map (fromJust . findSatisfyingPackage ghcEnvironmentPackageDatabase) buildDepends
    known_modules =
        Map.unions
            (Map.map KnownModuleInProject built_modules
            :map extractKnownModulesFromInstalledPackage installed_package_dependencies
            )
    interface_directory_option = ["-hidir" ++ interface_directory]
    package_name = display package
    library_interface_directory = interface_directory </> "library"
    library_object_directory = object_directory </> "library"
    program_interface_directory = interface_directory </> "program"
    program_object_directory = object_directory </> "program"
-- }}}
configure :: OptionValues → Job Configuration -- {{{
configure options = do
    cabal_file ← findDefaultCabalFile
    ghc_environment ← configureGHCEnvironmentUsingOptions options
    (package_description,_) ←
        readAndConfigurePackageDescription
            ghc_environment
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
-- }}}
configureGHC :: GHCOptions → Job GHC -- {{{
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
-- }}}
configureGHCEnvironment :: GHCOptions → Job GHCEnvironment -- {{{
configureGHCEnvironment options@GHCOptions{..} = do
    ghc@GHC{..} ← configureGHC options
    package_database ← configurePackageDatabase pathToGHCPkg ghcOptionPackageLocality
    return $
        GHCEnvironment
            ghc
            package_database
            ghcOptionPackageLocality
            ghcOptionFlags
-- }}}
configureGHCEnvironmentUsingOptions :: OptionValues → Job GHCEnvironment -- {{{
configureGHCEnvironmentUsingOptions = configureGHCEnvironment . extractGHCOptions
-- }}}
configureGHCUsingOptions :: OptionValues → Job GHC -- {{{
configureGHCUsingOptions = configureGHC . extractGHCOptions
-- }}}
configureInstallationEnvironmentUsingOptions :: OptionValues → Job InstallationEnvironment -- {{{
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
-- }}}
configurePackageDatabase :: FilePath → PackageLocality → Job PackageDatabase -- {{{
configurePackageDatabase path_to_ghc_pkg locality =
    onceAndCached my_id
    $
    \maybe_cache → do
        liftIO . infoM "Blueprint.Tools.Compilers.GHC" $
            "(GHC) Reading package atoms..."
        let options =
                case locality of
                    Global → ["--simple-output","--global"]
                    User → ["--simple-output"]
        liftIO . noticeM "Blueprint.Tools.Compilers.GHC" $
            unwords ("(GHC) Executing":path_to_ghc_pkg:"list":options)
        list_of_package_atoms ← fmap words (liftIO $ readProcess path_to_ghc_pkg ("list":options) "")
        installed_packages ← case maybe_cache of
            Just cache@(old_list_of_package_atoms,installed_packages)
                | old_list_of_package_atoms == list_of_package_atoms → return installed_packages
            _ → liftIO $ do
                    infoM "Blueprint.Tools.Compilers.GHC" "(GHC) Loading package database..."
                    liftIO . noticeM "Blueprint.Tools.Compilers.GHC" $
                        unwords ("(GHC) Executing":path_to_ghc_pkg:"dump":options)
                    fmap (map parseInstalledPackageInformation . splitOn "---")
                         (readProcess path_to_ghc_pkg ("dump":options) "")
        let package_database = constructPackageDatabaseFromInstalledPackages installed_packages
        return (Just (list_of_package_atoms,installed_packages), package_database)
  where
    my_id = identifier "734f6cec-8a79-4aa2-ad3b-ebe0937cd125" "configuring GHC package database"
-- }}}
constructPackageDatabaseFromInstalledPackages :: [InstalledPackage] → PackageDatabase -- {{{
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
-- }}}
createCompilationJobsForModules :: -- {{{
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
                (interface_directory </> dotsToPath module_name <.> "hi")
                (object_directory </> dotsToPath module_name <.> "o")
        ) module_sources

    new_known_modules =
        Map.union
            (Map.map KnownModuleInProject jobs)
            known_modules
-- }}}
defaultMain = -- {{{
    Main.defaultMain
        (ghc_options `mappend` installation_options)
        "configuration.cfg"
        "configuration.cache"
        "build.cache"
        configure
        runTarget
        displayModesMessage
-- }}}
determineGHCVersion :: FilePath → IO Version -- {{{
determineGHCVersion = determineProgramVersion parseGHCVersion ["--version"]
-- }}}
displayModesMessage :: MonadIO m ⇒ m () -- {{{
displayModesMessage = liftIO . putStrLn . render $
    indentedListWithHeading 4
        "The possible modes of operation are:"
        ["configure"
        ,"build [target1 target2...]"
        ,"install [target1 target2...]"
        ,"test"
        ]
-- }}}
dotsToPath = map (\c → if c == '.' then pathSeparator else c)
extractGHCOptions :: OptionValues → GHCOptions -- {{{
extractGHCOptions =
    GHCOptions
        <$> Map.lookup ghc_search_option_path_to_ghc
        <*> Map.lookup ghc_search_option_path_to_ghc_pkg
        <*> fmap readVersion . Map.lookup ghc_search_option_desired_version
        <*> maybe [] splitSearchPath . Map.lookup ghc_search_option_search_paths
        <*> maybe User read . Map.lookup ghc_package_database_option_locality
        <*> maybe [] words . Map.lookup ghc_cabal_flags
-- }}}
extractHaskellSourceDirectoriesFrom :: BuildInfo → [FilePath] -- {{{
extractHaskellSourceDirectoriesFrom BuildInfo{hsSourceDirs=[]} = ["."]
extractHaskellSourceDirectoriesFrom BuildInfo{hsSourceDirs} = hsSourceDirs
-- }}}
extractImportedModulesFromHaskellSource :: L.ByteString → [String] -- {{{
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
-- }}}
extractKnownModulesFromInstalledPackage :: InstalledPackage → KnownModules -- {{{
extractKnownModulesFromInstalledPackage installed_package@InstalledPackage{..} =
    Map.fromList
    .
    map (,KnownModuleInExternalPackage installed_package)
    $
    installedPackageModules
-- }}}
fetchAllBuildTargetsIn :: PackageDescription → BuildTargets -- {{{
fetchAllBuildTargetsIn PackageDescription{..} =
    BuildTargets
        (case library of
            Just Library{..} | buildable libBuildInfo → library
            _ → Nothing
        )
        (filter (buildable . buildInfo) executables)
-- }}}
fetchBuildTargetsIn :: PackageDescription → [String] → BuildTargets -- {{{
fetchBuildTargetsIn package_description@PackageDescription{..} targets =
    case (library_target_or_error,executable_targets_and_errors) of
        (Right library,([],executables)) → BuildTargets library executables
        (_,(bad_targets,_)) →
            throw
            .
            uncurry (BadTargets package_description)
            .
            partitionEithers
            .
            (case library_target_or_error of { Left error → (error:); Right _ → id })
            $
            bad_targets
  where
    library_target_or_error
      | "library" `elem` targets
        = case library of
            Nothing → Left (Left "library")
            Just library →
                if (buildable . libBuildInfo) library
                    then Right (Just library)
                    else Left (Right "library")
      | otherwise = Right (Nothing)
    executable_targets_and_errors =
        partitionEithers
        .
        map (\name →
            case find ((==name) . exeName) executables of
                Nothing → Left (Left name)
                Just executable →
                    if (buildable . buildInfo) executable
                        then Right executable
                        else Left (Right name)
        )
        .
        delete "library"
        $
        targets
-- }}}
findDefaultCabalFile :: MonadIO m ⇒ m FilePath -- {{{
findDefaultCabalFile = liftIO $ do
    cabal_files ← fmap (filter ((== ".cabal") . takeExtension)) (getDirectoryContents ".")
    case cabal_files of
        [cabal_file] → return cabal_file
        [] → throwIO NoCabalFile
        _ → throwIO $ MultipleCabalFiles cabal_files
-- }}}
findSatisfyingPackage :: PackageDatabase → Package.Dependency → Maybe InstalledPackage -- {{{
findSatisfyingPackage PackageDatabase{..} (Package.Dependency name version_range) =
    Map.lookup (display name) packageDatabaseIndexedByPackageNameAndVersion
    >>=
    find (flip withinRange version_range . fst)
    >>=
    return . head . snd
-- }}}
installPackage :: -- {{{
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
        archive_destination = destination_directory </> ("lib" ++ library_name) <.> (takeExtension archive_source)
    infoM "Blueprint.Tools.Compilers.GHC" $
        ("(GHC) Copying " ++ archive_source ++ " -> " ++ archive_destination)
    copyFile archive_source archive_destination
    runProductionCommand
        path_to_ghc_pkg
        ["register","--auto-ghci-libs","--"++show package_locality,"-"]
        (showInstalledPackageInfo installed_package_info)
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
-- }}}
installPackageUsingEnvironments :: -- {{{
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
-- }}}
installPackageUsingConfiguration :: -- {{{
    Configuration →
    HaskellLibrary →
    Job InstalledPackageInfo
installPackageUsingConfiguration =
    liftA3 installPackageUsingEnvironments
        configurationBuildEnvironment
        configurationInstallationEnvironment
        configurationPackageDescription
-- }}}
installProgram :: FilePath → ProgramFile → Job () -- {{{
installProgram program_directory File{..} = liftIO $ do
    noticeM "Blueprint.Tools.Compilers.GHC" $ "(GHC) Installing program " ++ takeFileName filePath ++ "..."
    infoM "Blueprint.Tools.Compilers.GHC" $ "(GHC) Copying " ++ filePath ++ " -> " ++ program_destination
    copyFile filePath program_destination
  where
    program_destination = replaceDirectory filePath program_directory
-- }}}
installProgramUsingInstallationEnvironment :: InstallationEnvironment → ProgramFile → Job () -- {{{
installProgramUsingInstallationEnvironment = installProgram . installationExecutableDirectory
-- }}}
installProgramUsingConfiguration :: Configuration → ProgramFile → Job () -- {{{
installProgramUsingConfiguration = installProgramUsingInstallationEnvironment . configurationInstallationEnvironment
-- }}}
installTargets :: Configuration → InstallTargets → Job () -- {{{
installTargets configuration InstallTargets{..} = do
    case libraryInstallTarget of
        Nothing → return ()
        Just library → installPackageUsingConfiguration configuration library >> return ()
    mapM_ (installProgramUsingConfiguration configuration) executableInstallTargets
-- }}}
linkProgram :: -- {{{
    FilePath →
    [String] →
    FilePath →
    [HaskellModule] →
    Job ProgramFile
linkProgram
    path_to_ghc
    additional_options
    program_filepath
    program_modules
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
    (collected_program_modules,collected_program_packages) = collectAllLinkDependencies program_modules
    dependency_digests =
        Map.fromList
        .
        map ((filePath &&& fileDigest) . haskellModuleObject)
        .
        Map.elems
        $
        collected_program_modules
    package_digests = Map.map installedPackageId collected_program_packages
    ghc_arguments =
        Map.keys dependency_digests
        ++
        concat [["-package",package_name] | package_name ← Map.keys package_digests]
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
-- }}}
linkProgramUsingBuildEnvironment :: -- {{{
    BuildEnvironment →
    FilePath →
    [HaskellModule] →
    Job ProgramFile
linkProgramUsingBuildEnvironment BuildEnvironment{..} =
    linkProgram
        (pathToGHC buildEnvironmentGHC)
        buildEnvironmentProgramLinkOptions
-- }}}
parseInstalledPackageInformation :: String → InstalledPackage -- {{{
parseInstalledPackageInformation package_description =
    InstalledPackage
    {   installedPackageId = installedPackageId
    ,   installedPackageQualifiedName = display sourcePackageId
    ,   installedPackageName = (display . Package.pkgName) sourcePackageId
    ,   installedPackageVersion = Package.pkgVersion sourcePackageId
    ,   installedPackageModules = map display exposedModules
    }
  where
    InstalledPackageInfo.InstalledPackageInfo{..} =
        case InstalledPackageInfo.parseInstalledPackageInfo package_description of
            ParseOk _ installed_package_info → installed_package_info
            ParseFailed parse_error → error $ "Error parsing description of package: " ++ show parse_error
-- }}}
lookupPackageNamed :: PackageDatabase → Package.PackageName → Maybe [(Version,[InstalledPackage])] -- {{{
lookupPackageNamed PackageDatabase{..} =
    flip Map.lookup packageDatabaseIndexedByPackageNameAndVersion
    .
    display
-- }}}
parseGHCVersion = extractVersion ghc_version_regex
readAndConfigurePackageDescription :: -- {{{
    GHCEnvironment →
    FilePath →
    Job (PackageDescription, FlagAssignment)
readAndConfigurePackageDescription GHCEnvironment{..} =
    liftIO . readPackageDescription silent
    >=>
    either
        (liftIO . throwIO . UnresolvedPackageDependenciesError ghcEnvironmentPackageDatabase)
        return
    .
    finalizePackageDescription
        (zip (map FlagName ghcEnvironmentFlags) (repeat True))
        (checkForSatisfyingPackage ghcEnvironmentPackageDatabase)
        buildPlatform
        (Compiler.CompilerId Compiler.GHC (ghcVersion ghcEnvironmentGHC))
        []
-- }}}
loadDefaultPackageDescription :: MonadIO m ⇒ m GenericPackageDescription -- {{{
loadDefaultPackageDescription = findDefaultCabalFile >>= liftIO . readPackageDescription silent
-- }}}
resolveModuleDependencies :: -- {{{
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
-- }}}
runTarget :: [String] → Configuration → Job () -- {{{
runTarget ("build":[]) configuration@Configuration{..} =
    buildTargets configuration (fetchAllBuildTargetsIn configurationPackageDescription) >> return ()
runTarget ("build":targets) configuration@Configuration{..} =
    buildTargets configuration (fetchBuildTargetsIn configurationPackageDescription targets) >> return ()
runTarget ("install":[]) configuration@Configuration{..} =
    buildTargets configuration (fetchAllBuildTargetsIn configurationPackageDescription)
    >>=
    installTargets configuration
runTarget ("install":targets) configuration@Configuration{..} =
    buildTargets configuration (fetchBuildTargetsIn configurationPackageDescription targets)
    >>=
    installTargets configuration
runTarget ("test":[]) configuration@Configuration{configurationPackageDescription=PackageDescription{executables}} =
    case find ((== "test") . exeName) executables of
        Nothing → liftIO $ do
            putStrLn "There is no executable target named \"test\" in this project."
            exitFailure
        Just executable@Executable{..}
          | not (buildable buildInfo) → liftIO $ do
                putStrLn "The test target is marked as being unbuildable;  most likely you need to pass in a configuration flag such as \"-ftest\" in order to enable it."
                exitFailure
          | otherwise →
                buildExecutableUsingConfiguration configuration "programs" executable
                >>=
                liftIO . system . filePath
                >>
                return ()
runTarget [] _ = displayModesMessage
runTarget mode _ = liftIO $ do
    putStrLn $ "Unknown mode of operation: " ++ unwords mode
    putStrLn ""
    displayModesMessage
-- }}}
scanForAndCompileModulesInAllOf :: -- {{{
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
            known_modules
            additional_compiler_options
            interface_directory
            object_directory
    )
    .
    scanForModulesInAllOf
-- }}}
scanForModulesIn :: FilePath → Job (Map String (Job HaskellSource)) -- {{{
scanForModulesIn root = once my_id $ scanForModulesWithParentIn Nothing root
  where
    my_id =
        identifierInNamespace
            (uuid "cd195cae-ff8f-4d16-9884-9fb924af2a7f")
            ("scanning for haskell modules in " ++ show root)
-- }}}
scanForModulesInAllOf :: [FilePath] → Job (Map String (Job HaskellSource)) -- {{{
scanForModulesInAllOf roots = once my_id . fmap Map.unions . traverse scanForModulesIn $ roots
  where
    my_id =
        identifierInNamespace
            (uuid "ce7a5c4a-2e7b-49c3-8d31-0bbc29fceb23")
            ("scanning for haskell modules in " ++ show roots)
-- }}}
scanForModulesWithParentIn :: -- {{{
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
        let filepath = root </> entry
            starts_with_uppercase = isUpper (head entry)
        is_directory ← liftIO (doesDirectoryExist filepath)
        case is_directory of
            True | starts_with_uppercase →
                scanForModulesWithParentIn
                    (Just . appendToParent $ entry)
                    filepath
            False | takeExtension entry == ".hs" →
                return
                $
                let module_name =
                        (if starts_with_uppercase
                            then appendToParent
                            else id
                        )
                        .
                        dropExtension
                        $
                        entry
                in Map.singleton
                    module_name
                    (fmap (HaskellSource module_name . File filepath) (digestFile filepath))
            _ → return Map.empty
    )
  where
    appendToParent child = maybe child (<.> child) maybe_parent
-- }}}
-- }}}

-- Options {{{
-- GHC {{{
ghc_search_option_path_to_ghc = identifier "8a0b2f67-9ff8-417d-aed0-372149d791d6" "path to ghc"
ghc_search_option_path_to_ghc_pkg = identifier "b832666c-f42f-4dc0-8ef9-561987334c37" "path to ghc-pkg"
ghc_search_option_desired_version = identifier "87cab19e-c87a-4480-8ed3-af04e4c4f6bc" "desired GHC version"
ghc_search_option_search_paths = identifier "fcb54ad5-4a10-419a-9e8c-12261952cfd9" "path to search for ghc"
ghc_package_database_option_locality = identifier "4b38e6c5-8162-49ea-9ca0-5a23e58c44b1" "should the local or global GHC package database be used"
ghc_cabal_flags = identifier "12320207-ee12-4b91-a86e-26af92907ad7" "cabal flag"

ghc_options =
    Options
        (Map.fromList
            [('f',(ghc_cabal_flags,RequiredArgument "FLAG"))  -- '
            ]
        )
        (Map.fromList
            [("with-ghc",(ghc_search_option_path_to_ghc,RequiredArgument "PATH"))
            ,("with-ghc-pkg",(ghc_search_option_path_to_ghc_pkg,RequiredArgument "PATH"))
            ,("with-ghc-version",(ghc_search_option_desired_version,RequiredArgument "VERSION"))
            ,("with-ghc-located-in",(ghc_search_option_search_paths,RequiredArgument "DIRECTORY"))
            ,("user",(ghc_package_database_option_locality,NoArgument "user"))
            ,("global",(ghc_package_database_option_locality,NoArgument "global"))
            ,("cabal-flag",(ghc_cabal_flags,RequiredArgument "FLAG"))
            ]
        )
        (Map.fromList
            [("tools.ghc.paths.ghc",ghc_search_option_path_to_ghc)
            ,("tools.ghc.paths.ghc-pkg",ghc_search_option_path_to_ghc_pkg)
            ,("tools.ghc.paths.search",ghc_search_option_search_paths)
            ,("tools.ghc.version",ghc_search_option_desired_version)
            ,("tools.ghc.package-locality",ghc_package_database_option_locality)
            ,("tools.ghc.cabal-flags",ghc_cabal_flags)
            ]
        )
        (Map.fromList
            [(ghc_search_option_search_paths,Right . intercalate [searchPathSeparator])
            ,(ghc_cabal_flags,Right . unwords)
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
-- }}}
-- Installation {{{
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
-- }}}
-- }}}
