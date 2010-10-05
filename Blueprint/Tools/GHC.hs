-- @+leo-ver=4-thin
-- @+node:gcross.20100927123234.1428:@thin GHC.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100927123234.1429:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as LU
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

import Blueprint.Cache
import Blueprint.Configuration
import Blueprint.Identifier
import Blueprint.Job
import Blueprint.Miscellaneous
import Blueprint.Options
import Blueprint.Tools
-- @nonl
-- @-node:gcross.20100927123234.1430:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100927123234.1453:Exceptions
-- @+node:gcross.20100927123234.1454:GHCConfigurationException
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
-- @-node:gcross.20100927123234.1454:GHCConfigurationException
-- @+node:gcross.20100928173417.1461:UnknownModulesException
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
-- @-node:gcross.20100928173417.1461:UnknownModulesException
-- @-node:gcross.20100927123234.1453:Exceptions
-- @+node:gcross.20100927123234.1433:Types
-- @+node:gcross.20100927222551.1442:BuildTargetType
data BuildTargetType = LibraryTarget | ExecutableTarget

-- @-node:gcross.20100927222551.1442:BuildTargetType
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
-- @+node:gcross.20100927222551.1428:HaskellInterface
data HaskellInterface = HaskellInterface
    {   haskellInterfaceFilePath :: FilePath
    ,   haskellInterfaceDigest :: MD5Digest
    } deriving Typeable
-- @-node:gcross.20100927222551.1428:HaskellInterface
-- @+node:gcross.20100927222551.1430:HaskellObject
data HaskellObject = HaskellObject
    {   haskellObjectFilePath :: FilePath
    ,   haskellObjectDigest :: MD5Digest
    ,   haskellObjectLinkDependencyObjects :: Map FilePath HaskellObject
    ,   haskellObjectLinkDependencyPackages :: Set String
    } deriving Typeable
-- @-node:gcross.20100927222551.1430:HaskellObject
-- @+node:gcross.20100927222551.1452:HaskellSource
data HaskellSource = HaskellSource
    {   haskellSourceFilePath :: FilePath
    ,   haskellSourceDigest :: MD5Digest
    }
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
    KnownModuleInExternalPackage String
  | KnownModuleInProject (Job (HaskellInterface,HaskellObject))
-- @-node:gcross.20100927222551.1434:KnownModule
-- @+node:gcross.20100927222551.1436:KnownModules
type KnownModules = Map String KnownModule
-- @-node:gcross.20100927222551.1436:KnownModules
-- @+node:gcross.20101004145951.1471:LinkCache
data LinkCache = LinkCache
    {   linkCacheDependencyDigests :: Map FilePath MD5Digest
    ,   linkCachePackageDependencies :: Set String
    ,   linkCacheAdditionalOptions :: [String]
    ,   linkCacheProgramDigest :: MD5Digest
    } deriving Typeable; $( derive makeBinary ''LinkCache )
-- @nonl
-- @-node:gcross.20101004145951.1471:LinkCache
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
data BuildEnvironment = BuildEnvironment
    {   buildEnvironmentGHC :: GHC
    ,   buildEnvironmentPackageDatabase :: PackageDatabase
    ,   buildEnvironmentKnownModules :: KnownModules
    ,   buildEnvironmentCompileOptions :: [String]
    ,   buildEnvironmentLinkOptions :: [String]
    }
-- @-node:gcross.20100927222551.1446:BuildEnvironment
-- @-node:gcross.20100927123234.1433:Types
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
-- @+node:gcross.20100927222551.1451:compileToObject
compileToObject ::
    FilePath →
    PackageDatabase →
    KnownModules →
    [String] →
    FilePath →
    FilePath →
    HaskellSource →
    Job (HaskellInterface,HaskellObject)
compileToObject
    path_to_ghc
    package_database
    known_modules
    additional_options
    interface_filepath
    object_filepath
    HaskellSource{..}
  = onceAndCached my_uuid
    $
    \cache → do
        ( imported_modules
         ,package_dependencies
         ,haskell_object_dependencies
         ,dependency_digests
         ,interface_digest
         ,object_digest
         ) ← case cache of
            Just CompileCache{..} | haskellSourceDigest == compileCacheSourceDigest → do
                (package_dependencies,dependency_digests,haskell_object_dependencies) ← resolve compileCacheImportedModules
                let buildIt = build package_dependencies
                (interface_digest,object_digest) ←
                    fmap toT $
                    if (compileCacheDependencyDigests /= dependency_digests) ||
                       (compileCacheAdditionalOptions /= additional_options)
                        then buildIt
                        else do
                            maybe_digests ←
                                fmap sequence
                                .
                                traverse digestFileIfExists
                                .
                                fromT
                                $
                                (interface_filepath,object_filepath)
                            case maybe_digests of
                                Just digests
                                  | digests == compileCacheInterfaceDigest :. compileCacheObjectDigest :. E
                                    → return digests
                                _ → buildIt
                return
                    (compileCacheImportedModules
                    ,package_dependencies
                    ,haskell_object_dependencies
                    ,dependency_digests
                    ,interface_digest
                    ,object_digest
                    )
            Nothing → do
                imported_modules ← scan
                (package_dependencies,dependency_digests,haskell_object_dependencies) ← resolve imported_modules
                (interface_digest,object_digest) ← fmap toT (build package_dependencies)
                return
                    (imported_modules
                    ,package_dependencies
                    ,haskell_object_dependencies
                    ,dependency_digests
                    ,interface_digest
                    ,object_digest
                    )
        let (collected_object_dependencies,collected_package_dependencies) =
                second (Set.union . Set.fromList $ package_dependencies)
                .
                collectAllLinkDependencies
                $
                haskell_object_dependencies
        return
            (Just $ CompileCache
                haskellSourceDigest
                imported_modules
                dependency_digests
                additional_options
                interface_digest
                object_digest
            ,(HaskellInterface interface_filepath interface_digest
             ,HaskellObject object_filepath object_digest collected_object_dependencies collected_package_dependencies
             )
            )
  where
    my_uuid =
        inNamespace
            (uuid "a807f1d2-c62d-4e44-9e8b-4c53e8410dee")
            (haskellSourceFilePath ++ interface_filepath ++ object_filepath)

    scan =
        fmap extractImportedModulesFromHaskellSource
        .
        liftIO
        .
        L.readFile
        $
        haskellSourceFilePath


    resolve imported_modules = do
        (package_dependencies,haskell_interface_dependencies,haskell_object_dependencies) ←
            resolveModuleDependencies
                package_database
                known_modules
                imported_modules
        let dependency_digests =
                Map.fromList
                .
                map (haskellInterfaceFilePath &&& haskellInterfaceDigest)
                $
                haskell_interface_dependencies
        return (package_dependencies,dependency_digests,haskell_object_dependencies)

    build package_dependency_names = do
        liftIO . noticeM "Blueprint.Tools.Compilers.GHC" $ "(GHC) Compiling " ++ haskellSourceFilePath
        let ghc_arguments =
                 "-c":haskellSourceFilePath
                :"-o":object_filepath
                :"-ohi":interface_filepath
                :
                concatMap (("-package":) . (:[])) package_dependency_names
                ++
                additional_options
        liftIO . infoM "Blueprint.Tools.Compilers.GHC" $ "(GHC) Executing '" ++ (unwords (path_to_ghc:ghc_arguments)) ++ "'"
        runProductionCommandAndDigestOutputs
            (object_filepath :. interface_filepath :. E)
            path_to_ghc
            ghc_arguments
-- @-node:gcross.20100927222551.1451:compile
-- @+node:gcross.20100927222551.1438:computeBuildEnvironment
computeBuildEnvironment ::
    GHCEnvironment →
    PackageDescription →
    BuildTargetType →
    Map String (Job (HaskellInterface,HaskellObject)) →
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
                (Map.map KnownModuleInProject built_modules
                :map extractKnownModulesFromInstalledPackage installed_package_dependencies
                )
    ,   buildEnvironmentCompileOptions = shared_options ++ additional_compile_options
    ,   buildEnvironmentLinkOptions = shared_options ++ additional_link_options
    }
  where
    build_for_package_options = 
        case build_target_type of
            LibraryTarget → ["-package-name",display package]
            _ → []
    installed_package_dependencies =
        map (fromJust . findSatisfyingPackage ghcEnvironmentPackageDatabase) buildDepends
    shared_options =
        ("-i" ++ interface_directory)
        :
        build_for_package_options
-- @-node:gcross.20100927222551.1438:computeBuildEnvironment
-- @+node:gcross.20100929125042.1466:collectAllLinkDependencies
collectAllLinkDependencies :: [HaskellObject] → (Map FilePath HaskellObject,Set String)
collectAllLinkDependencies haskell_objects =
    (Map.union
        (Map.fromList . map (haskellObjectFilePath &&& id) $ haskell_objects)
        (Map.unions . map haskellObjectLinkDependencyObjects $ haskell_objects)
    ,Set.unions . map haskellObjectLinkDependencyPackages $ haskell_objects
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
-- @+node:gcross.20100927123234.1456:determineGHCVersion
determineGHCVersion :: FilePath → IO Version
determineGHCVersion = determineProgramVersion parseGHCVersion ["--version"]
-- @-node:gcross.20100927123234.1456:determineGHCVersion
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
extractKnownModulesFromInstalledPackage InstalledPackage{..} =
    Map.fromList
    .
    map (,KnownModuleInExternalPackage installedPackageQualifiedName)
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
-- @+node:gcross.20101004145951.1467:linkProgram
linkProgram ::
    FilePath →
    [String] →
    [HaskellObject] →
    FilePath →
    Job Program
linkProgram
    path_to_ghc
    additional_options
    haskell_objects
    program_filepath
  = onceAndCached my_uuid
    $
    \maybe_cache →
        case maybe_cache of
            Just cache@LinkCache{..}
              | linkCacheAdditionalOptions == additional_options
              , linkCacheDependencyDigests == dependency_digests
              , linkCachePackageDependencies == package_dependencies
              → do  maybe_digest ← digestFileIfExists program_filepath
                    case maybe_digest of
                        Just digest | digest == linkCacheProgramDigest →
                            return
                                (Just cache
                                ,Program program_filepath digest
                                )
                        _ → build
            _ → build
  where
    my_uuid = (inNamespace (uuid "eb95ef18-e0c3-476e-894c-aefb8e5b931a") program_filepath)
    (haskell_object_dependencies,package_dependencies) = collectAllLinkDependencies haskell_objects
    dependency_digests = Map.map haskellObjectDigest haskell_object_dependencies
    ghc_arguments =
        Map.keys haskell_object_dependencies
        ++
        concat [["-package",package_name] | package_name ← Set.elems package_dependencies]
        ++
        ["-o",program_filepath]
        ++
        additional_options
    build = do
        liftIO . noticeM "Blueprint.Tools.Compilers.GHC" $ "(GHC) Linking program " ++ program_filepath
        liftIO . infoM "Blueprint.Tools.Compilers.GHC" $ "(GHC) Executing '" ++ (unwords (path_to_ghc:ghc_arguments)) ++ "'"
        program_digest ←
            fmap toT $
            runProductionCommandAndDigestOutputs
                (program_filepath :. E)
                path_to_ghc
                ghc_arguments
        return $
            (Just $ LinkCache
                        dependency_digests
                        package_dependencies
                        additional_options
                        program_digest
            ,Program program_filepath program_digest
            )
-- @-node:gcross.20101004145951.1467:linkProgram
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
-- @+node:gcross.20100927123234.1458:parseGHCVersion
parseGHCVersion = extractVersion ghc_version_regex
-- @-node:gcross.20100927123234.1458:parseGHCVersion
-- @+node:gcross.20100928173417.1463:resolveModuleDependencies
resolveModuleDependencies ::
    PackageDatabase →
    KnownModules →
    [String] →
    Job ([String],[HaskellInterface],[HaskellObject])
resolveModuleDependencies PackageDatabase{..} known_modules module_names =
    case partitionEithers resolved_modules of
        ([],resolutions) → do
            let (package_names,jobs) = partitionEithers resolutions
            (haskell_interfaces,haskell_objctes) ← fmap unzip (sequenceA jobs)
            return (package_names,haskell_interfaces,haskell_objctes)
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
