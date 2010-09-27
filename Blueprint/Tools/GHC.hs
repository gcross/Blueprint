-- @+leo-ver=4-thin
-- @+node:gcross.20100927123234.1428:@thin GHC.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100927123234.1429:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100927123234.1429:<< Language extensions >>
-- @nl

module Blueprint.Tools.GHC where

-- @<< Import needed modules >>
-- @+node:gcross.20100927123234.1430:<< Import needed modules >>
import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Abort
import Control.Monad.Trans.Goto

import Data.Binary
import Data.DeriveTH
import Data.Either
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable (traverse)
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

import Blueprint.Configuration
import Blueprint.Identifier
import Blueprint.Job
import Blueprint.Miscellaneous
import Blueprint.Options
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
-- @-node:gcross.20100927123234.1453:Exceptions
-- @+node:gcross.20100927123234.1433:Types
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
-- @+node:gcross.20100927123234.1441:GHC
data GHC = GHC
    {   ghcVersion :: Version
    ,   pathToGHC :: FilePath
    ,   pathToGHCPkg :: FilePath
    } deriving Typeable; $( derive makeBinary ''GHC )
-- @-node:gcross.20100927123234.1441:GHC
-- @+node:gcross.20100927123234.1443:GHCEnvironment
data GHCEnvironment = GHCEnvironment
    {   ghcEnvironmentGHC :: GHC
    ,   ghcEnvironmentPackageDatabase :: PackageDatabase
    } deriving Typeable

-- @-node:gcross.20100927123234.1443:GHCEnvironment
-- @+node:gcross.20100927123234.1445:GHCOptions
data GHCOptions = GHCOptions
    {   ghcOptionPathToGHC :: Maybe FilePath
    ,   ghcOptionPathToGHCPkg :: Maybe FilePath
    ,   ghcOptionDesiredVersion :: Maybe Version
    ,   ghcOptionSearchPaths :: [FilePath]
    } deriving (Eq,Show,Typeable)

$(derive makeBinary ''GHCOptions)
-- @nonl
-- @-node:gcross.20100927123234.1445:GHCOptions
-- @-node:gcross.20100927123234.1433:Types
-- @+node:gcross.20100927123234.1459:Values
-- @+node:gcross.20100927123234.1461:ghc_version_regex
ghc_version_regex = makeRegex "version ([0-9.]*)" :: Regex
-- @-node:gcross.20100927123234.1461:ghc_version_regex
-- @-node:gcross.20100927123234.1459:Values
-- @+node:gcross.20100927123234.1448:Functions
-- @+node:gcross.20100927123234.1450:configureGHC
configureGHC :: GHCOptions → Job GHC
configureGHC search_options@GHCOptions{..} =
    once my_uuid
    .
    cache my_uuid
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
    once my_uuid
    .
    cache my_uuid
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
