-- @+leo-ver=4-thin
-- @+node:gcross.20091121204836.1242:@thin GHC.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091122100142.1309:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20091122100142.1309:<< Language extensions >>
-- @nl

module Blueprint.Tools.GHC where

-- @<< Import needed modules >>
-- @+node:gcross.20091121210308.1269:<< Import needed modules >>
import Prelude hiding (catch)

import Control.Arrow hiding ((<+>))
import Control.Applicative
import Control.Applicative.Infix
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Control.Parallel.Strategies

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Data
import Data.Digest.Pure.MD5
import Data.Dynamic
import Data.Either
import Data.Either.Unwrap
import Data.ErrorMessage
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Version

import Distribution.ModuleName
import Distribution.InstalledPackageInfo
            (InstalledPackageInfo_(..)
            ,InstalledPackageInfo
            ,showInstalledPackageInfo
            )
import Distribution.Package
            (PackageIdentifier(..)
            ,PackageName(..)
            ,Dependency(..)
            ,InstalledPackageId(..)
            )
import qualified Distribution.Package as Package
import Distribution.PackageDescription (PackageDescription)
import qualified Distribution.PackageDescription as PackageDescription
import Distribution.PackageDescription.Configuration
import qualified Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.Verbosity
import Distribution.Version

import System.Directory
import System.Exit
import System.FilePath
import System.FilePath.Find (fileName,(==?),extension)
import qualified System.FilePath.Find as Find
import System.IO
import System.IO.Unsafe
import System.Process

import Text.PrettyPrint.ANSI.Leijen hiding ((</>),(<$>))
import Text.PrettyPrint.HughesPJ (render)

import Blueprint.Cache.ExplicitDependencies
import Blueprint.Cache.ImplicitDependencies
import Blueprint.Configuration
import Blueprint.Miscellaneous
import Blueprint.Options
import Blueprint.Resources
import Blueprint.Tools
import Blueprint.Tools.Installer

import Debug.Trace
-- @-node:gcross.20091121210308.1269:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091129000542.1587:Keys
ghcOptionSectionKey = makeOptionSectionKey "GHC"

ghcVersionKey = makeConfigurationKey "ghc version"
ghcCompilerPathKey = makeConfigurationKey "path to ghc"
ghcPackageManagerKey = makeConfigurationKey "path to ghc-pkg"
ghcPackagesKey = makeConfigurationKey "packages"
ghcCachedPackageNamesKey = makeConfigurationKey "cached package names"
ghcCachedPackageModulesKey = makeConfigurationKey "cached package modules"
-- @-node:gcross.20091129000542.1587:Keys
-- @+node:gcross.20091121210308.1270:Types
-- @+node:gcross.20091121210308.1271:GHCConfiguration
data GHCConfiguration = GHCConfiguration
    {   ghcVersion :: Version
    ,   ghcCompilerPath :: String
    ,   ghcPackageManagerPath :: String
    } deriving (Show)
-- @-node:gcross.20091121210308.1271:GHCConfiguration
-- @+node:gcross.20091129000542.1481:GHCOptions
data GHCOptions = GHCOptions
    {   ghcOptionCompilerPath :: Maybe FilePath
    ,   ghcOptionPackageManagerPath :: Maybe FilePath
    } deriving (Typeable, Show)

-- @-node:gcross.20091129000542.1481:GHCOptions
-- @+node:gcross.20091121210308.2025:PackageModules
type PackageModules = Set String
-- @-node:gcross.20091121210308.2025:PackageModules
-- @+node:gcross.20091128201230.1462:ResolvedPackages
newtype ResolvedPackages = ResolvedPackages [InstalledPackageId]
-- @-node:gcross.20091128201230.1462:ResolvedPackages
-- @+node:gcross.20091212120817.2104:PackageInformation
data PackageInformation = PackageInformation
    {   packageName :: String
    ,   packageQualifiedName :: String
    ,   packageIdentifier :: PackageIdentifier
    ,   packageExternalDependencies :: [Dependency]
    ,   packageDescription :: PackageDescription
    ,   packageConfigurationFilePath :: FilePath
    }
-- @-node:gcross.20091212120817.2104:PackageInformation
-- @+node:gcross.20100503170100.2079:PackageResolution
data ResolvedPackage = ResolvedPackage
    {   resolvedInstalledPackageId :: !InstalledPackageId
    ,   resolvedPackageName :: !String
    ,   resolvedPackageVersion :: !Version
    ,   resolvedPackageQualifiedName :: !String
    } deriving (Eq,Ord,Read,Show)
-- @-node:gcross.20100503170100.2079:PackageResolution
-- @-node:gcross.20091121210308.1270:Types
-- @+node:gcross.20091127142612.1405:Instances
-- @+node:gcross.20091127142612.1406:ConfigurationData GHCConfiguration
instance ConfigurationData GHCConfiguration where
    readConfig =
        liftM3 GHCConfiguration
            (fmap readVersion $ getConfig ghcVersionKey)
            (getConfig ghcCompilerPathKey)
            (getConfig ghcPackageManagerKey)
    writeConfig =
        (setConfig ghcVersionKey . showVersion . ghcVersion)
        <^(>>)^>
        (setConfig ghcCompilerPathKey . ghcCompilerPath)
        <^(>>)^>
        (setConfig ghcPackageManagerKey . ghcPackageManagerPath)
-- @-node:gcross.20091127142612.1406:ConfigurationData GHCConfiguration
-- @+node:gcross.20091128000856.1410:AutomaticallyConfigurable GHCConfiguration
instance AutomaticallyConfigurable GHCConfiguration where
    automaticallyConfigure parsed_options =
        case lookupAndUnwrapOptionSection ghcOptionSectionKey parsed_options of
            Nothing -> configureFromScratch
            Just opt@(GHCOptions maybe_path_to_ghc maybe_path_to_ghc_pkg) ->
                case (maybe_path_to_ghc,maybe_path_to_ghc_pkg) of
                    (Nothing,Nothing) ->
                        configureFromScratch
                    (Just path_to_ghc,Just path_to_ghc_pkg) ->
                        verifyConsistentVersionsAndReturn path_to_ghc path_to_ghc_pkg
                    (Just path_to_ghc,Nothing) ->
                        findProgramUsingPath "ghc-pkg" path_to_ghc
                        >>=
                        verifyConsistentVersionsAndReturn path_to_ghc 
                    (Nothing, Just path_to_ghc_pkg) ->
                        findProgramUsingPath "ghc" path_to_ghc_pkg
                        >>=
                        flip verifyConsistentVersionsAndReturn path_to_ghc_pkg
      where
        -- @        @+others
        -- @+node:gcross.20091129000542.1491:configurationError
        configurationError = leftErrorMessageText "configuring GHC"
        -- @nonl
        -- @-node:gcross.20091129000542.1491:configurationError
        -- @+node:gcross.20091129000542.1492:configureFromScratch
        configureFromScratch :: Either ErrorMessage GHCConfiguration
        configureFromScratch =
            case unsafePerformIO . findExecutable $ "ghc" of
                Nothing -> configurationError "Unable to find ghc in the path"
                Just path_to_ghc -> do
                    path_to_ghc_pkg <- findProgramUsingPath "ghc-pkg" path_to_ghc
                    verifyConsistentVersionsAndReturn path_to_ghc path_to_ghc_pkg
        -- @-node:gcross.20091129000542.1492:configureFromScratch
        -- @+node:gcross.20091129000542.1493:findProgramUsingPath
        findProgramUsingPath program_name = findProgramUsingDirectory program_name . takeDirectory
        -- @nonl
        -- @-node:gcross.20091129000542.1493:findProgramUsingPath
        -- @+node:gcross.20091129000542.1494:findProgramUsingDirectory
        findProgramUsingDirectory :: String -> FilePath -> Either ErrorMessage FilePath
        findProgramUsingDirectory program_name directory_to_search
            | unsafePerformIO . doesFileExist $ first_location_to_check
                = Right first_location_to_check
            | Just location <- unsafePerformIO . findExecutable $ program_name
                = Right location
            | otherwise
                = configurationError $ "Unable to find " ++ show program_name
          where
            first_location_to_check = directory_to_search </> program_name
        -- @-node:gcross.20091129000542.1494:findProgramUsingDirectory
        -- @+node:gcross.20091129000542.1495:verifyConsistentVersionsAndReturn
        verifyConsistentVersionsAndReturn :: FilePath -> FilePath -> Either ErrorMessage GHCConfiguration
        verifyConsistentVersionsAndReturn path_to_ghc path_to_ghc_pkg =
            mapLeft (errorMessage "configuring GHC") $ do
                (ghc_version,ghc_pkg_version) <- liftA2 (,) (getVersionOf path_to_ghc) (getVersionOf path_to_ghc_pkg)
                if ghc_version == ghc_pkg_version 
                        then return $
                            GHCConfiguration
                                {   ghcVersion = ghc_version
                                ,   ghcCompilerPath = path_to_ghc
                                ,   ghcPackageManagerPath = path_to_ghc_pkg
                                }
                        else Left . text $
                                "'ghc' and 'ghc-pkg' have different version! ("
                                ++ showVersion ghc_version ++
                                " /= "
                                ++ showVersion ghc_pkg_version ++
                                ")"
          where
            getVersionOf :: String -> Either Doc Version
            getVersionOf path_to_program =
                mapLeft (\(_ :: SomeException) ->
                    text $ "Unable to determine the version of " ++ path_to_program
                )
                .
                unsafePerformIO
                .
                try
                $
                (
                    readProcess path_to_program ["--version"] ""
                    >>=
                    evaluate
                    .
                    readVersion
                    .
                    last
                    .
                    words
                )
        -- @-node:gcross.20091129000542.1495:verifyConsistentVersionsAndReturn
        -- @-others
-- @-node:gcross.20091128000856.1410:AutomaticallyConfigurable GHCConfiguration
-- @-node:gcross.20091127142612.1405:Instances
-- @+node:gcross.20091121210308.2014:Values
-- @+node:gcross.20091121210308.2015:regular expressions
import_matching_regex = compileRegularExpression 2 "\\s*import +(qualified +)?([A-Z][A-Za-z0-9_.]*)[\\s;]?"
pragma_matching_regex = compileRegularExpression 1 "\\s*\\{\\-\\# +BLUEPRINT-LINK-DEPENDENCY +([A-Za-z0-9_.]* +[A-Za-z0-9_.]*) +\\#\\-\\}[\\s;]?"
-- @-node:gcross.20091121210308.2015:regular expressions
-- @-node:gcross.20091121210308.2014:Values
-- @+node:gcross.20091121210308.2016:Functions
-- @+node:gcross.20091121210308.2017:readDependenciesOf
readDependenciesOf :: FilePath -> IO [String]
readDependenciesOf = fmap (applyRegularExpression import_matching_regex) . L.readFile
-- @-node:gcross.20091121210308.2017:readDependenciesOf
-- @+node:gcross.20091122100142.1335:prefixWith
prefixWith :: String -> [String] -> [String]
prefixWith _ [] = []
prefixWith s list = s:intersperse s list
-- @-node:gcross.20091122100142.1335:prefixWith
-- @+node:gcross.20091129000542.1709:makeEverythingReadableIn
makeEverythingReadableIn :: FilePath -> IO ()
makeEverythingReadableIn path = do
    putStrLn $ "Setting read permissions on " ++ path
    is_file <- doesFileExist path
    if is_file
        then setPermissions path (Permissions True False False False)
        else
            setPermissions path (Permissions True False False True)
            >>
            fmap (filter ((/= '.') . head)) (getDirectoryContents path)
            >>=
            mapM_ (makeEverythingReadableIn . (path </>))
-- @-node:gcross.20091129000542.1709:makeEverythingReadableIn
-- @+node:gcross.20091129000542.1710:createDirectoryNoisilyIfMissing
createDirectoryNoisilyIfMissing directory =
    doesDirectoryExist directory
    >>=
    flip unless (
      do
        putStrLn $ "Creating directory " ++ directory
        createDirectoryIfMissing True directory
    )
-- @-node:gcross.20091129000542.1710:createDirectoryNoisilyIfMissing
-- @+node:gcross.20091130193227.2256:flagsFromPackageDependencies
flagsFromPackageDependencies :: [String] -> [String]
flagsFromPackageDependencies [] = []
flagsFromPackageDependencies (package:rest) = "-package":package:flagsFromPackageDependencies rest
-- @nonl
-- @-node:gcross.20091130193227.2256:flagsFromPackageDependencies
-- @+node:gcross.20091204093401.2928:toQualifiedName
toQualifiedName :: PackageIdentifier -> String
toQualifiedName (PackageIdentifier (PackageName name) version) = name ++ "-" ++ showVersion version
-- @-node:gcross.20091204093401.2928:toQualifiedName
-- @+node:gcross.20091210094146.1982:parseDependency
parseDependency dependency_string =
    fromMaybe (error $ "Unable to parse dependency string " ++ dependency_string)
    .
    simpleParse
    $
    dependency_string
-- @-node:gcross.20091210094146.1982:parseDependency
-- @+node:gcross.20091212120817.2103:loadInformationFromCabalFile
loadInformationFromCabalFile :: PackageInformation
loadInformationFromCabalFile = unsafePerformIO $ do
    cabal_filepaths <- Find.find (fileName ==? ".") (extension ==? ".cabal") "."
    when (null cabal_filepaths) $ do
        putStrLn "Unable to find a .cabal file."
        exitFailure
    let package_description = readPackageDescription . head $ cabal_filepaths
        package_identifier = PackageDescription.package package_description
        PackageIdentifier (PackageName package_name) version = package_identifier
    return PackageInformation
        {   packageDescription = package_description
        ,   packageExternalDependencies = PackageDescription.buildDepends package_description
        ,   packageIdentifier = package_identifier
        ,   packageName = package_name
        ,   packageQualifiedName = package_name ++ "-" ++ showVersion version
        ,   packageConfigurationFilePath = package_name <.> "cfg"
        }
-- @-node:gcross.20091212120817.2103:loadInformationFromCabalFile
-- @+node:gcross.20091215145007.1610:libraryLinkFlags
libraryLinkFlags :: PackageInformation -> [String]
libraryLinkFlags package_information =
    (map ("-l"++) . PackageDescription.extraLibs $ build_info)
    ++
    (map ("-L"++) . PackageDescription.extraLibDirs $ build_info)
  where
    build_info = PackageDescription.libBuildInfo . fromJust . PackageDescription.library . packageDescription $ package_information
-- @-node:gcross.20091215145007.1610:libraryLinkFlags
-- @-node:gcross.20091121210308.2016:Functions
-- @+node:gcross.20091129000542.1479:Options processing
ghcOptions =
    OptionSection
    {   optionSectionKey = ghcOptionSectionKey
    ,   optionSectionOptions =
        [   Option "ghc"
                [] ["with-ghc"]
                (ArgumentRequired "PROGRAM")
                "location of the Glasglow Haskell Compiler"
        ,   Option "ghc-pkg"
                [] ["with-ghc-pkg"]
                (ArgumentRequired "PROGRAM")
                "location of the GHC package database management tool"
        ]
    ,   optionSectionPostprocessor = postprocessOptions
    }
  where
    postprocessOptions :: Map String [Maybe String] -> Either Doc Dynamic
    postprocessOptions option_map = fmap toDyn $
        liftA2 GHCOptions
            (lookupOptionAndVerifyFileExists "ghc" option_map)
            (lookupOptionAndVerifyFileExists "ghc-pkg" option_map)
-- @-node:gcross.20091129000542.1479:Options processing
-- @+node:gcross.20091121210308.2023:Package queries
-- @+node:gcross.20091121210308.2018:queryPackage
queryPackage :: GHCConfiguration -> String -> String -> Maybe [String]
queryPackage tools field_name package_name =
    case unsafePerformIO $
            readProcessWithExitCode (ghcPackageManagerPath tools) ["field",package_name,field_name] ""
    of (ExitSuccess,response,_) -> Just . filter (/= (field_name ++ ":")) . words $ response 
       _ -> Nothing
-- @-node:gcross.20091121210308.2018:queryPackage
-- @+node:gcross.20091121210308.2019:getPackageModules
getPackageModules :: GHCConfiguration -> String -> Maybe PackageModules
getPackageModules configuration qualified_package_name =
    fmap (Set.fromList)
    $
    queryPackage configuration "exposed-modules" qualified_package_name

-- @-node:gcross.20091121210308.2019:getPackageModules
-- @+node:gcross.20091121210308.2024:findPackagesExposingModule
findPackagesExposingModule :: GHCConfiguration -> String -> [String]
findPackagesExposingModule tools module_name =
    words
    .
    unsafePerformIO
    .
    readProcess (ghcPackageManagerPath tools) ["--simple-output","find-module",module_name]
    $
    ""
-- @-node:gcross.20091121210308.2024:findPackagesExposingModule
-- @+node:gcross.20091128201230.1459:readPackageDescription
readPackageDescription :: FilePath -> PackageDescription
readPackageDescription =
    flattenPackageDescription
    .
    unsafePerformIO
    .   
    Distribution.PackageDescription.Parse.readPackageDescription silent
-- @-node:gcross.20091128201230.1459:readPackageDescription
-- @+node:gcross.20091128201230.1461:configurePackageResolutions
configurePackageResolutions :: GHCConfiguration -> [Dependency] -> String -> Configurer [ResolvedPackage]
configurePackageResolutions tools package_description =
    configureUsingSectionWith config_reader config_writer automatic_configurer
  where
    config_reader = fmap read (getConfig ghcPackagesKey)
    config_writer = setConfig ghcPackagesKey . show
    automatic_configurer _ =
        (\(unresolved_packages,resolved_packages) ->
            if null unresolved_packages
                then Right resolved_packages
                else Left
                     .
                     errorMessage "resolving package dependencies for"
                     .
                     vcat
                     .
                     map (
                        (\(package_name,versions_found) ->
                            text package_name
                            <+>
                            (list . map (text . showVersion) $ versions_found)
                        )
                     )
                     $
                     unresolved_packages                     
        )
        .
        partitionEithers
        .
        parMap rwhnf resolvePackage
        $
        package_description

    resolvePackage :: Dependency -> Either (String,[Version]) ResolvedPackage
    resolvePackage dependency@(Dependency (PackageName package_name) version_range) =
        let versions_found = 
                map readVersion
                .
                fromMaybe []
                .
                queryPackage tools "version"
                $
                package_name
        in case find (flip withinRange version_range) versions_found of
                Nothing -> Left (package_name,versions_found)
                Just package_version -> Right $
                    let qualified_package_name =
                            package_name ++ "-" ++ showVersion package_version
                        installed_package_id =
                            InstalledPackageId
                            .
                            head
                            .
                            fromJust
                            .
                            queryPackage tools "id"
                            $
                            qualified_package_name
                    in ResolvedPackage
                        {   resolvedInstalledPackageId = installed_package_id
                        ,   resolvedPackageName = package_name
                        ,   resolvedPackageVersion = package_version
                        ,   resolvedPackageQualifiedName = qualified_package_name
                        }
-- @-node:gcross.20091128201230.1461:configurePackageResolutions
-- @+node:gcross.20091201134050.1973:configurePackageModules
configurePackageModules :: GHCConfiguration -> [ResolvedPackage] -> String -> Configurer PackageModules
configurePackageModules configuration packages =
    configureUsingSectionWith config_reader config_writer automatic_configurer
  where
    sorted_packages = sort packages
    config_reader =
        fmap read (getConfig ghcCachedPackageNamesKey)
        >>= \cached_packages ->
                if cached_packages == sorted_packages
                    then fmap (Set.fromDistinctAscList . words) (getConfig ghcCachedPackageModulesKey)
                    else signalRefreshNeeded
    config_writer package_modules = do
        setConfig ghcCachedPackageNamesKey . show $ sorted_packages
        setConfig ghcCachedPackageModulesKey . unwords . Set.toAscList $ package_modules
    automatic_configurer _ =
        mapBoth
            (errorMessage "finding exposed modules for the following packages")
            Set.unions
        .
        gatherResultsOrError
        .
        parMap rwhnf (
            \(ResolvedPackage { resolvedPackageQualifiedName = qualified_package_name}) ->
                case getPackageModules configuration qualified_package_name of
                    Just package_modules -> Right package_modules
                    Nothing -> Left . text $ qualified_package_name
        )
        $
        sorted_packages
-- @-node:gcross.20091201134050.1973:configurePackageModules
-- @+node:gcross.20091129000542.1711:registerPackage
registerPackage :: GHCConfiguration -> InstalledPackageInfo -> IO (Maybe Doc)
registerPackage configuration = do
    readProcessWithExitCode
        (ghcPackageManagerPath configuration)
        ["update","-"]
    .
    (\x -> trace x $ x)
    .
    showInstalledPackageInfo
    >=>
    \(exit_code,_,error_message) ->
        return $ case exit_code of
            ExitSuccess -> Nothing 
            ExitFailure _ -> Just . vcat . map text . lines $ error_message
-- @-node:gcross.20091129000542.1711:registerPackage
-- @-node:gcross.20091121210308.2023:Package queries
-- @+node:gcross.20091129000542.1701:Package installation
-- @+node:gcross.20091129000542.1702:createInstalledPackageInfoFromPackageDescription
createInstalledPackageInfoFromPackageDescription ::
    PackageDescription ->
    Bool -> -- is the package exposed?
    [ModuleName] -> -- exposed modules
    [ModuleName] -> -- hidden modules
    [FilePath] -> -- import directories
    [FilePath] -> -- library directories
    [String] -> -- haskell libraries
    [String] -> -- extra libraries
    [String] -> -- extra GHCI libraries
    [FilePath] -> -- include directories
    [String] -> -- includes
    [InstalledPackageId] -> -- package dependencies
    [String] -> -- hugs options
    [String] -> -- cc options
    [String] -> -- ld options
    [FilePath] -> -- framework directories
    [String] -> -- frameworks
    [FilePath] -> -- haddock interfaces
    [FilePath] -> -- haddock HTMLs
    InstalledPackageInfo
createInstalledPackageInfoFromPackageDescription
    = InstalledPackageInfo
        <$> (
                InstalledPackageId
                .
                render
                .
                disp
                .
                PackageDescription.package
            )
        <*> PackageDescription.package
        <*> PackageDescription.license
        <*> PackageDescription.copyright
        <*> PackageDescription.maintainer
        <*> PackageDescription.author
        <*> PackageDescription.stability
        <*> PackageDescription.homepage
        <*> PackageDescription.pkgUrl
        <*> PackageDescription.description
        <*> PackageDescription.category
-- @-node:gcross.20091129000542.1702:createInstalledPackageInfoFromPackageDescription
-- @+node:gcross.20091129000542.1703:installSimplePackage
installSimplePackage ::
    GHCConfiguration ->
    InstallerConfiguration ->
    PackageInformation ->
    [ResolvedPackage] ->
    [Resource] ->
    Maybe ErrorMessage
installSimplePackage
    ghc_configuration
    installer_configuration
    package_information
    resolved_package_dependencies
    resources_to_install
  = let library_destination_path =
            installerLibraryPath installer_configuration
            </>
            (packageQualifiedName package_information)
            </>
            (("ghc-" ++) . showVersion . ghcVersion $ ghc_configuration)

        haskell_libraries :: [FilePath]
        haskell_libraries =
            map (drop 3 . dotsToSubdirectories . resourceName)
            .
            filter ((=="a") . resourceType)
            $
            resources_to_install

        package_description = packageDescription package_information
        library = fromJust . PackageDescription.library $ package_description
        build_info = PackageDescription.libBuildInfo library

        hidden_modules = PackageDescription.otherModules build_info

        exposed_modules :: [ModuleName]
        exposed_modules =
            if not . null . PackageDescription.exposedModules $ library
               then PackageDescription.exposedModules library
               else (\\ hidden_modules)
                    .
                    map (fromJust . simpleParse . resourceName)
                    .
                    filter ((== "hi") . resourceType)
                    $
                    resources_to_install

        installed_package_info :: InstalledPackageInfo
        installed_package_info =
            createInstalledPackageInfoFromPackageDescription
                package_description
                (PackageDescription.libExposed library)
                exposed_modules
                hidden_modules
                [library_destination_path]
                [library_destination_path]
                haskell_libraries
                []
                []
                []
                []
                (map resolvedInstalledPackageId resolved_package_dependencies)
                []
                []
                (libraryLinkFlags package_information)
                []
                []
                []
                []

        installation_result = unsafePerformIO . try $  do
            createDirectoryNoisilyIfMissing library_destination_path
            forM_ resources_to_install $ \resource ->
                let source_filepath = resourceFilePath resource
                    destination_filepath =
                        library_destination_path
                        </>
                        (dotsToSubdirectories . resourceName $ resource)
                        <.>
                        (resourceType resource)
                    destination_directory = takeDirectory destination_filepath
                in do
                    createDirectoryNoisilyIfMissing destination_directory
                    putStrLn $ "Copying " ++ source_filepath ++ " --> " ++ destination_filepath
                    copyFile source_filepath destination_filepath
            makeEverythingReadableIn library_destination_path
            putStrLn $ "Registering " ++ (packageQualifiedName package_information)
            fmap (fmap (errorMessage "installing package")) $
                registerPackage ghc_configuration installed_package_info

    in case installation_result of
        Right Nothing -> Nothing
        Right (Just error_message) -> Just $ error_message
        Left (e :: SomeException) -> Just $ errorMessageText "installing package" (show e)
-- @-node:gcross.20091129000542.1703:installSimplePackage
-- @-node:gcross.20091129000542.1701:Package installation
-- @+node:gcross.20091121210308.1275:Tools
-- @+node:gcross.20091121210308.2022:ghcCompile
ghcCompile ::
    GHCConfiguration ->
    FilePath ->
    [String] ->
    PackageModules ->
    FilePath ->
    FilePath ->
    Resources ->
    Resource ->
    [Resource]
ghcCompile
    configuration
    cache_directory
    options
    known_package_modules
    object_destination_directory
    interface_destination_directory
    known_resources
    source_resource
    = [object_resource,interface_resource]
  where
    source_filepath = resourceFilePath source_resource
    source_name = resourceName source_resource
    source_id = resourceId source_resource
    object_filepath = getFilePathForNameAndType object_destination_directory source_name "o"
    interface_filepath = getFilePathForNameAndType interface_destination_directory source_name "hi"

    object_resource = Resource
        {   resourceName = source_name
        ,   resourceType = "o"
        ,   resourceFilePath = object_filepath
        ,   resourceDigest = object_digest
        ,   resourceLinkDependencies = link_dependency_resources
        }
    interface_resource = Resource
        {   resourceName = source_name
        ,   resourceType = "hi"
        ,   resourceFilePath = interface_filepath
        ,   resourceDigest = interface_digest
        ,   resourceLinkDependencies = notLinkable interface_resource
        }

    scanner =
        (
            runScanner
                import_matching_regex
                (\module_name ->
                    if Set.member module_name known_package_modules
                        then Nothing
                        else Just [(BuildDependency,(module_name,"hi"))
                                  ,(LinkDependency,(module_name,"o"))
                                  ]
                )
                (\(_,(resource_name,resource_type)) ->
                    if resource_type == "hi"
                        then Just . text . (resource_name ++) $
                                 case findPackagesExposingModule configuration resource_name of
                                    [] -> " (no idea where to find it)"
                                    packages -> " which appears in packages " ++ (show packages)
                        else Nothing
                )
                known_resources
                source_filepath
        )
        `thenScanner`
        (
            runScanner
                pragma_matching_regex
                (\dependency -> let [resource_name,resource_type] = words dependency in Just [(LinkDependency,(resource_name,resource_type))])
                undefined
                known_resources
                source_filepath
        )

    builder =
        runProductionCommand
            ("Error compiling " ++ source_name ++ ":")
            [object_filepath,interface_filepath]
            []
            (ghcCompilerPath configuration)
            (options ++
                ["-i"++interface_destination_directory
                ,"-c",source_filepath
                ,"-o",object_filepath
                ,"-ohi",interface_filepath
                ]
            )

    ([object_digest,interface_digest],[],link_dependency_resources) =
        analyzeImplicitDependenciesAndRebuildIfNecessary
            builder
            scanner
            known_resources
            (cache_directory </> source_name <.> "o")
            [object_filepath,interface_filepath]
            []
            (unwords options)
            source_resource
-- @-node:gcross.20091121210308.2022:ghcCompile
-- @+node:gcross.20091121210308.2038:ghcCompileAdditional
ghcCompileAdditional ::
    GHCConfiguration ->
    FilePath ->
    [String] ->
    PackageModules ->
    FilePath ->
    FilePath ->
    Resources ->
    Resources ->
    Resources
ghcCompileAdditional
    configuration
    cache_directory
    options
    known_package_modules
    object_destination_directory
    interface_destination_directory
    =
    compileAdditionalWithImplicitDependencies
        ["hs"]
        (ghcCompile
            configuration
            cache_directory
            options
            known_package_modules
            object_destination_directory
            interface_destination_directory
        )
-- @-node:gcross.20091121210308.2038:ghcCompileAdditional
-- @+node:gcross.20091201161628.1568:ghcCompileAll
ghcCompileAll ::
    GHCConfiguration ->
    FilePath ->
    [String] ->
    PackageModules ->
    FilePath ->
    FilePath ->
    Resources ->
    Resources
ghcCompileAll
    configuration
    cache_directory
    options
    known_package_modules
    object_destination_directory
    interface_destination_directory
    source_resources
    =
    ghcCompileAdditional
        configuration
        cache_directory
        options
        known_package_modules
        object_destination_directory
        interface_destination_directory
        source_resources
        Map.empty
-- @-node:gcross.20091201161628.1568:ghcCompileAll
-- @+node:gcross.20091127142612.1402:ghcLinkProgram
ghcLinkProgram ::
    GHCConfiguration ->
    FilePath ->
    [String] ->
    [String] ->
    FilePath ->
    [ResourceId] ->
    Resources ->
    Resource
ghcLinkProgram _ _ _ _ _ [] _ = error "You need at least one object to link for a program."
ghcLinkProgram
    configuration
    cache_directory
    options
    package_dependencies
    program_destination_directory
    program_object_ids@((program_resource_name,_):_)
    resources
    = let resource = Resource
            {   resourceName = program_resource_name
            ,   resourceType = ""
            ,   resourceFilePath = program_resource_filepath
            ,   resourceDigest = program_digest
            ,   resourceLinkDependencies = notLinkable resource
            }
      in resource
  where
    program_resource_filepath = program_destination_directory </> program_resource_name

    error_message_heading = "Error linking program " ++ program_resource_name ++ ":"

    program_digest = fmap (head . fst) $
        attemptGetResources error_message_heading resources program_object_ids
        >>=
        findAllObjectDependenciesOf resources . Set.fromList
        >>=
        \program_resources_set ->
            let program_resources = Set.toAscList program_resources_set
                builder =
                    runProductionCommand
                        error_message_heading
                        [program_resource_filepath]
                        []
                        (ghcCompilerPath configuration)
                        (concat
                            [options
                            ,flagsFromPackageDependencies package_dependencies
                            ,["-o",program_resource_filepath]
                            ,map resourceFilePath program_resources
                            ]
                        )
            in analyzeExplicitDependenciesAndRebuildIfNecessary_
                builder
                (cache_directory </> program_resource_name <.> "")
                [program_resource_filepath]
                []
                (unwords options)
                program_resources
-- @-node:gcross.20091127142612.1402:ghcLinkProgram
-- @+node:gcross.20091201183231.1597:ghcLinkPrograms
ghcLinkPrograms ::
    GHCConfiguration ->
    FilePath ->
    [String] ->
    [String] ->
    FilePath ->
    [[ResourceId]] ->
    Resources ->
    [Resource]
ghcLinkPrograms _ _ _ _ _ [] _ = error "You need at least program to link."
ghcLinkPrograms
    tools
    cache_directory
    options
    package_dependencies
    program_destination_directory
    program_object_id_groups
    object_resources
    = map (\program_object_ids ->
        ghcLinkProgram
            tools
            cache_directory
            options
            package_dependencies
            program_destination_directory
            program_object_ids
            object_resources
    ) program_object_id_groups
-- @-node:gcross.20091201183231.1597:ghcLinkPrograms
-- @-node:gcross.20091121210308.1275:Tools
-- @-others
-- @-node:gcross.20091121204836.1242:@thin GHC.hs
-- @-leo
