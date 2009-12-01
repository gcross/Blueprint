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

import Data.Array
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Data
import Data.Digest.Pure.MD5
import Data.Dynamic
import Data.Either
import Data.Either.Unwrap
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
import Distribution.PackageDescription as Package
import qualified Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.Verbosity
import Distribution.Version

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Process

import Text.PrettyPrint.ANSI.Leijen hiding ((</>),(<$>))

import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString.Lazy

import Blueprint.Cache.ExplicitDependencies
import Blueprint.Cache.ImplicitDependencies
import Blueprint.Configuration
import Blueprint.Error
import Blueprint.Miscellaneous
import Blueprint.Options
import Blueprint.Resources
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
newtype ResolvedPackages = ResolvedPackages [String]
-- @-node:gcross.20091128201230.1462:ResolvedPackages
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
import_matching_regex = fromRight . compile defaultCompOpt defaultExecOpt . L8.pack $ "\\s*import +(qualified +)?([A-Z][A-Za-z0-9_.]+)[\\s;]?"
-- @-node:gcross.20091121210308.2015:regular expressions
-- @-node:gcross.20091121210308.2014:Values
-- @+node:gcross.20091121210308.2016:Functions
-- @+node:gcross.20091121210308.2017:readDependenciesOf
readDependenciesOf :: FilePath -> IO [String]
readDependenciesOf =
    L.readFile
    >=>
    return
        .
        map (L8.unpack . fst . (! 2))
        .
        matchAllText import_matching_regex
-- @-node:gcross.20091121210308.2017:readDependenciesOf
-- @+node:gcross.20091122100142.1335:prefixWith
prefixWith :: String -> [String] -> [String]
prefixWith _ [] = []
prefixWith s list = s:intersperse s list
-- @-node:gcross.20091122100142.1335:prefixWith
-- @+node:gcross.20091127142612.1403:findAllObjectDependenciesOf
findAllObjectDependenciesOf :: Resources -> Resource -> [Resource]
findAllObjectDependenciesOf known_resources object_resource =
   Map.elems $ findAsMapAllObjectDependenciesOf known_resources object_resource
-- @-node:gcross.20091127142612.1403:findAllObjectDependenciesOf
-- @+node:gcross.20091127142612.1404:findAsMapAllObjectDependenciesOf
findAsMapAllObjectDependenciesOf :: Resources -> Resource -> Map ResourceId Resource
findAsMapAllObjectDependenciesOf known_resources object_resource =
    Map.insert (resourceId object_resource) object_resource
    .
    Map.unions
    .
    catMaybes
    .
    map (\(resource_name,resource_type) ->
        if resource_type /= "hi"
            then Nothing
            else
                case Map.lookup (resource_name,"o") known_resources of
                    Nothing ->
                        error $ "Unable to find in known resources the dependent resource with id " ++ show (resource_name,"o")
                    Just object_resource ->
                        Just (findAsMapAllObjectDependenciesOf known_resources object_resource)
    )
    .
    resourceDependencies
    $
    object_resource
-- @-node:gcross.20091127142612.1404:findAsMapAllObjectDependenciesOf
-- @+node:gcross.20091129000542.1705:qualifiedNameToPackageIdentifier
qualifiedNameToPackageIdentifier :: String -> PackageIdentifier
qualifiedNameToPackageIdentifier name =
    uncurry PackageIdentifier
    .
    (PackageName *** (readVersion . tail))
    .
    flip splitAt name
    .
    last
    .
    elemIndices '-'
    $
    name
-- @-node:gcross.20091129000542.1705:qualifiedNameToPackageIdentifier
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
getPackageModules configuration name =
    fmap (Set.fromList)
    $
    queryPackage configuration "exposed-modules" name
-- @-node:gcross.20091121210308.2019:getPackageModules
-- @+node:gcross.20091121210308.2024:findPackagesExposingModule
findPackagesExposingModule :: GHCConfiguration -> String -> [String]
findPackagesExposingModule tools package_name =
    words
    .
    unsafePerformIO
    .
    readProcess (ghcPackageManagerPath tools) ["--simple-output","find-module",package_name]
    $
    ""
-- @-node:gcross.20091121210308.2024:findPackagesExposingModule
-- @+node:gcross.20091128201230.1459:readPackageDescription
readPackageDescription :: FilePath -> PackageDescription
readPackageDescription =
    packageDescription
    .
    unsafePerformIO
    .   
    Distribution.PackageDescription.Parse.readPackageDescription silent
-- @-node:gcross.20091128201230.1459:readPackageDescription
-- @+node:gcross.20091128201230.1461:configurePackageResolutions
configurePackageResolutions :: GHCConfiguration -> PackageDescription -> String -> Configurer [String]
configurePackageResolutions tools package_description =
    configureUsingSectionWith config_reader config_writer automatic_configurer
  where
    config_reader = fmap words (getConfig ghcPackagesKey)
    config_writer = setConfig ghcPackagesKey . unwords
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
        myParListWHNF
        .
        map resolvePackage
        .
        buildDepends
        $
        package_description

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
                Just version -> Right $ package_name ++ "-" ++ showVersion version
-- @-node:gcross.20091128201230.1461:configurePackageResolutions
-- @+node:gcross.20091201134050.1973:configurePackageModules
configurePackageModules :: GHCConfiguration -> [String] -> String -> Configurer PackageModules
configurePackageModules configuration qualified_package_names =
    configureUsingSectionWith config_reader config_writer automatic_configurer
  where
    sorted_package_names = sort qualified_package_names
    config_reader =
        fmap words (getConfig ghcCachedPackageNamesKey)
        >>= \cached_package_names ->
                if cached_package_names == sorted_package_names
                    then fmap (Set.fromDistinctAscList . words) (getConfig ghcCachedPackageModulesKey)
                    else signalRefreshNeeded
    config_writer package_modules = do
        setConfig ghcCachedPackageNamesKey . unwords $ sorted_package_names
        setConfig ghcCachedPackageModulesKey . unwords . Set.toAscList $ package_modules
    automatic_configurer _ =
        mapBoth
            (errorMessage "finding exposed modules for the following packages")
            Set.unions
        .
        extractResultsOrError
        .
        myParListWHNF
        .
        map (
            \qualified_package_name ->
                case getPackageModules configuration qualified_package_name of
                    Just package_modules -> Right package_modules
                    Nothing -> Left . text $ qualified_package_name
        )
        $
        qualified_package_names
-- @-node:gcross.20091201134050.1973:configurePackageModules
-- @+node:gcross.20091129000542.1711:registerPackage
registerPackage :: GHCConfiguration -> InstalledPackageInfo -> IO (Maybe Doc)
registerPackage configuration = do
    readProcessWithExitCode
        (ghcPackageManagerPath configuration)
        ["update","-"]
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
-- @+node:gcross.20091129000542.1702:createInstalledPackageInfo
createInstalledPackageInfoFromPackageDescription ::
    Package.PackageDescription ->
    Bool -> -- is library exposed?
    [ModuleName] -> -- exposed modules
    [ModuleName] -> -- hidden modules
    [FilePath] -> -- import directories
    [FilePath] -> -- library directories
    [String] -> -- haskell libraries
    [String] -> -- extra libraries
    [String] -> -- extra GHCI libraries
    [FilePath] -> -- include directories
    [String] -> -- includes
    [PackageIdentifier] -> -- package dependencies
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
        <$> Package.package
        <*> Package.license
        <*> Package.copyright
        <*> Package.maintainer
        <*> Package.author
        <*> Package.stability
        <*> Package.homepage
        <*> Package.pkgUrl
        <*> Package.description
        <*> Package.category
-- @-node:gcross.20091129000542.1702:createInstalledPackageInfo
-- @+node:gcross.20091129000542.1703:installSimplePackage
installSimplePackage ::
    GHCConfiguration ->
    InstallerConfiguration ->
    Package.PackageDescription ->
    [String] ->
    [Resource] ->
    Maybe ErrorMessage
installSimplePackage
    ghc_configuration
    installer_configuration
    package_description
    dependency_package_names
    resources_to_install
  = let PackageIdentifier (PackageName name) version = Package.package package_description
        qualified_package_name = name ++ "-" ++ showVersion version
        library_destination_path =
            installerLibraryPath installer_configuration
            </>
            qualified_package_name
            </>
            (("ghc-" ++) . showVersion . ghcVersion $ ghc_configuration)
        haskell_libraries :: [FilePath]
        haskell_libraries =
            map (drop 3 . dotsToSubdirectories . resourceName)
            .
            filter ((=="a") . resourceType)
            $
            resources_to_install

        exposed_modules :: [ModuleName]
        exposed_modules =
            map (fromJust . simpleParse . resourceName)
            .
            filter ((== "hi") . resourceType)
            $
            resources_to_install

        installed_package_info :: InstalledPackageInfo
        installed_package_info =
            createInstalledPackageInfoFromPackageDescription
                package_description
                True
                exposed_modules
                []
                [library_destination_path]
                [library_destination_path]
                haskell_libraries
                []
                []
                []
                []
                (map qualifiedNameToPackageIdentifier dependency_package_names)
                []
                []
                []
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
            putStrLn $ "Registering " ++ qualified_package_name
            fmap (fmap (errorMessage "installing package")) $
                registerPackage ghc_configuration installed_package_info

    in case installation_result of
        Right Nothing -> Nothing
        Right (Just error_message) -> Just $ error_message
        Left (e :: SomeException) -> Just $ errorMessageText "installing package" (show e)
-- @-node:gcross.20091129000542.1703:installSimplePackage
-- @-node:gcross.20091129000542.1701:Package installation
-- @+node:gcross.20091121210308.2031:Error reporting
-- @+node:gcross.20091121210308.2032:reportUnknownModules
reportUnknownModules :: GHCConfiguration -> String -> [String] -> ErrorMessage
reportUnknownModules tools source_name =
    errorMessage ("tracing the following module dependencies for " ++ source_name)
    .
    vcat
    .
    map (\module_name -> text $
        case findPackagesExposingModule tools module_name of
            [] -> module_name ++ " (no idea where to find it)"
            packages -> module_name ++ " which appears in packages " ++ (show packages)
    )
-- @-node:gcross.20091121210308.2032:reportUnknownModules
-- @-node:gcross.20091121210308.2031:Error reporting
-- @+node:gcross.20091121210308.1275:Tools
-- @+node:gcross.20091121210308.2022:ghcCompile
ghcCompile ::
    GHCConfiguration ->
    [String] ->
    PackageModules ->
    Resources ->
    FilePath ->
    FilePath ->
    FilePath ->
    Resource ->
    (Resource,Resource)
ghcCompile
    tools
    options
    known_package_modules
    known_resources
    object_destination_directory
    interface_destination_directory
    cache_directory
    source_resource
    =
    (Resource
        {   resourceName = source_name
        ,   resourceType = "o"
        ,   resourceFilePath = object_filepath
        ,   resourceDigest = object_digest
        ,   resourceDependencies = source_id:implicit_dependencies
        }
    ,Resource
        {   resourceName = source_name
        ,   resourceType = "hi"
        ,   resourceFilePath = interface_filepath
        ,   resourceDigest = interface_digest
        ,   resourceDependencies = source_id:implicit_dependencies
        }
    )
  where
    source_filepath = resourceFilePath source_resource
    source_name = resourceName source_resource
    source_id = resourceId source_resource
    object_filepath = getFilePathForNameAndType object_destination_directory source_name "o"
    interface_filepath = getFilePathForNameAndType interface_destination_directory source_name "hi"

    scanner = do
        dependencies <- readDependenciesOf source_filepath

        let (unknown_dependencies,resource_dependencies) =
                partitionEithers
                .
                catMaybes
                .
                map (\module_name ->
                    if Set.member module_name known_package_modules
                        then Nothing
                        else let resource_id = (module_name,"hi")
                              in if Map.member resource_id known_resources
                                    then Just . Right $ resource_id
                                    else Just . Left $ module_name
                )
                $
                dependencies

        if null unknown_dependencies
            then return . Right $ resource_dependencies
            else return . Left . reportUnknownModules tools source_name $ unknown_dependencies

    builder =
        let arguments = 
                options ++
                ["-i"++interface_destination_directory
                ,"-c",source_filepath
                ,"-o",object_filepath
                ,"-ohi",interface_filepath
                ]
            path_to_ghc = ghcCompilerPath tools
        in do
            createDirectoryIfMissing True . takeDirectory $ object_filepath
            createDirectoryIfMissing True . takeDirectory $ interface_filepath
            putStrLn . unwords $ (path_to_ghc:arguments)
            compilation_result <- readProcessWithExitCode path_to_ghc arguments ""
            case compilation_result of
                (ExitFailure _,_,error_message) ->
                    return
                    .
                    Just
                    .
                    errorMessageTextWithLines ("compiling " ++ source_name)
                    $
                    error_message
                (ExitSuccess,_,_) -> return Nothing

    ((object_digest,interface_digest),implicit_dependencies) =
        case analyzeImplicitDependenciesAndRebuildIfNecessary
                builder
                scanner
                known_resources
                (cache_directory </> source_name <.> "o")
                [object_filepath,interface_filepath]
                (unwords options)
                source_resource
        of Left error_message -> ((Left error_message,Left error_message),[])
           Right ([object_digest,interface_digest],implicit_dependencies) ->
            ((Right object_digest,Right interface_digest),implicit_dependencies)
           x -> error $ "Programmer error:  Builder returned the wrong number of digests! (" ++ show x ++ ")"
-- @-node:gcross.20091121210308.2022:ghcCompile
-- @+node:gcross.20091121210308.2038:ghcCompileAll
ghcCompileAll ::
    GHCConfiguration ->
    [String] ->
    PackageModules ->
    FilePath ->
    FilePath ->
    FilePath ->
    Resources ->
    Resources
ghcCompileAll
    tools
    options
    known_package_modules
    object_destination_directory
    interface_destination_directory
    cache_directory
    old_resources
    =
    let new_resources = go old_resources (Map.elems old_resources)
        go accum_resources [] = accum_resources
        go accum_resources (resource:rest_resources) =
            if resourceType (resource) == "hs"
                then let (object_resource,interface_resource) =
                            ghcCompile
                                tools
                                options
                                known_package_modules
                                new_resources
                                object_destination_directory
                                interface_destination_directory
                                cache_directory
                                resource
                     in go (addResource object_resource . addResource interface_resource $ accum_resources) rest_resources
                else go accum_resources rest_resources
    in new_resources
-- @-node:gcross.20091121210308.2038:ghcCompileAll
-- @+node:gcross.20091127142612.1402:ghcLinkProgram
ghcLinkProgram ::
    GHCConfiguration ->
    [String] ->
    FilePath ->
    [String] ->
    [Resource] ->
    String ->
    FilePath ->
    Resource
ghcLinkProgram
    tools
    options
    cache_directory
    package_dependencies
    object_resources
    program_resource_name
    program_resource_filepath
    = Resource
        {   resourceName = program_resource_name
        ,   resourceType = ""
        ,   resourceFilePath = program_resource_filepath
        ,   resourceDigest = program_digest
        ,   resourceDependencies = map resourceId object_resources
        }
  where
    program_digest = either Left (Right . head) $
        analyzeExplicitDependenciesAndRebuildIfNecessary
            builder
            (cache_directory </> program_resource_name <.> "")
            [program_resource_filepath]
            ()
            object_resources

    builder = do
        createDirectoryIfMissing True . takeDirectory $ program_resource_filepath
        let arguments = 
                options ++
                flagsFromPackageDependencies package_dependencies ++
                ["-o",program_resource_filepath
                ] ++
                (map resourceFilePath object_resources)
            command = ghcCompilerPath tools
        putStrLn . unwords . (command:) $ arguments
        compilation_result <-
            readProcessWithExitCode
            command
            arguments
            ""
        case compilation_result of
            (ExitFailure _,_,error_message) ->
                return
                .
                Just
                .
                errorMessageTextWithLines ("linking " ++ program_resource_name)
                $
                error_message
            (ExitSuccess,_,_) -> return Nothing
-- @-node:gcross.20091127142612.1402:ghcLinkProgram
-- @-node:gcross.20091121210308.1275:Tools
-- @-others
-- @-node:gcross.20091121204836.1242:@thin GHC.hs
-- @-leo
