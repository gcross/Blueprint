-- @+leo-ver=4-thin
-- @+node:gcross.20091121204836.1242:@thin GHC.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091122100142.1309:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}
-- @-node:gcross.20091122100142.1309:<< Language extensions >>
-- @nl

module Blueprint.Tools.GHC where

-- @<< Import needed modules >>
-- @+node:gcross.20091121210308.1269:<< Import needed modules >>
import Control.Arrow hiding ((<+>))
import Control.Applicative.Infix
import Control.Applicative
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
import Data.Version

import Distribution.Package
import Distribution.PackageDescription
import qualified Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.Version

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Process

import Text.PrettyPrint.ANSI.Leijen hiding ((</>))

import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString.Lazy

import Blueprint.Cache.ExplicitDependencies
import Blueprint.Cache.ImplicitDependencies
import Blueprint.Configuration
import Blueprint.Error
import Blueprint.Miscellaneous
import Blueprint.Options
import Blueprint.Resources

import Debug.Trace
-- @-node:gcross.20091121210308.1269:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091121210308.1270:Types
-- @+node:gcross.20091121210308.1271:GHCTools
data GHCTools = GHCTools
    {   ghcVersion :: Version
    ,   ghcCompilerPath :: String
    ,   ghcPackageQueryPath :: String
    } deriving (Show)
-- @-node:gcross.20091121210308.1271:GHCTools
-- @+node:gcross.20091129000542.1481:GHCToolsOptions
data GHCToolsOptions = GHCToolsOptions
    {   ghcOptionCompilerPath :: Maybe FilePath
    ,   ghcOptionPackageQueryPath :: Maybe FilePath
    } deriving (Typeable)

-- @-node:gcross.20091129000542.1481:GHCToolsOptions
-- @+node:gcross.20091121210308.2025:PackageModules
type PackageModules = Map String String
-- @-node:gcross.20091121210308.2025:PackageModules
-- @+node:gcross.20091128201230.1462:ResolvedPackages
newtype ResolvedPackages = ResolvedPackages [String]
-- @-node:gcross.20091128201230.1462:ResolvedPackages
-- @-node:gcross.20091121210308.1270:Types
-- @+node:gcross.20091127142612.1405:Instances
-- @+node:gcross.20091127142612.1406:ConfigurationData GHCTools
instance ConfigurationData GHCTools where
    readConfig =
        liftM3 GHCTools
            (fmap readVersion $ getConfig "version")
            (getConfig "path to compiler")
            (getConfig "path to package manager")
    writeConfig =
        (setConfig "version" . showVersion . ghcVersion)
        <^(>>)^>
        (setConfig "path to compiler" . ghcCompilerPath)
        <^(>>)^>
        (setConfig "path to package manager" . ghcPackageQueryPath)
-- @-node:gcross.20091127142612.1406:ConfigurationData GHCTools
-- @+node:gcross.20091128000856.1410:AutomaticallyConfigurable GHCTools
instance AutomaticallyConfigurable GHCTools where
    automaticallyConfigure wrapped_options =
        case fmap unwrapDynamic wrapped_options of
            Nothing -> configureFromScratch
            Just (GHCToolsOptions Nothing Nothing) -> configureFromScratch
            Just (GHCToolsOptions (Just path_to_ghc) (Just path_to_ghc_pkg)) -> verifyConsistentVersionsAndReturn path_to_ghc path_to_ghc_pkg
            Just (GHCToolsOptions (Just path_to_ghc) Nothing) -> configureUsingPath path_to_ghc
            Just (GHCToolsOptions Nothing (Just path_to_ghc_pkg)) -> configureUsingPath path_to_ghc_pkg
      where
        -- @        @+others
        -- @+node:gcross.20091129000542.1491:configurationError
        configurationError = leftErrorMessageText "configuring GHCTools"
        -- @nonl
        -- @-node:gcross.20091129000542.1491:configurationError
        -- @+node:gcross.20091129000542.1492:configureFromScratch
        configureFromScratch :: Either ErrorMessage GHCTools
        configureFromScratch =
            case unsafePerformIO . findExecutable $ "ghc" of
                Nothing -> configurationError "Unable to find ghc in the path"
                Just path_to_ghc -> configureUsingPath path_to_ghc
        -- @-node:gcross.20091129000542.1492:configureFromScratch
        -- @+node:gcross.20091129000542.1493:configureUsingPath
        configureUsingPath = configureUsingDirectory . takeDirectory
        -- @nonl
        -- @-node:gcross.20091129000542.1493:configureUsingPath
        -- @+node:gcross.20091129000542.1494:configureUsingDirectory
        configureUsingDirectory :: FilePath -> Either ErrorMessage GHCTools
        configureUsingDirectory directory_to_search = do
            path_to_ghc <- findProgram "ghc"
            path_to_ghc_pkg <- findProgram "ghc-pkg"
            verifyConsistentVersionsAndReturn path_to_ghc path_to_ghc_pkg
          where
            findProgram program_name
                | unsafePerformIO . doesFileExist $ first_location_to_check
                    = Right first_location_to_check
                | Just location <- unsafePerformIO . findExecutable $ program_name
                    = Right location
                | otherwise
                    = configurationError $ "Unable to find " ++ show program_name
              where
                first_location_to_check = directory_to_search </> program_name
        -- @nonl
        -- @-node:gcross.20091129000542.1494:configureUsingDirectory
        -- @+node:gcross.20091129000542.1495:verifyConsistentVersionsAndReturn
        verifyConsistentVersionsAndReturn :: FilePath -> FilePath -> Either ErrorMessage GHCTools
        verifyConsistentVersionsAndReturn path_to_ghc path_to_ghc_pkg =
            let ghc_version = getVersionOf path_to_ghc
                ghc_pkg_version = getVersionOf path_to_ghc_pkg
            in if ghc_version == ghc_pkg_version 
                    then Right $
                        GHCTools
                            {   ghcVersion = ghc_version
                            ,   ghcCompilerPath = path_to_ghc
                            ,   ghcPackageQueryPath = path_to_ghc_pkg
                            }
                    else configurationError $
                            "'ghc' and 'ghc-pkg' have different version! ("
                            ++ showVersion ghc_version ++
                            " /= "
                            ++ showVersion ghc_pkg_version ++
                            ")"
          where
            getVersionOf path_to_program =
                readVersion
                .
                last
                .
                words
                .
                unsafePerformIO
                $
                readProcess path_to_program ["--version"] ""
        -- @-node:gcross.20091129000542.1495:verifyConsistentVersionsAndReturn
        -- @-others

-- @-node:gcross.20091128000856.1410:AutomaticallyConfigurable GHCTools
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
-- @-node:gcross.20091121210308.2016:Functions
-- @+node:gcross.20091129000542.1479:Options processing
ghcToolsOptions section_heading =
    OptionSection
    {   optionSectionHeading = section_heading
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
        liftA2 GHCToolsOptions
            (lookupOptionAndVerifyFileExists "ghc" option_map)
            (lookupOptionAndVerifyFileExists "ghc-pkg" option_map)
-- @-node:gcross.20091129000542.1479:Options processing
-- @+node:gcross.20091121210308.2023:Package Queries
-- @+node:gcross.20091121210308.2018:queryPackage
queryPackage :: GHCTools -> String -> String -> Maybe [String]
queryPackage tools field_name package_name =
    case unsafePerformIO $
            readProcessWithExitCode (ghcPackageQueryPath tools) ["field",package_name,field_name] ""
    of (ExitSuccess,response,_) -> Just . filter (/= (field_name ++ ":")) . words $ response 
       _ -> Nothing
-- @-node:gcross.20091121210308.2018:queryPackage
-- @+node:gcross.20091121210308.2019:getPackage
getPackage :: GHCTools -> String -> Maybe (Map String String)
getPackage tools name = fmap (Map.fromList . map (flip (,) name)) $ queryPackage tools "exposed-modules" name
-- @-node:gcross.20091121210308.2019:getPackage
-- @+node:gcross.20091121210308.2021:getPackages
getPackages :: GHCTools -> [String] -> Either [String] PackageModules
getPackages tools names =
    let either_packages =
            flip map names $ \name -> maybe (Left name) Right (getPackage tools name)
    in case partitionEithers either_packages of
        ([],packages) -> Right . Map.unions $ packages
        (not_found,_) -> Left not_found
-- @-node:gcross.20091121210308.2021:getPackages
-- @+node:gcross.20091121210308.2024:findPackagesExposingModule
findPackagesExposingModule :: GHCTools -> String -> [String]
findPackagesExposingModule tools package_name =
    words
    .
    unsafePerformIO
    .
    readProcess (ghcPackageQueryPath tools) ["--simple-output","find-module",package_name]
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
configurePackageResolutions :: GHCTools -> PackageDescription -> String -> String -> Configurer [String]
configurePackageResolutions tools package_description =
    configureUsingSectionWith config_reader config_writer automatic_configurer
  where
    config_reader = fmap words (getConfig "packages")
    config_writer = setConfig "packages" . unwords
    automatic_configurer Nothing =
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
-- @-node:gcross.20091121210308.2023:Package Queries
-- @+node:gcross.20091121210308.2031:Error reporting
-- @+node:gcross.20091121210308.2032:reportUnknownModules
reportUnknownModules :: GHCTools -> String -> [String] -> ErrorMessage
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
    GHCTools ->
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
                    if Map.member module_name known_package_modules
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
    GHCTools ->
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
    GHCTools ->
    [String] ->
    FilePath ->
    [Resource] ->
    String ->
    FilePath ->
    Resource
ghcLinkProgram
    tools
    options
    cache_directory
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
