-- @+leo-ver=4-thin
-- @+node:gcross.20091121204836.1242:@thin GHC.hs
-- @@language Haskell

module Blueprint.Tools.GHC where

-- @<< Import needed modules >>
-- @+node:gcross.20091121210308.1269:<< Import needed modules >>
import Control.Arrow

import Data.Array
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Either
import Data.Either.Unwrap
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import System.Directory
import System.Exit
import System.FilePath
import System.IO.Unsafe
import System.Process

import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString.Lazy

import Blueprint.Resources
-- @-node:gcross.20091121210308.1269:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091121210308.1270:Types
-- @+node:gcross.20091121210308.1271:GHCTools
data GHCTools = GHCTools
    {   ghcVersion :: [Int]
    ,   ghcCompilerPath :: String
    ,   ghcPackageQueryPath :: String
    } deriving (Show)
-- @-node:gcross.20091121210308.1271:GHCTools
-- @+node:gcross.20091121210308.2029:GHCOptions
data GHCOptions = GHCOptions
    {   ghcOptionPackages :: [String]
    ,   ghcOptionIncludeDirectories :: [FilePath]
    }

defaultOptions = GHCOptions [] []
-- @-node:gcross.20091121210308.2029:GHCOptions
-- @+node:gcross.20091121210308.2025:PackageModules
type PackageModules = Map String String
-- @-node:gcross.20091121210308.2025:PackageModules
-- @-node:gcross.20091121210308.1270:Types
-- @+node:gcross.20091121210308.2014:Values
-- @+node:gcross.20091121210308.2015:regular expressions
import_matching_regex = fromRight . compile defaultCompOpt defaultExecOpt . L8.pack $ "\\s*import +(qualified +)?([A-Z][A-Za-z0-9_.]+)[\\s;]?"
-- @-node:gcross.20091121210308.2015:regular expressions
-- @-node:gcross.20091121210308.2014:Values
-- @+node:gcross.20091121210308.2016:Functions
-- @+node:gcross.20091121210308.2017:dependenciesOf
dependenciesOf :: FilePath -> [String]
dependenciesOf =
    map (L8.unpack . fst . (! 2))
    .
    matchAllText import_matching_regex
    .
    unsafePerformIO
    .
    L.readFile
-- @-node:gcross.20091121210308.2017:dependenciesOf
-- @+node:gcross.20091121210308.2030:optionsToArguments
optionsToArguments :: GHCOptions -> [String]
optionsToArguments =
    concat
    .
    flip map
        [("-package":) . intersperse "-package" . ghcOptionPackages
        ,map ("-i" ++) . ghcOptionIncludeDirectories
        ]
    .
    flip ($)
-- @-node:gcross.20091121210308.2030:optionsToArguments
-- @-node:gcross.20091121210308.2016:Functions
-- @+node:gcross.20091121210308.2023:Package Queries
-- @+node:gcross.20091121210308.2018:modulesExposedBy
modulesExposedBy :: GHCTools -> String -> Maybe [String]
modulesExposedBy tools package_name =
    case unsafePerformIO $
            readProcessWithExitCode (ghcPackageQueryPath tools) ["field",package_name,"exposed-modules"] ""
    of (ExitSuccess,response,_) -> Just . filter (/= "exposed-modules:") . words $ response 
       _ -> Nothing
-- @-node:gcross.20091121210308.2018:modulesExposedBy
-- @+node:gcross.20091121210308.2019:getPackage
getPackage :: GHCTools -> String -> Maybe (Map String String)
getPackage tools name = fmap (Map.fromList . map (flip (,) name)) $ modulesExposedBy tools name
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
-- @-node:gcross.20091121210308.2023:Package Queries
-- @+node:gcross.20091121210308.1273:Configuration
-- @+node:gcross.20091121210308.1274:ghcTools
ghcTools :: Maybe GHCTools
ghcTools = unsafePerformIO $ do
    maybe_path_to_ghc <- findExecutable "ghc"
    case maybe_path_to_ghc of
        Nothing -> return Nothing
        Just path_to_ghc -> do
            version_as_string <- readProcess path_to_ghc ["--numeric-version"] ""
            return . Just $
                GHCTools
                    {   ghcVersion = map read . splitDot $ version_as_string
                    ,   ghcCompilerPath = path_to_ghc
                    ,   ghcPackageQueryPath = path_to_ghc ++ "-pkg"
                    }
-- @-node:gcross.20091121210308.1274:ghcTools
-- @-node:gcross.20091121210308.1273:Configuration
-- @+node:gcross.20091121210308.2031:Error reporting
-- @+node:gcross.20091121210308.2032:reportUnknownModules
reportUnknownModules :: GHCTools -> String -> [String] -> Map String String
reportUnknownModules tools source_filepath =
    Map.singleton source_filepath
    .
    unlines
    .
    map (\module_name ->
        case findPackagesExposingModule tools module_name of
            [] -> "\t" ++ module_name ++ " (no idea where to find it)"
            packages -> "\t" ++ module_name ++ " which appears in packages " ++ (show packages)
    )
-- @-node:gcross.20091121210308.2032:reportUnknownModules
-- @-node:gcross.20091121210308.2031:Error reporting
-- @+node:gcross.20091121210308.1275:Tools
-- @+node:gcross.20091121210308.2022:ghcCompile
ghcCompile :: GHCTools -> GHCOptions -> PackageModules -> Resources -> FilePath -> FilePath -> Resource -> (Resource,Resource)
ghcCompile
    tools
    options
    known_package_modules
    known_resources
    object_destination_directory
    interface_destination_directory
    source
    =
    let source_filepath = resourceFilePath source
        resource_name = resourceName source
        object_filepath = getFilePathForNameAndType object_destination_directory resource_name "o"
        object_resource =
            Resource
                {   resourceName = resource_name
                ,   resourceType = "o"
                ,   resourceFilePath = object_filepath
                ,   resourceDigest = fst object_and_interface_digests
                }
        interface_filepath = getFilePathForNameAndType interface_destination_directory resource_name "hi"
        interface_resource =
            Resource
                {   resourceName = resource_name
                ,   resourceType = "hi"
                ,   resourceFilePath = interface_filepath
                ,   resourceDigest = snd object_and_interface_digests
                }
        required_modules =  source_filepath
        (unknown_dependencies,(package_dependencies,resource_dependencies)) =
            (second partitionEithers)
            .
            partitionEithers
            .
            map (\module_name ->
                case (Map.lookup (module_name,"hi") known_resources, Map.lookup module_name known_package_modules) of
                    (Just resource,_) -> Right (Right resource)
                    (_,Just package_name) -> Right (Left package_name)
                    (Nothing,Nothing) -> Left module_name
            )
            .
            dependenciesOf
            $
            source_filepath
        object_and_interface_digests =
            if not . null $ unknown_dependencies
                then let error_message = reportUnknownModules tools source_filepath unknown_dependencies
                        in (Left error_message, Left error_message)
                else
                    let option_arguments = optionsToArguments $ options
                            {   ghcOptionPackages =
                                    (package_dependencies ++)
                                    .
                                    ghcOptionPackages
                                    $
                                    options
                            ,   ghcOptionIncludeDirectories =
                                    (interface_destination_directory:)
                                    .
                                    ghcOptionIncludeDirectories
                                    $
                                    options
                            }
                        arguments = 
                            option_arguments ++
                            ["-c",source_filepath
                            ,"-o",object_filepath
                            ,"-ohi",interface_filepath
                            ]
                        compilation_result = unsafePerformIO $ do
                            let path_to_ghc = ghcCompilerPath tools
                            createDirectoryIfMissing True . takeDirectory $ object_filepath
                            createDirectoryIfMissing True . takeDirectory $ interface_filepath
                            putStrLn . unwords $ (path_to_ghc:arguments)
                            readProcessWithExitCode path_to_ghc arguments ""
                        dependency_errors =
                            catMaybes
                            .
                            (map $ \dependency_resource ->
                                case resourceDigest dependency_resource of
                                    Left errors -> Just errors
                                    Right _ -> Nothing
                            )
                            $
                            resource_dependencies

                    in if (not . null) dependency_errors
                        then let my_errors = Map.unions dependency_errors in (Left my_errors,Left my_errors)  
                        else case compilation_result of
                                (ExitFailure _,_,error_message) ->
                                    let my_error = Map.singleton source_filepath error_message
                                    in (Left my_error,Left my_error)
                                (ExitSuccess,_,_) ->
                                    (Right (digestOf object_filepath), Right (digestOf interface_filepath))

    in (object_resource,interface_resource)
-- @-node:gcross.20091121210308.2022:ghcCompile
-- @+node:gcross.20091121210308.2038:ghcCompileAll
ghcCompileAll ::
    GHCTools ->
    GHCOptions ->
    PackageModules ->
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
                                resource
                     in go (addResource object_resource . addResource interface_resource $ accum_resources) rest_resources
                else go accum_resources rest_resources
    in new_resources
-- @-node:gcross.20091121210308.2038:ghcCompileAll
-- @-node:gcross.20091121210308.1275:Tools
-- @-others
-- @-node:gcross.20091121204836.1242:@thin GHC.hs
-- @-leo
