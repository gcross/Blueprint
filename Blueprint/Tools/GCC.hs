-- @+leo-ver=4-thin
-- @+node:gcross.20091123114318.1335:@thin GCC.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091123114318.1337:<< Language extensions >>
-- @-node:gcross.20091123114318.1337:<< Language extensions >>
-- @nl

module Blueprint.Tools.GCC where

-- @<< Import needed modules >>
-- @+node:gcross.20091123114318.1339:<< Import needed modules >>
import Control.Applicative.Infix
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import System.Directory
import System.Exit
import System.FilePath
import System.IO.Unsafe
import System.Process

import Text.PrettyPrint.ANSI.Leijen hiding ((</>))

import Blueprint.Cache.ExplicitDependencies
import Blueprint.Configuration
import Blueprint.Error
import Blueprint.Options
import Blueprint.Resources
-- @-node:gcross.20091123114318.1339:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091129000542.1577:Keys
gccOptionSectionKey = makeOptionSectionKey "GCC"
gccConfigurationKey = makeConfigurationKey "path to gcc"
-- @-node:gcross.20091129000542.1577:Keys
-- @+node:gcross.20091123114318.1342:Types
-- @+node:gcross.20091123114318.1343:GCCConfiguration
data GCCConfiguration = GCCConfiguration { gccCompilerPath :: FilePath } deriving (Show)
-- @-node:gcross.20091123114318.1343:GCCConfiguration
-- @-node:gcross.20091123114318.1342:Types
-- @+node:gcross.20091128000856.1436:Instances
-- @+node:gcross.20091128000856.1437:ConfigurationData GCCConfiguration
instance ConfigurationData GCCConfiguration where
    readConfig  = simpleReadConfig  gccConfigurationKey GCCConfiguration 
    writeConfig = simpleWriteConfig gccConfigurationKey gccCompilerPath
-- @-node:gcross.20091128000856.1437:ConfigurationData GCCConfiguration
-- @+node:gcross.20091128000856.1438:AutomaticallyConfigurable GCCConfiguration
instance AutomaticallyConfigurable GCCConfiguration where
    automaticallyConfigure = simpleSearchForProgram gccOptionSectionKey GCCConfiguration "gcc"
-- @-node:gcross.20091128000856.1438:AutomaticallyConfigurable GCCConfiguration
-- @-node:gcross.20091128000856.1436:Instances
-- @+node:gcross.20091129000542.1585:Options processing
gccOptions = makeSimpleOptionSectionForProgram "gcc" gccOptionSectionKey
-- @-node:gcross.20091129000542.1585:Options processing
-- @+node:gcross.20091123114318.1372:Tools
-- @+node:gcross.20091123114318.1368:gccCompile
gccCompile ::
    GCCConfiguration ->
    [String] ->
    FilePath ->
    FilePath ->
    Resource ->
    Resource
gccCompile
    tools
    options
    object_destination_directory
    cache_directory
    source_resource
    =
    Resource
        {   resourceName = source_name
        ,   resourceType = "o"
        ,   resourceFilePath = object_filepath
        ,   resourceDigest = object_digest
        ,   resourceDependencies = [resourceId source_resource]
        }
  where
    source_filepath = resourceFilePath source_resource
    source_name = resourceName source_resource
    object_filepath = getFilePathForNameAndType object_destination_directory source_name "o"

    builder =
        let arguments = 
                options ++
                ["-c",source_filepath
                ,"-o",object_filepath
                ]
            path_to_gcc = gccCompilerPath tools
        in do
            createDirectoryIfMissing True . takeDirectory $ object_filepath
            putStrLn . unwords $ (path_to_gcc:arguments)
            compilation_result <- readProcessWithExitCode path_to_gcc arguments ""
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

    object_digest =
        case analyzeDependencyAndRebuildIfNecessary
                builder
                (cache_directory </> source_name <.> "o")
                [object_filepath]
                (unwords options)
                source_resource
        of Left error_message -> Left error_message
           Right [object_digest] -> Right object_digest
           x -> error $ "Programmer error:  Builder returned the wrong number of digests! (" ++ show x ++ ")"
-- @-node:gcross.20091123114318.1368:gccCompile
-- @+node:gcross.20091123114318.1371:gccCompileAll
gccCompileAll ::
    GCCConfiguration ->
    [String] ->
    FilePath ->
    FilePath ->
    Resources ->
    Resources
gccCompileAll
    tools
    options
    object_destination_directory
    cache_directory
    old_resources
    =
    new_resources
  where
    new_resources = go old_resources (Map.elems old_resources)
    go accum_resources [] = accum_resources
    go accum_resources (resource:rest_resources) =
        if resourceType (resource) `elem` ["c","cc","cpp","C"]
            then let object_resource =
                        gccCompile
                            tools
                            options
                            object_destination_directory
                            cache_directory
                            resource
                 in go (addResource object_resource accum_resources) rest_resources
            else go accum_resources rest_resources
-- @-node:gcross.20091123114318.1371:gccCompileAll
-- @-node:gcross.20091123114318.1372:Tools
-- @-others
-- @-node:gcross.20091123114318.1335:@thin GCC.hs
-- @-leo
