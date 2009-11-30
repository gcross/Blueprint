-- @+leo-ver=4-thin
-- @+node:gcross.20091129000542.1552:@thin GFortran.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091129000542.1553:<< Language extensions >>
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
-- @nonl
-- @-node:gcross.20091129000542.1553:<< Language extensions >>
-- @nl

module Blueprint.Tools.GFortran where

-- @<< Import needed modules >>
-- @+node:gcross.20091129000542.1554:<< Import needed modules >>
import Control.Monad

import Data.Dynamic
import qualified Data.Map as Map

import System.Directory
import System.Exit
import System.FilePath
import System.IO.Unsafe
import System.Process

import Blueprint.Configuration
import Blueprint.Cache.ExplicitDependencies
import Blueprint.Error
import Blueprint.Miscellaneous
import Blueprint.Options
import Blueprint.Resources
-- @-node:gcross.20091129000542.1554:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091129000542.1579:Keys
gfortranOptionSectionKey = makeOptionSectionKey "gfortran"
gfortranConfigurationKey = makeConfigurationKey "path to gfortran"
-- @-node:gcross.20091129000542.1579:Keys
-- @+node:gcross.20091129000542.1555:Types
-- @+node:gcross.20091129000542.1556:GFortranConfiguration
data GFortranConfiguration = GFortranConfiguration { gfortranCompilerPath :: FilePath } deriving (Show)
-- @-node:gcross.20091129000542.1556:GFortranConfiguration
-- @-node:gcross.20091129000542.1555:Types
-- @+node:gcross.20091129000542.1557:Instances
-- @+node:gcross.20091129000542.1558:ConfigurationData GFortranTools
instance ConfigurationData GFortranConfiguration where
    readConfig  = simpleReadConfig  gfortranConfigurationKey GFortranConfiguration 
    writeConfig = simpleWriteConfig gfortranConfigurationKey gfortranCompilerPath
-- @-node:gcross.20091129000542.1558:ConfigurationData GFortranTools
-- @+node:gcross.20091129000542.1559:AutomaticallyConfigurable GFortranTools
instance AutomaticallyConfigurable GFortranConfiguration where
    automaticallyConfigure = simpleSearchForProgram gfortranOptionSectionKey GFortranConfiguration "gfortran"
-- @-node:gcross.20091129000542.1559:AutomaticallyConfigurable GFortranTools
-- @-node:gcross.20091129000542.1557:Instances
-- @+node:gcross.20091129000542.1560:Options processing
gfortranOptions = makeSimpleOptionSectionForProgram "gfortran" gfortranOptionSectionKey
-- @-node:gcross.20091129000542.1560:Options processing
-- @+node:gcross.20091129000542.1566:Tools
-- @+node:gcross.20091129000542.1567:gfortranCompile
gfortranCompile ::
    GFortranConfiguration ->
    [String] ->
    FilePath ->
    FilePath ->
    FilePath ->
    Resource ->
    Resource
gfortranCompile
    tools
    options
    object_destination_directory
    interface_destination_directory
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
                ["-J"++interface_destination_directory
                ,"-c",source_filepath
                ,"-o",object_filepath
                ]
            path_to_gfortran = gfortranCompilerPath tools
        in do
            createDirectoryIfMissing True . takeDirectory $ interface_destination_directory
            createDirectoryIfMissing True . takeDirectory $ object_filepath
            putStrLn . unwords $ (path_to_gfortran:arguments)
            compilation_result <- readProcessWithExitCode path_to_gfortran arguments ""
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
-- @-node:gcross.20091129000542.1567:gfortranCompile
-- @+node:gcross.20091129000542.1568:gfortranCompileAll
gfortranCompileAll ::
    GFortranConfiguration ->
    [String] ->
    FilePath ->
    FilePath ->
    FilePath ->
    Resources ->
    Resources
gfortranCompileAll
    tools
    options
    object_destination_directory
    interface_destination_directory
    cache_directory
    old_resources
    =
    new_resources
  where
    new_resources = go old_resources (Map.elems old_resources)
    go accum_resources [] = accum_resources
    go accum_resources (resource:rest_resources) =
        if resourceType (resource) `elem` ["f","f77","f90","f95"]
            then let object_resource =
                        gfortranCompile
                            tools
                            options
                            object_destination_directory
                            interface_destination_directory
                            cache_directory
                            resource
                 in go (addResource object_resource $ accum_resources) rest_resources
            else go accum_resources rest_resources

-- @-node:gcross.20091129000542.1568:gfortranCompileAll
-- @-node:gcross.20091129000542.1566:Tools
-- @-others
-- @-node:gcross.20091129000542.1552:@thin GFortran.hs
-- @-leo
