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
import Blueprint.Tools
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
    FilePath ->
    [String] ->
    FilePath ->
    FilePath ->
    Resource ->
    Resource
gfortranCompile
    tools
    cache_directory
    options
    object_destination_directory
    interface_destination_directory
    source_resource
    =
    Resource
        {   resourceName = source_name
        ,   resourceType = "o"
        ,   resourceFilePath = object_filepath
        ,   resourceDigest = object_digest
        ,   resourceLinkDependencies = noLinkDependencies
        }
  where
    source_filepath = resourceFilePath source_resource
    source_name = resourceName source_resource
    object_filepath = getFilePathForNameAndType object_destination_directory source_name "o"

    builder = 
        runProductionCommand
            ("compiling " ++ source_name)
            [object_filepath]
            (gfortranCompilerPath tools)
            (options ++
                ["-J"++interface_destination_directory
                ,"-c",source_filepath
                ,"-o",object_filepath
                ]
            )

    object_digest = fmap head $
        analyzeDependencyAndRebuildIfNecessary
            builder
            (cache_directory </> source_name <.> "o")
            [object_filepath]
            (unwords options)
            source_resource
-- @-node:gcross.20091129000542.1567:gfortranCompile
-- @+node:gcross.20091129000542.1568:gfortranCompileAll
gfortranCompileAll ::
    GFortranConfiguration ->
    FilePath ->
    [String] ->
    FilePath ->
    FilePath ->
    Resources ->
    Resources
gfortranCompileAll
    tools
    cache_directory
    options
    object_destination_directory
    interface_destination_directory
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
                            cache_directory
                            options
                            object_destination_directory
                            interface_destination_directory
                            resource
                 in go (addResource object_resource $ accum_resources) rest_resources
            else go accum_resources rest_resources

-- @-node:gcross.20091129000542.1568:gfortranCompileAll
-- @-node:gcross.20091129000542.1566:Tools
-- @-others
-- @-node:gcross.20091129000542.1552:@thin GFortran.hs
-- @-leo
