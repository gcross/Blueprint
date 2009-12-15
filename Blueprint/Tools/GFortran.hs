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

import Data.ErrorMessage
import Data.Dynamic
import qualified Data.Map as Map

import System.Directory
import System.Exit
import System.FilePath
import System.IO.Unsafe
import System.Process

import Text.PrettyPrint.ANSI.Leijen hiding ((</>),(<$>))

import Blueprint.Configuration
import Blueprint.Cache.ImplicitDependencies
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
-- @+node:gcross.20091214124713.1574:Values
-- @+node:gcross.20091214124713.1575:regular expression
use_matching_regex = compileRegularExpression 1 "\\s*use +([A-Za-z_][A-Za-z0-9_]*)"
-- @-node:gcross.20091214124713.1575:regular expression
-- @-node:gcross.20091214124713.1574:Values
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
-- @+node:gcross.20091214124713.1582:Functions
-- @+node:gcross.20091214124713.1583:doubleUnderscoreToDots
doubleUnderscoreToDots :: String -> String
doubleUnderscoreToDots [] = []
doubleUnderscoreToDots ('_':'_':xs) = '.':doubleUnderscoreToDots xs -- '
doubleUnderscoreToDots (x:xs) = x:doubleUnderscoreToDots xs
-- @-node:gcross.20091214124713.1583:doubleUnderscoreToDots
-- @-node:gcross.20091214124713.1582:Functions
-- @+node:gcross.20091129000542.1566:Tools
-- @+node:gcross.20091129000542.1567:gfortranCompile
gfortranCompile ::
    GFortranConfiguration ->
    FilePath ->
    [String] ->
    FilePath ->
    FilePath ->
    Resources ->
    Resource ->
    [Resource]
gfortranCompile
    configuration
    cache_directory
    options
    object_destination_directory
    interface_destination_directory
    known_resources
    source_resource
    = [object_resource,interface_resource]
  where
    source_filepath = resourceFilePath source_resource
    source_name = resourceName source_resource
    object_filepath = getFilePathForNameAndType object_destination_directory source_name "o"
    interface_filepath = getFilePathForNameAndType interface_destination_directory source_name "hi"

    object_resource = Resource
        {   resourceName = source_name
        ,   resourceType = "o"
        ,   resourceFilePath = object_filepath
        ,   resourceDigest = object_digest
        ,   resourceLinkDependencies = noLinkDependencies
        }
    interface_resource = Resource
        {   resourceName = source_name
        ,   resourceType = "mod"
        ,   resourceFilePath = interface_filepath
        ,   resourceDigest = interface_digest
        ,   resourceLinkDependencies = notLinkable interface_resource
        }

    scanner =
        runScanner
            use_matching_regex
            (\dependency ->
                let resource_name = doubleUnderscoreToDots dependency
                in Just
                    [(BuildDependency,(resource_name,"mod"))
                    ,(LinkDependency,(resource_name,"o"))
                    ]
            )
            (\(_,(resource_name,resource_type)) ->
                if resource_type == "mod"
                    then Just . text $ resource_name
                    else Nothing
            )
            known_resources
            source_filepath

    builder = 
        runProductionCommand
            [object_filepath,interface_filepath]
            ("Error compiling " ++ source_name ++ ":")
            (gfortranCompilerPath configuration)
            (options ++
                ["-J"++interface_destination_directory
                ,"-c",source_filepath
                ,"-o",object_filepath
                ]
            )

    (object_digest:interface_digest:_,link_dependency_resources) =
        analyzeImplicitDependenciesAndRebuildIfNecessary
            builder
            scanner
            known_resources
            (cache_directory </> source_name <.> "o")
            [object_filepath,interface_filepath]
            (unwords options)
            source_resource

-- @-node:gcross.20091129000542.1567:gfortranCompile
-- @+node:gcross.20091129000542.1568:gfortranCompileAdditional
gfortranCompileAdditional ::
    GFortranConfiguration ->
    FilePath ->
    [String] ->
    FilePath ->
    FilePath ->
    Resources ->
    Resources ->
    Resources
gfortranCompileAdditional
    configuration
    cache_directory
    options
    object_destination_directory
    interface_destination_directory
    =
    compileAdditionalWithImplicitDependencies
        ["f","f77","f90","f95"]
        (gfortranCompile
            configuration
            cache_directory
            options
            object_destination_directory
            interface_destination_directory
        )
-- @-node:gcross.20091129000542.1568:gfortranCompileAdditional
-- @+node:gcross.20091214124713.1590:gfortranCompileAll
gfortranCompileAll ::
    GFortranConfiguration ->
    FilePath ->
    [String] ->
    FilePath ->
    FilePath ->
    Resources ->
    Resources
gfortranCompileAll
    configuration
    cache_directory
    options
    object_destination_directory
    interface_destination_directory
    source_resources
    =
    gfortranCompileAdditional
        configuration
        cache_directory
        options
        object_destination_directory
        interface_destination_directory
        source_resources
        Map.empty
-- @-node:gcross.20091214124713.1590:gfortranCompileAll
-- @-node:gcross.20091129000542.1566:Tools
-- @-others
-- @-node:gcross.20091129000542.1552:@thin GFortran.hs
-- @-leo
