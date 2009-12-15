-- @+leo-ver=4-thin
-- @+node:gcross.20091130051619.1559:@thin Ld.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091129000542.1729:<< Language extensions >>
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
-- @nonl
-- @-node:gcross.20091129000542.1729:<< Language extensions >>
-- @nl

module Blueprint.Tools.Ld where

-- @<< Import needed modules >>
-- @+node:gcross.20091129000542.1730:<< Import needed modules >>
import Control.Monad

import Data.Dynamic
import Data.ErrorMessage

import System.Directory
import System.Exit
import System.FilePath
import System.IO.Unsafe
import System.Process

import Blueprint.Configuration
import Blueprint.Cache.ExplicitDependencies
import Blueprint.Miscellaneous
import Blueprint.Options
import Blueprint.Resources
import Blueprint.Tools
-- @-node:gcross.20091129000542.1730:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091129000542.1731:Keys
ldOptionSectionKey = makeOptionSectionKey "ld"
ldConfigurationKey = makeConfigurationKey "path to ld"
-- @-node:gcross.20091129000542.1731:Keys
-- @+node:gcross.20091129000542.1732:Types
-- @+node:gcross.20091129000542.1733:ArConfiguration
data LdConfiguration = LdConfiguration { ldPath :: FilePath } deriving (Show)
-- @-node:gcross.20091129000542.1733:ArConfiguration
-- @-node:gcross.20091129000542.1732:Types
-- @+node:gcross.20091129000542.1734:Instances
-- @+node:gcross.20091129000542.1735:ConfigurationData LdConfiguration
instance ConfigurationData LdConfiguration where
    readConfig  = simpleReadConfig  ldConfigurationKey LdConfiguration 
    writeConfig = simpleWriteConfig ldConfigurationKey ldPath
-- @-node:gcross.20091129000542.1735:ConfigurationData LdConfiguration
-- @+node:gcross.20091129000542.1736:AutomaticallyConfigurable LdConfiguration
instance AutomaticallyConfigurable LdConfiguration where
    automaticallyConfigure = simpleSearchForProgram ldOptionSectionKey LdConfiguration "ld"
-- @-node:gcross.20091129000542.1736:AutomaticallyConfigurable LdConfiguration
-- @-node:gcross.20091129000542.1734:Instances
-- @+node:gcross.20091129000542.1737:Options processing
ldOptions = makeSimpleOptionSectionForProgram "ld" ldOptionSectionKey
-- @-node:gcross.20091129000542.1737:Options processing
-- @+node:gcross.20091129000542.1738:Tools
-- @+node:gcross.20091129000542.1739:linkIntoObject
linkIntoObject ::
    LdConfiguration ->
    FilePath ->
    [Resource] ->
    String ->
    FilePath ->
    Resource
linkIntoObject
    tools
    cache_directory
    object_resources
    library_resource_name
    library_resource_filepath
    = Resource
        {   resourceName = library_resource_name
        ,   resourceType = "o"
        ,   resourceFilePath = library_resource_filepath
        ,   resourceDigest = library_digest
        ,   resourceLinkDependencies = noLinkDependencies
        }
  where
    ([library_digest],[]) =
        analyzeExplicitDependenciesAndRebuildIfNecessary
            builder
            (cache_directory </> library_resource_name <.> "a")
            [library_resource_filepath]
            []
            ()
            object_resources

    builder =
        runProductionCommand
            ("Error linking object " ++ library_resource_name ++ ":")
            [library_resource_filepath]
            []
            (ldPath tools)
            ("-x":"-r":"-o":library_resource_filepath:map resourceFilePath object_resources)
-- @-node:gcross.20091129000542.1739:linkIntoObject
-- @-node:gcross.20091129000542.1738:Tools
-- @-others
-- @-node:gcross.20091130051619.1559:@thin Ld.hs
-- @-leo
