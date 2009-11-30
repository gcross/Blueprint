-- @+leo-ver=4-thin
-- @+node:gcross.20091129000542.1642:@thin Installer.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091129000542.1643:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
-- @-node:gcross.20091129000542.1643:<< Language extensions >>
-- @nl

module Blueprint.Tools.Installer where

-- @<< Import needed modules >>
-- @+node:gcross.20091129000542.1644:<< Import needed modules >>
import Control.Applicative
import Control.Monad

import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable

import System.FilePath

import Text.PrettyPrint.ANSI.Leijen hiding ((</>))

import Blueprint.Error
import Blueprint.Options
import Blueprint.Configuration
-- @-node:gcross.20091129000542.1644:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091129000542.1645:Keys
installerOptionSectionKey = makeOptionSectionKey "Installer"

installerLibraryPathKey = makeConfigurationKey "destination for libraries"
-- @-node:gcross.20091129000542.1645:Keys
-- @+node:gcross.20091129000542.1646:Types
-- @+node:gcross.20091129000542.1647:InstallerConfiguration
data InstallerConfiguration = InstallerConfiguration
    {   installerLibraryPath :: FilePath
    } deriving (Show)
-- @-node:gcross.20091129000542.1647:InstallerConfiguration
-- @+node:gcross.20091129000542.1648:InstallerOptions
data InstallerOptions = InstallerOptions
    {   installerOptionPrefix :: Maybe FilePath
    ,   installerOptionLibraryPath :: Maybe FilePath
    } deriving (Typeable, Show)

-- @-node:gcross.20091129000542.1648:InstallerOptions
-- @-node:gcross.20091129000542.1646:Types
-- @+node:gcross.20091129000542.1651:Instances
-- @+node:gcross.20091129000542.1652:ConfigurationData InstallerConfiguration
instance ConfigurationData InstallerConfiguration where
    readConfig =
        liftM InstallerConfiguration
            (getConfig installerLibraryPathKey)
    writeConfig =
        (setConfig installerLibraryPathKey . installerLibraryPath)
-- @-node:gcross.20091129000542.1652:ConfigurationData InstallerConfiguration
-- @+node:gcross.20091129000542.1700:AutomaticallyConfigurable InstallerConfiguration
instance AutomaticallyConfigurable InstallerConfiguration where
    automaticallyConfigure parsed_options =
        case lookupAndUnwrapOptionSection installerOptionSectionKey parsed_options of
            Nothing -> configureUsingPrefixOnly "/usr/local"
            Just (InstallerOptions maybe_prefix maybe_libdir) ->
                case (maybe_prefix,maybe_libdir) of
                    (Nothing,Nothing) -> configureUsingPrefixOnly "/usr/local"
                    (Just prefix,Nothing) -> configureUsingPrefixOnly "/usr/prefix"
                    (_,Just libdir) -> Right $ InstallerConfiguration libdir
      where
        configureUsingPrefixOnly prefix = Right $ InstallerConfiguration (prefix </> "lib")
-- @-node:gcross.20091129000542.1700:AutomaticallyConfigurable InstallerConfiguration
-- @-node:gcross.20091129000542.1651:Instances
-- @+node:gcross.20091129000542.1666:Options processing
installerOptions =
    OptionSection
    {   optionSectionKey = installerOptionSectionKey
    ,   optionSectionOptions =
        [   Option "prefix"
                [] ["prefix"]
                (ArgumentRequired "PREFIX")
                "install files in PREFIX [/usr/local]"
        ,   Option "libraries"
                [] ["libdir"]
                (ArgumentRequired "DIRECTORY")
                "install libraries in DIRECTORY [PREFIX/lib]"
        ]
    ,   optionSectionPostprocessor = postprocessOptions
    }
  where
    postprocessOptions :: Map String [Maybe String] -> Either Doc Dynamic
    postprocessOptions option_map = fmap toDyn $
        liftA2 InstallerOptions
            (lookupOptionAndVerifyDirectoryExists "prefix" option_map)
            (lookupOptionAndVerifyDirectoryExists "libraries" option_map)
-- @-node:gcross.20091129000542.1666:Options processing
-- @-others
-- @-node:gcross.20091129000542.1642:@thin Installer.hs
-- @-leo
