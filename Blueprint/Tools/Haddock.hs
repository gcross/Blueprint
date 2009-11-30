-- @+leo-ver=4-thin
-- @+node:gcross.20091128000856.1464:@thin Haddock.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091128000856.1465:<< Language extensions >>
-- @-node:gcross.20091128000856.1465:<< Language extensions >>
-- @nl

module Blueprint.Tools.Haddock where

-- @<< Import needed modules >>
-- @+node:gcross.20091128000856.1466:<< Import needed modules >>
import Control.Monad

import Data.Maybe

import System.Directory
import System.Exit
import System.FilePath
import System.IO.Unsafe
import System.Process

import Blueprint.Configuration
import Blueprint.Cache.ExplicitDependencies
import Blueprint.Error
import Blueprint.Resources
import Blueprint.Options
import Blueprint.Tools.GHC
-- @-node:gcross.20091128000856.1466:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091129000542.1581:Keys
haddockOptionSectionKey = makeOptionSectionKey "Haddock"
haddockConfigurationKey = makeConfigurationKey "path to hadhock"
-- @-node:gcross.20091129000542.1581:Keys
-- @+node:gcross.20091128000856.1467:Types
-- @+node:gcross.20091128000856.1468:HaddockConfiguration
data HaddockConfiguration = HaddockConfiguration { haddockPath :: FilePath } deriving (Show)
-- @-node:gcross.20091128000856.1468:HaddockConfiguration
-- @-node:gcross.20091128000856.1467:Types
-- @+node:gcross.20091128000856.1469:Instances
-- @+node:gcross.20091128000856.1470:ConfigurationData HaddockConfiguration
instance ConfigurationData HaddockConfiguration where
    readConfig  = simpleReadConfig  haddockConfigurationKey HaddockConfiguration
    writeConfig = simpleWriteConfig haddockConfigurationKey haddockPath
-- @-node:gcross.20091128000856.1470:ConfigurationData HaddockConfiguration
-- @+node:gcross.20091128000856.1471:AutomaticallyConfigurable HaddockConfiguration
instance AutomaticallyConfigurable HaddockConfiguration where
    automaticallyConfigure = simpleSearchForProgram haddockOptionSectionKey HaddockConfiguration "haddock"
-- @-node:gcross.20091128000856.1471:AutomaticallyConfigurable HaddockConfiguration
-- @-node:gcross.20091128000856.1469:Instances
-- @+node:gcross.20091129000542.1510:Options processing
haddockOptions = makeSimpleOptionSectionForProgram "haddock" haddockOptionSectionKey
-- @-node:gcross.20091129000542.1510:Options processing
-- @+node:gcross.20091128000856.1472:Tools
-- @+node:gcross.20091128000856.1473:createDocumentation
createDocumentation ::
    HaddockConfiguration ->
    GHCConfiguration ->
    [String] ->
    [String] ->
    FilePath ->
    [Resource] ->
    FilePath ->
    Resource
createDocumentation _ _ _ _ _ [] _ = error "No source resources specified for the documentation!"
createDocumentation
    tools
    ghc_tools
    options
    package_names
    cache_directory
    source_resources
    documentation_destination
    = Resource
        {   resourceName = "interface"
        ,   resourceType = "haddock"
        ,   resourceFilePath = interface_filepath
        ,   resourceDigest = interface_digest
        ,   resourceDependencies = map resourceId source_resources
        }
  where
    interface_filepath = documentation_destination </> "interface.haddock"

    interface_digest = either Left (Right . head) $
        analyzeExplicitDependenciesAndRebuildIfNecessary
            builder
            (cache_directory </> "interface.haddock")
            [interface_filepath]
            ()
            source_resources

    builder = do
        createDirectoryIfMissing True . takeDirectory $ interface_filepath
        let arguments =
                ["--odir="++documentation_destination
                ,"--dump-interface="++interface_filepath
                ,"--html"
                ]
                ++ (map ("--read-interface="++)
                    .
                    filter (unsafePerformIO . doesFileExist)
                    .
                    concat
                    .
                    catMaybes
                    .
                    map (queryPackage ghc_tools "haddock-interfaces")
                    $
                    package_names
                   )
                ++ options
                ++ map resourceFilePath source_resources
            command = (haddockPath tools)
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
                errorMessageTextWithLines ("creating documentation")
                $
                error_message
            (ExitSuccess,_,_) ->
                doesFileExist interface_filepath
                >>= \interface_file_exists -> return $
                    if interface_file_exists
                        then Nothing
                        else Just . errorMessageText "creating documentaton" $
                            "For unknown reasons, the interface file " ++ show interface_filepath ++ " was not created."
-- @-node:gcross.20091128000856.1473:createDocumentation
-- @-node:gcross.20091128000856.1472:Tools
-- @-others
-- @-node:gcross.20091128000856.1464:@thin Haddock.hs
-- @-leo
