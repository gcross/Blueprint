-- @+leo-ver=4-thin
-- @+node:gcross.20091204093401.2246:@thin Simple.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091204093401.2249:<< Language extensions >>
-- @-node:gcross.20091204093401.2249:<< Language extensions >>
-- @nl

module Blueprint.Tools.GHC.Simple where

-- @<< Import needed modules >>
-- @+node:gcross.20091204093401.2252:<< Import needed modules >>
import Data.ErrorMessage
import Data.Digest.Pure.MD5
import qualified Data.Map as Map

import Distribution.Package (PackageIdentifier(..),PackageName(..),Dependency(..))
import qualified Distribution.Package as Package
import qualified Distribution.PackageDescription as PackageDescription
import Distribution.PackageDescription.Configuration
import qualified Distribution.PackageDescription.Parse as Parse
import Distribution.Text
import Distribution.Verbosity

import Blueprint.Configuration
import Blueprint.Main
import Blueprint.Options
import Blueprint.Resources
import Blueprint.Targets
import Blueprint.Tools.GHC
import Blueprint.Tools.GHC.Helpers
import Blueprint.Tools.GHC.Targets
-- @-node:gcross.20091204093401.2252:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091204093401.2260:Values
-- @+node:gcross.20091204093401.2263:ghc_flags
ghc_flags = ["-O","-threaded"]
-- @-node:gcross.20091204093401.2263:ghc_flags
-- @-node:gcross.20091204093401.2260:Values
-- @+node:gcross.20091204093401.2248:Functions
-- @+node:gcross.20091204093401.2247:defaultMain
defaultMain :: SourceDirectorySpecification a => a -> Maybe (a,[String]) -> IO ()
defaultMain source_directory_specification maybe_test_information =
    let source_resources = getSourceResources source_directory_specification
        PackageInformation
            {   packageDescription = package_description
            ,   packageExternalDependencies = package_dependencies
            ,   packageIdentifier = package_identifier
            ,   packageName = package_name
            ,   packageQualifiedName = qualified_package_name
            ,   packageConfigurationFilePath = configuration_filepath
            } = loadInformationFromCabalFile
        configure =
            parseCommandLineOptionsAndThenRunConfigurer
                configuration_filepath
                ghc_options
            $
            makeConfigurer package_dependencies
        build =
            makeBuildLibraryTarget
                configure
                qualified_package_name
                ghc_flags
                source_resources
        install =
            makeInstallTarget
                configure
                build
                package_description
        targets =
            [target "configure" configure
            ,target "build" build
            ,target "install" install
            ,target "reconfigure" $
                makeReconfigureTarget configuration_filepath targets
            ,target "self" $
                makeSelfTarget
                    configure
                    ghc_flags
                    source_resources
            ,target "reself" $
                makeReselfTarget targets
            ,target "rebuild" $
                makeRebuildTarget targets
            ,target "clean" $
                makeCleanTarget
                    ["build"
                    ,"dist"
                    ]
            ,target "distclean" $
                makeDistCleanTarget
                    [configuration_filepath
                    ]
                    targets
            ] ++ case maybe_test_information of
                    Nothing -> []
                    Just (test_directory_specification,additional_test_dependencies) ->
                        [target "test" $
                            makeTestTarget
                                (
                                    parseCommandLineOptionsAndThenRunConfigurer
                                        configuration_filepath
                                        ghc_options
                                    $
                                    makeTestConfigurer package_dependencies additional_test_dependencies
                                )
                                ghc_flags
                                (addResources
                                    (getSourceResources test_directory_specification) 
                                    source_resources
                                )
                        ,target "retest" $
                            makeRetestTarget [programBuildRoot] targets
                        ]
    in Blueprint.Main.defaultMain
        (createDefaultHelpMessage ghc_options . map fst $ targets)
        targets
-- @-node:gcross.20091204093401.2247:defaultMain
-- @-node:gcross.20091204093401.2248:Functions
-- @-others
-- @-node:gcross.20091204093401.2246:@thin Simple.hs
-- @-leo
