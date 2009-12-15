-- @+leo-ver=4-thin
-- @+node:gcross.20091214124713.1714:@thin Main.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091214124713.1715:<< Language extensions >>
-- @-node:gcross.20091214124713.1715:<< Language extensions >>
-- @nl

module Blueprint.Tools.GHC.Main where

-- @<< Import needed modules >>
-- @+node:gcross.20091214124713.1716:<< Import needed modules >>
import Control.Applicative
import Control.Monad

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
import qualified Blueprint.Main
import Blueprint.Options
import Blueprint.Resources
import Blueprint.Targets
import Blueprint.Tools.GHC
import Blueprint.Tools.GHC.Helpers
-- @-node:gcross.20091214124713.1716:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091214124713.1719:Functions
-- @+node:gcross.20091214124713.1720:defaultMain
defaultMain ::
    SourceDirectorySpecification a =>
    Configurer c ->
    (Configuration -> c -> FilePath -> FilePath -> FilePath -> FilePath -> Resources -> Resources) ->
    [OptionSection] ->
    a ->
    Maybe (a,[String]) ->
    [String] ->
    IO ()
defaultMain
    configureAdditional
    compileAdditional
    additional_options
    source_directory_specification
    maybe_test_information
    ghc_flags
    =
    let source_resources = getSourceResources source_directory_specification
        PackageInformation
            {   packageDescription = package_description
            ,   packageExternalDependencies = package_dependencies
            ,   packageIdentifier = package_identifier
            ,   packageName = package_name
            ,   packageQualifiedName = qualified_package_name
            ,   packageConfigurationFilePath = configuration_filepath
            } = loadInformationFromCabalFile
        doConfigure configurer =
            parseCommandLineOptionsAndThenRunConfigurer
                configuration_filepath
                (additional_options ++ ghc_options)
            $ liftA2 (,)
                configurer
                configureAdditional
        configure = doConfigure (makeConfigurer package_dependencies)
        buildUsing builder build_root resources (configuration,additional_configuration) =
            builder
                configuration
                ghc_flags
            .
            compileAdditional
                configuration
                additional_configuration
                build_root
                (digestCacheSubdirectory build_root)
                (objectSubdirectory build_root)
                (interfaceSubdirectory build_root)
            $
            resources
        build =
            configure
            >>=
            buildUsing
                (buildAndLinkLibrary qualified_package_name)
                libraryBuildRoot
                source_resources

        targets =
            [target "configure" configure
            ,target "reconfigure" $
                makeReconfigureTarget configuration_filepath targets
            ,target "build" build
            ,target "rebuild" $
                makeRebuildTarget targets
            ,target "install" $
                (liftA2 (,)
                    (fmap fst configure)
                    build
                )
                >>=
                uncurry (installLibrary package_description)
            ,target "self" $
                configure
                >>=
                buildUsing
                    (buildProgram "Setup")
                    libraryBuildRoot
                    (addSourceResourceFor "Setup.hs" source_resources)                                            
            ,target "reself" $
                makeRemoveFilesAndDirectoriesTarget [programBuildRoot]
                `thenTarget`
                lookupOldTarget "self" "reself" targets
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
                            doConfigure (makeTestConfigurer package_dependencies additional_test_dependencies)
                            >>=
                            makeRunTestTarget
                            .
                            (buildUsing
                                (buildProgram "test")
                                programBuildRoot
                                .
                                addResources (getSourceResources test_directory_specification)
                                $
                                source_resources
                            )                            
                        ,target "retest" $
                            makeRetestTarget [programBuildRoot] targets
                        ]
    in Blueprint.Main.defaultMain
        (createDefaultHelpMessage ghc_options . map fst $ targets)
        targets
-- @-node:gcross.20091214124713.1720:defaultMain
-- @+node:gcross.20091214215701.1629:simpleDefaultMain
simpleDefaultMain ::
    SourceDirectorySpecification a =>
    a ->
    Maybe (a,[String]) ->
    [String] ->
    IO ()
simpleDefaultMain =
    defaultMain
        (return ())
        (\_ _ _ _ _ _ -> id)
        []

-- @-node:gcross.20091214215701.1629:simpleDefaultMain
-- @-node:gcross.20091214124713.1719:Functions
-- @-others
-- @-node:gcross.20091214124713.1714:@thin Main.hs
-- @-leo
