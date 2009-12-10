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
import Control.Applicative
import Control.Applicative.Infix
import Control.Monad
import Control.Parallel

import Data.ErrorMessage
import Data.Digest.Pure.MD5
import qualified Data.Map as Map

import Distribution.Package
import qualified Distribution.PackageDescription as Package
import Distribution.PackageDescription.Configuration
import qualified Distribution.PackageDescription.Parse as Parse
import Distribution.Text
import Distribution.Verbosity

import System.Exit
import System.FilePath
import System.FilePath.Find
import System.IO.Unsafe
import System.Process

import Blueprint.Configuration
import Blueprint.Main
import Blueprint.Options
import Blueprint.Resources
import Blueprint.Tools.Ar
import Blueprint.Tools.GHC
import Blueprint.Tools.Installer
import Blueprint.Tools.Ld
-- @-node:gcross.20091204093401.2252:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091204093401.2260:Values
-- @+node:gcross.20091204093401.2261:options
options =
    [   installerOptions
    ,   arOptions
    ,   ldOptions
    ,   ghcOptions
    ]
-- @-node:gcross.20091204093401.2261:options
-- @+node:gcross.20091204093401.2263:flags
ghc_flags = ["-O","-threaded"]
-- @-node:gcross.20091204093401.2263:flags
-- @-node:gcross.20091204093401.2260:Values
-- @+node:gcross.20091204093401.2266:Types
-- @+node:gcross.20091204093401.2267:Configuration
data Configuration = Configuration
    {   ghcConfiguration :: GHCConfiguration
    ,   arConfiguration :: ArConfiguration
    ,   ldConfiguration :: LdConfiguration
    ,   installerConfiguration :: InstallerConfiguration
    ,   packageDependencies :: [String]
    ,   packageModules :: PackageModules
    }
-- @-node:gcross.20091204093401.2267:Configuration
-- @-node:gcross.20091204093401.2266:Types
-- @+node:gcross.20091204093401.2764:Targets
-- @+node:gcross.20091204093401.2766:doConfigure
doConfigure :: String -> String -> FilePath -> [Dependency] -> ErrorMessageOr Configuration
doConfigure package_resolution_section package_cache_section configuration_filepath package_dependencies =
    parseCommandLineOptions options
    >>=
    \(_,options) -> runConfigurer configuration_filepath options $ do
    configurations@
        (ghc_configuration
        ,ar_configuration
        ,ld_configuration
        ,install_configuration
        ) <- (,,,)
            <$> (configureUsingSection "GHC")
            <*> (configureUsingSection "Binutils")
            <*> (configureUsingSection "Binutils")
            <*> (configureUsingSection "Installation Directories")
    package_resolutions <- configurePackageResolutions ghc_configuration package_dependencies package_resolution_section
    package_modules <- configurePackageModules ghc_configuration package_resolutions package_cache_section
    return $
        Configuration
            ghc_configuration
            ar_configuration
            ld_configuration
            install_configuration
            package_resolutions
            package_modules
-- @-node:gcross.20091204093401.2766:doConfigure
-- @+node:gcross.20091204093401.2768:buildObjects
buildObjects :: String -> [String] -> Resources -> Configuration -> Resources
buildObjects build_root flags source_resources configuration =
    ghcCompileAll
        (ghcConfiguration configuration)
        (build_root </> "digest-cache")
        flags
        (packageModules configuration)
        (build_root </> "objects")
        (build_root </> "haskell-interfaces")
        source_resources
-- @-node:gcross.20091204093401.2768:buildObjects
-- @+node:gcross.20091204093401.2770:buildLibrary
buildLibrary :: Resources -> PackageIdentifier -> Configuration -> ErrorMessageOr (Resource,Resource,Resources)
buildLibrary source_resources package_identifier@(PackageIdentifier (PackageName package_name) _) configuration =
    let compiled_resources =
            buildObjects
                "build"
                (("-package-name="++(toQualifiedName package_identifier)):ghc_flags)
                source_resources
                configuration
        object_subdirectory = "build" </> "objects"
        object_resources =
            filter ((== object_subdirectory) . take (length object_subdirectory) . resourceFilePath)
            .
            Map.elems
            $
            compiled_resources
        library = formStaticLibrary
            (arConfiguration configuration)
            "build/digest-cache"
            object_resources
            ("lib" ++ package_name)
            ("build/libraries/lib" ++ package_name ++ ".a")
        ghci_library = linkIntoObject
            (ldConfiguration configuration)
            "build/digest-cache"
            object_resources
            package_name
            ("build/libraries/" ++ package_name ++ ".o")
    in do
        attemptGetDigests [library,ghci_library]
        >>
        return (library,ghci_library,compiled_resources)
-- @-node:gcross.20091204093401.2770:buildLibrary
-- @+node:gcross.20091204093401.2771:buildSelf
buildSelf :: Resources -> Configuration -> ErrorMessageOr MD5Digest
buildSelf source_resources configuration =
    resourceDigest
    .
    ghcLinkProgram
        (ghcConfiguration configuration)
        "build/self/digest-cache"
        ghc_flags
        (packageDependencies configuration)
        "."
        [("Setup","o")]
    $
    buildObjects
        "build/self"
        ghc_flags
        (addSourceResourceFor "Setup.hs" source_resources)
        configuration
-- @-node:gcross.20091204093401.2771:buildSelf
-- @+node:gcross.20091204093401.2776:doTest
doTest :: Resources -> Configuration -> ErrorMessageOr ()
doTest source_resources configuration =
    (resourceDigest
     .
     ghcLinkProgram
        (ghcConfiguration configuration)
        "build/self/digest-cache"
        ghc_flags
        (packageDependencies configuration)
        "."
        [("test","o")]
     $
     buildObjects
         "build/self"
         ghc_flags
         source_resources
         configuration
    )
    >>
    (unsafePerformIO (system "./test") `pseq` return ())
-- @-node:gcross.20091204093401.2776:doTest
-- @+node:gcross.20091204093401.2778:doInstall
doInstall :: Package.PackageDescription -> Configuration -> (Resource,Resource,Resources) -> ErrorMessageOr ()
doInstall package_description configuration (library_resource,ghci_library_resource,compiled_resources) =
    let interface_subdirectory = "build" </> "haskell-interfaces"
        interface_resources =
            filter ((== interface_subdirectory) . take (length interface_subdirectory) . resourceFilePath)
            .
            Map.elems
            $
            compiled_resources
        installation_result =
            installSimplePackage
                (ghcConfiguration configuration)
                (installerConfiguration configuration)
                package_description
                (packageDependencies configuration)
                (library_resource
                 :ghci_library_resource
                 :interface_resources
                )
    in case installation_result of
            Nothing -> Right ()
            Just error_message -> Left error_message
-- @-node:gcross.20091204093401.2778:doInstall
-- @-node:gcross.20091204093401.2764:Targets
-- @+node:gcross.20091204093401.2248:Functions
-- @+node:gcross.20091204093401.2247:defaultMain
defaultMain :: SourceDirectorySpecification a => a -> Maybe (a,[String]) -> IO ()
defaultMain source_directory_specification maybe_test_information = do
    let source_resources = getSourceResources source_directory_specification
    cabal_filepaths <- find (fileName ==? ".") (extension ==? ".cabal") "."
    when (null cabal_filepaths) $ do
        putStrLn "Unable to find a .cabal file."
        exitFailure
    let package_description = readPackageDescription . head $ cabal_filepaths
        package_dependencies = Package.buildDepends package_description
        package_identifier = Package.package package_description
        PackageIdentifier (PackageName package_name) _ = package_identifier
        configuration_filepath = package_name <.> "cfg"
        configure =
            doConfigure
                "GHC"
                "ZZZ - Please do not edit this unless you know what you are doing"
                configuration_filepath
                package_dependencies
        reconfigure = makeReconfigureTarget configuration_filepath targets
        self =
            configure
            >>=
            buildSelf source_resources
        reself = 
            (makeRemoveFilesAndDirectoriesTarget ["build/self"])
            `thenTarget`
            toTarget self
        build_library = buildLibrary source_resources package_identifier
        build = configure >>= build_library
        rebuild = makeRebuildTarget targets
        install = configure >>= (build_library <^(>>=)^> doInstall package_description)
        clean = 
            makeCleanTarget
                ["build"
                ,"dist"
                ]
        distclean =
            makeDistCleanTarget
                [configuration_filepath
                ]
                targets
        targets =
            [target "configure" configure
            ,target "reconfigure" reconfigure
            ,target "self" self
            ,target "reself" reself
            ,target "build" build
            ,target "rebuild" rebuild
            ,target "install" install
            ,target "clean" clean
            ,target "distclean" distclean
            ] ++ case maybe_test_information of
                    Nothing -> []
                    Just (test_directory_specification,additional_test_dependencies) ->
                        let test_dependencies = package_dependencies ++ map parseDependency additional_test_dependencies
                            test =
                                doConfigure
                                    "Test Packages"
                                    "ZZZZ - Please do not edit this unless you know what you are doing"
                                    configuration_filepath
                                    test_dependencies
                                >>=
                                doTest
                                    (addResources
                                        (getSourceResources test_directory_specification) 
                                        source_resources
                                    )
                            retest =
                                (makeRemoveFilesAndDirectoriesTarget ["build/self"])
                                `thenTarget`
                                toTarget test
                        in  [target "test" test
                            ,target "retest" retest
                            ]
    Blueprint.Main.defaultMain
        (createDefaultHelpMessage options . map fst $ targets)
        targets
-- @-node:gcross.20091204093401.2247:defaultMain
-- @-node:gcross.20091204093401.2248:Functions
-- @-others
-- @-node:gcross.20091204093401.2246:@thin Simple.hs
-- @-leo
