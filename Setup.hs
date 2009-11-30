-- @+leo-ver=4-thin
-- @+node:gcross.20091121210308.1291:@thin Setup.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091129000542.1713:<< Language extensions >>
{-# LANGUAGE PackageImports #-}
-- @-node:gcross.20091129000542.1713:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20091128000856.1439:<< Import needed modules >>
import Control.Applicative
import Control.Applicative.Infix
import Control.Exception
import Control.Monad
import Control.Parallel

import Data.ConfigFile hiding (options)
import Data.Either.Unwrap
import Data.Maybe
import qualified Data.Map as Map
import Data.Version

import Distribution.Package
import qualified Distribution.PackageDescription as Package

import System.Directory
import System.IO.Unsafe

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blueprint.Configuration
import Blueprint.Error
import Blueprint.Main
import Blueprint.Miscellaneous
import Blueprint.Options
import Blueprint.Resources
import Blueprint.Tools.Ar
import Blueprint.Tools.GHC
import Blueprint.Tools.Haddock
import Blueprint.Tools.Installer
import Blueprint.Tools.Ld
-- @-node:gcross.20091128000856.1439:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091129000542.1484:Options
options =
    [   installerOptions
    ,   arOptions
    ,   ldOptions
    ,   ghcOptions
    ,   haddockOptions
    ]
-- @-node:gcross.20091129000542.1484:Options
-- @+node:gcross.20091128000856.1452:Flags
bootstrap_ghc_flags = ["-O","-threaded"]

ghc_flags = ("-package-name="++qualified_package_name):bootstrap_ghc_flags
-- @nonl
-- @-node:gcross.20091128000856.1452:Flags
-- @+node:gcross.20091129000542.1707:Types
-- @+node:gcross.20091129000542.1708:Configuration
data Configuration = Configuration
    {   ghcConfiguration :: GHCConfiguration
    ,   arConfiguration :: ArConfiguration
    ,   ldConfiguration :: LdConfiguration
    ,   haddockConfiguration :: HaddockConfiguration
    ,   installerConfiguration :: InstallerConfiguration
    ,   packageDependencies :: [String]
    }
-- @-node:gcross.20091129000542.1708:Configuration
-- @-node:gcross.20091129000542.1707:Types
-- @+node:gcross.20091128000856.1475:Values
-- @+node:gcross.20091128000856.1476:source resources
source_resources = resourcesWithPrefixIn "Blueprint" "Blueprint"
-- @-node:gcross.20091128000856.1476:source resources
-- @+node:gcross.20091128201230.1460:package description
package_description = readPackageDescription "Blueprint.cabal"
-- @-node:gcross.20091128201230.1460:package description
-- @+node:gcross.20091129000542.1593:configuration file path
configurationFilePath = "Blueprint.cfg"
-- @-node:gcross.20091129000542.1593:configuration file path
-- @+node:gcross.20091129000542.1712:qualified package name
qualified_package_name =
    let PackageIdentifier (PackageName name) version = Package.package package_description
    in name ++ "-" ++ showVersion version
-- @-node:gcross.20091129000542.1712:qualified package name
-- @-node:gcross.20091128000856.1475:Values
-- @+node:gcross.20091128000856.1448:Targets
targets =
    [target "configure" configure
    ,target "reconfigure" $ makeReconfigureTarget configurationFilePath targets
    ,target "bootstrap" bootstrap
    ,target "build" build
    ,target "rebuild" $ makeRebuildTarget targets
    ,target "haddock" haddock
    ,target "install" install
    ,target "clean" $
        makeCleanTarget
            ["objects"
            ,"digest-cache"
            ,"haskell-interfaces"
            ,"libraries"
            ,"haddock"
            ,"bootstrap"
            ]
            targets
    ,target "distclean" $
        makeDistCleanTarget
            [configurationFilePath
            ]
            targets
    ]
-- @+node:gcross.20091128000856.1449:configure
configure = parseCommandLineOptions options >>= \(_,options) -> runConfigurer "Blueprint.cfg" options $ do
    configurations@
        (ghc_configuration
        ,ar_configuration
        ,ld_configuration
        ,haddock_configuration
        ,install_configuration
        ) <- (,,,,)
            <$> (configureUsingSection "GHC")
            <*> (configureUsingSection "Binutils")
            <*> (configureUsingSection "Binutils")
            <*> (configureUsingSection "Haddock")
            <*> (configureUsingSection "Installation Directories")
    package_dependencies <- configurePackageResolutions ghc_configuration package_description "GHC"
    return $
        Configuration
            ghc_configuration
            ar_configuration
            ld_configuration
            haddock_configuration
            install_configuration
            package_dependencies
-- @-node:gcross.20091128000856.1449:configure
-- @+node:gcross.20091128000856.1450:build
build = configure >>= \configuration ->
    let Right package_modules = getPackages <$> ghcConfiguration <*> packageDependencies $ configuration
        compiled_resources = 
            ghcCompileAll
                (ghcConfiguration configuration)
                ghc_flags
                package_modules
                "objects"
                "haskell-interfaces"
                "digest-cache"
                source_resources
        object_resources =
            map snd
            .
            filter ((=="o"). snd . fst)
            .
            Map.toList
            $
            compiled_resources
        library = formStaticLibrary
            (arConfiguration configuration)
            "digest-cache"
            object_resources
            "libblueprint"
            "libraries/libblueprint.a"
        ghci_library = linkIntoObject
            (ldConfiguration configuration)
            "digest-cache"
            object_resources
            "blueprint"
            "libraries/blueprint.o"
    in do
        attemptGetDigests [library,ghci_library]
        >>
        return (library,ghci_library,compiled_resources)
-- @-node:gcross.20091128000856.1450:build
-- @+node:gcross.20091129000542.1715:bootstrap
bootstrap = configure >>= \configuration ->
    let Right package_modules = getPackages <$> ghcConfiguration <*> packageDependencies $ configuration
        compiled_resources = 
            ghcCompileAll
                (ghcConfiguration configuration)
                bootstrap_ghc_flags
                package_modules
                "bootstrap/objects"
                "bootstrap/haskell-interfaces"
                "bootstrap/digest-cache"
                source_resources
        (setup_object,_) =
            ghcCompile
                (ghcConfiguration configuration)
                bootstrap_ghc_flags
                package_modules
                compiled_resources
                "bootstrap/objects"
                "bootstrap/haskell-interfaces"
                "bootstrap/digest-cache"
                (createResourceFor "" "Setup.hs")

        setup_program = ghcLinkProgram
            (ghcConfiguration configuration)
            (((bootstrap_ghc_flags ++) . map ("-package" ++) . packageDependencies $ configuration))
            "bootstrap/digest-cache"
            (findAllObjectDependenciesOf compiled_resources setup_object)
            "Setup"
            "Setup"
    in attemptGetDigests [setup_program]
-- @-node:gcross.20091129000542.1715:bootstrap
-- @+node:gcross.20091129000542.1706:install
install = do
    configuration <- configure
    (library_resource,ghci_library_resource,compiled_resources) <- build
    let interface_resources = filter ((=="hi") . resourceType) . Map.elems $ compiled_resources
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
    case installation_result of
        Nothing -> Right ()
        Just error_message -> Left error_message
-- @-node:gcross.20091129000542.1706:install
-- @+node:gcross.20091128000856.1474:haddock
haddock = configure >>= \configuration ->
    resourceDigest $
        createDocumentation
            (haddockConfiguration configuration)
            (ghcConfiguration configuration)
            []
            []
            "digest-cache"
            (Map.elems source_resources)
            "haddock"
-- @-node:gcross.20091128000856.1474:haddock
-- @-node:gcross.20091128000856.1448:Targets
-- @+node:gcross.20091129000542.1485:main
main = defaultMain
        (createDefaultHelpMessage options . map fst $ targets)
        targets
-- @-node:gcross.20091129000542.1485:main
-- @-others
-- @-node:gcross.20091121210308.1291:@thin Setup.hs
-- @-leo
