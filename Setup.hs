-- @+leo-ver=4-thin
-- @+node:gcross.20091121210308.1291:@thin Setup.hs
-- @@language Haskell

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

import System.Directory
import System.IO.Unsafe

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blueprint.Configuration
import Blueprint.Error
import Blueprint.Main
import Blueprint.Options
import Blueprint.Resources
import Blueprint.Tools.Ar
import Blueprint.Tools.GHC
import Blueprint.Tools.Haddock
import Blueprint.Tools.Installer
-- @-node:gcross.20091128000856.1439:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091129000542.1484:Options
options =
    [   installerOptions
    ,   arOptions
    ,   ghcOptions
    ,   haddockOptions
    ]
-- @-node:gcross.20091129000542.1484:Options
-- @+node:gcross.20091128000856.1452:Flags
ghc_flags = ["-O","-threaded"]
-- @-node:gcross.20091128000856.1452:Flags
-- @+node:gcross.20091129000542.1707:Types
-- @+node:gcross.20091129000542.1708:Configuration
data Configuration = Configuration
    {   ghcConfiguration :: GHCConfiguration
    ,   arConfiguration :: ArConfiguration
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
-- @-node:gcross.20091128000856.1475:Values
-- @+node:gcross.20091128000856.1448:Targets
targets =
    [target "configure" configure
    ,target "reconfigure" $ makeReconfigureTarget configurationFilePath targets
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
        ,haddock_configuration
        ,install_configuration
        ) <- (,,,)
            <$> (configureUsingSection "GHC")
            <*> (configureUsingSection "ar")
            <*> (configureUsingSection "Haddock")
            <*> (configureUsingSection "Installation Directories")
    package_dependencies <- configurePackageResolutions ghc_configuration package_description "GHC"
    return $
        Configuration
            ghc_configuration
            ar_configuration
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
        library = formStaticLibrary
            (arConfiguration configuration)
            "digest-cache"
            (map snd . filter ((=="o"). snd . fst) . Map.toList $ compiled_resources)
            "libblueprint"
            "libraries/libblueprint.a"
        (setup_object,_) =
            ghcCompile
                (ghcConfiguration configuration)
                ghc_flags
                package_modules
                compiled_resources
                "objects"
                "haskell-interfaces"
                "digest-cache"
                (createResourceFor "" "Setup.hs")
        setup_program = ghcLinkProgram
            (ghcConfiguration configuration)
            ((ghc_flags ++) . map ("-package" ++) . packageDependencies $ configuration)
            "digest-cache"
            (findAllObjectDependenciesOf compiled_resources setup_object)
            "Setup"
            "Setup"
    in do
        attemptGetDigests [library,setup_program] >> return (library,compiled_resources)
-- @-node:gcross.20091128000856.1450:build
-- @+node:gcross.20091129000542.1706:install
install = do
    configuration <- configure
    (library_resource,compiled_resources) <- build
    let interface_resources = filter ((=="hi") . resourceType) . Map.elems $ compiled_resources
        installation_result =
            installSimplePackage
                (ghcConfiguration configuration)
                (installerConfiguration configuration)
                package_description
                (packageDependencies configuration)
                (library_resource:interface_resources)
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
