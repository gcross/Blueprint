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

import Text.PrettyPrint.ANSI.Leijen

import Blueprint.Configuration
import Blueprint.Error
import Blueprint.Main
import Blueprint.Options
import Blueprint.Resources
import Blueprint.Tools.Ar
import Blueprint.Tools.GHC
import Blueprint.Tools.Haddock
-- @-node:gcross.20091128000856.1439:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091129000542.1484:Options
options =
    [   arToolsOptions "ar Tools"
    ,   ghcToolsOptions "GHC Tools"
    ]
-- @-node:gcross.20091129000542.1484:Options
-- @+node:gcross.20091128000856.1452:Flags
ghc_flags = ["-O2","-threaded"]
-- @-node:gcross.20091128000856.1452:Flags
-- @+node:gcross.20091128000856.1475:Values
-- @+node:gcross.20091128000856.1476:source resources
source_resources = resourcesWithPrefixIn "Blueprint" "Blueprint"
-- @-node:gcross.20091128000856.1476:source resources
-- @+node:gcross.20091128201230.1460:package description
package_description = readPackageDescription "Blueprint.cabal"
-- @-node:gcross.20091128201230.1460:package description
-- @-node:gcross.20091128000856.1475:Values
-- @+node:gcross.20091128000856.1448:Targets
targets =
    [target "configure" configure
    ,target "reconfigure" reconfigure
    ,target "build" build
    ,target "rebuild" rebuild
    ,target "haddock" haddock
    ,target "clean" clean
    ,target "distclean" distclean
    ]
-- @+node:gcross.20091128000856.1449:configure
configure = parseCommandLineOptions options >>= \(_,options) -> runConfigurer "Blueprint.cfg" options $ do
    (ghc_tools,ar_tools) <- 
        liftA2 (,)
            (configureUsingSection "GHC" "GHC Tools")
            (configureUsingSection "ar" "ar Tools")
    package_resolutions <- configurePackageResolutions ghc_tools package_description "GHC" ""
    return (ghc_tools,ar_tools,package_resolutions)
-- @-node:gcross.20091128000856.1449:configure
-- @+node:gcross.20091128201230.1465:reconfigure
reconfigure = unsafePerformIO $ do
    file_exists <- doesFileExist "Blueprint.cfg"
    when file_exists $ removeFile "Blueprint.cfg"
    return configure
-- @-node:gcross.20091128201230.1465:reconfigure
-- @+node:gcross.20091128000856.1450:build
build = configure >>= \(ghc_tools,ar_tools,package_resolutions) ->
    let Right package_modules = getPackages ghc_tools package_resolutions
        compiled_resources = 
            ghcCompileAll
                ghc_tools
                ghc_flags
                package_modules
                "objects"
                "haskell-interfaces"
                "digest-cache"
                source_resources
        library = formStaticLibrary
            ar_tools
            "digest-cache"
            (map snd . filter ((=="o"). snd . fst) . Map.toList $ compiled_resources)
            "libblueprint"
            "libraries/libblueprint.a"
        (setup_object,_) =
            ghcCompile
                ghc_tools
                ghc_flags
                package_modules
                compiled_resources
                "objects"
                "haskell-interfaces"
                "digest-cache"
                (createResourceFor "" "Setup.hs")
        setup_program = ghcLinkProgram
            ghc_tools
            (ghc_flags ++ ["-package " ++ package_resolution | package_resolution <- package_resolutions])
            "digest-cache"
            (findAllObjectDependenciesOf compiled_resources setup_object)
            "Setup"
            "Setup"
    in attemptGetDigests [library,setup_program]
-- @-node:gcross.20091128000856.1450:build
-- @+node:gcross.20091129000542.1506:rebuild
rebuild = clean `pseq` build
-- @-node:gcross.20091129000542.1506:rebuild
-- @+node:gcross.20091128000856.1474:haddock
haddock = do
    ((ghc_tools,_,_),haddock_tools) <- configure <^(,)^> (runConfigurer "Blueprint.cfg" noOptions $ configureUsingSection "GHC" "")
    resourceDigest $
        createDocumentation
            haddock_tools
            ghc_tools
            []
            []
            "digest-cache"
            (Map.elems source_resources)
            "haddock"
-- @-node:gcross.20091128000856.1474:haddock
-- @+node:gcross.20091128000856.1477:clean
clean =
    removeFilesAndDirectoriesTarget
        ["objects"
        ,"digest-cache"
        ,"haskell-interfaces"
        ,"libraries"
        ,"haddock"
        ]
-- @-node:gcross.20091128000856.1477:clean
-- @+node:gcross.20091128201230.1468:distclean
distclean =
    clean
    `pseq`
    removeFilesAndDirectoriesTarget
        ["Blueprint.cfg"
        ]
-- @-node:gcross.20091128201230.1468:distclean
-- @-node:gcross.20091128000856.1448:Targets
-- @+node:gcross.20091129000542.1485:main
main = defaultMain
        (createDefaultHelpMessage options . map fst $ targets)
        targets
-- @-node:gcross.20091129000542.1485:main
-- @-others
-- @-node:gcross.20091121210308.1291:@thin Setup.hs
-- @-leo
