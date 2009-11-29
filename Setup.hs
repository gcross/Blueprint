-- @+leo-ver=4-thin
-- @+node:gcross.20091121210308.1291:@thin Setup.hs
-- @@language Haskell

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20091128000856.1439:<< Import needed modules >>
import Control.Applicative
import Control.Applicative.Infix

import Data.ConfigFile hiding (options)
import Data.Either.Unwrap
import Data.Maybe
import qualified Data.Map as Map

import Text.PrettyPrint.ANSI.Leijen

import Blueprint.Configuration
import Blueprint.Error
import Blueprint.Main
import Blueprint.Resources
import Blueprint.Tools.Ar
import Blueprint.Tools.GHC
-- @nonl
-- @-node:gcross.20091128000856.1439:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091128000856.1452:Options
options = ["-O"]
-- @-node:gcross.20091128000856.1452:Options
-- @+node:gcross.20091128000856.1451:Packages
package_names =
    ["base"
    ,"bytestring"
    ,"pureMD5"
    ,"containers"
    ,"directory"
    ,"filepath"
    ,"array"
    ,"process"
    ,"regex-tdfa"
    ,"either-unwrap"
    ,"binary"
    ,"parallel"
    ,"mtl"
    ,"ConfigFile"
    ,"InfixApplicative"
    ,"ansi-wl-pprint"
    ,"stringtable-atom"
    ]
-- @-node:gcross.20091128000856.1451:Packages
-- @+node:gcross.20091128000856.1448:Targets
targets =
    [target "configure" configure
    ,target "build" build
    ]
-- @+node:gcross.20091128000856.1449:configure
configure = runConfigurer "Blueprint.cfg" $
    liftA2 (,)
        (configureUsingSection "GHC")
        (configureUsingSection "Binutils")
-- @-node:gcross.20091128000856.1449:configure
-- @+node:gcross.20091128000856.1450:build
build = configure >>= \(ghc_tools,ar_tools) ->
    let src_resources = resourcesIn "src"
        Right package_modules = getPackages ghc_tools package_names
        compiled_resources = 
            ghcCompileAll
                ghc_tools
                options
                package_modules
                "objects"
                "haskell-interfaces"
                "digest-cache"
                src_resources
        library = formStaticLibrary
            ar_tools
            "digest-cache"
            (map snd . filter ((=="o"). snd . fst) . Map.toList $ compiled_resources)
            "libblueprint"
            "lib/libblueprint.a"
        (setup_object,_) =
            ghcCompile
                ghc_tools
                options
                package_modules
                compiled_resources
                "objects"
                "haskell-interfaces"
                "digest-cache"
                (createResourceFor "" "Setup.hs")
        setup_program = ghcLinkProgram
            ghc_tools
            (options ++ ["-package " ++ package_name | package_name <- package_names])
            "digest-cache"
            (findAllObjectDependenciesOf compiled_resources setup_object)
            "Setup"
            "Setup"
    in resourceDigest library <^(,)^> resourceDigest setup_program
-- @-node:gcross.20091128000856.1450:build
-- @-node:gcross.20091128000856.1448:Targets
-- @-others

main = defaultMain targets
-- @-node:gcross.20091121210308.1291:@thin Setup.hs
-- @-leo
