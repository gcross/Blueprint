-- @+leo-ver=4-thin
-- @+node:gcross.20091121210308.1291:@thin Setup.hs
-- @@language Haskell

import Control.Applicative
import Control.Applicative.Infix

import Data.ConfigFile
import Data.Either.Unwrap
import Data.Maybe
import qualified Data.Map as Map

import Blueprint.Configuration
import Blueprint.Resources
import Blueprint.Tools.Ar
import Blueprint.Tools.GHC

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
    ]

configuration = runConfigurer "Blueprint.cfg" $
    liftA2 (,)
        (configureUsingSection "GHC")
        (configureUsingSection "Binutils")

build = configuration >>= \(ghc_tools,ar_tools) ->
    let src_resources = resourcesIn "src"
        Right package_modules = getPackages ghc_tools package_names
        compiled_resources = 
            ghcCompileAll
                ghc_tools
                ["-O2"] -- "-package-name blueprint"]
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
                ["-O2"]
                package_modules
                compiled_resources
                "objects"
                "haskell-interfaces"
                "digest-cache"
                (createResourceFor "" "Setup.hs")
        setup_program = ghcLinkProgram
            ghc_tools
            ["-package " ++ package_name | package_name <- package_names]
            "digest-cache"
            (findAllObjectDependenciesOf compiled_resources setup_object)
            "Setup"
            "Setup"
    in resourceDigest library <^(,)^> resourceDigest setup_program

main = putStrLn $
    case build of
        Left error_message -> makeCompositeErrorMessage error_message
        Right digests -> show digests
-- @-node:gcross.20091121210308.1291:@thin Setup.hs
-- @-leo
