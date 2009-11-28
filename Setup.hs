-- @+leo-ver=4-thin
-- @+node:gcross.20091121210308.1291:@thin Setup.hs
-- @@language Haskell

import Data.Maybe
import qualified Data.Map as Map

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

main = do
    let src_resources = resourcesIn "src"
        Right package_modules = getPackages tools package_names
        Just tools = ghcTools
        compiled_resources = 
            ghcCompileAll
                tools
                [] -- "-package-name blueprint"]
                package_modules
                "objects"
                "haskell-interfaces"
                "digest-cache"
                src_resources
        library = formStaticLibrary
            (fromJust arTools)
            "digest-cache"
            (map snd . filter ((=="o"). snd . fst) . Map.toList $ compiled_resources)
            "libblueprint"
            "lib/libblueprint.a"
        (setup_object,_) =
            ghcCompile
                (fromJust ghcTools)
                []
                package_modules
                compiled_resources
                "objects"
                "haskell-interfaces"
                "digest-cache"
                (createResourceFor "" "Setup.hs")
        setup_program = ghcLinkProgram
            (fromJust ghcTools)
            ["-package " ++ package_name | package_name <- package_names]
            "digest-cache"
            (findAllObjectDependenciesOf compiled_resources setup_object)
            "Setup"
            "Setup"
    case resourceDigest library of
        Left error_message -> putStrLn $ makeCompositeErrorMessage error_message
        Right digest -> putStrLn . show $ digest
    case resourceDigest setup_program of
        Left error_message -> putStrLn $ makeCompositeErrorMessage error_message
        Right digest -> putStrLn . show $ digest
-- @-node:gcross.20091121210308.1291:@thin Setup.hs
-- @-leo
