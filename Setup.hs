-- @+leo-ver=4-thin
-- @+node:gcross.20091121210308.1291:@thin Setup.hs
-- @@language Haskell

import Data.Maybe
import qualified Data.Map as Map

import Blueprint.Resources
import Blueprint.Tools.Ar
import Blueprint.Tools.GHC

main = do
    let src_resources = resourcesIn "src"
        Right package_modules = getPackages tools
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
            ]
        Just tools = ghcTools
        compiled_resources = 
            ghcCompileAll
                tools
                ["-package-name blueprint"]
                package_modules
                "objects"
                "haskell-interfaces"
                "hash-cache"
                src_resources
        library = formStaticLibrary
            (fromJust arTools)
            "hash-cache"
            (map snd . filter ((=="o"). snd . fst) . Map.toList $ compiled_resources)
            "libblueprint"
            "lib/libblueprint.a"
    case resourceDigest library of
        Left error_message -> putStrLn $ makeCompositeErrorMessage error_message
        Right digest -> putStrLn . show $ digest
-- @-node:gcross.20091121210308.1291:@thin Setup.hs
-- @-leo
