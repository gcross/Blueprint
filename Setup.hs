-- @+leo-ver=4-thin
-- @+node:gcross.20091121210308.1291:@thin Setup.hs
-- @@language Haskell

import qualified Data.Map as Map

import Blueprint.Resources
import Blueprint.Tools.GHC

main = do
    let src_resources = resourcesIn "src"
        Right package_modules = getPackages tools
            ["base"
            ,"bytestring"
            ,"pureMD5"
            ,"containers"
            ,"directory"
            ,"executable-path"
            ,"filepath"
            ,"array"
            ,"process"
            ,"regex-tdfa"
            ,"either-unwrap"
            ]
        Just tools = ghcTools
        compiled_resources = 
            ghcCompileAll
                tools
                defaultOptions
                package_modules
                "obj"
                "haskint"
                src_resources
        Just demanded_resource = Map.lookup ("Blueprint.Tools.GHC","o") compiled_resources
    case resourceDigest demanded_resource of
        Left error_message -> putStrLn $ makeCompositeErrorMessage error_message
        Right digest -> putStrLn . show $ digest
-- @-node:gcross.20091121210308.1291:@thin Setup.hs
-- @-leo
