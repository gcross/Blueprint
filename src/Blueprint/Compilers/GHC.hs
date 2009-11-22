-- @+leo-ver=4-thin
-- @+node:gcross.20091121204836.1242:@thin GHC.hs
-- @@language Haskell

module Blueprint.Compilers.GHC where

-- @<< Import needed modules >>
-- @+node:gcross.20091121210308.1269:<< Import needed modules >>
-- @-node:gcross.20091121210308.1269:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091121210308.1270:Types
-- @+node:gcross.20091121210308.1271:GHCTools
data GHCTools = GHCTools
    {   ghcVersion :: [Int]
    ,   ghcCompiler :: GHCCompilerOptions -> Resource -> (Resource,Resource)
    --,   ghcProgramLinker :: GHCCompilerOptions -> [ObjectFile] -> ExecutableFile
    }
-- @-node:gcross.20091121210308.1271:GHCTools
-- @+node:gcross.20091121210308.1272:GHCCompilerOptions
data GHCCompilerOptions = GHCCompilerOptions ()
-- @-node:gcross.20091121210308.1272:GHCCompilerOptions
-- @-node:gcross.20091121210308.1270:Types
-- @+node:gcross.20091121210308.1273:Configuration
-- @+node:gcross.20091121210308.1274:ghcTools
-- @-node:gcross.20091121210308.1274:ghcTools
-- @-node:gcross.20091121210308.1273:Configuration
-- @+node:gcross.20091121210308.1275:Tools
-- @-node:gcross.20091121210308.1275:Tools
-- @-others
-- @-node:gcross.20091121204836.1242:@thin GHC.hs
-- @-leo
