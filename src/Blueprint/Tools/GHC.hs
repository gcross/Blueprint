-- @+leo-ver=4-thin
-- @+node:gcross.20091121204836.1242:@thin GHC.hs
-- @@language Haskell

module Blueprint.Tools.GHC where

-- @<< Import needed modules >>
-- @+node:gcross.20091121210308.1269:<< Import needed modules >>
import System.Directory
import System.IO.Unsafe
import System.Process

import Blueprint.Miscellaneous
-- @-node:gcross.20091121210308.1269:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091121210308.1270:Types
-- @+node:gcross.20091121210308.1271:GHCTools
data GHCTools = GHCTools
    {   ghcVersion :: [Int]
    ,   ghcCompilerPath :: String
    } deriving (Show)
-- @-node:gcross.20091121210308.1271:GHCTools
-- @+node:gcross.20091121210308.1272:GHCCompilerOptions
data GHCCompilerOptions = GHCCompilerOptions ()
-- @-node:gcross.20091121210308.1272:GHCCompilerOptions
-- @-node:gcross.20091121210308.1270:Types
-- @+node:gcross.20091121210308.1273:Configuration
-- @+node:gcross.20091121210308.1274:ghcTools
ghcTools :: Maybe GHCTools
ghcTools = unsafePerformIO $ do
    maybe_path_to_ghc <- findExecutable "ghc"
    case maybe_path_to_ghc of
        Nothing -> return Nothing
        Just path_to_ghc -> do
            version_as_string <- readProcess path_to_ghc ["--numeric-version"] ""
            return . Just $
                GHCTools
                    {   ghcVersion = map read . splitDot $ version_as_string
                    ,   ghcCompilerPath = path_to_ghc
                    }
-- @-node:gcross.20091121210308.1274:ghcTools
-- @-node:gcross.20091121210308.1273:Configuration
-- @+node:gcross.20091121210308.1275:Tools
-- @-node:gcross.20091121210308.1275:Tools
-- @-others
-- @-node:gcross.20091121204836.1242:@thin GHC.hs
-- @-leo
