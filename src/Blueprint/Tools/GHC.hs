-- @+leo-ver=4-thin
-- @+node:gcross.20091121204836.1242:@thin GHC.hs
-- @@language Haskell

module Blueprint.Tools.GHC where

-- @<< Import needed modules >>
-- @+node:gcross.20091121210308.1269:<< Import needed modules >>
import Data.Array
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Either
import Data.Either.Unwrap
import Data.Map (Map)
import qualified Data.Map as Map

import System.Directory
import System.Exit
import System.IO.Unsafe
import System.Process

import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString.Lazy

import Blueprint.Miscellaneous
-- @-node:gcross.20091121210308.1269:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091121210308.1270:Types
-- @+node:gcross.20091121210308.1271:GHCTools
data GHCTools = GHCTools
    {   ghcVersion :: [Int]
    ,   ghcCompilerPath :: String
    ,   ghcPackageQueryPath :: String
    } deriving (Show)
-- @-node:gcross.20091121210308.1271:GHCTools
-- @-node:gcross.20091121210308.1270:Types
-- @+node:gcross.20091121210308.2014:Values
-- @+node:gcross.20091121210308.2015:regular expressions
import_matching_regex = fromRight . compile defaultCompOpt defaultExecOpt . L8.pack $ "\\s*import +(qualified +)?([A-Z][A-Za-z0-9_.]+)[\\s;]?"
-- @-node:gcross.20091121210308.2015:regular expressions
-- @-node:gcross.20091121210308.2014:Values
-- @+node:gcross.20091121210308.2016:Functions
-- @+node:gcross.20091121210308.2017:dependenciesOf
dependenciesOf :: FilePath -> [String]
dependenciesOf =
    map (L8.unpack . fst . (! 2))
    .
    matchAllText import_matching_regex
    .
    unsafePerformIO
    .
    L.readFile
-- @-node:gcross.20091121210308.2017:dependenciesOf
-- @+node:gcross.20091121210308.2018:modulesExposedBy
modulesExposedBy :: GHCTools -> String -> Maybe [String]
modulesExposedBy tools package_name =
    case unsafePerformIO $
            readProcessWithExitCode (ghcPackageQueryPath tools) ["field",package_name,"exposed-modules"] ""
    of (ExitSuccess,response,_) -> Just . filter (/= "exposed-modules:") . words $ response 
       _ -> Nothing
-- @-node:gcross.20091121210308.2018:modulesExposedBy
-- @+node:gcross.20091121210308.2019:getPackage
getPackage :: GHCTools -> String -> Maybe (Map String String)
getPackage tools name = fmap (Map.fromList . map (flip (,) name)) $ modulesExposedBy tools name
-- @-node:gcross.20091121210308.2019:getPackage
-- @+node:gcross.20091121210308.2021:getPackages
getPackages :: GHCTools -> [String] -> Either [String] (Map String String)
getPackages tools names =
    let either_packages =
            flip map names $ \name -> maybe (Left name) Right (getPackage tools name)
    in case partitionEithers either_packages of
        ([],packages) -> Right . Map.unions $ packages
        (not_found,_) -> Left not_found
-- @-node:gcross.20091121210308.2021:getPackages
-- @-node:gcross.20091121210308.2016:Functions
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
                    ,   ghcPackageQueryPath = path_to_ghc ++ "-pkg"
                    }
-- @-node:gcross.20091121210308.1274:ghcTools
-- @-node:gcross.20091121210308.1273:Configuration
-- @+node:gcross.20091121210308.1275:Tools
-- @-node:gcross.20091121210308.1275:Tools
-- @-others
-- @-node:gcross.20091121204836.1242:@thin GHC.hs
-- @-leo
