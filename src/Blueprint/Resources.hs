-- @+leo-ver=4-thin
-- @+node:gcross.20091121210308.1276:@thin Resources.hs
-- @@language Haskell

module Blueprint.Resources where

-- @<< Import needed modules >>
-- @+node:gcross.20091121210308.1278:<< Import needed modules >>
import Control.Arrow

import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.MD5
import Data.Either
import Data.Map (Map)
import qualified Data.Map as Map

import System.Directory
import System.Environment.Executable
import System.FilePath
import System.IO.Unsafe

import Debug.Trace
-- @-node:gcross.20091121210308.1278:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091121210308.1277:Types
-- @+node:gcross.20091121210308.1279:Resource
data Resource = Resource
    {   resourceName :: !String
    ,   resourceType :: !String
    ,   resourceFilePath :: !FilePath
    ,   resourceDigest :: MD5Digest
    } deriving (Eq,Show)
-- @-node:gcross.20091121210308.1279:Resource
-- @+node:gcross.20091121210308.1284:Resources
type Resources = Map (String,String) Resource
-- @-node:gcross.20091121210308.1284:Resources
-- @-node:gcross.20091121210308.1277:Types
-- @+node:gcross.20091121210308.1280:Functions
-- @+node:gcross.20091121210308.1293:applyPrefix
applyPrefix :: String -> String -> String
applyPrefix "" = id
applyPrefix prefix = (prefix ++) . ('.':)
-- @-node:gcross.20091121210308.1293:applyPrefix
-- @+node:gcross.20091121210308.1285:digestOf
digestOf :: FilePath -> MD5Digest
digestOf = md5 . unsafePerformIO . L.readFile
-- @-node:gcross.20091121210308.1285:digestOf
-- @+node:gcross.20091121210308.1283:programPath
programPath = "" -- fst . unsafePerformIO $ splitExecutablePath
-- @-node:gcross.20091121210308.1283:programPath
-- @+node:gcross.20091121210308.1292:resourcesIn
resourcesIn = resourcesWithPrefixIn ""
-- @-node:gcross.20091121210308.1292:resourcesIn
-- @+node:gcross.20091121210308.1281:resourcesWithPrefixIn
resourcesWithPrefixIn :: String -> FilePath -> Resources
resourcesWithPrefixIn prefix directory =
    let (resource_list,subdirectory_resources) =
            partitionEithers
            .
            map (\filename ->
                let filepath = combine directory filename
                in if (unsafePerformIO . doesDirectoryExist $ filepath)
                    then Right . resourcesWithPrefixIn (applyPrefix prefix filename) $ filepath
                    else Left . createResourceFor prefix $ filepath
            )
            .
            filter ((/= '.') . head)
            .
            unsafePerformIO
            .
            getDirectoryContents
            $
            directory
    in Map.unions . (toResources resource_list:) $ subdirectory_resources
-- @-node:gcross.20091121210308.1281:resourcesWithPrefixIn
-- @+node:gcross.20091121210308.1286:createResourceFor
createResourceFor :: String -> FilePath -> Resource
createResourceFor prefix filepath =
    let (name, extension) = splitExtension . takeFileName $ filepath
    in Resource
        {   resourceName = applyPrefix prefix name
        ,   resourceType = if extension == "" then "" else tail extension
        ,   resourceFilePath = filepath
        ,   resourceDigest = digestOf filepath
        }
-- @-node:gcross.20091121210308.1286:createResourceFor
-- @+node:gcross.20091121210308.1288:toResources
toResources :: [Resource] -> Resources
toResources =
    Map.fromList
    .
    map ((resourceName &&& resourceType) &&& id)
-- @-node:gcross.20091121210308.1288:toResources
-- @-node:gcross.20091121210308.1280:Functions
-- @-others
-- @-node:gcross.20091121210308.1276:@thin Resources.hs
-- @-leo
