-- @+leo-ver=4-thin
-- @+node:gcross.20091121210308.1276:@thin Resources.hs
-- @@language Haskell

module Blueprint.Resources where

-- @<< Import needed modules >>
-- @+node:gcross.20091121210308.1278:<< Import needed modules >>
import Control.Arrow
import Control.Parallel.Strategies

import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.MD5
import Data.Either
import Data.List
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
    ,   resourceDigest :: Either ErrorMessage MD5Digest
    } deriving (Eq,Show)
-- @-node:gcross.20091121210308.1279:Resource
-- @+node:gcross.20091121210308.1284:Resources
type Resources = Map ResourceId Resource
-- @-node:gcross.20091121210308.1284:Resources
-- @+node:gcross.20091122100142.1326:ResourceId
type ResourceId = (String,String)
-- @-node:gcross.20091122100142.1326:ResourceId
-- @+node:gcross.20091122100142.1333:ErrorMessage
type ErrorMessage = (Map String String)
-- @-node:gcross.20091122100142.1333:ErrorMessage
-- @-node:gcross.20091121210308.1277:Types
-- @+node:gcross.20091121210308.1280:Functions
-- @+node:gcross.20091122100142.1388:resourceId
resourceId :: Resource -> ResourceId
resourceId = (resourceName &&& resourceType)
-- @-node:gcross.20091122100142.1388:resourceId
-- @+node:gcross.20091121210308.2037:splitDot
splitDot :: String -> [String]
splitDot "" = []
splitDot s =
    let (first_part, rest_string) = break (== '.') s
    in first_part : if null rest_string then [] else splitDot . tail $ rest_string
-- @-node:gcross.20091121210308.2037:splitDot
-- @+node:gcross.20091121210308.2035:unsplitDot
unsplitDot = intercalate "."
-- @nonl
-- @-node:gcross.20091121210308.2035:unsplitDot
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
        ,   resourceDigest = Right . digestOf $ filepath
        }
-- @-node:gcross.20091121210308.1286:createResourceFor
-- @+node:gcross.20091121210308.1288:toResources
toResources :: [Resource] -> Resources
toResources =
    Map.fromList
    .
    map ((resourceName &&& resourceType) &&& id)
-- @-node:gcross.20091121210308.1288:toResources
-- @+node:gcross.20091121210308.2033:getFilePathForNameAndType
getFilePathForNameAndType :: FilePath -> String -> String -> FilePath
getFilePathForNameAndType directory name typ =
    let partial_path = (joinPath (directory:splitDot name))
    in if null typ then partial_path else partial_path <.> typ
-- @-node:gcross.20091121210308.2033:getFilePathForNameAndType
-- @+node:gcross.20091121210308.2039:addResource
addResource :: Resource -> Resources -> Resources
addResource resource = Map.insert ((resourceName &&& resourceType) resource) resource
-- @-node:gcross.20091121210308.2039:addResource
-- @+node:gcross.20091121210308.2040:makeCompositeErrorMessage
makeCompositeErrorMessage :: Map String String -> String
makeCompositeErrorMessage =
    unlines
    .
    map (\(name,message) -> "Error compiling " ++ name ++ ":\n\n" ++ message)
    .
    Map.toList
-- @nonl
-- @-node:gcross.20091121210308.2040:makeCompositeErrorMessage
-- @+node:gcross.20091122100142.1329:attemptGetResources
attemptGetResources :: Resources -> [ResourceId] -> Either [ResourceId] [Resource]
attemptGetResources resources resource_ids =
    let (unknown_resource_ids,fetched_resources) =
            partitionEithers
            .
            map (\resource_id ->
                case Map.lookup resource_id resources of
                    Nothing -> Left resource_id
                    Just resource -> Right resource
            )
            $
            resource_ids
    in if null unknown_resource_ids
        then Right fetched_resources
        else Left unknown_resource_ids
-- @-node:gcross.20091122100142.1329:attemptGetResources
-- @+node:gcross.20091122100142.1331:attemptGetDigests
attemptGetDigests :: [Resource] -> Either ErrorMessage [MD5Digest]
attemptGetDigests resources =
    let (error_messages,digests) =
            partitionEithers
            .
            parMap rwhnf resourceDigest
            $
            resources
    in if null error_messages
        then Right digests
        else Left . Map.unions $ error_messages
-- @-node:gcross.20091122100142.1331:attemptGetDigests
-- @+node:gcross.20091122100142.1332:attemptGetResourceDigests
attemptGetResourceDigests :: Resources -> [ResourceId] -> Either (Either [ResourceId] (Map String String)) [MD5Digest]
attemptGetResourceDigests resources resource_ids =
    case attemptGetResources resources resource_ids of
        Left unknown_resource_ids -> Left . Left $ unknown_resource_ids
        Right resource_list -> either (Left . Right) Right $ attemptGetDigests resource_list
-- @-node:gcross.20091122100142.1332:attemptGetResourceDigests
-- @+node:gcross.20091122100142.1384:reportUnknownResources
reportUnknownResources :: String -> [ResourceId] -> Map String String
reportUnknownResources filepath =
    Map.singleton filepath
    .
    unlines
    .
    map show
-- @-node:gcross.20091122100142.1384:reportUnknownResources
-- @-node:gcross.20091121210308.1280:Functions
-- @-others
-- @-node:gcross.20091121210308.1276:@thin Resources.hs
-- @-leo
