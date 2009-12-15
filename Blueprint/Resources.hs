-- @+leo-ver=4-thin
-- @+node:gcross.20091121210308.1276:@thin Resources.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091204093401.2853:<< Language extensions >>
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- @-node:gcross.20091204093401.2853:<< Language extensions >>
-- @nl

module Blueprint.Resources where

-- @<< Import needed modules >>
-- @+node:gcross.20091121210308.1278:<< Import needed modules >>
import Control.Arrow
import Control.Monad
import Control.Parallel.Strategies

import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.MD5
import Data.Either
import Data.Either.Unwrap
import Data.ErrorMessage
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import System.Directory
import System.FilePath
import System.IO.Unsafe

import Text.PrettyPrint.ANSI.Leijen

import Blueprint.Miscellaneous
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
    ,   resourceLinkDependencies :: Either ErrorMessage (Set Resource)
    }
-- @-node:gcross.20091121210308.1279:Resource
-- @+node:gcross.20091121210308.1284:Resources
type Resources = Map ResourceId Resource
-- @-node:gcross.20091121210308.1284:Resources
-- @+node:gcross.20091122100142.1326:ResourceId
type ResourceId = (String,String)
-- @-node:gcross.20091122100142.1326:ResourceId
-- @+node:gcross.20091201183231.1600:ResourceSet
type ResourceSet = Set Resource
-- @-node:gcross.20091201183231.1600:ResourceSet
-- @-node:gcross.20091121210308.1277:Types
-- @+node:gcross.20091204093401.2258:Classes (with instances)
-- @+node:gcross.20091204093401.2259:SourceDirectorySpecification
class SourceDirectorySpecification a where
    getSourceResources :: a -> Resources

instance SourceDirectorySpecification FilePath where
    getSourceResources = sourceResourcesIn

instance SourceDirectorySpecification (String,FilePath) where
    getSourceResources = uncurry sourceResourcesWithPrefixIn

instance (SourceDirectorySpecification a) => SourceDirectorySpecification [a] where
    getSourceResources = Map.unions . map getSourceResources
-- @-node:gcross.20091204093401.2259:SourceDirectorySpecification
-- @-node:gcross.20091204093401.2258:Classes (with instances)
-- @+node:gcross.20091201183231.1585:Instances
-- @+node:gcross.20091201183231.1587:Eq, Ord (Resource)
instance Eq Resource where
    (==) = (==) `on` resourceId

instance Ord Resource where
    compare = compare `on` resourceId
-- @-node:gcross.20091201183231.1587:Eq, Ord (Resource)
-- @-node:gcross.20091201183231.1585:Instances
-- @+node:gcross.20091201183231.1586:Functions
-- @+node:gcross.20091201183231.1569:Turning sources into resources
-- @+node:gcross.20091201183231.1571:sourceResourcesIn
sourceResourcesIn = sourceResourcesWithPrefixIn ""
-- @-node:gcross.20091201183231.1571:sourceResourcesIn
-- @+node:gcross.20091201183231.1573:sourceResourcesWithPrefixIn
sourceResourcesWithPrefixIn :: String -> FilePath -> Resources
sourceResourcesWithPrefixIn prefix directory =
    let (resource_list,subdirectory_resources) =
            partitionEithers
            .
            parMap rwhnf (\filename ->
                let filepath = combine directory filename
                in if (unsafePerformIO . doesDirectoryExist $ filepath)
                    then Right . sourceResourcesWithPrefixIn (applyPrefix prefix filename) $ filepath
                    else Left . createSourceResourceFor prefix $ filepath
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
-- @-node:gcross.20091201183231.1573:sourceResourcesWithPrefixIn
-- @+node:gcross.20091201183231.1577:createSourceResourceFor
createSourceResourceFor :: String -> FilePath -> Resource
createSourceResourceFor prefix filepath =
    let (name, extension) = splitExtension . takeFileName $ filepath
        resource = Resource
            {   resourceName = applyPrefix prefix name
            ,   resourceType = if extension == "" then "" else tail extension
            ,   resourceFilePath = filepath
            ,   resourceDigest = Right . digestOf $ filepath
            ,   resourceLinkDependencies = notLinkable resource
            }
    in resource
-- @-node:gcross.20091201183231.1577:createSourceResourceFor
-- @-node:gcross.20091201183231.1569:Turning sources into resources
-- @+node:gcross.20091201183231.1578:Fetching with the possibility of errors
-- @+node:gcross.20091201183231.1580:attemptGetDigests
attemptGetDigests :: [Resource] -> Either ErrorMessage [MD5Digest]
attemptGetDigests =
    gatherResultsOrError
    .
    parMap rwhnf resourceDigest
-- @-node:gcross.20091201183231.1580:attemptGetDigests
-- @+node:gcross.20091201183231.1582:attemptGetResourceDigests
attemptGetResourceDigests :: String -> Resources -> [ResourceId] -> Either ErrorMessage [MD5Digest]
attemptGetResourceDigests error_message_heading resources =
    attemptGetResources error_message_heading resources
    >=>
    attemptGetDigests
-- @-node:gcross.20091201183231.1582:attemptGetResourceDigests
-- @+node:gcross.20091201183231.1584:attemptGetResources
attemptGetResources :: String -> Resources -> [ResourceId] -> Either ErrorMessage [Resource]
attemptGetResources error_heading resources =
    mapLeft (
        errorMessage error_heading
        .
        (text "Unable to resolve the following resource identifiers:" <$$>)
        .
        indent 4
    )
    .
    gatherResultsOrError
    .
    map (\resource_id ->
        case Map.lookup resource_id resources of
            Nothing -> Left . text . show $ resource_id
            Just resource -> Right resource
    )
-- @-node:gcross.20091201183231.1584:attemptGetResources
-- @+node:gcross.20091214124713.1702:assertResourceExists
assertResourceExists :: Resource -> ErrorMessageOr Resource
assertResourceExists resource = resourceDigest resource >> return resource
-- @-node:gcross.20091214124713.1702:assertResourceExists
-- @-node:gcross.20091201183231.1578:Fetching with the possibility of errors
-- @+node:gcross.20091121210308.1280:Unclassified
-- @+node:gcross.20091201183231.1598:selectResourcesOfType
selectResourcesOfType desired_type = Map.filter ((== desired_type) . resourceType)
-- @-node:gcross.20091201183231.1598:selectResourcesOfType
-- @+node:gcross.20091201183231.1599:selectResourcesOfTypeAsList
selectResourcesOfTypeAsList desired_type = Map.elems . selectResourcesOfType desired_type
-- @-node:gcross.20091201183231.1599:selectResourcesOfTypeAsList
-- @+node:gcross.20091214124713.1703:selectResourcesInSubdirectoryAsList
selectResourcesInSubdirectoryAsList :: FilePath -> Resources -> [Resource]
selectResourcesInSubdirectoryAsList subdirectory =
    filter ((== subdirectory) . take (length subdirectory) . resourceFilePath)
    .
    Map.elems
-- @-node:gcross.20091214124713.1703:selectResourcesInSubdirectoryAsList
-- @+node:gcross.20091121210308.1285:digestOf
digestOf :: FilePath -> MD5Digest
digestOf = md5 . unsafePerformIO . L.readFile
-- @-node:gcross.20091121210308.1285:digestOf
-- @+node:gcross.20091121210308.2039:addResource
addResource :: Resource -> Resources -> Resources
addResource resource = Map.insert ((resourceName &&& resourceType) resource) resource
-- @-node:gcross.20091121210308.2039:addResource
-- @+node:gcross.20091201161628.1575:addResources
addResources :: Resources -> Resources -> Resources
addResources = Map.union
-- @-node:gcross.20091201161628.1575:addResources
-- @+node:gcross.20091121210308.1293:applyPrefix
applyPrefix :: String -> String -> String
applyPrefix "" = id
applyPrefix prefix = (prefix ++) . ('.':)
-- @-node:gcross.20091121210308.1293:applyPrefix
-- @+node:gcross.20091121210308.2033:getFilePathForNameAndType
getFilePathForNameAndType :: FilePath -> String -> String -> FilePath
getFilePathForNameAndType directory name typ =
    let partial_path = (joinPath (directory:splitDot name))
    in if null typ then partial_path else partial_path <.> typ
-- @-node:gcross.20091121210308.2033:getFilePathForNameAndType
-- @+node:gcross.20091122100142.1388:resourceId
resourceId :: Resource -> ResourceId
resourceId = (resourceName &&& resourceType)
-- @-node:gcross.20091122100142.1388:resourceId
-- @+node:gcross.20091121210308.1288:toResources
toResources :: [Resource] -> Resources
toResources =
    Map.fromList
    .
    map ((resourceName &&& resourceType) &&& id)
-- @-node:gcross.20091121210308.1288:toResources
-- @+node:gcross.20091201161628.1579:findAllObjectDependenciesOf
findAllObjectDependenciesOf :: Resources -> ResourceSet -> Either ErrorMessage ResourceSet
findAllObjectDependenciesOf known_resources = go Set.empty
  where
    go accum resources_to_scan =
        case Set.minView resources_to_scan of
            Nothing -> return accum
            Just (resource,rest_resources) ->
                resourceLinkDependencies resource
                >>=
                \additional_resources ->
                    let new_accum = resource `Set.insert` accum
                        culled_additional_resources = additional_resources `Set.difference` new_accum
                    in go new_accum (rest_resources `Set.union` culled_additional_resources)
-- @-node:gcross.20091201161628.1579:findAllObjectDependenciesOf
-- @+node:gcross.20091201183231.1589:notLinkable
notLinkable :: Resource -> Either ErrorMessage (Set Resource)
notLinkable resource = Left $
    errorMessageText ("linking resource with id " ++ (show . resourceId) resource) ("Resource type '" ++ resourceType resource ++ "' is not linkable.")
-- @-node:gcross.20091201183231.1589:notLinkable
-- @+node:gcross.20091201183231.1595:noLinkDependencies
noLinkDependencies :: Either ErrorMessage ResourceSet
noLinkDependencies = Right Set.empty
-- @nonl
-- @-node:gcross.20091201183231.1595:noLinkDependencies
-- @+node:gcross.20091204093401.2772:addSourceResourceFor
addSourceResourceFor :: FilePath -> Resources -> Resources
addSourceResourceFor = addPrefixedSourceResourceFor ""
-- @-node:gcross.20091204093401.2772:addSourceResourceFor
-- @+node:gcross.20091204093401.2774:addPrefixedSourceResourceFor
addPrefixedSourceResourceFor :: String -> FilePath -> Resources -> Resources
addPrefixedSourceResourceFor prefix = addResource . createSourceResourceFor prefix
-- @-node:gcross.20091204093401.2774:addPrefixedSourceResourceFor
-- @-node:gcross.20091121210308.1280:Unclassified
-- @-node:gcross.20091201183231.1586:Functions
-- @-others
-- @-node:gcross.20091121210308.1276:@thin Resources.hs
-- @-leo
