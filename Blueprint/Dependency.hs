-- @+leo-ver=4-thin
-- @+node:gcross.20100624100717.1713:@thin Dependency.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100624100717.1714:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100624100717.1714:<< Language extensions >>
-- @nl

module Blueprint.Dependency where

-- @<< Import needed modules >>
-- @+node:gcross.20100624100717.1715:<< Import needed modules >>
import Control.Applicative
import Control.Arrow
import Control.Exception

import Data.Binary
import Data.Either
import Data.DeriveTH
import Data.Digest.Pure.MD5
import Data.List
import Data.Record
import Data.Typeable

import Blueprint.Identifier
import Blueprint.Jobs
-- @-node:gcross.20100624100717.1715:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100624100717.1720:Types
-- @+node:gcross.20100624100717.1740:DependencyType
data OfDependencyType
type DependencyType = Identifier OfDependencyType
-- @-node:gcross.20100624100717.1740:DependencyType
-- @+node:gcross.20100624100717.1721:Dependency
data Dependency = Dependency
    {   dependencyType :: DependencyType
    ,   dependencyName :: String
    } deriving (Show,Eq,Ord,Typeable); $(derive makeBinary ''Dependency)
-- @-node:gcross.20100624100717.1721:Dependency
-- @+node:gcross.20100624100717.1726:DependencyExporters
data DependencyExporters = DependencyExporters
    {   dependencyExporterType :: DependencyType
    ,   dependencyExporterNames :: [String]
    } deriving (Eq)
-- @-node:gcross.20100624100717.1726:DependencyExporters
-- @+node:gcross.20100624100717.1741:ResolvedDependencies
data ResolvedDependencies = ResolvedDependencies
    {   resolvedImmediateDependencies :: [JobId]
    ,   resolvedDeferredDependencies :: [Dependency]
    } deriving Show
-- @-node:gcross.20100624100717.1741:ResolvedDependencies
-- @+node:gcross.20100624100717.1727:UnresolvedDependency
data UnresolvedDependency = UnresolvedDependency
    {   unresolvedDependencyIsExternal :: Maybe Bool
    ,   unresolvedDependency :: Dependency
    } deriving (Show,Eq,Typeable);  $( derive makeBinary ''UnresolvedDependency )
-- @-node:gcross.20100624100717.1727:UnresolvedDependency
-- @+node:gcross.20100624100717.2064:UnknownDependency
data UnknownDependency = UnknownDependency
    {   unknownDependency :: Dependency
    ,   unknownDependencyExporters :: Maybe DependencyExporters
    } deriving (Show,Eq,Typeable)

-- @-node:gcross.20100624100717.2064:UnknownDependency
-- @+node:gcross.20100624100717.1737:DependencyResolution
type DependencyResolution = Either UnknownDependency ResolvedDependencies
-- @-node:gcross.20100624100717.1737:DependencyResolution
-- @+node:gcross.20100624100717.1725:DependencyResolver
type DependencyResolver = UnresolvedDependency → JobTask JobId Record DependencyResolution
-- @-node:gcross.20100624100717.1725:DependencyResolver
-- @-node:gcross.20100624100717.1720:Types
-- @+node:gcross.20100624100717.2063:Exceptions
-- @+node:gcross.20100624100717.2065:UnknownDependenciesError
data UnknownDependenciesError = UnknownDependenciesError [UnknownDependency] deriving (Typeable)

instance Show UnknownDependenciesError where
    show (UnknownDependenciesError unknown_dependencies) =
        intercalate "\n"
        .
        nub
        .
        map (\(UnknownDependency (Dependency{..}) maybe_unknown_dependency_exporters) →
            "Unable to find " ++ show dependencyType ++ " '" ++ dependencyName ++ "'" ++
            case maybe_unknown_dependency_exporters of
                Nothing → []
                Just (DependencyExporters _ []) → []
                Just exporters → " (but it is exported by the " ++ show exporters ++ ")"
        )
        $
        unknown_dependencies


instance Exception UnknownDependenciesError
-- @-node:gcross.20100624100717.2065:UnknownDependenciesError
-- @+node:gcross.20100630111926.1866:UnrecognizedDependencyType
data UnrecognizedDependencyType = UnrecognizedDependencyType String DependencyType deriving Typeable

instance Show UnrecognizedDependencyType where
    show (UnrecognizedDependencyType actor dependency_type) =
        actor ++ " does not recognize the dependency type " ++ show dependency_type

instance Exception UnrecognizedDependencyType
-- @-node:gcross.20100630111926.1866:UnrecognizedDependencyType
-- @+node:gcross.20100628115452.1902:UnrecognizedDependencyTypes
data UnrecognizedDependencyTypes = UnrecognizedDependencyTypes String [DependencyType] deriving Typeable

instance Show UnrecognizedDependencyTypes where
    show (UnrecognizedDependencyTypes actor dependency_types) =
        actor ++ " does not recognize the following types of dependencies: " ++ show dependency_types

instance Exception UnrecognizedDependencyTypes
-- @-node:gcross.20100628115452.1902:UnrecognizedDependencyTypes
-- @-node:gcross.20100624100717.2063:Exceptions
-- @+node:gcross.20100624100717.2149:Instances
-- @+node:gcross.20100624100717.2150:Show DependencyExporters
instance Show DependencyExporters where
    show (DependencyExporters{..}) = show dependencyExporterType ++ "(s) " ++ show dependencyExporterNames
-- @nonl
-- @-node:gcross.20100624100717.2150:Show DependencyExporters
-- @-node:gcross.20100624100717.2149:Instances
-- @+node:gcross.20100624100717.2066:Functions
-- @+node:gcross.20100624100717.2067:extractDependenciesOrError
extractDependenciesOrError :: [DependencyResolution] → [ResolvedDependencies]
extractDependenciesOrError resolutions =
    case partitionEithers resolutions of
        ([],successful_resolutions) → successful_resolutions
        (failed_resolutions,_) → throw $ UnknownDependenciesError failed_resolutions
-- @-node:gcross.20100624100717.2067:extractDependenciesOrError
-- @+node:gcross.20100624100717.2068:concatResolvedDependencies
concatResolvedDependencies :: [ResolvedDependencies] → ResolvedDependencies
concatResolvedDependencies =
    uncurry ResolvedDependencies
    .
    ((nub . concat) *** (nub . concat))
    .
    unzip
    .
    map (\(ResolvedDependencies a b) → (a,b))
-- @-node:gcross.20100624100717.2068:concatResolvedDependencies
-- @+node:gcross.20100624100717.2073:extractAndConcatenateDependencies
extractAndConcatenateDependencies :: [DependencyResolution] → ResolvedDependencies
extractAndConcatenateDependencies =
    concatResolvedDependencies
    .
    extractDependenciesOrError
-- @-node:gcross.20100624100717.2073:extractAndConcatenateDependencies
-- @+node:gcross.20100705150931.1950:separateDependenciesByType
separateDependenciesByType :: String → [DependencyType] → [Dependency] → [[String]]
separateDependenciesByType actor_name = go
  where
    go [] [] = []
    go [] unrecognized_dependencies =
        throw
        .
        UnrecognizedDependencyTypes actor_name
        .
        nub
        .
        map dependencyType 
        $
        unrecognized_dependencies
    go (dependency_type:rest_dependency_types) dependencies =
        uncurry (:)
        .
        (map dependencyName *** go rest_dependency_types)
        .
        partition (
            (== dependency_type)
            .
            dependencyType
        )
        $
        dependencies
-- @-node:gcross.20100705150931.1950:separateDependenciesByType
-- @-node:gcross.20100624100717.2066:Functions
-- @-others
-- @-node:gcross.20100624100717.1713:@thin Dependency.hs
-- @-leo
