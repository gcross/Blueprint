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
    } deriving (Show,Eq,Typeable); $(derive makeBinary ''Dependency)
-- @-node:gcross.20100624100717.1721:Dependency
-- @+node:gcross.20100624100717.1726:DependencyExporters
data DependencyExporters = DependencyExporters
    {   dependencyExporterType :: String
    ,   dependencyExporterNames :: [String]
    } deriving (Eq)
-- @-node:gcross.20100624100717.1726:DependencyExporters
-- @+node:gcross.20100624100717.1741:ResolvedDependencies
data ResolvedDependencies = ResolvedDependencies
    {   resolvedImmediateDependencies :: [JobId]
    ,   resolvedDeferredDependencies :: [Dependency]
    }
-- @-node:gcross.20100624100717.1741:ResolvedDependencies
-- @+node:gcross.20100624100717.1727:UnresolvedDependency
data UnresolvedDependency = UnresolvedDependency
    {   unresolvedDependencyIsExternal :: Maybe Bool
    ,   unresolvedDependency :: Dependency
    } deriving (Show,Eq);  $( derive makeBinary ''UnresolvedDependency )
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
type DependencyResolver = UnresolvedDependency → DependencyResolution
-- @-node:gcross.20100624100717.1725:DependencyResolver
-- @-node:gcross.20100624100717.1720:Types
-- @+node:gcross.20100624100717.2063:Exceptions
-- @+node:gcross.20100624100717.2065:UnknownDependenciesError
data UnknownDependenciesError = UnknownDependenciesError [UnknownDependency] deriving (Typeable)

instance Show UnknownDependenciesError where
    show (UnknownDependenciesError unknown_dependencies) =
        concat
        .
        map (\(UnknownDependency (Dependency{..}) maybe_unknown_dependency_exporters) →
            "Unable to find " ++ show dependencyType ++ " '" ++ dependencyName ++ "'" ++
            case maybe_unknown_dependency_exporters of
                Nothing → []
                Just exporters → " (but it is exported by the " ++ show exporters ++ ")"
        )
        $
        unknown_dependencies


instance Exception UnknownDependenciesError
-- @-node:gcross.20100624100717.2065:UnknownDependenciesError
-- @-node:gcross.20100624100717.2063:Exceptions
-- @+node:gcross.20100624100717.2149:Instances
-- @+node:gcross.20100624100717.2150:Show DependencyExporters
instance Show DependencyExporters where
    show (DependencyExporters{..}) = dependencyExporterType ++ "(s) " ++ show dependencyExporterNames
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
-- @+node:gcross.20100624100717.2073:resolveAllDependenciesUsing
resolveAndExtractAndConcatenateDependenciesUsing :: DependencyResolver → [UnresolvedDependency] → ResolvedDependencies
resolveAndExtractAndConcatenateDependenciesUsing resolveDependency =
    concatResolvedDependencies
    .
    extractDependenciesOrError
    .
    map resolveDependency
-- @-node:gcross.20100624100717.2073:resolveAllDependenciesUsing
-- @+node:gcross.20100628115452.1863:binDependencies
binDependencies = bin . map (liftA2 (,) dependencyType dependencyName)
-- @-node:gcross.20100628115452.1863:binDependencies
-- @-node:gcross.20100624100717.2066:Functions
-- @-others
-- @-node:gcross.20100624100717.1713:@thin Dependency.hs
-- @-leo
