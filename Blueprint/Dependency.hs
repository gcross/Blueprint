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
import Data.DeriveTH
import Data.Digest.Pure.MD5
import Data.Either
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Blueprint.Record
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable

import Language.Haskell.TH

import Blueprint.Identifier
import Blueprint.Jobs
import Blueprint.Miscellaneous
-- @nonl
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
-- @+node:gcross.20100624100717.1741:RequiredDependencies
data RequiredDependencies = RequiredDependencies
    {   requiredImmediateDependencies :: [(Dependency, Maybe JobId)]
    ,   requiredDeferredDependencies :: [Dependency]
    } deriving Show
-- @-node:gcross.20100624100717.1741:RequiredDependencies
-- @+node:gcross.20100624100717.2064:UnknownDependency
data UnknownDependency = UnknownDependency
    {   unknownDependency :: Dependency
    ,   unknownDependencyExporters :: Maybe DependencyExporters
    } deriving (Show,Eq,Typeable)

-- @-node:gcross.20100624100717.2064:UnknownDependency
-- @+node:gcross.20100902134026.2111:DependencyResolution
type DependencyResolution = Either UnknownDependency RequiredDependencies
-- @-node:gcross.20100902134026.2111:DependencyResolution
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
-- @+node:gcross.20100902134026.2106:Monoid RequiredDependencies
instance Monoid RequiredDependencies where
    mempty = RequiredDependencies [] []
    x1 `mappend` x2 =
        RequiredDependencies
            (nub (requiredImmediateDependencies x1 ++ requiredImmediateDependencies x2))
            (nub (requiredDeferredDependencies x1 ++ requiredDeferredDependencies x2))
    mconcat =
        liftA2 RequiredDependencies
            (Set.toList . Set.unions . map (Set.fromList . requiredImmediateDependencies))
            (Set.toList . Set.unions . map (Set.fromList . requiredDeferredDependencies))
-- @-node:gcross.20100902134026.2106:Monoid RequiredDependencies
-- @-node:gcross.20100624100717.2149:Instances
-- @+node:gcross.20100624100717.2066:Functions
-- @+node:gcross.20100906112631.2092:createDependencyDeclarations
createDependencyDeclarations :: String → String → Q [Dec]
createDependencyDeclarations uuid_as_string name = do
    let dependency_type = wordsToUnderscores name ++ "_dependency_type"
        dependency_type_name = mkName dependency_type
        dependency_type_name_var = return . VarE $ dependency_type_name
        name_lit = return . LitE . StringL $ name
        dependency_wrapper_fn_name = mkName (wordsToCamelCase name ++ "Dependency")
    dependency_type_qexp ← [|identifier uuid_as_string $(name_lit)|]
    dependency_wrapper_fn_qexp ← [|Dependency $(dependency_type_name_var)|]
    return
        [ValD (VarP dependency_type_name) (NormalB dependency_type_qexp) []
        ,ValD (VarP dependency_wrapper_fn_name) (NormalB dependency_wrapper_fn_qexp) []
        ]
-- @-node:gcross.20100906112631.2092:createDependencyDeclarations
-- @+node:gcross.20100902134026.2101:classifyDependenciesAndRejectUnrecognizedTypes
classifyDependenciesAndRejectUnrecognizedTypes :: String → [DependencyType] → [Dependency] → [[String]]
classifyDependenciesAndRejectUnrecognizedTypes actor_name recognized_dependency_types =
    extractRecognizedDependencesOrError actor_name recognized_dependency_types
    .
    map (dependencyType &&& dependencyName)
-- @-node:gcross.20100902134026.2101:classifyDependenciesAndRejectUnrecognizedTypes
-- @+node:gcross.20100902134026.2118:classifyTaggedDependenciesAndRejectUnrecognizedTypes
classifyTaggedDependenciesAndRejectUnrecognizedTypes :: String → [DependencyType] → [(Dependency,a)] → [[(String,a)]]
classifyTaggedDependenciesAndRejectUnrecognizedTypes actor_name recognized_dependency_types =
    extractRecognizedDependencesOrError actor_name recognized_dependency_types
    .
    map (\(Dependency{..},x) → ((dependencyType,(dependencyName,x))))
-- @-node:gcross.20100902134026.2118:classifyTaggedDependenciesAndRejectUnrecognizedTypes
-- @+node:gcross.20100902134026.2104:extractRecognizedDependencesOrError
extractRecognizedDependencesOrError :: String → [DependencyType] → [(DependencyType,a)] → [[a]]
extractRecognizedDependencesOrError actor_name recognized_dependency_types dependencies
  | (not . null) unrecognized_dependency_types =
        throw
        $
        UnrecognizedDependencyTypes
            actor_name
            unrecognized_dependency_types
  | otherwise =
        map (fromMaybe [] . flip lookup classified_dependencies) recognized_dependency_types
  where
    classified_dependencies = gather dependencies
    unrecognized_dependency_types = (\\ recognized_dependency_types) . map fst $ classified_dependencies
-- @-node:gcross.20100902134026.2104:extractRecognizedDependencesOrError
-- @+node:gcross.20100624100717.2067:extractRequiredDependenciesOrError
extractRequiredDependenciesOrError :: [DependencyResolution] → RequiredDependencies
extractRequiredDependenciesOrError resolutions =
    case partitionEithers resolutions of
        ([],successful_resolutions) → mconcat successful_resolutions
        (failed_resolutions,_) → throw $ UnknownDependenciesError failed_resolutions
-- @-node:gcross.20100624100717.2067:extractRequiredDependenciesOrError
-- @-node:gcross.20100624100717.2066:Functions
-- @-others
-- @-node:gcross.20100624100717.1713:@thin Dependency.hs
-- @-leo
