-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1682:@thin Haskell.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1683:<< Language extensions >>
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1683:<< Language extensions >>
-- @nl

module Blueprint.Language.Programming.Haskell where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1684:<< Import needed modules >>
import Data.Array ((!))
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lazy.Char8 (unpack)

import Text.Regex.PCRE
import Text.Regex.PCRE.String

import Blueprint.Dependency
import Blueprint.Identifier
import Blueprint.Jobs
import Blueprint.Language
import Blueprint.Language.Programming
import Blueprint.Record
import Blueprint.SourceFile
-- @-node:gcross.20100611224425.1684:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100615082419.1704:Languages
-- @+node:gcross.20100615082419.1707:Haskell
data Haskell

instance Language Haskell where
    languageUUID _ = uuid "5fb30321-bfcd-488e-b798-6c000a22b47f"
    languageName _ = "Haskell"
    languageFileExtensions _ = ["hs"]

instance ProgrammingLanguage Haskell where
    languageHelloWorldScript _ = scriptFromLines $ ["main = putStrLn \"Hello, world!\""]
-- @nonl
-- @-node:gcross.20100615082419.1707:Haskell
-- @-node:gcross.20100615082419.1704:Languages
-- @+node:gcross.20100630111926.2042:Types
-- @+node:gcross.20100630111926.2043:HaskellSource
data HaskellSource = HaskellSource
    {   haskellSourceFilePath :: FilePath
    ,   haskellSourceHierarchalPath :: HierarchalPath
    ,   haskellSourceDigestJobId :: JobId
    ,   haskellSourceModuleName :: String
    }
-- @-node:gcross.20100630111926.2043:HaskellSource
-- @+node:gcross.20100708192404.2002:HaskellSources
type HaskellSources = [HaskellSource]
-- @-node:gcross.20100708192404.2002:HaskellSources
-- @-node:gcross.20100630111926.2042:Types
-- @+node:gcross.20100709210816.2101:Instances
-- @+node:gcross.20100709210816.2102:Show HaskellSource
instance Show HaskellSource where
    show HaskellSource{..} =
        haskellSourceModuleName ++ " @ " ++ haskellSourceFilePath
-- @-node:gcross.20100709210816.2102:Show HaskellSource
-- @-node:gcross.20100709210816.2101:Instances
-- @+node:gcross.20100615082419.1705:Functions
-- @+node:gcross.20100615082419.1706:extractImportedModulesFromHaskellSource
extractImportedModulesFromHaskellSource :: L.ByteString → [String]
extractImportedModulesFromHaskellSource =
    map (
        unpack
        .
        fst
        .
        (! 1)
    )
    .
    matchAllText import_regex
-- @-node:gcross.20100615082419.1706:extractImportedModulesFromHaskellSource
-- @+node:gcross.20100630111926.2041:extractHaskellSources
extractHaskellSources :: [SourceFile] → [HaskellSource]
extractHaskellSources source_files =
    [ HaskellSource
      {   haskellSourceFilePath = sourceFilePath
      ,   haskellSourceHierarchalPath = sourceFileHierarchalPath
      ,   haskellSourceDigestJobId = sourceFileDigestJobId
      ,   haskellSourceModuleName = hierarchalPathToDots sourceFileHierarchalPath
      }
    | SourceFile{..} ← source_files
    , sourceFileExtension == ".hs"
    ]
-- @-node:gcross.20100630111926.2041:extractHaskellSources
-- @+node:gcross.20100902134026.2108:haskellInterfaceDependency
haskellInterfaceDependency :: String → Dependency
haskellInterfaceDependency = Dependency haskell_interface_dependency_type
-- @-node:gcross.20100902134026.2108:haskellInterfaceDependency
-- @+node:gcross.20100902134026.2110:haskellModuleDependency
haskellModuleDependency :: String → Dependency
haskellModuleDependency = Dependency haskell_module_dependency_type
-- @-node:gcross.20100902134026.2110:haskellModuleDependency
-- @+node:gcross.20100709210816.2211:haskellPackageDependency
haskellPackageDependency :: String → Dependency
haskellPackageDependency = Dependency haskell_package_dependency_type
-- @-node:gcross.20100709210816.2211:haskellPackageDependency
-- @-node:gcross.20100615082419.1705:Functions
-- @+node:gcross.20100611224425.1689:Values
-- @+node:gcross.20100611224425.1708:regular expression
import_regex :: Regex
import_regex = makeRegex "^\\s*import\\s+(?:qualified\\s+)?([A-Z][A-Za-z0-9_.]*)"
-- @-node:gcross.20100611224425.1708:regular expression
-- @-node:gcross.20100611224425.1689:Values
-- @+node:gcross.20100630111926.1880:Dependency Types
haskell_module_dependency_type = identifier "450299f0-5957-4e05-a185-88d765a032b8" "haskell module"
haskell_package_dependency_type = identifier "b0094d7f-1cf0-4cbf-8938-e40bf38a6e81" "haskell package"
haskell_interface_dependency_type = identifier "17047d2b-e8e2-4220-bd83-d61cda2fcbdc" "haskell interface file"
-- @-node:gcross.20100630111926.1880:Dependency Types
-- @-others
-- @-node:gcross.20100611224425.1682:@thin Haskell.hs
-- @-leo
