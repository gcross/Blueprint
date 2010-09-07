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
import Blueprint.Miscellaneous
import Blueprint.Record
import Blueprint.SourceFile
-- @-node:gcross.20100611224425.1684:<< Import needed modules >>
-- @nl

-- @+others
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
-- @-node:gcross.20100615082419.1705:Functions
-- @+node:gcross.20100611224425.1689:Values
-- @+node:gcross.20100611224425.1708:regular expression
import_regex :: Regex
import_regex = makeRegex "^\\s*import\\s+(?:qualified\\s+)?([A-Z][A-Za-z0-9_.]*)"
-- @-node:gcross.20100611224425.1708:regular expression
-- @-node:gcross.20100611224425.1689:Values
-- @+node:gcross.20100630111926.1880:Dependency Types
createDependencyDeclarations "450299f0-5957-4e05-a185-88d765a032b8" "module"
-- @-node:gcross.20100630111926.1880:Dependency Types
-- @-others
-- @-node:gcross.20100611224425.1682:@thin Haskell.hs
-- @-leo
