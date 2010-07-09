-- @+leo-ver=4-thin
-- @+node:gcross.20100630111926.2023:@thin SourceFile.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100630111926.2024:<< Language extensions >>
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100630111926.2024:<< Language extensions >>
-- @nl

module Blueprint.SourceFile where

-- @<< Import needed modules >>
-- @+node:gcross.20100630111926.2025:<< Import needed modules >>
import Control.Monad.Trans.Class
import Control.Monad.Trans.List

import qualified Data.Foldable as Fold
import Data.List
import Data.Record
import Data.Sequence (Seq,(><),(|>))
import qualified Data.Sequence as Seq
import Data.UUID
import Data.UUID.V5

import System.Directory
import System.FilePath

import Blueprint.Identifier
import Blueprint.Jobs
-- @-node:gcross.20100630111926.2025:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100630111926.2028:Types
-- @+node:gcross.20100708102250.2000:HierarchalPath
type HierarchalPath = Seq String
-- @-node:gcross.20100708102250.2000:HierarchalPath
-- @+node:gcross.20100708102250.1996:SourceFile
data SourceFile = SourceFile
    {   sourceFilePath :: FilePath
    ,   sourceFileHierarchalPath :: HierarchalPath
    ,   sourceFileExtension :: String
    ,   sourceFileDigestJobId :: JobId
    }
-- @-node:gcross.20100708102250.1996:SourceFile
-- @-node:gcross.20100630111926.2028:Types
-- @+node:gcross.20100708102250.1997:Functions
-- @+node:gcross.20100708102250.1998:getAllSourceFilesIn
getAllSourceFilesIn :: FilePath → IO [SourceFile]
getAllSourceFilesIn = runListT . go Seq.empty
  where
    go :: Seq String → FilePath → ListT IO SourceFile
    go parent directory_path = do
        item_path ← ListT . getDirectoryContents $ directory_path
        let hierarchal_path = parent |> (takeBaseName item_path)
        is_directory ← lift (doesDirectoryExist item_path)
        if is_directory
            then go hierarchal_path item_path
            else return $
                    SourceFile
                    {   sourceFilePath = item_path
                    ,   sourceFileHierarchalPath = hierarchal_path
                    ,   sourceFileExtension = takeExtensions item_path
                    ,   sourceFileDigestJobId = computeJobIdOfSourceFileDigest item_path
                    }
-- @-node:gcross.20100708102250.1998:getAllSourceFilesIn
-- @+node:gcross.20100708102250.2001:hierarchalPathToDots
hierarchalPathToDots :: HierarchalPath → String
hierarchalPathToDots = intercalate "." . Fold.toList
-- @-node:gcross.20100708102250.2001:hierarchalPathToDots
-- @+node:gcross.20100708215239.2087:hierarchalPathToFilePath
hierarchalPathToFilePath :: HierarchalPath → String
hierarchalPathToFilePath = joinPath . Fold.toList
-- @-node:gcross.20100708215239.2087:hierarchalPathToFilePath
-- @+node:gcross.20100708102250.1999:prependParentToHierarchalPath
prependParentToHierarchalPath :: Seq String → SourceFile → SourceFile
prependParentToHierarchalPath parent source_file@SourceFile{sourceFileHierarchalPath} =
    source_file
    {   sourceFileHierarchalPath = parent >< sourceFileHierarchalPath
    }
-- @-node:gcross.20100708102250.1999:prependParentToHierarchalPath
-- @+node:gcross.20100708102250.2004:computeJobIdOfSourceFileDigest
computeJobIdOfSourceFileDigest :: FilePath → JobId
computeJobIdOfSourceFileDigest file_path =
    identifierInNamespace
        digest_source_file_namespace
        file_path
        ("Digest " ++ file_path)
-- @-node:gcross.20100708102250.2004:computeJobIdOfSourceFileDigest
-- @-node:gcross.20100708102250.1997:Functions
-- @+node:gcross.20100708102250.2002:Values
-- @+node:gcross.20100708102250.2003:digest_source_file_namespace
digest_source_file_namespace = uuid "d4135617-a282-471e-83c3-33a64c7bd29f"
-- @-node:gcross.20100708102250.2003:digest_source_file_namespace
-- @-node:gcross.20100708102250.2002:Values
-- @-others
-- @-node:gcross.20100630111926.2023:@thin SourceFile.hs
-- @-leo
