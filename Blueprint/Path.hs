-- @+leo-ver=4-thin
-- @+node:gcross.20100630111926.2023:@thin Path.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100630111926.2024:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100630111926.2024:<< Language extensions >>
-- @nl

module Blueprint.Path where

-- @<< Import needed modules >>
-- @+node:gcross.20100630111926.2025:<< Import needed modules >>
import Control.Arrow

import Data.List (intercalate)
import Data.List.Split

import System.Directory
import System.Environment.FindBin
import System.FilePath
import System.FilePath.Find
-- @-node:gcross.20100630111926.2025:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100630111926.2028:Types
-- @+node:gcross.20100630111926.2029:Path
type Path = [String]
-- @-node:gcross.20100630111926.2029:Path
-- @-node:gcross.20100630111926.2028:Types
-- @+node:gcross.20100630111926.2026:Functions
-- @+node:gcross.20100630111926.2027:changeToProgramDirectory
changeToProgramDirectory :: IO ()
changeToProgramDirectory = getProgPath >>= setCurrentDirectory
-- @-node:gcross.20100630111926.2027:changeToProgramDirectory
-- @+node:gcross.20100630111926.2038:findAllPaths
findAllPaths :: FilterPredicate → FilePath → IO [(FilePath,Path)]
findAllPaths filter_predicate = fmap (map (id &&& filePathToPath)) . find always filter_predicate
-- @-node:gcross.20100630111926.2038:findAllPaths
-- @+node:gcross.20100630111926.2040:findAllPathsWithExtensions
findAllPathsWithExtensions :: [String] → FilePath → IO [(FilePath,Path)]
findAllPathsWithExtensions = findAllPaths . flip fmap extension . flip elem
-- @-node:gcross.20100630111926.2040:findAllPathsWithExtensions
-- @+node:gcross.20100630111926.2036:paths <--> dots
-- @+node:gcross.20100630111926.2030:dotsToPath
dotsToPath :: String → Path
dotsToPath = splitOn "."
-- @-node:gcross.20100630111926.2030:dotsToPath
-- @+node:gcross.20100630111926.2032:pathToDots
pathToDots :: Path → String
pathToDots = intercalate "."
-- @-node:gcross.20100630111926.2032:pathToDots
-- @-node:gcross.20100630111926.2036:paths <--> dots
-- @+node:gcross.20100630111926.2037:paths <--> file paths
-- @+node:gcross.20100630111926.2033:pathToFilePath
pathToFilePath :: Path → FilePath
pathToFilePath = joinPath
-- @-node:gcross.20100630111926.2033:pathToFilePath
-- @+node:gcross.20100630111926.2046:pathToFilePathInSubdirectory
pathToFilePathInSubdirectory :: FilePath → Path → FilePath
pathToFilePathInSubdirectory subdirectory = (</> subdirectory) . pathToFilePath
-- @-node:gcross.20100630111926.2046:pathToFilePathInSubdirectory
-- @+node:gcross.20100630111926.2044:pathToFilePathWithExtension
pathToFilePathWithExtension extension = (<.> extension) . pathToFilePath
-- @-node:gcross.20100630111926.2044:pathToFilePathWithExtension
-- @+node:gcross.20100630111926.2048:pathToFilePathInSubdirectoryWithExtension
pathToFilePathInSubdirectoryWithExtension extension subdirectory =
    (<.> extension)
    .
    pathToFilePathInSubdirectory subdirectory
-- @-node:gcross.20100630111926.2048:pathToFilePathInSubdirectoryWithExtension
-- @+node:gcross.20100630111926.2035:filePathToPath
filePathToPath :: FilePath → Path
filePathToPath = splitDirectories . dropExtension
-- @-node:gcross.20100630111926.2035:filePathToPath
-- @-node:gcross.20100630111926.2037:paths <--> file paths
-- @-node:gcross.20100630111926.2026:Functions
-- @-others
-- @-node:gcross.20100630111926.2023:@thin Path.hs
-- @-leo
