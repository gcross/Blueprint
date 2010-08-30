-- @+leo-ver=4-thin
-- @+node:gcross.20100830091258.2004:@thin Tools.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100830091258.2005:<< Language extensions >>
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100830091258.2005:<< Language extensions >>
-- @nl

module Blueprint.Configuration.Tools where

-- @<< Import needed modules >>
-- @+node:gcross.20100830091258.2006:<< Import needed modules >>
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe
import Data.Version

import System.Directory
import System.Environment
import System.FilePath
import System.Process

import Text.Regex.Base

import Blueprint.Miscellaneous
-- @nonl
-- @-node:gcross.20100830091258.2006:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100830091258.2017:Types
-- @+node:gcross.20100830091258.2030:VersionExtractor
data VersionExtractor = VersionExtractor
    {   versionExtractorOptions :: [String]
    ,   versionExtractorParseVersion :: (String → Maybe Version)
    }
-- @-node:gcross.20100830091258.2030:VersionExtractor
-- @+node:gcross.20100830091258.2018:VersionedProgram
data VersionedProgram = VersionedProgram
    {   versionedProgramName :: String
    ,   versionedProgramVersionExtractor :: VersionExtractor
    }
-- @-node:gcross.20100830091258.2018:VersionedProgram
-- @-node:gcross.20100830091258.2017:Types
-- @+node:gcross.20100830091258.2007:Functions
-- @+node:gcross.20100830091258.2014:getEnvironmentPath
getEnvironmentPath :: IO [FilePath]
getEnvironmentPath = fmap splitSearchPath (getEnv "PATH")

-- @-node:gcross.20100830091258.2014:getEnvironmentPath
-- @+node:gcross.20100830091258.2010:getProgramVersion
getProgramVersion :: VersionExtractor → FilePath → IO (Maybe Version)
getProgramVersion VersionExtractor{..} program_path =
    fmap versionExtractorParseVersion (readProcess program_path versionExtractorOptions "")
-- @-node:gcross.20100830091258.2010:getProgramVersion
-- @+node:gcross.20100830091258.2011:lookForProgramInPaths
lookForProgramInPaths :: String → [FilePath] → IO [FilePath]
lookForProgramInPaths program = filterM (doesFileExist . (</> addExe program))
-- @-node:gcross.20100830091258.2011:lookForProgramInPaths
-- @+node:gcross.20100830091258.2016:lookForVersionedProgramInPaths
lookForVersionedProgramInPaths :: VersionedProgram → [FilePath] → IO (Map FilePath Version)
lookForVersionedProgramInPaths VersionedProgram{..} =
    lookForProgramInPaths versionedProgramName
    >=>
    fmap (Map.fromList . catMaybes)
    .
    mapM (
        \path → fmap (fmap ((,) path)) (getProgramVersion versionedProgramVersionExtractor (path </> versionedProgramName))
    )
-- @-node:gcross.20100830091258.2016:lookForVersionedProgramInPaths
-- @-node:gcross.20100830091258.2007:Functions
-- @-others
-- @-node:gcross.20100830091258.2004:@thin Tools.hs
-- @-leo
