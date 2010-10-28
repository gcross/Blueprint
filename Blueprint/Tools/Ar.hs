-- @+leo-ver=4-thin
-- @+node:gcross.20101005114926.1468:@thin Ar.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20101005114926.1469:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20101005114926.1469:<< Language extensions >>
-- @nl

module Blueprint.Tools.Ar where

-- @<< Import needed modules >>
-- @+node:gcross.20101005114926.1470:<< Import needed modules >>
import Control.Monad.IO.Class

import Data.Binary
import Data.DeriveTH
import Data.Digest.Pure.MD5
import Data.List.Tagged (TaggedList(..),toTuple)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable

import System.Log.Logger

import Blueprint.Cache
import Blueprint.Configuration
import Blueprint.Identifier
import Blueprint.Job
import Blueprint.Miscellaneous
import Blueprint.Tools
-- @nonl
-- @-node:gcross.20101005114926.1470:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20101005114926.1472:Program
data Ar deriving Typeable; instance ProgramName Ar where { programNameFrom _ = "ar" }
-- @-node:gcross.20101005114926.1472:Program
-- @+node:gcross.20101005114926.1474:Options
ar_options = unwrapOptions (programOptions :: OptionsFor Ar)
-- @-node:gcross.20101005114926.1474:Options
-- @+node:gcross.20101005122519.1483:File Type
declareFileType "Archive"
-- @-node:gcross.20101005122519.1483:File Type
-- @+node:gcross.20101005114926.1475:Functions
-- @+node:gcross.20101005114926.1477:makeArchive
makeArchive ::
    ProgramConfiguration Ar →
    Map FilePath MD5Digest →
    FilePath →
    Job ArchiveFile
makeArchive
    ProgramConfiguration{..}
    object_digests
    archive_filepath
  = once my_id
    .
    fmap (File archive_filepath)
    $
    runIfDependencyOrProductHasChanged
        my_id
        (programExtraArguments,object_digests)
        (\old_digest → fmap (/= Just old_digest) (digestFileIfExists archive_filepath))
        build
  where
    my_id =
        identifierInNamespace
            (uuid "5a0923aa-3580-4b71-8a73-c187eced95b3")
            ("creating archive " ++ archive_filepath)

    ar_arguments = "cqs":archive_filepath:(Map.keys object_digests ++ programExtraArguments)
    build = do
        liftIO . noticeM "Blueprint.Tools.Ar" $
            "(Ar) Creating archive " ++ archive_filepath
        liftIO . infoM "Blueprint.Tools.Ar" $
            "(Ar) Executing '" ++ (unwords (programFilePath:ar_arguments)) ++ "'"
        fmap toTuple $
            runProductionCommandAndDigestOutputs
                (archive_filepath :. E)
                programFilePath
                ar_arguments
-- @nonl
-- @-node:gcross.20101005114926.1477:makeArchive
-- @-node:gcross.20101005114926.1475:Functions
-- @-others
-- @-node:gcross.20101005114926.1468:@thin Ar.hs
-- @-leo
