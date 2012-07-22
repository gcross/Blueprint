-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Blueprint.Tools.Ar where

-- Imports {{{
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
-- }}}

data Ar deriving Typeable; instance ProgramName Ar where { programNameFrom _ = "ar" }

arOptions = unwrapOptions (programOptions :: OptionsFor Ar)

declareFileType "Archive"

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
