-- @+leo-ver=4-thin
-- @+node:gcross.20101005114926.1468:@thin Ar.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20101005114926.1469:<< Language extensions >>
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20101005114926.1469:<< Language extensions >>
-- @nl

module Blueprint.Tools.Ar where

-- @<< Import needed modules >>
-- @+node:gcross.20101005114926.1470:<< Import needed modules >>
import Data.Typeable

import Blueprint.Configuration
-- @-node:gcross.20101005114926.1470:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20101005114926.1472:Program
data Ar deriving Typeable; instance ProgramName Ar where { programNameFrom _ = "ar" }
-- @-node:gcross.20101005114926.1472:Program
-- @+node:gcross.20101005114926.1474:Options
arOptions = unwrapOptions (programOptions :: OptionsFor Ar)
-- @-node:gcross.20101005114926.1474:Options
-- @+node:gcross.20101005114926.1475:Functions
-- @+node:gcross.20101005114926.1477:createArMakeArchiveIncompleteJob
createArMakeArchiveIncompleteJob ::
    ProgramConfiguration Ar →
    Map FilePath MD5Digest →
    FilePath →
    IncompleteToolJob ArchiveComponents
createArMakeArchiveIncompleteJob
    ProgramConfiguration{..}
    BuiltProduct{..}
    =
    incompleteJobWithCache [builtProductJobId]
    $
    \archive_components@ArchiveComponents{..} →
        let ar_arguments = "cqs":builtProductName:(archiveComponentObjectFilePaths ++ programExtraArguments)
            builder = liftIO $ do
                noticeM "Blueprint.Tools.Ar" $
                    "(GHC) Creating archive "
                    ++ builtProductName
                infoM "Blueprint.Tools.Ar" $
                    "(GHC) Executing '" ++ (unwords (programFilePath:ar_arguments)) ++ "'"
                runProductionCommandAndDigestOutputs
                    [builtProductName]
                    []
                    programFilePath
                    ar_arguments
        in  runJobAnalyzer
            .
            fmap (
                zipWith ($)
                .
                (:[])
                $
                (
                    setDeferredDependencies archiveComponentDeferredDependencies
                    .
                    setFilePath builtProductName
                )
            )
            $
            compareToCacheAndRebuildIfNecessary
                builder
                (liftIO . checkDigestsOfFilesIfExisting [builtProductName])
                (archiveComponentObjectFilePaths,archiveComponentObjectDigests)
-- @nonl
-- @-node:gcross.20101005114926.1477:createArMakeArchiveIncompleteJob
-- @-node:gcross.20101005114926.1475:Functions
-- @-others
-- @-node:gcross.20101005114926.1468:@thin Ar.hs
-- @-leo
