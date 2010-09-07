-- @+leo-ver=4-thin
-- @+node:gcross.20100906112631.2146:@thin build-library.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100906112631.2147:<< Language extensions >>
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100906112631.2147:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100906112631.2148:<< Import needed modules >>
import Control.Applicative
import Control.Arrow
import Control.Monad

import Data.Binary
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Seq
import Data.Version

import System.Directory
import System.Exit
import System.FilePath
import System.Log.Logger

import Blueprint.Configuration.Tools
import Blueprint.Dependency.File.Object
import Blueprint.Fields.DeferredDependencies
import Blueprint.Jobs
import Blueprint.Jobs.Combinators
import Blueprint.Language.Programming.Haskell
import Blueprint.Miscellaneous
import Blueprint.Options
import Blueprint.Product
import Blueprint.Product.File.Program
import Blueprint.SourceFile
import Blueprint.Tools
import Blueprint.Tools.Ar
import Blueprint.Tools.Compilers.GHC
-- @-node:gcross.20100906112631.2148:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100906112631.2149:main
main = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    sources ← getAllSourceFilesAndPrependParentIn (Seq.singleton "Blueprint") "Blueprint"
    let haskell_sources = extractHaskellSources sources
        object_directory = "build" </> "library" </> "objects"
        interface_directory = "build" </> "library" </> "interfaces"
        built_modules =
            map (haskellSourceToBuiltModule object_directory interface_directory)
                haskell_sources
    (_,options) ← loadOptions "configuration.cfg" (ghcOptions `mappend` arOptions)
    (ghc_environment@GHCEnvironment{..},ar_configuration) ←
        runJobApplicativeUsingCacheFile 4 "configuration.cache"
        $
        liftA2 (,)
            (configureGHCEnvironmentUsingOptions options)
            (configureProgramUsingOptions options)
    (package_description,_) ←
        readAndConfigurePackageDescription
            ghc_environment
            []
            "Blueprint.cabal"
    let build_environment@BuildEnvironment{..} =
            computeBuildEnvironment
                ghc_environment
                package_description
                ExecutableTarget
                built_modules
                []
                []
                interface_directory
        compilation_jobs = createGHCCompileToObjectJobsFromBuildEnvironment build_environment
        built_archive = builtArchive ("build" </> "library" </> "archive" </> "libBlueprint.a")
        archive_jobs =
            createArFetchDeferredDependencesAndMakeArchiveJobs
                ar_configuration
                built_archive
                (lookupObjectJobIdInBuiltModules built_modules)
            .
            map builtModuleObjectFilePath
            $
            built_modules
    withJobServerUsingCacheFile 4 "build.cache" $ do
        mapM_ (submitJob . createSourceFileDigestJob) sources
        mapM_ submitJob (compilation_jobs ++ archive_jobs)
        requestJobResult . builtProductJobId $ built_archive
-- @-node:gcross.20100906112631.2149:main
-- @-others
-- @-node:gcross.20100906112631.2146:@thin build-library.hs
-- @-leo
