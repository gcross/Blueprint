-- @+leo-ver=4-thin
-- @+node:gcross.20100709210816.2097:@thin build-test.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100709210816.2098:<< Language extensions >>
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100709210816.2098:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100709210816.2099:<< Import needed modules >>
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

import Blueprint.Configuration
import Blueprint.Configuration.Tools
import Blueprint.Dependency.File.Object
import Blueprint.Fields.DeferredDependencies
import Blueprint.Jobs
import Blueprint.Jobs.Combinators
import Blueprint.Language.Programming.Haskell
import Blueprint.Main
import Blueprint.Miscellaneous
import Blueprint.Options
import Blueprint.Product
import Blueprint.Product.File.Program
import Blueprint.SourceFile
import Blueprint.Phase
import Blueprint.Tools
import Blueprint.Tools.Compilers.GHC
-- @nonl
-- @-node:gcross.20100709210816.2099:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100709210816.2100:main
main = runMain
    [("configure",runPhase configure)
    ,("build",runPhase (configure >>= build))
    ]
-- @nonl
-- @-node:gcross.20100709210816.2100:main
-- @+node:gcross.20100906112631.2217:Phases
-- @+node:gcross.20100906112631.2218:configure
configure =
    configurationPhase
        "configuration.cfg"
        "configuration.cache"
        ghcOptions
        configureGHCEnvironmentUsingOptions
-- @nonl
-- @-node:gcross.20100906112631.2218:configure
-- @+node:gcross.20100906112631.2219:build
build ghc_environment@GHCEnvironment{..} = Phase . const $ do
    sources ←
        fmap ((sourceFile "test.hs" (Seq.singleton "Main"):) . concat)
        .
        mapM (\subdirectory →
            getAllSourceFilesAndPrependParentIn (Seq.singleton subdirectory) subdirectory
        )
        $
        ["Blueprint"]
    let haskell_sources = extractHaskellSources sources
        object_directory = "build" </> "program" </> "objects"
        interface_directory = "build" </> "program" </> "interfaces"
        built_modules =
            map (haskellSourceToBuiltModule object_directory interface_directory)
                haskell_sources
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
        built_program = builtProgram "test"
        link_jobs =
            createGHCFetchDeferredDependencesAndLinkProgramJobs
                (pathToGHC ghcEnvironmentGHC)
                []
                built_program
                buildEnvironmentLookupDependencyJobId
                [objectDependency (builtModuleObjectFilePath . head $ built_modules)]
    withJobServerUsingCacheFile 4 "build.cache" $ do
        mapM_ (submitJob . createSourceFileDigestJob) sources
        mapM_ submitJob (compilation_jobs ++ link_jobs)
        requestJobResult . builtProductJobId $ built_program
-- @nonl
-- @-node:gcross.20100906112631.2219:build
-- @-node:gcross.20100906112631.2217:Phases
-- @-others
-- @-node:gcross.20100709210816.2097:@thin build-test.hs
-- @-leo
