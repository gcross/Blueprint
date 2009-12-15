-- @+leo-ver=4-thin
-- @+node:gcross.20091214124713.1617:@thin Targets.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091214124713.1623:<< Language extensions >>
-- @-node:gcross.20091214124713.1623:<< Language extensions >>
-- @nl

module Blueprint.Tools.GHC.Targets where

-- @<< Import needed modules >>
-- @+node:gcross.20091214124713.1625:<< Import needed modules >>
import Control.Arrow
import Control.Monad

import Data.ErrorMessage
import Data.Digest.Pure.MD5

import Distribution.PackageDescription (PackageDescription)

import System.FilePath

import Blueprint.Configuration
import Blueprint.Resources
import Blueprint.Targets
import Blueprint.Tools.GHC
import Blueprint.Tools.GHC.Helpers
-- @-node:gcross.20091214124713.1625:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091214124713.1620:Targets
-- @+node:gcross.20091214124713.1701:makeTestTarget
makeTestTarget :: ErrorMessageOr Configuration -> [String] -> Resources -> ErrorMessageOr ()
makeTestTarget
    configure
    ghc_flags
    source_resources
  = makeRunTestTarget
        (configure
         >>=
         \configuration ->
            buildTestProgram
                configuration
                ghc_flags
                source_resources
        )
-- @-node:gcross.20091214124713.1701:makeTestTarget
-- @+node:gcross.20091214124713.1621:makeSelfTarget
makeSelfTarget :: ErrorMessageOr Configuration -> [String] -> Resources -> ErrorMessageOr Resource
makeSelfTarget configuration_or_error ghc_flags resources =
    configuration_or_error
    >>=
    \configuration ->
        assertResourceExists
        .
        ghcLinkProgram
            (ghcConfiguration configuration)
            (programBuildRoot </> "digest-cache")
            ghc_flags
            (packageDependencies configuration)
            "."
            [("Setup","o")]
        .
        compileObjectsForProgram
            configuration
            ghc_flags
        .
        addSourceResourceFor "Setup.hs"
        $
        resources
-- @-node:gcross.20091214124713.1621:makeSelfTarget
-- @+node:gcross.20091214124713.1687:makeReselfTarget
makeReselfTarget targets =
    makeRemoveFilesAndDirectoriesTarget [programBuildRoot]
    `thenTarget`
    lookupOldTarget "self" "reself" targets
-- @-node:gcross.20091214124713.1687:makeReselfTarget
-- @+node:gcross.20091214124713.1690:makeBuildLibraryTarget
makeBuildLibraryTarget ::
    ErrorMessageOr Configuration ->
    String ->
    [String] ->
    Resources ->
    ErrorMessageOr [Resource]
makeBuildLibraryTarget
    configure
    qualified_package_name
    ghc_flags
    source_resources
 = configure >>= \configuration ->
    (
        linkLibrary
            configuration
            qualified_package_name
        .
        compileObjectsForLibrary
            configuration
            qualified_package_name
            ghc_flags
        $
        source_resources
    )
-- @-node:gcross.20091214124713.1690:makeBuildLibraryTarget
-- @+node:gcross.20091214124713.1698:makeInstallTarget
makeInstallTarget ::
    ErrorMessageOr Configuration ->
    ErrorMessageOr [Resource] ->
    PackageDescription ->
    ErrorMessageOr ()
makeInstallTarget
    configure
    build
    package_description
   = do configuration <- configure
        built_files <- build
        installLibrary
            package_description
            configuration
            built_files
-- @-node:gcross.20091214124713.1698:makeInstallTarget
-- @-node:gcross.20091214124713.1620:Targets
-- @-others
-- @-node:gcross.20091214124713.1617:@thin Targets.hs
-- @-leo
