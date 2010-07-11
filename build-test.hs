-- @+leo-ver=4-thin
-- @+node:gcross.20100709210816.2097:@thin build-test.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100709210816.2098:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100709210816.2098:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100709210816.2099:<< Import needed modules >>
import Control.Applicative
import Control.Arrow
import Control.Monad

import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Seq
import Data.Version

import System.Exit
import System.Log.Logger

import Blueprint.Fields.DeferredDependencies
import Blueprint.Jobs
import Blueprint.Language.Programming.Haskell
import Blueprint.Miscellaneous
import Blueprint.SourceFile
import Blueprint.Tools.Compilers.GHC
-- @-node:gcross.20100709210816.2099:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100709210816.2100:main
main = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    sources ←
        fmap ((sourceFile "test.hs" (Seq.singleton "Main"):) . concat)
        .
        mapM (\subdirectory →
            getAllSourceFilesAndPrependParentIn (Seq.singleton subdirectory) subdirectory
        )
        $
        ["Blueprint","Control","Data"]
    let path_to_ghc = "ghc"
        path_to_ghc_pkg = "ghc-pkg"
        packages_required =
            map (second . uncurry $ betweenVersions)
            [("base",("4","5"))
            ,("binary",("0.5","0.6"))
            ,("pureMD5",("1.1","1.2"))
            ,("derive",("2.3","2.4"))
            ,("Vec",("0.9","1"))
            ,("parallel",("2.2","2.3"))
            ,("uuid",("1.2.1","1.3"))
            ,("bytestring",("0.9","1.0"))
            ,("containers",("0.3","0.4"))
            ,("utf8-string",("0.3","0.4"))
            ,("transformers",("0.2","0.3"))
            ,("data-accessor-template",("0.2","0.3"))
            ,("data-accessor-transformers",("0.2","0.3"))
            ,("MonadCatchIO-transformers",("0.2","0.3"))
            ,("either-unwrap",("1.1","1.2"))
            ,("HUnit",("1.2","1.3"))
            ,("test-framework",("0.3","0.4"))
            ,("test-framework-hunit",("0.2","0.3"))
            ,("test-framework-quickcheck2",("0.2","0.3"))
            ,("random",("1.0","1.1"))
            ,("QuickCheck",("2.1","2.2"))
            ,("HStringTemplate",("0.6","0.7"))
            ,("directory",("1.0","1.1"))
            ,("filepath",("1.1","1.2"))
            ,("process",("1.0","1.1"))
            ,("hslogger",("1.0","1.1"))
            ,("regex-pcre-builtin",("0.94","0.95"))
            ,("array",("0.3","0.4"))
            ]
    known_package_modules ←
        resolvePackages path_to_ghc_pkg packages_required
        >>=
        (\resolutions →
            case resolutions of
                Left unresolved_packages → do
                    putStrLn "Unable to resolve the following packages:"
                    forM_ unresolved_packages $ \(package_name,package_versions) →
                        putStrLn $ "\t" ++ package_name ++ " " ++ show (map showVersion package_versions)
                    exitFailure
                Right package_ids → return package_ids
        )
        >>=
        fmap fromJust . fetchKnownModulesFromPackages path_to_ghc_pkg
    let haskell_sources = extractHaskellSources sources
        built_modules = map (haskellSourceToBuiltModule "objects" "interfaces") haskell_sources
        known_modules =
            (builtModulesToKnownModules built_modules)
            `mappend`
            known_package_modules
        options_arguments = []
        compilation_jobs = 
            map (
                createGHCCompileToObjectJob
                    path_to_ghc
                    path_to_ghc_pkg
                    known_modules
                    options_arguments
            ) built_modules
    withJobServer 4 Map.empty $ \job_server → do
        mapM_ (submitJob job_server . createSourceFileDigestJob) sources
        mapM_ (submitJob job_server) compilation_jobs
        object_result ← requestJobResult job_server . builtModuleObjectJobId . head $ built_modules
        putStrLn . show . getDeferredDependencies $ object_result
-- @-node:gcross.20100709210816.2100:main
-- @+node:gcross.20100709210816.2217:betweenVersions
betweenVersions lower upper = liftA2 (&&) (>= readVersion lower) (< readVersion upper)
-- @nonl
-- @-node:gcross.20100709210816.2217:betweenVersions
-- @-others
-- @-node:gcross.20100709210816.2097:@thin build-test.hs
-- @-leo
