-- @+leo-ver=4-thin
-- @+node:gcross.20091122100142.1360:@thin Ar.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091122100142.1361:<< Language extensions >>
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
-- @nonl
-- @-node:gcross.20091122100142.1361:<< Language extensions >>
-- @nl

module Blueprint.Tools.Ar where

-- @<< Import needed modules >>
-- @+node:gcross.20091122100142.1362:<< Import needed modules >>
import Control.Monad

import Data.Dynamic

import System.Directory
import System.Exit
import System.FilePath
import System.IO.Unsafe
import System.Process

import Blueprint.Configuration
import Blueprint.Cache.ExplicitDependencies
import Blueprint.Error
import Blueprint.Miscellaneous
import Blueprint.Options
import Blueprint.Resources
-- @-node:gcross.20091122100142.1362:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091122100142.1363:Types
-- @+node:gcross.20091122100142.1364:ArTools
data ArTools = ArTools { arPath :: FilePath } deriving (Show)
-- @-node:gcross.20091122100142.1364:ArTools
-- @-node:gcross.20091122100142.1363:Types
-- @+node:gcross.20091128000856.1422:Instances
-- @+node:gcross.20091128000856.1423:ConfigurationData ArTools
instance ConfigurationData ArTools where
    readConfig = liftM ArTools (getConfig "path to ar")
    writeConfig = (setConfig "path to ar" . arPath)
-- @-node:gcross.20091128000856.1423:ConfigurationData ArTools
-- @+node:gcross.20091128000856.1424:AutomaticallyConfigurable ArTools
instance AutomaticallyConfigurable ArTools where
    automaticallyConfigure = simpleSearchForProgram ArTools "ArTools" "ar"
-- @-node:gcross.20091128000856.1424:AutomaticallyConfigurable ArTools
-- @-node:gcross.20091128000856.1422:Instances
-- @+node:gcross.20091129000542.1502:Options processing
arToolsOptions = makeSimpleOptionSection "ar"
-- @-node:gcross.20091129000542.1502:Options processing
-- @+node:gcross.20091122100142.1367:Tools
-- @+node:gcross.20091122100142.1368:formStaticLibrary
formStaticLibrary ::
    ArTools ->
    FilePath ->
    [Resource] ->
    String ->
    FilePath ->
    Resource
formStaticLibrary
    tools
    cache_directory
    object_resources
    library_resource_name
    library_resource_filepath
    = Resource
        {   resourceName = library_resource_name
        ,   resourceType = "a"
        ,   resourceFilePath = library_resource_filepath
        ,   resourceDigest = library_digest
        ,   resourceDependencies = map resourceId object_resources
        }
  where
    library_digest = either Left (Right . head) $
        analyzeExplicitDependenciesAndRebuildIfNecessary
            builder
            (cache_directory </> library_resource_name <.> "a")
            [library_resource_filepath]
            ()
            object_resources

    builder = do
        createDirectoryIfMissing True . takeDirectory $ library_resource_filepath
        let arguments = ("cqs":library_resource_filepath:map resourceFilePath object_resources)
            command = (arPath tools)
        putStrLn . unwords . (command:) $ arguments
        compilation_result <-
            readProcessWithExitCode
            command
            arguments
            ""
        case compilation_result of
            (ExitFailure _,_,error_message) ->
                return
                .
                Just
                .
                errorMessageTextWithLines ("linking " ++ library_resource_name)
                $
                error_message
            (ExitSuccess,_,_) -> return Nothing

-- @-node:gcross.20091122100142.1368:formStaticLibrary
-- @-node:gcross.20091122100142.1367:Tools
-- @-others
-- @-node:gcross.20091122100142.1360:@thin Ar.hs
-- @-leo
