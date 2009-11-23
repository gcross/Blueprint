-- @+leo-ver=4-thin
-- @+node:gcross.20091123114318.1335:@thin GCC.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091123114318.1337:<< Language extensions >>
-- @-node:gcross.20091123114318.1337:<< Language extensions >>
-- @nl

module Blueprint.Tools.GCC where

-- @<< Import needed modules >>
-- @+node:gcross.20091123114318.1339:<< Import needed modules >>
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import System.Directory
import System.Exit
import System.FilePath
import System.IO.Unsafe
import System.Process

import Blueprint.Cache.SingleDependency
import Blueprint.Resources
-- @-node:gcross.20091123114318.1339:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091123114318.1342:Types
-- @+node:gcross.20091123114318.1343:GCCTools
data GCCTools = GCCTools
        {   gccCCompilerPath :: FilePath
        ,   gccFortranCompilerPath :: FilePath
        } deriving (Show)
-- @-node:gcross.20091123114318.1343:GCCTools
-- @-node:gcross.20091123114318.1342:Types
-- @+node:gcross.20091123114318.1346:Configuration
-- @+node:gcross.20091123114318.1347:gccTools
gccTools :: Maybe GCCTools
gccTools = unsafePerformIO $ do
    maybe_path_to_gcc <- findExecutable "gcc"
    maybe_path_to_gfortran <- findExecutable "gfortran"
    return $ liftM2 GCCTools maybe_path_to_gcc maybe_path_to_gfortran
-- @-node:gcross.20091123114318.1347:gccTools
-- @-node:gcross.20091123114318.1346:Configuration
-- @+node:gcross.20091123114318.1367:Tools
-- @+node:gcross.20091123114318.1372:C compilation
-- @+node:gcross.20091123114318.1368:gccCompileC
gccCompileC ::
    GCCTools ->
    [String] ->
    FilePath ->
    FilePath ->
    Resource ->
    Resource
gccCompileC
    tools
    options
    object_destination_directory
    cache_directory
    source_resource
    =
    let source_filepath = resourceFilePath source_resource
        source_name = resourceName source_resource
        object_filepath = getFilePathForNameAndType object_destination_directory source_name "o"
        object_resource =
            Resource
                {   resourceName = source_name
                ,   resourceType = "o"
                ,   resourceFilePath = object_filepath
                ,   resourceDigest = object_digest
                }

        builder =
            let arguments = 
                    options ++
                    ["-c",source_filepath
                    ,"-o",object_filepath
                    ]
                path_to_gcc = gccCCompilerPath tools
            in do
                createDirectoryIfMissing True . takeDirectory $ object_filepath
                putStrLn . unwords $ (path_to_gcc:arguments)
                compilation_result <- readProcessWithExitCode path_to_gcc arguments ""
                case compilation_result of
                    (ExitFailure _,_,error_message) -> return . Just . Map.singleton source_filepath $ error_message
                    (ExitSuccess,_,_) -> return Nothing

        object_digest =
            case analyzeDependencyAndRebuildIfNecessary
                    builder
                    (cache_directory </> source_name <.> "o")
                    [object_filepath]
                    (unwords options)
                    source_resource
            of Left error_message -> Left error_message
               Right [object_digest] -> Right object_digest
               x -> error $ "Programmer error:  Builder returned the wrong number of digests! (" ++ show x ++ ")"

    in object_resource
-- @-node:gcross.20091123114318.1368:gccCompileC
-- @+node:gcross.20091123114318.1371:gccCompileAllC
gccCompileAllC ::
    GCCTools ->
    [String] ->
    FilePath ->
    FilePath ->
    Resources ->
    Resources
gccCompileAllC
    tools
    options
    object_destination_directory
    cache_directory
    old_resources
    =
    let new_resources = go old_resources (Map.elems old_resources)
        go accum_resources [] = accum_resources
        go accum_resources (resource:rest_resources) =
            if resourceType (resource) == "c"
                then let object_resource =
                            gccCompileC
                                tools
                                options
                                object_destination_directory
                                cache_directory
                                resource
                     in go (addResource object_resource accum_resources) rest_resources
                else go accum_resources rest_resources
    in new_resources
-- @-node:gcross.20091123114318.1371:gccCompileAllC
-- @-node:gcross.20091123114318.1372:C compilation
-- @+node:gcross.20091123114318.1376:Fortran compilation
-- @+node:gcross.20091123114318.1377:gccCompileFortran
gccCompileFortran ::
    GCCTools ->
    [String] ->
    FilePath ->
    FilePath ->
    FilePath ->
    Resource ->
    Resource
gccCompileFortran
    tools
    options
    object_destination_directory
    interface_destination_directory
    cache_directory
    source_resource
    =
    let source_filepath = resourceFilePath source_resource
        source_name = resourceName source_resource
        object_filepath = getFilePathForNameAndType object_destination_directory source_name "o"
        object_resource =
            Resource
                {   resourceName = source_name
                ,   resourceType = "o"
                ,   resourceFilePath = object_filepath
                ,   resourceDigest = object_digest
                }

        interface_filepath = getFilePathForNameAndType interface_destination_directory source_name "mod"
-- @+at
--          interface_resource =
--              Resource
--                  {   resourceName = source_name
--                  ,   resourceType = "mod"
--                  ,   resourceFilePath = interface_filepath
--                  ,   resourceDigest = interface_digest
--                  }
-- @-at
-- @@c
        builder =
            let arguments = 
                    options ++
                    ["-J"++interface_destination_directory
                    ,"-c",source_filepath
                    ,"-o",object_filepath
                    ]
                path_to_gfortran = gccFortranCompilerPath tools
            in do
                createDirectoryIfMissing True . takeDirectory $ interface_filepath
                createDirectoryIfMissing True . takeDirectory $ object_filepath
                putStrLn . unwords $ (path_to_gfortran:arguments)
                compilation_result <- readProcessWithExitCode path_to_gfortran arguments ""
                case compilation_result of
                    (ExitFailure _,_,error_message) -> return . Just . Map.singleton source_filepath $ error_message
                    (ExitSuccess,_,_) -> return Nothing

        object_digest =
            case analyzeDependencyAndRebuildIfNecessary
                    builder
                    (cache_directory </> source_name <.> "o")
                    [object_filepath]
                    (unwords options)
                    source_resource
            of Left error_message -> Left error_message
               Right [object_digest] -> Right object_digest
               x -> error $ "Programmer error:  Builder returned the wrong number of digests! (" ++ show x ++ ")"

    in object_resource
-- @-node:gcross.20091123114318.1377:gccCompileFortran
-- @+node:gcross.20091123114318.1378:gccCompileAllFortran
gccCompileAllFortran ::
    GCCTools ->
    [String] ->
    FilePath ->
    FilePath ->
    FilePath ->
    Resources ->
    Resources
gccCompileAllFortran
    tools
    options
    object_destination_directory
    interface_destination_directory
    cache_directory
    old_resources
    =
    let new_resources = go old_resources (Map.elems old_resources)
        go accum_resources [] = accum_resources
        go accum_resources (resource:rest_resources) =
            if resourceType (resource) `elem` ["f","f77","f90","f95"]
                then let object_resource =
                            gccCompileFortran
                                tools
                                options
                                object_destination_directory
                                interface_destination_directory
                                cache_directory
                                resource
                     in go (addResource object_resource $ accum_resources) rest_resources
                else go accum_resources rest_resources
    in new_resources
-- @-node:gcross.20091123114318.1378:gccCompileAllFortran
-- @-node:gcross.20091123114318.1376:Fortran compilation
-- @-node:gcross.20091123114318.1367:Tools
-- @-others
-- @-node:gcross.20091123114318.1335:@thin GCC.hs
-- @-leo
