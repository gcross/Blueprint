-- @+leo-ver=4-thin
-- @+node:gcross.20101007134409.1495:@thin Main.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20101007134409.1496:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20101007134409.1496:<< Language extensions >>
-- @nl

module Blueprint.Main where

-- @<< Import needed modules >>
-- @+node:gcross.20101007134409.1497:<< Import needed modules >>
import Data.Map (Map)
import qualified Data.Map as Map

import System.Environment
import System.Exit
import System.Log.Logger

import Blueprint.Job
import Blueprint.Options
-- @nonl
-- @-node:gcross.20101007134409.1497:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20101007134409.1499:Functions
-- @+node:gcross.20101007134409.1500:defaultMain
defaultMain ::
    Options →
    FilePath →
    FilePath →
    FilePath →
    (OptionValues → Job α) →
    Map String (α → Job ()) →
    IO ()
defaultMain
    options
    configuration_filepath
    configuration_cache_filepath
    cache_filepath
    configure
    modes
  = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    (arguments,command_line_option_values) ← getAndParseCommandLineOptions options
    let mode = unwords arguments
    if mode == "configure"
        then do
            option_values ←
                getParseAndUpdateConfigurationFile
                    options
                    configuration_filepath
                    command_line_option_values
            runJobUsingCacheFileAndExtractResult 1 configuration_cache_filepath $
                configure option_values
            return ()
        else do
            job ← case Map.lookup mode modes of
                    Just job → return job
                    Nothing → do
                        putStrLn $ "Unknown mode of operator '" ++ mode ++ "'"
                        putStrLn $ "Allowed modes are:"
                        mapM_ (putStrLn . ('\t':)) . Map.keys $ modes -- '
                        exitFailure
            option_values ←
                fmap (Map.union command_line_option_values)
                $
                getAndParseConfigurationFile
                    options
                    configuration_filepath
            (runJobUsingCacheFileAndExtractResult
                1
                configuration_cache_filepath
                (configure option_values)
             >>=
             runJobUsingCacheFileAndExtractResult
                1
                cache_filepath
             .
             job
             )
-- @-node:gcross.20101007134409.1500:defaultMain
-- @-node:gcross.20101007134409.1499:Functions
-- @-others
-- @-node:gcross.20101007134409.1495:@thin Main.hs
-- @-leo
