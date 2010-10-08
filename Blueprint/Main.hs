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
import Data.Monoid

import System.Environment
import System.Exit
import System.Log.Logger

import Blueprint.Identifier
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
    user_options
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
            runJobUsingCacheFileAndExtractResult
                (extractNumberOfSimultaneousTasksFrom option_values)
                configuration_cache_filepath
                (configure option_values)
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
            let number_of_simultaneous_tasks = extractNumberOfSimultaneousTasksFrom option_values
            (runJobUsingCacheFileAndExtractResult
                number_of_simultaneous_tasks
                configuration_cache_filepath
                (configure option_values)
             >>=
             runJobUsingCacheFileAndExtractResult
                number_of_simultaneous_tasks
                cache_filepath
             .
             job
             )
  where
    options = user_options `mappend` main_options

    extractNumberOfSimultaneousTasksFrom :: OptionValues → Int
    extractNumberOfSimultaneousTasksFrom =
        maybe 1 read
        .
        Map.lookup main_option_number_of_simultaneous_tasks
-- @-node:gcross.20101007134409.1500:defaultMain
-- @-node:gcross.20101007134409.1499:Functions
-- @+node:gcross.20101007134409.1504:Options
main_option_number_of_simultaneous_tasks = identifier "4b286db3-9976-472d-a865-2e2e7e5cb2aa" "number of simultaneous tasks"
main_option_help = identifier "027b2e80-afc4-4429-85f7-0752eff276ec" "help"

main_options =
    Options
        (Map.fromList
            [('j',(main_option_number_of_simultaneous_tasks,RequiredArgument "#"))
            ,('h',(main_option_help,NoArgument "1")) -- '
            ]
        )
        (Map.fromList
            [("number-of-simultaneous-tasks",(main_option_number_of_simultaneous_tasks,RequiredArgument "#"))
            ,("help",(main_option_help,NoArgument "1"))
            ]
        )
        Map.empty
        Map.empty
        (Map.fromList
            [(main_option_number_of_simultaneous_tasks,("Main","Number of tasks to run simultaneously (defaults to 1)"))
            ,(main_option_help,("Main","Display this help message and then exit."))
            ]
        )
-- @-node:gcross.20101007134409.1504:Options
-- @-others
-- @-node:gcross.20101007134409.1495:@thin Main.hs
-- @-leo
