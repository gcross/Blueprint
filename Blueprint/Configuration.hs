-- @+leo-ver=4-thin
-- @+node:gcross.20100927123234.1487:@thin Configuration.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100927123234.1488:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100927123234.1488:<< Language extensions >>
-- @nl

module Blueprint.Configuration where

-- @<< Import needed modules >>
-- @+node:gcross.20100927123234.1489:<< Import needed modules >>
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Abort

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Binary
import Data.DeriveTH
import Data.Dynamic
import Data.List
import Data.Maybe
import Data.Typeable
import Data.Version

import System.Directory
import System.Environment
import System.FilePath
import System.Process

import Text.Regex.Base

import Blueprint.Identifier
import Blueprint.Job
import Blueprint.Miscellaneous
import Blueprint.Options
-- @-node:gcross.20100927123234.1489:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100927123234.1490:Exceptions
-- @+node:gcross.20100927123234.1491:BadProgramVersionException
data BadProgramVersionException = BadProgramVersionException FilePath String deriving (Show,Eq,Typeable)

instance Exception BadProgramVersionException
-- @-node:gcross.20100927123234.1491:BadProgramVersionException
-- @+node:gcross.20100927123234.1492:ProgramDoesNotExistException
data ProgramDoesNotExistException = ProgramDoesNotExistException FilePath deriving (Show,Eq,Typeable)

instance Exception ProgramDoesNotExistException

-- @-node:gcross.20100927123234.1492:ProgramDoesNotExistException
-- @+node:gcross.20100927123234.1493:UnableToLocateProgramException
data UnableToLocateProgramException = UnableToLocateProgramException FilePath [FilePath] deriving (Eq,Show,Typeable)

instance Exception UnableToLocateProgramException
-- @nonl
-- @-node:gcross.20100927123234.1493:UnableToLocateProgramException
-- @-node:gcross.20100927123234.1490:Exceptions
-- @+node:gcross.20100927123234.1494:Classes
-- @+node:gcross.20100927123234.1495:ProgramName
class Typeable a => ProgramName a where
    programNameFrom :: f a → String
-- @-node:gcross.20100927123234.1495:ProgramName
-- @-node:gcross.20100927123234.1494:Classes
-- @+node:gcross.20100927123234.1496:Types
-- @+node:gcross.20100927123234.1497:ProgramConfigurationOptionIds
data ProgramConfigurationOptionIds a = ProgramConfigurationOptionIds
    {   optionIdForProgramLocation :: OptionId
    ,   optionIdForSearchPaths :: OptionId
    ,   optionIdForExtraArguments :: OptionId
    }
-- @-node:gcross.20100927123234.1497:ProgramConfigurationOptionIds
-- @+node:gcross.20100927123234.1498:ProgramConfigurationOptions
data ProgramConfigurationOptions a = ProgramConfigurationOptions
    {   configurationOptionProgramLocation :: Maybe FilePath
    ,   configurationOptionSearchPaths :: [FilePath]
    ,   configurationOptionExtraArguments :: [String]
    } deriving (Eq,Show,Typeable)

$(derive makeBinary ''ProgramConfigurationOptions)
-- @-node:gcross.20100927123234.1498:ProgramConfigurationOptions
-- @+node:gcross.20100927123234.1499:ProgramConfiguration
data ProgramConfiguration a = ProgramConfiguration
    {   programFilePath :: FilePath
    ,   programExtraArguments :: [String]
    } deriving (Eq,Show,Typeable)

$(derive makeBinary ''ProgramConfiguration)
-- @-node:gcross.20100927123234.1499:ProgramConfiguration
-- @+node:gcross.20100927123234.1500:OptionsFor
newtype OptionsFor a = OptionsFor { unwrapOptions :: Options }
-- @-node:gcross.20100927123234.1500:OptionsFor
-- @-node:gcross.20100927123234.1496:Types
-- @+node:gcross.20100927123234.1501:Functions
-- @+node:gcross.20100927123234.1504:configureProgram
configureProgram ::
    ProgramName α ⇒
    ProgramConfigurationOptions α →
    Job (ProgramConfiguration α)
configureProgram options@ProgramConfigurationOptions{..} =
    once my_uuid
    .
    cache my_uuid
    $
    liftIO . configureIt
    >=>
    (\path_to_program → return $
        (Just (options, path_to_program)
        ,ProgramConfiguration path_to_program configurationOptionExtraArguments
        )
    )
 where
    program_name = programNameFrom options
    my_uuid = inNamespace (uuid "432fb242-cc22-4827-baee-70a7443f9b3b") program_name

    configureIt maybe_old_options
      | Just (old_options,old_config) ← maybe_old_options
      , old_options == options
        = return old_config
      | Just path_to_program ← configurationOptionProgramLocation
        = do
            exists ← doesFileExist path_to_program
            if exists
                then return path_to_program
                else throwIO (ProgramDoesNotExistException path_to_program)            
      | [] ← configurationOptionSearchPaths
        = getSearchPath >>= configureUsingSearchPaths
      | otherwise
        = configureUsingSearchPaths configurationOptionSearchPaths

    configureUsingSearchPaths :: [FilePath] → IO FilePath
    configureUsingSearchPaths search_paths = runAbortT $ do
        forM_ search_paths $ \search_path → do
            let path_to_program = search_path </> program_name
            exists ← liftIO . doesFileExist $ path_to_program
            when exists (abort path_to_program)
        liftIO . throwIO $ UnableToLocateProgramException program_name search_paths
-- @-node:gcross.20100927123234.1504:configureProgram
-- @+node:gcross.20100927123234.1503:configureProgramUsingOptions
configureProgramUsingOptions ::
    ProgramName α ⇒
    OptionValues →
    Job (ProgramConfiguration α)
configureProgramUsingOptions = configureProgram . extractProgramConfigurationOptions
-- @-node:gcross.20100927123234.1503:configureProgramUsingOptions
-- @+node:gcross.20100927123234.1505:determineProgramVersion
determineProgramVersion ::
    (String → Maybe Version) →
    [String] →
    FilePath →
    IO Version
determineProgramVersion tryParseVersion arguments program =
    readProcess program arguments ""
    >>=
    \output →
        case tryParseVersion output of
            Nothing → throwIO $ BadProgramVersionException program output
            Just version → return version
-- @-node:gcross.20100927123234.1505:determineProgramVersion
-- @+node:gcross.20100927123234.1507:extractProgramConfigurationOptions
extractProgramConfigurationOptions ::
    ProgramName α ⇒
    OptionValues →
    ProgramConfigurationOptions α
extractProgramConfigurationOptions = extractProgramConfigurationOptionsUsing programOptionIds
-- @-node:gcross.20100927123234.1507:extractProgramConfigurationOptions
-- @+node:gcross.20100927123234.1506:extractProgramConfigurationOptionsUsing
extractProgramConfigurationOptionsUsing ::
    ProgramConfigurationOptionIds α →
    OptionValues →
    ProgramConfigurationOptions α
extractProgramConfigurationOptionsUsing ProgramConfigurationOptionIds{..} =
    ProgramConfigurationOptions
        <$> Map.lookup optionIdForProgramLocation
        <*> maybe [] splitSearchPath . Map.lookup optionIdForSearchPaths
        <*> maybe [] words . Map.lookup optionIdForExtraArguments
-- @-node:gcross.20100927123234.1506:extractProgramConfigurationOptionsUsing
-- @+node:gcross.20100927123234.1508:programOptionIds
programOptionIds :: ProgramName α ⇒ ProgramConfigurationOptionIds α
programOptionIds =
    let option_ids@ProgramConfigurationOptionIds{..} =
            ProgramConfigurationOptionIds
                (identifierInNamespace
                    program_configuration_namespace
                    ("path to " ++ program_name)
                    ("path to " ++ program_name)
                )
                (identifierInNamespace
                    program_configuration_namespace
                    ("paths to search for " ++ program_name)
                    ("paths to search for " ++ program_name)
                )
                (identifierInNamespace
                    program_configuration_namespace
                    ("additional arguments for " ++ program_name)
                    ("additional arguments for " ++ program_name)
                )
        program_name = programNameFrom option_ids
    in option_ids
-- @-node:gcross.20100927123234.1508:programOptionIds
-- @+node:gcross.20100927123234.1509:programOptionIdsFor
programOptionIdsFor :: ProgramName α ⇒ OptionsFor α → ProgramConfigurationOptionIds α
programOptionIdsFor = const programOptionIds
-- @-node:gcross.20100927123234.1509:programOptionIdsFor
-- @+node:gcross.20100927123234.1510:programOptions
programOptions :: ProgramName α ⇒ OptionsFor α
programOptions = options
  where
    program_name = programNameFrom options
    option_ids@ProgramConfigurationOptionIds{..} = programOptionIdsFor options
    options = OptionsFor $
        Options
            Map.empty
            (Map.fromList
                [("with-" ++ program_name,(optionIdForProgramLocation,(RequiredArgument "PATH")))
                ,("with-" ++ program_name ++ "-located-in",(optionIdForSearchPaths,(RequiredArgument "DIRECTORIES")))
                ,(program_name ++ "-options",(optionIdForExtraArguments,(RequiredArgument "ARGUMENTS")))
                ]
            )
            (Map.fromList
                [("tools." ++ program_name ++ ".location",optionIdForProgramLocation)
                ,("tools." ++ program_name ++ ".paths-to-search",optionIdForSearchPaths)
                ,("tools." ++ program_name ++ ".options",optionIdForExtraArguments)
                ]
            )
            (Map.fromList
                [(optionIdForSearchPaths,Right . intercalate [searchPathSeparator])
                ,(optionIdForExtraArguments,Right . unwords)
                ]
            )
            (Map.fromList
                [(optionIdForProgramLocation,(program_name,"Path to " ++ program_name))
                ,(optionIdForSearchPaths,(program_name,"Directories to search for " ++ program_name ++ " (separated by " ++ [searchPathSeparator] ++ ")"))
                ,(optionIdForExtraArguments,(program_name,"Additional arguments to be passed to " ++ program_name ++ ", separated by spaces;  on the command line one can equivalently use --" ++ program_name ++ "-options multiple times to pass multiple options."))
                ]
            )
-- @-node:gcross.20100927123234.1510:programOptions
-- @-node:gcross.20100927123234.1501:Functions
-- @+node:gcross.20100927123234.1511:Namespaces
program_configuration_namespace = uuid "8ea8f257-d1a1-4b6f-8ad0-0abd8de10211"
-- @nonl
-- @-node:gcross.20100927123234.1511:Namespaces
-- @-others
-- @-node:gcross.20100927123234.1487:@thin Configuration.hs
-- @-leo
