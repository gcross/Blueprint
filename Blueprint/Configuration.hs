-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Blueprint.Configuration where

-- Imports {{{
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
-- }}}

-- Exceptions {{{
data BadProgramVersionException = BadProgramVersionException FilePath String deriving (Show,Eq,Typeable)

instance Exception BadProgramVersionException
data ProgramDoesNotExistException = ProgramDoesNotExistException FilePath deriving (Show,Eq,Typeable)

instance Exception ProgramDoesNotExistException

data UnableToLocateProgramException = UnableToLocateProgramException FilePath [FilePath] deriving (Eq,Show,Typeable)

instance Exception UnableToLocateProgramException
-- }}}

-- Classes {{{
class Typeable a => ProgramName a where
    programNameFrom :: f a → String
-- }}}

-- Types {{{
data ProgramConfigurationOptionIds a = ProgramConfigurationOptionIds -- {{{
    {   optionIdForProgramLocation :: OptionId
    ,   optionIdForSearchPaths :: OptionId
    ,   optionIdForExtraArguments :: OptionId
    }
-- }}}
data ProgramConfigurationOptions a = ProgramConfigurationOptions -- {{{
    {   configurationOptionProgramLocation :: Maybe FilePath
    ,   configurationOptionSearchPaths :: [FilePath]
    ,   configurationOptionExtraArguments :: [String]
    } deriving (Eq,Show,Typeable)

$(derive makeBinary ''ProgramConfigurationOptions)
-- }}}
data ProgramConfiguration a = ProgramConfiguration -- {{{
    {   programFilePath :: FilePath
    ,   programExtraArguments :: [String]
    } deriving (Eq,Show,Typeable)

$(derive makeBinary ''ProgramConfiguration)
-- }}}
newtype OptionsFor a = OptionsFor { unwrapOptions :: Options }
-- }}}

-- Functions {{{
configureProgram :: -- {{{
    ProgramName α ⇒
    ProgramConfigurationOptions α →
    Job (ProgramConfiguration α)
configureProgram options@ProgramConfigurationOptions{..} =
    once my_id
    .
    cache my_id
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
    my_id =
        identifierInNamespace
            (uuid "432fb242-cc22-4827-baee-70a7443f9b3b")
            ("configuring " ++ program_name)

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
-- }}}
configureProgramUsingOptions :: -- {{{
    ProgramName α ⇒
    OptionValues →
    Job (ProgramConfiguration α)
configureProgramUsingOptions = configureProgram . extractProgramConfigurationOptions
-- }}}
determineProgramVersion :: -- {{{
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
-- }}}
extractProgramConfigurationOptions :: -- {{{
    ProgramName α ⇒
    OptionValues →
    ProgramConfigurationOptions α
extractProgramConfigurationOptions = extractProgramConfigurationOptionsUsing programOptionIds
-- }}}
extractProgramConfigurationOptionsUsing :: -- {{{
    ProgramConfigurationOptionIds α →
    OptionValues →
    ProgramConfigurationOptions α
extractProgramConfigurationOptionsUsing ProgramConfigurationOptionIds{..} =
    ProgramConfigurationOptions
        <$> Map.lookup optionIdForProgramLocation
        <*> maybe [] splitSearchPath . Map.lookup optionIdForSearchPaths
        <*> maybe [] words . Map.lookup optionIdForExtraArguments
-- }}}
programOptionIds :: ProgramName α ⇒ ProgramConfigurationOptionIds α -- {{{
programOptionIds =
    let option_ids@ProgramConfigurationOptionIds{..} =
            ProgramConfigurationOptionIds
                (identifierInNamespace
                    program_configuration_namespace
                    ("path to " ++ program_name)
                )
                (identifierInNamespace
                    program_configuration_namespace
                    ("paths to search for " ++ program_name)
                )
                (identifierInNamespace
                    program_configuration_namespace
                    ("additional arguments for " ++ program_name)
                )
        program_name = programNameFrom option_ids
    in option_ids
-- }}}
programOptionIdsFor :: ProgramName α ⇒ OptionsFor α → ProgramConfigurationOptionIds α -- {{{
programOptionIdsFor = const programOptionIds
-- }}}
programOptions :: ProgramName α ⇒ OptionsFor α -- {{{
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
-- }}}
-- }}}

-- Namespaces {{{
program_configuration_namespace = uuid "8ea8f257-d1a1-4b6f-8ad0-0abd8de10211"
-- }}}
