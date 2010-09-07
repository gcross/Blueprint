-- @+leo-ver=4-thin
-- @+node:gcross.20100830091258.2004:@thin Tools.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100830091258.2005:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100830091258.2005:<< Language extensions >>
-- @nl

module Blueprint.Configuration.Tools where

-- @<< Import needed modules >>
-- @+node:gcross.20100830091258.2006:<< Import needed modules >>
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
import Blueprint.Jobs
import Blueprint.Jobs.Combinators
import Blueprint.Miscellaneous
import Blueprint.Options
-- @-node:gcross.20100830091258.2006:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100905161144.1938:Exceptions
-- @+node:gcross.20100905161144.1939:BadProgramVersionException
data BadProgramVersionException = BadProgramVersionException FilePath String deriving (Show,Eq,Typeable)

instance Exception BadProgramVersionException
-- @-node:gcross.20100905161144.1939:BadProgramVersionException
-- @+node:gcross.20100906112631.1979:ProgramDoesNotExistException
data ProgramDoesNotExistException = ProgramDoesNotExistException FilePath deriving (Show,Eq,Typeable)

instance Exception ProgramDoesNotExistException

-- @-node:gcross.20100906112631.1979:ProgramDoesNotExistException
-- @+node:gcross.20100906112631.1980:UnableToLocateProgramException
data UnableToLocateProgramException = UnableToLocateProgramException FilePath [FilePath] deriving (Eq,Show,Typeable)

instance Exception UnableToLocateProgramException
-- @nonl
-- @-node:gcross.20100906112631.1980:UnableToLocateProgramException
-- @-node:gcross.20100905161144.1938:Exceptions
-- @+node:gcross.20100906112631.2124:Classes
-- @+node:gcross.20100906112631.2125:ProgramName
class Typeable a => ProgramName a where
    programNameFrom :: f a → String
-- @-node:gcross.20100906112631.2125:ProgramName
-- @-node:gcross.20100906112631.2124:Classes
-- @+node:gcross.20100906112631.1981:Types
-- @+node:gcross.20100906112631.2121:ProgramConfigurationOptionIds
data ProgramConfigurationOptionIds a = ProgramConfigurationOptionIds
    {   optionIdForProgramLocation :: OptionId
    ,   optionIdForSearchPaths :: OptionId
    ,   optionIdForExtraArguments :: OptionId
    }
-- @-node:gcross.20100906112631.2121:ProgramConfigurationOptionIds
-- @+node:gcross.20100906112631.1982:ProgramConfigurationOptions
data ProgramConfigurationOptions a = ProgramConfigurationOptions
    {   configurationOptionProgramLocation :: Maybe FilePath
    ,   configurationOptionSearchPaths :: [FilePath]
    ,   configurationOptionExtraArguments :: [String]
    } deriving (Eq,Show,Typeable)

$(derive makeBinary ''ProgramConfigurationOptions)
-- @-node:gcross.20100906112631.1982:ProgramConfigurationOptions
-- @+node:gcross.20100906112631.1985:ProgramConfiguration
data ProgramConfiguration a = ProgramConfiguration
    {   programFilePath :: FilePath
    ,   programExtraArguments :: [String]
    } deriving (Eq,Show,Typeable)

$(derive makeBinary ''ProgramConfiguration)
-- @-node:gcross.20100906112631.1985:ProgramConfiguration
-- @+node:gcross.20100906112631.2139:OptionsFor
newtype OptionsFor a = OptionsFor { unwrapOptions :: Options }
-- @-node:gcross.20100906112631.2139:OptionsFor
-- @-node:gcross.20100906112631.1981:Types
-- @+node:gcross.20100830091258.2007:Functions
-- @+node:gcross.20100906112631.2118:configureProgram
configureProgram ::
    ProgramName a =>
    String →
    ProgramConfigurationOptions a →
    JobApplicative JobId Dynamic (ProgramConfiguration a)
configureProgram job_distinguisher configuration_options =
    JobApplicative
    {   jobApplicativeJobs = [job]
    ,   jobApplicativeResultJobNames = job_names
    ,   jobApplicativeResultExtractorJobName =
            identifierInNamespace
                program_configuration_namespace
                (job_distinguisher ++ program_name ++ "Y")
                ("configure " ++ program_name)
    ,   jobApplicativeResultExtractor = fromJust . fromDynamic . head
    }
  where
    job@(Job job_names job_runner) = createProgramConfigurationJob job_distinguisher configuration_options
    program_name = programNameFrom configuration_options
-- @-node:gcross.20100906112631.2118:configureProgram
-- @+node:gcross.20100906112631.2136:configureProgramUsingOptions
configureProgramUsingOptions ::
    ProgramName a =>
    OptionValues →
    JobApplicative JobId Dynamic (ProgramConfiguration a)
configureProgramUsingOptions =
    configureProgram ""
    .
    extractProgramConfigurationOptions
-- @-node:gcross.20100906112631.2136:configureProgramUsingOptions
-- @+node:gcross.20100906112631.2116:createProgramConfigurationJob
createProgramConfigurationJob ::
    ProgramName a =>
    String →
    ProgramConfigurationOptions a →
    Job JobId Dynamic
createProgramConfigurationJob
    job_distinguisher
    options@ProgramConfigurationOptions{..}
    =
    jobWithCache
        [identifierInNamespace
            program_configuration_namespace
            (program_name ++ job_distinguisher ++ "X")
            (program_name ++ " configuration")
        ]
        (liftIO . configureIt
         >=>
         \path_to_program →
            returnWrappedValueAndCache
                (noop options $ ProgramConfiguration path_to_program configurationOptionExtraArguments)
                (options, path_to_program)
        )
 where
    program_name = programNameFrom options

    noop :: ProgramConfigurationOptions a → ProgramConfiguration a → ProgramConfiguration a
    noop = const id

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
-- @-node:gcross.20100906112631.2116:createProgramConfigurationJob
-- @+node:gcross.20100905161144.1941:determineProgramVersion
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
-- @-node:gcross.20100905161144.1941:determineProgramVersion
-- @+node:gcross.20100906112631.2123:extractProgramConfigurationOptionsUsing
extractProgramConfigurationOptionsUsing ::
    ProgramConfigurationOptionIds a →
    OptionValues →
    ProgramConfigurationOptions a
extractProgramConfigurationOptionsUsing ProgramConfigurationOptionIds{..} =
    ProgramConfigurationOptions
        <$> Map.lookup optionIdForProgramLocation
        <*> maybe [] splitSearchPath . Map.lookup optionIdForSearchPaths
        <*> maybe [] words . Map.lookup optionIdForExtraArguments
-- @-node:gcross.20100906112631.2123:extractProgramConfigurationOptionsUsing
-- @+node:gcross.20100906112631.2138:extractProgramConfigurationOptions
extractProgramConfigurationOptions ::
    ProgramName a =>
    OptionValues →
    ProgramConfigurationOptions a
extractProgramConfigurationOptions = extractProgramConfigurationOptionsUsing programOptionIds
-- @-node:gcross.20100906112631.2138:extractProgramConfigurationOptions
-- @+node:gcross.20100906112631.2127:programOptionIds
programOptionIds :: ProgramName a => ProgramConfigurationOptionIds a
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
-- @-node:gcross.20100906112631.2127:programOptionIds
-- @+node:gcross.20100906112631.2154:programOptionIdsFor
programOptionIdsFor :: ProgramName a => OptionsFor a → ProgramConfigurationOptionIds a
programOptionIdsFor = const programOptionIds
-- @-node:gcross.20100906112631.2154:programOptionIdsFor
-- @+node:gcross.20100906112631.2135:programOptions
programOptions :: ProgramName a => OptionsFor a
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
-- @-node:gcross.20100906112631.2135:programOptions
-- @-node:gcross.20100830091258.2007:Functions
-- @+node:gcross.20100906112631.2119:Namespaces
program_configuration_namespace = uuid "8ea8f257-d1a1-4b6f-8ad0-0abd8de10211"
-- @nonl
-- @-node:gcross.20100906112631.2119:Namespaces
-- @-others
-- @-node:gcross.20100830091258.2004:@thin Tools.hs
-- @-leo
