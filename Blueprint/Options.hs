-- @+leo-ver=4-thin
-- @+node:gcross.20100903200211.2235:@thin Options.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100903200211.2236:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100903200211.2236:<< Language extensions >>
-- @nl

module Blueprint.Options where

-- @<< Import needed modules >>
-- @+node:gcross.20100903200211.2237:<< Import needed modules >>
import Control.Arrow
import Control.Exception
import Control.Monad

import Data.ConfigFile
import Data.Either
import Data.Either.Unwrap
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Typeable

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit

import Blueprint.Identifier
import Blueprint.Miscellaneous
-- @-node:gcross.20100903200211.2237:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100903200211.2257:Exceptions
-- @+node:gcross.20100903200211.2258:OptionConflictsException
data ConflictingOptionsException = ConflictingOptionsException Conflicts deriving (Typeable,Show)

instance Exception ConflictingOptionsException

-- @-node:gcross.20100903200211.2258:OptionConflictsException
-- @+node:gcross.20100905161144.1927:ConfigurationFileException
data ConfigurationFileErrors = ConfigurationFileErrors FilePath [CPError] deriving (Show,Eq,Typeable)

instance Exception ConfigurationFileErrors

-- @-node:gcross.20100905161144.1927:ConfigurationFileException
-- @-node:gcross.20100903200211.2257:Exceptions
-- @+node:gcross.20100903200211.2238:Types
-- @+node:gcross.20100903200211.2240:OptionId
data OfOption
type OptionId = Identifier OfOption
-- @-node:gcross.20100903200211.2240:OptionId
-- @+node:gcross.20100903200211.2241:ArgumentType
data ArgumentType =
    NoArgument
    {   argumentDefaultValue :: String
    }
  | OptionalArgument
    {   argumentDescription :: String
    ,   argumentDefaultValue :: String
    }
  | RequiredArgument 
    {   argumentDescription :: String
    }
  deriving Show
-- @-node:gcross.20100903200211.2241:ArgumentType
-- @+node:gcross.20100903200211.2243:Options
data Options = Options
    {   optionShortForms :: Map Char (OptionId,ArgumentType)
    ,   optionLongForms :: Map String (OptionId,ArgumentType)
    ,   optionConfigurationKeys :: Map String OptionId
    ,   optionValueCombiners :: Map OptionId ([String] → Either String String)
    ,   optionDescriptions :: Map OptionId (String,String)
    }
-- @-node:gcross.20100903200211.2243:Options
-- @+node:gcross.20100905161144.1934:OptionValues
type OptionValues = Map OptionId String
-- @-node:gcross.20100905161144.1934:OptionValues
-- @+node:gcross.20100903200211.2248:Conflicts
data Conflicts = Conflicts
    {   conflictingShortForms :: Map Char [OptionId]
    ,   conflictingLongForms :: Map String [OptionId]
    ,   conflictingConfigurationKeys :: Map String [OptionId]
    ,   conflictingValueCombiners :: Map OptionId Int
    ,   conflictingDescriptions :: Map OptionId [(String,String)]
    } deriving (Eq,Show)
-- @-node:gcross.20100903200211.2248:Conflicts
-- @-node:gcross.20100903200211.2238:Types
-- @+node:gcross.20100903200211.2246:Instances
-- @+node:gcross.20100903200211.2250:Monoid Conflicts
instance Monoid Conflicts where
    mempty = Conflicts Map.empty Map.empty Map.empty Map.empty Map.empty
    c1 `mappend` c2 =
        Conflicts
        {   conflictingShortForms = (Map.unionWith (++) `on` conflictingShortForms) c1 c2
        ,   conflictingLongForms = (Map.unionWith (++) `on` conflictingLongForms) c1 c2
        ,   conflictingConfigurationKeys = (Map.unionWith (++) `on` conflictingConfigurationKeys) c1 c2
        ,   conflictingValueCombiners = (Map.unionWith (+) `on` conflictingValueCombiners) c1 c2
        ,   conflictingDescriptions = (Map.unionWith (++) `on` conflictingDescriptions) c1 c2
        }
-- @-node:gcross.20100903200211.2250:Monoid Conflicts
-- @+node:gcross.20100903200211.2253:Monoid Options
instance Monoid Options where
    mempty = Options Map.empty Map.empty Map.empty Map.empty Map.empty
    o1 `mappend` o2 =
        Options
        {   optionShortForms = (Map.union `on` optionShortForms) o1 o2
        ,   optionLongForms = (Map.union `on` optionLongForms) o1 o2
        ,   optionConfigurationKeys = (Map.union `on` optionConfigurationKeys) o1 o2
        ,   optionValueCombiners = (Map.union `on` optionValueCombiners) o1 o2
        ,   optionDescriptions = (Map.union `on` optionDescriptions) o1 o2
        }
-- @-node:gcross.20100903200211.2253:Monoid Options
-- @+node:gcross.20100903200211.2249:Monoid (Either Conflicts Options)
instance Monoid (Either Conflicts Options) where
    mempty = Right mempty
    Left x `mappend` Left y = Left (x `mappend` y)
    x@(Right _) `mappend` y@(Left _) = y `mappend` x
    Left Conflicts{..} `mappend` Right Options{..} =
        Left $ Conflicts
        {   conflictingShortForms =
                intersectAndUnion ((:) . fst)
                    optionShortForms
                    conflictingShortForms
        ,   conflictingLongForms =
                intersectAndUnion ((:) . fst)
                    optionLongForms
                    conflictingLongForms
        ,   conflictingConfigurationKeys =
                intersectAndUnion (:)
                    optionConfigurationKeys
                    conflictingConfigurationKeys
        ,   conflictingValueCombiners =
                intersectAndUnion (const (+1))
                    optionValueCombiners
                    conflictingValueCombiners
        ,   conflictingDescriptions =
                intersectAndUnion (:)
                    optionDescriptions
                    conflictingDescriptions
        }
    Right o1 `mappend` Right o2
      | conflicts == mempty = Left conflicts
      | otherwise           = Right (o1 `mappend` o2)
      where
        conflicts =
            Conflicts
            {   conflictingShortForms =
                    (Map.intersectionWith (doubleton `on` fst) `on` optionShortForms) o1 o2
            ,   conflictingLongForms =
                    (Map.intersectionWith (doubleton `on` fst) `on` optionLongForms) o1 o2
            ,   conflictingConfigurationKeys =
                    (Map.intersectionWith doubleton `on` optionConfigurationKeys) o1 o2
            ,   conflictingValueCombiners =
                    (Map.intersectionWith (\_ _ → 2) `on` optionValueCombiners) o1 o2
            ,   conflictingDescriptions =
                    (Map.intersectionWith doubleton `on` optionDescriptions) o1 o2
            }
-- @-node:gcross.20100903200211.2249:Monoid (Either Conflicts Options)
-- @-node:gcross.20100903200211.2246:Instances
-- @+node:gcross.20100903200211.2255:Functions
-- @+node:gcross.20100903200211.2256:extractOptionsOrError
extractOptionsOrError :: Either Conflicts Options → Options
extractOptionsOrError = either (throw . ConflictingOptionsException) id
-- @-node:gcross.20100903200211.2256:extractOptionsOrError
-- @+node:gcross.20100903200211.2259:parseCommandLine
parseCommandLine :: Options → [String] → Either [String] ([String],OptionValues)
parseCommandLine Options{..} arguments =
    case error_messages of
        [] → Right (leftovers,options)
        _ → Left error_messages
  where
    (error_messages,options) =
        ((++parsing_error_messages) *** Map.fromList)
        .
        partitionEithers
        .
        map (\(option_id,values) →
            case values of
                [x] → Right (option_id,x)
                xs → case Map.lookup option_id optionValueCombiners of
                    Nothing → Left $
                        "Multiple incompatible values specified for option "
                        ++ show option_id ++
                        ": ["
                        ++ intercalate " " values ++
                        "]"
                    Just combine → mapRight (option_id,) (combine values)
        )
        .
        gather
        $
        parsed_options
    (parsed_options,leftovers,parsing_error_messages) = getOpt Permute descriptors arguments
    descriptors =
        [ computeOption [short_form] [] option_id argument_type
        | (short_form,(option_id,argument_type)) ← Map.toList optionShortForms
        ]
        ++
        [ computeOption [] [long_form] option_id argument_type
        | (long_form,(option_id,argument_type)) ← Map.toList optionLongForms
        ]

    computeOption :: [Char] → [String] → OptionId → ArgumentType → OptDescr (OptionId,String)
    computeOption short_forms long_forms option_id argument_type =
        Option
            short_forms
            long_forms
            (case argument_type of
                NoArgument{..} → NoArg (option_id,argumentDefaultValue)
                OptionalArgument{..} → OptArg (maybe (option_id,argumentDefaultValue) (option_id,)) undefined
                RequiredArgument{..} → ReqArg (option_id,) undefined
            )
            undefined

-- @-node:gcross.20100903200211.2259:parseCommandLine
-- @+node:gcross.20100905161144.1926:parseConfigurationFile
parseConfigurationFile :: Options → ConfigParser → Either [CPError] OptionValues
parseConfigurationFile Options{optionConfigurationKeys} cp =
    let (parse_errors,values) =
            partitionEithers
            .
            catMaybes
            .
            map (\(configuration_key,option_id) →
                case interpolatingAccess 10 cp "DEFAULT" configuration_key of
                    Right value → Just (Right (option_id,value))
                    Left (NoOption _,_) → Nothing
                    Left cp_error → Just (Left cp_error)
            )
            .
            Map.toList
            $
            optionConfigurationKeys
    in case parse_errors of
        [] → Right (Map.fromList values)
        errors → Left errors
-- @-node:gcross.20100905161144.1926:parseConfigurationFile
-- @+node:gcross.20100905161144.1929:updateConfigurationFile
updateConfigurationFile :: Options → ConfigParser → OptionValues → Maybe ConfigParser
updateConfigurationFile Options{optionConfigurationKeys} cp option_values
  | config_file_updated = Just new_config_file
  | otherwise           = Nothing
  where
    (config_file_updated,new_config_file) = foldl' -- '
        (\same@(config_file_updated,current_config_file) (configuration_key,option_id) →
            case Map.lookup option_id option_values of
                Just new_value →
                    case interpolatingAccess 10 current_config_file "DEFAULT" configuration_key of
                        Right old_value
                          | new_value == "" →
                            (True
                            ,fromRight (remove_option current_config_file "DEFAULT" configuration_key)
                            )
                          | new_value == old_value → same
                        _ → (True
                            ,fromRight (set current_config_file "DEFAULT" configuration_key new_value)
                            )
                _ → same
        )
        (False,cp)
        .
        Map.toList
        $
        optionConfigurationKeys
-- @-node:gcross.20100905161144.1929:updateConfigurationFile
-- @+node:gcross.20100903200211.2261:getAndParseCommandLine
getAndParseCommandLineOptions :: Options → IO ([String],OptionValues)
getAndParseCommandLineOptions =
    flip fmap getArgs . parseCommandLine
    >=>
    either
        (\error_messages → do
            putStrLn "Error parsing the command line options:"
            mapM_ (putStrLn . ("* " ++)) error_messages
            exitFailure
        )
        return
-- @-node:gcross.20100903200211.2261:getAndParseCommandLine
-- @+node:gcross.20100905161144.1930:getParseAndUpdateConfigurationFile
getParseAndUpdateConfigurationFile :: Options → FilePath → OptionValues → IO OptionValues
getParseAndUpdateConfigurationFile options configuration_filepath old_option_values = do
    cp ← do
        exists ← doesFileExist configuration_filepath
        if exists
            then do
                readfile emptyCP configuration_filepath
                >>=
                either
                    (throwIO . ConfigurationFileErrors configuration_filepath . (:[]))
                    return
            else return emptyCP
    new_option_values ←
        either
            (throwIO . ConfigurationFileErrors configuration_filepath)
            (return . mappend old_option_values)
        (parseConfigurationFile options cp)
    case updateConfigurationFile options cp new_option_values of
        Nothing → return ()
        Just updated_cp → writeFile configuration_filepath (to_string updated_cp)
    return . Map.filter (/= "") $ new_option_values
-- @-node:gcross.20100905161144.1930:getParseAndUpdateConfigurationFile
-- @+node:gcross.20100905161144.1931:loadOptions
loadOptions :: FilePath → Options → IO ([String],OptionValues)
loadOptions configuration_filepath options = do
    (leftovers,command_line_option_values) ← getAndParseCommandLineOptions options
    fmap (leftovers,) $
        getParseAndUpdateConfigurationFile options configuration_filepath command_line_option_values
-- @-node:gcross.20100905161144.1931:loadOptions
-- @-node:gcross.20100903200211.2255:Functions
-- @-others
-- @-node:gcross.20100903200211.2235:@thin Options.hs
-- @-leo
