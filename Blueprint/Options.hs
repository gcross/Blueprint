-- @+leo-ver=4-thin
-- @+node:gcross.20100903200211.2235:@thin Options.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100903200211.2236:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
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
import System.Environment
import System.Exit

import Blueprint.Identifier
import Blueprint.Miscellaneous
-- @-node:gcross.20100903200211.2237:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100903200211.2257:Exceptions
-- @+node:gcross.20100903200211.2258:OptionConflictsExceptions
data ConflictingOptionsException = ConflictingOptionsException Conflicts deriving (Typeable,Show)

instance Exception ConflictingOptionsException

-- @-node:gcross.20100903200211.2258:OptionConflictsExceptions
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
    ,   optionConflictResolutions :: Map OptionId ([String] → Either String String)
    ,   optionDescriptions :: Map OptionId (String,String)
    }
-- @-node:gcross.20100903200211.2243:Options
-- @+node:gcross.20100903200211.2248:Conflicts
data Conflicts = Conflicts
    {   conflictingShortForms :: Map Char [OptionId]
    ,   conflictingLongForms :: Map String [OptionId]
    ,   conflictingConfigurationKeys :: Map String [OptionId]
    ,   conflictingConflictResolutions :: Map OptionId Int
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
        ,   conflictingConflictResolutions = (Map.unionWith (+) `on` conflictingConflictResolutions) c1 c2
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
        ,   optionConflictResolutions = (Map.union `on` optionConflictResolutions) o1 o2
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
        ,   conflictingConflictResolutions =
                intersectAndUnion (const (+1))
                    optionConflictResolutions
                    conflictingConflictResolutions
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
            ,   conflictingConflictResolutions =
                    (Map.intersectionWith (\_ _ → 2) `on` optionConflictResolutions) o1 o2
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
-- @+node:gcross.20100903200211.2259:parseCommandLineOptions
parseCommandLineOptions :: Options → [String] → Either [String] ([String],Map OptionId String)
parseCommandLineOptions Options{..} arguments =
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
                xs → case Map.lookup option_id optionConflictResolutions of
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
        [ Option
            [short_form]
            []
            (case argument_type of
                NoArgument{..} → NoArg (option_id,argumentDefaultValue)
                OptionalArgument{..} → OptArg (maybe (option_id,argumentDefaultValue) (option_id,)) undefined
                RequiredArgument{..} → ReqArg (option_id,) undefined
            )
            undefined
        | (short_form,(option_id,argument_type)) ← Map.toList optionShortForms
        ]
-- @nonl
-- @-node:gcross.20100903200211.2259:parseCommandLineOptions
-- @+node:gcross.20100903200211.2261:getAndParseCommandLineOptions
getAndParseCommandLineOptions :: Options → IO ([String],Map OptionId String)
getAndParseCommandLineOptions =
    flip fmap getArgs . parseCommandLineOptions
    >=>
    \parse_results →
        case parse_results of
            Left error_messages → do
                putStrLn "Error parsing the command line options:"
                mapM_ (putStrLn . ("* " ++)) error_messages
                exitFailure
            Right options →
                return options
-- @-node:gcross.20100903200211.2261:getAndParseCommandLineOptions
-- @-node:gcross.20100903200211.2255:Functions
-- @-others
-- @-node:gcross.20100903200211.2235:@thin Options.hs
-- @-leo
