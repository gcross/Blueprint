-- @+leo-ver=4-thin
-- @+node:gcross.20100602141408.1273:@thin Options.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100602152546.1277:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100602152546.1277:<< Language extensions >>
-- @nl

module Blueprint.Options where

-- @<< Import needed modules >>
-- @+node:gcross.20100602152546.1270:<< Import needed modules >>
import Control.Arrow
import Control.Exception

import Data.Either
import Data.Either.Unwrap
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable

import qualified System.Console.GetOpt as GetOpt
-- @-node:gcross.20100602152546.1270:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100602195250.1300:Exceptions
-- @+node:gcross.20100602195250.1301:ConflictingOptionFormsException
data ConflictingOptionFormsException =
    ConflictingOptionFormsException 
    {   conflictingShortForms :: Map Char (Set String)
    ,   conflictingLongForms :: Map String (Set String)
    } deriving (Show,Typeable)

instance Exception ConflictingOptionFormsException
-- @-node:gcross.20100602195250.1301:ConflictingOptionFormsException
-- @-node:gcross.20100602195250.1300:Exceptions
-- @+node:gcross.20100602141408.1274:Types
-- @+node:gcross.20100602141408.1275:Option
data Option =
    Option
    {   optionShortForms :: Set Char
    ,   optionLongForms :: Set String
    ,   optionDefaultValue :: String
    ,   optionArgumentType :: OptionArgumentType
    ,   optionDescription :: String
    }
-- @-node:gcross.20100602141408.1275:Option
-- @+node:gcross.20100602152546.1272:OptionArgument
data OptionArgumentType =
    NoArgument { optionValueIfPresent :: String }
  | RequiredArgument { optionValueTypeDescription :: String }
  | OptionalArgument
    {   optionValueTypeDescription :: String
    ,   optionValueIfPresentButNotSpecified :: String
    }
-- @-node:gcross.20100602152546.1272:OptionArgument
-- @+node:gcross.20100602152546.1274:OptionSpecification
data OptionSpecification =
    OptionSpecification
    {   optionSpecificationResolvedShortForms :: Map Char String
    ,   optionSpecificationResolvedLongForms :: Map String String
    ,   optionSpecificationOptions :: Map String Option
    }
-- @-node:gcross.20100602152546.1274:OptionSpecification
-- @+node:gcross.20100602152546.1279:OptionValues
type OptionValues = Map String String
-- @-node:gcross.20100602152546.1279:OptionValues
-- @-node:gcross.20100602141408.1274:Types
-- @+node:gcross.20100602152546.1268:Functions
-- @+node:gcross.20100602152546.1269:optionToOptDesc
computeGetOptDescriptors :: OptionSpecification → [GetOpt.OptDescr (String,String)]
computeGetOptDescriptors (OptionSpecification short_form_resolutions long_form_resolutions options) =
    map (uncurry makeDescriptor)
    .
    Map.toList
    $
    options
  where
    makeDescriptor key (Option{optionShortForms,optionLongForms,optionArgumentType,optionDescription}) =
        GetOpt.Option
            (filterConflictsAndConvertToList key short_form_resolutions optionShortForms)
            (filterConflictsAndConvertToList key long_form_resolutions optionLongForms)
            (case optionArgumentType of
                NoArgument value_if_present →
                    GetOpt.NoArg (key,value_if_present)
                RequiredArgument type_description →
                    GetOpt.ReqArg
                        ((,) key)
                        type_description
                OptionalArgument type_description value_if_present_but_not_specified →
                    GetOpt.OptArg
                        ((,) key . fromMaybe value_if_present_but_not_specified)
                        type_description
            )
            optionDescription
-- @-node:gcross.20100602152546.1269:optionToOptDesc
-- @+node:gcross.20100602152546.1876:createOptionSpecification
createOptionSpecification = createOptionSpecificationAcceptingDuplicateOptionForms Map.empty Map.empty

-- @-node:gcross.20100602152546.1876:createOptionSpecification
-- @+node:gcross.20100602152546.1273:createOptionSpecificationAcceptingDuplicateOptionForms
createOptionSpecificationAcceptingDuplicateOptionForms ::
    Map Char String →
    Map String String →
    Map String Option →
    OptionSpecification
createOptionSpecificationAcceptingDuplicateOptionForms
    short_form_conflict_resolutions
    long_form_conflict_resolutions
    options
    =
    if Map.null conflicting_short_forms && Map.null conflicting_long_forms
        then
            OptionSpecification
                short_form_conflict_resolutions
                long_form_conflict_resolutions
                options
        else throw $
            ConflictingOptionFormsException
                conflicting_short_forms
                conflicting_long_forms
 where
    options_as_list = Map.toList options

    conflicting_short_forms =
        findConflicts
            (Map.keysSet short_form_conflict_resolutions)
            fst
            (optionShortForms . snd)
        $
        options_as_list

    conflicting_long_forms =
        findConflicts
            (Map.keysSet long_form_conflict_resolutions)
            fst
            (optionLongForms . snd)
        $
        options_as_list
-- @-node:gcross.20100602152546.1273:createOptionSpecificationAcceptingDuplicateOptionForms
-- @+node:gcross.20100602152546.1882:findConflicts
findConflicts :: Ord a => Set a → (b → String) → (b → Set a) → [b] → Map a (Set String)
findConflicts values_to_ignore getName getValues =
    Map.filter ((> 1) . Set.size)
    .
    foldl' -- '
        addFoundConflicts
        Map.empty
  where
    addFoundConflicts conflicts x =
        Set.fold
            (\value conflicts →
                (\new_set -> Map.insert value new_set conflicts)
                .
                Set.insert name
                .
                fromMaybe Set.empty
                .
                Map.lookup value
                $
                conflicts
            )
            conflicts
            $
            getValues x `Set.difference` values_to_ignore
      where
        name = getName x
-- @-node:gcross.20100602152546.1882:findConflicts
-- @+node:gcross.20100602195250.1296:filterConflictsAndConvertToList
filterConflictsAndConvertToList :: Ord a => String → Map a String → Set a → [a]
filterConflictsAndConvertToList key conflict_resolutions =
    filter (
        maybe True (== key)
        .
        flip Map.lookup conflict_resolutions
    )
    .
    Set.toList
-- @-node:gcross.20100602195250.1296:filterConflictsAndConvertToList
-- @-node:gcross.20100602152546.1268:Functions
-- @-others
-- @-node:gcross.20100602141408.1273:@thin Options.hs
-- @-leo
