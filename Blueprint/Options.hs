-- @+leo-ver=4-thin
-- @+node:gcross.20100602141408.1273:@thin Options.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100602152546.1277:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
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

import Blueprint.Identifier
-- @-node:gcross.20100602152546.1270:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100602195250.1300:Exceptions
-- @+node:gcross.20100602195250.1301:ConflictingOptionFormsException
data ConflictingOptionFormsException =
    ConflictingOptionFormsException 
    {   conflictingShortForms :: Map Char (Set OptionId)
    ,   conflictingLongForms :: Map String (Set OptionId)
    } deriving (Eq,Show,Typeable)

instance Exception ConflictingOptionFormsException
-- @-node:gcross.20100602195250.1301:ConflictingOptionFormsException
-- @-node:gcross.20100602195250.1300:Exceptions
-- @+node:gcross.20100602141408.1274:Types
-- @+node:gcross.20100903190909.1950:OptionId
data OfOption
type OptionId = Identifier OfOption
-- @-node:gcross.20100903190909.1950:OptionId
-- @+node:gcross.20100602141408.1275:Option
data Option =
    Option
    {   optionShortForms :: Set Char
    ,   optionLongForms :: Set String
    ,   optionArgumentType :: OptionArgumentType
    ,   optionDescription :: String
    } deriving (Eq,Show)
-- @-node:gcross.20100602141408.1275:Option
-- @+node:gcross.20100602152546.1272:OptionArgumentType
data OptionArgumentType =
    NoArgument { optionValueIfPresent :: String }
  | RequiredArgument { optionValueTypeDescription :: String }
  | OptionalArgument
    {   optionValueTypeDescription :: String
    ,   optionValueIfPresentButNotSpecified :: String
    }
  deriving (Eq,Show)
-- @-node:gcross.20100602152546.1272:OptionArgumentType
-- @+node:gcross.20100602152546.1274:OptionSpecification
data OptionSpecification =
    OptionSpecification
    {   optionSpecificationResolvedShortForms :: Map Char OptionId
    ,   optionSpecificationResolvedLongForms :: Map String OptionId
    ,   optionSpecificationOptions :: Map OptionId Option
    } deriving (Eq,Show)
-- @-node:gcross.20100602152546.1274:OptionSpecification
-- @+node:gcross.20100602152546.1279:OptionValues
type OptionValues = Map OptionId String
-- @-node:gcross.20100602152546.1279:OptionValues
-- @-node:gcross.20100602141408.1274:Types
-- @+node:gcross.20100602152546.1268:Functions
-- @+node:gcross.20100603132252.2071:processOptions
processOptions :: OptionSpecification → [String] → Either [String] (OptionValues,[String])
processOptions option_specification arguments =
    if null error_messages
        then Right (option_values,non_matching_arguments)
        else Left error_messages
  where
    (key_value_updates,non_matching_arguments,error_messages) =
        GetOpt.getOpt
            GetOpt.Permute
            (computeGetOptDescriptors option_specification)
            arguments

    option_values = Map.fromList key_value_updates
-- @-node:gcross.20100603132252.2071:processOptions
-- @+node:gcross.20100602152546.1269:computeGetOptDescriptors
computeGetOptDescriptors :: OptionSpecification → [GetOpt.OptDescr (OptionId,String)]
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
-- @-node:gcross.20100602152546.1269:computeGetOptDescriptors
-- @+node:gcross.20100602152546.1876:createOptionSpecification
createOptionSpecification = createOptionSpecificationWithResolvedConflicts Map.empty Map.empty
-- @-node:gcross.20100602152546.1876:createOptionSpecification
-- @+node:gcross.20100602152546.1273:createOptionSpecificationWithResolvedConflicts
createOptionSpecificationWithResolvedConflicts ::
    Map Char OptionId →
    Map String OptionId →
    Map OptionId Option →
    OptionSpecification
createOptionSpecificationWithResolvedConflicts
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
-- @nonl
-- @-node:gcross.20100602152546.1273:createOptionSpecificationWithResolvedConflicts
-- @+node:gcross.20100602152546.1882:findConflicts
findConflicts :: Ord a => Set a → (b → OptionId) → (b → Set a) → [b] → Map a (Set OptionId)
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
                (\new_set → Map.insert value new_set conflicts)
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
-- @nonl
-- @-node:gcross.20100602152546.1882:findConflicts
-- @+node:gcross.20100602195250.1296:filterConflictsAndConvertToList
filterConflictsAndConvertToList :: Ord a => OptionId → Map a OptionId → Set a → [a]
filterConflictsAndConvertToList key conflict_resolutions =
    filter (
        maybe True (== key)
        .
        flip Map.lookup conflict_resolutions
    )
    .
    Set.toList
-- @-node:gcross.20100602195250.1296:filterConflictsAndConvertToList
-- @+node:gcross.20100603132252.1336:options
options :: [(OptionId,[Char],[String],OptionArgumentType,String)] → Map OptionId Option
options =
    Map.fromList
    .
    map (
        \(key,short_forms,long_forms,argument_type,description) →
            (key
            ,Option
                (Set.fromList short_forms)
                (Set.fromList long_forms)
                argument_type
                description
            )
    )
-- @-node:gcross.20100603132252.1336:options
-- @-node:gcross.20100602152546.1268:Functions
-- @-others
-- @-node:gcross.20100602141408.1273:@thin Options.hs
-- @-leo
