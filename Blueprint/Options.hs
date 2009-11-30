-- @+leo-ver=4-thin
-- @+node:gcross.20091129000542.1450:@thin Options.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091129000542.1451:<< Language extensions >>
-- @-node:gcross.20091129000542.1451:<< Language extensions >>
-- @nl

module Blueprint.Options where

-- @<< Import needed modules >>
-- @+node:gcross.20091129000542.1452:<< Import needed modules >>
import Control.Applicative hiding (empty)
import Control.Applicative.Infix
import Control.Arrow hiding ((<+>))
import Control.Monad
import Control.Monad.Error

import Data.Dynamic
import Data.Either
import Data.Either.Unwrap
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid

import qualified System.Console.GetOpt as GetOpt
import System.Environment
import System.IO.Unsafe

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blueprint.Error
-- @-node:gcross.20091129000542.1452:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091129000542.1489:Values
-- @+node:gcross.20091129000542.1490:noOptions
noOptions :: ParsedOptions
noOptions = Map.empty
-- @nonl
-- @-node:gcross.20091129000542.1490:noOptions
-- @-node:gcross.20091129000542.1489:Values
-- @+node:gcross.20091129000542.1453:Types
-- @+node:gcross.20091129000542.1454:Option
data Option = Option
    {   optionName :: String
    ,   optionShortForms :: [Char]
    ,   optionLongForms :: [String]
    ,   optionArgumentExpectation :: ArgumentExpectation
    ,   optionDescription :: String
    }
-- @-node:gcross.20091129000542.1454:Option
-- @+node:gcross.20091129000542.1462:OptionSection
data OptionSection = OptionSection
    {   optionSectionHeading :: String
    ,   optionSectionOptions :: [Option]
    ,   optionSectionPostprocessor :: Map String [Maybe String] -> Either Doc Dynamic
    }
-- @-node:gcross.20091129000542.1462:OptionSection
-- @+node:gcross.20091129000542.1458:ArgumentExpectation
data ArgumentExpectation =
    NoArgumentExpected
  | ArgumentRequired String
  | ArgumentOptional String
-- @-node:gcross.20091129000542.1458:ArgumentExpectation
-- @+node:gcross.20091129000542.1473:ParsedOptionValue
type ParsedOptionValue = Map String (Map String [Maybe String]) -> Map String (Map String [Maybe String])
-- @-node:gcross.20091129000542.1473:ParsedOptionValue
-- @+node:gcross.20091129000542.1496:ParsedOptions
type ParsedOptions = Map String Dynamic
-- @-node:gcross.20091129000542.1496:ParsedOptions
-- @+node:gcross.20091129000542.1472:OptionDescriptor
type OptionDescriptor = GetOpt.OptDescr ParsedOptionValue
-- @nonl
-- @-node:gcross.20091129000542.1472:OptionDescriptor
-- @+node:gcross.20091129000542.1474:ArgumentDescriptor
type ArgumentDescriptor = GetOpt.ArgDescr ParsedOptionValue
-- @-node:gcross.20091129000542.1474:ArgumentDescriptor
-- @-node:gcross.20091129000542.1453:Types
-- @+node:gcross.20091129000542.1455:Functions
-- @+node:gcross.20091129000542.1456:toOptionDescriptor
toOptionDescriptor :: String -> Option -> OptionDescriptor
toOptionDescriptor section_heading =
    GetOpt.Option
        <$> optionShortForms
        <*> optionLongForms
        <*> (liftA2 (toArgumentDescriptor section_heading)
                optionName
                optionArgumentExpectation
            )
        <*> optionDescription
-- @-node:gcross.20091129000542.1456:toOptionDescriptor
-- @+node:gcross.20091129000542.1457:toArgumentDescriptor
toArgumentDescriptor :: String -> String -> ArgumentExpectation -> ArgumentDescriptor
toArgumentDescriptor section_name option_name argument_expectation =
    case argument_expectation of
        NoArgumentExpected -> GetOpt.NoArg (addToSection Nothing)
        ArgumentRequired datatype -> GetOpt.ReqArg (addToSection . Just) datatype
        ArgumentOptional datatype -> GetOpt.OptArg (addToSection) datatype
  where
    addToSection :: Maybe String -> ParsedOptionValue
    addToSection value old_map =
        case Map.lookup section_name old_map of
            Nothing ->
                Map.insert section_name (Map.singleton option_name [value]) old_map
            Just section_options_map ->
                flip (Map.insert section_name) old_map
                .
                flip (Map.insert option_name) section_options_map
                $
                case Map.lookup option_name section_options_map of
                    Nothing -> [value]
                    Just old_values -> (value:old_values)
-- @-node:gcross.20091129000542.1457:toArgumentDescriptor
-- @+node:gcross.20091129000542.1465:removeDuplicateSections
removeDuplicateSections :: [OptionSection] -> [OptionSection]
removeDuplicateSections = nubBy (\x y -> optionSectionHeading x == optionSectionHeading y)
-- @-node:gcross.20091129000542.1465:removeDuplicateSections
-- @+node:gcross.20091129000542.1478:createHelpMessageForSection
createHelpMessageForSection :: OptionSection -> Doc
createHelpMessageForSection section =
    vcat
    .
    map text
    .
    lines
    .
    GetOpt.usageInfo (optionSectionHeading section)
    .
    createOptionDescriptorsForSection
    $
    section
-- @-node:gcross.20091129000542.1478:createHelpMessageForSection
-- @+node:gcross.20091129000542.1477:createHelpMessageForOptionSections
createHelpMessageForOptionSections :: [OptionSection] -> Doc
createHelpMessageForOptionSections =
    vcat    
    .
    map createHelpMessageForSection
    .
    removeDuplicateSections

-- @-node:gcross.20091129000542.1477:createHelpMessageForOptionSections
-- @+node:gcross.20091129000542.1486:createDefaultHelpMessage
createDefaultHelpMessage :: [OptionSection] -> [String] -> Doc
createDefaultHelpMessage option_sections target_names =
    text "Usage: Setup <target> [options...]"
    <$$>
    empty
    <$$>
    createHelpMessageForOptionSections option_sections
    <$$>
    empty
    <$$>
    text "Available targets:"
    <$$>
    (indent 4 . vcat . map text $ target_names)
-- @-node:gcross.20091129000542.1486:createDefaultHelpMessage
-- @+node:gcross.20091129000542.1471:createOptionDescriptorsForSection
createOptionDescriptorsForSection :: OptionSection -> [OptionDescriptor]
createOptionDescriptorsForSection =
    liftA2 map
        (toOptionDescriptor . optionSectionHeading)
        optionSectionOptions
-- @-node:gcross.20091129000542.1471:createOptionDescriptorsForSection
-- @+node:gcross.20091129000542.1475:formatSectionPostprocessingErrorMessage
formatSectionPostprocessingErrorMessage :: String -> Doc -> Doc
formatSectionPostprocessingErrorMessage = curry $
    uncurry (</>)
    .
    (text *** indent 4)
-- @-node:gcross.20091129000542.1475:formatSectionPostprocessingErrorMessage
-- @+node:gcross.20091129000542.1461:findConflictingOptions
findConflictingOptions :: [OptionSection] -> [(Either Char String, [(String,Int)])]
findConflictingOptions =
    uncurry (++)
    .
    (map (first Left) . findConflicts *** map (first Right) . findConflicts)
    .
    processSections Map.empty Map.empty
  where
    -- @    @+others
    -- @+node:gcross.20091129000542.1463:processSections
    processSections ::
        Map Char (Map String Int) ->
        Map String (Map String Int) ->
        [OptionSection] ->
        (Map Char (Map String Int),Map String (Map String Int))
    processSections processed_short_options processed_long_options [] =
        (processed_short_options,processed_long_options)
    processSections processed_short_options processed_long_options (section:rest_sections) =
        processSections
            (processOptions processed_short_options optionShortForms)
            (processOptions processed_long_options optionLongForms)
            rest_sections
      where
        tagWithSectionHeading x = (x,Map.singleton (optionSectionHeading section) 1)

        processOptions :: Ord a => Map a (Map String Int) -> (Option -> [a]) -> Map a (Map String Int)
        processOptions processed_options extractForms =
            foldl' processOption processed_options -- '
            .
            map extractForms
            .
            optionSectionOptions
            $
            section

        processOption :: Ord a => Map a (Map String Int) -> [a] -> Map a (Map String Int)
        processOption previously_seen_options =
            Map.unionWith (Map.unionWith (+)) previously_seen_options
            .
            Map.fromList
            .
            map tagWithSectionHeading
    -- @-node:gcross.20091129000542.1463:processSections
    -- @+node:gcross.20091129000542.1464:findConflicts
    findConflicts :: Ord a => Map a (Map String Int) -> [(a,[(String,Int)])]
    findConflicts = catMaybes . map (uncurry findConflictsForOption) . Map.assocs
      where
        findConflictsForOption option_form option_appearances
         | (head . Map.elems) option_appearances > 1 || Map.size option_appearances > 1
            = Just (option_form,Map.assocs option_appearances)
         | otherwise
            = Nothing
    -- @-node:gcross.20091129000542.1464:findConflicts
    -- @-others
-- @-node:gcross.20091129000542.1461:findConflictingOptions
-- @+node:gcross.20091129000542.1476:parseCommandLineOptions
parseCommandLineOptions :: [OptionSection] -> Either ErrorMessage (([String],Map String Dynamic))
parseCommandLineOptions = parseOptions (tail . unsafePerformIO $ getArgs)
-- @-node:gcross.20091129000542.1476:parseCommandLineOptions
-- @+node:gcross.20091129000542.1466:parseOptions
parseOptions :: [String] -> [OptionSection] -> Either ErrorMessage ([String],ParsedOptions)
parseOptions args sections_with_possible_duplicates = do
    when (not . null $ conflicts) $
        -- @        << Report that there were conflicting options. >>
        -- @+node:gcross.20091129000542.1469:<< Report that there were conflicting options. >>
        throwError
        .
        errorMessage "caused by the programmer"
        .
        nest 4
        .
        (text "The following command line options appear multiple times:" <$$>)
        .
        vcat
        .
        map (uncurry formatConflictingOption)
        $
        conflicts
        -- @-node:gcross.20091129000542.1469:<< Report that there were conflicting options. >>
        -- @nl
    let (results,non_options,error_messages) =
            GetOpt.getOpt GetOpt.Permute option_descriptors args
    when (not . null $ error_messages) $
        -- @        << Report that there were problems parsing the options. >>
        -- @+node:gcross.20091129000542.1470:<< Report that there were problems parsing the options. >>
        throwError
        .
        errorMessage "parsing command-line options"
        .
        (<$$> text "(Use the --help option to get more information on the allowed options.)")
        .
        vcat
        .
        map text
        $
        error_messages
        -- @-node:gcross.20091129000542.1470:<< Report that there were problems parsing the options. >>
        -- @nl
    let option_map :: Map String (Map String [Maybe String])
        option_map = go Map.empty results
          where
            go final_result [] = final_result
            go current_result (fn:rest_fns) = go (fn current_result) rest_fns
        (section_error_messages,processed_options) =
            partitionEithers
            .
            map (\section ->
                let heading = optionSectionHeading section
                in mapBoth ((,) heading) ((,) heading)
                   .
                   optionSectionPostprocessor section
                   .
                   fromMaybe Map.empty
                   .
                   Map.lookup heading
                   $
                   option_map
            )
            $
            sections
    if null section_error_messages
        then
            Right
            .
            (,) non_options
            .
            Map.fromList 
            $
            processed_options
        else
            Left
            .
            errorMessage "validating options"
            .
            vcat
            .
            map (uncurry formatSectionPostprocessingErrorMessage)
            $
            section_error_messages

  where
    sections = removeDuplicateSections sections_with_possible_duplicates
    conflicts = findConflictingOptions sections
    option_descriptors = concat . map createOptionDescriptorsForSection $ sections

    -- @    @+others
    -- @+node:gcross.20091129000542.1467:formatConflictingOption
    formatConflictingOption :: Either Char String -> [(String,Int)] -> Doc
    formatConflictingOption option_name conflicts =
        let option_name_heading =
                case option_name of
                    Left char -> '-':char:[]
                    Right string -> '-':'-':string
            conflict_strings =
                vcat
                .
                map (uncurry formatConflict)
                $
                conflicts
        in text option_name_heading </> conflict_strings
    -- @nonl
    -- @-node:gcross.20091129000542.1467:formatConflictingOption
    -- @+node:gcross.20091129000542.1468:formatConflict
    formatConflict :: String -> Int -> Doc
    formatConflict heading number_of_appearances =
        let appearance_string = text $
                case number_of_appearances of
                    1 -> "once"
                    2 -> "twice"
                    3 -> "thrice"
                    x -> show x ++ " times"
        in text heading <+> parens (text "appears" <+> appearance_string)
    -- @nonl
    -- @-node:gcross.20091129000542.1468:formatConflict
    -- @-others
-- @-node:gcross.20091129000542.1466:parseOptions
-- @+node:gcross.20091129000542.1483:isHelpFlag
isHelpFlag = (== "-h") <^(||)^> (== "--help") <^(||)^> (== "-?")
-- @-node:gcross.20091129000542.1483:isHelpFlag
-- @-node:gcross.20091129000542.1455:Functions
-- @-others
-- @-node:gcross.20091129000542.1450:@thin Options.hs
-- @-leo
