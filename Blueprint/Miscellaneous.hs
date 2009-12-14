-- @+leo-ver=4-thin
-- @+node:gcross.20091127142612.1413:@thin Miscellaneous.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091127142612.1414:<< Language extensions >>
-- @-node:gcross.20091127142612.1414:<< Language extensions >>
-- @nl

module Blueprint.Miscellaneous where

-- @<< Import needed modules >>
-- @+node:gcross.20091127142612.1415:<< Import needed modules >>
import Control.Parallel
import Control.Parallel.Strategies

import Data.Array
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Dynamic
import Data.Either.Unwrap
import Data.List
import Data.Typeable
import Data.Version

import System.Directory
import System.FilePath
import System.IO.Unsafe

import Text.ParserCombinators.ReadP
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString.Lazy
-- @-node:gcross.20091127142612.1415:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091127142612.1416:Functions
-- @+node:gcross.20091214092727.1585:Regular expressions
-- @+node:gcross.20091214092727.1583:compileRegularExpression
compileRegularExpression :: String -> Regex
compileRegularExpression = fromRight . compile defaultCompOpt defaultExecOpt . L8.pack
-- @-node:gcross.20091214092727.1583:compileRegularExpression
-- @+node:gcross.20091214092727.1586:applyRegularExpression
applyRegularExpression :: Regex -> L.ByteString -> [String]
applyRegularExpression regex = map (L8.unpack . fst . (! 2)) . matchAllText regex
-- @-node:gcross.20091214092727.1586:applyRegularExpression
-- @+node:gcross.20091214092727.1588:applyRegularExpressionToString
applyRegularExpressionToString :: Regex -> String -> [String]
applyRegularExpressionToString regex = applyRegularExpression regex . L8.pack
-- @-node:gcross.20091214092727.1588:applyRegularExpressionToString
-- @-node:gcross.20091214092727.1585:Regular expressions
-- @+node:gcross.20091129000542.1704:dotsToSubdirectories
dotsToSubdirectories :: String -> FilePath
dotsToSubdirectories = joinPath . splitDot
-- @-node:gcross.20091129000542.1704:dotsToSubdirectories
-- @+node:gcross.20091129000542.1500:findProgramInPath
findProgramInPath = unsafePerformIO . findExecutable
-- @-node:gcross.20091129000542.1500:findProgramInPath
-- @+node:gcross.20091129000542.1699:isDirectoryAt
isDirectoryAt = unsafePerformIO . doesDirectoryExist
-- @-node:gcross.20091129000542.1699:isDirectoryAt
-- @+node:gcross.20091130193227.1923:isDotFree
isDotFree = notElem '.'
-- @nonl
-- @-node:gcross.20091130193227.1923:isDotFree
-- @+node:gcross.20091129000542.1503:isFileAt
isFileAt = unsafePerformIO . doesFileExist
-- @-node:gcross.20091129000542.1503:isFileAt
-- @+node:gcross.20091128000856.1484:readVersion
readVersion :: String -> Version
readVersion = fst . last . readP_to_S parseVersion
-- @-node:gcross.20091128000856.1484:readVersion
-- @+node:gcross.20091127142612.1418:splitDot
splitDot :: String -> [String]
splitDot "" = []
splitDot s =
    let (first_part, rest_string) = break (== '.') s
    in first_part : if null rest_string then [] else splitDot . tail $ rest_string
-- @-node:gcross.20091127142612.1418:splitDot
-- @+node:gcross.20091127142612.1420:unsplitDot
unsplitDot = intercalate "."
-- @nonl
-- @-node:gcross.20091127142612.1420:unsplitDot
-- @+node:gcross.20091129000542.1499:unwrapDynamic
unwrapDynamic :: Typeable a => Dynamic -> a
unwrapDynamic dyn = fromDyn dyn (error $ "Unable to cast Dynamic to the expected type!  (Type of Dynamic is " ++ show (dynTypeRep dyn) ++ ".)")

-- @-node:gcross.20091129000542.1499:unwrapDynamic
-- @-node:gcross.20091127142612.1416:Functions
-- @-others
-- @-node:gcross.20091127142612.1413:@thin Miscellaneous.hs
-- @-leo
