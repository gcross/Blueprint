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

import Data.Dynamic
import Data.List
import Data.Typeable
import Data.Version

import System.Directory
import System.FilePath
import System.IO.Unsafe

import Text.ParserCombinators.ReadP
-- @-node:gcross.20091127142612.1415:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091127142612.1416:Functions
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
-- @+node:gcross.20091128000856.1440:myParListWHNF
myParListWHNF :: Strategy [a]
myParListWHNF list = go list
  where
    go [] = list
    go (x:xs) = x `par` go xs
-- @-node:gcross.20091128000856.1440:myParListWHNF
-- @+node:gcross.20091128000856.1484:readVersion
readVersion :: String -> Version
readVersion = fst . last . readP_to_S parseVersion
-- @-node:gcross.20091128000856.1484:readVersion
-- @+node:gcross.20091129000542.1499:unwrapDynamic
unwrapDynamic :: Typeable a => Dynamic -> a
unwrapDynamic dyn = fromDyn dyn (error $ "Unable to cast Dynamic to the expected type!  (Type of Dynamic is " ++ show (dynTypeRep dyn) ++ ".)")

-- @-node:gcross.20091129000542.1499:unwrapDynamic
-- @+node:gcross.20091129000542.1500:findProgramInPath
findProgramInPath = unsafePerformIO . findExecutable
-- @-node:gcross.20091129000542.1500:findProgramInPath
-- @+node:gcross.20091129000542.1503:isFileAt
isFileAt = unsafePerformIO . doesFileExist
-- @-node:gcross.20091129000542.1503:isFileAt
-- @+node:gcross.20091129000542.1699:isDirectoryAt
isDirectoryAt = unsafePerformIO . doesDirectoryExist
-- @-node:gcross.20091129000542.1699:isDirectoryAt
-- @-node:gcross.20091127142612.1416:Functions
-- @-others
-- @-node:gcross.20091127142612.1413:@thin Miscellaneous.hs
-- @-leo
