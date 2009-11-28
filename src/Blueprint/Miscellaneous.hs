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
import Data.List
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
-- @+node:gcross.20091127142612.1422:readVersion/showVersion
readVersion :: String -> [Int]
readVersion = map read . splitDot

showVersion :: [Int] -> String
showVersion = unsplitDot . map show
-- @-node:gcross.20091127142612.1422:readVersion/showVersion
-- @-node:gcross.20091127142612.1416:Functions
-- @-others
-- @-node:gcross.20091127142612.1413:@thin Miscellaneous.hs
-- @-leo
