-- @+leo-ver=4-thin
-- @+node:gcross.20091121210308.1295:@thin Miscellaneous.hs
-- @@language Haskell

module Blueprint.Miscellaneous where

-- @<< Import needed modules >>
-- @+node:gcross.20091121210308.1846:<< Import needed modules >>
import Control.Arrow

import Data.Function
import Data.List
-- @-node:gcross.20091121210308.1846:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091121210308.1296:splitDot
splitDot :: String -> [String]
splitDot "" = []
splitDot s =
    let (first_part, rest_string) = break (== '.') s
    in first_part : if null rest_string then [] else splitDot . tail $ rest_string
-- @-node:gcross.20091121210308.1296:splitDot
-- @+node:gcross.20091121210308.1847:unsplitDot
unsplitDot = intercalate "."
-- @nonl
-- @-node:gcross.20091121210308.1847:unsplitDot
-- @-others
-- @-node:gcross.20091121210308.1295:@thin Miscellaneous.hs
-- @-leo
