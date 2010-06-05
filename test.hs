-- @+leo-ver=4-thin
-- @+node:gcross.20100602152546.1280:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100602152546.1867:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100602152546.1867:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100602152546.1869:<< Import needed modules >>
import qualified Data.Map as Map

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Blueprint.Jobs
import Blueprint.Options
-- @-node:gcross.20100602152546.1869:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100602195250.1297:Functions
-- @+node:gcross.20100602195250.1299:findDuplicates
-- @+at
--  findDuplicates :: Ord ɑ => [ɑ] → [ɑ]
--  findDuplicates = go1 . sort
--    where
--      go1 [] = []
--      go1 (value:rest_values) = go2 value rest_values
--  
--      go2 _ [] = []
--      go2 previous_value (value:rest_values) =
--          if previous_value == value
--              then value:go1 (dropWhile (== value) rest_values)
--              else go2 value rest_values
-- @-at
-- @@c
-- @-node:gcross.20100602195250.1299:findDuplicates
-- @-node:gcross.20100602195250.1297:Functions
-- @+node:gcross.20100602152546.1874:Values
-- @-node:gcross.20100602152546.1874:Values
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20100602152546.1870:<< Tests >>
    [
    -- @+others
    -- @-others
    -- @-node:gcross.20100602152546.1870:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100602152546.1280:@thin test.hs
-- @-leo
