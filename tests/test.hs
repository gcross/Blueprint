-- @+leo-ver=4-thin
-- @+node:gcross.20091121210308.1806:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091121210308.1807:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20091121210308.1807:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091121210308.1808:<< Import needed modules >>
import Control.Arrow

import Data.List

import Debug.Trace

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Blueprint.Resources
-- @-node:gcross.20091121210308.1808:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091121210308.1844:Functions
-- @+node:gcross.20091121210308.1845:echo
echo x = trace (show x) x
-- @-node:gcross.20091121210308.1845:echo
-- @-node:gcross.20091121210308.1844:Functions
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20091121210308.1816:<< Tests >>
    -- @+others
    -- @+node:gcross.20091121210308.1817:Blueprint.Resources
    [testGroup "Blueprint.Resources"
        -- @    @+others
        -- @+node:gcross.20091121210308.1818:splitDot
        [testProperty "splitDot" $ (uncurry (==)) . (id &&& splitDot . unsplitDot) . filter (not . null) . map (filter (/= '.'))
        -- @-node:gcross.20091121210308.1818:splitDot
        -- @-others
        ]
    -- @-node:gcross.20091121210308.1817:Blueprint.Resources
    -- @-others
    -- @-node:gcross.20091121210308.1816:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091121210308.1806:@thin test.hs
-- @-leo
