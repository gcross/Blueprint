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

import Blueprint.Miscellaneous
import Blueprint.Tools.GFortran
import Blueprint.Tools.GHC
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
    -- @+node:gcross.20091121210308.1817:Blueprint.Miscellaneous
    [testGroup "Blueprint.Miscellaneous"
        -- @    @+others
        -- @+node:gcross.20091121210308.1818:splitDot
        [testProperty "splitDot" $ (uncurry (==)) . (id &&& splitDot . unsplitDot) . filter (not . null) . map (filter (/= '.'))
        -- @-node:gcross.20091121210308.1818:splitDot
        -- @-others
        ]
    -- @-node:gcross.20091121210308.1817:Blueprint.Miscellaneous
    -- @+node:gcross.20091214092727.1580:Blueprint.Tools.GHC
    ,testGroup "Blueprint.Tools.GHC"
        -- @    @+others
        -- @+node:gcross.20091214092727.1582:regular expression
        [testCase "regular expression" $
            let correct_modules =
                    ["A"
                    ,"B"
                    ,"C"
                    ,"D"
                    ]
                found_modules = applyRegularExpressionToString import_matching_regex . unlines . map ('i':) $ --'
                    ["mport A"
                    ,"mport qualified B as BB"
                    ,"mport C (x,y,z)"
                    ,"mport D hiding (x,y,z)"
                    ]
            in assertEqual "Was the correct list of modules obtained?"
                correct_modules
                found_modules
        -- @-node:gcross.20091214092727.1582:regular expression
        -- @-others
        ]
    -- @-node:gcross.20091214092727.1580:Blueprint.Tools.GHC
    -- @+node:gcross.20091214124713.1578:Blueprint.Tools.GFortran
    ,testGroup "Blueprint.Tools.GFortran"
        -- @    @+others
        -- @+node:gcross.20091214124713.1579:regular expression
        [testCase "regular expression" $
            let correct_modules =
                    ["a"
                    ,"b"
                    ,"c"
                    ]
                found_modules = applyRegularExpressionToString use_matching_regex . unlines $
                    ["use a"
                    ,"use b, c => d"
                    ,"use c, only: d"
                    ]
            in assertEqual "Was the correct list of modules obtained?"
                correct_modules
                found_modules
        -- @-node:gcross.20091214124713.1579:regular expression
        -- @-others
        ]
    -- @-node:gcross.20091214124713.1578:Blueprint.Tools.GFortran
    -- @-others
    -- @-node:gcross.20091121210308.1816:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091121210308.1806:@thin test.hs
-- @-leo
