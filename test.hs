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
import Control.Arrow

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

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
    -- @+others
    -- @+node:gcross.20100603132252.1293:findConflicts
    [testGroup "findConflicts"
        -- @    @+others
        -- @+node:gcross.20100603132252.1300:lists with () values
        [testGroup "lists with () values"
            -- @    @+others
            -- @+node:gcross.20100603132252.1294:empty list
            [testCase "empty list" $
                assert . Map.null . findConflicts (undefined :: Set ()) undefined undefined $ ([] :: [()])
            -- @-node:gcross.20100603132252.1294:empty list
            -- @+node:gcross.20100603132252.1296:singleton list
            ,testCase "singleton list" $
                assertEqual
                    "Is the output set correct?"
                    Map.empty
                    .
                    findConflicts Set.empty (const "") Set.singleton
                    $
                    [()]
            -- @-node:gcross.20100603132252.1296:singleton list
            -- @+node:gcross.20100603132252.1298:2-element list with conflicts
            ,testCase "2-element list with conflicts" $
                assertEqual
                    "Is the output set correct?"
                    (Map.singleton () (Set.fromList ["A","B"]))
                    .
                    findConflicts Set.empty fst (Set.singleton . snd)
                    $
                    [("A",())
                    ,("B",())
                    ]
            -- @-node:gcross.20100603132252.1298:2-element list with conflicts
            -- @+node:gcross.20100603132252.1299:list with simple resolved conflicts
            ,testCase "list with simple resolved conflicts" $
                assertEqual
                    "Is the output set correct?"
                    Map.empty
                    .
                    findConflicts (Set.singleton ()) fst (Set.singleton . snd)
                    $
                    [("A",())
                    ,("B",())
                    ,("C",())
                    ]
            -- @-node:gcross.20100603132252.1299:list with simple resolved conflicts
            -- @-others
            ]
        -- @-node:gcross.20100603132252.1300:lists with () values
        -- @+node:gcross.20100603132252.1306:lists with Int values
        ,testGroup "lists with Int values"
            -- @    @+others
            -- @+node:gcross.20100603132252.1309:list with no conflicts
            [testCase "list with no conflicts" $
                assertEqual
                    "Is the output set correct?"
                    Map.empty
                    .
                    findConflicts Set.empty fst (Set.fromList . snd)
                    $
                    [("A",[1,2])
                    ,("B",[3,4])
                    ,("C",[5,6,7,8])
                    ]
            -- @-node:gcross.20100603132252.1309:list with no conflicts
            -- @+node:gcross.20100603132252.1310:list with unresolved conflicts
            ,testCase "list with unresolved conflicts" $
                assertEqual
                    "Is the output set correct?"
                    (
                        Map.fromList
                        .
                        map (second Set.fromList)
                        $
                        [(2,["A","B","D"])
                        ,(3,["B","D"])
                        ]
                    )
                    .
                    findConflicts Set.empty fst (Set.fromList . snd)
                    $
                    [("A",[1,2])
                    ,("B",[2,3])
                    ,("C",[5,6,7,8])
                    ,("D",[2,3,4])
                    ]
            -- @-node:gcross.20100603132252.1310:list with unresolved conflicts
            -- @+node:gcross.20100603132252.1312:list with some resolved conflicts
            ,testCase "list with some resolved conflicts" $
                assertEqual
                    "Is the output set correct?"
                    (
                        Map.fromList
                        .
                        map (second Set.fromList)
                        $
                        [(3,["B","D"])
                        ]
                    )
                    .
                    findConflicts (Set.fromList [1,2]) fst (Set.fromList . snd)
                    $
                    [("A",[1,2])
                    ,("B",[2,3])
                    ,("C",[5,6,7,8])
                    ,("D",[2,3,4])
                    ,("E",[1])
                    ]
            -- @-node:gcross.20100603132252.1312:list with some resolved conflicts
            -- @-others
            ]
        -- @-node:gcross.20100603132252.1306:lists with Int values
        -- @-others
        ]
    -- @-node:gcross.20100603132252.1293:findConflicts
    -- @-others
    -- @-node:gcross.20100602152546.1870:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100602152546.1280:@thin test.hs
-- @-leo
