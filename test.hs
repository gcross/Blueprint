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
import Prelude hiding (catch)

import Control.Arrow
import Control.Exception hiding (assert)

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
-- @+node:gcross.20100603132252.2058:assertThrows
assertThrows :: (Eq e, Exception e) => e → IO a → Assertion
assertThrows exception thunk =
    catch
        (thunk >> assertFailure "No exception was thrown.")
        (assertEqual
            "Was the correct exception thrown?"
            exception
        )
-- @-node:gcross.20100603132252.2058:assertThrows
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
    -- @+node:gcross.20100603132252.1314:filterConflictsAndConvertToList
    ,testGroup "filterConflictsAndConvertToList"
        -- @    @+others
        -- @+node:gcross.20100603132252.1316:lists with () values
        [testGroup "lists with () values"
            -- @    @+others
            -- @+node:gcross.20100603132252.1317:empty set
            [testCase "empty set" $
                assert . null . filterConflictsAndConvertToList undefined undefined $ (Set.empty :: Set ())
            -- @-node:gcross.20100603132252.1317:empty set
            -- @+node:gcross.20100603132252.1319:singleton set with no conflict
            ,testCase "singleton set with no conflict" $
                assertEqual
                    "Is the output set correct?"
                    [()]
                    .
                    filterConflictsAndConvertToList undefined Map.empty
                    $
                    Set.singleton ()
            -- @-node:gcross.20100603132252.1319:singleton set with no conflict
            -- @+node:gcross.20100603132252.1321:singleton set with conflict
            ,testCase "singleton set with conflict" $
                assertEqual
                    "Is the output set correct?"
                    []
                    .
                    filterConflictsAndConvertToList "A" (Map.singleton () "B")
                    $
                    Set.singleton ()
            -- @-node:gcross.20100603132252.1321:singleton set with conflict
            -- @+node:gcross.20100603132252.1323:singleton set with resolved conflict
            ,testCase "singleton set with conflict" $
                assertEqual
                    "Is the output set correct?"
                    [()]
                    .
                    filterConflictsAndConvertToList "A" (Map.singleton () "A")
                    $
                    Set.singleton ()
            -- @-node:gcross.20100603132252.1323:singleton set with resolved conflict
            -- @-others
            ]
        -- @-node:gcross.20100603132252.1316:lists with () values
        -- @+node:gcross.20100603132252.1329:lists with Int values
        ,testGroup "lists with Int values"
            -- @    @+others
            -- @+node:gcross.20100603132252.1331:set with no conflicts
            [testCase "set with no conflict" $
                assertEqual
                    "Is the output set correct?"
                    [1,2,3,4]
                    .
                    filterConflictsAndConvertToList undefined Map.empty
                    .
                    Set.fromList
                    $
                    [1,2,3,4]
            -- @-node:gcross.20100603132252.1331:set with no conflicts
            -- @+node:gcross.20100603132252.1335:set with some conflicts
            ,testCase "set with some conflicts" $
                assertEqual
                    "Is the output set correct?"
                    [1,2,4,6,7,8,10]
                    .
                    filterConflictsAndConvertToList "A"
                        (Map.fromList
                            [(1,"A")
                            ,(3,"B")
                            ,(5,"C")
                            ,(8,"A")
                            ,(9,"D")
                            ]
                        )
                    .
                    Set.fromList
                    $
                    [1..10]
            -- @-node:gcross.20100603132252.1335:set with some conflicts
            -- @-others
            ]
        -- @-node:gcross.20100603132252.1329:lists with Int values
        -- @-others
        ]
    -- @-node:gcross.20100603132252.1314:filterConflictsAndConvertToList
    -- @+node:gcross.20100603132252.1341:createOptionSpecificationWithResolvedConflicts
    ,testGroup "createOptionSpecificationWithResolvedConflicts"
        -- @    @+others
        -- @+node:gcross.20100603132252.1342:empty
        [testCase "empty" $
            let opts =
                    options $
                        []
            in assertEqual
                "Is the computed specification correct?"
                (OptionSpecification
                    Map.empty
                    Map.empty
                    opts
                )
                $
                createOptionSpecificationWithResolvedConflicts
                    Map.empty
                    Map.empty
                    opts
        -- @nonl
        -- @-node:gcross.20100603132252.1342:empty
        -- @+node:gcross.20100603132252.1344:some non-conflicting options
        ,testCase "some non-conflicting options" $
            let opts =
                    options $
                        [("A","abc",["long-1","long-2"],"0",NoArgument "1","Option A")
                        ,("B","def",["long"],"",RequiredArgument "TYPE","Option B")
                        ]
            in assertEqual
                "Is the computed specification correct?"
                (OptionSpecification
                    Map.empty
                    Map.empty
                    opts
                )
                $
                createOptionSpecificationWithResolvedConflicts
                    Map.empty
                    Map.empty
                    opts
        -- @nonl
        -- @-node:gcross.20100603132252.1344:some non-conflicting options
        -- @+node:gcross.20100603132252.1346:some options with resolved conflicts
        ,testCase "some options with resolved conflicts" $
            let opts =
                    options
                        [("A","zabc",["long-1","long-2"],"0",NoArgument "1","Option A")
                        ,("B","zdef",["long-1"],"",RequiredArgument "TYPE","Option B")
                        ,("C","zbcf",["long-2"],"",OptionalArgument "TYPE" "None","Option C")
                        ]
                short_form_resolutions =
                    Map.fromList
                        [('b',"A")
                        ,('c',"C")
                        ,('f',"B")
                        ,('z',"A") -- '
                        ]
                long_form_resolutions =
                    Map.fromList
                        [("long-1","B")
                        ,("long-2","C")
                        ]
            in assertEqual
                "Is the computed specification correct?"
                (OptionSpecification
                    short_form_resolutions
                    long_form_resolutions
                    opts
                )
                $
                createOptionSpecificationWithResolvedConflicts
                    short_form_resolutions
                    long_form_resolutions
                    opts
        -- @nonl
        -- @-node:gcross.20100603132252.1346:some options with resolved conflicts
        -- @+node:gcross.20100603132252.1348:some options with unresolved conflicts
        ,testCase "some options with unresolved conflicts" $
            let opts =
                    options
                        [("A","zabc",["long-1","long-2"],"0",NoArgument "1","Option A")
                        ,("B","zdef",["long-1"],"",RequiredArgument "TYPE","Option B")
                        ,("C","zbcf",["long-2"],"",OptionalArgument "TYPE" "None","Option C")
                        ]
                short_form_resolutions =
                    Map.fromList
                        [('b',"A")
                        ,('c',"C") -- '
                        ]
                long_form_resolutions =
                    Map.fromList
                        [("long-1","B")
                        ]
            in assertThrows
                (ConflictingOptionFormsException
                    (Map.fromList
                        [('z',Set.fromList ["A","B","C"])
                        ,('f',Set.fromList ["B","C"]) -- '
                        ]
                    )
                    (Map.fromList
                        [("long-2",Set.fromList ["A","C"])
                        ]
                    )
                )
                .
                evaluate
                $
                createOptionSpecificationWithResolvedConflicts
                    short_form_resolutions
                    long_form_resolutions
                    opts
        -- @nonl
        -- @-node:gcross.20100603132252.1348:some options with unresolved conflicts
        -- @-others
        ]
    -- @nonl
    -- @-node:gcross.20100603132252.1341:createOptionSpecificationWithResolvedConflicts
    -- @+node:gcross.20100603132252.2064:computeGetOptDescriptors
    ,testGroup "processOptions"
        -- @    @+others
        -- @+node:gcross.20100603132252.2065:empty
        [testCase "empty" $
            assertEqual
                "Are the parsed option correct?"
                (Right (Map.empty,[]))
            $
            processOptions
                (createOptionSpecificationWithResolvedConflicts
                    Map.empty
                    Map.empty
                    Map.empty
                )
                []

        -- @-node:gcross.20100603132252.2065:empty
        -- @-others
        ]
    -- @nonl
    -- @-node:gcross.20100603132252.2064:computeGetOptDescriptors
    -- @+node:gcross.20100603132252.1338:options
    ,testCase "options" $
        assertEqual
            "Is the options map correct?"
            (Map.fromList
                [("A"
                 ,Option
                    (Set.fromList "abc")
                    (Set.fromList ["long-1","long-2"])
                    "0"
                    (NoArgument "1")
                    "Option A"
                 )
                ,("B"
                 ,Option
                    (Set.fromList "def")
                    (Set.fromList ["long"])
                    ""
                    (RequiredArgument "TYPE")
                    "Option B"
                 )
                ]
            )
            .
            options
            $
            [("A","abc",["long-1","long-2"],"0",NoArgument "1","Option A")
            ,("B","def",["long"],"",RequiredArgument "TYPE","Option B")
            ]
    -- @-node:gcross.20100603132252.1338:options
    -- @-others
    -- @-node:gcross.20100602152546.1870:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100602152546.1280:@thin test.hs
-- @-leo
