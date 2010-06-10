-- @+leo-ver=4-thin
-- @+node:gcross.20100602152546.1280:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100602152546.1867:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100602152546.1867:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100602152546.1869:<< Import needed modules >>
import Prelude hiding (catch)

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception hiding (assert)
import Control.Monad
import Control.Monad.IO.Class

import Data.Binary
import Data.Either.Unwrap
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Data.Vec ((:.)(..))

import Debug.Trace

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import System.IO

import Data.Object

import Blueprint.IOTask
import Blueprint.Jobs
import Blueprint.Options
-- @-node:gcross.20100602152546.1869:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100607083309.1379:Exceptions
-- @+node:gcross.20100607083309.1380:TestException
data TestException = TestException deriving (Show,Eq,Typeable)
instance Exception TestException
-- @-node:gcross.20100607083309.1380:TestException
-- @-node:gcross.20100607083309.1379:Exceptions
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
assertThrows :: Exception e => e → IO a → Assertion
assertThrows exception thunk =
    catch
        (thunk >> assertFailure "No exception was thrown.")
        (assertEqual
            "Was the correct exception thrown?"
            (show exception)
            .
            (show :: SomeException → String)
        )
-- @-node:gcross.20100603132252.2058:assertThrows
-- @-node:gcross.20100602195250.1297:Functions
-- @+node:gcross.20100602152546.1874:Values
-- @+node:gcross.20100609163522.1701:Test fields
_a = field "a" "e75eb3a0-5986-4772-9e3c-2926ded9239c" :: Field Int
_b = field "b" "52a37070-f576-4f44-b6f5-b6825f5d756f" :: Field Char
_c = field "c" "acc11d70-deae-4745-ba97-112350d5930f" :: Field Bool
_a_as_char = field "a" "e75eb3a0-5986-4772-9e3c-2926ded9239c" :: Field Char
-- @-node:gcross.20100609163522.1701:Test fields
-- @-node:gcross.20100602152546.1874:Values
-- @+node:gcross.20100609163522.1717:Types
-- @+node:gcross.20100609163522.1718:TestRecord
data TestRecord = TestRecord
    {   t_a :: Int
    ,   t_b :: Char
    } deriving (Eq,Show)

fields = (_a,t_a) :. (_b,t_b) :. ()

instance Castable TestRecord where
    toObject = toObjectUsingFields fields
    fromObject = fromObjectUsingFields fields TestRecord
-- @-node:gcross.20100609163522.1718:TestRecord
-- @-node:gcross.20100609163522.1717:Types
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20100602152546.1870:<< Tests >>
    -- @+others
    -- @+node:gcross.20100609163522.1702:Data.Object
    [testGroup "Data.Object"
        -- @    @+others
        -- @+node:gcross.20100609163522.1703:fromEntity . toEntity
        [testProperty "fromEntity . toEntity" $
            \(x :: Int) -> Just x == (fromEntity . toEntity) x
        -- @-node:gcross.20100609163522.1703:fromEntity . toEntity
        -- @+node:gcross.20100609163522.1705:getField/setField
        ,testGroup "getField/setField"
            -- @    @+others
            -- @+node:gcross.20100609163522.1706:set a, get a
            [testProperty "set a, get a" $
                \x -> Just x == (getField _a . setField _a x) emptyObject
            -- @-node:gcross.20100609163522.1706:set a, get a
            -- @+node:gcross.20100609163522.1728:set a (as char), get a
            ,testCase "set a (as char), get a" $
                assertThrows
                    (TypeError _a)
                    $
                    evaluate ((getField _a . setField _a_as_char 'Q') emptyObject)
            -- @-node:gcross.20100609163522.1728:set a (as char), get a
            -- @+node:gcross.20100609163522.1708:set b, get b
            ,testProperty "set a, get b" $
                \x -> Just x == (getField _b . setField _b x) emptyObject
            -- @-node:gcross.20100609163522.1708:set b, get b
            -- @+node:gcross.20100609163522.1710:set a, get b
            ,testProperty "set a, get b" $
                \x -> Nothing == (getField _b . setField _a x) emptyObject
            -- @-node:gcross.20100609163522.1710:set a, get b
            -- @+node:gcross.20100609163522.1712:set b, get a
            ,testProperty "set b, get a" $
                \x -> Nothing == (getField _a . setField _b x) emptyObject
            -- @-node:gcross.20100609163522.1712:set b, get a
            -- @+node:gcross.20100609163522.1714:set a, set b, get a
            ,testProperty "set a, set b, get a" $
                \a b -> Just a == (getField _a . setField _b b . setField _a a) emptyObject
            -- @-node:gcross.20100609163522.1714:set a, set b, get a
            -- @+node:gcross.20100609163522.1716:set a, set b, get b
            ,testProperty "set a, set b, get a" $
                \a b -> Just b == (getField _b . setField _b b . setField _a a) emptyObject
            -- @-node:gcross.20100609163522.1716:set a, set b, get b
            -- @-others
            ]
        -- @-node:gcross.20100609163522.1705:getField/setField
        -- @+node:gcross.20100609163522.1733:getFields . setFields
        ,testProperty "getFields . setFields" $
            \a c ->
                (Just a :. Nothing :. Just c :. ()) == 
                    (getFields (_a :. _b :. _c :. ())
                     .
                     setFields ((_a,a) :. (_c,c) :. ())
                    ) emptyObject

        -- @-node:gcross.20100609163522.1733:getFields . setFields
        -- @+node:gcross.20100609163522.1719:field lists
        ,testGroup "field lists"
            -- @    @+others
            -- @+node:gcross.20100609163522.1720:cast of empty object
            [testCase "cast of empty object" $
                assertEqual
                    "Did the object fail to cast?"
                    (Nothing :: Maybe TestRecord)
                    (fromObject emptyObject)
            -- @-node:gcross.20100609163522.1720:cast of empty object
            -- @+node:gcross.20100609163522.1722:fromObject . toObject
            ,testProperty "fromObject . toObject" $
                \a b -> let record = TestRecord a b in Just record == (fromObject . toObject) record
            -- @-node:gcross.20100609163522.1722:fromObject . toObject
            -- @+node:gcross.20100609163522.1723:set a, set b, fromObject
            ,testProperty "set a, set b, fromObject" $
                \a b -> let record = TestRecord a b in Just record == (fromObject . setField _a a . setField _b b) emptyObject

            -- @-node:gcross.20100609163522.1723:set a, set b, fromObject
            -- @+node:gcross.20100609163522.1730:set a, set b, set c, fromObject
            ,testProperty "set a, set b, set c, fromObject" $
                \a b c -> let record = TestRecord a b in Just record == (fromObject . setField _c c . setField _a a . setField _b b) emptyObject
            -- @-node:gcross.20100609163522.1730:set a, set b, set c, fromObject
            -- @+node:gcross.20100609163522.1725:set a, fromObject
            ,testProperty "set a, fromObject" $
                \a -> (Nothing :: Maybe TestRecord) == (fromObject . setField _a a) emptyObject
            -- @-node:gcross.20100609163522.1725:set a, fromObject
            -- @-others
            ]
        -- @-node:gcross.20100609163522.1719:field lists
        -- @+node:gcross.20100609163522.1735:decode . encode
        ,testProperty "decode . encode" $
            \a b -> let record = TestRecord a b in Just record == (fromObject . decode . encode . toObject) record
        -- @-node:gcross.20100609163522.1735:decode . encode
        -- @-others
        ]
    -- @-node:gcross.20100609163522.1702:Data.Object
    -- @+node:gcross.20100604204549.7679:Blueprint.IOTask
    ,testGroup "Blueprint.IOTask"
        -- @    @+others
        -- @+node:gcross.20100604204549.7689:single runner
        [testGroup "single runner" $
            let withSingleRunner :: [IOTask a] → IO [a]
                withSingleRunner requests = do
                    task_queue ← newChan
                    result_queue ← newChan
                    runner_thread_id ← spawnIOTaskRunner task_queue result_queue
                    writeList2Chan task_queue requests
                    responses ← replicateM (length requests) (readChan result_queue)
                    killThread runner_thread_id
                    return responses
            in
                -- @        @+others
                -- @+node:gcross.20100604204549.7690:single job
                [testGroup "single job"
                    -- @    @+others
                    -- @+node:gcross.20100604204549.7691:return ()
                    [testCase "return ()" $
                        withSingleRunner [IOTask (return ()) (mapLeft (const ()))]
                        >>=
                        assertEqual
                            "Was the correct result obtained?"
                            [Right ()]
                    -- @-node:gcross.20100604204549.7691:return ()
                    -- @+node:gcross.20100607083309.1376:write to IORef
                    ,testCase "write to IORef" $ do
                        ref ← newIORef 0
                        withSingleRunner [IOTask (writeIORef ref 1) (mapLeft (return ()))]
                            >>=
                            assertEqual
                                "Was the correct result obtained?"
                                [Right ()]
                        readIORef ref
                            >>=
                            assertEqual
                                "Was the correct result written to ref?"
                                1
                    -- @nonl
                    -- @-node:gcross.20100607083309.1376:write to IORef
                    -- @+node:gcross.20100607083309.1378:throw exception
                    ,testCase "throw exception" $ do
                        ref ← newIORef 0
                        withSingleRunner [IOTask (throwIO TestException >> writeIORef ref 1) (mapLeft fromException)]
                            >>=
                            assertEqual
                                "Was the correct result obtained?"
                                [Left (Just TestException)]
                        readIORef ref
                            >>=
                            assertEqual
                                "Is ref unchanged?"
                                0
                    -- @nonl
                    -- @-node:gcross.20100607083309.1378:throw exception
                    -- @-others
                    ]
                -- @-node:gcross.20100604204549.7690:single job
                -- @+node:gcross.20100607083309.1385:multiple jobs
                ,testGroup "multiple jobs"
                    -- @    @+others
                    -- @+node:gcross.20100607083309.1386:return ()
                    [testCase "return ()" $
                        withSingleRunner
                            [IOTask (return ()) (mapLeft (const ()))
                            ,IOTask (throwIO TestException) (mapLeft (const ()))
                            ,IOTask (return ()) (mapLeft (const ()))
                            ]
                        >>=
                        assertEqual
                            "Was the correct result obtained?"
                            [Right ()
                            ,Left ()
                            ,Right ()
                            ]
                    -- @-node:gcross.20100607083309.1386:return ()
                    -- @+node:gcross.20100607083309.1387:write to IORef
                    ,testCase "write to IORef" $ do
                        ref ← newIORef 0
                        withSingleRunner
                            [IOTask (readIORef ref >>= \old_value → writeIORef ref 1 >> return old_value) (mapLeft . const $ ())
                            ,IOTask (readIORef ref >>= \old_value → writeIORef ref 2 >> return old_value) (mapLeft . const $ ())
                            ,IOTask (readIORef ref >>= \old_value → writeIORef ref 4 >> return old_value) (mapLeft . const $ ())
                            ]
                            >>=
                            assertEqual
                                "Was the correct result obtained?"
                                [Right 0
                                ,Right 1
                                ,Right 2
                                ]
                        readIORef ref
                            >>=
                            assertEqual
                                "Was the correct result written to ref?"
                                4
                    -- @nonl
                    -- @-node:gcross.20100607083309.1387:write to IORef
                    -- @+node:gcross.20100607083309.1388:throw exception
                    ,testCase "throw exception" $ do
                        ref ← newIORef 0
                        withSingleRunner
                            [IOTask (throwIO TestException >> writeIORef ref 1) (mapLeft fromException)
                            ,IOTask (modifyIORef ref (+2) >> throwIO TestException) (mapLeft fromException)
                            ]
                            >>=
                            assertEqual
                                "Was the correct result obtained?"
                                [Left (Just TestException)
                                ,Left (Just TestException)
                                ]
                        readIORef ref
                            >>=
                            assertEqual
                                "Is ref correct?"
                                2
                    -- @nonl
                    -- @-node:gcross.20100607083309.1388:throw exception
                    -- @-others
                    ]
                -- @-node:gcross.20100607083309.1385:multiple jobs
                -- @-others
                ]
        -- @nonl
        -- @-node:gcross.20100604204549.7689:single runner
        -- @+node:gcross.20100607083309.1389:multiple runners
        ,testGroup "multiple runners" $
            let withMultipleRunners :: Int → [IOTask a] → IO [a]
                withMultipleRunners number_of_runners requests = do
                    task_queue ← newChan
                    result_queue ← newChan
                    runner_thread_ids ← replicateM number_of_runners (spawnIOTaskRunner task_queue result_queue)
                    writeList2Chan task_queue requests
                    responses ← replicateM (length requests) (readChan result_queue)
                    mapM_ killThread runner_thread_ids
                    return responses
            in
                -- @        @+others
                -- @+node:gcross.20100607083309.1411:return ()
                [testCase "return ()" $
                    withMultipleRunners 4
                        [IOTask (return ()) (mapLeft (const ()))
                        ,IOTask (return ()) (mapLeft (const ()))
                        ,IOTask (return ()) (mapLeft (const ()))
                        ]
                    >>=
                    assertEqual
                        "Was the correct result obtained?"
                        [Right ()
                        ,Right ()
                        ,Right ()
                        ]
                -- @-node:gcross.20100607083309.1411:return ()
                -- @+node:gcross.20100607083309.1390:throw TestException
                ,testCase "throw TestException" $
                    withMultipleRunners 4
                        [IOTask (throwIO TestException) (mapLeft (const ()))
                        ,IOTask (throwIO TestException) (mapLeft (const ()))
                        ,IOTask (throwIO TestException) (mapLeft (const ()))
                        ]
                    >>=
                    assertEqual
                        "Was the correct result obtained?"
                        [Left () :: Either () ()
                        ,Left ()
                        ,Left ()
                        ]
                -- @-node:gcross.20100607083309.1390:throw TestException
                -- @+node:gcross.20100607083309.1392:3 parallel runners
                ,testCase "3 parallel runners" $ do
                    v1 ← newEmptyMVar
                    v2 ← newEmptyMVar
                    withMultipleRunners 3
                        [IOTask (readMVar v1) (mapLeft (const ()))
                        ,IOTask (do {value ← readMVar v2; putMVar v1 value; return value;}) (mapLeft (const ()))
                        ,IOTask (putMVar v2 1 >> return 1) (mapLeft (const ()))
                        ]
                    >>=
                    assertEqual
                        "Was the correct result obtained?"
                        [Right 1
                        ,Right 1
                        ,Right 1
                        ]
                -- @nonl
                -- @-node:gcross.20100607083309.1392:3 parallel runners
                -- @-others
                ]
        -- @-node:gcross.20100607083309.1389:multiple runners
        -- @-others
        ]
    -- @-node:gcross.20100604204549.7679:Blueprint.IOTask
    -- @+node:gcross.20100607083309.1396:Blueprint.Jobs
    ,testGroup "Blueprint.Jobs" $
        -- @    @+others
        -- @+node:gcross.20100607083309.1403:start job server
        [testCase "start job server" $ do
            job_server ← startJobServer 4 Map.empty :: IO (JobServer ())
            killJobServer job_server

        -- @-node:gcross.20100607083309.1403:start job server
        -- @+node:gcross.20100607083309.1478:single jobs
        ,testGroup "single jobs" $
            -- @    @+others
            -- @+node:gcross.20100607083309.1479:return cached value (Nothing)
            [testCase "return cached value (Nothing)" . withJobServer 1 Map.empty $ \job_server → do
                submitJob job_server ["job"] $ \maybe_cached_value →
                    returnWithoutCache maybe_cached_value
                requestJobResult job_server "job"
                    >>=
                    assertEqual
                        "Is the job's result correct?"
                        Nothing
            -- @-node:gcross.20100607083309.1479:return cached value (Nothing)
            -- @+node:gcross.20100607083309.1480:read/write IORef
            ,testCase "read/write MVar" . withJobServer 1 Map.empty $ \job_server → do
                in_var ← newEmptyMVar
                out_var ← newEmptyMVar
                submitJob job_server ["job"] . const $
                    liftIO $ do
                        value ← takeMVar in_var
                        putMVar out_var (value+1)
                        returnWithoutCache value
                putMVar in_var 1
                requestJobResult job_server "job" >>= (@=? (1 :: Int))
                takeMVar out_var >>= (@=? (2 :: Int))
            -- @-node:gcross.20100607083309.1480:read/write IORef
            -- @+node:gcross.20100607083309.1481:bad requests
            ,testGroup "bad requests" $
                -- @    @+others
                -- @+node:gcross.20100607083309.1482:self
                [testCase "self" . withJobServer 1 Map.empty $ \job_server → do
                    submitJob job_server ["job"] . const $ do
                        request ["non-existent job"]
                        returnWithoutCache ()
                    assertThrows
                        (CombinedException [(["job"],toException . NoSuchJobsException $ ["non-existent job"])])
                        (requestJobResult job_server "job")
                -- @-node:gcross.20100607083309.1482:self
                -- @+node:gcross.20100607083309.1483:cyclic dependency
                ,testCase "cyclic dependency" . withJobServer 1 Map.empty $ \job_server → do
                    submitJob job_server ["job"] . const $ do
                        request ["job"]
                        returnWithoutCache ()
                    assertThrows
                        (CombinedException [(["job"],toException CyclicDependencyException)])
                        (requestJobResult job_server "job")
                -- @-node:gcross.20100607083309.1483:cyclic dependency
                -- @-others
                ]
            -- @-node:gcross.20100607083309.1481:bad requests
            -- @+node:gcross.20100607083309.1484:IO thrown exception
            ,testCase "IO thrown exception" . withJobServer 1 Map.empty $ \job_server → do
                submitJob job_server ["job"] . const $
                    liftIO $ do
                        throwIO TestException
                        returnWithoutCache ()
                assertThrows
                    (CombinedException [(["job"],toException TestException)])
                    (requestJobResult job_server "job")
            -- @-node:gcross.20100607083309.1484:IO thrown exception
            -- @+node:gcross.20100607083309.1488:two jobs
            ,testGroup "single jobs" $
                -- @    @+others
                -- @+node:gcross.20100607083309.1489:simple request
                [testCase "simple request" . withJobServer 1 Map.empty $ \job_server → do
                    submitJob job_server ["job1"] . const $
                        returnWithoutCache 1
                    submitJob job_server ["job2"] . const $
                        request ["job1"] >>= returnWithoutCache . head
                    requestJobResult job_server "job2"
                        >>=
                        assertEqual
                            "Is the job's result correct?"
                            (1 :: Int)
                -- @-node:gcross.20100607083309.1489:simple request
                -- @+node:gcross.20100607083309.1490:cyclic request
                ,testCase "cyclic request" . withJobServer 1 Map.empty $ \job_server → do
                    dummy ← newEmptyMVar
                    submitJob job_server ["job1"] . const $ do
                        liftIO $ takeMVar dummy
                        request ["job2"] >>= returnWithoutCache . head
                    submitJob job_server ["job2"] . const $ do
                        request ["job1"] >>= returnWithoutCache . head
                    putMVar dummy ()
                    result ← try (requestJobResult job_server "job2")
                    case result of
                        Right () → assertFailure "Failed to detect cyclic dependency."
                        Left e → assertBool "Was the correct exception thrown?" . (show CyclicDependencyException `isInfixOf`) . show $ (e :: SomeException)
                -- @-node:gcross.20100607083309.1490:cyclic request
                -- @+node:gcross.20100607083309.1448:exception
                ,testCase "exception" . withJobServer 1 Map.empty $ \job_server → do
                    submitJob job_server ["job1"] . const $ do
                        throw TestException
                        returnWithoutCache ()
                    submitJob job_server ["job2"] . const $
                        request ["job1"] >>= returnWithoutCache . head
                    assertThrows
                        (CombinedException [(["job1"],toException TestException)])
                        (requestJobResult job_server "job2")
                -- @-node:gcross.20100607083309.1448:exception
                -- @-others
                ]
            -- @-node:gcross.20100607083309.1488:two jobs
            -- @-others
            ]
        -- @-node:gcross.20100607083309.1478:single jobs
        -- @-others
        ]
    -- @-node:gcross.20100607083309.1396:Blueprint.Jobs
    -- @+node:gcross.20100604204549.1358:Blueprint.Options
    ,testGroup "Blueprint.Options" $
        -- @    @+others
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
        -- @+node:gcross.20100603132252.2064:processOptions
        ,testGroup "processOptions"
            -- @    @+others
            -- @+node:gcross.20100603132252.2065:empty
            [testCase "empty" $
                assertEqual
                    "Were the options parsed correctly?"
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
            -- @+node:gcross.20100603184437.1358:test case #1
            ,testCase "test case #1" $
                assertEqual
                    "Were the options parsed correctly?"
                    (Right
                        (Map.fromList $
                            [("A","1")
                            ,("B","2")
                            ]
                        ,["foo","bar"]
                        )
                    )
                $
                processOptions
                    (createOptionSpecificationWithResolvedConflicts
                        Map.empty
                        Map.empty
                        (options
                            [("A","abc",["long-1","long-2"],"0",NoArgument "1","Option A")
                            ,("B","def",["long"],"",RequiredArgument "TYPE","Option B")
                            ]
                        )
                    )
                    ["foo","-a","--long=2","bar"]
            -- @-node:gcross.20100603184437.1358:test case #1
            -- @+node:gcross.20100604110000.1360:test case #2
            ,testCase "test case #2" $
                assertEqual
                    "Were the options parsed correctly?"
                    (Right
                        (Map.fromList $
                            [("A","1")
                            ,("B","")
                            ,("C","1")
                            ]
                        ,["foo","bar"]
                        )
                    )
                $
                processOptions
                    (createOptionSpecificationWithResolvedConflicts
                        (Map.fromList
                            [('d',"C") -- '
                            ]
                        )
                        Map.empty
                        (options
                            [("A","abc",["long-1","long-2"],"0",NoArgument "1","Option A")
                            ,("B","def",["long"],"",RequiredArgument "TYPE","Option B")
                            ,("C","dg",[],"0",OptionalArgument "TYPE" "1","Option B")
                            ]
                        )
                    )
                    ["foo","-a","bar","-d"]
            -- @-node:gcross.20100604110000.1360:test case #2
            -- @+node:gcross.20100604110000.1362:test case #2
            ,testCase "test case #3" $
                assertEqual
                    "Were the options parsed correctly?"
                    (Left 2)
                .
                mapLeft length
                $
                processOptions
                    (createOptionSpecificationWithResolvedConflicts
                        (Map.fromList
                            [('d',"C") -- '
                            ]
                        )
                        Map.empty
                        (options
                            [("A","abc",["long-1","long-2"],"0",NoArgument "1","Option A")
                            ,("B","def",["long"],"",RequiredArgument "TYPE","Option B")
                            ,("C","dg",[],"0",OptionalArgument "TYPE" "1","Option B")
                            ]
                        )
                    )
                    ["-q","--long"]
            -- @-node:gcross.20100604110000.1362:test case #2
            -- @-others
            ]
        -- @nonl
        -- @-node:gcross.20100603132252.2064:processOptions
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
        ]
    -- @-node:gcross.20100604204549.1358:Blueprint.Options
    -- @-others
    -- @-node:gcross.20100602152546.1870:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100602152546.1280:@thin test.hs
-- @-leo
