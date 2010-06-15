-- @+leo-ver=4-thin
-- @+node:gcross.20100602152546.1280:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100602152546.1867:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp #-}
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
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Dynamic
import Data.Either.Unwrap
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Data.Vec ((:.)(..))
import Data.Version

import Debug.Trace

import GHC.IO.Exception

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Text.StringTemplate
import Text.StringTemplate.GenericStandard

import System.IO

import Data.Object

import Blueprint.Configuration.Libraries.LAPACK
import Blueprint.Configuration.Tools
import Blueprint.Language
import Blueprint.Language.C
import Blueprint.Language.CPP
import Blueprint.Language.Haskell
import Blueprint.Miscellaneous
import Blueprint.Tools.Compilers
import Blueprint.Tools.Compilers.GCC
import Blueprint.Tools.Compilers.GHC
import Blueprint.IOTask
import Blueprint.Jobs
import Blueprint.Options
import Blueprint.Phases
import qualified Blueprint.Phases.Configuration as Configuration
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
-- @+node:gcross.20100607205618.1430:assertJobCacheEmpty
assertJobCacheEmpty =
    requestJobCache
    >=>
    assertBool
        "Is the job cache empty?"
    .
    Map.null
-- @-node:gcross.20100607205618.1430:assertJobCacheEmpty
-- @+node:gcross.20100607205618.1434:assertJobCacheEqualTo
assertJobCacheEqualTo job_server correct_cache =
    requestJobCache job_server
    >>=
    assertEqual
        "Is the job cache correct?"
        correct_cache
-- @-node:gcross.20100607205618.1434:assertJobCacheEqualTo
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

instance Record Dynamic TestRecord where
    toTable = toTableUsingFields fields
    fromTable = fromTableUsingFields fields TestRecord

instance Record Entity TestRecord where
    toTable = toTableUsingFields fields
    fromTable = fromTableUsingFields fields TestRecord
-- @-node:gcross.20100609163522.1718:TestRecord
-- @-node:gcross.20100609163522.1717:Types
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20100602152546.1870:<< Tests >>
    -- @+others
    -- @+node:gcross.20100609163522.1702:Data.Object
    [testGroup "Data.Object" $
        let 
            makeTests name emptyObject toEntity_ =
                testGroup name
                    -- @                @+others
                    -- @+node:gcross.20100609223718.1567:fromEntity . toEntity
                    [testProperty "fromEntity . toEntity" $
                        \(x :: Int) -> Just x == (fromEntity . toEntity_) x
                    -- @-node:gcross.20100609223718.1567:fromEntity . toEntity
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
                                (fromTable emptyObject)
                        -- @nonl
                        -- @-node:gcross.20100609163522.1720:cast of empty object
                        -- @+node:gcross.20100609163522.1722:fromTable . toTable
                        ,testProperty "fromTable . toTable" $
                            \a b -> let record = TestRecord a b in Just record == (fromTable . mappend emptyObject . toTable) record
                        -- @nonl
                        -- @-node:gcross.20100609163522.1722:fromTable . toTable
                        -- @+node:gcross.20100609163522.1723:set a, set b, fromTable
                        ,testProperty "set a, set b, fromTable" $
                            \a b -> let record = TestRecord a b in Just record == (fromTable . setField _a a . setField _b b) emptyObject
                        -- @-node:gcross.20100609163522.1723:set a, set b, fromTable
                        -- @+node:gcross.20100609163522.1730:set a, set b, set c, fromTable
                        ,testProperty "set a, set b, set c, fromTable" $
                            \a b c -> let record = TestRecord a b in Just record == (fromTable . setField _c c . setField _a a . setField _b b) emptyObject
                        -- @nonl
                        -- @-node:gcross.20100609163522.1730:set a, set b, set c, fromTable
                        -- @+node:gcross.20100609163522.1725:set a, fromTable
                        ,testProperty "set a, fromTable" $
                            \a -> (Nothing :: Maybe TestRecord) == (fromTable . setField _a a) emptyObject
                        -- @nonl
                        -- @-node:gcross.20100609163522.1725:set a, fromTable
                        -- @-others
                        ]
                    -- @-node:gcross.20100609163522.1719:field lists
                    -- @+node:gcross.20100609203325.1470:Monoid Object
                    ,testGroup "Monoid Object"
                        -- @    @+others
                        -- @+node:gcross.20100609203325.1471:a + a
                        [testProperty "a + a" $
                            \a1 a2 -> Just a2 == getField _a ((withFields ((_a,a1::Int):.())) `mappend` (withFields ((_a,a2):.())) `mappend` emptyObject)
                        -- @-node:gcross.20100609203325.1471:a + a
                        -- @+node:gcross.20100609203325.1475:a + b
                        ,testProperty "a + b" $
                            \a b -> (Just a :. Just b :. ()) == getFields (_a :. _b :. ()) ((withFields ((_a,a):.())) `mappend` (withFields ((_b,b::Char):.())) `mappend` emptyObject)
                        -- @-node:gcross.20100609203325.1475:a + b
                        -- @+node:gcross.20100609203325.1477:(a,b) + (b,c)
                        ,testProperty "(a,b) + (b,c)" $
                            \a b1 b2 c ->
                                (Just a :. Just b2 :. Just c :. ()) ==
                                    getFields (_a :. _b :. _c :. ())
                                        ((withFields ((_a,a):.(_b,b1::Char):.())) `mappend` (withFields ((_b,b2):.(_c,c):.())) `mappend` emptyObject)
                        -- @-node:gcross.20100609203325.1477:(a,b) + (b,c)
                        -- @+node:gcross.20100609203325.1479:(a,b) + (a,c)
                        ,testProperty "(a,b) + (a,c)" $
                            \a1 a2 b c ->
                                (Just a2 :. Just b :. Just c :. ()) ==
                                    getFields (_a :. _b :. _c :. ())
                                        ((withFields ((_a,a1::Int):.(_b,b):.())) `mappend` (withFields ((_a,a2::Int):.(_c,c):.())) `mappend` emptyObject)
                        -- @-node:gcross.20100609203325.1479:(a,b) + (a,c)
                        -- @+node:gcross.20100609203325.1481:(a,b) + (a,c)
                        ,testProperty "(a,c) + (a,b)" $
                            \a1 a2 b c ->
                                (Just a1 :. Just b :. Just c :. ()) ==
                                    getFields (_a :. _b :. _c :. ())
                                        ((withFields ((_a,a2::Int):.(_c,c):.())) `mappend` (withFields ((_a,a1::Int):.(_b,b):.())) `mappend` emptyObject)

                        -- @-node:gcross.20100609203325.1481:(a,b) + (a,c)
                        -- @+node:gcross.20100609203325.1473:(a,c) `mappend` (TestRecord)
                        ,testProperty "(a,c) `mappend` (TestRecord)" $
                            \a b c a_ ->
                                (Just a :. Just b :. Just c :. ()) ==
                                    getFields (_a :. _b :. _c :. ())
                                    ((withFields ((_a,(a_::Int)) :. (_c,c) :. ())) `mappend` (toTable (TestRecord a b)) `mappend` emptyObject)
                        -- @-node:gcross.20100609203325.1473:(a,c) `mappend` (TestRecord)
                        -- @-others
                        ]
                    -- @-node:gcross.20100609203325.1470:Monoid Object
                    -- @+node:gcross.20100609203325.1465:updateTableWith
                    ,testGroup "updateTableWith"
                        -- @    @+others
                        -- @+node:gcross.20100609203325.1466:update empty object
                        [testProperty "update empty object" $
                            \a b -> let record = TestRecord a b in Just record == (fromTable . mappend emptyObject . updateTableWith record) emptyObject
                        -- @nonl
                        -- @-node:gcross.20100609203325.1466:update empty object
                        -- @+node:gcross.20100609203325.1468:completely overwrite object
                        ,testProperty "completely overwrite object" $
                            \a b c d ->
                                let record1 = TestRecord a b
                                    record2 = TestRecord c d
                                in Just record2 == (fromTable . mappend emptyObject . updateTableWith record2 . toTable) record1
                        -- @-node:gcross.20100609203325.1468:completely overwrite object
                        -- @+node:gcross.20100609203325.1469:set a, set c, updateObjectWith (TestRecord a b)
                        ,testProperty "set a, set c, updateObjectWith (TestRecord a b)" $
                            \a b c a_ ->
                                (Just a :. Just b :. Just c :. ()) ==
                                  (
                                    getFields (_a :. _b :. _c :. ())
                                    .
                                    (mappend emptyObject)
                                    .
                                    updateTableWith (TestRecord a b)
                                  ) (withFields ((_a,(a_::Int)) :. (_c,c) :. ()))
                        -- @-node:gcross.20100609203325.1469:set a, set c, updateObjectWith (TestRecord a b)
                        -- @-others
                        ]
                    -- @-node:gcross.20100609203325.1465:updateTableWith
                    -- @-others
                    ]
        in  [makeTests
                "Dynamic"
                (emptyTable :: Object)
                toDyn
            ,makeTests
                "Entity"
                (emptyTable :: SerializableObject)
                (toEntity :: (Binary value, Typeable value) => value -> Entity)
            ,testProperty "decode . encode" $
                \a b -> let record = TestRecord a b
                        in
                            (== Just record)
                            .
                            fromTable
                            .
                            (mappend (emptyTable :: SerializableObject))
                            .
                            decode
                            .
                            encode
                            .
                            (mappend (emptyTable :: SerializableObject))
                            .
                            toTable
                            $
                            record
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
            job_server ← startJobServer 4 Map.empty :: IO (JobServer String ())
            killJobServer job_server

        -- @-node:gcross.20100607083309.1403:start job server
        -- @+node:gcross.20100607083309.1478:single job
        ,testGroup "single job" $
            -- @    @+others
            -- @+node:gcross.20100607083309.1479:return cached value (Nothing)
            [testCase "return cached value (Nothing)" . withJobServer 1 Map.empty $ \job_server → do
                submitJob job_server ["job"] $ \maybe_cached_value →
                    returnValue maybe_cached_value
                requestJobResult job_server "job"
                    >>=
                    assertEqual
                        "Is the job's result correct?"
                        Nothing
                assertJobCacheEmpty job_server
            -- @nonl
            -- @-node:gcross.20100607083309.1479:return cached value (Nothing)
            -- @+node:gcross.20100607083309.1480:read/write IORef
            ,testCase "read/write MVar" . withJobServer 1 Map.empty $ \job_server → do
                in_var ← newEmptyMVar
                out_var ← newEmptyMVar
                submitJob job_server ["job"] . const $
                    liftIO $ do
                        value ← takeMVar in_var
                        putMVar out_var (value+1)
                        returnValue value
                putMVar in_var 1
                requestJobResult job_server "job" >>= (@=? (1 :: Int))
                takeMVar out_var >>= (@=? (2 :: Int))
                assertJobCacheEmpty job_server
            -- @nonl
            -- @-node:gcross.20100607083309.1480:read/write IORef
            -- @+node:gcross.20100607083309.1481:bad requests
            ,testGroup "bad requests" $
                -- @    @+others
                -- @+node:gcross.20100607083309.1482:self
                [testCase "self" . withJobServer 1 Map.empty $ \job_server → do
                    submitJob job_server ["job"] . const $ do
                        request ["non-existent job"]
                        returnValue ()
                    assertThrows
                        (CombinedException [(["job"],toException . NoSuchJobsException $ ["non-existent job"])])
                        (requestJobResult job_server "job")
                    assertJobCacheEmpty job_server
                -- @nonl
                -- @-node:gcross.20100607083309.1482:self
                -- @+node:gcross.20100607083309.1483:cyclic dependency
                ,testCase "cyclic dependency" . withJobServer 1 Map.empty $ \job_server → do
                    submitJob job_server ["job"] . const $ do
                        request ["job"]
                        returnValue ()
                    assertThrows
                        (CombinedException [(["job"],toException CyclicDependencyException)])
                        (requestJobResult job_server "job")
                    assertJobCacheEmpty job_server
                -- @nonl
                -- @-node:gcross.20100607083309.1483:cyclic dependency
                -- @-others
                ]
            -- @-node:gcross.20100607083309.1481:bad requests
            -- @+node:gcross.20100607083309.1484:IO thrown exception
            ,testCase "IO thrown exception" . withJobServer 1 Map.empty $ \job_server → do
                submitJob job_server ["job"] . const $
                    liftIO $ do
                        throwIO TestException
                        returnValue ()
                assertThrows
                    (CombinedException [(["job"],toException TestException)])
                    (requestJobResult job_server "job")
                assertJobCacheEmpty job_server
            -- @nonl
            -- @-node:gcross.20100607083309.1484:IO thrown exception
            -- @+node:gcross.20100607205618.1445:multiple results
            ,testCase "multiple results" . withJobServer 0 Map.empty $ \job_server → do
                submitJob job_server ["job1","job2"] . const $
                    returnValues [1,2]
                requestJobResult job_server "job1"
                    >>=
                    assertEqual
                        "Is the job's first result correct?"
                        (1 :: Int)
                requestJobResult job_server "job2"
                    >>=
                    assertEqual
                        "Is the job's second result correct?"
                        (2 :: Int)
                assertJobCacheEmpty job_server
            -- @-node:gcross.20100607205618.1445:multiple results
            -- @+node:gcross.20100607205618.1447:too many results
            ,testCase "too many results" . withJobServer 0 Map.empty $ \job_server → do
                submitJob job_server ["job"] . const $
                    returnValues [1,2::Int]
                assertThrows
                    (CombinedException [(["job"],toException $ ReturnedWrongNumberOfResults 2 1)])
                    (requestJobResult job_server "job")
                assertJobCacheEmpty job_server
            -- @-node:gcross.20100607205618.1447:too many results
            -- @+node:gcross.20100607205618.1449:too few results
            ,testCase "too few results" . withJobServer 0 Map.empty $ \job_server → do
                submitJob job_server ["job"] . const $
                    returnValues ([] :: [()])
                assertThrows
                    (CombinedException [(["job"],toException $ ReturnedWrongNumberOfResults 0 1)])
                    (requestJobResult job_server "job")
                assertJobCacheEmpty job_server
            -- @-node:gcross.20100607205618.1449:too few results
            -- @-others
            ]
        -- @-node:gcross.20100607083309.1478:single job
        -- @+node:gcross.20100607083309.1488:two jobs
        ,testGroup "two jobs" $
            -- @    @+others
            -- @+node:gcross.20100607083309.1489:simple request
            [testCase "simple request" . withJobServer 1 Map.empty $ \job_server → do
                submitJob job_server ["job1"] . const $
                    returnValue 1
                submitJob job_server ["job2"] . const $
                    request ["job1"] >>= returnValue . head
                requestJobResult job_server "job2"
                    >>=
                    assertEqual
                        "Is the job's result correct?"
                        (1 :: Int)
                assertJobCacheEmpty job_server
            -- @nonl
            -- @-node:gcross.20100607083309.1489:simple request
            -- @+node:gcross.20100607205618.1451:simple request, multiple results
            ,testCase "simple request, multiple results" . withJobServer 1 Map.empty $ \job_server → do
                submitJob job_server ["job1A","job1B"] . const $
                    returnValues [1,2]
                submitJob job_server ["job2"] . const $
                    request ["job1B"] >>= returnValue . head
                requestJobResult job_server "job2"
                    >>=
                    assertEqual
                        "Is the job's result correct?"
                        (2 :: Int)
                assertJobCacheEmpty job_server
            -- @-node:gcross.20100607205618.1451:simple request, multiple results
            -- @+node:gcross.20100607083309.1490:cyclic request
            ,testCase "cyclic request" . withJobServer 1 Map.empty $ \job_server → do
                dummy ← newEmptyMVar
                submitJob job_server ["job1"] . const $ do
                    liftIO $ takeMVar dummy
                    request ["job2"] >>= returnValue . head
                submitJob job_server ["job2"] . const $ do
                    request ["job1"] >>= returnValue . head
                putMVar dummy ()
                result ← try (requestJobResult job_server "job2")
                case result of
                    Right () → assertFailure "Failed to detect cyclic dependency."
                    Left e → assertBool "Was the correct exception thrown?" . (show CyclicDependencyException `isInfixOf`) . show $ (e :: SomeException)
                assertJobCacheEmpty job_server
            -- @nonl
            -- @-node:gcross.20100607083309.1490:cyclic request
            -- @+node:gcross.20100607083309.1448:exception
            ,testCase "exception" . withJobServer 1 Map.empty $ \job_server → do
                submitJob job_server ["job1"] . const $ do
                    throw TestException
                    returnValue ()
                submitJob job_server ["job2"] . const $
                    request ["job1"] >>= returnValue . head
                assertThrows
                    (CombinedException [(["job1"],toException TestException)])
                    (requestJobResult job_server "job2")
                assertJobCacheEmpty job_server
            -- @nonl
            -- @-node:gcross.20100607083309.1448:exception
            -- @+node:gcross.20100607205618.1422:IO tasks run in parallel
            ,testCase "IO tasks run in parallel" . withJobServer 2 Map.empty $ \job_server → do
                chan1 ← newChan
                chan2 ← newChan
                submitJob job_server ["job1"] . const $
                    liftIO $ (writeChan chan2 2 >> readChan chan1)
                    >>=
                    returnValue
                submitJob job_server ["job2"] . const $ 
                    liftIO $ (writeChan chan1 1 >> readChan chan2)
                    >>=
                    returnValue
                submitJob job_server ["job3"] . const $ 
                    request ["job1","job2"]
                    >>=
                    returnValue . sum
                requestJobResult job_server "job3"
                    >>=
                    assertEqual
                        "Is job 1's result correct?"
                        (3 :: Int)
                requestJobResult job_server "job2"
                    >>=
                    assertEqual
                        "Is job 1's result correct?"
                        (2 :: Int)
                requestJobResult job_server "job1"
                    >>=
                    assertEqual
                        "Is job 2's result correct?"
                        (1 :: Int)
                assertJobCacheEmpty job_server
            -- @nonl
            -- @-node:gcross.20100607205618.1422:IO tasks run in parallel
            -- @+node:gcross.20100607205618.1424:IO tasks limited to slaves
            ,testCase "IO tasks limited to slaves" . withJobServer 1 Map.empty $ \job_server → do
                chan1 ← newChan
                chan2 ← newChan
                submitJob job_server ["job1"] . const $
                    liftIO $ (writeChan chan2 (2 :: Int) >> readChan chan1)
                    >>=
                    returnValue
                submitJob job_server ["job2"] . const $ 
                    liftIO $ (writeChan chan1 1 >> readChan chan2)
                    >>=
                    returnValue
                submitJob job_server ["job3"] . const $ 
                    request ["job1","job2"]
                    >>=
                    returnValue . sum
                hPutStrLn stderr "(Note that if this test succeeds, then you *should* see a JOB SERVER KILLED message soon.)"
                assertThrows
                    BlockedIndefinitelyOnMVar
                    (requestJobResult job_server "job3")
                assertThrows
                    BlockedIndefinitelyOnMVar
                    (requestJobResult job_server "job2")
                assertThrows
                    BlockedIndefinitelyOnMVar
                    (requestJobResult job_server "job1")
            -- @nonl
            -- @-node:gcross.20100607205618.1424:IO tasks limited to slaves
            -- @-others
            ]
        -- @-node:gcross.20100607083309.1488:two jobs
        -- @+node:gcross.20100607205618.1416:several jobs
        ,testGroup "several jobs" $
            -- @    @+others
            -- @+node:gcross.20100607205618.1418:diamond dependency
            [testCase "diamond dependency" . withJobServer 1 Map.empty $ \job_server → do
                submitJob job_server ["job1"] . const $
                    returnValue 1
                submitJob job_server ["job2"] . const $
                    request ["job1"] >>= returnValue . head
                submitJob job_server ["job3"] . const $
                    request ["job1"] >>= returnValue . head
                submitJob job_server ["job4"] . const $
                    request ["job2","job3"] >>= returnValue . sum
                requestJobResult job_server "job4"
                    >>=
                    assertEqual
                        "Is the job's result correct?"
                        (2 :: Int)
                assertJobCacheEmpty job_server
            -- @nonl
            -- @-node:gcross.20100607205618.1418:diamond dependency
            -- @+node:gcross.20100607205618.1420:cyclic diamond dependency
            ,testCase "cyclic diamond dependency" . withJobServer 1 Map.empty $ \job_server → do
                dummy ← newEmptyMVar
                submitJob job_server ["job1"] . const $ do
                    liftIO . takeMVar $ dummy
                    request ["job4"]
                    returnValue (1 :: Int)
                submitJob job_server ["job2"] . const $
                    request ["job1"] >>= returnValue . head
                submitJob job_server ["job3"] . const $
                    request ["job1"] >>= returnValue . head
                submitJob job_server ["job4"] . const $
                    request ["job2","job3"] >>= returnValue . sum
                putMVar dummy ()
                result ← try (requestJobResult job_server "job4")
                case result of
                    Right _ → assertFailure "Failed to detect cyclic dependency."
                    Left e → assertBool "Was the correct exception thrown?" . (show CyclicDependencyException `isInfixOf`) . show $ (e :: SomeException)
                assertJobCacheEmpty job_server
            -- @nonl
            -- @-node:gcross.20100607205618.1420:cyclic diamond dependency
            -- @-others
            ]
        -- @-node:gcross.20100607205618.1416:several jobs
        -- @+node:gcross.20100607205618.1431:job cache
        ,testGroup "job cache" $
            -- @    @+others
            -- @+node:gcross.20100607205618.1432:return cached value
            [testCase "return cached value" . withJobServer 1
                (Map.fromList
                    [(["job"],encode (1 :: Int))
                    ]
                )
              $ \job_server → do
                submitJob job_server ["job"] $ \maybe_cached_value →
                    returnValueAndCache maybe_cached_value 2
                requestJobResult job_server "job"
                    >>=
                    assertEqual
                        "Is the job's result correct?"
                        (Just (1 :: Int))
                assertJobCacheEqualTo job_server . Map.fromList $
                    [(["job"],encode $ (2 :: Int))
                    ]
            -- @nonl
            -- @-node:gcross.20100607205618.1432:return cached value
            -- @+node:gcross.20100607205618.1440:add cached value
            ,testCase "add cached value" . withJobServer 1 Map.empty $ \job_server → do
                submitJob job_server ["job"] $ \maybe_cached_value →
                    returnValueAndCache maybe_cached_value (2 :: Int)
                requestJobResult job_server "job"
                    >>=
                    assertEqual
                        "Is the job's result correct?"
                        Nothing
                assertJobCacheEqualTo job_server . Map.fromList $
                    [(["job"],encode $ (2 :: Int))
                    ]
            -- @nonl
            -- @-node:gcross.20100607205618.1440:add cached value
            -- @+node:gcross.20100607205618.1436:destroy cached value
            ,testCase "destroy cached value" . withJobServer 1
                (Map.fromList
                    [(["job"],encode (1 :: Int))
                    ]
                )
              $ \job_server → do
                submitJob job_server ["job"] $ \maybe_cached_value → do
                    throw TestException
                    returnValueAndCache () (2 :: Int)
                assertThrows
                    (CombinedException [(["job"],toException TestException)])
                    (requestJobResult job_server "job")
                assertJobCacheEmpty job_server
            -- @nonl
            -- @-node:gcross.20100607205618.1436:destroy cached value
            -- @-others
            ]
        -- @-node:gcross.20100607205618.1431:job cache
        -- @-others
        ]
    -- @-node:gcross.20100607083309.1396:Blueprint.Jobs
    -- @+node:gcross.20100611224425.1713:Blueprint.Languages.Haskell
    ,testGroup "Blueprint.Languages.Haskell"
        -- @    @+others
        -- @+node:gcross.20100611224425.1714:version parser
        [testCase "dependency extractor" $
            assertEqual
                "Were the dependencies extracted correctly?"
                [Dependency Nothing ["A/B/C.hi"]
                ,Dependency Nothing ["X/Y.hi"]
                ,Dependency Nothing ["P/Q.hi"]
                ,Dependency Nothing ["U.hi"]
                ]
            .
            languageDependencyExtractor languageHaskell
            .
            L.pack
            .
            unlines
            $
            ["import A.B.C"
            ,"import  qualified  X.Y as Z"
            ,"import P.Q (a,b,c)"
            ,"     import    U    "
            ]
        -- @-node:gcross.20100611224425.1714:version parser
        -- @-others
        ]
    -- @-node:gcross.20100611224425.1713:Blueprint.Languages.Haskell
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
    -- @+node:gcross.20100614121927.1729:Blueprint.Tools.Compilers
    ,testGroup "Blueprint.Tools.Compilers" $
        -- @    @+others
        -- @+node:gcross.20100614121927.1730:computeCompileToProgramCommand
        [testCase "computeCompileToProgramCommand" $
            assertEqual
                "Is the computed program command correct?"
                ("compile",["source.file.1","and","source.file.2","and","source.file.3","to","program"])
                $
                let compiler = 
                        Compiler
                        {   compilerProgram = undefined
                        ,   compilerInvocationToCompileObject = undefined
                        ,   compilerInvocationToCompileProgram =
                                newAngleSTMP $
                                    "compile <sources; separator=\" and \"> to <program>"
                        ,   compilationLibraryDependencies = undefined
                        } :: Compiler NullLanguage
                    sources = ["source.file." ++ show i | i ← [1..3::Int]]
                    program = "program"
                in computeCompileToProgramCommand compiler (map SourceCodeFile sources) program
        -- @-node:gcross.20100614121927.1730:computeCompileToProgramCommand
        -- @-others
        ]
    -- @-node:gcross.20100614121927.1729:Blueprint.Tools.Compilers
    -- @+node:gcross.20100614121927.2365:Blueprint.Tools.Compilers.GCC
    ,testGroup "Blueprint.Tools.Compilers.GCC"
        -- @    @+others
        -- @+node:gcross.20100614121927.2366:version parser
        [testGroup "version parser" $
                [testCase ("test #" ++ show test_number)
                    .
                    assertEqual
                        "Is the parsed version correct?"
                        correct_value
                    .
                    extractVersion gcc_version_regex
                    .
                    S.pack
                    .
                    unlines
                    $
                    test_string
                | test_number ← [1..]
                | (correct_value,test_string) ←
                    [(Just (Version [4,2,1] [])
                     ,["i686-apple-darwin10-gcc-4.2.1 (GCC) 4.2.1 (Apple Inc. build 5659)"
                      ,"Copyright (C) 2007 Free Software Foundation, Inc."
                      ,"This is free software; see the source for copying conditions.  There is NO"
                      ,"warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."
                      ]
                     )
                    ,(Just (Version [4,4,2] [])
                     ,["gcc (Gentoo 4.4.2 p1.0) 4.4.2"
                      ,"Copyright (C) 2009 Free Software Foundation, Inc."
                      ,"This is free software; see the source for copying conditions.  There is NO"
                      ,"warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."
                      ]
                     )
                    ,(Nothing
                     ,["The Glorious Glasgow Haskell Compilation System, version 6.12.1"
                      ]
                     )
                    ]
                ]
        -- @-node:gcross.20100614121927.2366:version parser
        -- @+node:gcross.20100614172544.1687:invocation template
        ,testGroup "invocation template" $
            [testCase correct_value $
                assertEqual
                    "Is the computed program command correct?"
                    (unwords . words $ correct_value)
                .
                unwords
                .
                words
                .
                toString
                .
                setAttribute "libraries" libraries
                .
                setManyAttrib attributes
                $
                gcc_invocation_template
            | (correct_value,attributes,libraries) ←
                -- @        @+others
                -- @+node:gcross.20100614172544.1688:gcc helloworld.c -o helloworld
                [("gcc helloworld.c -o helloworld"
                 ,[("command","gcc")
                  ,("sources","helloworld.c")
                  ,("target","helloworld")
                  ]
                 ,[]
                 )
                -- @-node:gcross.20100614172544.1688:gcc helloworld.c -o helloworld
                -- @+node:gcross.20100614172544.1714:gfortran a.f b.f -o helloworld
                ,("gfortran a.f b.f -o helloworld"
                 ,[("command","gfortran")
                  ,("sources","b.f")
                  ,("sources","a.f")
                  ,("target","helloworld")
                  ]
                 ,[]
                 )
                -- @-node:gcross.20100614172544.1714:gfortran a.f b.f -o helloworld
                -- @+node:gcross.20100614172544.1712:gcc -x f77 helloworld.c -o helloworld
                ,("gcc -x f77 helloworld.f -o helloworld"
                 ,[("command","gcc")
                  ,("language","f77")
                  ,("sources","helloworld.f")
                  ,("target","helloworld")
                  ]
                 ,[]
                 )
                -- @-node:gcross.20100614172544.1712:gcc -x f77 helloworld.c -o helloworld
                -- @+node:gcross.20100614172544.1716:g++ -c helloworld.cc -o helloworld.o
                ,("g++ -c helloworld.cc -o helloworld.o"
                 ,[("command","g++")
                  ,("object","true")
                  ,("sources","helloworld.cc")
                  ,("target","helloworld.o")
                  ]
                 ,[]
                 )
                -- @-node:gcross.20100614172544.1716:g++ -c helloworld.cc -o helloworld.o
                -- @+node:gcross.20100614172544.1717:gcc helloworld.c -llapack /usr/lib/foo.a -o helloworld
                ,("gcc helloworld.c -llapack /usr/lib/foo.a -o helloworld"
                 ,[("command","gcc")
                  ,("sources","helloworld.c")
                  ,("target","helloworld")
                  ]
                 ,[Library "lapack" Nothing Nothing
                  ,Library "foo" (Just "/usr/lib/foo.a") Nothing
                  ]
                 )
                -- @-node:gcross.20100614172544.1717:gcc helloworld.c -llapack /usr/lib/foo.a -o helloworld
                -- @-others
                ]
            ]
        -- @-node:gcross.20100614172544.1687:invocation template
        -- @-others
        ]
    -- @-node:gcross.20100614121927.2365:Blueprint.Tools.Compilers.GCC
    -- @+node:gcross.20100614121927.2369:Blueprint.Tools.Compilers.GHC
    ,testGroup "Blueprint.Configuration.Tools.Compilers.GHC"
        -- @    @+others
        -- @+node:gcross.20100614121927.2370:version parser
        [testGroup "version parser" $
                [testCase ("test #" ++ show test_number)
                    .
                    assertEqual
                        "Is the parsed version correct?"
                        correct_value
                    .
                    extractVersion ghc_version_regex
                    .
                    S.pack
                    .
                    unlines
                    $
                    test_string
                | test_number ← [1..]
                | (correct_value,test_string) ←
                    [(Just (Version [6,12,1] [])
                     ,["The Glorious Glasgow Haskell Compilation System, version 6.12.1"
                      ]
                     )
                    ,(Nothing
                     ,["gcc (Gentoo 4.4.2 p1.0) 4.4.2"
                      ,"Copyright (C) 2009 Free Software Foundation, Inc."
                      ,"This is free software; see the source for copying conditions.  There is NO"
                      ,"warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."
                      ]
                     )
                    ]
                ]
        -- @-node:gcross.20100614121927.2370:version parser
        -- @-others
        ]
    -- @-node:gcross.20100614121927.2369:Blueprint.Tools.Compilers.GHC
    -- @-others
    -- @-node:gcross.20100602152546.1870:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100602152546.1280:@thin test.hs
-- @-leo
