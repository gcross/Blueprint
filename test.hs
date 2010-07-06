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

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception hiding (assert)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Data.Binary (Binary,encode,decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5
import Data.Dynamic
import Data.Either.Unwrap
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Data.UUID (UUID)
import qualified Data.UUID as UUID
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
import System.Random

import Control.Monad.Trans.Abort
import Control.Monad.Trans.Goto

import Data.Record

import Blueprint.Configuration.Libraries.LAPACK
import Blueprint.Configuration.Tools
import Blueprint.Dependency
import Blueprint.Fields.DeferredDependencies
import Blueprint.Fields.Digest
import Blueprint.Identifier
import Blueprint.Language.Programming.Haskell
import Blueprint.Miscellaneous
import Blueprint.Tools.Compilers
import Blueprint.Tools.Compilers.GCC
import Blueprint.Tools.Compilers.GHC
import Blueprint.IOTask
import Blueprint.Jobs
import Blueprint.Options
import Blueprint.Path
import Blueprint.Phases
import qualified Blueprint.Phases.Configuration as Configuration
import Blueprint.Tools
import Blueprint.Tools.JobAnalyzer
-- @nonl
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
-- @+node:gcross.20100628115452.1892:Generators
-- @+node:gcross.20100628115452.1893:Arbitrary UUID
instance Arbitrary UUID where
    arbitrary = fmap (fromJust . UUID.fromByteString . L.pack) . vectorOf 16 $ arbitrary
-- @-node:gcross.20100628115452.1893:Arbitrary UUID
-- @-node:gcross.20100628115452.1892:Generators
-- @+node:gcross.20100602152546.1874:Values
-- @+node:gcross.20100609163522.1701:Test fields
_a = field "a" "e75eb3a0-5986-4772-9e3c-2926ded9239c" :: Field Int
_b = field "b" "52a37070-f576-4f44-b6f5-b6825f5d756f" :: Field Char
_c = field "c" "acc11d70-deae-4745-ba97-112350d5930f" :: Field Bool
_a_as_char = field "a" "e75eb3a0-5986-4772-9e3c-2926ded9239c" :: Field Char

_bool = field "bool" "00E4325A-2D6D-4BCB-B6ED-2B966231E55D" :: Field Bool
_digests = field "digests" "0F58880E-962B-4AA3-97A5-FB08F2026EE5" :: Field [MD5Digest]
-- @-node:gcross.20100609163522.1701:Test fields
-- @+node:gcross.20100628115452.1835:Test dependencies
test_dependency_type :: DependencyType
test_dependency_type = identifier "b1a70c20-95ae-4a2e-805e-5c15cb430a61" "test"

test_dependency, test_dependency_1, test_dependency_2 :: Dependency
test_dependency =
    Dependency
    {   dependencyName = "test"
    ,   dependencyType = test_dependency_type
    }
test_dependency_1 =
    Dependency
    {   dependencyName = "test 1"
    ,   dependencyType = test_dependency_type
    }
test_dependency_2 = 
    Dependency
    {   dependencyName = "test 2"
    ,   dependencyType = test_dependency_type
    }

test_unresolved_dependency :: UnresolvedDependency
test_unresolved_dependency =
    UnresolvedDependency
    {   unresolvedDependency = test_dependency
    ,   unresolvedDependencyIsExternal = Nothing
    }

-- @-node:gcross.20100628115452.1835:Test dependencies
-- @+node:gcross.20100628115452.1891:Test identifiers
test_identifier, test_identifier_1, test_identifier_2 :: Identifier ()
test_identifier = identifier "1fa9d5fa-0b71-4e59-9534-6c7d2c146717" "test identifier"
test_identifier_1 = identifier "4b33b60f-09c1-416e-a421-d048aca91699" "test identifier 1"
test_identifier_2 = identifier "d3e9ab2c-1430-466e-a451-906f98031c22" "test identifier 2"
-- @-node:gcross.20100628115452.1891:Test identifiers
-- @-node:gcross.20100602152546.1874:Values
-- @+node:gcross.20100609163522.1717:Types
-- @+node:gcross.20100609163522.1718:TestRecord
data TestRecord = TestRecord
    {   t_a :: Int
    ,   t_b :: Char
    } deriving (Eq,Show)

fields = (_a,t_a) :. (_b,t_b) :. ()

instance Castable Dynamic TestRecord where
    toRecord = toRecordUsingFields fields
    fromRecord = fromRecordUsingFields fields TestRecord

instance Castable Entity TestRecord where
    toRecord = toRecordUsingFields fields
    fromRecord = fromRecordUsingFields fields TestRecord
-- @-node:gcross.20100609163522.1718:TestRecord
-- @-node:gcross.20100609163522.1717:Types
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20100602152546.1870:<< Tests >>
    -- @+others
    -- @+node:gcross.20100609163522.1702:Data.Record
    [testGroup "Data.Record" $
        let 
            makeTests name emptyRecord toEntity_ =
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
                            \x -> Just x == (getField _a . setField _a x) emptyRecord
                        -- @nonl
                        -- @-node:gcross.20100609163522.1706:set a, get a
                        -- @+node:gcross.20100609163522.1728:set a (as char), get a
                        ,testCase "set a (as char), get a" $
                            assertThrows
                                (TypeError _a)
                                $
                                evaluate ((getField _a . setField _a_as_char 'Q') emptyRecord)
                        -- @nonl
                        -- @-node:gcross.20100609163522.1728:set a (as char), get a
                        -- @+node:gcross.20100609163522.1708:set b, get b
                        ,testProperty "set a, get b" $
                            \x -> Just x == (getField _b . setField _b x) emptyRecord
                        -- @nonl
                        -- @-node:gcross.20100609163522.1708:set b, get b
                        -- @+node:gcross.20100609163522.1710:set a, get b
                        ,testProperty "set a, get b" $
                            \x -> Nothing == (getField _b . setField _a x) emptyRecord
                        -- @nonl
                        -- @-node:gcross.20100609163522.1710:set a, get b
                        -- @+node:gcross.20100609163522.1712:set b, get a
                        ,testProperty "set b, get a" $
                            \x -> Nothing == (getField _a . setField _b x) emptyRecord
                        -- @nonl
                        -- @-node:gcross.20100609163522.1712:set b, get a
                        -- @+node:gcross.20100609163522.1714:set a, set b, get a
                        ,testProperty "set a, set b, get a" $
                            \a b -> Just a == (getField _a . setField _b b . setField _a a) emptyRecord
                        -- @nonl
                        -- @-node:gcross.20100609163522.1714:set a, set b, get a
                        -- @+node:gcross.20100609163522.1716:set a, set b, get b
                        ,testProperty "set a, set b, get a" $
                            \a b -> Just b == (getField _b . setField _b b . setField _a a) emptyRecord
                        -- @nonl
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
                                ) emptyRecord
                    -- @-node:gcross.20100609163522.1733:getFields . setFields
                    -- @+node:gcross.20100609163522.1719:field lists
                    ,testGroup "field lists"
                        -- @    @+others
                        -- @+node:gcross.20100609163522.1720:cast of empty Record
                        [testCase "cast of empty Record" $
                            assertEqual
                                "Did the Record fail to cast?"
                                (Nothing :: Maybe TestRecord)
                                (fromRecord emptyRecord)
                        -- @nonl
                        -- @-node:gcross.20100609163522.1720:cast of empty Record
                        -- @+node:gcross.20100609163522.1722:fromRecord . toRecord
                        ,testProperty "fromRecord . toRecord" $
                            \a b -> let record = TestRecord a b in Just record == (fromRecord . mappend emptyRecord . toRecord) record
                        -- @nonl
                        -- @-node:gcross.20100609163522.1722:fromRecord . toRecord
                        -- @+node:gcross.20100609163522.1723:set a, set b, fromRecord
                        ,testProperty "set a, set b, fromRecord" $
                            \a b -> let record = TestRecord a b in Just record == (fromRecord . setField _a a . setField _b b) emptyRecord
                        -- @nonl
                        -- @-node:gcross.20100609163522.1723:set a, set b, fromRecord
                        -- @+node:gcross.20100609163522.1730:set a, set b, set c, fromRecord
                        ,testProperty "set a, set b, set c, fromRecord" $
                            \a b c -> let record = TestRecord a b in Just record == (fromRecord . setField _c c . setField _a a . setField _b b) emptyRecord
                        -- @nonl
                        -- @-node:gcross.20100609163522.1730:set a, set b, set c, fromRecord
                        -- @+node:gcross.20100609163522.1725:set a, fromRecord
                        ,testProperty "set a, fromRecord" $
                            \a -> (Nothing :: Maybe TestRecord) == (fromRecord . setField _a a) emptyRecord
                        -- @nonl
                        -- @-node:gcross.20100609163522.1725:set a, fromRecord
                        -- @-others
                        ]
                    -- @-node:gcross.20100609163522.1719:field lists
                    -- @+node:gcross.20100609203325.1470:Monoid Record
                    ,testGroup "Monoid Record"
                        -- @    @+others
                        -- @+node:gcross.20100609203325.1471:a + a
                        [testProperty "a + a" $
                            \a1 a2 -> Just a2 == getField _a ((withFields ((_a,a1::Int):.())) `mappend` (withFields ((_a,a2):.())) `mappend` emptyRecord)
                        -- @nonl
                        -- @-node:gcross.20100609203325.1471:a + a
                        -- @+node:gcross.20100609203325.1475:a + b
                        ,testProperty "a + b" $
                            \a b -> (Just a :. Just b :. ()) == getFields (_a :. _b :. ()) ((withFields ((_a,a):.())) `mappend` (withFields ((_b,b::Char):.())) `mappend` emptyRecord)
                        -- @nonl
                        -- @-node:gcross.20100609203325.1475:a + b
                        -- @+node:gcross.20100609203325.1477:(a,b) + (b,c)
                        ,testProperty "(a,b) + (b,c)" $
                            \a b1 b2 c ->
                                (Just a :. Just b2 :. Just c :. ()) ==
                                    getFields (_a :. _b :. _c :. ())
                                        ((withFields ((_a,a):.(_b,b1::Char):.())) `mappend` (withFields ((_b,b2):.(_c,c):.())) `mappend` emptyRecord)
                        -- @nonl
                        -- @-node:gcross.20100609203325.1477:(a,b) + (b,c)
                        -- @+node:gcross.20100609203325.1479:(a,b) + (a,c)
                        ,testProperty "(a,b) + (a,c)" $
                            \a1 a2 b c ->
                                (Just a2 :. Just b :. Just c :. ()) ==
                                    getFields (_a :. _b :. _c :. ())
                                        ((withFields ((_a,a1::Int):.(_b,b):.())) `mappend` (withFields ((_a,a2::Int):.(_c,c):.())) `mappend` emptyRecord)
                        -- @nonl
                        -- @-node:gcross.20100609203325.1479:(a,b) + (a,c)
                        -- @+node:gcross.20100609203325.1481:(a,b) + (a,c)
                        ,testProperty "(a,c) + (a,b)" $
                            \a1 a2 b c ->
                                (Just a1 :. Just b :. Just c :. ()) ==
                                    getFields (_a :. _b :. _c :. ())
                                        ((withFields ((_a,a2::Int):.(_c,c):.())) `mappend` (withFields ((_a,a1::Int):.(_b,b):.())) `mappend` emptyRecord)
                        -- @-node:gcross.20100609203325.1481:(a,b) + (a,c)
                        -- @+node:gcross.20100609203325.1473:(a,c) `mappend` (TestRecord)
                        ,testProperty "(a,c) `mappend` (TestRecord)" $
                            \a b c a_ ->
                                (Just a :. Just b :. Just c :. ()) ==
                                    getFields (_a :. _b :. _c :. ())
                                    ((withFields ((_a,(a_::Int)) :. (_c,c) :. ())) `mappend` (toRecord (TestRecord a b)) `mappend` emptyRecord)
                        -- @nonl
                        -- @-node:gcross.20100609203325.1473:(a,c) `mappend` (TestRecord)
                        -- @-others
                        ]
                    -- @nonl
                    -- @-node:gcross.20100609203325.1470:Monoid Record
                    -- @+node:gcross.20100609203325.1465:updateRecordWith
                    ,testGroup "updateRecordWith"
                        -- @    @+others
                        -- @+node:gcross.20100609203325.1466:update empty Record
                        [testProperty "update empty Record" $
                            \a b -> let record = TestRecord a b in Just record == (fromRecord . mappend emptyRecord . updateRecordWith record) emptyRecord
                        -- @nonl
                        -- @-node:gcross.20100609203325.1466:update empty Record
                        -- @+node:gcross.20100609203325.1468:completely overwrite Record
                        ,testProperty "completely overwrite Record" $
                            \a b c d ->
                                let record1 = TestRecord a b
                                    record2 = TestRecord c d
                                in Just record2 == (fromRecord . mappend emptyRecord . updateRecordWith record2 . toRecord) record1
                        -- @nonl
                        -- @-node:gcross.20100609203325.1468:completely overwrite Record
                        -- @+node:gcross.20100609203325.1469:set a, set c, updateRecordWith (TestRecord a b)
                        ,testProperty "set a, set c, updateRecordWith (TestRecord a b)" $
                            \a b c a_ ->
                                (Just a :. Just b :. Just c :. ()) ==
                                  (
                                    getFields (_a :. _b :. _c :. ())
                                    .
                                    (mappend emptyRecord)
                                    .
                                    updateRecordWith (TestRecord a b)
                                  ) (withFields ((_a,(a_::Int)) :. (_c,c) :. ()))
                        -- @nonl
                        -- @-node:gcross.20100609203325.1469:set a, set c, updateRecordWith (TestRecord a b)
                        -- @-others
                        ]
                    -- @nonl
                    -- @-node:gcross.20100609203325.1465:updateRecordWith
                    -- @-others
                    ]
        in  [makeTests
                "Dynamic"
                (emptyTable :: Record)
                toDyn
            ,makeTests
                "Entity"
                (emptyTable :: SerializableRecord)
                (toEntity :: (Binary value, Typeable value) => value -> Entity)
            ,testProperty "decode . encode" $
                \a b -> let record = TestRecord a b
                        in
                            (== Just record)
                            .
                            fromRecord
                            .
                            (mappend (emptyTable :: SerializableRecord))
                            .
                            decode
                            .
                            encode
                            .
                            (mappend (emptyTable :: SerializableRecord))
                            .
                            toRecord
                            $
                            record
            ]
    -- @nonl
    -- @-node:gcross.20100609163522.1702:Data.Record
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
                (map (UnresolvedDependency Nothing . haskellModuleDependency)
                    ["A.B.C"
                    ,"X.Y"
                    ,"P.Q"
                    ,"U"
                    ]
                )
            .
            extractDependenciesFromHaskellSource
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
                  ,("source","helloworld.c")
                  ,("program","helloworld")
                  ]
                 ,[]
                 )
                -- @-node:gcross.20100614172544.1688:gcc helloworld.c -o helloworld
                -- @+node:gcross.20100614172544.1714:gfortran a.f b.f -o helloworld
                ,("gfortran a.f b.f -o helloworld"
                 ,[("command","gfortran")
                  ,("source","b.f")
                  ,("source","a.f")
                  ,("program","helloworld")
                  ]
                 ,[]
                 )
                -- @-node:gcross.20100614172544.1714:gfortran a.f b.f -o helloworld
                -- @+node:gcross.20100614172544.1712:gcc -x f77 helloworld.c -o helloworld
                ,("gcc -x f77 helloworld.f -o helloworld"
                 ,[("command","gcc")
                  ,("language","f77")
                  ,("source","helloworld.f")
                  ,("program","helloworld")
                  ]
                 ,[]
                 )
                -- @-node:gcross.20100614172544.1712:gcc -x f77 helloworld.c -o helloworld
                -- @+node:gcross.20100614172544.1716:g++ -c helloworld.cc -o helloworld.o
                ,("g++ -c helloworld.cc -o helloworld.o"
                 ,[("command","g++")
                  ,("source","helloworld.cc")
                  ,("Record","helloworld.o")
                  ]
                 ,[]
                 )
                -- @nonl
                -- @-node:gcross.20100614172544.1716:g++ -c helloworld.cc -o helloworld.o
                -- @+node:gcross.20100614185504.1694:g++ -c hello-world.cc -o helloworld.o
                ,("g++ -c hello-world.cc -o helloworld.o"
                 ,[("command","g++")
                  ,("source","world.cc")
                  ,("source","hello-")
                  ,("Record","helloworld.o")
                  ]
                 ,[]
                 )
                -- @nonl
                -- @-node:gcross.20100614185504.1694:g++ -c hello-world.cc -o helloworld.o
                -- @+node:gcross.20100614172544.1717:gcc helloworld.c -llapack /usr/lib/foo.a -o helloworld
                ,("gcc helloworld.c -llapack /usr/lib/foo.a -o helloworld"
                 ,[("command","gcc")
                  ,("source","helloworld.c")
                  ,("program","helloworld")
                  ]
                 ,[Library "lapack" Nothing Nothing
                  ,Library "foo" (Just "/usr/lib/foo.a") Nothing
                  ]
                 )
                -- @-node:gcross.20100614172544.1717:gcc helloworld.c -llapack /usr/lib/foo.a -o helloworld
                -- @+node:gcross.20100614185504.1696:gcc -c helloworld.cc -o helloworld.o
                ,("gcc -c helloworld.c -o helloworld.o"
                 ,[("command","gcc")
                  ,("source","helloworld.c")
                  ,("Record","helloworld.o")
                  ]
                 ,[Library "lapack" Nothing Nothing
                  ,Library "foo" (Just "/usr/lib/foo.a") Nothing
                  ]
                 )
                -- @nonl
                -- @-node:gcross.20100614185504.1696:gcc -c helloworld.cc -o helloworld.o
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
    -- @+node:gcross.20100630111926.1978:Control.Monad.Trans.Abort
    ,testGroup "Control.Monad.Trans.Abort"
        -- @    @+others
        -- @+node:gcross.20100630111926.1979:Functor
        [testGroup "Functor"
            -- @    @+others
            -- @+node:gcross.20100630111926.1980:Identity
            [testGroup "Identity"
                -- @    @+others
                -- @+node:gcross.20100630111926.1981:without Abort
                [testProperty "without Abort" $
                    \(x :: Int) (y :: Int) → (== x+y) . runAbort . fmap (+y) . return $ x
                -- @nonl
                -- @-node:gcross.20100630111926.1981:without Abort
                -- @+node:gcross.20100630111926.1982:with Abort
                ,testProperty "with goto" $
                    \(x :: Int) (y :: Int) → (== x) . runAbort . fmap (+y) . abort $ x
                -- @nonl
                -- @-node:gcross.20100630111926.1982:with Abort
                -- @-others
                ]
            -- @-node:gcross.20100630111926.1980:Identity
            -- @+node:gcross.20100630111926.1983:Maybe
            ,testGroup "Maybe"
                -- @    @+others
                -- @+node:gcross.20100630111926.1984:without Abort
                [testProperty "without Abort" $
                    \(x :: Int) (y :: Int) → (== Just (x+y)) . runAbortT . fmap (+y) . lift . Just $ x
                -- @nonl
                -- @-node:gcross.20100630111926.1984:without Abort
                -- @+node:gcross.20100630111926.1985:with Abort
                ,testProperty "with Abort" $
                    \(x :: Int) (y :: Int) → (== Just x) . runAbortT . fmap (+y) . (>>= abort) . lift . Just $ x
                -- @nonl
                -- @-node:gcross.20100630111926.1985:with Abort
                -- @-others
                ]
            -- @-node:gcross.20100630111926.1983:Maybe
            -- @-others
            ]
        -- @-node:gcross.20100630111926.1979:Functor
        -- @+node:gcross.20100630111926.1986:Applicative
        ,testGroup "Applicative"
            -- @    @+others
            -- @+node:gcross.20100630111926.1987:Identity
            [testGroup "Identity"
                -- @    @+others
                -- @+node:gcross.20100630111926.1988:without Abort
                [testProperty "without Abort" $
                    \(x :: Int) (y :: Int) → runGoto (return (+y) <*> return x) == x+y
                -- @nonl
                -- @-node:gcross.20100630111926.1988:without Abort
                -- @+node:gcross.20100630111926.1989:with Abort
                ,testProperty "with Abort" $
                    \(x :: Int) (y :: Int) → runAbort (return (+y) <*> abort x) == x
                -- @nonl
                -- @-node:gcross.20100630111926.1989:with Abort
                -- @-others
                ]
            -- @-node:gcross.20100630111926.1987:Identity
            -- @-others
            ]
        -- @-node:gcross.20100630111926.1986:Applicative
        -- @+node:gcross.20100630111926.1994:Monad
        ,testGroup "Monad"
            -- @    @+others
            -- @+node:gcross.20100630111926.1995:Maybe
            [testGroup "Maybe"
                -- @    @+others
                -- @+node:gcross.20100630111926.1996:Just
                [testGroup "Just"
                    -- @    @+others
                    -- @+node:gcross.20100630111926.1997:without Abort
                    [testProperty "without Abort" $
                        \(x :: Int) (y :: Int) → (== Just (x+y)) . runAbortT $ do
                            a ← lift (Just x)
                            b ← lift (Just y)
                            return (a+b)
                    -- @nonl
                    -- @-node:gcross.20100630111926.1997:without Abort
                    -- @+node:gcross.20100630111926.1998:with Abort
                    ,testProperty "with Abort" $
                        \(x :: Int) (y :: Int) → (== Just x) . runAbortT $ do
                            a ← lift (Just x)
                            abort a
                            b ← lift (Just y)
                            return (a+b)
                    -- @nonl
                    -- @-node:gcross.20100630111926.1998:with Abort
                    -- @-others
                    ]
                -- @-node:gcross.20100630111926.1996:Just
                -- @+node:gcross.20100630111926.1999:Nothing
                ,testGroup "Nothing"
                    -- @    @+others
                    -- @+node:gcross.20100630111926.2000:without Abort
                    [testProperty "without Abort" $
                        \(x :: Int) (y :: Int) → (== Nothing) . runAbortT $ do
                            a ← lift (Just x)
                            b ← lift (Just y)
                            lift Nothing
                            return (a+b)
                    -- @nonl
                    -- @-node:gcross.20100630111926.2000:without Abort
                    -- @+node:gcross.20100630111926.2001:with Abort
                    ,testProperty "with Abort" $
                        \(x :: Int) (y :: Int) → (== Just x) . runAbortT $ do
                            a ← lift (Just x)
                            abort a
                            b ← lift (Just y)
                            lift Nothing
                            return (a+b)
                    -- @nonl
                    -- @-node:gcross.20100630111926.2001:with Abort
                    -- @-others
                    ]
                -- @-node:gcross.20100630111926.1999:Nothing
                -- @-others
                ]
            -- @-node:gcross.20100630111926.1995:Maybe
            -- @+node:gcross.20100630111926.2002:State
            ,testGroup "State"
                -- @    @+others
                -- @+node:gcross.20100630111926.2003:without Abort
                [testProperty "without Abort" $
                    \(x :: Int) (y :: Int) → (== x+y) . flip execState x . runAbortT $ do
                        lift (modify (+y))
                -- @nonl
                -- @-node:gcross.20100630111926.2003:without Abort
                -- @+node:gcross.20100630111926.2004:with Abort
                ,testProperty "with Abort" $
                    \(x :: Int) (y :: Int) → (== x) . flip execState x . runAbortT $ do
                        abort ()
                        lift (modify (+y))
                -- @nonl
                -- @-node:gcross.20100630111926.2004:with Abort
                -- @-others
                ]
            -- @-node:gcross.20100630111926.2002:State
            -- @-others
            ]
        -- @-node:gcross.20100630111926.1994:Monad
        -- @-others
        ]
    -- @nonl
    -- @-node:gcross.20100630111926.1978:Control.Monad.Trans.Abort
    -- @+node:gcross.20100624100717.1917:Control.Monad.Trans.Goto
    ,testGroup "Control.Monad.Trans.Goto"
        -- @    @+others
        -- @+node:gcross.20100624100717.1918:Functor
        [testGroup "Functor"
            -- @    @+others
            -- @+node:gcross.20100624100717.1919:Identity
            [testGroup "Identity"
                -- @    @+others
                -- @+node:gcross.20100624100717.1920:without goto
                [testProperty "without goto" $
                    \(x :: Int) (y :: Int) → (== x+y) . runGoto . fmap (+y) . return $ x
                -- @-node:gcross.20100624100717.1920:without goto
                -- @+node:gcross.20100624100717.1922:with goto
                ,testProperty "with goto" $
                    \(x :: Int) (y :: Int) → (== x) . runGoto . fmap (+y) . goto . return $ x
                -- @-node:gcross.20100624100717.1922:with goto
                -- @-others
                ]
            -- @-node:gcross.20100624100717.1919:Identity
            -- @+node:gcross.20100624100717.1926:Maybe
            ,testGroup "Maybe"
                -- @    @+others
                -- @+node:gcross.20100624100717.1927:without goto
                [testProperty "without goto" $
                    \(x :: Int) (y :: Int) → (== Just (x+y)) . runGotoT . fmap (+y) . lift . Just $ x
                -- @-node:gcross.20100624100717.1927:without goto
                -- @+node:gcross.20100624100717.1928:with goto
                ,testProperty "with goto" $
                    \(x :: Int) (y :: Int) → (== Just x) . runGotoT . fmap (+y) . goto . lift . Just $ x
                -- @-node:gcross.20100624100717.1928:with goto
                -- @-others
                ]
            -- @-node:gcross.20100624100717.1926:Maybe
            -- @-others
            ]
        -- @-node:gcross.20100624100717.1918:Functor
        -- @+node:gcross.20100624100717.1936:Applicative
        ,testGroup "Applicative"
            -- @    @+others
            -- @+node:gcross.20100624100717.1937:Identity
            [testGroup "Identity"
                -- @    @+others
                -- @+node:gcross.20100624100717.1938:without goto
                [testProperty "without goto" $
                    \(x :: Int) (y :: Int) → runGoto (return (+y) <*> return x) == x+y
                -- @-node:gcross.20100624100717.1938:without goto
                -- @+node:gcross.20100624100717.1939:with goto
                ,testProperty "with goto" $
                    \(x :: Int) (y :: Int) → runGoto (return (+y) <*> goto (return x)) == x
                -- @-node:gcross.20100624100717.1939:with goto
                -- @-others
                ]
            -- @-node:gcross.20100624100717.1937:Identity
            -- @+node:gcross.20100624100717.1943:Maybe
            ,testGroup "Maybe"
                -- @    @+others
                -- @+node:gcross.20100624100717.1940:Just
                [testGroup "Just"
                    -- @    @+others
                    -- @+node:gcross.20100624100717.1941:without goto
                    [testProperty "without goto" $
                        \(x :: Int) (y :: Int) → runGotoT (lift (Just (+y)) <*> lift (Just x)) == Just (x+y)
                    -- @-node:gcross.20100624100717.1941:without goto
                    -- @+node:gcross.20100624100717.1942:with goto
                    ,testProperty "with goto" $
                        \(x :: Int) (y :: Int) → runGotoT (lift (Just (+y)) <*> goto (lift (Just x))) == Just x
                    -- @-node:gcross.20100624100717.1942:with goto
                    -- @-others
                    ]
                -- @-node:gcross.20100624100717.1940:Just
                -- @-others
                ]
            -- @-node:gcross.20100624100717.1943:Maybe
            -- @-others
            ]
        -- @-node:gcross.20100624100717.1936:Applicative
        -- @+node:gcross.20100624100717.1970:Monad
        ,testGroup "Monad"
            -- @    @+others
            -- @+node:gcross.20100624100717.1981:Maybe
            [testGroup "Maybe"
                -- @    @+others
                -- @+node:gcross.20100624100717.1982:Just
                [testGroup "Just"
                    -- @    @+others
                    -- @+node:gcross.20100624100717.1983:without goto
                    [testProperty "without goto" $
                        \(x :: Int) (y :: Int) → (== Just (x+y)) . runGotoT $ do
                            a ← lift (Just x)
                            b ← lift (Just y)
                            return (a+b)
                    -- @-node:gcross.20100624100717.1983:without goto
                    -- @+node:gcross.20100624100717.1984:with goto
                    ,testProperty "with goto" $
                        \(x :: Int) (y :: Int) → (== Just x) . runGotoT $ do
                            a ← lift (Just x)
                            goto $ return a
                            b ← lift (Just y)
                            return (a+b)
                    -- @nonl
                    -- @-node:gcross.20100624100717.1984:with goto
                    -- @-others
                    ]
                -- @-node:gcross.20100624100717.1982:Just
                -- @+node:gcross.20100624100717.1988:Nothing
                ,testGroup "Nothing"
                    -- @    @+others
                    -- @+node:gcross.20100624100717.1989:without goto
                    [testProperty "without goto" $
                        \(x :: Int) (y :: Int) → (== Nothing) . runGotoT $ do
                            a ← lift (Just x)
                            b ← lift (Just y)
                            lift Nothing
                            return (a+b)
                    -- @-node:gcross.20100624100717.1989:without goto
                    -- @+node:gcross.20100624100717.1990:with goto
                    ,testProperty "with goto" $
                        \(x :: Int) (y :: Int) → (== Just x) . runGotoT $ do
                            a ← lift (Just x)
                            goto $ return a
                            b ← lift (Just y)
                            lift Nothing
                            return (a+b)
                    -- @nonl
                    -- @-node:gcross.20100624100717.1990:with goto
                    -- @-others
                    ]
                -- @-node:gcross.20100624100717.1988:Nothing
                -- @-others
                ]
            -- @-node:gcross.20100624100717.1981:Maybe
            -- @+node:gcross.20100624100717.1991:State
            ,testGroup "State"
                -- @    @+others
                -- @+node:gcross.20100624100717.1992:without goto
                [testProperty "without goto" $
                    \(x :: Int) (y :: Int) → (== x+y) . flip execState x . runGotoT $ do
                        lift (modify (+y))
                -- @-node:gcross.20100624100717.1992:without goto
                -- @+node:gcross.20100624100717.1994:with goto
                ,testProperty "with goto" $
                    \(x :: Int) (y :: Int) → (== x) . flip execState x . runGotoT $ do
                        goto $ return ()
                        lift (modify (+y))
                -- @-node:gcross.20100624100717.1994:with goto
                -- @-others
                ]
            -- @-node:gcross.20100624100717.1991:State
            -- @-others
            ]
        -- @-node:gcross.20100624100717.1970:Monad
        -- @-others
        ]
    -- @-node:gcross.20100624100717.1917:Control.Monad.Trans.Goto
    -- @+node:gcross.20100628115452.1875:Blueprint.Identifier
    ,testGroup "Blueprint.Identifier" $
        -- @    @+others
        -- @+node:gcross.20100628115452.1885:sortIntoLabeledBins
        [testGroup "sortIntoLabeledBins" $
            -- @    @+others
            -- @+node:gcross.20100628115452.1886:trivial
            [testCase "trivial" $
                assertEqual
                    "Is the resulting map correct?"
                    (Map.empty :: Map (Identifier ()) [()])
                .
                sortIntoLabeledBins
                $
                []
            -- @-node:gcross.20100628115452.1886:trivial
            -- @+node:gcross.20100628115452.1888:singleton
            ,testCase "singleton" $
                assertEqual
                    "Is the resulting map correct?"
                    (Map.singleton test_identifier [()])
                .
                sortIntoLabeledBins
                $
                [(test_identifier,())]
            -- @-node:gcross.20100628115452.1888:singleton
            -- @+node:gcross.20100628115452.1894:random data
            ,testProperty "random data" $ do
                identifiers ←
                    choose (1,10)
                    >>=
                    fmap (zipWith (\index uuid → Identifier uuid ("uuid #" ++ show index)) [1..])
                    .
                    flip vectorOf arbitrary
                random_data ←
                    choose (1,100)
                    >>=
                    flip vectorOf (liftA2 (,) (elements identifiers) (arbitrary :: Gen Int))
                let binned_data = sortIntoLabeledBins random_data
                return
                    .
                    all (
                        \identifier →
                            (map snd . filter ((== identifier) . fst) $ random_data)
                            ==
                            fromMaybe [] (Map.lookup identifier binned_data)
                    )

                    $
                    identifiers
            -- @-node:gcross.20100628115452.1894:random data
            -- @-others
            ]
        -- @-node:gcross.20100628115452.1885:sortIntoLabeledBins
        -- @-others
        ]
    -- @-node:gcross.20100628115452.1875:Blueprint.Identifier
    -- @+node:gcross.20100706002630.1965:Blueprint.Tools.JobAnalyzer
    ,testGroup "Blueprint.Tools.JobAnalyzer" $
        -- @    @+others
        -- @+node:gcross.20100706002630.1968:writeToCache
        [testCase "writeToCache" $
            let job_id = Identifier UUID.nil "job"
                job_ids = [job_id]
                correct_cache :: SerializableRecord
                correct_cache = withField _a 42
            in withJobServer 1 Map.empty $ \job_server → do
                submitJob job_server job_ids . runJobAnalyzer $ do
                    writeToCache _a 42
                    return [undefined]
                _ ← requestJobResult job_server job_id
                new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                assertEqual
                    "Is the cache correct?"
                    (encode correct_cache)
                    new_cache
        -- @-node:gcross.20100706002630.1968:writeToCache
        -- @+node:gcross.20100706002630.1970:readAndCache
        ,testCase "readAndCache" $
            let job_id = Identifier UUID.nil "job"
                job_ids = [job_id]
                cache :: SerializableRecord
                cache = withField _a 42
            in withJobServer 1 (Map.singleton job_ids (encode cache)) $ \job_server → do
                submitJob job_server job_ids . runJobAnalyzer $ do
                    maybe_a ← readAndCache _a
                    case maybe_a of
                        Nothing → return [emptyTable]
                        Just a → return [withField _a a]
                result ← requestJobResult job_server job_id
                new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                assertEqual
                    "Is the result correct?"
                    (Just 42)
                    (getField _a result)
                assertEqual
                    "Is the cache correct?"
                    (encode cache)
                    new_cache
        -- @-node:gcross.20100706002630.1970:readAndCache
        -- @+node:gcross.20100706002630.1972:readRequiredAndCache
        ,testCase "readRequiredAndCache" $
            let job_id = Identifier UUID.nil "job"
                job_ids = [job_id]
                cache :: SerializableRecord
                cache = withField _a 42
            in withJobServer 1 (Map.singleton job_ids (encode cache)) $ \job_server → do
                submitJob job_server job_ids . runJobAnalyzer $ do
                    a ← readRequiredAndCache _a
                    return [withField _a a]
                result ← requestJobResult job_server job_id
                new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                assertEqual
                    "Is the result correct?"
                    (Just 42)
                    (getField _a result)
                assertEqual
                    "Is the cache correct?"
                    (encode cache)
                    new_cache
        -- @-node:gcross.20100706002630.1972:readRequiredAndCache
        -- @+node:gcross.20100706002630.1975:checkForChangesIn
        ,testGroup "checkForChangesIn" $
            -- @    @+others
            -- @+node:gcross.20100706002630.1974:no cache
            [testCase "no cache" $
                let job_id = Identifier UUID.nil "job"
                    job_ids = [job_id]
                    correct_cache :: SerializableRecord
                    correct_cache = withField _a 42
                in withJobServer 1 Map.empty $ \job_server → do
                    submitJob job_server job_ids . runJobAnalyzer $ do
                        c ← checkForChangesIn _a 42
                        return [withField _c c]
                    result ← requestJobResult job_server job_id
                    new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                    assertEqual
                        "Is the result correct?"
                        (Just True)
                        (getField _c result)
                    assertEqual
                        "Is the cache correct?"
                        (encode correct_cache)
                        new_cache
            -- @-node:gcross.20100706002630.1974:no cache
            -- @+node:gcross.20100706002630.1977:changed
            ,testCase "changed" $
                let job_id = Identifier UUID.nil "job"
                    job_ids = [job_id]
                    cache :: SerializableRecord
                    cache = withField _a 24
                    correct_cache :: SerializableRecord
                    correct_cache = withField _a 42
                in withJobServer 1 (Map.singleton job_ids (encode cache)) $ \job_server → do
                    submitJob job_server job_ids . runJobAnalyzer $ do
                        c ← checkForChangesIn _a 42
                        return [withField _c c]
                    result ← requestJobResult job_server job_id
                    new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                    assertEqual
                        "Is the result correct?"
                        (Just True)
                        (getField _c result)
                    assertEqual
                        "Is the cache correct?"
                        (encode correct_cache)
                        new_cache
            -- @-node:gcross.20100706002630.1977:changed
            -- @+node:gcross.20100706002630.1979:unchanged
            ,testCase "unchanged" $
                let job_id = Identifier UUID.nil "job"
                    job_ids = [job_id]
                    cache :: SerializableRecord
                    cache = withField _a 42
                    correct_cache :: SerializableRecord
                    correct_cache = withField _a 42
                in withJobServer 1 (Map.singleton job_ids (encode cache)) $ \job_server → do
                    submitJob job_server job_ids . runJobAnalyzer $ do
                        c ← checkForChangesIn _a 42
                        return [withField _c c]
                    result ← requestJobResult job_server job_id
                    new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                    assertEqual
                        "Is the result correct?"
                        (Just False)
                        (getField _c result)
                    assertEqual
                        "Is the cache correct?"
                        (encode correct_cache)
                        new_cache
            -- @-node:gcross.20100706002630.1979:unchanged
            -- @-others
            ]
        -- @-node:gcross.20100706002630.1975:checkForChangesIn
        -- @+node:gcross.20100706002630.1981:runTaskAndCacheResult
        ,testCase "runTaskAndCacheResult" $
            let job_id = Identifier UUID.nil "job"
                job_ids = [job_id]
                correct_cache :: SerializableRecord
                correct_cache = withField _a 42
            in withJobServer 1 Map.empty $ \job_server → do
                submitJob job_server job_ids . runJobAnalyzer $ do
                    a ← runTaskAndCacheResult _a (return 42)
                    return [withField _a a]
                result ← requestJobResult job_server job_id
                new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                assertEqual
                    "Is the result correct?"
                    (Just 42)
                    (getField _a result)
                assertEqual
                    "Is the cache correct?"
                    (encode correct_cache)
                    new_cache
        -- @-node:gcross.20100706002630.1981:runTaskAndCacheResult
        -- @+node:gcross.20100706002630.1982:rerunTaskAndCacheResultOnlyIf
        ,testGroup "rerunTaskAndCacheResultOnlyIf" $
            -- @    @+others
            -- @+node:gcross.20100706002630.1984:True
            [testCase "True" $
                let job_id = Identifier UUID.nil "job"
                    job_ids = [job_id]
                    cache :: SerializableRecord
                    cache = withField _a 24
                    correct_cache :: SerializableRecord
                    correct_cache = withField _a 42
                in withJobServer 1 (Map.singleton job_ids (encode cache)) $ \job_server → do
                    submitJob job_server job_ids . runJobAnalyzer $ do
                        a ← rerunTaskAndCacheResultOnlyIf _a (return 42) True
                        return [withField _a a]
                    result ← requestJobResult job_server job_id
                    new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                    assertEqual
                        "Is the result correct?"
                        (Just 42)
                        (getField _a result)
                    assertEqual
                        "Is the cache correct?"
                        (encode correct_cache)
                        new_cache
            -- @-node:gcross.20100706002630.1984:True
            -- @+node:gcross.20100706002630.1986:False
            ,testCase "False" $
                let job_id = Identifier UUID.nil "job"
                    job_ids = [job_id]
                    cache :: SerializableRecord
                    cache = withField _a 24
                    correct_cache :: SerializableRecord
                    correct_cache = withField _a 24
                in withJobServer 1 (Map.singleton job_ids (encode cache)) $ \job_server → do
                    submitJob job_server job_ids . runJobAnalyzer $ do
                        a ← rerunTaskAndCacheResultOnlyIf _a (return 42) False
                        return [withField _a a]
                    result ← requestJobResult job_server job_id
                    new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                    assertEqual
                        "Is the result correct?"
                        (Just 24)
                        (getField _a result)
                    assertEqual
                        "Is the cache correct?"
                        (encode correct_cache)
                        new_cache
            -- @-node:gcross.20100706002630.1986:False
            -- @-others
            ]
        -- @-node:gcross.20100706002630.1982:rerunTaskAndCacheResultOnlyIf
        -- @+node:gcross.20100706002630.1990:fetchDigestsAndCheckForChanges
        ,testGroup "fetchDigestsAndCheckForChanges" $
            -- @    @+others
            -- @+node:gcross.20100706002630.1991:no changes
            [testCase "no changes" $
                let job_id = Identifier UUID.nil "job"
                    job_ids = [job_id]

                    source_job_id = identifier "4be9942a-adee-40a2-9914-2070ca3ae90f" "job"
                    source_job_ids = [source_job_id]
                    source_digest = md5 . L.pack $ "Source digest"

                    cache :: SerializableRecord
                    cache = withField _digests [source_digest]
                    correct_cache :: SerializableRecord
                    correct_cache = withField _digests [source_digest]
                in withJobServer 1 (Map.singleton job_ids (encode cache)) $ \job_server → do
                    submitJob job_server source_job_ids $
                        const . returnValue . withFields $ ((_digest,source_digest):.())
                    submitJob job_server job_ids . runJobAnalyzer $ do
                        bool ← fetchDigestsAndCheckForChanges _digests [source_job_id]
                        return [withField _bool bool]
                    result ← requestJobResult job_server job_id
                    new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                    assertEqual
                        "Is the result correct?"
                        (Just False)
                        (getField _bool result)
                    assertEqual
                        "Is the cache correct?"
                        (encode correct_cache)
                        new_cache
            -- @-node:gcross.20100706002630.1991:no changes
            -- @+node:gcross.20100706002630.1994:changes
            ,testCase "changes" $
                let job_id = Identifier UUID.nil "job"
                    job_ids = [job_id]

                    source_job_id = identifier "4be9942a-adee-40a2-9914-2070ca3ae90f" "job"
                    source_job_ids = [source_job_id]
                    source_digest = md5 . L.pack $ "Source digest"

                    cache :: SerializableRecord
                    cache = withField _digests []
                    correct_cache :: SerializableRecord
                    correct_cache = withField _digests [source_digest]
                in withJobServer 1 (Map.singleton job_ids (encode cache)) $ \job_server → do
                    submitJob job_server source_job_ids $
                        const . returnValue . withFields $ ((_digest,source_digest):.())
                    submitJob job_server job_ids . runJobAnalyzer $ do
                        bool ← fetchDigestsAndCheckForChanges _digests [source_job_id]
                        return [withField _bool bool]
                    result ← requestJobResult job_server job_id
                    new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                    assertEqual
                        "Is the result correct?"
                        (Just True)
                        (getField _bool result)
                    assertEqual
                        "Is the cache correct?"
                        (encode correct_cache)
                        new_cache
            -- @-node:gcross.20100706002630.1994:changes
            -- @-others
            ]
        -- @-node:gcross.20100706002630.1990:fetchDigestsAndCheckForChanges
        -- @+node:gcross.20100706002630.1997:analyzeDependenciesAndRebuildIfNecessary
        ,testGroup "analyzeDependenciesAndRebuildIfNecessary" $
            let source_digests_field =
                    field "source digests" "da0f7975-5565-43ba-a253-746d37cf5ca8"
                        :: Field [MD5Digest]
                implicit_dependencies_field =
                    field "implicit dependencies" "65b0972e-c17f-426b-a00d-8cc8cb52dc4b"
                        :: Field [UnresolvedDependency]
                dependencies_and_digests_field =
                    field "dependencies and digests" "4292b8d0-5d15-49de-8032-92be8e67680f"
                        :: Field ([UnresolvedDependency],[MD5Digest])
                product_digests_field =
                    field "product digests" "101ba0a4-35a9-44f7-98c0-87d26027a375"
                        :: Field [MD5Digest]
                miscellaneous_information_field :: forall a. (Binary a, Typeable a) ⇒ Field a
                miscellaneous_information_field = field "miscellaneous information" "b77f1940-ac94-4a22-980f-e0f52c26af28"  
            in
                -- @        @+others
                -- @+node:gcross.20100706002630.1999:no cache
                [testCase "no cache" $
                    let job_id = Identifier UUID.nil "job"
                        job_ids = [job_id]
                        product_digest = md5 . L.pack $ "Job results"
                        product_digests = [product_digest]
                        correct_cache :: SerializableRecord
                        correct_cache = withFields (
                            (source_digests_field, [] :: [MD5Digest])
                         :. (implicit_dependencies_field, [] :: [UnresolvedDependency])
                         :. (dependencies_and_digests_field, ([],[]) :: ([UnresolvedDependency],[MD5Digest]))
                         :. (miscellaneous_information_field :: Field (), ())
                         :. (product_digests_field, product_digests)
                         :. () )
                    in withJobServer 1 Map.empty $ \job_server → do
                        scanner_called_ref ← newIORef False
                        builder_called_ref ← newIORef False
                        digester_ignored_ref ← newIORef True
                        submitJob job_server [job_id] . runJobAnalyzer $
                            analyzeImplicitDependenciesAndRebuildIfNecessary
                                (liftIO (writeIORef scanner_called_ref True) >> return [])
                                (liftIO (writeIORef builder_called_ref True) >> return product_digests)
                                (const $ liftIO (writeIORef digester_ignored_ref False) >> return False)
                                undefined
                                ()
                                []
                                []
                        result ← requestJobResult job_server job_id
                        cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                        readIORef scanner_called_ref >>= assertBool "Was the scanner called?"
                        readIORef builder_called_ref >>= assertBool "Was the builder called?"
                        readIORef digester_ignored_ref >>= assertBool "Was the digester ignored?"
                        assertEqual
                            "Is the digest correct?"
                            (Just product_digest)
                            (getField _digest result)
                        assertEqual
                            "Are the deferred dependencies correct?"
                            (Just [])
                            (getField _deferred_dependencies result)
                        assertEqual
                            "Is the cache correct?"
                            (encode correct_cache)
                            cache
                -- @-node:gcross.20100706002630.1999:no cache
                -- @+node:gcross.20100706133004.1984:no-op
                ,testCase "no-op" $
                    let job_id = Identifier UUID.nil "job"
                        job_ids = [job_id]
                        product_digest = md5 . L.pack $ "Job results"
                        product_digests = [product_digest]
                        cache :: SerializableRecord
                        cache = withFields (
                            (source_digests_field, [] :: [MD5Digest])
                         :. (implicit_dependencies_field, [] :: [UnresolvedDependency])
                         :. (dependencies_and_digests_field, ([],[]) :: ([UnresolvedDependency],[MD5Digest]))
                         :. (miscellaneous_information_field :: Field (), ())
                         :. (product_digests_field, product_digests)
                         :. () )
                    in withJobServer 1 (Map.singleton job_ids (encode cache)) $ \job_server → do
                        scanner_ignored_ref ← newIORef True
                        builder_ignored_ref ← newIORef True
                        digester_called_ref ← newIORef False
                        submitJob job_server job_ids . runJobAnalyzer $
                            analyzeImplicitDependenciesAndRebuildIfNecessary
                                (liftIO (writeIORef scanner_ignored_ref False) >> return [])
                                (liftIO (writeIORef builder_ignored_ref False) >> return product_digests)
                                (const $ liftIO (writeIORef digester_called_ref True) >> return True)
                                undefined
                                ()
                                []
                                []
                        result ← requestJobResult job_server job_id
                        new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                        readIORef scanner_ignored_ref >>= assertBool "Was the scanner ignored?"
                        readIORef builder_ignored_ref >>= assertBool "Was the builder ignored?"
                        readIORef digester_called_ref >>= assertBool "Was the digester called?"
                        assertEqual
                            "Is the digest correct?"
                            (Just product_digest)
                            (getField _digest result)
                        assertEqual
                            "Are the deferred dependencies correct?"
                            (Just [])
                            (getField _deferred_dependencies result)
                        assertEqual
                            "Is the cache correct?"
                            (encode cache)
                            new_cache
                -- @-node:gcross.20100706133004.1984:no-op
                -- @+node:gcross.20100706133004.1986:modified sources
                ,testCase "modified sources" $
                    let job_id = Identifier UUID.nil "job"
                        job_ids = [job_id]
                        product_digest = md5 . L.pack $ "Job results"
                        product_digests = [product_digest]
                        cache :: SerializableRecord
                        cache = withFields (
                            (source_digests_field, product_digests)
                         :. (implicit_dependencies_field, [] :: [UnresolvedDependency])
                         :. (dependencies_and_digests_field, ([],[]) :: ([UnresolvedDependency],[MD5Digest]))
                         :. (miscellaneous_information_field :: Field (), ())
                         :. (product_digests_field, product_digests)
                         :. () )
                        source_job_id = identifier "4be9942a-adee-40a2-9914-2070ca3ae90f" "job"
                        source_job_ids = [source_job_id]
                        source_digest = md5 . L.pack $ "Source digest"
                        source_digests = [source_digest]
                        correct_cache :: SerializableRecord
                        correct_cache = withFields (
                            (source_digests_field, source_digests)
                         :. (implicit_dependencies_field, [] :: [UnresolvedDependency])
                         :. (dependencies_and_digests_field, ([],[]) :: ([UnresolvedDependency],[MD5Digest]))
                         :. (miscellaneous_information_field :: Field (), ())
                         :. (product_digests_field, product_digests)
                         :. () )
                    in withJobServer 1 (Map.singleton job_ids (encode cache)) $ \job_server → do
                        scanner_called_ref ← newIORef False
                        builder_called_ref ← newIORef False
                        digester_ignored_ref ← newIORef True
                        submitJob job_server source_job_ids $
                            const . returnValue . withFields $ ((_digest,source_digest):.())
                        submitJob job_server job_ids . runJobAnalyzer $
                            analyzeImplicitDependenciesAndRebuildIfNecessary
                                (liftIO (writeIORef scanner_called_ref True) >> return [])
                                (liftIO (writeIORef builder_called_ref True) >> return product_digests)
                                (const $ liftIO (writeIORef digester_ignored_ref False) >> return True)
                                undefined
                                ()
                                source_job_ids
                                []
                        result ← requestJobResult job_server job_id
                        new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                        readIORef scanner_called_ref >>= assertBool "Was the scanner called?"
                        readIORef builder_called_ref >>= assertBool "Was the builder called?"
                        readIORef digester_ignored_ref >>= assertBool "Was the digester ignored?"
                        assertEqual
                            "Is the digest correct?"
                            (Just product_digest)
                            (getField _digest result)
                        assertEqual
                            "Are the deferred dependencies correct?"
                            (Just [])
                            (getField _deferred_dependencies result)
                        assertEqual
                            "Is the cache correct?"
                            (encode correct_cache)
                            new_cache
                -- @-node:gcross.20100706133004.1986:modified sources
                -- @+node:gcross.20100706133004.1988:modified explicit dependencies
                ,testCase "modified explicit dependencies" $
                    let job_id = Identifier UUID.nil "job"
                        job_ids = [job_id]
                        product_digest = md5 . L.pack $ "Job results"
                        product_digests = [product_digest]
                        cache :: SerializableRecord
                        cache = withFields (
                            (source_digests_field, [] :: [MD5Digest])
                         :. (implicit_dependencies_field, [] :: [UnresolvedDependency])
                         :. (dependencies_and_digests_field, ([],product_digests) :: ([UnresolvedDependency],[MD5Digest]))
                         :. (miscellaneous_information_field :: Field (), ())
                         :. (product_digests_field, product_digests)
                         :. () )
                        dependency_job_id = identifier "4be9942a-adee-40a2-9914-2070ca3ae90f" "job"
                        dependency_job_ids = [dependency_job_id]
                        dependency_digest = md5 . L.pack $ "Dependency results"
                        correct_cache :: SerializableRecord
                        correct_cache = withFields (
                            (source_digests_field, [] :: [MD5Digest])
                         :. (implicit_dependencies_field, [] :: [UnresolvedDependency])
                         :. (dependencies_and_digests_field, ([test_unresolved_dependency],[dependency_digest]))
                         :. (miscellaneous_information_field :: Field (), ())
                         :. (product_digests_field, product_digests)
                         :. () )
                    in withJobServer 1 (Map.singleton job_ids (encode cache)) $ \job_server → do
                        scanner_ignored_ref ← newIORef True
                        builder_called_ref ← newIORef False
                        digester_ignored_ref ← newIORef True
                        submitJob job_server dependency_job_ids $
                            const . returnValue . withFields $ ((_digest,dependency_digest):.())
                        submitJob job_server job_ids . runJobAnalyzer $
                            analyzeImplicitDependenciesAndRebuildIfNecessary
                                (liftIO (writeIORef scanner_ignored_ref False) >> return [])
                                (liftIO (writeIORef builder_called_ref True) >> return product_digests)
                                (const $ liftIO (writeIORef digester_ignored_ref False) >> return True)
                                (const . return . Right $ ResolvedDependencies [dependency_job_id] [test_dependency_2])
                                ()
                                []
                                [test_unresolved_dependency]
                        result ← requestJobResult job_server job_id
                        new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                        readIORef scanner_ignored_ref >>= assertBool "Was the scanner ignored?"
                        readIORef builder_called_ref >>= assertBool "Was the builder called?"
                        readIORef digester_ignored_ref >>= assertBool "Was the digester ignored?"
                        assertEqual
                            "Is the digest correct?"
                            (Just product_digest)
                            (getField _digest result)
                        assertEqual
                            "Are the deferred dependencies correct?"
                            (Just [test_dependency_2])
                            (getField _deferred_dependencies result)
                        assertEqual
                            "Is the cache correct?"
                            (encode correct_cache)
                            new_cache
                -- @-node:gcross.20100706133004.1988:modified explicit dependencies
                -- @+node:gcross.20100706133004.1990:modified explicit dependency digests
                ,testCase "modified explicit dependency digests" $
                    let job_id = Identifier UUID.nil "job"
                        job_ids = [job_id]
                        product_digest = md5 . L.pack $ "Job results"
                        product_digests = [product_digest]
                        cache :: SerializableRecord
                        cache = withFields (
                            (source_digests_field, [] :: [MD5Digest])
                         :. (implicit_dependencies_field, [] :: [UnresolvedDependency])
                         :. (dependencies_and_digests_field, ([test_unresolved_dependency],[product_digest]))
                         :. (miscellaneous_information_field :: Field (), ())
                         :. (product_digests_field, product_digests)
                         :. () )
                        dependency_job_id = identifier "4be9942a-adee-40a2-9914-2070ca3ae90f" "job"
                        dependency_job_ids = [dependency_job_id]
                        dependency_digest = md5 . L.pack $ "Dependency results"
                        correct_cache :: SerializableRecord
                        correct_cache = withFields (
                            (source_digests_field, [] :: [MD5Digest])
                         :. (implicit_dependencies_field, [] :: [UnresolvedDependency])
                         :. (dependencies_and_digests_field, ([test_unresolved_dependency],[dependency_digest]))
                         :. (miscellaneous_information_field :: Field (), ())
                         :. (product_digests_field, product_digests)
                         :. () )
                    in withJobServer 1 (Map.singleton job_ids (encode cache)) $ \job_server → do
                        scanner_ignored_ref ← newIORef True
                        builder_called_ref ← newIORef False
                        digester_ignored_ref ← newIORef True
                        submitJob job_server dependency_job_ids $
                            const . returnValue . withFields $ ((_digest,dependency_digest):.())
                        submitJob job_server job_ids . runJobAnalyzer $
                            analyzeImplicitDependenciesAndRebuildIfNecessary
                                (liftIO (writeIORef scanner_ignored_ref False) >> return [])
                                (liftIO (writeIORef builder_called_ref True) >> return product_digests)
                                (const $ liftIO (writeIORef digester_ignored_ref False) >> return True)
                                (const . return . Right $ ResolvedDependencies [dependency_job_id] [test_dependency_2])
                                ()
                                []
                                [test_unresolved_dependency]
                        result ← requestJobResult job_server job_id
                        new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                        readIORef scanner_ignored_ref >>= assertBool "Was the scanner ignored?"
                        readIORef builder_called_ref >>= assertBool "Was the builder called?"
                        readIORef digester_ignored_ref >>= assertBool "Was the digester ignored?"
                        assertEqual
                            "Is the digest correct?"
                            (Just product_digest)
                            (getField _digest result)
                        assertEqual
                            "Are the deferred dependencies correct?"
                            (Just [test_dependency_2])
                            (getField _deferred_dependencies result)
                        assertEqual
                            "Is the cache correct?"
                            (encode correct_cache)
                            new_cache
                -- @-node:gcross.20100706133004.1990:modified explicit dependency digests
                -- @+node:gcross.20100706133004.1992:modified implicit dependency digests
                ,testCase "modified implicit dependency digests" $
                    let job_id = Identifier UUID.nil "job"
                        job_ids = [job_id]
                        product_digest = md5 . L.pack $ "Job results"
                        product_digests = [product_digest]
                        cache :: SerializableRecord
                        cache = withFields (
                            (source_digests_field, [] :: [MD5Digest])
                         :. (implicit_dependencies_field, [test_unresolved_dependency])
                         :. (dependencies_and_digests_field, ([test_unresolved_dependency],[product_digest]))
                         :. (miscellaneous_information_field :: Field (), ())
                         :. (product_digests_field, product_digests)
                         :. () )
                        dependency_job_id = identifier "4be9942a-adee-40a2-9914-2070ca3ae90f" "job"
                        dependency_job_ids = [dependency_job_id]
                        dependency_digest = md5 . L.pack $ "Dependency results"
                        correct_cache :: SerializableRecord
                        correct_cache = withFields (
                            (source_digests_field, [] :: [MD5Digest])
                         :. (implicit_dependencies_field, [test_unresolved_dependency])
                         :. (dependencies_and_digests_field, ([test_unresolved_dependency],[dependency_digest]))
                         :. (miscellaneous_information_field :: Field (), ())
                         :. (product_digests_field, product_digests)
                         :. () )
                    in withJobServer 1 (Map.singleton job_ids (encode cache)) $ \job_server → do
                        scanner_ignored_ref ← newIORef True
                        builder_called_ref ← newIORef False
                        digester_ignored_ref ← newIORef True
                        submitJob job_server dependency_job_ids $
                            const . returnValue . withFields $ ((_digest,dependency_digest):.())
                        submitJob job_server job_ids . runJobAnalyzer $
                            analyzeImplicitDependenciesAndRebuildIfNecessary
                                (liftIO (writeIORef scanner_ignored_ref False) >> return [test_unresolved_dependency])
                                (liftIO (writeIORef builder_called_ref True) >> return product_digests)
                                (const $ liftIO (writeIORef digester_ignored_ref False) >> return True)
                                (const . return . Right $ ResolvedDependencies [dependency_job_id] [test_dependency_2])
                                ()
                                []
                                []
                        result ← requestJobResult job_server job_id
                        new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                        readIORef scanner_ignored_ref >>= assertBool "Was the scanner ignored?"
                        readIORef builder_called_ref >>= assertBool "Was the builder called?"
                        readIORef digester_ignored_ref >>= assertBool "Was the digester ignored?"
                        assertEqual
                            "Is the digest correct?"
                            (Just product_digest)
                            (getField _digest result)
                        assertEqual
                            "Are the deferred dependencies correct?"
                            (Just [test_dependency_2])
                            (getField _deferred_dependencies result)
                        assertEqual
                            "Is the cache correct?"
                            (encode correct_cache)
                            new_cache
                -- @-node:gcross.20100706133004.1992:modified implicit dependency digests
                -- @+node:gcross.20100706133004.1994:modified miscellaneous information
                ,testCase "modified miscellaneous information" $
                    let job_id = Identifier UUID.nil "job"
                        job_ids = [job_id]
                        product_digest = md5 . L.pack $ "Job results"
                        product_digests = [product_digest]
                        cache, correct_cache :: SerializableRecord
                        cache = withFields (
                            (source_digests_field, [] :: [MD5Digest])
                         :. (implicit_dependencies_field, [] :: [UnresolvedDependency])
                         :. (dependencies_and_digests_field, ([],[]) :: ([UnresolvedDependency],[MD5Digest]))
                         :. (miscellaneous_information_field :: Field Bool, True)
                         :. (product_digests_field, product_digests)
                         :. () )
                        correct_cache = withFields (
                            (source_digests_field, [] :: [MD5Digest])
                         :. (implicit_dependencies_field, [] :: [UnresolvedDependency])
                         :. (dependencies_and_digests_field, ([],[]) :: ([UnresolvedDependency],[MD5Digest]))
                         :. (miscellaneous_information_field :: Field Bool, False)
                         :. (product_digests_field, product_digests)
                         :. () )
                    in withJobServer 1 (Map.singleton job_ids (encode cache)) $ \job_server → do
                        scanner_ignored_ref ← newIORef True
                        builder_called_ref ← newIORef False
                        digester_ignored_ref ← newIORef True
                        submitJob job_server job_ids . runJobAnalyzer $
                            analyzeImplicitDependenciesAndRebuildIfNecessary
                                (liftIO (writeIORef scanner_ignored_ref False) >> return [])
                                (liftIO (writeIORef builder_called_ref True) >> return product_digests)
                                (const $ liftIO (writeIORef digester_ignored_ref False) >> return True)
                                undefined
                                False
                                []
                                []
                        result ← requestJobResult job_server job_id
                        new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                        readIORef scanner_ignored_ref >>= assertBool "Was the scanner ignored?"
                        readIORef builder_called_ref >>= assertBool "Was the builder called?"
                        readIORef digester_ignored_ref >>= assertBool "Was the digester ignored?"
                        assertEqual
                            "Is the digest correct?"
                            (Just product_digest)
                            (getField _digest result)
                        assertEqual
                            "Are the deferred dependencies correct?"
                            (Just [])
                            (getField _deferred_dependencies result)
                        assertEqual
                            "Is the cache correct?"
                            (encode correct_cache)
                            new_cache
                -- @-node:gcross.20100706133004.1994:modified miscellaneous information
                -- @+node:gcross.20100706133004.1996:modified products
                ,testCase "modified products" $
                    let job_id = Identifier UUID.nil "job"
                        job_ids = [job_id]
                        product_digest = md5 . L.pack $ "Job results"
                        product_digests = [product_digest]
                        cache, correct_cache :: SerializableRecord
                        cache = withFields (
                            (source_digests_field, [] :: [MD5Digest])
                         :. (implicit_dependencies_field, [] :: [UnresolvedDependency])
                         :. (dependencies_and_digests_field, ([],[]) :: ([UnresolvedDependency],[MD5Digest]))
                         :. (miscellaneous_information_field :: Field (), ())
                         :. (product_digests_field, [] :: [MD5Digest])
                         :. () )
                        correct_cache = withFields (
                            (source_digests_field, [] :: [MD5Digest])
                         :. (implicit_dependencies_field, [] :: [UnresolvedDependency])
                         :. (dependencies_and_digests_field, ([],[]) :: ([UnresolvedDependency],[MD5Digest]))
                         :. (miscellaneous_information_field :: Field (), ())
                         :. (product_digests_field, product_digests)
                         :. () )
                    in withJobServer 1 (Map.singleton job_ids (encode cache)) $ \job_server → do
                        scanner_ignored_ref ← newIORef True
                        builder_called_ref ← newIORef False
                        digester_called_ref ← newIORef False
                        submitJob job_server job_ids . runJobAnalyzer $
                            analyzeImplicitDependenciesAndRebuildIfNecessary
                                (liftIO (writeIORef scanner_ignored_ref False) >> return [])
                                (liftIO (writeIORef builder_called_ref True) >> return product_digests)
                                (const $ liftIO (writeIORef digester_called_ref True) >> return False)
                                undefined
                                ()
                                []
                                []
                        result ← requestJobResult job_server job_id
                        new_cache ← fmap (fromJust . Map.lookup job_ids) . requestJobCache $ job_server
                        readIORef scanner_ignored_ref >>= assertBool "Was the scanner ignored?"
                        readIORef builder_called_ref >>= assertBool "Was the builder called?"
                        readIORef digester_called_ref >>= assertBool "Was the digester called?"
                        assertEqual
                            "Is the digest correct?"
                            (Just product_digest)
                            (getField _digest result)
                        assertEqual
                            "Are the deferred dependencies correct?"
                            (Just [])
                            (getField _deferred_dependencies result)
                        assertEqual
                            "Is the cache correct?"
                            (encode correct_cache)
                            new_cache
                -- @-node:gcross.20100706133004.1996:modified products
                -- @-others
                ]
        -- @-node:gcross.20100706002630.1997:analyzeDependenciesAndRebuildIfNecessary
        -- @-others
        ]
    -- @-node:gcross.20100706002630.1965:Blueprint.Tools.JobAnalyzer
    -- @-others
    -- @-node:gcross.20100602152546.1870:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100602152546.1280:@thin test.hs
-- @-leo
