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
import Control.Exception hiding (assert, catch, try, throw)
import Control.Monad
import Control.Monad.CatchIO
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

import Test.HUnit hiding (assertEqual)
import qualified Test.HUnit as HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Text.StringTemplate
import Text.StringTemplate.GenericStandard

import System.IO
import System.Random

import Blueprint.Record

import Blueprint.Dependency
import Blueprint.Fields.DeferredDependencies
import Blueprint.Fields.Digest
import Blueprint.Identifier
import Blueprint.Language.Programming.Haskell
import Blueprint.Miscellaneous
import Blueprint.Tools.Compilers
import Blueprint.Tools.Compilers.GHC
import Blueprint.IOTask
import Blueprint.Jobs
import Blueprint.Options
import Blueprint.SourceFile
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
-- @+node:gcross.20100603132252.2058:assertEqual
assertEqual :: (MonadIO m, Show a, Eq a) => String → a → a → m ()
assertEqual message correct_value actual_value =
    liftIO (HUnit.assertEqual message correct_value actual_value)
-- @-node:gcross.20100603132252.2058:assertEqual
-- @+node:gcross.20100709210816.2232:assertThrows
assertThrows :: (MonadCatchIO m, Exception e) => e → m a → m ()
assertThrows exception thunk =
    catch
        (   thunk
            >>
            (liftIO . assertFailure $ "No exception was thrown.")
        )
        (   liftIO
            .
            assertEqual
                "Was the correct exception thrown?"
                (show exception)
            .
            (show :: SomeException → String)
        )
-- @-node:gcross.20100709210816.2232:assertThrows
-- @+node:gcross.20100607205618.1430:assertJobCacheEmpty
assertJobCacheEmpty =
    requestJobCache
    >>=
    liftIO
    .
    assertBool
        "Is the job cache empty?"
    .
    Map.null
-- @-node:gcross.20100607205618.1430:assertJobCacheEmpty
-- @+node:gcross.20100607205618.1434:assertJobCacheEqualTo
assertJobCacheEqualTo correct_cache =
    requestJobCache
    >>=
    liftIO
    .
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

test_dependency, test_dependency_1, test_dependency_2, test_dependency_3, test_dependency_4 :: Dependency
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
test_dependency_3 =
    Dependency
    {   dependencyName = "test 3"
    ,   dependencyType = test_dependency_type
    }
test_dependency_4 = 
    Dependency
    {   dependencyName = "test 4"
    ,   dependencyType = test_dependency_type
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
    -- @+node:gcross.20100609163522.1702:Blueprint.Record
    [testGroup "Blueprint.Record" $
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
    -- @-node:gcross.20100609163522.1702:Blueprint.Record
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
            [testCase "return cached value (Nothing)" . withJobServer 1 Map.empty $ do
                submitJob . jobWithCache ["job"] $ \maybe_cached_value →
                    returnValue maybe_cached_value
                requestJobResult "job"
                    >>=
                    liftIO
                    .
                    assertEqual
                        "Is the job's result correct?"
                        Nothing
                assertJobCacheEmpty
            -- @-node:gcross.20100607083309.1479:return cached value (Nothing)
            -- @+node:gcross.20100607083309.1480:read/write IORef
            ,testCase "read/write MVar" . withJobServer 1 Map.empty $ do
                in_var ← liftIO newEmptyMVar
                out_var ← liftIO newEmptyMVar
                submitJob . job ["job"] $
                    liftIO $ do
                        value ← takeMVar in_var
                        putMVar out_var (value+1)
                        returnValue value
                liftIO (putMVar in_var 1)
                requestJobResult "job" >>= liftIO . (@=? (1 :: Int))
                liftIO (takeMVar out_var >>= (@=? (2 :: Int)))
                assertJobCacheEmpty 
            -- @-node:gcross.20100607083309.1480:read/write IORef
            -- @+node:gcross.20100607083309.1481:bad requests
            ,testGroup "bad requests" $
                -- @    @+others
                -- @+node:gcross.20100607083309.1482:self
                [testCase "self" . withJobServer 1 Map.empty $ do
                    submitJob . job ["job"] $ do
                        request ["non-existent job"]
                        returnValue ()
                    assertThrows
                        (CombinedException [(["job"],toException . NoSuchJobsException $ ["non-existent job"])])
                        (requestJobResult "job")
                    assertJobCacheEmpty 
                -- @nonl
                -- @-node:gcross.20100607083309.1482:self
                -- @+node:gcross.20100607083309.1483:cyclic dependency
                ,testCase "cyclic dependency" . withJobServer 1 Map.empty $ do
                    submitJob . job ["job"] $ do
                        request ["job"]
                        returnValue ()
                    assertThrows
                        (CombinedException [(["job"],toException CyclicDependencyException)])
                        (requestJobResult "job")
                    assertJobCacheEmpty 
                -- @nonl
                -- @-node:gcross.20100607083309.1483:cyclic dependency
                -- @-others
                ]
            -- @-node:gcross.20100607083309.1481:bad requests
            -- @+node:gcross.20100607083309.1484:IO thrown exception
            ,testCase "IO thrown exception" . withJobServer 1 Map.empty $ do
                submitJob . job ["job"] $
                    liftIO $ do
                        throwIO TestException
                        returnValue ()
                assertThrows
                    (CombinedException [(["job"],toException TestException)])
                    (requestJobResult "job")
                assertJobCacheEmpty 
            -- @nonl
            -- @-node:gcross.20100607083309.1484:IO thrown exception
            -- @+node:gcross.20100607205618.1445:multiple results
            ,testCase "multiple results" . withJobServer 0 Map.empty $ do
                submitJob . job ["job1","job2"] $
                    returnValues [1,2]
                requestJobResult "job1"
                    >>=
                    liftIO
                    .
                    assertEqual
                        "Is the job's first result correct?"
                        (1 :: Int)
                requestJobResult "job2"
                    >>=
                    liftIO
                    .
                    assertEqual
                        "Is the job's second result correct?"
                        (2 :: Int)
                assertJobCacheEmpty 
            -- @nonl
            -- @-node:gcross.20100607205618.1445:multiple results
            -- @+node:gcross.20100607205618.1447:too many results
            ,testCase "too many results" . withJobServer 0 Map.empty $ do
                submitJob . job ["job"] $
                    returnValues [1,2::Int]
                assertThrows
                    (CombinedException [(["job"],toException $ ReturnedWrongNumberOfResults 2 1)])
                    (requestJobResult "job")
                assertJobCacheEmpty 
            -- @nonl
            -- @-node:gcross.20100607205618.1447:too many results
            -- @+node:gcross.20100607205618.1449:too few results
            ,testCase "too few results" . withJobServer 0 Map.empty $ do
                submitJob . job ["job"] $
                    returnValues ([] :: [()])
                assertThrows
                    (CombinedException [(["job"],toException $ ReturnedWrongNumberOfResults 0 1)])
                    (requestJobResult "job")
                assertJobCacheEmpty 
            -- @nonl
            -- @-node:gcross.20100607205618.1449:too few results
            -- @-others
            ]
        -- @-node:gcross.20100607083309.1478:single job
        -- @+node:gcross.20100607083309.1488:two jobs
        ,testGroup "two jobs" $
            -- @    @+others
            -- @+node:gcross.20100607083309.1489:simple request
            [testCase "simple request" . withJobServer 1 Map.empty $  do
                submitJob . job ["job1"] $
                    returnValue 1
                submitJob . job ["job2"] $
                    request ["job1"] >>= returnValue . head
                requestJobResult  "job2"
                    >>=
                    liftIO
                    .
                    assertEqual
                        "Is the job's result correct?"
                        (1 :: Int)
                assertJobCacheEmpty 
            -- @nonl
            -- @-node:gcross.20100607083309.1489:simple request
            -- @+node:gcross.20100607205618.1451:simple request, multiple results
            ,testCase "simple request, multiple results" . withJobServer 1 Map.empty $  do
                submitJob . job ["job1A","job1B"] $
                    returnValues [1,2]
                submitJob . job ["job2"] $
                    request ["job1B"] >>= returnValue . head
                requestJobResult  "job2"
                    >>=
                    liftIO
                    .
                    assertEqual
                        "Is the job's result correct?"
                        (2 :: Int)
                assertJobCacheEmpty 
            -- @nonl
            -- @-node:gcross.20100607205618.1451:simple request, multiple results
            -- @+node:gcross.20100607083309.1490:cyclic request
            ,testCase "cyclic request" . withJobServer 1 Map.empty $  do
                dummy ← liftIO newEmptyMVar
                submitJob . job ["job1"] $ do
                    liftIO $ takeMVar dummy
                    request ["job2"] >>= returnValue . head
                submitJob . job ["job2"] $ do
                    request ["job1"] >>= returnValue . head
                liftIO $ putMVar dummy ()
                result ← try (requestJobResult  "job2")
                liftIO $ case result of
                    Right () → assertFailure "Failed to detect cyclic dependency."
                    Left e → assertBool "Was the correct exception thrown?" . (show CyclicDependencyException `isInfixOf`) . show $ (e :: SomeException)
                assertJobCacheEmpty
            -- @nonl
            -- @-node:gcross.20100607083309.1490:cyclic request
            -- @+node:gcross.20100607083309.1448:exception
            ,testCase "exception" . withJobServer 1 Map.empty $  do
                submitJob . job ["job1"] $ do
                    throw TestException
                    returnValue ()
                submitJob . job ["job2"] $
                    request ["job1"] >>= returnValue . head
                assertThrows
                    (CombinedException [(["job1"],toException TestException)])
                    (requestJobResult  "job2")
                assertJobCacheEmpty 
            -- @nonl
            -- @-node:gcross.20100607083309.1448:exception
            -- @+node:gcross.20100607205618.1422:IO tasks run in parallel
            ,testCase "IO tasks run in parallel" . withJobServer 2 Map.empty $  do
                chan1 ← liftIO newChan
                chan2 ← liftIO newChan
                submitJob . job ["job1"] $
                    liftIO $ (writeChan chan2 2 >> readChan chan1)
                    >>=
                    returnValue
                submitJob . job ["job2"] $ 
                    liftIO $ (writeChan chan1 1 >> readChan chan2)
                    >>=
                    returnValue
                submitJob . job ["job3"] $ 
                    request ["job1","job2"]
                    >>=
                    returnValue . sum
                requestJobResult  "job3"
                    >>=
                    liftIO
                    .
                    assertEqual
                        "Is job 1's result correct?"
                        (3 :: Int)
                requestJobResult  "job2"
                    >>=
                    liftIO
                    .
                    assertEqual
                        "Is job 1's result correct?"
                        (2 :: Int)
                requestJobResult  "job1"
                    >>=
                    liftIO
                    .
                    assertEqual
                        "Is job 2's result correct?"
                        (1 :: Int)
                assertJobCacheEmpty 
            -- @nonl
            -- @-node:gcross.20100607205618.1422:IO tasks run in parallel
            -- @+node:gcross.20100607205618.1424:IO tasks limited to slaves
            ,testCase "IO tasks limited to slaves" . withJobServer 1 Map.empty $  do
                chan1 ← liftIO newChan
                chan2 ← liftIO newChan
                submitJob . job ["job1"] $
                    liftIO $ (writeChan chan2 (2 :: Int) >> readChan chan1)
                    >>=
                    returnValue
                submitJob . job ["job2"] $ 
                    liftIO $ (writeChan chan1 1 >> readChan chan2)
                    >>=
                    returnValue
                submitJob . job ["job3"] $ 
                    request ["job1","job2"]
                    >>=
                    returnValue . sum
                liftIO $ hPutStrLn stderr "(Note that if this test succeeds, then you *should* see a JOB SERVER KILLED message soon.)"
                assertThrows
                    BlockedIndefinitelyOnMVar
                    (requestJobResult  "job3")
                assertThrows
                    BlockedIndefinitelyOnMVar
                    (requestJobResult  "job2")
                assertThrows
                    BlockedIndefinitelyOnMVar
                    (requestJobResult  "job1")
            -- @nonl
            -- @-node:gcross.20100607205618.1424:IO tasks limited to slaves
            -- @-others
            ]
        -- @-node:gcross.20100607083309.1488:two jobs
        -- @+node:gcross.20100607205618.1416:several jobs
        ,testGroup "several jobs" $
            -- @    @+others
            -- @+node:gcross.20100607205618.1418:diamond dependency
            [testCase "diamond dependency" . withJobServer 1 Map.empty $  do
                submitJob . job ["job1"] $
                    returnValue 1
                submitJob . job ["job2"] $
                    request ["job1"] >>= returnValue . head
                submitJob . job ["job3"] $
                    request ["job1"] >>= returnValue . head
                submitJob . job ["job4"] $
                    request ["job2","job3"] >>= returnValue . sum
                requestJobResult  "job4"
                    >>=
                    liftIO
                    .
                    assertEqual
                        "Is the job's result correct?"
                        (2 :: Int)
                assertJobCacheEmpty 
            -- @nonl
            -- @-node:gcross.20100607205618.1418:diamond dependency
            -- @+node:gcross.20100607205618.1420:cyclic diamond dependency
            ,testCase "cyclic diamond dependency" . withJobServer 1 Map.empty $  do
                dummy ← liftIO newEmptyMVar
                submitJob . job ["job1"] $ do
                    liftIO . takeMVar $ dummy
                    request ["job4"]
                    returnValue (1 :: Int)
                submitJob . job ["job2"] $
                    request ["job1"] >>= returnValue . head
                submitJob . job ["job3"] $
                    request ["job1"] >>= returnValue . head
                submitJob . job ["job4"] $
                    request ["job2","job3"] >>= returnValue . sum
                liftIO $ putMVar dummy ()
                result ← try (requestJobResult  "job4")
                liftIO $ case result of
                    Right _ → assertFailure "Failed to detect cyclic dependency."
                    Left e → assertBool "Was the correct exception thrown?" . (show CyclicDependencyException `isInfixOf`) . show $ (e :: SomeException)
                assertJobCacheEmpty 
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
              $  do
                submitJob . jobWithCache ["job"] $ \maybe_cached_value →
                    returnValueAndCache maybe_cached_value 2
                requestJobResult  "job"
                    >>=
                    liftIO
                    .
                    assertEqual
                        "Is the job's result correct?"
                        (Just (1 :: Int))
                assertJobCacheEqualTo . Map.fromList $
                    [(["job"],encode $ (2 :: Int))
                    ]
            -- @nonl
            -- @-node:gcross.20100607205618.1432:return cached value
            -- @+node:gcross.20100607205618.1440:add cached value
            ,testCase "add cached value" . withJobServer 1 Map.empty $  do
                submitJob . jobWithCache ["job"] $ \maybe_cached_value →
                    returnValueAndCache maybe_cached_value (2 :: Int)
                requestJobResult  "job"
                    >>=
                    liftIO
                    .
                    assertEqual
                        "Is the job's result correct?"
                        Nothing
                assertJobCacheEqualTo  . Map.fromList $
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
              $  do
                submitJob . jobWithCache ["job"] $ \maybe_cached_value → do
                    throw TestException
                    returnValueAndCache () (2 :: Int)
                assertThrows
                    (CombinedException [(["job"],toException TestException)])
                    (requestJobResult  "job")
                assertJobCacheEmpty 
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
                ["A.B.C"
                ,"X.Y"
                ,"P.Q"
                ,"U"
                ]
            .
            extractImportedModulesFromHaskellSource
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
            in withJobServer 1 Map.empty $ do
                submitJob . jobWithCache job_ids . runJobAnalyzer $ do
                    writeToCache _a 42
                    return [undefined]
                _ ← requestJobResult job_id
                new_cache ← fmap (fromJust . Map.lookup job_ids) requestJobCache
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
            in withJobServer 1 (Map.singleton job_ids (encode cache)) $ do
                submitJob . jobWithCache job_ids . runJobAnalyzer $ do
                    maybe_a ← readAndCache _a
                    case maybe_a of
                        Nothing → return [emptyTable]
                        Just a → return [withField _a a]
                result ← requestJobResult job_id
                new_cache ← fmap (fromJust . Map.lookup job_ids) requestJobCache
                assertEqual
                    "Is the result correct?"
                    (Just 42)
                    (getField _a result)
                assertEqual
                    "Is the cache correct?"
                    (encode cache)
                    new_cache
        -- @nonl
        -- @-node:gcross.20100706002630.1970:readAndCache
        -- @+node:gcross.20100706002630.1972:readRequiredAndCache
        ,testCase "readRequiredAndCache" $
            let job_id = Identifier UUID.nil "job"
                job_ids = [job_id]
                cache :: SerializableRecord
                cache = withField _a 42
            in withJobServer 1 (Map.singleton job_ids (encode cache)) $ do
                submitJob . jobWithCache job_ids . runJobAnalyzer $ do
                    a ← readRequiredAndCache _a
                    return [withField _a a]
                result ← requestJobResult job_id
                new_cache ← fmap (fromJust . Map.lookup job_ids) requestJobCache
                assertEqual
                    "Is the result correct?"
                    (Just 42)
                    (getField _a result)
                assertEqual
                    "Is the cache correct?"
                    (encode cache)
                    new_cache
        -- @nonl
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
                in withJobServer 1 Map.empty $ do
                    submitJob . jobWithCache job_ids . runJobAnalyzer $ do
                        c ← checkForChangesIn _a 42
                        return [withField _c c]
                    result ← requestJobResult job_id
                    new_cache ← fmap (fromJust . Map.lookup job_ids) requestJobCache
                    assertEqual
                        "Is the result correct?"
                        (Just True)
                        (getField _c result)
                    assertEqual
                        "Is the cache correct?"
                        (encode correct_cache)
                        new_cache
            -- @nonl
            -- @-node:gcross.20100706002630.1974:no cache
            -- @+node:gcross.20100706002630.1977:changed
            ,testCase "changed" $
                let job_id = Identifier UUID.nil "job"
                    job_ids = [job_id]
                    cache :: SerializableRecord
                    cache = withField _a 24
                    correct_cache :: SerializableRecord
                    correct_cache = withField _a 42
                in withJobServer 1 (Map.singleton job_ids (encode cache)) $ do
                    submitJob . jobWithCache job_ids . runJobAnalyzer $ do
                        c ← checkForChangesIn _a 42
                        return [withField _c c]
                    result ← requestJobResult job_id
                    new_cache ← fmap (fromJust . Map.lookup job_ids) requestJobCache
                    assertEqual
                        "Is the result correct?"
                        (Just True)
                        (getField _c result)
                    assertEqual
                        "Is the cache correct?"
                        (encode correct_cache)
                        new_cache
            -- @nonl
            -- @-node:gcross.20100706002630.1977:changed
            -- @+node:gcross.20100706002630.1979:unchanged
            ,testCase "unchanged" $
                let job_id = Identifier UUID.nil "job"
                    job_ids = [job_id]
                    cache :: SerializableRecord
                    cache = withField _a 42
                    correct_cache :: SerializableRecord
                    correct_cache = withField _a 42
                in withJobServer 1 (Map.singleton job_ids (encode cache)) $ do
                    submitJob . jobWithCache job_ids . runJobAnalyzer $ do
                        c ← checkForChangesIn _a 42
                        return [withField _c c]
                    result ← requestJobResult job_id
                    new_cache ← fmap (fromJust . Map.lookup job_ids) requestJobCache
                    assertEqual
                        "Is the result correct?"
                        (Just False)
                        (getField _c result)
                    assertEqual
                        "Is the cache correct?"
                        (encode correct_cache)
                        new_cache
            -- @nonl
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
            in withJobServer 1 Map.empty $ do
                submitJob . jobWithCache job_ids . runJobAnalyzer $ do
                    a ← runTaskAndCacheResult _a (return 42)
                    return [withField _a a]
                result ← requestJobResult job_id
                new_cache ← fmap (fromJust . Map.lookup job_ids) requestJobCache
                assertEqual
                    "Is the result correct?"
                    (Just 42)
                    (getField _a result)
                assertEqual
                    "Is the cache correct?"
                    (encode correct_cache)
                    new_cache
        -- @nonl
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
                in withJobServer 1 (Map.singleton job_ids (encode cache)) $ do
                    submitJob . jobWithCache job_ids . runJobAnalyzer $ do
                        a ← rerunTaskAndCacheResultOnlyIf _a (return 42) True
                        return [withField _a a]
                    result ← requestJobResult job_id
                    new_cache ← fmap (fromJust . Map.lookup job_ids) requestJobCache
                    assertEqual
                        "Is the result correct?"
                        (Just 42)
                        (getField _a result)
                    assertEqual
                        "Is the cache correct?"
                        (encode correct_cache)
                        new_cache
            -- @nonl
            -- @-node:gcross.20100706002630.1984:True
            -- @+node:gcross.20100706002630.1986:False
            ,testCase "False" $
                let job_id = Identifier UUID.nil "job"
                    job_ids = [job_id]
                    cache :: SerializableRecord
                    cache = withField _a 24
                    correct_cache :: SerializableRecord
                    correct_cache = withField _a 24
                in withJobServer 1 (Map.singleton job_ids (encode cache)) $ do
                    submitJob . jobWithCache job_ids . runJobAnalyzer $ do
                        a ← rerunTaskAndCacheResultOnlyIf _a (return 42) False
                        return [withField _a a]
                    result ← requestJobResult job_id
                    new_cache ← fmap (fromJust . Map.lookup job_ids) requestJobCache
                    assertEqual
                        "Is the result correct?"
                        (Just 24)
                        (getField _a result)
                    assertEqual
                        "Is the cache correct?"
                        (encode correct_cache)
                        new_cache
            -- @nonl
            -- @-node:gcross.20100706002630.1986:False
            -- @-others
            ]
        -- @-node:gcross.20100706002630.1982:rerunTaskAndCacheResultOnlyIf
        -- @-others
        ]
    -- @-node:gcross.20100706002630.1965:Blueprint.Tools.JobAnalyzer
    -- @-others
    -- @-node:gcross.20100602152546.1870:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100602152546.1280:@thin test.hs
-- @-leo
