-- @+leo-ver=4-thin
-- @+node:gcross.20100604184944.1289:@thin Jobs.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100604184944.1291:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RelaxedPolyRec #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- @-node:gcross.20100604184944.1291:<< Language extensions >>
-- @nl

module Blueprint.Jobs where

-- @<< Import needed modules >>
-- @+node:gcross.20100604184944.1292:<< Import needed modules >>
import Prelude hiding (catch)

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import qualified Control.Monad.CatchIO as M
import Control.Monad.IO.Class
import Control.Monad.Trans.State (StateT,evalStateT)
import Control.Parallel.Strategies

import Data.Accessor.Monad.Trans.State
import Data.Accessor.Template
import Data.Binary (Binary,encode,decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Either
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable

import Debug.Trace
import System.IO

import System.Mem.Weak

import Blueprint.IOTask
-- @-node:gcross.20100604184944.1292:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100604204549.1369:Exceptions
-- @+node:gcross.20100604204549.1370:ConflictingJobException
data ConflictingJobException = ConflictingJobException [String] [String]
  deriving (Typeable)

instance Show ConflictingJobException where
    show (ConflictingJobException names conflicting_names) =
        "Attempted to submit job under names " ++ show names ++ " which conflicts with existing jobs that already have names " ++ show conflicting_names ++ "."

instance Exception ConflictingJobException
-- @-node:gcross.20100604204549.1370:ConflictingJobException
-- @+node:gcross.20100604204549.1374:NoSuchJobsException
data NoSuchJobsException = NoSuchJobsException [String]
  deriving (Typeable, Eq)

instance Show NoSuchJobsException where
    show (NoSuchJobsException names) =
        "Requsted result for jobs " ++ show names ++ " which do no exist."

instance Exception NoSuchJobsException
-- @-node:gcross.20100604204549.1374:NoSuchJobsException
-- @+node:gcross.20100604204549.1377:CombinedException
data CombinedException = CombinedException [([String],SomeException)] deriving Typeable

instance Show CombinedException where
    show (CombinedException exceptions) =
        intercalate (replicate 72 '=' ++ "\n")
        .
        map (\(names,exception) →
            "Error executing job " ++ show names ++ ":\n" ++ show exception
        )
        $
        exceptions

instance Exception CombinedException
-- @nonl
-- @-node:gcross.20100604204549.1377:CombinedException
-- @+node:gcross.20100604204549.7672:CyclicDependencyException
data CyclicDependencyException = CyclicDependencyException deriving Typeable

instance Show CyclicDependencyException where
    show CyclicDependencyException = "Job has cyclic dependency."

instance Exception CyclicDependencyException
-- @-node:gcross.20100604204549.7672:CyclicDependencyException
-- @-node:gcross.20100604204549.1369:Exceptions
-- @+node:gcross.20100604184944.1293:Types
-- @+node:gcross.20100604204549.1368:Job
data Job result = 
    ∀ cache. Binary cache => JobSubmission [String] (Maybe cache → JobTask result (result,cache)) ThreadId
  | JobFailure Int (IntMap SomeException)
  | ExternalRequest String (MVar (Either SomeException result))
  | ∀ cache. Binary cache => JobTask Int (JobTask result (result,cache))
-- @nonl
-- @-node:gcross.20100604204549.1368:Job
-- @+node:gcross.20100604184944.1294:JobStatus
data JobStatus result =
    ∀ cache. Binary cache => Pending (Maybe cache → JobTask result (result,cache))
  | Running [Int] [MVar (Either SomeException result)]
  | Succeeded result
  | Failed (IntMap SomeException) CombinedException
-- @nonl
-- @-node:gcross.20100604184944.1294:JobStatus
-- @+node:gcross.20100604184944.1295:JobTask
data JobTask r a =
    Request [String] ([r] → JobTask r a)
  | ∀ b. PerformIO (IO b) (b → JobTask r a)
  | Return a
-- @nonl
-- @-node:gcross.20100604184944.1295:JobTask
-- @+node:gcross.20100604204549.1380:PausedJob
data PausedJob result = ∀ cache. Binary cache => PausedJob
    {   pausedJobPendingRequests :: IntSet
    ,   pausedJobRequests :: [Int]
    ,   pausedJobComputeTask :: [result] → JobTask result (result,cache)
    }
-- @nonl
-- @-node:gcross.20100604204549.1380:PausedJob
-- @+node:gcross.20100604204549.1375:JobServerState
data JobServerState result = JobServerState
    {   serverNextJobId_ :: Int
    ,   serverJobIds_ :: Map String Int
    ,   serverJobNames_ :: IntMap [String]
    ,   serverJobStatuses_ :: IntMap (JobStatus result)
    ,   serverJobQueue_ :: Chan (Job result)
    ,   serverPausedJobs_ :: IntMap (PausedJob result)
    ,   serverIOTaskQueue_ :: Chan (IOTask (Job result))
    ,   serverJobCache_ :: Map [String] ByteString
    }

$( deriveAccessors ''JobServerState )
-- @-node:gcross.20100604204549.1375:JobServerState
-- @+node:gcross.20100607083309.1398:JobServer
data JobServer result = JobServer (Chan (Job result)) [ThreadId]
-- @-node:gcross.20100607083309.1398:JobServer
-- @-node:gcross.20100604184944.1293:Types
-- @+node:gcross.20100604184944.1297:Instances
-- @+node:gcross.20100604184944.1309:Functor JobTask
instance Functor (JobTask r) where
    fmap f m =
        case m of
            Request requests g → Request requests (fmap f . g)
            PerformIO task g → PerformIO task (fmap f . g)
            Return a → Return (f a)
-- @-node:gcross.20100604184944.1309:Functor JobTask
-- @+node:gcross.20100604184944.1298:Monad JobTask
instance Monad (JobTask r) where
    Return a >>= b = b a
    Request requests f >>= b = Request requests (f >=> b)
    PerformIO task f >>= b = PerformIO task (f >=> b)
    return = Return
    fail = error
-- @-node:gcross.20100604184944.1298:Monad JobTask
-- @+node:gcross.20100604184944.1306:MonadIO JobTask
instance MonadIO (JobTask r) where
    liftIO task = PerformIO task return
-- @-node:gcross.20100604184944.1306:MonadIO JobTask
-- @-node:gcross.20100604184944.1297:Instances
-- @+node:gcross.20100604204549.1359:Functions
-- @+node:gcross.20100607083309.1441:Debugging
-- @+node:gcross.20100607083309.1442:traceStatus
traceStatus x = trace (jobStatusAsString x) x
-- @-node:gcross.20100607083309.1442:traceStatus
-- @+node:gcross.20100607083309.1491:jobStatusAsString
jobStatusAsString (Pending _) = "Pending"
jobStatusAsString (Running _ _) = "Running"
jobStatusAsString (Succeeded _) = "Succeeded"
jobStatusAsString (Failed _ _) = "Failed"
-- @-node:gcross.20100607083309.1491:jobStatusAsString
-- @+node:gcross.20100607083309.1445:putStrLnStdErr
putStrLnStdErr message = liftIO $ do
    thread_id ← myThreadId
    L.hPut stderr . L.pack $ "[" ++ show thread_id ++ "] " ++ message ++ "\n"
-- @-node:gcross.20100607083309.1445:putStrLnStdErr
-- @-node:gcross.20100607083309.1441:Debugging
-- @+node:gcross.20100607083309.1404:Interface
-- @+node:gcross.20100607083309.1397:startJobServer
startJobServer ::
    NFData result =>
    Int →
    Map [String] ByteString →
    IO (JobServer result)
startJobServer number_of_io_slaves starting_cache = do
    job_queue ← newChan
    io_task_queue ← newChan
    io_slave_thread_ids ← replicateM number_of_io_slaves $ spawnIOTaskRunner io_task_queue job_queue
    job_server_thread_id ←
        forkIO . liftIO . evalStateT runJobServer $
            JobServerState
            {   serverNextJobId_ = 0
            ,   serverJobIds_ = Map.empty
            ,   serverJobNames_ = IntMap.empty
            ,   serverJobStatuses_ = IntMap.empty
            ,   serverJobQueue_ = job_queue
            ,   serverPausedJobs_ = IntMap.empty
            ,   serverIOTaskQueue_ = io_task_queue
            ,   serverJobCache_ = starting_cache
            }
    return $ JobServer job_queue (job_server_thread_id:io_slave_thread_ids)
-- @-node:gcross.20100607083309.1397:startJobServer
-- @+node:gcross.20100607083309.1469:killJobServer
killJobServer :: JobServer result → IO ()
killJobServer (JobServer _ threads) = mapM_ killThread threads
-- @-node:gcross.20100607083309.1469:killJobServer
-- @+node:gcross.20100607083309.1470:withJobServer
withJobServer ::
    NFData result =>
    Int →
    Map [String] ByteString →
    (JobServer result → IO a) →
    IO a
withJobServer number_of_io_slaves starting_cache thunk =
    bracket
        (startJobServer number_of_io_slaves starting_cache)
        killJobServer
        thunk
-- @-node:gcross.20100607083309.1470:withJobServer
-- @+node:gcross.20100607083309.1406:submitJob
submitJob ::
    Binary cache =>
    JobServer result →
    [String] →
    (Maybe cache → JobTask result (result,cache)) →
    IO ()
submitJob (JobServer job_queue _) names job =
    myThreadId >>= writeChan job_queue . JobSubmission names job
-- @-node:gcross.20100607083309.1406:submitJob
-- @+node:gcross.20100607083309.1409:requestJobResult
requestJobResult :: JobServer result → String → IO result
requestJobResult (JobServer job_queue _) requested_job_name = do
    result_var ← newEmptyMVar
    writeChan job_queue $ ExternalRequest requested_job_name result_var
    result ← takeMVar result_var
    case result of
        Left exception → throwIO exception
        Right value → return value
-- @-node:gcross.20100607083309.1409:requestJobResult
-- @+node:gcross.20100607083309.1414:returnWithoutCache
returnWithoutCache value = return (value,())
-- @-node:gcross.20100607083309.1414:returnWithoutCache
-- @+node:gcross.20100607083309.1417:request
request :: [String] → JobTask result [result]
request = flip Request return
-- @-node:gcross.20100607083309.1417:request
-- @-node:gcross.20100607083309.1404:Interface
-- @+node:gcross.20100607083309.1405:Implementation
-- @+node:gcross.20100604204549.7666:failJobWithExceptions
failJobWithExceptions :: Int → IntMap SomeException → StateT (JobServerState result) IO ()
failJobWithExceptions job_id exceptions = do
    combined_exception ← combineExceptions exceptions
    Just (Running requesting_job_ids listeners) ←
        fmap (IntMap.lookup job_id)
        .
        getAndModify serverJobStatuses
        $
        IntMap.insert job_id (Failed exceptions combined_exception)
    let r = Left . toException $ combined_exception
    liftIO . mapM (flip putMVar r) $ listeners
    notifyPausedJobsThatRequestedJobIsFinished job_id requesting_job_ids
    Just names ← fmap (IntMap.lookup job_id) (get serverJobNames)
    serverJobCache %:
        (Map.delete names)
-- @-node:gcross.20100604204549.7666:failJobWithExceptions
-- @+node:gcross.20100604204549.1388:fetchResultsAndRunJobTask
fetchResultsAndRunJobTask ::
    Binary cache =>
    Int →
    [Int] →
    ([result] → JobTask result (result,cache)) →
    StateT (JobServerState result) IO ()
fetchResultsAndRunJobTask job_id requested_job_ids computeTask = do
    server_job_statuses ← get serverJobStatuses
    let (exceptions,results) =
            first IntMap.unions
            .
            partitionEithers
            .
            map (
                \job_id →
                    case fromJust (IntMap.lookup job_id server_job_statuses) of
                        Failed exceptions _ → Left exceptions
                        Succeeded result → Right result
            )
            $
            requested_job_ids
    if (not . IntMap.null) exceptions
        then failJobWithExceptions job_id exceptions
        else do
            server_job_queue ← get serverJobQueue
            (liftIO . writeChan server_job_queue . JobTask job_id . computeTask) $|| rwhnf $ results
-- @nonl
-- @-node:gcross.20100604204549.1388:fetchResultsAndRunJobTask
-- @+node:gcross.20100604204549.1387:combineExceptions
combineExceptions :: IntMap SomeException → StateT (JobServerState result) IO CombinedException
combineExceptions exceptions =
    get serverJobNames
    >>=
    \server_job_names →
        return
        .
        CombinedException
        .
        map (first $ fromJust . flip IntMap.lookup server_job_names)
        .
        IntMap.assocs
        $
        exceptions
-- @-node:gcross.20100604204549.1387:combineExceptions
-- @+node:gcross.20100604204549.7678:notifyPausedJobsThatRequestedJobIsFinished
notifyPausedJobsThatRequestedJobIsFinished :: Int → [Int] → StateT (JobServerState result) IO ()
notifyPausedJobsThatRequestedJobIsFinished job_id =
    mapM_ $ \paused_job_id → do
        Just (paused_job@PausedJob{..}) ← fmap (IntMap.lookup paused_job_id) (get serverPausedJobs)
        if IntSet.size pausedJobPendingRequests == 1
            then do
                serverPausedJobs %: IntMap.delete paused_job_id
                fetchResultsAndRunJobTask paused_job_id pausedJobRequests pausedJobComputeTask
            else
                serverPausedJobs %:
                    (IntMap.insert paused_job_id
                     $
                     paused_job { pausedJobPendingRequests = IntSet.delete job_id pausedJobPendingRequests }
                    )
-- @nonl
-- @-node:gcross.20100604204549.7678:notifyPausedJobsThatRequestedJobIsFinished
-- @+node:gcross.20100604204549.7668:startJobTask
startJobTask ::
    Binary cache =>
    Int →
    (Maybe cache → JobTask result (result,cache)) →
    StateT (JobServerState result) IO ()
startJobTask job_id computeTaskFromCache = do
    server_job_cache ← get serverJobCache
    server_job_names ← get serverJobNames
    server_job_queue ← get serverJobQueue
    ((liftIO
        .
        writeChan server_job_queue
        .
        JobTask job_id
      ) .|| rwhnf )(
        computeTaskFromCache
        .
        fmap decode
        .
        flip Map.lookup server_job_cache
        .
        fromJust
        .
        flip IntMap.lookup server_job_names
      ) $
        job_id
    modify serverJobStatuses
        .
        IntMap.insert job_id
        $
        Running [] []
-- @nonl
-- @-node:gcross.20100604204549.7668:startJobTask
-- @+node:gcross.20100607083309.1418:addJobResultListener
addJobResultListener :: Int → MVar (Either SomeException result) → StateT (JobServerState result) IO ()
addJobResultListener job_id listener =
    serverJobStatuses %:
        (IntMap.adjust (\(Running dependent_job_ids listeners) → Running dependent_job_ids (listener:listeners)) job_id)
-- @-node:gcross.20100607083309.1418:addJobResultListener
-- @+node:gcross.20100604204549.7659:processJob
processJob ::
    NFData result =>
    Job result →
    StateT (JobServerState result) IO ()
-- @+others
-- @+node:gcross.20100604204549.7660:JobSubmission
processJob (JobSubmission names computeTaskFromCache thread_id) = do
    server_job_ids ← get serverJobIds
    case filter (flip Map.member server_job_ids) names of
        [] → do
            job_id ← getAndModify serverNextJobId (+1)
            serverJobIds %:
                \server_job_ids →
                    foldl' -- '
                        (\job_ids name → Map.insert name job_id job_ids) 
                        server_job_ids
                        names
            serverJobNames %:
                IntMap.insert job_id names
            serverJobStatuses %:
                IntMap.insert job_id (Pending computeTaskFromCache)
        conflicting_names →
            liftIO
                .
                throwTo thread_id
                .
                ConflictingJobException names
                $
                conflicting_names
-- @-node:gcross.20100604204549.7660:JobSubmission
-- @+node:gcross.20100604204549.7661:ExternalRequest
processJob (ExternalRequest name destination_var) = do
    maybe_job_id ← fmap (Map.lookup name) (get serverJobIds)
    case maybe_job_id of
        Nothing →
            liftIO
            .
            putMVar destination_var
            .
            Left
            .
            toException
            .
            NoSuchJobsException
            .
            (:[])
            $
            name
        Just job_id → do
            job_status ← fmap (fromJust . IntMap.lookup job_id) (get serverJobStatuses)
            case job_status of
                Pending computeTaskFromCache → do
                    startJobTask job_id computeTaskFromCache
                    addJobResultListener job_id destination_var
                Running _ _ → do
                    addJobResultListener job_id destination_var
                Succeeded result → do
                    liftIO
                    .
                    putMVar destination_var
                    .
                    Right
                    $
                    result
                Failed _ exception → do
                    liftIO
                    .
                    putMVar destination_var
                    .
                    Left
                    .
                    toException
                    $
                    exception
-- @-node:gcross.20100604204549.7661:ExternalRequest
-- @+node:gcross.20100604204549.7665:JobFailure
processJob (JobFailure job_id exceptions) =
    failJobWithExceptions job_id exceptions
-- @-node:gcross.20100604204549.7665:JobFailure
-- @+node:gcross.20100604204549.7662:JobTask
processJob (JobTask job_id task) =
    flip M.catch (failJobWithExceptions job_id . IntMap.singleton job_id)
    $
    case task of
        -- @        @+others
        -- @+node:gcross.20100604204549.7667:Request
        Request requested_job_names computeTask → do
            -- @    << Fetch ids of the requested jobs. >>
            -- @+node:gcross.20100604204549.7673:<< Fetch ids of the requested jobs. >>
            (non_existing_jobs,requested_job_ids) ← get serverJobIds >>= \server_job_ids →
                return
                .
                partitionEithers
                .
                map (
                    \request_name →
                        maybe (Left request_name) Right
                        $
                        Map.lookup request_name server_job_ids
                )
                $
                requested_job_names
            -- @-node:gcross.20100604204549.7673:<< Fetch ids of the requested jobs. >>
            -- @nl
            if (not . null) non_existing_jobs
                then
                    failJobWithExceptions job_id
                    .
                    IntMap.singleton job_id
                    .
                    toException
                    .
                    NoSuchJobsException
                    $
                    non_existing_jobs
                else do
                    -- @            << Look up statuses of the requested job ids. >>
                    -- @+node:gcross.20100604204549.7670:<< Look up statuses of the requested job ids. >>
                    pending_job_ids ← get serverJobStatuses >>= \server_job_statuses →
                        filterM
                            (\requested_job_id → do
                                case fromJust (IntMap.lookup requested_job_id server_job_statuses) of
                                    Pending computeTaskFromCache → do
                                        startJobTask requested_job_id computeTaskFromCache
                                        return True
                                    Running _ _ → return True
                                    Succeeded _ → return False
                                    Failed exceptions _ → return False
                            )
                            requested_job_ids
                    -- @-node:gcross.20100604204549.7670:<< Look up statuses of the requested job ids. >>
                    -- @nl
                    if null pending_job_ids
                        then fetchResultsAndRunJobTask job_id requested_job_ids computeTask
                        else do
                            -- @                    << Compute all dependencies. >>
                            -- @+node:gcross.20100604204549.7669:<< Compute all dependencies. >>
                            all_dependency_ids ← get serverPausedJobs >>= \paused_jobs →
                                let getDependencies accum (IntSet.minView → Nothing) = accum
                                    getDependencies accum (IntSet.minView → Just (dependency_job_id,rest_dependency_job_ids)) =
                                        getDependencies (IntSet.insert dependency_job_id accum)
                                        $
                                        case IntMap.lookup dependency_job_id paused_jobs of
                                            Nothing → rest_dependency_job_ids
                                            Just paused_job →
                                                (IntSet.union rest_dependency_job_ids)
                                                .
                                                (`IntSet.difference` accum)
                                                .
                                                pausedJobPendingRequests
                                                $
                                                paused_job
                                in
                                    return
                                    .
                                    getDependencies IntSet.empty
                                    .
                                    IntSet.fromList
                                    $
                                    pending_job_ids
                            -- @-node:gcross.20100604204549.7669:<< Compute all dependencies. >>
                            -- @nl
                            if IntSet.member job_id all_dependency_ids
                                then
                                    failJobWithExceptions job_id
                                    .
                                    IntMap.singleton job_id
                                    .
                                    toException
                                    $
                                    CyclicDependencyException
                                else do
                                    -- @                            << Update the state of the job server. >>
                                    -- @+node:gcross.20100604204549.7671:<< Update the state of the job server. >>
                                    (serverPausedJobs %:)
                                        .
                                        IntMap.insert job_id
                                        $
                                        PausedJob
                                        {   pausedJobPendingRequests = IntSet.fromList pending_job_ids
                                        ,   pausedJobRequests = requested_job_ids
                                        ,   pausedJobComputeTask = computeTask
                                        }
                                    (serverJobStatuses %:)
                                        $
                                        \statuses →
                                        foldl' -- '
                                            (\statuses requested_job_id →
                                                IntMap.adjust
                                                    (\(Running requesting_job_ids result_ivar) →
                                                       Running (job_id:requesting_job_ids) result_ivar
                                                    )
                                                    requested_job_id
                                                    statuses
                                            )
                                            statuses
                                            pending_job_ids
                                    -- @-node:gcross.20100604204549.7671:<< Update the state of the job server. >>
                                    -- @nl
        -- @-node:gcross.20100604204549.7667:Request
        -- @+node:gcross.20100604204549.1382:Return
        Return (result_,cache_) → do
            result ← liftIO . evaluate . withStrategy rdeepseq $ result_
            Just (Running requesting_job_ids listeners) ←
                fmap (IntMap.lookup job_id)
                .
                getAndModify serverJobStatuses
                .
                IntMap.insert job_id
                .
                Succeeded
                $
                result
            let r = Right result
            liftIO . mapM (flip putMVar r) $ listeners
            notifyPausedJobsThatRequestedJobIsFinished job_id requesting_job_ids

            cache ← liftIO . evaluate . withStrategy rwhnf . encode $ cache_
            Just names ← fmap (IntMap.lookup job_id) (get serverJobNames)
            serverJobCache %:
                (Map.insert names cache)
        -- @-node:gcross.20100604204549.1382:Return
        -- @+node:gcross.20100604204549.7663:PerformIO
        PerformIO action computeTask →
            get serverIOTaskQueue
            >>=
            \io_task_queue →
                liftIO
                .
                writeChan io_task_queue
                .
                IOTask action
                $
                either
                    (JobFailure job_id . IntMap.singleton job_id)
                    ((JobTask job_id . computeTask) $|| rwhnf)
        -- @nonl
        -- @-node:gcross.20100604204549.7663:PerformIO
        -- @-others
-- @-node:gcross.20100604204549.7662:JobTask
-- @-others
-- @nonl
-- @-node:gcross.20100604204549.7659:processJob
-- @+node:gcross.20100607083309.1402:runJobServer
runJobServer :: NFData result => StateT (JobServerState result) IO ()
runJobServer =
    (forever $
        get serverJobQueue >>= liftIO . readChan >>= processJob
    ) `M.catch` (
        \e → unless (e == ThreadKilled) . liftIO . throwIO $ e
    ) `M.catch` (
        \e → putStrLnStdErr $ "JOB SERVER KILLED:  " ++ show (e :: SomeException)
    )
-- @-node:gcross.20100607083309.1402:runJobServer
-- @-node:gcross.20100607083309.1405:Implementation
-- @-node:gcross.20100604204549.1359:Functions
-- @-others
-- @-node:gcross.20100604184944.1289:@thin Jobs.hs
-- @-leo
