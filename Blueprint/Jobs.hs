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
import Data.Either
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IVar.Simple (IVar)
import qualified Data.IVar.Simple as IVar
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable

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
-- @+node:gcross.20100604204549.1374:NoSuchJobException
data NoSuchJobsException = NoSuchJobsException [String]
  deriving (Typeable)

instance Show NoSuchJobsException where
    show (NoSuchJobsException names) =
        "Requsted result for jobs '" ++ show names ++ "' which do no exist."

instance Exception NoSuchJobsException
-- @-node:gcross.20100604204549.1374:NoSuchJobException
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
-- @+node:gcross.20100604204549.7672:JobHasCyclicDependency
data JobHasCyclicDependency = JobHasCyclicDependency deriving Typeable

instance Show JobHasCyclicDependency where
    show JobHasCyclicDependency = "Job has cyclic dependency."

instance Exception JobHasCyclicDependency
-- @-node:gcross.20100604204549.7672:JobHasCyclicDependency
-- @-node:gcross.20100604204549.1369:Exceptions
-- @+node:gcross.20100604184944.1293:Types
-- @+node:gcross.20100604204549.1368:Job
data Job result = 
    forall cache. Binary cache => JobSubmission [String] (Maybe cache → JobTask result (result,cache)) ThreadId
  | JobFailure Int (IntMap SomeException)
  | ExternalRequest String (IVar result)
  | forall cache. Binary cache => JobTask Int (JobTask result (result,cache))
-- @-node:gcross.20100604204549.1368:Job
-- @+node:gcross.20100604184944.1294:JobStatus
data JobStatus result =
    forall cache. Binary cache => Pending (Maybe cache → JobTask result (result,cache))
  | Running IntSet [Int] (IVar result)
  | Succeeded result
  | Failed (IntMap SomeException) CombinedException
-- @-node:gcross.20100604184944.1294:JobStatus
-- @+node:gcross.20100604184944.1295:JobTask
data JobTask r a =
    Request [String] ([r] → JobTask r a)
  | forall b. PerformIO (IO b) (b → JobTask r a)
  | Return a
-- @-node:gcross.20100604184944.1295:JobTask
-- @+node:gcross.20100604204549.1380:PausedJob
data PausedJob result = forall cache. Binary cache => PausedJob
    {   pausedJobPendingRequests :: IntSet
    ,   pausedJobRequests :: [Int]
    ,   pausedJobComputeTask :: [result] → JobTask result (result,cache)
    }
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
data JobServer result = JobServer (Chan (Job result))
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
-- @+node:gcross.20100604204549.7666:failJobWithExceptions
failJobWithExceptions :: Int → IntMap SomeException → StateT (JobServerState result) IO ()
failJobWithExceptions job_id exceptions = do
    combined_exception ← combineExceptions exceptions
    Just (Running _ requesting_job_ids result_ivar) ←
        fmap (IntMap.lookup job_id)
        .
        getAndModify serverJobStatuses
        $
        IntMap.insert job_id (Failed exceptions combined_exception)
    liftIO . IVar.write result_ivar . throw $ combined_exception
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
    (Maybe cache -> JobTask result (result,cache)) →
    StateT (JobServerState result) IO (IVar result)
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
    result_ivar ← liftIO IVar.new
    modify serverJobStatuses
        .
        IntMap.insert job_id
        $
        Running IntSet.empty [] result_ivar
    return result_ivar
-- @-node:gcross.20100604204549.7668:startJobTask
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
processJob (ExternalRequest name destination_ivar) = do
    maybe_job_id ← fmap (Map.lookup name) (get serverJobIds)
    case maybe_job_id of
        Nothing →
            liftIO
            .
            IVar.write destination_ivar
            .
            throw
            .
            NoSuchJobsException
            .
            (:[])
            $
            name
        Just job_id → do
            job_status ← fmap (fromJust . IntMap.lookup job_id) (get serverJobStatuses)
            case job_status of
                Pending computeTaskFromCache →
                    startJobTask job_id computeTaskFromCache
                    >>=
                    liftIO
                        .
                        IVar.write destination_ivar
                        .
                        IVar.read
                Running _ _ result_ivar →
                    liftIO
                    .
                    IVar.write destination_ivar
                    .
                    IVar.read
                    $
                    result_ivar
                Succeeded result → do
                    liftIO
                    .
                    IVar.write destination_ivar
                    $
                    result
                Failed _ exception → do
                    liftIO
                    .
                    IVar.write destination_ivar
                    .
                    throw
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
            server_job_ids ← get serverJobIds
            let (non_existing_jobs,requested_job_ids) =
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
            -- @nonl
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
                    server_job_statuses ← get serverJobStatuses
                    pending_job_ids ←
                        filterM
                            (\requested_job_id →
                                case fromJust (IntMap.lookup requested_job_id server_job_statuses) of
                                    Pending computeTaskFromCache → do
                                        startJobTask requested_job_id computeTaskFromCache
                                        return True
                                    Running _ _ _ → return True
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
                            paused_jobs ← get serverPausedJobs
                            let getDependencies accum (IntSet.minView → Nothing) = accum
                                getDependencies accum (IntSet.minView → Just (dependency_job_id,rest_dependency_job_ids)) =
                                    getDependencies (IntSet.insert dependency_job_id accum)
                                    .
                                    (IntSet.union rest_dependency_job_ids)
                                    .
                                    (`IntSet.difference` accum)
                                    .
                                    pausedJobPendingRequests
                                    .
                                    fromJust
                                    .
                                    flip IntMap.lookup paused_jobs
                                    $
                                    dependency_job_id
                                all_dependency_ids =
                                    getDependencies IntSet.empty
                                    .
                                    IntSet.fromList
                                    $
                                    pending_job_ids
                            -- @nonl
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
                                    JobHasCyclicDependency
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
                                                    (\(Running dependent_job_ids requesting_job_ids result_ivar) →
                                                        Running
                                                            dependent_job_ids
                                                            (job_id:requesting_job_ids)
                                                            result_ivar
                                                    )
                                                    requested_job_id
                                                    statuses
                                            )
                                            statuses
                                            pending_job_ids
                                    (serverJobStatuses %:)
                                        $
                                        \statuses →
                                            foldl' -- '
                                                (\statuses dependency_job_id →
                                                    IntMap.adjust
                                                        (\(Running dependent_job_ids requesting_job_ids result_ivar) →
                                                            Running
                                                                (IntSet.insert job_id dependent_job_ids)
                                                                requesting_job_ids
                                                                result_ivar
                                                        )
                                                        dependency_job_id
                                                        statuses
                                                )
                                                statuses
                                            .
                                            IntSet.toList
                                            $
                                            all_dependency_ids
                                    -- @nonl
                                    -- @-node:gcross.20100604204549.7671:<< Update the state of the job server. >>
                                    -- @nl
        -- @-node:gcross.20100604204549.7667:Request
        -- @+node:gcross.20100604204549.1382:Return
        Return (result_,cache_) → do
            result ← liftIO . evaluate . withStrategy rdeepseq $ result_
            Just (Running _ requesting_job_ids result_ivar) ←
                fmap (IntMap.lookup job_id)
                .
                getAndModify serverJobStatuses
                .
                IntMap.insert job_id
                .
                Succeeded
                $
                result
            liftIO . IVar.write result_ivar $ result
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
-- @+node:gcross.20100607083309.1397:startJobServer
startJobServer :: NFData result => Int → IO (JobServer result)
startJobServer number_of_io_slaves = do
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
            ,   serverJobCache_ = Map.empty
            }
    let job_server = JobServer job_queue
    addFinalizer job_server . mapM_ killThread . (job_server_thread_id:) $ io_slave_thread_ids
    return job_server
-- @-node:gcross.20100607083309.1397:startJobServer
-- @+node:gcross.20100607083309.1402:runJobServer
runJobServer :: NFData result => StateT (JobServerState result) IO ()
runJobServer = forever $
    get serverJobQueue >>= liftIO . readChan >>= processJob
-- @-node:gcross.20100607083309.1402:runJobServer
-- @-node:gcross.20100604204549.1359:Functions
-- @-others
-- @-node:gcross.20100604184944.1289:@thin Jobs.hs
-- @-leo
