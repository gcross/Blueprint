-- @+leo-ver=4-thin
-- @+node:gcross.20100604184944.1289:@thin Jobs.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100604184944.1291:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
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
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State (StateT,evalStateT)
import Control.Parallel
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

import Blueprint.Identifier
import Blueprint.IOTask
-- @nonl
-- @-node:gcross.20100604184944.1292:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100604204549.1369:Exceptions
-- @+node:gcross.20100613184558.1652:Helper functions
-- @+node:gcross.20100613184558.1651:showLabels
showLabels :: Show label ⇒ [label] → String
showLabels labels =
    case nub . map show $ labels of
        [string] → "'" ++ string ++ "'"
        strings → ("["++) . (++ "]") . intercalate ", " $ strings
-- @-node:gcross.20100613184558.1651:showLabels
-- @-node:gcross.20100613184558.1652:Helper functions
-- @+node:gcross.20100604204549.1370:ConflictingJobException
data ConflictingJobException = ∀ label. (Show label, Typeable label) ⇒ ConflictingJobException [label] [label]
  deriving (Typeable)

instance Show ConflictingJobException where
    show (ConflictingJobException names conflicting_names) =
        "Attempted to submit job under name(s) " ++ showLabels names ++ " which conflicts with existing jobs that already have name(s) " ++ showLabels conflicting_names ++ "."

instance Exception ConflictingJobException
-- @-node:gcross.20100604204549.1370:ConflictingJobException
-- @+node:gcross.20100604204549.1374:NoSuchJobsException
data NoSuchJobsException = ∀ label. (Show label, Typeable label) ⇒ NoSuchJobsException [label]
  deriving (Typeable)

instance Show NoSuchJobsException where
    show (NoSuchJobsException names) =
        "Requsted result for jobs " ++ showLabels names ++ " which do no exist."

instance Exception NoSuchJobsException
-- @-node:gcross.20100604204549.1374:NoSuchJobsException
-- @+node:gcross.20100604204549.1377:CombinedException
data CombinedException = ∀ label. (Show label, Typeable label) ⇒ CombinedException [([label],SomeException)] deriving Typeable

instance Show CombinedException where
    show (CombinedException exceptions) =
        intercalate (replicate 72 '=' ++ "\n")
        .
        map (\(names,exception) →
            "Error executing job " ++ showLabels names ++ ":\n" ++ show exception
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
-- @+node:gcross.20100607205618.1444:ReturnedWrongNumberOfResults
data ReturnedWrongNumberOfResults = ReturnedWrongNumberOfResults Int Int
  deriving (Typeable)

instance Show ReturnedWrongNumberOfResults where
    show (ReturnedWrongNumberOfResults actual_count expected_count) =
        "Job returned " ++ show actual_count ++ " results, when it had promised to return " ++ show expected_count ++ " results."

instance Exception ReturnedWrongNumberOfResults
-- @-node:gcross.20100607205618.1444:ReturnedWrongNumberOfResults
-- @-node:gcross.20100604204549.1369:Exceptions
-- @+node:gcross.20100604184944.1293:Types
-- @+node:gcross.20100709210816.2107:Job
data Job label result cache = Job
    {   jobNames :: [label]
    ,   jobTask :: JobRunner label result cache
    }
-- @nonl
-- @-node:gcross.20100709210816.2107:Job
-- @+node:gcross.20100624100717.2145:JobId
data OfJob deriving Typeable
type JobId = Identifier OfJob
-- @-node:gcross.20100624100717.2145:JobId
-- @+node:gcross.20100624100717.1751:JobResults
data JobResults result cache = JobResults
    {   jobResults :: [result]
    ,   jobCache :: cache
    }
-- @-node:gcross.20100624100717.1751:JobResults
-- @+node:gcross.20100624100717.1749:JobTask
data JobTask label result a =
    Request [label] ([result] → JobTask label result a)
  | ∀ b. PerformIO (IO b) (b → JobTask label result a)
  | Return a
-- @-node:gcross.20100624100717.1749:JobTask
-- @+node:gcross.20100624100717.1747:JobTaskResult
type JobTaskResult label result cache = JobTask label result (JobResults result cache)
-- @-node:gcross.20100624100717.1747:JobTaskResult
-- @+node:gcross.20100624100717.1753:JobStatus
data JobStatus label result =
    ∀ cache. Binary cache => Pending (JobRunner label result cache)
  | Running [Int] [(Int,MVar (Either SomeException result))]
  | Succeeded [result]
  | Failed (IntMap SomeException) CombinedException
-- @-node:gcross.20100624100717.1753:JobStatus
-- @+node:gcross.20100604204549.1368:JobQueueEntry
data JobQueueEntry label result = 
    ∀ cache. Binary cache => JobSubmission (Job label result cache) ThreadId
  | JobFailure Int (IntMap SomeException)
  | ExternalRequest label (MVar (Either SomeException result))
  | ExternalRequestForCache (MVar (Map [label] ByteString))
  | ∀ cache. Binary cache => JobTask Int (JobTaskResult label result cache)
-- @-node:gcross.20100604204549.1368:JobQueueEntry
-- @+node:gcross.20100624100717.1743:JobRunner
type JobRunner label result cache = Maybe cache → JobTaskResult label result cache
-- @-node:gcross.20100624100717.1743:JobRunner
-- @+node:gcross.20100607083309.1398:JobServer
data JobServer label result = JobServer (Chan (JobQueueEntry label result)) [ThreadId]
-- @-node:gcross.20100607083309.1398:JobServer
-- @+node:gcross.20100624100717.1755:PausedJob
data PausedJob label result = ∀ cache. Binary cache => PausedJob
    {   pausedJobPendingRequests :: IntSet
    ,   pausedJobRequests :: [(Int,Int)]
    ,   pausedJobComputeTask :: [result] → JobTask label result (JobResults result cache)
    }
-- @-node:gcross.20100624100717.1755:PausedJob
-- @+node:gcross.20100604204549.1375:JobServerState
data JobServerState label result = JobServerState
    {   serverNextJobId_ :: Int
    ,   serverJobIds_ :: Map label (Int,Int)
    ,   serverJobKeys_ :: IntMap [label]
    ,   serverJobStatuses_ :: IntMap (JobStatus label result)
    ,   serverJobQueue_ :: Chan (JobQueueEntry label result)
    ,   serverPausedJobs_ :: IntMap (PausedJob label result)
    ,   serverIOTaskQueue_ :: Chan (IOTask (JobQueueEntry label result))
    ,   serverJobCache_ :: Map [label] ByteString
    }

$( deriveAccessors ''JobServerState )
-- @-node:gcross.20100604204549.1375:JobServerState
-- @+node:gcross.20100709210816.2224:JobServerMonad
type JobServerMonad label result = ReaderT (JobServer label result) IO
-- @-node:gcross.20100709210816.2224:JobServerMonad
-- @-node:gcross.20100604184944.1293:Types
-- @+node:gcross.20100604184944.1297:Instances
-- @+node:gcross.20100604184944.1309:Functor JobTask
instance Functor (JobTask label result) where
    fmap f m =
        case m of
            Request requests g → Request requests (fmap f . g)
            PerformIO task g → PerformIO task (fmap f . g)
            Return a → Return (f a)
-- @-node:gcross.20100604184944.1309:Functor JobTask
-- @+node:gcross.20100604184944.1298:Monad JobTask
instance Monad (JobTask label result) where
    Return a >>= b = b a
    Request requests f >>= b = Request requests (f >=> b)
    PerformIO task f >>= b = PerformIO task (f >=> b)
    return = Return
    fail = error
-- @-node:gcross.20100604184944.1298:Monad JobTask
-- @+node:gcross.20100604184944.1306:MonadIO JobTask
instance MonadIO (JobTask label result) where
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
    (Ord label
    ,Show label
    ,Typeable label
    ) =>
    Int →
    Map [label] ByteString →
    IO (JobServer label result)
startJobServer number_of_io_slaves starting_cache = do
    job_queue ← newChan
    io_task_queue ← newChan
    io_slave_thread_ids ← replicateM number_of_io_slaves $ spawnIOTaskRunner io_task_queue job_queue
    job_server_thread_id ←
        forkIO . liftIO . evalStateT runJobServer $
            JobServerState
            {   serverNextJobId_ = 0
            ,   serverJobIds_ = Map.empty
            ,   serverJobKeys_ = IntMap.empty
            ,   serverJobStatuses_ = IntMap.empty
            ,   serverJobQueue_ = job_queue
            ,   serverPausedJobs_ = IntMap.empty
            ,   serverIOTaskQueue_ = io_task_queue
            ,   serverJobCache_ = starting_cache
            }
    return $ JobServer job_queue (job_server_thread_id:io_slave_thread_ids)
-- @-node:gcross.20100607083309.1397:startJobServer
-- @+node:gcross.20100607083309.1469:killJobServer
killJobServer :: JobServer label result → IO ()
killJobServer (JobServer _ threads) = mapM_ killThread threads
-- @-node:gcross.20100607083309.1469:killJobServer
-- @+node:gcross.20100607083309.1470:withJobServer
withJobServer ::
    (Ord label
    ,Show label
    ,Typeable label
    ) =>
    Int →
    Map [label] ByteString →
    JobServerMonad label result a →
    IO a
withJobServer number_of_io_slaves starting_cache thunk =
    bracket
        (startJobServer number_of_io_slaves starting_cache)
        killJobServer
        (runReaderT thunk)
-- @-node:gcross.20100607083309.1470:withJobServer
-- @+node:gcross.20100607083309.1406:submitJobToServer
submitJobToServer ::
    Binary cache =>
    JobServer label result →
    Job label result cache →
    IO ()
submitJobToServer (JobServer job_queue _) job =
    myThreadId >>= writeChan job_queue . JobSubmission job
-- @-node:gcross.20100607083309.1406:submitJobToServer
-- @+node:gcross.20100709210816.2230:submitJob
submitJob ::
    Binary cache =>
    Job label result cache →
    JobServerMonad label result ()
submitJob = ReaderT . flip submitJobToServer
-- @-node:gcross.20100709210816.2230:submitJob
-- @+node:gcross.20100607083309.1409:requestJobResultFromServer
requestJobResultFromServer :: JobServer label result → label → IO result
requestJobResultFromServer (JobServer job_queue _) requested_job_name = do
    result_var ← newEmptyMVar
    writeChan job_queue $ ExternalRequest requested_job_name result_var
    result ← takeMVar result_var
    case result of
        Left exception → throwIO exception
        Right value → return value
-- @-node:gcross.20100607083309.1409:requestJobResultFromServer
-- @+node:gcross.20100709210816.2226:requestJobResult
requestJobResult :: label → JobServerMonad label result result
requestJobResult = ReaderT . flip requestJobResultFromServer
-- @-node:gcross.20100709210816.2226:requestJobResult
-- @+node:gcross.20100709210816.2228:requestJobCacheFromServer
requestJobCacheFromServer :: JobServer label result → IO (Map [label] ByteString)
requestJobCacheFromServer (JobServer job_queue _) = do
    result_var ← newEmptyMVar
    writeChan job_queue $ ExternalRequestForCache result_var
    takeMVar result_var
-- @-node:gcross.20100709210816.2228:requestJobCacheFromServer
-- @+node:gcross.20100607205618.1429:requestJobCache
requestJobCache :: JobServerMonad label result (Map [label] ByteString)
requestJobCache = ReaderT requestJobCacheFromServer
-- @-node:gcross.20100607205618.1429:requestJobCache
-- @+node:gcross.20100607083309.1414:returnValuesAndCache
returnValuesAndCache values cache = values `par` cache `par` return (JobResults values cache)
-- @-node:gcross.20100607083309.1414:returnValuesAndCache
-- @+node:gcross.20100607205618.1443:returnValueAndCache
returnValueAndCache value cache = value `par` cache `par` return (JobResults [value] cache)
-- @-node:gcross.20100607205618.1443:returnValueAndCache
-- @+node:gcross.20100607205618.1427:returnValue
returnValue value = returnValueAndCache value ()
-- @-node:gcross.20100607205618.1427:returnValue
-- @+node:gcross.20100607205618.1441:returnValues
returnValues values = returnValuesAndCache values ()
-- @-node:gcross.20100607205618.1441:returnValues
-- @+node:gcross.20100607083309.1417:request
request :: [label] → JobTask label result [result]
request [] = return []
request requests = Request requests return
-- @-node:gcross.20100607083309.1417:request
-- @-node:gcross.20100607083309.1404:Interface
-- @+node:gcross.20100607083309.1405:Implementation
-- @+node:gcross.20100604204549.7666:failJobWithExceptions
failJobWithExceptions ::
    (Ord label
    ,Show label
    ,Typeable label
    ) =>
    Int →
    IntMap SomeException →
    StateT (JobServerState label result) IO ()
failJobWithExceptions job_id exceptions = do
    combined_exception ← combineExceptions exceptions
    Just (Running requesting_job_ids listeners) ←
        fmap (IntMap.lookup job_id)
        .
        getAndModify serverJobStatuses
        $
        IntMap.insert job_id (Failed exceptions combined_exception)
    let r = Left . toException $ combined_exception
    liftIO . mapM (flip putMVar r . snd) $ listeners
    notifyPausedJobsThatRequestedJobIsFinished job_id requesting_job_ids
    Just names ← fmap (IntMap.lookup job_id) (get serverJobKeys)
    serverJobCache %:
        (Map.delete names)
-- @nonl
-- @-node:gcross.20100604204549.7666:failJobWithExceptions
-- @+node:gcross.20100604204549.1388:fetchResultsAndRunJobTask
fetchResultsAndRunJobTask ::
    (Ord label
    ,Show label
    ,Typeable label
    ,Binary cache
    ) =>
    Int →
    [(Int,Int)] →
    ([result] → JobTaskResult label result cache) →
    StateT (JobServerState label result) IO ()
fetchResultsAndRunJobTask job_id requests computeTask = do
    server_job_statuses ← get serverJobStatuses
    let (exceptions,results) =
            first IntMap.unions
            .
            partitionEithers
            .
            map (
                \(job_id,result_number) →
                    case fromJust (IntMap.lookup job_id server_job_statuses) of
                        Failed exceptions _ → Left exceptions
                        Succeeded result → Right (result !! result_number)
            )
            $
            requests
    if (not . IntMap.null) exceptions
        then failJobWithExceptions job_id exceptions
        else do
            server_job_queue ← get serverJobQueue
            (liftIO . writeChan server_job_queue . JobTask job_id . computeTask) $|| rwhnf $ results
-- @nonl
-- @-node:gcross.20100604204549.1388:fetchResultsAndRunJobTask
-- @+node:gcross.20100604204549.1387:combineExceptions
combineExceptions ::
    (Ord label
    ,Show label
    ,Typeable label
    ) =>
    IntMap SomeException →
    StateT (JobServerState label result) IO CombinedException
combineExceptions exceptions =
    get serverJobKeys
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
notifyPausedJobsThatRequestedJobIsFinished ::
    (Ord label
    ,Show label
    ,Typeable label
    ) => 
    Int →
    [Int] →
    StateT (JobServerState label result) IO ()
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
    (Ord label
    ,Show label
    ,Binary cache
    ) =>
    Int →
    JobRunner label result cache →
    StateT (JobServerState label result) IO ()
startJobTask job_id computeTaskFromCache = do
    server_job_cache ← get serverJobCache
    server_job_names ← get serverJobKeys
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
addJobResultListener ::
    Int →
    Int →
    MVar (Either SomeException result) →
    StateT (JobServerState label result) IO ()
addJobResultListener job_id result_number destination_var =
    serverJobStatuses %:
        (IntMap.adjust (\(Running dependent_job_ids listeners) → Running dependent_job_ids ((result_number,destination_var):listeners)) job_id)
-- @-node:gcross.20100607083309.1418:addJobResultListener
-- @+node:gcross.20100604204549.7659:processJobQueueEntry
processJobQueueEntry ::
    (Ord label
    ,Show label
    ,Typeable label
    ) =>
    JobQueueEntry label result →
    StateT (JobServerState label result) IO ()
-- @+others
-- @+node:gcross.20100604204549.7660:JobSubmission
processJobQueueEntry (JobSubmission (Job names computeTaskFromCache) thread_id) = do
    server_job_ids ← get serverJobIds
    case filter (flip Map.member server_job_ids) names of
        [] → do
            job_id ← getAndModify serverNextJobId (+1)
            serverJobIds %:
                \server_job_ids →
                    foldl' -- '
                        (\job_ids (result_number,name) → Map.insert name (job_id,result_number) job_ids) 
                        server_job_ids
                    .
                    zip [0..]
                    $
                    names
            serverJobKeys %:
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
-- @nonl
-- @-node:gcross.20100604204549.7660:JobSubmission
-- @+node:gcross.20100604204549.7661:ExternalRequest
processJobQueueEntry (ExternalRequest name destination_var) = do
    maybe_id ← fmap (Map.lookup name) (get serverJobIds)
    case maybe_id of
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
        Just (job_id,result_number) → do
            job_status ← fmap (fromJust . IntMap.lookup job_id) (get serverJobStatuses)
            case job_status of
                Pending computeTaskFromCache → do
                    startJobTask job_id computeTaskFromCache
                    addJobResultListener job_id result_number destination_var
                Running _ _ → do
                    addJobResultListener job_id result_number destination_var
                Succeeded result → do
                    liftIO
                    .
                    putMVar destination_var
                    .
                    Right
                    .
                    (!! result_number)
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
-- @+node:gcross.20100607205618.1425:ExternalRequestForCache
processJobQueueEntry (ExternalRequestForCache destination_var) =
    get serverJobCache >>= liftIO . putMVar destination_var
-- @-node:gcross.20100607205618.1425:ExternalRequestForCache
-- @+node:gcross.20100604204549.7665:JobFailure
processJobQueueEntry (JobFailure job_id exceptions) =
    failJobWithExceptions job_id exceptions
-- @-node:gcross.20100604204549.7665:JobFailure
-- @+node:gcross.20100604204549.7662:JobTask
processJobQueueEntry (JobTask job_id task) =
    flip M.catch (failJobWithExceptions job_id . IntMap.singleton job_id)
    $
    case task of
        -- @        @+others
        -- @+node:gcross.20100604204549.7667:Request
        Request requested_job_names computeTask → do
            -- @    << Fetch ids of the requested jobs. >>
            -- @+node:gcross.20100604204549.7673:<< Fetch ids of the requested jobs. >>
            (non_existing_jobs,requests) ← get serverJobIds >>= \server_job_ids →
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
            let requested_job_ids = nub . map fst $ requests
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
                        then fetchResultsAndRunJobTask job_id requests computeTask
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
                                        ,   pausedJobRequests = requests
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
        Return (JobResults results_ cache_) → do
            results ← liftIO . evaluate $ results_
            let number_of_returned_results = length results
            number_of_expected_results ←
                fmap (
                    length
                    .
                    fromJust
                    .
                    IntMap.lookup job_id
                )
                $
                get serverJobKeys
            when (number_of_returned_results /= number_of_expected_results) $
                liftIO . throwIO $
                    ReturnedWrongNumberOfResults number_of_returned_results number_of_expected_results
            Just (Running requesting_job_ids listeners) ←
                fmap (IntMap.lookup job_id)
                .
                getAndModify serverJobStatuses
                .
                IntMap.insert job_id
                .
                Succeeded
                $
                results
            liftIO . forM_ listeners $ \(result_number,destination_var) →
                putMVar destination_var . Right . (!! result_number) $ results
            notifyPausedJobsThatRequestedJobIsFinished job_id requesting_job_ids

            cache ← liftIO . evaluate . withStrategy rwhnf . encode $ cache_
            Just names ← fmap (IntMap.lookup job_id) (get serverJobKeys)
            if L.null cache
                then serverJobCache %: (Map.delete names)
                else serverJobCache %: (Map.insert names cache)
        -- @nonl
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
-- @-node:gcross.20100604204549.7659:processJobQueueEntry
-- @+node:gcross.20100607083309.1402:runJobServer
runJobServer ::
    (Ord label
    ,Show label
    ,Typeable label
    ) =>
    StateT (JobServerState label result) IO ()
runJobServer =
    (forever $
        get serverJobQueue >>= liftIO . readChan >>= processJobQueueEntry
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
