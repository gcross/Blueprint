-- @+leo-ver=4-thin
-- @+node:gcross.20100604184944.1289:@thin Jobs.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100604184944.1291:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100604184944.1291:<< Language extensions >>
-- @nl

module Blueprint.Jobs where

-- @<< Import needed modules >>
-- @+node:gcross.20100604184944.1292:<< Import needed modules >>
import Prelude hiding (catch)

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IVar.Simple (IVar)
import qualified Data.IVar.Simple as IVar
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
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
data NoSuchJobException = NoSuchJobException String
  deriving (Typeable)

instance Show NoSuchJobException where
    show (NoSuchJobException name) =
        "Requsted result for job '" ++ name ++ "' which does not exist."

instance Exception NoSuchJobException
-- @-node:gcross.20100604204549.1374:NoSuchJobException
-- @+node:gcross.20100604204549.1377:CombinedException
data CombinedException = CombinedException [SomeException] deriving (Typeable)

instance Show CombinedException where
    show (CombinedException exceptions) = intercalate "\n" . map show $ exceptions

instance Exception CombinedException
-- @-node:gcross.20100604204549.1377:CombinedException
-- @-node:gcross.20100604204549.1369:Exceptions
-- @+node:gcross.20100604184944.1293:Types
-- @+node:gcross.20100604204549.1364:IOTask
data IOTask a = forall r. IOTask (IO r) (Either SomeException r → a)
-- @-node:gcross.20100604204549.1364:IOTask
-- @+node:gcross.20100604204549.1368:Job
data Job result = 
    JobSubmission [String] (JobTask result result) ThreadId
  | ExternalRequest String (IVar result)
  | JobTask Int (JobTask result result)
-- @-node:gcross.20100604204549.1368:Job
-- @+node:gcross.20100604184944.1294:JobStatus
data JobStatus result =
    Pending (JobTask result result)
  | Running (IVar result)
  | Succeeded result
  | Failed (IntMap SomeException)
-- @-node:gcross.20100604184944.1294:JobStatus
-- @+node:gcross.20100604184944.1295:JobTask
data JobTask r a =
    Request (r → JobTask r a)
  | forall b. PerformIO (IO b) (b → JobTask r a)
  | Return a
-- @-node:gcross.20100604184944.1295:JobTask
-- @+node:gcross.20100604204549.1343:JobRequest
data JobRequest result =
    RegisterJob String (JobTask result result)
  | FetchResult String
-- @-node:gcross.20100604204549.1343:JobRequest
-- @+node:gcross.20100604204549.1345:JobRegistry
type JobRegistry result = Map String (JobStatus result)
-- @-node:gcross.20100604204549.1345:JobRegistry
-- @+node:gcross.20100604204549.1344:JobServer
type JobServer result = Chan (JobRequest result)
-- @-node:gcross.20100604204549.1344:JobServer
-- @+node:gcross.20100604204549.1375:JobServerState
data JobServerState result = JobServerState
    {   serverNextJobId :: Int
    ,   serverJobIds :: Map String Int
    ,   serverJobStatuses :: IntMap (JobStatus result)
    ,   serverJobQueue :: Chan (Job result)
    -- ,   serverIOTaskQueue :: Chan (IOTask _)
    }
-- @-node:gcross.20100604204549.1375:JobServerState
-- @-node:gcross.20100604184944.1293:Types
-- @+node:gcross.20100604184944.1297:Instances
-- @+node:gcross.20100604184944.1309:Functor JobTask
instance Functor (JobTask r) where
    fmap f m =
        case m of
            Request g -> Request (fmap f . g)
            PerformIO task g -> PerformIO task (fmap f . g)
            Return a -> Return (f a)
-- @-node:gcross.20100604184944.1309:Functor JobTask
-- @+node:gcross.20100604184944.1298:Monad JobTask
instance Monad (JobTask r) where
    Return a >>= b = b a
    Request f >>= b = Request (f >=> b)
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
-- @+node:gcross.20100604204549.1366:spawnIOTaskRunner
spawnIOTaskRunner :: Chan (IOTask return) → Chan return → IO ThreadId
spawnIOTaskRunner task_queue result_queue =
    forkIO . forever $
        readChan task_queue
        >>=
        \(IOTask task f) → (try task >>= writeChan result_queue . f)
-- @-node:gcross.20100604204549.1366:spawnIOTaskRunner
-- @+node:gcross.20100604204549.1367:processJob
processJob :: JobServerState result -> Job result -> IO (JobServerState result)
processJob state@JobServerState{..} job =
    case job of
        -- @        @+others
        -- @+node:gcross.20100604204549.1372:JobSubmission
        JobSubmission names task thread_id →
            case filter (flip Map.member serverJobIds) names of
                [] →
                    return $
                    state
                    {   serverNextJobId = serverNextJobId+1
                    ,   serverJobIds = 
                            foldl' -- '
                                (\job_ids name → Map.insert name serverNextJobId job_ids) 
                                serverJobIds
                                names
                    ,   serverJobStatuses =
                            IntMap.insert serverNextJobId (Pending task) serverJobStatuses
                    }
                conflicting_names →
                    throwTo thread_id (ConflictingJobException names conflicting_names)
                    >>
                    return state
        -- @-node:gcross.20100604204549.1372:JobSubmission
        -- @+node:gcross.20100604204549.1373:ExternalRequest
        ExternalRequest name destination_ivar ->
            case Map.lookup name serverJobIds of
                Nothing -> do
                    IVar.write destination_ivar (throw $ NoSuchJobException name)
                    return state
                Just job_id ->
                    case fromJust (IntMap.lookup job_id serverJobStatuses) of
                        Pending task -> do
                            result_ivar <- IVar.new
                            IVar.write destination_ivar (IVar.read result_ivar)
                            writeChan serverJobQueue (JobTask job_id task)
                            return $
                                state
                                {   serverJobStatuses =
                                        IntMap.insert job_id (Running result_ivar) serverJobStatuses
                                }
                        Running result_ivar -> do
                            IVar.write destination_ivar (IVar.read result_ivar)
                            return state
                        Succeeded result -> do
                            IVar.write destination_ivar result
                            return state
                        Failed exceptions -> do
                            IVar.write destination_ivar
                                .
                                throw
                                .
                                CombinedException
                                .
                                IntMap.elems
                                $
                                exceptions
                            return state
        -- @-node:gcross.20100604204549.1373:ExternalRequest
        -- @+node:gcross.20100604204549.1378:JobTask
        JobTask _ _ -> return state
        -- @-node:gcross.20100604204549.1378:JobTask
        -- @-others
-- @-node:gcross.20100604204549.1367:processJob
-- @-node:gcross.20100604204549.1359:Functions
-- @-others
-- @-node:gcross.20100604184944.1289:@thin Jobs.hs
-- @-leo
