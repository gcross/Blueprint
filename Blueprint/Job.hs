-- @+leo-ver=4-thin
-- @+node:gcross.20100924160650.2044:@thin Job.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100924160650.2045:<< Language extensions >>
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100924160650.2045:<< Language extensions >>
-- @nl

module Blueprint.Job where

-- @<< Import needed modules >>
-- @+node:gcross.20100924160650.2046:<< Import needed modules >>
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Data.Binary
import qualified Data.ByteString.Lazy as L
import Data.Dynamic
import Data.IVar.Simple (IVar)
import qualified Data.IVar.Simple as IVar
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import Data.UUID
-- @-node:gcross.20100924160650.2046:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100924160650.2047:Types
-- @+node:gcross.20100924160650.2048:Job
data Job α where
    Result :: α → Job α
    Task :: IO α → (α → Job β) → Job β
    Fork :: Job (α → β) → Job α → (β → Job ɣ) → Job ɣ
    Errors :: Map String SomeException → Job α
    Once :: Typeable α ⇒ UUID → Job α → (α → Job β) → Job β
    Cache :: Binary α ⇒ UUID → (Maybe α → Job (Maybe α,β)) → (β → Job ɣ) → Job ɣ
-- @-node:gcross.20100924160650.2048:Job
-- @+node:gcross.20100925004153.1301:JobEnvironment
data JobEnvironment = JobEnvironment
    {   environmentCompletedJobs :: IORef (Map UUID Dynamic)
    ,   environmentTaskRunner :: (∀ α. IO α → IO α)
    ,   environmentInputCache :: Map UUID L.ByteString
    ,   environmentOutputCache :: Chan (UUID,Maybe L.ByteString)
    }
-- @-node:gcross.20100925004153.1301:JobEnvironment
-- @+node:gcross.20100925004153.1302:TaskRunner
data TaskRunner = TaskRunner
    {   taskRunnerThreads :: [ThreadId]
    ,   taskRunnerQueue :: Chan TaskSubmission
    }
-- @-node:gcross.20100925004153.1302:TaskRunner
-- @+node:gcross.20100925004153.1303:TaskSubmission
data TaskSubmission where
    TaskSubmission :: IO α → IVar α → TaskSubmission
-- @-node:gcross.20100925004153.1303:TaskSubmission
-- @-node:gcross.20100924160650.2047:Types
-- @+node:gcross.20100924174906.1276:Instances
-- @+node:gcross.20100924174906.1278:Applicative Job
instance Applicative Job where
    pure = Result
    Result f <*> x = fmap f x
    Errors e1 <*> Errors e2 = Errors (Map.union e1 e2)
    f <*> x = Fork f x Result
-- @-node:gcross.20100924174906.1278:Applicative Job
-- @+node:gcross.20100924174906.1281:Functor Job
instance Functor Job where
    fmap f (Result x) = Result (f x)
    fmap f (Task io g) = Task io (fmap f . g)
    fmap f (Fork jf jx g) = Fork jf jx (fmap f . g)
    fmap f (Errors errors) = Errors errors
    fmap f (Once uuid jx g) = Once uuid jx (fmap f . g)
    fmap f (Cache uuid g h) = Cache uuid g (fmap f . h)
-- @-node:gcross.20100924174906.1281:Functor Job
-- @+node:gcross.20100924174906.1277:Monad Job
instance Monad Job where
    return = Result
    Result x >>= f = f x
    Task io f >>= g = Task io (f >=> g)
    Fork jf jx f >>= g = Fork jf jx (f >=> g)
    Errors errors >>= _ = Errors errors
    Once uuid x f >>= g = Once uuid x (f >=> g)
    Cache uuid f g >>= h = Cache uuid f (g >=> h)
-- @-node:gcross.20100924174906.1277:Monad Job
-- @+node:gcross.20100925004153.1329:MonadIO Job
instance MonadIO Job where
    liftIO io = Task io return
-- @-node:gcross.20100925004153.1329:MonadIO Job
-- @-node:gcross.20100924174906.1276:Instances
-- @+node:gcross.20100925004153.1297:Functions
-- @+node:gcross.20100925004153.1299:cache
cache :: Binary α ⇒ UUID → (Maybe α → Job (Maybe α,β)) → Job β
cache uuid computeJob = Cache uuid computeJob return
-- @-node:gcross.20100925004153.1299:cache
-- @+node:gcross.20100925004153.1298:once
once :: Typeable α ⇒ UUID → Job α → Job α
once uuid job = Once uuid job return
-- @-node:gcross.20100925004153.1298:once
-- @+node:gcross.20100925004153.1307:runJob
runJob :: JobEnvironment → Job α → IO (Either (Map String SomeException) α)
runJob job_environment@JobEnvironment{..} job =
    case job of
        Result x → return (Right x)
        Task io computeNextJob →
            environmentTaskRunner io
            >>=
            runJob job_environment . computeNextJob
        Fork jf jx computeNextJob → do
            f_ivar ← IVar.new
            forkIO $ nestedRunJob jf >>= IVar.write f_ivar
            x_ivar ← IVar.new
            forkIO $ nestedRunJob jx >>= IVar.write x_ivar
            case (IVar.read f_ivar, IVar.read x_ivar) of
                (Right f, Right x) → nestedRunJob (computeNextJob (f x))
                (Left errors, Right _) → return (Left errors)
                (Right _, Left errors) → return (Left errors)
                (Left errors1, Left errors2) → return (Left (Map.union errors1 errors2))
        Errors errors → return (Left errors)
        Once uuid job computeNextJob → do
            result_ivar ← IVar.new
            maybe_previous_result ←
                atomicModifyIORef environmentCompletedJobs $ \completed_jobs →
                    case Map.lookup uuid completed_jobs of
                        Nothing →
                            (Map.insert uuid (IVar.read result_ivar) completed_jobs
                            ,Nothing
                            )
                        Just previous_result →
                            (completed_jobs
                            ,Just previous_result
                            )
            case maybe_previous_result of
                Nothing → do
                    job_result ← nestedRunJob job
                    case job_result of
                        Left errors → return (Left errors)
                        Right result → do
                            IVar.write result_ivar (toDyn result)
                            nestedRunJob (computeNextJob result)
                Just previous_result →
                    nestedRunJob
                    .
                    computeNextJob
                    .
                    fromJust
                    .
                    fromDynamic
                    $
                    previous_result
        Cache uuid
         computeJob computeNextJob → do
            let maybe_old_cached_value =
                    fmap decode
                    .
                    Map.lookup uuid
                    $
                    environmentInputCache
            job_result ← nestedRunJob (computeJob maybe_old_cached_value)
            case job_result of
                Left errors → return (Left errors)
                Right (maybe_new_cached_value,job_result) → do
                    writeChan environmentOutputCache (uuid,fmap encode maybe_new_cached_value)
                    nestedRunJob (computeNextJob job_result)
  where
    nestedRunJob = runJob job_environment
-- @-node:gcross.20100925004153.1307:runJob
-- @+node:gcross.20100925004153.1306:startTaskRunner
startTaskRunner :: Int → IO TaskRunner
startTaskRunner number_of_slaves = do
    task_queue ← newChan
    slave_thread_ids ←
        replicateM number_of_slaves $
            startTaskSlave task_queue
    return $
        TaskRunner
            slave_thread_ids
            task_queue
-- @-node:gcross.20100925004153.1306:startTaskRunner
-- @+node:gcross.20100925004153.1304:startTaskSlave
startTaskSlave :: Chan TaskSubmission → IO ThreadId
startTaskSlave task_queue =
    forkIO . forever $
        readChan task_queue
        >>=
        \(TaskSubmission task ivar) →
            fmap (either (throw :: SomeException → α) id) (try task)
            >>=
            IVar.write ivar
-- @-node:gcross.20100925004153.1304:startTaskSlave
-- @-node:gcross.20100925004153.1297:Functions
-- @-others
-- @-node:gcross.20100924160650.2044:@thin Job.hs
-- @-leo
