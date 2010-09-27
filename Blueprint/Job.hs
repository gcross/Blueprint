-- @+leo-ver=4-thin
-- @+node:gcross.20100924160650.2044:@thin Job.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100924160650.2045:<< Language extensions >>
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100924160650.2045:<< Language extensions >>
-- @nl

module Blueprint.Job where

-- @<< Import needed modules >>
-- @+node:gcross.20100924160650.2046:<< Import needed modules >>
import Prelude hiding (catch)

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.Trans.Reader

import Data.Binary
import qualified Data.ByteString.Lazy as L
import Data.Dynamic
import Data.IVar.Simple (IVar)
import qualified Data.IVar.Simple as IVar
import Data.IORef
import Data.List
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
    Errors :: [SomeException] → Job α
    Once :: Typeable α ⇒ UUID → Job α → (α → Job β) → Job β
    Cache :: Binary α ⇒ UUID → (Maybe α → Job (Maybe α,β)) → (β → Job ɣ) → Job ɣ
-- @-node:gcross.20100924160650.2048:Job
-- @+node:gcross.20100925004153.1301:JobEnvironment
data JobEnvironment = JobEnvironment
    {   environmentCompletedJobs :: IORef (Map UUID Dynamic)
    ,   environmentTaskSemaphore :: QSem
    ,   environmentInputCache :: Map UUID L.ByteString
    ,   environmentOutputCache :: Chan (UUID,Maybe L.ByteString)
    }
-- @-node:gcross.20100925004153.1301:JobEnvironment
-- @+node:gcross.20100927123234.1301:JobMonad
type JobMonad = ReaderT JobEnvironment IO
-- @-node:gcross.20100927123234.1301:JobMonad
-- @-node:gcross.20100924160650.2047:Types
-- @+node:gcross.20100924174906.1276:Instances
-- @+node:gcross.20100924174906.1278:Applicative Job
instance Applicative Job where
    pure = Result
    Result f <*> x = fmap f x
    Errors e1 <*> Errors e2 = Errors (e1 ++ e2)
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
-- @+node:gcross.20100927123234.1300:runJob
runJob :: Job α → JobMonad (Either [SomeException] α)
runJob job = do
    job_environment ← ask
    liftIO $ runJobInEnvironment job_environment job
-- @-node:gcross.20100927123234.1300:runJob
-- @+node:gcross.20100925004153.1307:runJobInEnvironment
runJobInEnvironment :: JobEnvironment → Job α → IO (Either [SomeException] α)
runJobInEnvironment job_environment@JobEnvironment{..} job =
    (case job of
        Result x → return (Right x)
        Task io computeNextJob →
            bracket_
                (waitQSem environmentTaskSemaphore)
                (signalQSem environmentTaskSemaphore)
                io
            >>=
            nestedRunJob . computeNextJob
        Fork jf jx computeNextJob → do
            x_or_error ← case jx of
                    Result x → return (Right x)
                    _ → do
                        x_ivar ← IVar.new
                        forkIO $ nestedRunJob jx >>= IVar.write x_ivar
                        return (IVar.read x_ivar)
            f_or_error ← nestedRunJob jf >>= evaluate
            case (f_or_error, x_or_error) of
                (Right f, Right x) → nestedRunJob . computeNextJob . f $ x
                (Left errors, Right _) → return . Left $ errors
                (Right _, Left errors) → return . Left $ errors
                (Left errors1, Left errors2) → return . Left $ errors1 ++ errors2
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
        Cache uuid computeJob computeNextJob → do
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
    ) `catch` (return . Left . (:[]))
  where
    nestedRunJob = runJobInEnvironment job_environment
-- @-node:gcross.20100925004153.1307:runJobInEnvironment
-- @+node:gcross.20100927123234.1302:withJobEnvironment
withJobEnvironment :: Int → Map UUID L.ByteString → JobMonad α → IO (α,Map UUID L.ByteString)
withJobEnvironment maximum_number_of_simultaneous_IO_tasks input_cache job_monad = do
    job_environment@JobEnvironment{..} ←
        newJobEnvironment
            maximum_number_of_simultaneous_IO_tasks
            input_cache
    result ← runReaderT job_monad job_environment
    cache_updates ←
        whileM
            (fmap not (isEmptyChan environmentOutputCache))
            (readChan environmentOutputCache)
    return
        (result
        ,foldl' -- '
            (\current_cache (key,maybe_value) →
                case maybe_value of
                    Nothing → Map.delete key current_cache
                    Just value → Map.insert key value current_cache
            )
            environmentInputCache
            cache_updates
        )
-- @-node:gcross.20100927123234.1302:withJobEnvironment
-- @+node:gcross.20100927123234.1303:newJobEnvironment
newJobEnvironment :: Int → Map UUID L.ByteString → IO JobEnvironment
newJobEnvironment maximum_number_of_simultaneous_IO_tasks input_cache =
    liftM4 JobEnvironment
        (newIORef Map.empty)
        (newQSem maximum_number_of_simultaneous_IO_tasks)
        (return input_cache)
        newChan
-- @-node:gcross.20100927123234.1303:newJobEnvironment
-- @-node:gcross.20100925004153.1297:Functions
-- @-others
-- @-node:gcross.20100924160650.2044:@thin Job.hs
-- @-leo