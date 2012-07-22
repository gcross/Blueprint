-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Blueprint.Job where

-- Imports {{{
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
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Dynamic
import Data.Either.Unwrap
import Data.IORef
import Data.IVar.Simple (IVar)
import qualified Data.IVar.Simple as IVar
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Data.UUID

import System.Directory

import Blueprint.Identifier
-- }}}

-- Types {{{
data Job α where -- {{{
    Result :: α → Job α
    Task :: IO α → (α → Job β) → Job β
    Fork :: Job (α → β) → Job α → (β → Job ɣ) → Job ɣ
    Once :: Typeable α ⇒ JobIdentifier → Job α → (α → Job β) → Job β
    Cache :: Binary α ⇒ JobIdentifier → (Maybe α → Job (Maybe α,β)) → (β → Job ɣ) → Job ɣ
  deriving Typeable
-- }}}
data JobEnvironment = JobEnvironment -- {{{
    {   environmentCompletedJobs :: IORef (Map UUID (Either JobError Dynamic))
    ,   environmentTaskSemaphore :: QSem
    ,   environmentInputCache :: Map UUID L.ByteString
    ,   environmentOutputCache :: Chan (UUID,Maybe L.ByteString)
    }
-- }}}
-- type JobIdentifier {{{
data OfJob
type JobIdentifier = Identifier OfJob
-- }}}
-- }}}

-- Exceptions {{{
data CycleDetected = CycleDetected deriving Typeable -- {{{

instance Show CycleDetected where
    show CycleDetected = "Cycle detected."

instance Exception CycleDetected
-- }}}
data ErrorInDependency = ErrorInDependency String deriving Typeable -- {{{

instance Show ErrorInDependency where
    show (ErrorInDependency dependency) = "Error " ++ dependency ++ "."

instance Exception ErrorInDependency
-- }}}
data JobError = JobError -- {{{
    {   jobErrorsWithHeadings :: Map String [SomeException]
    ,   jobErrors :: Map String SomeException
    } deriving Typeable

instance Show JobError where
    show (JobError{..}) =
        unlines (
            concatMap
                (\(heading,exceptions) →
                     ""
                    :("Error " ++ heading ++ ":")
                    :(  map ('\t':) -- '
                        .
                        concatMap (lines . show)
                     ) exceptions
                )
                (Map.assocs jobErrorsWithHeadings)
            ++
            concatMap
                (("":) . (:[]))
                (Map.keys jobErrors)
        )

instance Exception JobError
-- }}}
-- }}}

-- Instances {{{
instance Applicative Job where -- {{{
    pure = Result
    Result f <*> x = fmap f x
    f <*> x = Fork f x Result
-- }}}
instance Functor Job where -- {{{
    fmap f (Result x) = Result (f x)
    fmap f (Task io g) = Task io (fmap f . g)
    fmap f (Fork jf jx g) = Fork jf jx (fmap f . g)
    fmap f (Once uuid jx g) = Once uuid jx (fmap f . g)
    fmap f (Cache uuid g h) = Cache uuid g (fmap f . h)
-- }}}
instance Monad Job where -- {{{
    return = Result
    Result x >>= f = f x
    Task io f >>= g = Task io (f >=> g)
    Fork jf jx f >>= g = Fork jf jx (f >=> g)
    Once uuid x f >>= g = Once uuid x (f >=> g)
    Cache uuid f g >>= h = Cache uuid f (g >=> h)
-- }}}
instance MonadIO Job where -- {{{
    liftIO io = Task io return
-- }}}
instance Monoid JobError where -- {{{
    mempty = JobError Map.empty Map.empty
    (JobError h1 e1) `mappend` (JobError h2 e2) = JobError (Map.union h2 h1) (Map.union e2 e1)
-- }}}
-- }}}

-- Functions {{{
cache :: Binary α ⇒ JobIdentifier → (Maybe α → Job (Maybe α,β)) → Job β -- {{{
cache uuid computeJob = Cache uuid computeJob return
-- }}}
extractResultOrThrowIO :: Either JobError α → IO α -- {{{
extractResultOrThrowIO = either throwIO evaluate
-- }}}
once :: Typeable α ⇒ JobIdentifier → Job α → Job α -- {{{
once uuid job = Once uuid job return
-- }}}
onceAndCached :: (Binary α, Typeable β) ⇒ JobIdentifier → (Maybe α → Job (Maybe α,β)) → Job β -- {{{
onceAndCached uuid = once uuid . cache uuid
-- }}}
runJob :: Int → Map UUID L.ByteString → Job α → IO (Either JobError α,Map UUID L.ByteString) -- {{{
runJob maximum_number_of_simultaneous_IO_tasks input_cache job = do
    job_environment@JobEnvironment{..} ←
        newJobEnvironment
            maximum_number_of_simultaneous_IO_tasks
            input_cache
    result ← runJobInEnvironment job_environment Set.empty job
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
-- }}}
runJobInEnvironment :: JobEnvironment → Set JobIdentifier → Job α → IO (Either JobError α) -- {{{
runJobInEnvironment job_environment@JobEnvironment{..} active_jobs job =
    (case job of
        Result x → fmap Right (evaluate x)
        Task io computeNextJob → -- {{{
            bracket_
                (waitQSem environmentTaskSemaphore)
                (signalQSem environmentTaskSemaphore)
                (io >>= evaluate)
            >>=
            nestedRunJob . computeNextJob
        -- }}}
        Fork unsimplified_jf unsimplified_jx computeNextJob → do -- {{{
            let computeAndRunNextJob = nestedRunJob . computeNextJob
            jf_or_error ← simplify unsimplified_jf
            jx_or_error ← simplify unsimplified_jx
            case (jf_or_error,jx_or_error) of
                (Left fe, Left xe) → return . Left $ fe `mappend` xe
                (Left fe, Right jx) → do
                    x_or_error ← nestedRunJob jx
                    return . Left $
                        case x_or_error of
                            Left xe → (fe `mappend` xe)
                            Right _ → fe
                (Right jf, Left xe) → do
                    f_or_error ← nestedRunJob jf
                    return . Left $
                        case f_or_error of
                            Left fe → (xe `mappend` fe)
                            Right _ → xe
                (Right (Result f),Right (Result x)) → computeAndRunNextJob (f x)
                (Right (Result f),Right jx) → do
                    x_or_error ← nestedRunJob jx
                    case x_or_error of
                        Left e → return (Left e)
                        Right x → computeAndRunNextJob (f x)
                (Right jf,Right (Result x)) → do
                    f_or_error ← nestedRunJob jf
                    case f_or_error of
                        Left e → return (Left e)
                        Right f → computeAndRunNextJob (f x)
                (Right jf, Right jx) → do
                    x_mvar ← newEmptyMVar
                    forkIO $ nestedRunJob jx >>= putMVar x_mvar
                    f_or_error ← nestedRunJob jf >>= evaluate
                    x_or_error ← takeMVar x_mvar
                    case (f_or_error, x_or_error) of
                        (Right f, Right x) → computeAndRunNextJob (f x)
                        (Left e1, Left e2) → return . Left $ e1 `mappend` e2
                        (Left e, _) → return . Left $ e
                        (_, Left e) → return . Left $ e
        -- }}}
        Once job_id@(Identifier uuid description) job computeNextJob -- {{{
          | Set.member job_id active_jobs
            → throwIO CycleDetected
          | otherwise
            → do
            let addHeaderToErrors JobError{..} =
                    JobError
                    {   jobErrorsWithHeadings =
                            Map.insert
                                description
                                (map (toException . ErrorInDependency)
                                     (Map.keys jobErrorsWithHeadings)
                                 ++
                                 Map.elems jobErrors
                                )
                                jobErrorsWithHeadings
                    ,   jobErrors = Map.empty
                    }
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
                    result_or_error ← try $
                        runJobInEnvironment
                            job_environment
                            (Set.insert job_id active_jobs)
                            job
                        >>=
                        extractResultOrThrowIO
                        >>=
                        evaluate
                    case result_or_error of
                        Right result → do
                            IVar.write result_ivar (Right . toDyn $ result)
                            nestedRunJob (computeNextJob result)
                        Left exc →
                            let e = maybe
                                        (JobError (Map.singleton description [exc]) Map.empty)
                                        addHeaderToErrors
                                        (fromException exc)
                            in IVar.write result_ivar (Left e) >> return (Left e)
                Just (Left job_error) → return . Left . addHeaderToErrors $ job_error
                Just (Right previous_result) →
                    nestedRunJob
                    .
                    computeNextJob
                    .
                    fromJust
                    .
                    fromDynamic
                    $
                    previous_result
        -- }}}
        Cache job_id@(Identifier uuid _) computeJob computeNextJob → do -- {{{
            let maybe_old_cached_value =
                    fmap decode
                    .
                    Map.lookup uuid
                    $
                    environmentInputCache
            job_result ← nestedRunJob (computeJob maybe_old_cached_value)
            case job_result of
                Left e → return (Left e)
                Right (maybe_new_cached_value,job_result) → do
                    writeChan environmentOutputCache (uuid,fmap encode maybe_new_cached_value)
                    nestedRunJob (computeNextJob job_result)
        -- }}}
    ) `catches`
        [Handler (return . Left)
        ,Handler (return . Left . wrapExceptionIntoJobError)
        ]
  where
    nestedRunJob :: Job α → IO (Either JobError α)
    nestedRunJob = runJobInEnvironment job_environment active_jobs

    simplify :: Job α → IO (Either JobError (Job α))
    simplify job@(Once (Identifier uuid _) _ computeNextJob) = do
        completed_jobs ← readIORef environmentCompletedJobs
        case Map.lookup uuid completed_jobs of
            Just (Right result) → simplify . computeNextJob . fromJust . fromDynamic $ result
            Just (Left job_error) → return (Left job_error)
            _ → return (Right job)
    simplify job = return (Right job)
-- }}}
runJobUsingCacheFile :: Int → FilePath → Job α → IO (Either JobError α) -- {{{
runJobUsingCacheFile maximum_number_of_simultaneous_IO_tasks cache_filepath job = do
    cache ← do
        cache_exists ← doesFileExist cache_filepath
        if cache_exists
            then fmap (decode . L.fromChunks . (:[])) (S.readFile cache_filepath)
                 `catch`
                 (\(ErrorCall msg) → do
                    putStrLn $ "Encountered error " ++ msg ++ " while reading cache file " ++ cache_filepath
                    putStrLn $ "Since this is most likely due to a change in file format, I am going to delete this file."
                    removeFile cache_filepath
                    return Map.empty
                 )
            else return Map.empty
    (result,new_cache) ← runJob maximum_number_of_simultaneous_IO_tasks cache job
    encodeFile cache_filepath new_cache
    return result
-- }}}
runJobUsingCacheFileAndExtractResult :: Int → FilePath → Job α → IO α -- {{{
runJobUsingCacheFileAndExtractResult maximum_number_of_simultaneous_IO_tasks cache_filepath job =
    runJobUsingCacheFile
        maximum_number_of_simultaneous_IO_tasks
        cache_filepath
        job
    >>=
    extractResultOrThrowIO
-- }}}
newJobEnvironment :: Int → Map UUID L.ByteString → IO JobEnvironment -- {{{
newJobEnvironment maximum_number_of_simultaneous_IO_tasks input_cache =
    liftM4 JobEnvironment
        (newIORef Map.empty)
        (newQSem maximum_number_of_simultaneous_IO_tasks)
        (return input_cache)
        newChan
-- }}}
wrapExceptionIntoJobError e = JobError Map.empty (Map.singleton (show e) e)
-- }}}

