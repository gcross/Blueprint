-- @+leo-ver=4-thin
-- @+node:gcross.20100831211145.2026:@thin Combinators.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100831211145.2027:<< Language extensions >>
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100831211145.2027:<< Language extensions >>
-- @nl

module Blueprint.Jobs.Combinators where

-- @<< Import needed modules >>
-- @+node:gcross.20100831211145.2028:<< Import needed modules >>
import Control.Applicative
import Control.Arrow
import Control.Category (Category)
import qualified Control.Category as Category

import Data.Binary (Binary)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Typeable

import Blueprint.Jobs
import Blueprint.Miscellaneous
import Blueprint.TaggedList (TaggedList(..))
import qualified Blueprint.TaggedList as T
import Blueprint.Wrapper
-- @-node:gcross.20100831211145.2028:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100831211145.2034:Types
-- @+node:gcross.20100831211145.2112:JobArrow
data JobArrow jobid result α β =
    NullJobArrow (α → β)
 |  ∀ n. JobArrow
    {   jobArrowIndependentJobs :: [Job jobid result]
    ,   jobArrowDependentJobs :: [IncompleteJob jobid result α]
    ,   jobArrowResultJobNames :: TaggedList n jobid
    ,   jobArrowResultExtractorJobName :: jobid → jobid
    ,   jobArrowResultExtractor :: α → TaggedList n result → β
    }
-- @-node:gcross.20100831211145.2112:JobArrow
-- @+node:gcross.20100831211145.2035:JobApplicative
data JobApplicative jobid result α =
    NullJobApplicative α
 |  ∀ n. JobApplicative
    {   jobApplicativeJobs :: [Job jobid result]
    ,   jobApplicativeResultJobNames :: TaggedList n jobid
    ,   jobApplicativeResultExtractorJobName :: jobid
    ,   jobApplicativeResultExtractor :: TaggedList n result → α
    }
-- @-node:gcross.20100831211145.2035:JobApplicative
-- @-node:gcross.20100831211145.2034:Types
-- @+node:gcross.20100831211145.2043:Instances
-- @+node:gcross.20100831211145.2044:Functor JobApplicative
instance Ord jobid => Functor (JobApplicative jobid result) where
    fmap f (NullJobApplicative x) = NullJobApplicative (f x)
    fmap f JobApplicative{..} = JobApplicative { jobApplicativeResultExtractor = f . jobApplicativeResultExtractor, .. }

    x <$ _ = NullJobApplicative x
-- @-node:gcross.20100831211145.2044:Functor JobApplicative
-- @+node:gcross.20100831211145.2045:Applicative JobApplicative
instance (Ord jobid, Monoid jobid) => Applicative (JobApplicative jobid result) where
    pure x = NullJobApplicative x
    (NullJobApplicative f) <*> (NullJobApplicative x) = NullJobApplicative (f x)
    (NullJobApplicative f) <*> JobApplicative{..} =
        JobApplicative
        {   jobApplicativeResultExtractor = f . jobApplicativeResultExtractor
        ,   ..
        }
    JobApplicative{..} <*> (NullJobApplicative x) =
        JobApplicative
        {   jobApplicativeResultExtractor = ($ x) . jobApplicativeResultExtractor
        ,   ..
        }
    (JobApplicative jobApplicativeJobs1 jobApplicativeResultJobNames1 jobApplicativeResultExtractorJobName1 jobApplicativeResultExtractor1) <*> (JobApplicative jobApplicativeJobs2 jobApplicativeResultJobNames2 jobApplicativeResultExtractorJobName2 jobApplicativeResultExtractor2)
        =
        let (joined_names,split) = T.join jobApplicativeResultJobNames1 jobApplicativeResultJobNames2
        in JobApplicative
            {   jobApplicativeJobs = jobApplicativeJobs1 ++ jobApplicativeJobs2
            ,   jobApplicativeResultJobNames = joined_names
            ,   jobApplicativeResultExtractorJobName = jobApplicativeResultExtractorJobName1 `mappend` jobApplicativeResultExtractorJobName2
            ,   jobApplicativeResultExtractor =
                    uncurry ($)
                    .
                    (jobApplicativeResultExtractor1 *** jobApplicativeResultExtractor2)
                    .
                    split
            }
    _ *> a = a
    a <* _ = a
-- @-node:gcross.20100831211145.2045:Applicative JobApplicative
-- @+node:gcross.20100831211145.2119:Functor JobArrow
instance Ord jobid => Functor (JobArrow jobid result α) where
    fmap f (NullJobArrow g) = NullJobArrow (f . g)
    fmap f JobArrow{..} = JobArrow { jobArrowResultExtractor = \x → f . jobArrowResultExtractor x, .. }

    x <$ _ = NullJobArrow (const x)
-- @-node:gcross.20100831211145.2119:Functor JobArrow
-- @+node:gcross.20100831211145.2120:Applicative JobArrow
instance (Ord jobid, Monoid jobid) => Applicative (JobArrow jobid result α) where
    pure x = NullJobArrow (const x)
    (NullJobArrow ff) <*> (NullJobArrow g) = NullJobArrow (\x → (ff x) (g x))
    (NullJobArrow ff) <*> JobArrow{..} =
        JobArrow
        {   jobArrowResultExtractor = \x → ff x . jobArrowResultExtractor x
        ,   ..
        }
    JobArrow{..} <*> (NullJobArrow f) =
        JobArrow
        {   jobArrowResultExtractor = \x → ($ f x) . jobArrowResultExtractor x
        ,   ..
        }
    (JobArrow jobArrowIndependentJobs1 jobArrowDependentJobs1 jobArrowResultJobNames1 jobArrowResultExtractorJobName1 jobArrowResultExtractor1) <*> (JobArrow jobArrowIndependentJobs2 jobArrowDependentJobs2 jobArrowResultJobNames2 jobArrowResultExtractorJobName2 jobArrowResultExtractor2) =
        let (joined_names,split) = T.join jobArrowResultJobNames1 jobArrowResultJobNames2
        in JobArrow
            {   jobArrowIndependentJobs = jobArrowIndependentJobs1 ++ jobArrowIndependentJobs2
            ,   jobArrowDependentJobs = jobArrowDependentJobs1 ++ jobArrowDependentJobs2
            ,   jobArrowResultJobNames = joined_names
            ,   jobArrowResultExtractorJobName =
                    liftA2 mappend
                        jobArrowResultExtractorJobName1
                        jobArrowResultExtractorJobName2
            ,   jobArrowResultExtractor = \x →
                    uncurry ($)
                    .
                    (jobArrowResultExtractor1 x *** jobArrowResultExtractor2 x)
                    .
                    split
            }
    _ *> a = a
    a <* _ = a
-- @-node:gcross.20100831211145.2120:Applicative JobArrow
-- @+node:gcross.20100831211145.2121:Category JobArrow
instance Ord jobid => Category (JobArrow jobid result) where
    id = NullJobArrow id
    (NullJobArrow f) . (NullJobArrow g) = NullJobArrow (f . g)
    JobArrow{..} . (NullJobArrow f) =
        JobArrow
        {   jobArrowDependentJobs = map (cofmap f) jobArrowDependentJobs
        ,   jobArrowResultExtractor = jobArrowResultExtractor . f
        ,   ..
        }
    (NullJobArrow f) . JobArrow{..} =
        JobArrow
        {   jobArrowResultExtractor = \x → f . jobArrowResultExtractor x
        ,   ..
        }
    (JobArrow jobArrowIndependentJobs2 jobArrowDependentJobs2 jobArrowResultJobNames2 jobArrowResultExtractorJobName2 jobArrowResultExtractor2) . (JobArrow jobArrowIndependentJobs1 jobArrowDependentJobs1 jobArrowResultJobNames1 jobArrowResultExtractorJobName1 jobArrowResultExtractor1)
        =
        JobArrow
        {   jobArrowIndependentJobs = jobArrowIndependentJobs1 ++ jobArrowIndependentJobs2
        ,   jobArrowDependentJobs = jobArrowDependentJobs1 ++ new_a2_dependent_jobs
        ,   jobArrowResultJobNames = joined_names
        ,   jobArrowResultExtractorJobName = jobArrowResultExtractorJobName2 . jobArrowResultExtractorJobName1
        ,   jobArrowResultExtractor = new_a2_job_extractor
        }
      where
        (joined_names,split) = T.join jobArrowResultJobNames1 jobArrowResultJobNames2
        new_a2_dependent_jobs =
            map (\(IncompleteJob job_names incomplete_job_runner) →
                IncompleteJob job_names $
                case incomplete_job_runner of
                    IncompleteJobRunner runJob → IncompleteJobRunner $ \x → do
                        results ← request jobArrowResultJobNames1
                        let y = jobArrowResultExtractor1 x results
                        runJob y
                    IncompleteJobRunnerWithCache runJob → IncompleteJobRunnerWithCache $ \x maybe_cache → do
                        results ← request jobArrowResultJobNames1
                        let y = jobArrowResultExtractor1 x results
                        runJob y maybe_cache
            ) jobArrowDependentJobs2
        new_a2_job_extractor x =
            uncurry jobArrowResultExtractor2
            .
            (first $ jobArrowResultExtractor1 x)
            .
            split
-- @-node:gcross.20100831211145.2121:Category JobArrow
-- @+node:gcross.20100831211145.2122:Arrow JobArrow
instance (Ord jobid, Monoid jobid) => Arrow (JobArrow jobid result) where
    arr f = NullJobArrow f
    first JobArrow{..} =
        JobArrow
        {   jobArrowDependentJobs = map (cofmap fst) jobArrowDependentJobs
        ,   jobArrowResultExtractor = \(x,y) results → (jobArrowResultExtractor x results,y)
        ,   ..
        }
    second JobArrow{..} =
        JobArrow
        {   jobArrowDependentJobs = map (cofmap snd) jobArrowDependentJobs
        ,   jobArrowResultExtractor = \(x,y) results → (x,jobArrowResultExtractor y results)
        ,   ..
        }

    (NullJobArrow f) &&& (NullJobArrow g) = NullJobArrow (f &&& g)
    (NullJobArrow f) &&& JobArrow{..} =
        JobArrow
        {   jobArrowResultExtractor = \x results → (f x,jobArrowResultExtractor x results)
        ,   ..
        }
    JobArrow{..} &&& (NullJobArrow g) =
        JobArrow
        {   jobArrowResultExtractor = \x results → (jobArrowResultExtractor x results, g x)
        ,   ..
        }
    (JobArrow jobArrowIndependentJobs1 jobArrowDependentJobs1 jobArrowResultJobNames1 jobArrowResultExtractorJobName1 jobArrowResultExtractor1) &&& (JobArrow jobArrowIndependentJobs2 jobArrowDependentJobs2 jobArrowResultJobNames2 jobArrowResultExtractorJobName2 jobArrowResultExtractor2) =
        let (joined_names,split) = T.join jobArrowResultJobNames1 jobArrowResultJobNames2
        in JobArrow
            {   jobArrowIndependentJobs = jobArrowIndependentJobs1 ++ jobArrowIndependentJobs2
            ,   jobArrowDependentJobs = jobArrowDependentJobs1 ++ jobArrowDependentJobs2
            ,   jobArrowResultJobNames = joined_names
            ,   jobArrowResultExtractorJobName = liftA2 mappend (jobArrowResultExtractorJobName1) (jobArrowResultExtractorJobName2)
            ,   jobArrowResultExtractor = \x →
                    (jobArrowResultExtractor1 x *** jobArrowResultExtractor2 x)
                    .
                    split
            }

    (NullJobArrow f) *** (NullJobArrow g) = NullJobArrow (f *** g)
    (NullJobArrow f) *** JobArrow{..} =
        JobArrow
        {   jobArrowDependentJobs = map (cofmap snd) jobArrowDependentJobs
        ,   jobArrowResultExtractor = \(x,y) results → (f x,jobArrowResultExtractor y results)
        ,   ..
        }
    JobArrow{..} *** (NullJobArrow g) =
        JobArrow
        {   jobArrowDependentJobs = map (cofmap fst) jobArrowDependentJobs
        ,   jobArrowResultExtractor = \(x,y) results → (jobArrowResultExtractor x results, g y)
        ,   ..
        }
    (JobArrow jobArrowIndependentJobs1 jobArrowDependentJobs1 jobArrowResultJobNames1 jobArrowResultExtractorJobName1 jobArrowResultExtractor1) *** (JobArrow jobArrowIndependentJobs2 jobArrowDependentJobs2 jobArrowResultJobNames2 jobArrowResultExtractorJobName2 jobArrowResultExtractor2) =
        let (joined_names,split) = T.join jobArrowResultJobNames1 jobArrowResultJobNames2
        in JobArrow
            {   jobArrowIndependentJobs = jobArrowIndependentJobs1 ++ jobArrowIndependentJobs2
            ,   jobArrowDependentJobs =
                    map (cofmap fst) (jobArrowDependentJobs1)
                    ++
                    map (cofmap snd) (jobArrowDependentJobs2)
            ,   jobArrowResultJobNames = joined_names
            ,   jobArrowResultExtractorJobName = liftA2 mappend (jobArrowResultExtractorJobName1) (jobArrowResultExtractorJobName2)
            ,   jobArrowResultExtractor = \(x,y) →
                    (jobArrowResultExtractor1 x *** jobArrowResultExtractor2 y)
                    .
                    split
            }
-- @nonl
-- @-node:gcross.20100831211145.2122:Arrow JobArrow
-- @-node:gcross.20100831211145.2043:Instances
-- @+node:gcross.20100831211145.2123:Functions
-- @+node:gcross.20100831211145.2124:(➤)
(➤) ::
    (Wrapper result α, Wrapper result β, Monoid jobid) =>
    JobApplicative jobid result α →
    JobArrow jobid result α β →
    JobApplicative jobid result β
(NullJobApplicative x) ➤ (NullJobArrow f) = NullJobApplicative (f x)
JobApplicative{..} ➤ (NullJobArrow f) =
    JobApplicative
    {   jobApplicativeResultExtractor = f . jobApplicativeResultExtractor
    ,   ..
    }
(NullJobApplicative x) ➤ JobArrow{..} =
    JobApplicative
    {   jobApplicativeJobs = jobArrowIndependentJobs ++ map (completeJobWith x) jobArrowDependentJobs
    ,   jobApplicativeResultJobNames = jobArrowResultJobNames
    ,   jobApplicativeResultExtractorJobName = jobArrowResultExtractorJobName mempty
    ,   jobApplicativeResultExtractor = jobArrowResultExtractor x
    }
JobApplicative{..} ➤ JobArrow{..} =
    JobApplicative
    {   jobApplicativeJobs =
            applicative_job
            :
            jobApplicativeJobs
            ++
            jobArrowIndependentJobs
            ++
            map (completeJobUsing (unwrap . T.head) (jobApplicativeResultExtractorJobName :. E))
                jobArrowDependentJobs
    ,   jobApplicativeResultJobNames = jobApplicativeResultExtractorJobName :. jobArrowResultJobNames
    ,   jobApplicativeResultExtractorJobName = jobArrowResultExtractorJobName jobApplicativeResultExtractorJobName
    ,   jobApplicativeResultExtractor = \z → case z of {(x_dyn :. results) → jobArrowResultExtractor (unwrap x_dyn) results}
    }
  where
    applicative_job =
        job jobApplicativeResultExtractorJobName $
            request jobApplicativeResultJobNames
            >>=
            returnValue . wrap . jobApplicativeResultExtractor
-- @-node:gcross.20100831211145.2124:(➤)
-- @+node:gcross.20100903104106.2078:(➠)
(➠) ::
    (Wrapper result α, Monoid jobid) =>
    JobApplicative jobid result α →
    [IncompleteJob jobid result α] →
    [Job jobid result]
(➠) (NullJobApplicative x) = map (completeJobWith x)
(➠) JobApplicative{..} =
    (applicative_job:)
    .
    (++ jobApplicativeJobs)
    .
    map (completeJobUsing (unwrap . T.head) (jobApplicativeResultExtractorJobName :. E))
  where
    applicative_job =
        job jobApplicativeResultExtractorJobName $
            request jobApplicativeResultJobNames
            >>=
            returnValue . wrap . jobApplicativeResultExtractor
-- @-node:gcross.20100903104106.2078:(➠)
-- @+node:gcross.20100901145855.2068:extractJobsFromJobApplicative
extractJobsFromJobApplicative ::
    (Monoid jobid, Wrapper result α) =>
    JobApplicative jobid result α →
    [Job jobid result]
extractJobsFromJobApplicative (NullJobApplicative _) = []
extractJobsFromJobApplicative JobApplicative{..} =
    job jobApplicativeResultExtractorJobName (
        request jobApplicativeResultJobNames
        >>=
        returnValue . wrap . jobApplicativeResultExtractor
     ):jobApplicativeJobs
-- @-node:gcross.20100901145855.2068:extractJobsFromJobApplicative
-- @+node:gcross.20100901145855.2067:runJobApplicative
runJobApplicative ::
    (Ord label
    ,Show label
    ,Typeable label
    ,Monoid label
    ,Wrapper result α
    ) =>
    Int →
    Cache label →
    JobApplicative label result α →
    IO α
runJobApplicative _ _ (NullJobApplicative value) = return value
runJobApplicative number_of_io_slaves cache job_applicative@JobApplicative{..} =
    withJobServer number_of_io_slaves cache $ do
        mapM_ submitJob (extractJobsFromJobApplicative job_applicative)
        fmap unwrap (requestJobResult jobApplicativeResultExtractorJobName)
-- @-node:gcross.20100901145855.2067:runJobApplicative
-- @+node:gcross.20100901145855.2071:runJobApplicativeUsingCacheFile
runJobApplicativeUsingCacheFile ::
    (Ord label
    ,Show label
    ,Typeable label
    ,Binary label
    ,Monoid label
    ,Wrapper result α
    ) =>
    Int →
    FilePath →
    JobApplicative label result α →
    IO α
runJobApplicativeUsingCacheFile _ _ (NullJobApplicative value) = return value
runJobApplicativeUsingCacheFile number_of_io_slaves path_to_cache job_applicative@JobApplicative{..} =
    withJobServerUsingCacheFile number_of_io_slaves path_to_cache $ do
        mapM_ submitJob (extractJobsFromJobApplicative job_applicative)
        fmap unwrap (requestJobResult jobApplicativeResultExtractorJobName)
-- @-node:gcross.20100901145855.2071:runJobApplicativeUsingCacheFile
-- @+node:gcross.20100902134026.2088:simpleJobApplicative
jobApplicativeFromJobTask :: Wrapper result α => jobid → jobid → JobTask jobid result α → JobApplicative jobid result α
jobApplicativeFromJobTask task_name extractor_name task =
    JobApplicative
    {   jobApplicativeJobs = [job task_name (task >>= returnWrappedValue)]
    ,   jobApplicativeResultJobNames = (task_name :. E)
    ,   jobApplicativeResultExtractorJobName = extractor_name
    ,   jobApplicativeResultExtractor = unwrap . T.head
    }
-- @-node:gcross.20100902134026.2088:simpleJobApplicative
-- @-node:gcross.20100831211145.2123:Functions
-- @-others
-- @-node:gcross.20100831211145.2026:@thin Combinators.hs
-- @-leo
