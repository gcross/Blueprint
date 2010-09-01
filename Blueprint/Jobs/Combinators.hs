-- @+leo-ver=4-thin
-- @+node:gcross.20100831211145.2026:@thin Combinators.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100831211145.2027:<< Language extensions >>
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

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable

import Blueprint.Jobs
-- @-node:gcross.20100831211145.2028:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100831211145.2034:Types
-- @+node:gcross.20100831211145.2112:JobArrow
data JobArrow jobid result α β =
    NullJobArrow (α → β)
 |  JobArrow
    {   jobArrowIndependentJobs :: Map [jobid] (JobRunner jobid result)
    ,   jobArrowDependentJobs :: Map [jobid] (IncompleteJobRunner jobid result α)
    ,   jobArrowResultJobNames :: [jobid]
    ,   jobArrowResultExtractor :: α → [result] → β
    }
-- @-node:gcross.20100831211145.2112:JobArrow
-- @+node:gcross.20100831211145.2035:JobApplicative
data JobApplicative jobid result α =
    NullJobApplicative α
 |  JobApplicative
    {   jobApplicativeJobs :: [Job jobid result]
    ,   jobApplicativeResultJobNames :: [jobid]
    ,   jobApplicativeResultExtractor :: [result] → α
    }
-- @nonl
-- @-node:gcross.20100831211145.2035:JobApplicative
-- @-node:gcross.20100831211145.2034:Types
-- @+node:gcross.20100831211145.2043:Instances
-- @+node:gcross.20100831211145.2044:Functor JobApplicative
instance Ord jobid => Functor (JobApplicative jobid result) where
    fmap f (NullJobApplicative x) = NullJobApplicative (f x)
    fmap f a@JobApplicative{..} = a { jobApplicativeResultExtractor = f . jobApplicativeResultExtractor }

    x <$ _ = NullJobApplicative x
-- @nonl
-- @-node:gcross.20100831211145.2044:Functor JobApplicative
-- @+node:gcross.20100831211145.2045:Applicative JobApplicative
instance Ord jobid => Applicative (JobApplicative jobid result) where
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
    a1 <*> a2 =
        JobApplicative
        {   jobApplicativeJobs = jobApplicativeJobs a1 ++ jobApplicativeJobs a2
        ,   jobApplicativeResultJobNames = jobApplicativeResultJobNames a1 ++ jobApplicativeResultJobNames a2
        ,   jobApplicativeResultExtractor =
                uncurry ($)
                .
                (jobApplicativeResultExtractor a1 *** jobApplicativeResultExtractor a2)
                .
                splitAt (length . jobApplicativeResultJobNames $ a1)
        }
    _ *> a = a
    a <* _ = a
-- @nonl
-- @-node:gcross.20100831211145.2045:Applicative JobApplicative
-- @+node:gcross.20100831211145.2119:Functor JobArrow
instance Ord jobid => Functor (JobArrow jobid result α) where
    fmap f (NullJobArrow g) = NullJobArrow (f . g)
    fmap f a@JobArrow{..} = a { jobArrowResultExtractor = \x → f . jobArrowResultExtractor x}

    x <$ _ = NullJobArrow (const x)
-- @-node:gcross.20100831211145.2119:Functor JobArrow
-- @+node:gcross.20100831211145.2120:Applicative JobArrow
instance Ord jobid => Applicative (JobArrow jobid result α) where
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
    a1 <*> a2 =
        JobArrow
        {   jobArrowIndependentJobs = jobArrowIndependentJobs a1 `Map.union` jobArrowIndependentJobs a2
        ,   jobArrowDependentJobs = jobArrowDependentJobs a1 `Map.union` jobArrowDependentJobs a2
        ,   jobArrowResultJobNames = jobArrowResultJobNames a1 ++ jobArrowResultJobNames a2
        ,   jobArrowResultExtractor = \x →
                uncurry ($)
                .
                (jobArrowResultExtractor a1 x *** jobArrowResultExtractor a2 x)
                .
                splitAt (length . jobArrowResultJobNames $ a1)
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
        {   jobArrowDependentJobs =
                Map.map (
                    \incomplete_runner →
                        case incomplete_runner of 
                            IncompleteJobRunner g → IncompleteJobRunner (g . f)
                            IncompleteJobRunnerWithCache g → IncompleteJobRunnerWithCache (g . f)
                ) jobArrowDependentJobs
        ,   jobArrowResultExtractor = jobArrowResultExtractor . f
        ,   ..
        }
    (NullJobArrow f) . JobArrow{..} =
        JobArrow
        {   jobArrowResultExtractor = \x → f . jobArrowResultExtractor x
        ,   ..
        }
    a2 . a1 =
        JobArrow
        {   jobArrowIndependentJobs = jobArrowIndependentJobs a1 `Map.union` jobArrowIndependentJobs a2
        ,   jobArrowDependentJobs = jobArrowDependentJobs a1 `Map.union` new_a2_dependent_jobs
        ,   jobArrowResultJobNames = jobArrowResultJobNames a2
        ,   jobArrowResultExtractor = new_a2_job_extractor
        }
      where
        new_a2_dependent_jobs =
            Map.map (\incomplete_job_runner →
                case incomplete_job_runner of
                    IncompleteJobRunner runJob → IncompleteJobRunner $ \x → do
                        results ← request (jobArrowResultJobNames a1)
                        let y = jobArrowResultExtractor a1 x results
                        runJob y
                    IncompleteJobRunnerWithCache runJob → IncompleteJobRunnerWithCache $ \x maybe_cache → do
                        results ← request (jobArrowResultJobNames a1)
                        let y = jobArrowResultExtractor a1 x results
                        runJob y maybe_cache
            ) (jobArrowDependentJobs a2)
        new_a2_job_extractor x =
            uncurry (jobArrowResultExtractor a2)
            .
            (first (jobArrowResultExtractor a1 x))
            .
            splitAt (length . jobArrowResultJobNames $ a1)
-- @-node:gcross.20100831211145.2121:Category JobArrow
-- @+node:gcross.20100831211145.2122:Arrow JobArrow
instance Ord jobid => Arrow (JobArrow jobid result) where
    arr f = NullJobArrow f
    first JobArrow{..} =
        JobArrow
        {   jobArrowDependentJobs =
                Map.map (
                    \incomplete_runner →
                        case incomplete_runner of 
                            IncompleteJobRunner g → IncompleteJobRunner (g . fst)
                            IncompleteJobRunnerWithCache g → IncompleteJobRunnerWithCache (g . fst)
                ) jobArrowDependentJobs
        ,   jobArrowResultExtractor = \(x,y) results → (jobArrowResultExtractor x results,y)
        ,   ..
        }
    second JobArrow{..} =
        JobArrow
        {   jobArrowDependentJobs =
                Map.map (
                    \incomplete_runner →
                        case incomplete_runner of 
                            IncompleteJobRunner g → IncompleteJobRunner (g . snd)
                            IncompleteJobRunnerWithCache g → IncompleteJobRunnerWithCache (g . snd)
                ) jobArrowDependentJobs
        ,   jobArrowResultExtractor = \(x,y) results → (x,jobArrowResultExtractor y results)
        ,   ..
        }
-- @-node:gcross.20100831211145.2122:Arrow JobArrow
-- @-node:gcross.20100831211145.2043:Instances
-- @-others
-- @-node:gcross.20100831211145.2026:@thin Combinators.hs
-- @-leo
