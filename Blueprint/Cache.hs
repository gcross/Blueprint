-- @+leo-ver=4-thin
-- @+node:gcross.20100925114718.1301:@thin Cache.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100925114718.1302:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100925114718.1302:<< Language extensions >>
-- @nl

module Blueprint.Cache where

-- @<< Import needed modules >>
-- @+node:gcross.20100925114718.1303:<< Import needed modules >>
import Data.Binary
import Data.UUID

import Blueprint.Job
-- @-node:gcross.20100925114718.1303:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100925114718.1304:Functions
-- @+node:gcross.20100925114718.1310:runUnlessUnchanged
runUnlessUnchanged :: (Binary α, Eq α, Binary β) ⇒ UUID → α → Job β → Job β
runUnlessUnchanged uuid current_value job =
    cache uuid $ \maybe_old_value →
        case maybe_old_value of
            Just (old_value,cached_job_result) | old_value == current_value →
                return (Just (current_value,cached_job_result),cached_job_result)
            _ → do
                job_result ← job
                return (Just (current_value,job_result),job_result)
-- @-node:gcross.20100925114718.1310:runUnlessUnchanged
-- @-node:gcross.20100925114718.1304:Functions
-- @-others
-- @-node:gcross.20100925114718.1301:@thin Cache.hs
-- @-leo
