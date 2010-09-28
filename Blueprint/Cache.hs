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
-- @+node:gcross.20100925114718.1310:runAndFlagUnlessUnchanged
runAndFlagUnlessUnchanged :: (Binary α, Eq α, Binary β) ⇒ UUID → α → Job β → Job (β,Bool)
runAndFlagUnlessUnchanged uuid current_value job =
    cache uuid $ \maybe_old_value →
        case maybe_old_value of
            Just (old_value,cached_job_result) | old_value == current_value →
                return (Just (current_value,cached_job_result),(cached_job_result,False))
            _ → do
                job_result ← job
                return (Just (current_value,job_result),(job_result,True))
-- @-node:gcross.20100925114718.1310:runAndFlagUnlessUnchanged
-- @+node:gcross.20100927222551.1457:runUnlessUnchanged
runUnlessUnchanged :: (Binary α, Eq α, Binary β) ⇒ UUID → α → Job β → Job β
runUnlessUnchanged uuid current_value job = fmap fst (runAndFlagUnlessUnchanged uuid current_value job)
-- @-node:gcross.20100927222551.1457:runUnlessUnchanged
-- @+node:gcross.20100927222551.1470:storeInCache
storeInCache :: (Binary α, Eq α) ⇒ UUID → α → Job ()
storeInCache uuid value = cache uuid . const . return $ (Just value,())
-- @-node:gcross.20100927222551.1470:storeInCache
-- @+node:gcross.20100927222551.1465:updateAndFlagUnlessUnchanged
updateAndFlagUnlessUnchanged :: (Binary α, Eq α) ⇒ UUID → α → Job α → Job (α,Bool)
updateAndFlagUnlessUnchanged uuid current_value job =
    cache uuid $ \maybe_old_value →
        case maybe_old_value of
            Just old_value | old_value == current_value →
                return (Just old_value,(old_value,False))
            _ → do
                new_value ← job
                return (Just new_value,(new_value,True))
-- @-node:gcross.20100927222551.1465:updateAndFlagUnlessUnchanged
-- @+node:gcross.20100927222551.1469:updateUnlessUnchanged
updateUnlessUnchanged :: (Binary α, Eq α) ⇒ UUID → α → Job α → Job α
updateUnlessUnchanged uuid current_value job = fmap fst (updateAndFlagUnlessUnchanged uuid current_value job)
-- @-node:gcross.20100927222551.1469:updateUnlessUnchanged
-- @-node:gcross.20100925114718.1304:Functions
-- @-others
-- @-node:gcross.20100925114718.1301:@thin Cache.hs
-- @-leo
