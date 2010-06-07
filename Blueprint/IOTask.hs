-- @+leo-ver=4-thin
-- @+node:gcross.20100604204549.7680:@thin IOTask.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100604204549.7681:<< Language extensions >>
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100604204549.7681:<< Language extensions >>
-- @nl

module Blueprint.IOTask where

-- @<< Import needed modules >>
-- @+node:gcross.20100604204549.7682:<< Import needed modules >>
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
-- @nonl
-- @-node:gcross.20100604204549.7682:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100604204549.7683:Types
-- @+node:gcross.20100604204549.7685:IOTask
data IOTask a = forall r. IOTask (IO r) (Either SomeException r → a)
-- @-node:gcross.20100604204549.7685:IOTask
-- @-node:gcross.20100604204549.7683:Types
-- @+node:gcross.20100604204549.7686:Functions
-- @+node:gcross.20100604204549.7688:spawnIOTaskRunner
spawnIOTaskRunner :: Chan (IOTask return) → Chan return → IO ThreadId
spawnIOTaskRunner task_queue result_queue =
    forkIO . forever $
        readChan task_queue
        >>=
        \(IOTask task f) → (try task >>= writeChan result_queue . f)
-- @-node:gcross.20100604204549.7688:spawnIOTaskRunner
-- @-node:gcross.20100604204549.7686:Functions
-- @-others
-- @-node:gcross.20100604204549.7680:@thin IOTask.hs
-- @-leo
