-- @+leo-ver=4-thin
-- @+node:gcross.20100604184944.1289:@thin Jobs.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100604184944.1291:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100604184944.1291:<< Language extensions >>
-- @nl

module Blueprint.Jobs where

-- @<< Import needed modules >>
-- @+node:gcross.20100604184944.1292:<< Import needed modules >>
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Data.IVar.Simple (IVar)
import qualified Data.IVar.Simple as IVar
import Data.Typeable
-- @-node:gcross.20100604184944.1292:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100604184944.1299:Exceptions
-- @+node:gcross.20100604184944.1300:JobFailed
data JobFailed = JobFailed String deriving (Show,Typeable)
instance Exception JobFailed
-- @-node:gcross.20100604184944.1300:JobFailed
-- @-node:gcross.20100604184944.1299:Exceptions
-- @+node:gcross.20100604184944.1293:Types
-- @+node:gcross.20100604184944.1294:JobStatus
data JobStatus result_type =
    Ready (JobTask result_type result_type)
 |  Running
 |  Succeeded result_type
-- @-node:gcross.20100604184944.1294:JobStatus
-- @+node:gcross.20100604184944.1295:JobTask
data JobTask r a =
    Request (r → JobTask r a)
  | forall b. PerformIO (IO b) (b → JobTask r a)
  | Return a
-- @-node:gcross.20100604184944.1295:JobTask
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
-- @-others
-- @-node:gcross.20100604184944.1289:@thin Jobs.hs
-- @-leo
