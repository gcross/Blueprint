-- @+leo-ver=4-thin
-- @+node:gcross.20091127142612.1383:@thin Error.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091127142612.1398:<< Language extensions >>
-- @-node:gcross.20091127142612.1398:<< Language extensions >>
-- @nl

module Blueprint.Error where

-- @<< Import needed modules >>
-- @+node:gcross.20091127142612.1384:<< Import needed modules >>
import Control.Applicative
import Control.Applicative.Infix
import Control.Monad
import Control.Monad.Error

import Data.Either
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
-- @-node:gcross.20091127142612.1384:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091127142612.1391:Types
-- @+node:gcross.20091127142612.1393:ErrorMessage
type ErrorMessage = Map String String
-- @-node:gcross.20091127142612.1393:ErrorMessage
-- @-node:gcross.20091127142612.1391:Types
-- @+node:gcross.20091127142612.1385:Instances
-- @+node:gcross.20091127142612.1386:Applicative (Either e a)
instance (Monoid e) => Applicative (Either e) where
    pure = Right
    (<*>) (Left error2) (Left error1) = Left (error1 `mappend` error2)
    (<*>) (Left error) _ = Left error
    (<*>) _ (Left error) = Left error
    (<*>) (Right function) (Right argument) = Right (function argument)
-- @-node:gcross.20091127142612.1386:Applicative (Either e a)
-- @+node:gcross.20091127142612.1388:Applicative (ErrorT e m a)
instance (Monoid e, Error e, Monad m) => Applicative (ErrorT e m) where
    pure = return
    e_fn <*> e_arg = ErrorT $ liftM2 (<*>) (runErrorT e_fn) (runErrorT e_arg)
-- @-node:gcross.20091127142612.1388:Applicative (ErrorT e m a)
-- @-node:gcross.20091127142612.1385:Instances
-- @+node:gcross.20091127142612.1396:Functions
-- @+node:gcross.20091127142612.1397:extractResultsOrError
extractResultsOrError :: Monoid e => [Either e a] -> Either e [a]
extractResultsOrError = either (Left . mconcat) Right . extractResultsOrErrors
-- @-node:gcross.20091127142612.1397:extractResultsOrError
-- @+node:gcross.20091127142612.1400:extractResultsOrErrors
extractResultsOrErrors :: [Either e a] -> Either [e] [a]
extractResultsOrErrors eithers =
    case partitionEithers (eithers) of
        ([],results) -> Right results
        (errors,_) -> Left errors
-- @-node:gcross.20091127142612.1400:extractResultsOrErrors
-- @-node:gcross.20091127142612.1396:Functions
-- @-others
-- @-node:gcross.20091127142612.1383:@thin Error.hs
-- @-leo
