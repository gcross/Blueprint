-- @+leo-ver=4-thin
-- @+node:gcross.20100924160650.2044:@thin Job.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100924160650.2045:<< Language extensions >>
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100924160650.2045:<< Language extensions >>
-- @nl

module Blueprint.Job where

-- @<< Import needed modules >>
-- @+node:gcross.20100924160650.2046:<< Import needed modules >>
import Control.Applicative
import Control.Exception
import Control.Monad

import Data.Binary
import Data.Map (Map)
import qualified Data.Map as Map
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
    Cached :: Typeable α ⇒ UUID → Job α → (α → Job β) → Job β
    Saved :: Binary α ⇒ UUID → α → (α → Job α) → (α → Job β) → Job β
-- @-node:gcross.20100924160650.2048:Job
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
    fmap f (Cached uuid jx g) = Cached uuid jx (fmap f . g)
    fmap f (Saved uuid jx g h) = Saved uuid jx g (fmap f . h)
-- @-node:gcross.20100924174906.1281:Functor Job
-- @+node:gcross.20100924174906.1277:Monad Job
instance Monad Job where
    return = Result
    Result x >>= f = f x
    Task io f >>= g = Task io (f >=> g)
    Fork jf jx f >>= g = Fork jf jx (f >=> g)
    Errors errors >>= _ = Errors errors
    Cached uuid x f >>= g = Cached uuid x (f >=> g)
    Saved uuid x f g >>= h = Saved uuid x f (g >=> h)
-- @-node:gcross.20100924174906.1277:Monad Job
-- @-node:gcross.20100924174906.1276:Instances
-- @-others
-- @-node:gcross.20100924160650.2044:@thin Job.hs
-- @-leo
