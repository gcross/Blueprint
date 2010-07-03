-- @+leo-ver=4-thin
-- @+node:gcross.20100630111926.1935:@thin Abort.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100630111926.1936:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100630111926.1936:<< Language extensions >>
-- @nl

module Control.Monad.Trans.Abort where

-- @<< Import needed modules >>
-- @+node:gcross.20100630111926.1937:<< Import needed modules >>
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Functor
import Data.Functor.Identity
-- @-node:gcross.20100630111926.1937:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100630111926.1938:Types
-- @+node:gcross.20100630111926.1939:AbortT
newtype AbortT r m a = AbortT { unwrapAbortT :: m (Either r a) }
-- @-node:gcross.20100630111926.1939:AbortT
-- @+node:gcross.20100630111926.1940:Abort
type Abort r = AbortT r Identity
-- @nonl
-- @-node:gcross.20100630111926.1940:Abort
-- @-node:gcross.20100630111926.1938:Types
-- @+node:gcross.20100630111926.1941:Instances
-- @+node:gcross.20100630111926.1942:Applicative (AbortT r m)
instance Applicative m ⇒ Applicative (AbortT r m) where
    pure = AbortT . fmap Right . pure
    (AbortT m) <*> (AbortT x) = AbortT ((fmap h m) <*> x)
      where
        h (Left g) = const (Left g)
        h (Right f) = either Left (Right . f)
-- @nonl
-- @-node:gcross.20100630111926.1942:Applicative (AbortT r m)
-- @+node:gcross.20100630111926.1943:Functor (AbortT r m)
instance Functor m ⇒ Functor (AbortT r m) where
    fmap f = AbortT . fmap (either Left (Right . f)) . unwrapAbortT
-- @nonl
-- @-node:gcross.20100630111926.1943:Functor (AbortT r m)
-- @+node:gcross.20100630111926.1944:Monad (AbortT r m)
instance Monad m ⇒ Monad (AbortT r m) where
    return = AbortT . return . Right

    (AbortT m) >>= f = AbortT $ m >>= either (return . Left) (unwrapAbortT . f)
-- @nonl
-- @-node:gcross.20100630111926.1944:Monad (AbortT r m)
-- @+node:gcross.20100630111926.1945:MonadIO (AbortT r m)
instance MonadIO m ⇒ MonadIO (AbortT r m) where
    liftIO = lift . liftIO
-- @nonl
-- @-node:gcross.20100630111926.1945:MonadIO (AbortT r m)
-- @+node:gcross.20100630111926.1946:MonadTrans (AbortT r)
instance MonadTrans (AbortT r) where
    lift = AbortT . (>>= return . Right)
-- @nonl
-- @-node:gcross.20100630111926.1946:MonadTrans (AbortT r)
-- @-node:gcross.20100630111926.1941:Instances
-- @+node:gcross.20100630111926.1947:Functions
-- @+node:gcross.20100630111926.1948:abortWith
abort :: Monad m ⇒ r → AbortT r m a
abort = AbortT . return . Left
-- @-node:gcross.20100630111926.1948:abortWith
-- @+node:gcross.20100630111926.1949:runAbort
runAbort = runIdentity . runAbortT
-- @nonl
-- @-node:gcross.20100630111926.1949:runAbort
-- @+node:gcross.20100630111926.1950:runAbortT
runAbortT :: Monad m ⇒ AbortT r m r → m r
runAbortT (AbortT m) = m >>= either return return
-- @nonl
-- @-node:gcross.20100630111926.1950:runAbortT
-- @-node:gcross.20100630111926.1947:Functions
-- @-others
-- @-node:gcross.20100630111926.1935:@thin Abort.hs
-- @-leo
