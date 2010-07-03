-- @+leo-ver=4-thin
-- @+node:gcross.20100630111926.1935:@thin Exit.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100630111926.1936:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100630111926.1936:<< Language extensions >>
-- @nl

module Control.Monad.Exit where

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
-- @+node:gcross.20100630111926.1939:ExitT
newtype ExitT r m a = ExitT { unwrapExitT :: m (Either r a) }
-- @-node:gcross.20100630111926.1939:ExitT
-- @+node:gcross.20100630111926.1940:Exit
type Exit r = ExitT r Identity
-- @-node:gcross.20100630111926.1940:Exit
-- @-node:gcross.20100630111926.1938:Types
-- @+node:gcross.20100630111926.1941:Instances
-- @+node:gcross.20100630111926.1942:Applicative (ExitT r m)
instance Applicative m ⇒ Applicative (ExitT r m) where
    pure = ExitT . fmap Right . pure
    (ExitT m) <*> (ExitT x) = ExitT ((fmap h m) <*> x)
      where
        h (Left g) = const (Left g)
        h (Right f) = either Left (Right . f)
-- @-node:gcross.20100630111926.1942:Applicative (ExitT r m)
-- @+node:gcross.20100630111926.1943:Functor (ExitT r m)
instance Functor m ⇒ Functor (ExitT r m) where
    fmap f = ExitT . fmap (either Left (Right . f)) . unwrapExitT
-- @-node:gcross.20100630111926.1943:Functor (ExitT r m)
-- @+node:gcross.20100630111926.1944:Monad (ExitT r m)
instance Monad m ⇒ Monad (ExitT r m) where
    return = ExitT . return . Right

    (ExitT m) >>= f = ExitT $ m >>= either (return . Left) (unwrapExitT . f)
-- @-node:gcross.20100630111926.1944:Monad (ExitT r m)
-- @+node:gcross.20100630111926.1945:MonadIO (ExitT r m)
instance MonadIO m ⇒ MonadIO (ExitT r m) where
    liftIO = lift . liftIO
-- @-node:gcross.20100630111926.1945:MonadIO (ExitT r m)
-- @+node:gcross.20100630111926.1946:MonadTrans (ExitT r)
instance MonadTrans (ExitT r) where
    lift = ExitT . (>>= return . Right)
-- @-node:gcross.20100630111926.1946:MonadTrans (ExitT r)
-- @-node:gcross.20100630111926.1941:Instances
-- @+node:gcross.20100630111926.1947:Functions
-- @+node:gcross.20100630111926.1948:exitWith
exitWith :: Monad m ⇒ r → ExitT r m a
exitWith = ExitT . return . Left
-- @-node:gcross.20100630111926.1948:exitWith
-- @+node:gcross.20100630111926.1949:runExit
runExit = runIdentity . runExitT
-- @-node:gcross.20100630111926.1949:runExit
-- @+node:gcross.20100630111926.1950:runExitT
runExitT :: Monad m ⇒ ExitT r m r → m r
runExitT (ExitT m) = m >>= either return return
-- @-node:gcross.20100630111926.1950:runExitT
-- @-node:gcross.20100630111926.1947:Functions
-- @-others
-- @-node:gcross.20100630111926.1935:@thin Exit.hs
-- @-leo
