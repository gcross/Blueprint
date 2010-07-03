-- @+leo-ver=4-thin
-- @+node:gcross.20100624100717.1785:@thin Goto.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100624100717.1786:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100624100717.1786:<< Language extensions >>
-- @nl

module Control.Monad.Trans.Goto where

-- @<< Import needed modules >>
-- @+node:gcross.20100624100717.1787:<< Import needed modules >>
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Functor
import Data.Functor.Identity
-- @-node:gcross.20100624100717.1787:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100624100717.1788:Types
-- @+node:gcross.20100624100717.1793:GotoT
newtype GotoT r m a = GotoT { unwrapGotoT :: m (Either (GotoT r m r) a) }
-- @-node:gcross.20100624100717.1793:GotoT
-- @+node:gcross.20100624100717.1798:Goto
type Goto r = GotoT r Identity
-- @-node:gcross.20100624100717.1798:Goto
-- @-node:gcross.20100624100717.1788:Types
-- @+node:gcross.20100624100717.1790:Instances
-- @+node:gcross.20100624100717.1804:Applicative (GotoT r m)
instance Applicative m ⇒ Applicative (GotoT r m) where
    pure = GotoT . fmap Right . pure
    (GotoT m) <*> (GotoT x) = GotoT ((fmap h m) <*> x)
      where
        h (Left g) = const (Left g)
        h (Right f) = either Left (Right . f)
-- @-node:gcross.20100624100717.1804:Applicative (GotoT r m)
-- @+node:gcross.20100624100717.1791:Functor (GotoT r m)
instance Functor m ⇒ Functor (GotoT r m) where
    fmap f = GotoT . fmap (either Left (Right . f)) . unwrapGotoT
-- @-node:gcross.20100624100717.1791:Functor (GotoT r m)
-- @+node:gcross.20100624100717.1792:Monad (GotoT r m)
instance Monad m ⇒ Monad (GotoT r m) where
    return = GotoT . return . Right

    (GotoT m) >>= f = GotoT $ m >>= either (return . Left) (unwrapGotoT . f)
-- @-node:gcross.20100624100717.1792:Monad (GotoT r m)
-- @+node:gcross.20100624100717.1801:MonadIO (GotoT r m)
instance MonadIO m ⇒ MonadIO (GotoT r m) where
    liftIO = lift . liftIO
-- @-node:gcross.20100624100717.1801:MonadIO (GotoT r m)
-- @+node:gcross.20100624100717.1803:MonadTrans (GotoT r)
instance MonadTrans (GotoT r) where
    lift = GotoT . (>>= return . Right)
-- @-node:gcross.20100624100717.1803:MonadTrans (GotoT r)
-- @-node:gcross.20100624100717.1790:Instances
-- @+node:gcross.20100624100717.1796:Functions
-- @+node:gcross.20100624100717.1800:goto
goto :: Monad m ⇒ GotoT r m r → GotoT r m a
goto = GotoT . return . Left
-- @-node:gcross.20100624100717.1800:goto
-- @+node:gcross.20100624100717.1799:runGoto
runGoto = runIdentity . runGotoT
-- @-node:gcross.20100624100717.1799:runGoto
-- @+node:gcross.20100624100717.1797:runGotoT
runGotoT :: Monad m ⇒ GotoT r m r → m r
runGotoT (GotoT m) = m >>= either runGotoT return
-- @-node:gcross.20100624100717.1797:runGotoT
-- @-node:gcross.20100624100717.1796:Functions
-- @-others
-- @-node:gcross.20100624100717.1785:@thin Goto.hs
-- @-leo
