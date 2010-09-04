-- @+leo-ver=4-thin
-- @+node:gcross.20100902094127.2076:@thin Wrapper.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100902094127.2077:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100902094127.2077:<< Language extensions >>
-- @nl

module Blueprint.Wrapper where

-- @<< Import needed modules >>
-- @+node:gcross.20100902094127.2078:<< Import needed modules >>
import Control.Monad

import Data.Dynamic
import Data.Maybe
import Data.Typeable

import Blueprint.Fields.Dynamic

import Blueprint.Record
-- @nonl
-- @-node:gcross.20100902094127.2078:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100902094127.2097:Classes
-- @+node:gcross.20100902094127.2098:Wrapper
class Wrapper wrapper α where
    wrap :: α → wrapper
    tryUnwrap :: wrapper → Maybe α
-- @-node:gcross.20100902094127.2098:Wrapper
-- @-node:gcross.20100902094127.2097:Classes
-- @+node:gcross.20100902094127.2101:Instances
-- @+node:gcross.20100902094127.2102:Wrapper Dynamic
instance Typeable α => Wrapper Dynamic α where
    wrap = toDyn
    tryUnwrap = fromDynamic
-- @-node:gcross.20100902094127.2102:Wrapper Dynamic
-- @+node:gcross.20100902094127.2104:Wrapper Record
instance (Typeable α, FieldValue entity Dynamic) => Wrapper (Table entity) α where
    wrap = withField _dynamic . toDyn
    tryUnwrap = getField _dynamic >=> fromDynamic
-- @-node:gcross.20100902094127.2104:Wrapper Record
-- @-node:gcross.20100902094127.2101:Instances
-- @+node:gcross.20100902094127.2099:Functions
-- @+node:gcross.20100902094127.2100:unwrap
unwrap :: Wrapper wrapper α => wrapper → α
unwrap = fromJust . tryUnwrap
-- @-node:gcross.20100902094127.2100:unwrap
-- @-node:gcross.20100902094127.2099:Functions
-- @-others
-- @-node:gcross.20100902094127.2076:@thin Wrapper.hs
-- @-leo
