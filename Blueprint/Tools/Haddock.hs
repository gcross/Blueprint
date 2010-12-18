-- @+leo-ver=5-thin
-- @+node:gcross.20101217170933.1576: * @thin Haddock.hs
-- @@language Haskell
-- @+<< Language extensions >>
-- @+node:gcross.20101217170933.1577: ** << Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
-- @-<< Language extensions >>

module Blueprint.Tools.Haddock where

-- @+<< Import needed modules >>
-- @+node:gcross.20101217170933.1578: ** << Import needed modules >>
import Data.Typeable

import Blueprint.Configuration
import Blueprint.Options
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20101217170933.1580: ** Programs
data Haddock deriving Typeable; instance ProgramName Haddock where { programNameFrom _ = "haddock" }
-- @+node:gcross.20101217170933.1582: ** Options
haddock_options = unwrapOptions (programOptions :: OptionsFor Haddock)
-- @-others
-- @-leo
