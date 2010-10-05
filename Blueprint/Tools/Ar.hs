-- @+leo-ver=4-thin
-- @+node:gcross.20101005114926.1468:@thin Ar.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20101005114926.1469:<< Language extensions >>
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20101005114926.1469:<< Language extensions >>
-- @nl

module Blueprint.Tools.Ar where

-- @<< Import needed modules >>
-- @+node:gcross.20101005114926.1470:<< Import needed modules >>
import Data.Typeable

import Blueprint.Configuration
-- @-node:gcross.20101005114926.1470:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20101005114926.1472:Program
data Ar deriving Typeable; instance ProgramName Ar where { programNameFrom _ = "ar" }
-- @-node:gcross.20101005114926.1472:Program
-- @-others
-- @-node:gcross.20101005114926.1468:@thin Ar.hs
-- @-leo
