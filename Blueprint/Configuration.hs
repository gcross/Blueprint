-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1646:@thin Configuration.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1647:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1647:<< Language extensions >>
-- @nl

module Blueprint.Configuration where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1648:<< Import needed modules >>
import Data.Typeable
-- @-node:gcross.20100611224425.1648:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100611224425.1649:Types
-- @+node:gcross.20100611224425.1650:Script
newtype Script language = Script String deriving Typeable

data Fortran deriving Typeable

-- @-node:gcross.20100611224425.1650:Script
-- @-node:gcross.20100611224425.1649:Types
-- @+node:gcross.20100611224425.1651:Functions
-- @+node:gcross.20100611224425.1652:scriptFromLines
scriptFromLines :: [String] -> Script language
scriptFromLines = Script . unlines
-- @-node:gcross.20100611224425.1652:scriptFromLines
-- @-node:gcross.20100611224425.1651:Functions
-- @-others
-- @-node:gcross.20100611224425.1646:@thin Configuration.hs
-- @-leo
