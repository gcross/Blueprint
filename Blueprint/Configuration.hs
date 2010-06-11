-- @+leo-ver=4-thin
-- @+node:gcross.20100610134550.1490:@thin Configuration.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100610134550.1492:<< Language extensions >>
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100610134550.1492:<< Language extensions >>
-- @nl

module Blueprint.Configuration where

-- @<< Import needed modules >>
-- @+node:gcross.20100610134550.1494:<< Import needed modules >>
-- @-node:gcross.20100610134550.1494:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100610134550.1495:Types
-- @+node:gcross.20100610134550.1496:Script
newtype Script language = Script String

data Fortran
-- @-node:gcross.20100610134550.1496:Script
-- @-node:gcross.20100610134550.1495:Types
-- @+node:gcross.20100610134550.1497:Functions
-- @+node:gcross.20100610134550.1498:scriptFromLines
scriptFromLines :: [String] -> Script language
scriptFromLines = Script . unlines
-- @-node:gcross.20100610134550.1498:scriptFromLines
-- @-node:gcross.20100610134550.1497:Functions
-- @-others
-- @-node:gcross.20100610134550.1490:@thin Configuration.hs
-- @-leo
