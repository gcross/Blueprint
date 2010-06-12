-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1588:@thin C.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1589:<< Language extensions >>
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1589:<< Language extensions >>
-- @nl

module Blueprint.Languages.C where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1590:<< Import needed modules >>
import Blueprint.Languages
-- @-node:gcross.20100611224425.1590:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100611224425.1625:Types
-- @+node:gcross.20100611224425.1626:C
data C
-- @nonl
-- @-node:gcross.20100611224425.1626:C
-- @-node:gcross.20100611224425.1625:Types
-- @+node:gcross.20100611224425.1627:Instances
-- @+node:gcross.20100611224425.1628:Language C
instance Language C where
    languageExtensions _ = ["c"]
-- @-node:gcross.20100611224425.1628:Language C
-- @-node:gcross.20100611224425.1627:Instances
-- @+node:gcross.20100611224425.1653:Values
-- @+node:gcross.20100611224425.1654:languageC
languageC = undefined :: C

-- @-node:gcross.20100611224425.1654:languageC
-- @-node:gcross.20100611224425.1653:Values
-- @-others
-- @-node:gcross.20100611224425.1588:@thin C.hs
-- @-leo
