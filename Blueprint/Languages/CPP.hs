-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1664:@thin CPP.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1665:<< Language extensions >>
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1665:<< Language extensions >>
-- @nl

module Blueprint.Languages.CPP where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1666:<< Import needed modules >>
import Data.Object
import Blueprint.Languages
-- @-node:gcross.20100611224425.1666:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100611224425.1667:Types
-- @+node:gcross.20100611224425.1668:CPP
data CPP
-- @nonl
-- @-node:gcross.20100611224425.1668:CPP
-- @-node:gcross.20100611224425.1667:Types
-- @+node:gcross.20100611224425.1669:Instances
-- @+node:gcross.20100611224425.1670:Language CPP
instance Language CPP where
    languageUUID _ = uuid "654515f6-e5ff-4888-9c58-dd1eae1e022e"
    languageName _ = "CPP"
    languageFileExtensions _ = ["cc","cpp","cxx"]
-- @-node:gcross.20100611224425.1670:Language CPP
-- @-node:gcross.20100611224425.1669:Instances
-- @+node:gcross.20100611224425.1671:Values
-- @+node:gcross.20100611224425.1672:languageCPP
languageCPP = undefined :: CPP

-- @-node:gcross.20100611224425.1672:languageCPP
-- @-node:gcross.20100611224425.1671:Values
-- @-others
-- @-node:gcross.20100611224425.1664:@thin CPP.hs
-- @-leo
