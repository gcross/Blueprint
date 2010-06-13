-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1682:@thin Haskell.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1683:<< Language extensions >>
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1683:<< Language extensions >>
-- @nl

module Blueprint.Languages.Haskell
 where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1684:<< Import needed modules >>
import Data.Array ((!))
import Data.ByteString.Lazy.Char8 (unpack)

import Text.Regex.Base
import Text.Regex.PCRE
import Text.Regex.PCRE.String

import Data.Object
import Blueprint.Languages
-- @-node:gcross.20100611224425.1684:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100611224425.1685:Types
-- @+node:gcross.20100611224425.1686:Haskell
data Haskell
-- @-node:gcross.20100611224425.1686:Haskell
-- @-node:gcross.20100611224425.1685:Types
-- @+node:gcross.20100611224425.1687:Instances
-- @+node:gcross.20100611224425.1688:Language Haskell
instance Language Haskell where
    languageUUID _ = uuid "5fb30321-bfcd-488e-b798-6c000a22b47f"
    languageName _ = "Haskell"
    languageFileExtensions _ = ["hs"]
    languageDependencyExtractor _ =
        map (
            Dependency Nothing
            .
            (:[])
            .
            (++ ".hi")
            .
            dotsToPath
            .
            unpack
            .
            fst
            .
            (! 1)
        )
        .
        matchAllText import_regex
-- @-node:gcross.20100611224425.1688:Language Haskell
-- @-node:gcross.20100611224425.1687:Instances
-- @+node:gcross.20100611224425.1689:Values
-- @+node:gcross.20100611224425.1708:regular expression
import_regex :: Regex
import_regex = makeRegex "^\\s*import\\s+(?:qualified\\s+)?([A-Z][A-Za-z0-9_.]*)"
-- @-node:gcross.20100611224425.1708:regular expression
-- @+node:gcross.20100611224425.1690:languageHaskell
languageHaskell = undefined :: Haskell
-- @-node:gcross.20100611224425.1690:languageHaskell
-- @-node:gcross.20100611224425.1689:Values
-- @-others
-- @-node:gcross.20100611224425.1682:@thin Haskell.hs
-- @-leo
