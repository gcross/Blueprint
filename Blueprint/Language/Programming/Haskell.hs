-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1682:@thin Haskell.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1683:<< Language extensions >>
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1683:<< Language extensions >>
-- @nl

module Blueprint.Language.Programming.Haskell where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1684:<< Import needed modules >>
import Data.Array ((!))
import Data.ByteString.Lazy.Char8 (unpack)

import Text.Regex.Base
import Text.Regex.PCRE
import Text.Regex.PCRE.String

import Data.Object
import Blueprint.Language
import Blueprint.Language.Programming
-- @-node:gcross.20100611224425.1684:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100615082419.1704:Languages
-- @+node:gcross.20100615082419.1707:Haskell
data Haskell

instance Language Haskell where
    languageUUID _ = uuid "5fb30321-bfcd-488e-b798-6c000a22b47f"
    languageName _ = "Haskell"
    languageFileExtensions _ = ["hs"]
    languageDependencyExtractor _ = extractDependencies

instance ProgrammingLanguage Haskell where
    languageHelloWorldScript _ = scriptFromLines $ ["main = putStrLn \"Hello, world!\""]
-- @nonl
-- @-node:gcross.20100615082419.1707:Haskell
-- @-node:gcross.20100615082419.1704:Languages
-- @+node:gcross.20100615082419.1705:Functions
-- @+node:gcross.20100615082419.1706:extractDependencies
extractDependencies =
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
-- @-node:gcross.20100615082419.1706:extractDependencies
-- @-node:gcross.20100615082419.1705:Functions
-- @+node:gcross.20100611224425.1689:Values
-- @+node:gcross.20100611224425.1708:regular expression
import_regex :: Regex
import_regex = makeRegex "^\\s*import\\s+(?:qualified\\s+)?([A-Z][A-Za-z0-9_.]*)"
-- @-node:gcross.20100611224425.1708:regular expression
-- @-node:gcross.20100611224425.1689:Values
-- @-others
-- @-node:gcross.20100611224425.1682:@thin Haskell.hs
-- @-leo
