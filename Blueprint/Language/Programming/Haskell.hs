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
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lazy.Char8 (unpack)

import Text.Regex.PCRE
import Text.Regex.PCRE.String

import Data.Record
import Blueprint.Dependency
import Blueprint.Identifier
import Blueprint.Language
import Blueprint.Language.Programming
-- @nonl
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

instance ProgrammingLanguage Haskell where
    languageHelloWorldScript _ = scriptFromLines $ ["main = putStrLn \"Hello, world!\""]
-- @nonl
-- @-node:gcross.20100615082419.1707:Haskell
-- @-node:gcross.20100615082419.1704:Languages
-- @+node:gcross.20100615082419.1705:Functions
-- @+node:gcross.20100615082419.1706:extractDependenciesFromHaskellSource
extractDependenciesFromHaskellSource :: L.ByteString → [UnresolvedDependency]
extractDependenciesFromHaskellSource =
    map (
        UnresolvedDependency Nothing
        .
        haskellModuleDependency
        .
        unpack
        .
        fst
        .
        (! 1)
    )
    .
    matchAllText import_regex
-- @-node:gcross.20100615082419.1706:extractDependenciesFromHaskellSource
-- @+node:gcross.20100628115452.1840:haskellModuleDependency
haskellModuleDependency :: String → Dependency
haskellModuleDependency = Dependency haskell_module_dependency_type
-- @-node:gcross.20100628115452.1840:haskellModuleDependency
-- @-node:gcross.20100615082419.1705:Functions
-- @+node:gcross.20100611224425.1689:Values
-- @+node:gcross.20100611224425.1708:regular expression
import_regex :: Regex
import_regex = makeRegex "^\\s*import\\s+(?:qualified\\s+)?([A-Z][A-Za-z0-9_.]*)"
-- @-node:gcross.20100611224425.1708:regular expression
-- @+node:gcross.20100628115452.1839:haskell_module_dependency_type
haskell_module_dependency_type = identifier "450299f0-5957-4e05-a185-88d765a032b8" "haskell module"
-- @-node:gcross.20100628115452.1839:haskell_module_dependency_type
-- @+node:gcross.20100628115452.1898:haskell_package_dependency_type
haskell_package_dependency_type = identifier "b0094d7f-1cf0-4cbf-8938-e40bf38a6e81" "haskell package"
-- @-node:gcross.20100628115452.1898:haskell_package_dependency_type
-- @-node:gcross.20100611224425.1689:Values
-- @-others
-- @-node:gcross.20100611224425.1682:@thin Haskell.hs
-- @-leo
