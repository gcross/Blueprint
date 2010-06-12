-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1634:@thin Languages.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1635:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1635:<< Language extensions >>
-- @nl

module Blueprint.Languages where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1636:<< Import needed modules >>
import Data.ByteString.Lazy as L

import System.FilePath
-- @nonl
-- @-node:gcross.20100611224425.1636:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100611224425.1637:Classes
-- @+node:gcross.20100611224425.1638:Language
class Language language where
    languageExtensions :: language → [String]
    languageDependencyExtractor :: language → L.ByteString → [Dependency]

    languageDependencyExtractor _ = const []
-- @-node:gcross.20100611224425.1638:Language
-- @-node:gcross.20100611224425.1637:Classes
-- @+node:gcross.20100611224425.1706:Types
-- @+node:gcross.20100611224425.1707:Dependency
data Dependency =
    Dependency
    {   dependencyBelongsToSystem :: Maybe Bool
    ,   dependencyPotentialRelativeFilePaths :: [String]
    } deriving (Eq, Show)

-- @-node:gcross.20100611224425.1707:Dependency
-- @-node:gcross.20100611224425.1706:Types
-- @+node:gcross.20100611224425.1709:Functions
-- @+node:gcross.20100611224425.1710:dotsToPath
dotsToPath :: String → String
dotsToPath [] = []
dotsToPath ('.':rest) = pathSeparator:dotsToPath rest
dotsToPath (c:rest) = c:dotsToPath rest
-- @-node:gcross.20100611224425.1710:dotsToPath
-- @-node:gcross.20100611224425.1709:Functions
-- @-others
-- @-node:gcross.20100611224425.1634:@thin Languages.hs
-- @-leo
