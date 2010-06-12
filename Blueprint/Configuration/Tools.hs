-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1545:@thin Tools.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1546:<< Language extensions >>
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1546:<< Language extensions >>
-- @nl

module Blueprint.Configuration.Tools where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1547:<< Import needed modules >>
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Version

import Text.ParserCombinators.ReadP
import Text.Regex.Base
-- @-node:gcross.20100611224425.1547:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100611224425.1548:Types
-- @+node:gcross.20100611224425.1549:Probe
data Probe = ∀ regex. RegexContext regex L.ByteString L.ByteString ⇒ Probe
    {   probePrograms :: [String]
    ,   probeArguments :: [String]
    ,   probeVersionRegEx :: regex
    }
-- @-node:gcross.20100611224425.1549:Probe
-- @-node:gcross.20100611224425.1548:Types
-- @+node:gcross.20100611224425.1550:Functions
-- @+node:gcross.20100611224425.1551:readVersion
readVersion :: String → Maybe Version
readVersion s =
    case readP_to_S parser s of
         [(version,"")] -> Just version
         _ -> Nothing
  where
    parser = do
        version <- parseVersion
        eof
        return version
-- @-node:gcross.20100611224425.1551:readVersion
-- @+node:gcross.20100611224425.1555:extractVersion
extractVersion ::
    RegexContext regex L.ByteString L.ByteString ⇒
    regex →
    L.ByteString →
    Maybe Version
extractVersion regex =
    readVersion
    .
    L.unpack
    .
    head
    .
    mrSubList
    .
    match regex
-- @-node:gcross.20100611224425.1555:extractVersion
-- @-node:gcross.20100611224425.1550:Functions
-- @-others
-- @-node:gcross.20100611224425.1545:@thin Tools.hs
-- @-leo
