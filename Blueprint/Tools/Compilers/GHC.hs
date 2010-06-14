-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1610:@thin GHC.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1611:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1611:<< Language extensions >>
-- @nl

module Blueprint.Tools.Compilers.GHC where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1612:<< Import needed modules >>
import Data.Either.Unwrap

import Text.Regex.Base
import Text.Regex.PCRE
import Text.Regex.PCRE.String

import Blueprint.Configuration.Tools
-- @-node:gcross.20100611224425.1612:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100611224425.1613:Values
-- @+node:gcross.20100611224425.1614:ghc_probe
ghc_version_regex = makeRegex "version ([0-9.]*)" :: Regex
-- @-node:gcross.20100611224425.1614:ghc_probe
-- @-node:gcross.20100611224425.1613:Values
-- @-others
-- @-node:gcross.20100611224425.1610:@thin GHC.hs
-- @-leo
