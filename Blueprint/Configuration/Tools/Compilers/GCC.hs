-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1531:@thin GCC.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1532:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1532:<< Language extensions >>
-- @nl

module Blueprint.Configuration.Tools.Compilers.GCC where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1533:<< Import needed modules >>
import Data.Either.Unwrap

import Text.Regex.Base
import Text.Regex.PCRE
import Text.Regex.PCRE.String

import Blueprint.Configuration.Tools
-- @-node:gcross.20100611224425.1533:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100611224425.1536:Values
-- @+node:gcross.20100611224425.1537:gcc_probe
gcc_probe :: Probe
gcc_probe =
    Probe
    {   probePrograms = ["ghc"]
    ,   probeArguments = ["--version"]
    ,   probeVersionRegEx = (makeRegex "\\S*? \\(.*?\\) ([0-9.]*)" :: Regex)
    }
-- @-node:gcross.20100611224425.1537:gcc_probe
-- @-node:gcross.20100611224425.1536:Values
-- @-others
-- @-node:gcross.20100611224425.1531:@thin GCC.hs
-- @-leo
