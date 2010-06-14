-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1605:@thin GCC.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1606:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1606:<< Language extensions >>
-- @nl

module Blueprint.Tools.Compilers.GCC where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1607:<< Import needed modules >>
import Data.Either.Unwrap
import Data.Version

import Text.Regex.Base
import Text.Regex.PCRE
import Text.Regex.PCRE.String

import Blueprint.Language.C
import Blueprint.Tools.Compilers
-- @nonl
-- @-node:gcross.20100611224425.1607:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100614121927.1635:Types
-- @+node:gcross.20100614121927.1636:GCC
data GCC = GCC
    {   gccVersion :: Version
    ,   gccCCompiler :: Compiler C
    }
-- @-node:gcross.20100614121927.1636:GCC
-- @-node:gcross.20100614121927.1635:Types
-- @+node:gcross.20100611224425.1608:Values
-- @+node:gcross.20100611224425.1609:gcc_version_regex
gcc_version_regex = makeRegex "\\S*? \\(.*?\\) ([0-9.]*)" :: Regex
-- @-node:gcross.20100611224425.1609:gcc_version_regex
-- @-node:gcross.20100611224425.1608:Values
-- @-others
-- @-node:gcross.20100611224425.1605:@thin GCC.hs
-- @-leo
