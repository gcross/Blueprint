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
import Text.StringTemplate

import Blueprint.Language.C
import Blueprint.Language.CPP
import Blueprint.Language.Fortran.F77
import Blueprint.Language.Fortran.F90
import Blueprint.Language.Fortran.F95
import Blueprint.Miscellaneous
import Blueprint.Tools.Compilers
-- @-node:gcross.20100611224425.1607:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100614121927.1635:Types
-- @+node:gcross.20100614121927.1636:GCC
data GCC = GCC
    {   gccVersion :: Version
    ,   gccCCompiler :: Maybe (Compiler C)
    ,   gccCPPCompiler :: Maybe (Compiler CPP)
    ,   gccFortran77Compiler :: Maybe (Compiler Fortran77)
    ,   gccFortran90Compiler :: Maybe (Compiler Fortran90)
    ,   gccFortran95Compiler :: Maybe (Compiler Fortran95)
    }
-- @-node:gcross.20100614121927.1636:GCC
-- @-node:gcross.20100614121927.1635:Types
-- @+node:gcross.20100611224425.1608:Values
-- @+node:gcross.20100611224425.1609:gcc_version_regex
gcc_version_regex = makeRegex "\\S*? \\(.*?\\) ([0-9.]*)" :: Regex
-- @-node:gcross.20100611224425.1609:gcc_version_regex
-- @+node:gcross.20100614172544.1686:gcc_invocation_template
gcc_invocation_template :: StringTemplate String
gcc_invocation_template = newAngleSTMP
    "<command>\
    \<if(language)>\
        \ -x <language>\
    \<endif>\
    \<if(object)>\
        \ -c <source>\
    \<else>\
        \ <source; separator=\" \">\
    \<endif>\
    \<if(program)>\
        \<libraries:{\
            \<if(it.libraryLocation)>\
                \ <it.libraryLocation>\
            \<else>\
                \ -l<it.libraryName>\
            \<endif>\
        \}>\
    \<endif>\
    \ -o <object><program>"
-- @-node:gcross.20100614172544.1686:gcc_invocation_template
-- @-node:gcross.20100611224425.1608:Values
-- @-others
-- @-node:gcross.20100611224425.1605:@thin GCC.hs
-- @-leo
