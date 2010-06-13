-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1700:@thin Compilers.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1701:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1701:<< Language extensions >>
-- @nl

module Blueprint.Tools.Compilers where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1702:<< Import needed modules >>
import Data.Object
import Data.UUID

import Text.StringTemplate

import Blueprint.Languages
-- @-node:gcross.20100611224425.1702:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100611224425.1703:Types
-- @+node:gcross.20100611224425.1704:Compiler
data Compiler language = Compiler
    {   compilerProgram :: FilePath
    ,   compileToObjectTemplate :: StringTemplate String
    ,   compileToProgramTemplate :: StringTemplate String
    }
-- @-node:gcross.20100611224425.1704:Compiler
-- @-node:gcross.20100611224425.1703:Types
-- @+node:gcross.20100611224425.1715:Functions
-- @+node:gcross.20100611224425.1716:compilerFieldForLanguage
compilerFieldForLanguage :: Language language ⇒ language → Field (Compiler language)
compilerFieldForLanguage language =
    Field ("compiler for " ++ languageName language)
    .
    uuidInNamespace compiler_namespace
    .
    languageUUID
    $
    language
-- @-node:gcross.20100611224425.1716:compilerFieldForLanguage
-- @-node:gcross.20100611224425.1715:Functions
-- @+node:gcross.20100611224425.1718:Values
-- @+node:gcross.20100611224425.1719:compiler_namespace
compiler_namespace = uuid "a3b4b965-9801-43bd-a0fd-9c8ea5699035"
-- @-node:gcross.20100611224425.1719:compiler_namespace
-- @-node:gcross.20100611224425.1718:Values
-- @-others
-- @-node:gcross.20100611224425.1700:@thin Compilers.hs
-- @-leo
