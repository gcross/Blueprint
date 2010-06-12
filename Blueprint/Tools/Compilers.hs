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
import Text.StringTemplate
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
-- @-others
-- @-node:gcross.20100611224425.1700:@thin Compilers.hs
-- @-leo
