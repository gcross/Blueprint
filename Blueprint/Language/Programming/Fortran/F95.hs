-- @+leo-ver=4-thin
-- @+node:gcross.20100614172544.1704:@thin F95.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100614172544.1705:<< Language extensions >>
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100614172544.1705:<< Language extensions >>
-- @nl

module Blueprint.Language.Programming.Fortran.F95 where

-- @<< Import needed modules >>
-- @+node:gcross.20100614172544.1706:<< Import needed modules >>
import Data.Record

import Blueprint.Language
import Blueprint.Language.Programming
import Blueprint.Language.Programming.Fortran
-- @nonl
-- @-node:gcross.20100614172544.1706:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100614172544.1707:Languages
-- @+node:gcross.20100614172544.1708:Fortran95
data Fortran95

instance Language Fortran95 where
    languageUUID _ = uuid "d898165f-7671-4036-9476-422d44256248"
    languageName _ = "Fortran 95"
    languageFileExtension _ = "f95"

instance ProgrammingLanguage Fortran95 where
    languageHelloWorldScript _ = scriptFromLines $
        ["program helloworld"
        ,"  print \"(A13)\", \"Hello, world!\""
        ,"end program"
        ]

instance Fortran Fortran95
-- @nonl
-- @-node:gcross.20100614172544.1708:Fortran95
-- @-node:gcross.20100614172544.1707:Languages
-- @-others
-- @-node:gcross.20100614172544.1704:@thin F95.hs
-- @-leo
