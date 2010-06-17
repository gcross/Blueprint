-- @+leo-ver=4-thin
-- @+node:gcross.20100614121927.1756:@thin F77.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100614121927.1757:<< Language extensions >>
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100614121927.1757:<< Language extensions >>
-- @nl

module Blueprint.Language.Programming.Fortran.F77 where

-- @<< Import needed modules >>
-- @+node:gcross.20100614121927.1758:<< Import needed modules >>
import Data.Object

import Blueprint.Language
import Blueprint.Language.Programming
import Blueprint.Language.Programming.Fortran
-- @-node:gcross.20100614121927.1758:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100614121927.1759:Languages
-- @+node:gcross.20100614121927.1760:Fortran77
data Fortran77

instance Language Fortran77 where
    languageUUID _ = uuid "38e69eb4-ecd7-41d1-992b-dade07f27a4e"
    languageName _ = "Fortran 77"
    languageFileExtension _ = "f"

instance ProgrammingLanguage Fortran77 where
    languageHelloWorldScript _ = scriptFromLines $
        ["      PROGRAM HELLOWORLD"
        ,"      PRINT *, 'Hello, world!'"
        ,"      END"
        ]

instance Fortran Fortran77
-- @nonl
-- @-node:gcross.20100614121927.1760:Fortran77
-- @-node:gcross.20100614121927.1759:Languages
-- @-others
-- @-node:gcross.20100614121927.1756:@thin F77.hs
-- @-leo
