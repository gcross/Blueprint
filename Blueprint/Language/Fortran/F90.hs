-- @+leo-ver=4-thin
-- @+node:gcross.20100614172544.1694:@thin F90.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100614172544.1695:<< Language extensions >>
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100614172544.1695:<< Language extensions >>
-- @nl

module Blueprint.Language.Fortran.F90 where

-- @<< Import needed modules >>
-- @+node:gcross.20100614172544.1696:<< Import needed modules >>
import Data.Object

import Blueprint.Language
import Blueprint.Language.Fortran
-- @-node:gcross.20100614172544.1696:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100614172544.1697:Languages
-- @+node:gcross.20100614172544.1698:Fortran90
data Fortran90

instance Language Fortran90 where
    languageUUID _ = uuid "7cdd8626-bbc3-462e-9f35-b13e88fcf9eb"
    languageName _ = "Fortran 90"
    languageFileExtension _ = "f90"

instance Fortran Fortran90

languageFortran90 = undefined :: Fortran90
-- @-node:gcross.20100614172544.1698:Fortran90
-- @-node:gcross.20100614172544.1697:Languages
-- @-others
-- @-node:gcross.20100614172544.1694:@thin F90.hs
-- @-leo