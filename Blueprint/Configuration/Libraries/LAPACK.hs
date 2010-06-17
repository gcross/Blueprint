-- @+leo-ver=4-thin
-- @+node:gcross.20100610134550.1478:@thin LAPACK.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100610134550.1488:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100610134550.1488:<< Language extensions >>
-- @nl

module Blueprint.Configuration.Libraries.LAPACK where

-- @<< Import needed modules >>
-- @+node:gcross.20100610134550.1485:<< Import needed modules >>
import Distribution.Version

import Blueprint.Configuration
import Blueprint.Language.Programming
import Blueprint.Language.Programming.Fortran.F77
-- @-node:gcross.20100610134550.1485:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100610134550.1479:Values
-- @+node:gcross.20100610134550.1480:fortran_test_program
lapack_version_fortran_program :: Script Fortran77
lapack_version_fortran_program = scriptFromLines $
    ["program lapack_version                          "
    ,"  implicit none                                 "
    ,"  external ilaver                               "
    ,"  integer vers_major, vers_minor, vers_patch    "
    ,"  call ilaver(vers_major,vers_minor,vers_patch) "
    ,"  print *, vers_major, vers_minor, vers_patch   "
    ,"end program                                     "
    ]
-- @-node:gcross.20100610134550.1480:fortran_test_program
-- @-node:gcross.20100610134550.1479:Values
-- @+node:gcross.20100610134550.1483:Functions
-- @+node:gcross.20100610134550.1484:extractLAPACKVersionFromTestProgramOutput
extractLAPACKVersionFromTestProgramOutput :: String → Maybe Version
extractLAPACKVersionFromTestProgramOutput output =
    Just $
    Version
    {   versionBranch = map read . words $ output
    ,   versionTags = []
    }
-- @-node:gcross.20100610134550.1484:extractLAPACKVersionFromTestProgramOutput
-- @-node:gcross.20100610134550.1483:Functions
-- @-others
-- @-node:gcross.20100610134550.1478:@thin LAPACK.hs
-- @-leo
