-- @+leo-ver=4-thin
-- @+node:gcross.20091121210308.1291:@thin Setup.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091129000542.1713:<< Language extensions >>
{-# LANGUAGE PackageImports #-}
-- @-node:gcross.20091129000542.1713:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20091128000856.1439:<< Import needed modules >>
import Blueprint.Tools.GHC.Main
-- @-node:gcross.20091128000856.1439:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091129000542.1485:main
main =
    simpleDefaultMain
        ("Blueprint","Blueprint")
        (Just
           (("","tests")
           ,[]
           ,["HUnit == 1.*"
            ,"QuickCheck == 2.*"
            ,"test-framework == 0.2.*"
            ,"test-framework-hunit == 0.2.*"
            ,"test-framework-quickcheck2 == 0.2.*"
            ]
           )
        )
        ["-O","-threaded"]
-- @-node:gcross.20091129000542.1485:main
-- @-others
-- @-node:gcross.20091121210308.1291:@thin Setup.hs
-- @-leo
