-- @+leo-ver=4-thin
-- @+node:gcross.20090601155538.43:@thin test.hs
-- @@language Haskell

module Main where

-- @<< Imports >>
-- @+node:gcross.20090601155538.44:<< Imports >>
import HardHat
import HardHat.Blueprint
import HardHat.Blueprint.GHC
-- @-node:gcross.20090601155538.44:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090601155538.46:Blueprint
blueprint :: Blueprint
blueprint = do
    haskellProgram []
-- @-node:gcross.20090601155538.46:Blueprint
-- @+node:gcross.20090601155538.50:main
main = build blueprint
-- @-node:gcross.20090601155538.50:main
-- @-others
-- @-node:gcross.20090601155538.43:@thin test.hs
-- @-leo
