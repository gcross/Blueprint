-- @+leo-ver=4-thin
-- @+node:gcross.20100709210816.2097:@thin build-test.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100709210816.2098:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100709210816.2098:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100709210816.2099:<< Import needed modules >>
import qualified Data.Sequence as Seq

import Blueprint.Language.Programming.Haskell
import Blueprint.SourceFile
-- @-node:gcross.20100709210816.2099:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100709210816.2100:main
main = do
    haskell_sources ←
        fmap (extractHaskellSources . concat)
        .
        mapM (\subdirectory →
            fmap (map . prependParentToHierarchalPath . Seq.singleton $ subdirectory)
            .
            getAllSourceFilesIn
            $
            subdirectory
        )
        $
        ["Blueprint","Control","Data"] 
    mapM_ (putStrLn . show) haskell_sources
-- @-node:gcross.20100709210816.2100:main
-- @-others
-- @-node:gcross.20100709210816.2097:@thin build-test.hs
-- @-leo
