-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1664:@thin CPP.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1665:<< Language extensions >>
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1665:<< Language extensions >>
-- @nl

module Blueprint.Language.CPP where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1666:<< Import needed modules >>
import Data.Object
import Blueprint.Language
-- @-node:gcross.20100611224425.1666:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100614172544.1709:Languages
-- @+node:gcross.20100614172544.1710:CPP
data CPP

instance Language CPP where
    languageUUID _ = uuid "ecbcc465-3c29-4fd4-a4a5-0aa71868b337"
    languageName _ = "CPP"
    languageFileExtensions _ = ["cc","cpp","cxx"]
    languageHelloWorld _ = scriptFromLines $
        ["#include <iostream>"
        ,"int main() { std::cout << \"Hello, world!\"; }"
        ]
-- @-node:gcross.20100614172544.1710:CPP
-- @-node:gcross.20100614172544.1709:Languages
-- @-others
-- @-node:gcross.20100611224425.1664:@thin CPP.hs
-- @-leo
