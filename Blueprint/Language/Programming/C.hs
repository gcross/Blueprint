-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1588:@thin C.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1589:<< Language extensions >>
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1589:<< Language extensions >>
-- @nl

module Blueprint.Language.Programming.C where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1590:<< Import needed modules >>
import Data.Object
import Blueprint.Language
import Blueprint.Language.Programming
-- @-node:gcross.20100611224425.1590:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100615082419.1699:Languages
-- @+node:gcross.20100615082419.1702:C
data C

instance Language C where
    languageUUID _ = uuid "853ec165-56e3-431f-99dc-c7ff8b043202"
    languageName _ = "C"
    languageFileExtensions _ = ["c"]

instance ProgrammingLanguage C where
    languageHelloWorldScript _ = scriptFromLines $
        ["#include <stdio.h>"
        ,"int main() { printf(\"Hello, world!\"; }"
        ]
-- @-node:gcross.20100615082419.1702:C
-- @-node:gcross.20100615082419.1699:Languages
-- @-others
-- @-node:gcross.20100611224425.1588:@thin C.hs
-- @-leo
