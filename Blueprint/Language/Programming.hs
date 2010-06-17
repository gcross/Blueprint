-- @+leo-ver=4-thin
-- @+node:gcross.20100616175443.1689:@thin Programming.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100616175443.1690:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100616175443.1690:<< Language extensions >>
-- @nl

module Blueprint.Language.Programming where

-- @<< Import needed modules >>
-- @+node:gcross.20100616175443.1691:<< Import needed modules >>
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as S
import Data.Typeable

import Blueprint.Language
-- @-node:gcross.20100616175443.1691:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100616175443.1692:Classes
-- @+node:gcross.20100616175443.1693:ProgrammingLanguage
class Language language ⇒ ProgrammingLanguage language where
    languageHelloWorldScript :: language → Script language
-- @-node:gcross.20100616175443.1693:ProgrammingLanguage
-- @-node:gcross.20100616175443.1692:Classes
-- @+node:gcross.20100616175443.1694:Types
-- @+node:gcross.20100616175443.1696:Script
newtype Script language = Script S.ByteString deriving Typeable
-- @-node:gcross.20100616175443.1696:Script
-- @-node:gcross.20100616175443.1694:Types
-- @+node:gcross.20100616175443.1697:Functions
-- @+node:gcross.20100616175443.1699:scriptFromLines
scriptFromLines :: [String] -> Script language
scriptFromLines = Script . pack . unlines
-- @-node:gcross.20100616175443.1699:scriptFromLines
-- @+node:gcross.20100616175443.1701:verifyHelloWorld
verifyHelloWorld :: String → Bool
verifyHelloWorld = (== ["Hello,","world!"]) . words
-- @-node:gcross.20100616175443.1701:verifyHelloWorld
-- @-node:gcross.20100616175443.1697:Functions
-- @-others
-- @-node:gcross.20100616175443.1689:@thin Programming.hs
-- @-leo
