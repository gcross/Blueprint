-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1634:@thin Language.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1635:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1635:<< Language extensions >>
-- @nl

module Blueprint.Language where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1636:<< Import needed modules >>
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import Data.Record
import Data.Typeable
import Data.UUID (UUID)

import System.FilePath
-- @nonl
-- @-node:gcross.20100611224425.1636:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100611224425.1637:Classes
-- @+node:gcross.20100611224425.1638:Language
class Language language where
    language :: language
    languageUUID :: language → UUID
    languageName :: language → String
    languageFileExtension :: language → String
    languageFileExtensions :: language → [String]

    language = undefined
    languageFileExtension = head . languageFileExtensions
    languageFileExtensions = (:[]) . languageFileExtension
-- @-node:gcross.20100611224425.1638:Language
-- @-node:gcross.20100611224425.1637:Classes
-- @+node:gcross.20100614121927.1732:Languages
-- @+node:gcross.20100614121927.1734:NullLanguage
data NullLanguage

instance Language NullLanguage where 
    languageUUID _ = uuid "2b47a276-77f8-11df-b74e-001aa0c5d320"
    languageName _ = "(null)"
    languageFileExtensions _ = []
-- @-node:gcross.20100614121927.1734:NullLanguage
-- @-node:gcross.20100614121927.1732:Languages
-- @+node:gcross.20100611224425.1709:Functions
-- @+node:gcross.20100611224425.1710:dotsToPath
dotsToPath :: String → String
dotsToPath [] = []
dotsToPath ('.':rest) = pathSeparator:dotsToPath rest
dotsToPath (c:rest) = c:dotsToPath rest
-- @-node:gcross.20100611224425.1710:dotsToPath
-- @+node:gcross.20100614121927.1764:languageOf
languageOf :: Language language ⇒ t language → language
languageOf = undefined
-- @-node:gcross.20100614121927.1764:languageOf
-- @-node:gcross.20100611224425.1709:Functions
-- @-others
-- @-node:gcross.20100611224425.1634:@thin Language.hs
-- @-leo
