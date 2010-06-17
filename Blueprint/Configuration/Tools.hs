-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1545:@thin Tools.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1546:<< Language extensions >>
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1546:<< Language extensions >>
-- @nl

module Blueprint.Configuration.Tools where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1547:<< Import needed modules >>
import Prelude hiding (catch)

import Control.Exception

import qualified Data.ByteString.Char8 as S
import Data.Version

import Text.ParserCombinators.ReadP
import Text.Regex.Base

import Blueprint.Miscellaneous
-- @-node:gcross.20100611224425.1547:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100611224425.1550:Functions
-- @+node:gcross.20100611224425.1551:readVersion
readVersion :: String → Maybe Version
readVersion s =
    case readP_to_S parser s of
         [(version,"")] -> Just version
         _ -> Nothing
  where
    parser = do
        version <- parseVersion
        eof
        return version
-- @-node:gcross.20100611224425.1551:readVersion
-- @+node:gcross.20100611224425.1555:extractVersion
extractVersion ::
    RegexLike regex S.ByteString ⇒
    regex →
    S.ByteString →
    Maybe Version
extractVersion regex s =
    case mrSubList (match regex s) of
        v:[] → readVersion (S.unpack v)
        _ → Nothing
-- @-node:gcross.20100611224425.1555:extractVersion
-- @+node:gcross.20100614121927.2359:probeVersion
probeVersion :: 
    RegexContext regex S.ByteString S.ByteString ⇒
    regex →
    FilePath →
    [String] →
    String →
    IO (Maybe Version)
probeVersion regex program arguments input =
    (fmap (extractVersion regex) $ readProcessByteString program arguments input)
    `catch`
    (\(e :: ProgramFailed) → return Nothing)
-- @-node:gcross.20100614121927.2359:probeVersion
-- @-node:gcross.20100611224425.1550:Functions
-- @-others
-- @-node:gcross.20100611224425.1545:@thin Tools.hs
-- @-leo
