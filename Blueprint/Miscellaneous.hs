-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Blueprint.Miscellaneous where

-- Imports {{{
import qualified Codec.Binary.UTF8.String as UTF8

import Control.Arrow

import Crypto.Classes

import Data.Binary
import Data.DeriveTH
import Data.Digest.Pure.MD5
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import Data.Version
import Data.UUID
import Data.UUID.V5

import Debug.Trace

import Text.ParserCombinators.ReadP (readP_to_S)
import Text.PrettyPrint
import Text.Regex.Base
-- }}}

-- Instances {{{
$(derive makeBinary ''Version)
deriving instance Typeable MD5Digest
-- }}}

-- Functions {{{
doubleton x y = [x,y]
echo x = trace (show x) x
echoWith h x = trace (h ++ " " ++ show x) x
extractVersion :: -- {{{
    RegexLike regex String =>
    regex →
    String →
    Maybe Version
extractVersion regex string =
    case mrSubList (match regex string) of
        v:[] → tryReadVersion v
        _ → Nothing
-- }}}
gather :: (Ord a, Eq a) => [(a,b)] → [(a,[b])] -- {{{
gather =
    map (fst . head &&& map snd)
    .
    groupBy ((==) `on` fst)
    .
    sortBy (compare `on` fst)
-- }}}
indentedListWithHeading :: Int → String → [String] → Doc -- {{{
indentedListWithHeading indentation heading =
    (text heading $$)
    .
    nest indentation
    .
    vcat
    .
    map text
-- }}}
inNamespace :: UUID → String → UUID -- {{{
inNamespace uuid =
    generateNamed uuid
    .
    UTF8.encode
-- }}}
intersectAndUnion :: Ord k => (a → b → b) → Map k a → Map k b → Map k b -- {{{
intersectAndUnion combine x y = Map.union (Map.intersectionWith combine x y) y
-- }}}
readVersion :: String → Version -- {{{
readVersion s =
    fromMaybe
        (error $ "Unable to parse version string '" ++ s ++ "'")
        (tryReadVersion s)
-- }}}
tryReadVersion :: String → Maybe Version -- {{{
tryReadVersion string =
    case readP_to_S parseVersion string of 
        [] → Nothing 
        parses → (Just . fst . last) parses

-- }}}
uuid :: String → UUID -- {{{
uuid = fromJust . fromString
-- }}}
-- }}}

