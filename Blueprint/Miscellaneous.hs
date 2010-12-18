-- @+leo-ver=5-thin
-- @+node:gcross.20100925004153.1323: * @thin Miscellaneous.hs
-- @@language Haskell
-- @+<< Language extensions >>
-- @+node:gcross.20100925004153.1324: ** << Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-<< Language extensions >>

module Blueprint.Miscellaneous where

-- @+<< Import needed modules >>
-- @+node:gcross.20100925004153.1325: ** << Import needed modules >>
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
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20100925004153.1330: ** Instances
-- @+node:gcross.20100927123234.1447: *3* Binary Version
$(derive makeBinary ''Version)
-- @+node:gcross.20100925004153.1331: *3* Typeable MD5Digest
deriving instance Typeable MD5Digest
-- @+node:gcross.20100925004153.1326: ** Functions
-- @+node:gcross.20100927123234.1419: *3* doubleton
doubleton x y = [x,y]
-- @+node:gcross.20101018141146.1542: *3* echo
echo x = trace (show x) x
-- @+node:gcross.20101018141146.1544: *3* echo
echoWith h x = trace (h ++ " " ++ show x) x
-- @+node:gcross.20100927161850.1428: *3* extractVersion
extractVersion ::
    RegexLike regex String =>
    regex →
    String →
    Maybe Version
extractVersion regex string =
    case mrSubList (match regex string) of
        v:[] → tryReadVersion v
        _ → Nothing
-- @+node:gcross.20100927123234.1423: *3* gather
gather :: (Ord a, Eq a) => [(a,b)] → [(a,[b])]
gather =
    map (fst . head &&& map snd)
    .
    groupBy ((==) `on` fst)
    .
    sortBy (compare `on` fst)
-- @+node:gcross.20101027123147.1546: *3* indentedListWithHeading
indentedListWithHeading :: Int → String → [String] → Doc
indentedListWithHeading indentation heading =
    (text heading $$)
    .
    nest indentation
    .
    vcat
    .
    map text
-- @+node:gcross.20100925004153.1327: *3* inNamespace
inNamespace :: UUID → String → UUID
inNamespace uuid =
    generateNamed uuid
    .
    UTF8.encode
-- @+node:gcross.20100927123234.1421: *3* intersectAndUnion
intersectAndUnion :: Ord k => (a → b → b) → Map k a → Map k b → Map k b
intersectAndUnion combine x y = Map.union (Map.intersectionWith combine x y) y
-- @+node:gcross.20100927161850.1446: *3* readVersion
readVersion :: String -> Version
readVersion s =
    fromMaybe
        (error $ "Unable to parse version string '" ++ s ++ "'")
        (tryReadVersion s)
-- @+node:gcross.20100927161850.1430: *3* tryReadVersion
tryReadVersion :: String → Maybe Version
tryReadVersion string =
    case readP_to_S parseVersion string of 
        [] → Nothing 
        parses → (Just . fst . last) parses

-- @+node:gcross.20100925004153.1328: *3* uuid
uuid :: String → UUID
uuid = fromJust . fromString
-- @-others
-- @-leo
