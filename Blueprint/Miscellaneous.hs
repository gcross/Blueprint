-- @+leo-ver=4-thin
-- @+node:gcross.20100925004153.1323:@thin Miscellaneous.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100925004153.1324:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100925004153.1324:<< Language extensions >>
-- @nl

module Blueprint.Miscellaneous where

-- @<< Import needed modules >>
-- @+node:gcross.20100925004153.1325:<< Import needed modules >>
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

import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Regex.Base
-- @-node:gcross.20100925004153.1325:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100925004153.1330:Instances
-- @+node:gcross.20100927123234.1447:Binary Version
$(derive makeBinary ''Version)
-- @-node:gcross.20100927123234.1447:Binary Version
-- @+node:gcross.20100925004153.1331:Typeable MD5Digest
deriving instance Typeable MD5Digest
-- @-node:gcross.20100925004153.1331:Typeable MD5Digest
-- @-node:gcross.20100925004153.1330:Instances
-- @+node:gcross.20100925004153.1326:Functions
-- @+node:gcross.20100927123234.1419:doubleton
doubleton x y = [x,y]
-- @-node:gcross.20100927123234.1419:doubleton
-- @+node:gcross.20100927161850.1428:extractVersion
extractVersion ::
    RegexLike regex String =>
    regex →
    String →
    Maybe Version
extractVersion regex string =
    case mrSubList (match regex string) of
        v:[] → tryReadVersion v
        _ → Nothing
-- @-node:gcross.20100927161850.1428:extractVersion
-- @+node:gcross.20100927123234.1423:gather
gather :: (Ord a, Eq a) => [(a,b)] → [(a,[b])]
gather =
    map (fst . head &&& map snd)
    .
    groupBy ((==) `on` fst)
    .
    sortBy (compare `on` fst)
-- @-node:gcross.20100927123234.1423:gather
-- @+node:gcross.20100925004153.1327:inNamespace
inNamespace :: UUID → String → UUID
inNamespace uuid =
    generateNamed uuid
    .
    UTF8.encode
-- @-node:gcross.20100925004153.1327:inNamespace
-- @+node:gcross.20100927123234.1421:intersectAndUnion
intersectAndUnion :: Ord k => (a → b → b) → Map k a → Map k b → Map k b
intersectAndUnion combine x y = Map.union (Map.intersectionWith combine x y) y
-- @-node:gcross.20100927123234.1421:intersectAndUnion
-- @+node:gcross.20100927161850.1446:readVersion
readVersion :: String -> Version
readVersion s =
    fromMaybe
        (error $ "Unable to parse version string '" ++ s ++ "'")
        (tryReadVersion s)
-- @-node:gcross.20100927161850.1446:readVersion
-- @+node:gcross.20100927161850.1430:tryReadVersion
tryReadVersion :: String → Maybe Version
tryReadVersion string =
    case readP_to_S parseVersion string of 
        [] → Nothing 
        parses → (Just . fst . last) parses

-- @-node:gcross.20100927161850.1430:tryReadVersion
-- @+node:gcross.20100925004153.1328:uuid
uuid :: String → UUID
uuid = fromJust . fromString
-- @-node:gcross.20100925004153.1328:uuid
-- @-node:gcross.20100925004153.1326:Functions
-- @-others
-- @-node:gcross.20100925004153.1323:@thin Miscellaneous.hs
-- @-leo
