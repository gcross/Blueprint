-- @+leo-ver=4-thin
-- @+node:gcross.20100925004153.1323:@thin Miscellaneous.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100925004153.1324:<< Language extensions >>
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100925004153.1324:<< Language extensions >>
-- @nl

module Blueprint.Miscellaneous where

-- @<< Import needed modules >>
-- @+node:gcross.20100925004153.1325:<< Import needed modules >>
import qualified Codec.Binary.UTF8.String as UTF8

import Control.Arrow

import Crypto.Classes

import Data.Digest.Pure.MD5
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import Data.UUID
import Data.UUID.V5
-- @-node:gcross.20100925004153.1325:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100925004153.1330:Instances
-- @+node:gcross.20100925004153.1331:Typeable MD5Digest
deriving instance Typeable MD5Digest
-- @-node:gcross.20100925004153.1331:Typeable MD5Digest
-- @-node:gcross.20100925004153.1330:Instances
-- @+node:gcross.20100925004153.1326:Functions
-- @+node:gcross.20100927123234.1419:doubleton
doubleton x y = [x,y]
-- @-node:gcross.20100927123234.1419:doubleton
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
-- @+node:gcross.20100925004153.1328:uuid
uuid :: String → UUID
uuid = fromJust . fromString
-- @-node:gcross.20100925004153.1328:uuid
-- @-node:gcross.20100925004153.1326:Functions
-- @-others
-- @-node:gcross.20100925004153.1323:@thin Miscellaneous.hs
-- @-leo
