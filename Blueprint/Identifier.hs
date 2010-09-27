-- @+leo-ver=4-thin
-- @+node:gcross.20100927123234.1382:@thin Identifier.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100927123234.1383:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100927123234.1383:<< Language extensions >>
-- @nl

module Blueprint.Identifier where

-- @<< Import needed modules >>
-- @+node:gcross.20100927123234.1384:<< Import needed modules >>
import qualified Codec.Binary.UTF8.String as UTF8

import Control.Monad

import Data.Binary
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Data.UUID
import qualified Data.UUID as UUID
import Data.UUID.V5 (generateNamed)

import Blueprint.Miscellaneous
-- @-node:gcross.20100927123234.1384:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100927123234.1385:Types
-- @+node:gcross.20100927123234.1386:Identifier
data Identifier a = Identifier
    {   identifierUUID :: !UUID
    ,   identifierDescription :: !String
    } deriving (Typeable)
-- @-node:gcross.20100927123234.1386:Identifier
-- @-node:gcross.20100927123234.1385:Types
-- @+node:gcross.20100927123234.1387:Instances
-- @+node:gcross.20100927123234.1388:Binary (Identifier a)
instance Binary (Identifier a) where
    put (Identifier a b) = do { put a; put b }
    get = liftM2 Identifier get get
-- @-node:gcross.20100927123234.1388:Binary (Identifier a)
-- @+node:gcross.20100927123234.1389:Eq (Identifier a)
instance Eq (Identifier a) where
    (==) = (==) `on` identifierUUID
-- @-node:gcross.20100927123234.1389:Eq (Identifier a)
-- @+node:gcross.20100927123234.1390:Ord (Identifier a)
instance Ord (Identifier a) where
    compare = compare `on` identifierUUID
-- @-node:gcross.20100927123234.1390:Ord (Identifier a)
-- @+node:gcross.20100927123234.1391:Show (Identifier a)
instance Show (Identifier a) where
    show = identifierDescription
-- @-node:gcross.20100927123234.1391:Show (Identifier a)
-- @+node:gcross.20100927123234.1392:Monoid (Identifier a)
instance Monoid (Identifier a) where
    mempty = Identifier nil ""
    i1 `mappend` i2
      | UUID.null (identifierUUID i1) = i2
      | UUID.null (identifierUUID i2) = i1
      | otherwise =
            identifierInNamespace
                (identifierUUID i1)
                (show . identifierUUID $ i2)
                (identifierDescription i1 ++ ", " ++ identifierDescription i2)
-- @-node:gcross.20100927123234.1392:Monoid (Identifier a)
-- @-node:gcross.20100927123234.1387:Instances
-- @+node:gcross.20100927123234.1393:Functions
-- @+node:gcross.20100927123234.1394:identifier
identifier :: String → String → Identifier a
identifier = Identifier . uuid
-- @-node:gcross.20100927123234.1394:identifier
-- @+node:gcross.20100927123234.1395:identifierInNamespace
identifierInNamespace :: UUID → String → String → Identifier a
identifierInNamespace namespace name_in_namespace display_name =
    Identifier
        (generateNamed namespace . UTF8.encode $ name_in_namespace)
        display_name
-- @-node:gcross.20100927123234.1395:identifierInNamespace
-- @+node:gcross.20100927123234.1396:sortIntoLabeledBins
sortIntoLabeledBins :: [(Identifier a, b)] → Map (Identifier a) [b]
sortIntoLabeledBins =
    foldr
        (\(k,v) bins →
            flip (Map.insert k) bins
            .
            maybe [v] (v:)
            .
            Map.lookup k
            $
            bins
        )
        Map.empty
-- @-node:gcross.20100927123234.1396:sortIntoLabeledBins
-- @+node:gcross.20100927123234.1397:getContentsOfBinLabeledBy
getContentsOfBinLabeledBy :: Identifier a → Map (Identifier a) [b] → [b]
getContentsOfBinLabeledBy key = fromMaybe [] . Map.lookup key
-- @-node:gcross.20100927123234.1397:getContentsOfBinLabeledBy
-- @-node:gcross.20100927123234.1393:Functions
-- @+node:gcross.20100927123234.1398:Values
-- @+node:gcross.20100927123234.1399:null_identifier
null_identifier :: Identifier a
null_identifier = Identifier UUID.nil ""
-- @-node:gcross.20100927123234.1399:null_identifier
-- @-node:gcross.20100927123234.1398:Values
-- @-others
-- @-node:gcross.20100927123234.1382:@thin Identifier.hs
-- @-leo
