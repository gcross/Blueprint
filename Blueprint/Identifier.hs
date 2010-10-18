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
data Identifier α = Identifier
    {   identifierUUID :: !UUID
    ,   identifierDescription :: !String
    } deriving (Typeable)
-- @nonl
-- @-node:gcross.20100927123234.1386:Identifier
-- @-node:gcross.20100927123234.1385:Types
-- @+node:gcross.20100927123234.1387:Instances
-- @+node:gcross.20100927123234.1388:Binary (Identifier α)
instance Binary (Identifier α) where
    put (Identifier α β) = do { put α; put β }
    get = liftM2 Identifier get get
-- @nonl
-- @-node:gcross.20100927123234.1388:Binary (Identifier α)
-- @+node:gcross.20100927123234.1389:Eq (Identifier α)
instance Eq (Identifier α) where
    (==) = (==) `on` identifierUUID
-- @nonl
-- @-node:gcross.20100927123234.1389:Eq (Identifier α)
-- @+node:gcross.20100927123234.1390:Ord (Identifier α)
instance Ord (Identifier α) where
    compare = compare `on` identifierUUID
-- @nonl
-- @-node:gcross.20100927123234.1390:Ord (Identifier α)
-- @+node:gcross.20100927123234.1391:Show (Identifier α)
instance Show (Identifier α) where
    show = identifierDescription
-- @nonl
-- @-node:gcross.20100927123234.1391:Show (Identifier α)
-- @+node:gcross.20100927123234.1392:Monoid (Identifier α)
instance Monoid (Identifier α) where
    mempty = Identifier nil ""
    i1 `mappend` i2
      | UUID.null (identifierUUID i1) = i2
      | UUID.null (identifierUUID i2) = i1
      | otherwise =
            identifierInNamespace
                (identifierUUID i1)
                (identifierDescription i1 ++ ", " ++ identifierDescription i2)
-- @nonl
-- @-node:gcross.20100927123234.1392:Monoid (Identifier α)
-- @-node:gcross.20100927123234.1387:Instances
-- @+node:gcross.20100927123234.1393:Functions
-- @+node:gcross.20100927123234.1394:identifier
identifier :: String → String → Identifier α
identifier = Identifier . uuid
-- @nonl
-- @-node:gcross.20100927123234.1394:identifier
-- @+node:gcross.20100927123234.1395:identifierInNamespace
identifierInNamespace :: UUID → String → Identifier α
identifierInNamespace namespace name =
    identifierInNamespaceWithDifferentDisplayName namespace name name
-- @nonl
-- @-node:gcross.20100927123234.1395:identifierInNamespace
-- @+node:gcross.20101018094310.1823:identifierInNamespaceWithDifferentDisplayName
identifierInNamespaceWithDifferentDisplayName :: UUID → String → String → Identifier α
identifierInNamespaceWithDifferentDisplayName namespace name display_name =
    Identifier
        (generateNamed namespace . UTF8.encode $ name)
        display_name
-- @-node:gcross.20101018094310.1823:identifierInNamespaceWithDifferentDisplayName
-- @+node:gcross.20100927123234.1396:sortIntoLabeledBins
sortIntoLabeledBins :: [(Identifier α, β)] → Map (Identifier α) [β]
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
-- @nonl
-- @-node:gcross.20100927123234.1396:sortIntoLabeledBins
-- @+node:gcross.20100927123234.1397:getContentsOfBinLabeledBy
getContentsOfBinLabeledBy :: Identifier α → Map (Identifier α) [β] → [β]
getContentsOfBinLabeledBy key = fromMaybe [] . Map.lookup key
-- @nonl
-- @-node:gcross.20100927123234.1397:getContentsOfBinLabeledBy
-- @-node:gcross.20100927123234.1393:Functions
-- @+node:gcross.20100927123234.1398:Values
-- @+node:gcross.20100927123234.1399:null_identifier
null_identifier :: Identifier α
null_identifier = Identifier UUID.nil ""
-- @nonl
-- @-node:gcross.20100927123234.1399:null_identifier
-- @-node:gcross.20100927123234.1398:Values
-- @-others
-- @-node:gcross.20100927123234.1382:@thin Identifier.hs
-- @-leo
