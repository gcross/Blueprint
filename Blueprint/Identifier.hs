-- @+leo-ver=5-thin
-- @+node:gcross.20100927123234.1382: * @thin Identifier.hs
-- @@language Haskell
-- @+<< Language extensions >>
-- @+node:gcross.20100927123234.1383: ** << Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-<< Language extensions >>

module Blueprint.Identifier where

-- @+<< Import needed modules >>
-- @+node:gcross.20100927123234.1384: ** << Import needed modules >>
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
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20100927123234.1385: ** Types
-- @+node:gcross.20100927123234.1386: *3* Identifier
data Identifier α = Identifier
    {   identifierUUID :: !UUID
    ,   identifierDescription :: !String
    } deriving (Typeable)
-- @+node:gcross.20100927123234.1387: ** Instances
-- @+node:gcross.20100927123234.1388: *3* Binary (Identifier α)
instance Binary (Identifier α) where
    put (Identifier α β) = do { put α; put β }
    get = liftM2 Identifier get get
-- @+node:gcross.20100927123234.1389: *3* Eq (Identifier α)
instance Eq (Identifier α) where
    (==) = (==) `on` identifierUUID
-- @+node:gcross.20100927123234.1390: *3* Ord (Identifier α)
instance Ord (Identifier α) where
    compare = compare `on` identifierUUID
-- @+node:gcross.20100927123234.1391: *3* Show (Identifier α)
instance Show (Identifier α) where
    show = identifierDescription
-- @+node:gcross.20100927123234.1392: *3* Monoid (Identifier α)
instance Monoid (Identifier α) where
    mempty = Identifier nil ""
    i1 `mappend` i2
      | UUID.null (identifierUUID i1) = i2
      | UUID.null (identifierUUID i2) = i1
      | otherwise =
            identifierInNamespace
                (identifierUUID i1)
                (identifierDescription i1 ++ ", " ++ identifierDescription i2)
-- @+node:gcross.20100927123234.1393: ** Functions
-- @+node:gcross.20100927123234.1394: *3* identifier
identifier :: String → String → Identifier α
identifier = Identifier . uuid
-- @+node:gcross.20100927123234.1395: *3* identifierInNamespace
identifierInNamespace :: UUID → String → Identifier α
identifierInNamespace namespace name =
    identifierInNamespaceWithDifferentDisplayName namespace name name
-- @+node:gcross.20101018094310.1823: *3* identifierInNamespaceWithDifferentDisplayName
identifierInNamespaceWithDifferentDisplayName :: UUID → String → String → Identifier α
identifierInNamespaceWithDifferentDisplayName namespace name display_name =
    Identifier
        (generateNamed namespace . UTF8.encode $ name)
        display_name
-- @+node:gcross.20100927123234.1396: *3* sortIntoLabeledBins
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
-- @+node:gcross.20100927123234.1397: *3* getContentsOfBinLabeledBy
getContentsOfBinLabeledBy :: Identifier α → Map (Identifier α) [β] → [β]
getContentsOfBinLabeledBy key = fromMaybe [] . Map.lookup key
-- @+node:gcross.20100927123234.1398: ** Values
-- @+node:gcross.20100927123234.1399: *3* null_identifier
null_identifier :: Identifier α
null_identifier = Identifier UUID.nil ""
-- @-others
-- @-leo
