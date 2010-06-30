-- @+leo-ver=4-thin
-- @+node:gcross.20100624100717.1731:@thin Identifier.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100624100717.1732:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100624100717.1732:<< Language extensions >>
-- @nl

module Blueprint.Identifier where

-- @<< Import needed modules >>
-- @+node:gcross.20100624100717.1733:<< Import needed modules >>
import Control.Monad

import Data.Binary
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Record (uuid)
import Data.Typeable
import Data.UUID
import qualified Data.UUID as UUID
-- @-node:gcross.20100624100717.1733:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100624100717.1734:Types
-- @+node:gcross.20100624100717.1735:Identifier
data Identifier a = Identifier
    {   identifierUUID :: !UUID
    ,   identifierDescription :: !String
    } deriving (Typeable)
-- @-node:gcross.20100624100717.1735:Identifier
-- @-node:gcross.20100624100717.1734:Types
-- @+node:gcross.20100624100717.1765:Instances
-- @+node:gcross.20100624100717.1766:Binary (Identifier a)
instance Binary (Identifier a) where
    put (Identifier a b) = do { put a; put b }
    get = liftM2 Identifier get get
-- @-node:gcross.20100624100717.1766:Binary (Identifier a)
-- @+node:gcross.20100624100717.2020:Eq (Identifier a)
instance Eq (Identifier a) where
    (==) = (==) `on` identifierUUID
-- @-node:gcross.20100624100717.2020:Eq (Identifier a)
-- @+node:gcross.20100624100717.2022:Ord (Identifier a)
instance Ord (Identifier a) where
    compare = compare `on` identifierUUID
-- @-node:gcross.20100624100717.2022:Ord (Identifier a)
-- @+node:gcross.20100624100717.2023:Show (Identifier a)
instance Show (Identifier a) where
    show = identifierDescription
-- @-node:gcross.20100624100717.2023:Show (Identifier a)
-- @-node:gcross.20100624100717.1765:Instances
-- @+node:gcross.20100624100717.1738:Functions
-- @+node:gcross.20100624100717.1739:identifier
identifier, (★) :: String → String → Identifier a
identifier = Identifier . uuid
(★) = identifier
-- @-node:gcross.20100624100717.1739:identifier
-- @+node:gcross.20100628115452.1862:bin
bin :: [(Identifier a, b)] → Map (Identifier a) [b]
bin =
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
-- @-node:gcross.20100628115452.1862:bin
-- @-node:gcross.20100624100717.1738:Functions
-- @+node:gcross.20100628115452.1889:Values
-- @+node:gcross.20100628115452.1890:null_identifier
null_identifier :: Identifier a
null_identifier = Identifier UUID.nil ""
-- @-node:gcross.20100628115452.1890:null_identifier
-- @-node:gcross.20100628115452.1889:Values
-- @-others
-- @-node:gcross.20100624100717.1731:@thin Identifier.hs
-- @-leo
