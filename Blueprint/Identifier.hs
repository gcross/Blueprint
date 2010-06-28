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
import Data.Record (uuid)
import Data.Typeable
import Data.UUID
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
-- @-node:gcross.20100624100717.1738:Functions
-- @-others
-- @-node:gcross.20100624100717.1731:@thin Identifier.hs
-- @-leo
