-- @+leo-ver=4-thin
-- @+node:gcross.20100613184558.1629:@thin Configuration.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100613184558.1630:<< Language extensions >>
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100613184558.1630:<< Language extensions >>
-- @nl

module Blueprint.Phases.Configuration where

-- @<< Import needed modules >>
-- @+node:gcross.20100613184558.1631:<< Import needed modules >>
import Control.Monad.Trans.RWS

import Data.Binary
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Object (Field, SerializableObject)
import qualified Data.Object as Object
import Data.Typeable

import Blueprint.Phases
-- @-node:gcross.20100613184558.1631:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100613184558.1632:Types
-- @+node:gcross.20100613184558.1633:Phase
type Phase = RWS (Map String String) () SerializableObject
-- @-node:gcross.20100613184558.1633:Phase
-- @-node:gcross.20100613184558.1632:Types
-- @+node:gcross.20100613184558.1636:Instances
-- @+node:gcross.20100613184558.1637:Instance BlueprintPhase Phase
instance BlueprintPhase Phase
-- @-node:gcross.20100613184558.1637:Instance BlueprintPhase Phase
-- @-node:gcross.20100613184558.1636:Instances
-- @+node:gcross.20100613184558.1634:Functions
-- @+node:gcross.20100613184558.1640:getOption
getOption :: String → Phase (Maybe String)
getOption = asks . Map.lookup
-- @-node:gcross.20100613184558.1640:getOption
-- @+node:gcross.20100613184558.1635:getField
getField :: (Binary a, Typeable a) ⇒ Field a → Phase (Maybe a)
getField = gets . Object.getField
-- @-node:gcross.20100613184558.1635:getField
-- @+node:gcross.20100613184558.1639:setField
setField :: (Binary a, Typeable a) ⇒ Field a → a → Phase ()
setField field value = modify (Object.setField field value)
-- @-node:gcross.20100613184558.1639:setField
-- @-node:gcross.20100613184558.1634:Functions
-- @-others
-- @-node:gcross.20100613184558.1629:@thin Configuration.hs
-- @-leo
