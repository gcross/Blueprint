-- @+leo-ver=4-thin
-- @+node:gcross.20100613184558.1617:@thin Phases.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100613184558.1618:<< Language extensions >>
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100613184558.1618:<< Language extensions >>
-- @nl

module Blueprint.Phases where

-- @<< Import needed modules >>
-- @+node:gcross.20100613184558.1619:<< Import needed modules >>
-- @-node:gcross.20100613184558.1619:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100613184558.1622:Classes
-- @+node:gcross.20100613184558.1623:BlueprintPhase
class Monad phase ⇒ BlueprintPhase phase
-- @-node:gcross.20100613184558.1623:BlueprintPhase
-- @-node:gcross.20100613184558.1622:Classes
-- @+node:gcross.20100613184558.1620:Types
-- @+node:gcross.20100613184558.1621:Blueprint
type Blueprint = (∀ phase . BlueprintPhase phase ⇒ phase ())
-- @-node:gcross.20100613184558.1621:Blueprint
-- @-node:gcross.20100613184558.1620:Types
-- @-others
-- @-node:gcross.20100613184558.1617:@thin Phases.hs
-- @-leo
