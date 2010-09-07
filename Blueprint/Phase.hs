-- @+leo-ver=4-thin
-- @+node:gcross.20100906112631.2173:@thin Phase.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100906112631.2174:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100906112631.2174:<< Language extensions >>
-- @nl

module Blueprint.Phase where

-- @<< Import needed modules >>
-- @+node:gcross.20100906112631.2175:<< Import needed modules >>
-- @-node:gcross.20100906112631.2175:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100906112631.2176:Types
-- @+node:gcross.20100906112631.2177:Phase
newtype Phase a = Phase { unwrapPhase :: Bool → IO a }
-- @nonl
-- @-node:gcross.20100906112631.2177:Phase
-- @-node:gcross.20100906112631.2176:Types
-- @+node:gcross.20100906112631.2178:Instances
-- @+node:gcross.20100906112631.2179:Monad Phase
instance Monad Phase where
    return = Phase . const . return
    Phase x >>= f = Phase $ \is_primary_Phase → do
        x_result ← x False
        unwrapPhase (f x_result) is_primary_Phase
    Phase x >> Phase y = Phase $ (x False >>) . y
-- @nonl
-- @-node:gcross.20100906112631.2179:Monad Phase
-- @-node:gcross.20100906112631.2178:Instances
-- @+node:gcross.20100906112631.2192:Functions
-- @+node:gcross.20100906112631.2193:runPhase
runPhase :: Phase a → IO ()
runPhase (Phase x) = x True >> return ()
-- @nonl
-- @-node:gcross.20100906112631.2193:runPhase
-- @-node:gcross.20100906112631.2192:Functions
-- @-others
-- @nonl
-- @-node:gcross.20100906112631.2173:@thin Phase.hs
-- @-leo
